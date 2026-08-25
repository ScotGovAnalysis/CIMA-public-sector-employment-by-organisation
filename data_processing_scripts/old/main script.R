#set libraries

library(tidyverse)
library(janitor)
library(readxl)
library(writexl)
library(openxlsx)
library(dplyr)
library(gt)

#read in data - this is effectively the file OCEA made available, but with one sheet instead of several
#for now, points to a local file on the C drive as not to stretch the data sharing agreement. in future, explore options to link to ADM directly

input_data<-read.xlsx("./data/pse_complete_2025_q3.xlsx",sheet=2,colNames=FALSE,fillMergedCells=TRUE) 

#read in rename and classification guide

rename_and_classify<-read.xlsx("./data/pse_adapted_lookup_2025_q3.xlsx",sheet=1) %>%  mutate(input_name=str_trim(input_name)) %>% mutate(input_name = gsub("[\r\n]", " ",input_name)) %>%  as_tibble

#email_addresses<-read.xlsx("./data/pse_email_contacts_2025_q3.xlsx",sheet=1,colNames=TRUE) 

#transpose the data into an easier format for quality assurance and tidying
#various additional tidying steps like trimming names, converting types, and rounding to nearest 1 (not final rounding for publication)

transposed<-t(input_data) %>% as_tibble() %>% row_to_names(row_number=1) %>% pivot_longer(-c("Organisation", "Variable"),values_to="count") %>% rename(date=name) %>% type_convert() %>% mutate(period=(quarter(ym(date), type="year.quarter"))) %>%  select(-c("date")) %>% mutate(Variable=recode(Variable, "HC Total" = "Headcount Total", "Perm FTE Total" = "FTE Permanent only", "Perm HC Total" = "Headcount Permanent only")) %>% mutate(count = round(count, 1)) %>% mutate(Organisation = gsub("[\r\n]", " ", Organisation)) %>%  mutate(Organisation = gsub("  ", " ", Organisation))

#transpose it back and do some more tidying

for_publication<-transposed %>% pivot_wider(names_from=period,values_from=count) %>% arrange(Organisation, Variable) %>% mutate(Organisation=str_trim(Organisation))

#join on rename and classification guide, and apply it

renamed_classified_data <- for_publication %>%
 left_join(rename_and_classify, by = c("Organisation" = "input_name")) %>%
  mutate(Organisation = coalesce(updated_name, Organisation)) %>%
  select(-updated_name) %>%  mutate(Organisation=str_trim(Organisation))

#remove any orgs where employment at the latest quarter is zero/NA

only_active<-renamed_classified_data %>% filter(!is.na(.[["2025.3"]]))

#remove variables relating to permanent staff only

disregard_perm_temp_split<-only_active %>% filter(!Variable %in% c("FTE Permanent only", "Headcount Permanent only") )

#strip out any organisations which are out of scope (those not currently classified, and the erroneously included Office of the Secretary of State for Scotland)

remove_out_of_scope<-disregard_perm_temp_split %>% filter(!Organisation %in% c("Police Investigations & Review Commissioner", "Risk Management Authority", "Scottish Road Works Commissioner", "Office of the Secretary of State for Scotland"))

#calculate totals and subtotals to compare with published PSE for the latest quarter; they should be consistent

category1_subtotals_all<-remove_out_of_scope %>% mutate(latest=.[["2025.3"]]) %>% group_by(classification, Variable) %>% summarise(total=sum(latest))
category1_counts_all<-remove_out_of_scope %>% mutate(latest=.[["2025.3"]]) %>% group_by(classification, Variable) %>% summarise(total=n())

category2_subtotals_all<-remove_out_of_scope %>% mutate(latest=.[["2025.3"]]) %>% group_by(classification2, Variable) %>% summarise(total=sum(latest))
category2_counts_all<-remove_out_of_scope %>% mutate(latest=.[["2025.3"]]) %>% group_by(classification2, Variable) %>% summarise(total=n())

totals_all<-category1_subtotals_all %>% group_by(Variable) %>% summarise(total2=sum(total))
counts_all<-category1_counts_all %>% group_by(Variable) %>% summarise(total2=sum(total))

#list the orgs we don't have consent to publish (they will be filtered out)
#list the orgs we need to suppress for secondary disclosure (they will be retained but their values suppressed)

list_of_refusals<-c("Caledonian Maritime Assets Ltd", "Crown Estate Scotland", "National Museums of Scotland", "Scottish Parliamentary Corporate Body", "Scottish Social Services Council", "Audit Scotland") %>% str_trim()
list_of_disclosive<-c("Edinburgh Tours") %>% str_trim()

#apply the lists immediately above to check that they're working as intended

refusals_and_disclosive<-remove_out_of_scope %>% filter(Organisation %in% c(list_of_refusals, list_of_disclosive))

#calculate counts for excluded bodies

count_removed_category1<-refusals_and_disclosive %>% mutate(latest=.[["2025.3"]]) %>% group_by(classification, Variable) %>% summarise(total=n())
count_removed_category2<-refusals_and_disclosive %>% mutate(latest=.[["2025.3"]]) %>% group_by(classification2, Variable) %>% summarise(total=n())

#apply suppression of small values and publication rounding, for now assigning specific negative numeric values so we can keep numeric variables (negative count of employees is otherwise not possible)

suppressed<-remove_out_of_scope %>% mutate(across(where(is.numeric),~ ifelse( is.na(.x), 0, .x)  )) %>% mutate(across(where(is.numeric),~ ifelse( .x < 5 & .x > 0 , -9, .x)  ))

rounded<- suppressed %>% mutate(across(where(is.numeric), ~round(.x/5)*5)) %>% relocate(classification, .after=Organisation)

#create an output dataset which includes everyone, for the purpose of considering disclosure control (since the values of excluded orgs matter for this)

for_output_all<-rounded %>% relocate (where(is.numeric), .after=where(is.character)) %>% relocate(classification2, .after=classification) %>% rename("Primary classification" = classification) %>% rename("Secondary classification" = classification2) 

#split the above output into those for inclusion and those for exclusion. the file for exclusion might not be used later
for_output_refused_redacted_removed<-for_output_all %>% filter(!Organisation %in% list_of_refusals) %>% filter(!Organisation %in% list_of_disclosive)
not_for_output_disclosive <-rounded %>% relocate (where(is.numeric), .after=where(is.character)) %>% relocate(classification2, .after=classification) %>% rename("Primary classification" = classification) %>% rename("Secondary classification" = classification2) 


#use the "includes everyone" dataset to create two additional sets with values suppressed - one for those who don't consent, and one for those suppressed for secondary disclosure
for_output_refused_stripped<-for_output_all %>%  filter(Organisation %in% list_of_refusals) %>% mutate(across(where(is.numeric), ~-50))
for_output_disclosive_stripped <-for_output_all %>% filter(Organisation %in% list_of_disclosive) %>% mutate(across(where(is.numeric), ~-100))

#combine the set of those to include, with those to suppress. Here we deliberately don't bring the refused category back in, so they are not named in the data. Additional formatting happens here, including converting everything to character, introducing symbols, and creating a spanning header. This spanning header is an unnecessary complication and should be removed at some stage

formatting_for_output<-rbind(for_output_refused_redacted_removed, for_output_disclosive_stripped) %>% arrange(.[[3]], .[[1]])  %>% mutate(across(where(is.numeric), as.character)) %>% mutate(across(everything(),~ case_when(.x == "-10" ~ "[low]", .x == "-50" ~ "c1",  .x == "-100" ~ "[c]",TRUE~.x))) %>% gt() %>% tab_spanner(label="Period", columns=c("2019.1":"2025.3"))

#write out the data for publication. this is where the spanning header is a complication; can't write directly to Excel, so write to HTML and then open that file in Excel

formatting_for_output %>%  gtsave(filename = "third_export.html")

