
# ==============================================================================
# Script Name : process.R
# Purpose     : process pse by org data - for publication
# Author      : Jackie
# Created     : YYYY-MM-DD
# Updated     : YYYY-MM-DD
# Version     : 1.0.0
# ==============================================================================


# ==============================================================================
# PACKAGES
# ==============================================================================

# Load required packages

# ==============================================================================
# IMPORT wrangled data
# ==============================================================================
source(here("data_processing_scripts/import/import and sort.R"))


# ==============================================================================
# FILTER OUT CONDITIONS
# ==============================================================================
# filter out Permanent only HC/FTE

pse_org2 <- pse_org |> 
  filter(!grepl("Perm", Organisation)) |> 
  filter(Organisation != "SCOTLAND") 

# check if latest data col is empty - remove if so

na_check <- pse_org2 |>
  filter(if_all(all_of(latest_data), ~ is.na(.)))

pse_org2 <- pse_org2 |>
  filter(if_all(all_of(latest_data), ~ !is.na(.)))





# ==============================================================================
# CHECK AGAINST Q3 2025
# ==============================================================================

# check for organisations in q1 2026 raw data not in q3 2025
diff <- setdiff(pse_org2$Organisation, lookup$`last lookup`$input_name)
same <- intersect(pse_org2$Organisation, lookup$`last lookup`$input_name)
diff 

# change names to match last lookup (from Fergus Q3)
pse_org2 <- pse_org2 %>%
  mutate(
    Organisation = recode(
      Organisation,
      "(Comprising of: Banff & Buchan College of Further Education & Aberdeen College)" =
        "North East Scotland College (Comprising of: Banff & Buchan College of Further Education & Aberdeen College)",
      
      "(Merger of Carnegie & Adam Smith college to form Fife College)" =
        "Fife College (Merger of Carnegie & Adam Smith college to form Fife College)",
      
      "Glasgow Kelvin College(Comprising of : John Wheatley College, Stow College, North Glasgow College)" =
        "Glasgow Kelvin College (Comprising of : John Wheatley College, Stow College, North Glasgow College)",
      
      "(Incorporates Dundee College)" =
        "Angus College (Incorporates Dundee College)",
      
      "(Merger of  Lews Castle College, West Highland College and North Highland College which took place on 1/8/2023)" =
        "UHI North, West and Hebrides (Merger of Lews Castle College, West Highland College and North Highland College which took place on 1/8/2023)"
    )
  )

# remove ...suffix from any org names
pse_org2 <- pse_org2 %>%
  mutate(
    Organisation = str_remove(Organisation, "\\.\\.\\.\\d+$")
  )

  

# ==============================================================================
# REMOVE Q12019 - Q32024 data for UHI (incomplete - only North Highland College)
# ==============================================================================
# create list of columns in which values will be replaced
quarter_cols <- c("Quarter 1",
                  "Quarter 2",
                  "Quarter 3",
                  "Quarter 4")

years <- c(2019,2020,2021,2022,2023)

zero_period <- unlist(lapply(years, function(i) paste(i, quarter_cols)))



# removing Q4 2023
zero_period <- zero_period[-length(zero_period)]

pse_org2 <- pse_org2 |>
  mutate(
    across(
      all_of(zero_period),
      ~ if_else(
        Organisation == "UHI North, West and Hebrides (Merger of Lews Castle College, West Highland College and North Highland College which took place on 1/8/2023)",
        0,
        .x
      )
    )
  )


# ==============================================================================
## join classifications 
# ==============================================================================

#rename before joining
prev_look<-lookup$`last lookup` 
prev_look <- prev_look |> 
  rename(Organisation = input_name)

pse_org2 <- left_join(pse_org2, prev_look, by = "Organisation")

# add "other public bodies" classification for Redress Scotland
# first check
na_check <- pse_org2 |>
  filter(if_all(classification, ~ is.na(.)))

pse_org2 <- pse_org2 |> 
  mutate(classification = ifelse(is.na(classification), 
                                       "Other Public Bodies",
                                 classification),
         classification2 = ifelse(is.na(classification2), 
                                  "Other Public Bodies",
                                  classification2))

