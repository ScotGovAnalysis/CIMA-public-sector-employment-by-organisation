
# ==============================================================================
# Script Name : disclosure_control.R
# Purpose     : apply suppression to PSE org data, output workbook for exchequer and create publication dataset
# Author      : Jackie
# Created     : YYYY-MM-DD
# Updated     : YYYY-MM-DD
# Version     : 1.0.0
# ==============================================================================


# ==============================================================================
# PACKAGES
# ==============================================================================

# Load required packages
library(purrr)
library(writexl)
library(tidyverse)
library(here)
library(openxlsx)
# ==============================================================================
# SETUP
# ==============================================================================
latest <- TRUE
source(here("data_processing_scripts/process/process.R"))


# list of excluded organisations (no consent)
list_of_refusals <- c(
  "Caledonian Maritime Assets Ltd",
  "Crown Estate Scotland",
  "National Museums of Scotland",
  "Scottish Parliamentary Corporate Body"
) %>% str_trim()

# orgs we're pre-selecting for exclusion but have their consent -
# exclusions based on e.g. data isssues, were previously secondary suppressed
other_exclusions <- c("Scottish Social Services Council",
  #"Audit Scotland", 
  "Edinburgh Tours"
  )

full_removals <- c(list_of_refusals, other_exclusions)


removed_data <-pse_org2 |> 
  filter(updated_name %in% full_removals|Organisation %in% full_removals)
# ==============================================================================
# Remove refused
# ==============================================================================
pse_consent <- pse_org2 |>
  anti_join(
   removed_data,
    by = c("updated_name", "Organisation")
  )
excluded <- setdiff(pse_org2, pse_consent)

# add marker to whole dataset
pse_org2 <- pse_org2 |>
  mutate(
    Consent = if_else(
      updated_name %in% list_of_refusals |
        Organisation %in% list_of_refusals,
      "No",
      "Yes"
    ),
    excluded = if_else(
      updated_name %in% full_removals |
        Organisation %in% full_removals,
      "Yes",
      "No"
    )
  )

exclusions_summary <- pse_org2 |> 
  group_by(classification,
           #Consent,
           excluded,
           Variable) |> 
  summarise(count = n()
            )
exclusions <- pse_org2 |> 
  group_by(classification, Variable) |> 
  summarise(total = n()
  )  

exclusions_summary <- left_join(exclusions_summary, exclusions, by = c("classification", "Variable")) |> 
  mutate(percent = (count/total*100))

exclusions_summary <- pse_org2 |> 
  group_by(Consent, excluded, Variable) |> 
  summarise(count = n()
  )
exclusions <- pse_org2 |> 
  group_by(Variable) |> 
  summarise(total = n()
  )  

exclusions_summary <- left_join(exclusions_summary, exclusions, by = c("Variable")) |> 
  mutate(percent = (count/total*100))


excluded_datset_summary <- pse_org2 |> select(Variable, excluded,
                                           all_of(latest_data)) |> 
  group_by(excluded, Variable) |> 
  summarise(count_latest_quarter = sum(across(any_of(latest_data)), na.rm = T
                        
  ))

whole_dataset_summary <- pse_org2 |> select(Variable, 
                                 all_of(latest_data)) |> 
  group_by(Variable) |> 
  summarise(whole_count_latest_quarter = sum(across(any_of(latest_data)), na.rm = T )) 
            
whole_dataset_summary <- left_join(excluded_datset_summary, whole_dataset_summary, by = c("Variable")) |> 
  mutate(percent = (count_latest_quarter/whole_count_latest_quarter*100))



excluded_datset_summary <- pse_org2 |> select(Variable, excluded, classification,
                                              all_of(latest_data)) |>
  group_by(excluded, classification, Variable) |>
  summarise(count_latest_quarter = sum(across(any_of(latest_data)), na.rm = T

  ))

whole_dataset_summary <- pse_org2 |> select(Variable, classification,
                                            all_of(latest_data)) |>
  group_by(Variable, classification) |>
  summarise(whole_count_latest_quarter = sum(across(any_of(latest_data)), na.rm = T ))

whole_dataset_summary <- left_join(excluded_datset_summary, whole_dataset_summary, by = c("classification", "Variable")) |>
  mutate(percent = (count_latest_quarter/whole_count_latest_quarter*100))

# ==============================================================================
# Secondary disclosure
# ==============================================================================
#  To be performed on no consents!
# 1. check if threshold rule is met (number of orgs  per category > 3)

thresh_check <- excluded |>
  group_by(classification, Variable) |> 
  summarise(cell_count = n()) |> 
  mutate(thresh_pass = ifelse(cell_count>=3, TRUE, FALSE))

# check against entire dataset - if total number of exclusions is more than 3

thresh_check_all <- (nrow(excluded) / 2) > 3

print(thresh_check_all)

# 2. p% rule ()

run_p_rule <- function(data,
                       value_col,
                       threshold_n = 3,
                       p = 0.1) {
  
  data |>
    group_by(classification, Variable) |>
    summarise(
      n_contributors = n(),
      A = max(.data[[value_col]], na.rm = TRUE),
      B = if_else(
        n() >= 2,
        sort(.data[[value_col]], decreasing = TRUE)[2],
        NA_real_
      ),
      C = if (n() >= 3)
        sum(sort(.data[[value_col]], decreasing = TRUE)[-(1:2)])
      else 0,
      threshold_pass = n_contributors >= threshold_n,
      p_rule_pass = C > p * A,
      non_disclosive = threshold_pass & p_rule_pass,
      .groups = "drop"
    )
  
}



# 2.1 Dominance check on latest quarter - entire dataset

# first rank EXCLUDED in order of highest to lowest
rank_exc <- excluded |> group_by(Variable) |> 
  arrange(classification, Variable, desc(.data[[latest_data]]))



#then take highest (A) and second highest (B) values and calculate sum of remaining contributors (C).
# if sum (C) > 10% of max value (A) then threshold rule is passed


# Current p% rule results
p_rule_inputs_class <- run_p_rule(
  rank_exc,
  latest_data
)

 # Classifications that still fail
failing_classes <- p_rule_inputs_class |>
  filter(!p_rule_pass)|>
  pull(classification)
# subset data to get all consenting orgs in excluded categories (exclusions are removed)
rank_inc <- pse_consent |> 
  filter(classification %in% excluded$classification) |> 
  group_by(classification, Variable) |> 
  arrange(classification, Variable, desc(.data[[latest_data]]))

# Next smallest contributor not already excluded
next_excluded <- rank_inc |>
  filter(classification %in% failing_classes) |>
  anti_join(
    excluded,
    by = c("classification", "Organisation")
  ) |>
  group_by(classification, Variable) |>
  slice_min( # change to min?
    order_by = .data[[latest_data]],
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup()

# Add next contributor to excluded set
excluded_updated <- bind_rows(
  excluded,
  next_excluded
)

# Re-run p% rule on updated exclusions
p_rule_inputs_class_updated <- 
  run_p_rule(
    excluded_updated,
    latest_data
  )
  
  
excluded_updated <- excluded

repeat {
  
  # Run p% rule on current excluded set
  p_rule_results <- run_p_rule(
    excluded_updated,
    latest_data
  )
    
  # Find classifications that still fail
  failing_classes <- p_rule_results |>
    filter(!p_rule_pass) |>
    pull(classification)
  
  # Stop if everything passes
  if (length(failing_classes) == 0) {
    break
  }
  
  # Find next largest non-excluded contributor
  
  # subset data to get all consenting orgs in excluded categories (exclusions are removed)
  rank_inc <- pse_consent |> 
    filter(classification %in% excluded$classification) |> 
    group_by(classification, Variable) |> 
    arrange(classification, Variable, desc(.data[[latest_data]]))
  
  
  next_excluded <- rank_inc |>
    filter(classification %in% failing_classes) |>
    anti_join(
      excluded_updated,
      by = c("classification", "Organisation")
    ) |>
    group_by(classification, Variable) |>
    slice_min(
      order_by = .data[[latest_data]],
      n = 1,
      with_ties = FALSE
    ) |>
    ungroup()
  
  # Stop if no more contributors available
  if (nrow(next_excluded) == 0) {
    warning("No additional contributors available for exclusion.")
    break
  }
  
  # Add them to the excluded set
  excluded_updated <- bind_rows(
    excluded_updated,
    next_excluded
  ) |> 
    arrange(classification, Variable)
}
excluded_updated
# check how many organisations for each excluded classifications remain
remaining_counts <- pse_org2|>
  anti_join(
    excluded_updated,
    by = c("classification", "Organisation")
  ) |>
  group_by(Variable) |> 
  count(classification, name = "remaining_n")

# Final results
p_rule_results

create_suppression_status <- function(
    p_rule_results,
    remaining_counts,
    threshold_n = 3
) {
  
  p_rule_results |>
    left_join(
      remaining_counts,
      by = c("classification", "Variable")
    ) |>
    mutate(
      remaining_n = coalesce(remaining_n, 0L),
      
      can_continue  = # can i suppress another org?
        !p_rule_pass &
        remaining_n > 0,
      
      manual_review = # have I run out of orgs to suppress?
        !p_rule_pass &
        remaining_n == 0,
      
      threshold_breach = # does the residual cell have <3 contributors?
        remaining_n < threshold_n
    )
  
}

status <- create_suppression_status(
  p_rule_results,
  remaining_counts
)


status
# get suppressed dataset after applying secondary disclosure

suppressed_data <- pse_org2 |> filter(!Organisation %in% excluded_updated$Organisation) |> 
  arrange(classification, Variable)


# now run  p% on the newly suppressed dataset:

supp_p_rule <- run_p_rule(
  suppressed_data,
  latest_data
)



second_suppressed <- setdiff(excluded_updated, removed_data) # additional orgs that will be secondary suppressed (exclusive of orgs listed in other exclusions)

# ==============================================================================
# Export - for scenario mapping 
# ==============================================================================
# df_list <-
#     list(
#       "full_data" = pse_org2,
#       "refusals" = list_of_refusals,
#       "other_exclusions" = other_exclusions,
#       "total_inclusions" = pse_consent,
#       "total_exclusions" = excluded,
#       "exclusions_summary" = exclusions_summary,
#       "p_rule_results_exclusions" = status,
#       "excluded_dataset" = excluded_updated,
#       "data_suppressed" = suppressed_data,
#       "p_rule_suppressed_data" = supp_p_rule
# 
#     )
#  
# 
# wb <- createWorkbook()
# 
# 
# for (sheet_name in names(df_list)) {
# 
#   df <- df_list[[sheet_name]]
# 
#   # Convert non-data.frames
#   if (!is.data.frame(df)) {
#     df <- data.frame(Organisation = as.character(df))
#   }
# 
#   addWorksheet(wb, sheet_name)
# 
#   writeData(
#     wb,
#     sheet = sheet_name,
#     x = df
#   )
# 
#   setColWidths(
#     wb,
#     sheet = sheet_name,
#     cols = seq_len(ncol(df)),
#     widths = "auto"
#   )
# }
#   saveWorkbook(
#     wb,
#     file.path(
#       adm_source_path,
#       "code outputs/disclosure",
#       paste0(
#         "disclosure_results_",
#         format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
#         ".xlsx"
#       )
#     ),
#     overwrite = FALSE
# #   )
# ==============================================================================
# Export - dataset for exchequer (NB full datasets - includes refusals )
# ==============================================================================

# 
# 

# workbook for exchequer analysts
# tidy names
row_order <- c("Devolved Civil Service", 
               "Further Education Colleges",
               "Local Government",
               "NHS",
               "Other Public Bodies",
               "Police and Fire Services",
               "Public Corporations")

exchequer_pse <- pse_org2 |> 
  mutate(Organisation = ifelse(is.na(updated_name), Organisation, updated_name),
         excluded = case_when(Organisation %in% excluded_updated$Organisation ~ "Yes",
                              Organisation %in% excluded_updated$updated_name ~ "Yes",
         .default = "No")) |> 
  arrange(match(classification, row_order), Organisation) |> 
  select(
         Organisation, Consent, excluded,
         `Primary classification` = classification,
         `Secondary classification` =classification2,
         Variable, everything()) |> 
  select(-updated_name, -Source) |> 
  mutate(across(where(is.numeric),~ ifelse( is.na(.x), 0, .x)  )) 

# rounding 
exchequer_suppressed<-exchequer_pse %>% 
  mutate(across(where(is.numeric),~ ifelse( is.na(.x), 0, .x)  ))%>%  # replace missing with 0 e.g. no data at that point
  mutate(across(where(is.numeric),~ ifelse( .x < 5 & .x > 0 , NA_integer_, .x)  )) |> # make low value (les than 5) missing - suppressing
  mutate(across(where(is.numeric), ~round(.x/5)*5)) 
 # mutate(across(where(is.numeric), ~ifelse(.x<0, "[Low]", .x))) |>
 

# Create workbook and worksheet


#  list of data frames
df_list <- list(
  full_data_q12026 = exchequer_pse,
  suppressed_rounded_q12026 = exchequer_suppressed
)

wb <- createWorkbook()

for (sheet_name in names(df_list)) {
  
  df <- df_list[[sheet_name]]
  
  addWorksheet(wb, sheet_name)
  
  if (sheet_name == "suppressed_rounded_q12026") {
    
    note_text <- paste(
      "Note: Blank cells indicate values have been suppressed due to counts fewer than 5.",
      "Cells containing 0 indicate zero FTE or headcount OR data not available for that period.",
      "All values have been rounded to the nearest 5."
    )
    
  } else {
    
    note_text <- "Note: No suppression or rounding has been applied to the data."
    
  }
  
  writeData(
    wb,
    sheet = sheet_name,
    x = note_text,
    startRow = 1,
    colNames = FALSE
  )
  
  note_style <- createStyle(
    textDecoration = "bold",
    wrapText = TRUE
  )
  
  addStyle(
    wb,
    sheet = sheet_name,
    style = note_style,
    rows = 1,
    cols = 1
  )
  
  setRowHeights(
    wb,
    sheet = sheet_name,
    rows = 1,
    heights = 45
  )
  
  writeData(
    wb,
    sheet = sheet_name,
    x = df,
    startRow = 3
  )
  
  setColWidths(
    wb,
    sheet = sheet_name,
    cols = seq_len(ncol(df)),
    widths = "auto"
  )
}
#saveWorkbook(wb, paste0(adm_source_path, "code outputs/exchequer_psebyorg_q126.xlsx"), overwrite = TRUE)

# ==============================================================================
# Export for publication -  Remove refusals, apply secondary suppression NA and low markers to dataset
# ==============================================================================
#list of organisations with <5 FTE/HC: for adding notes to publication table later
list_of_low <- exchequer_suppressed |>
  filter(if_any(contains("Quarter"), is.na))

list_of_low <- unique(list_of_low$Organisation)

publication_dataset <- exchequer_suppressed |>
  filter(Consent == "Yes") |> # remove no consents
  filter(excluded == "No") |> # remove excluded orgs- wont appear in table at all.  
  mutate(
    across(
      contains("Quarter"),
      ~ case_when(
        Organisation %in% second_suppressed$Organisation|
          Organisation %in% second_suppressed$updated_name|
          Organisation %in% other_exclusions ~ "[c]",# secondary suppress - this won't be relevant as filtered out aboved
        .x == 0 ~ "[z]",        # organisation didn't exist
        is.na(.x) ~ "[low]",    # count lower than five
        TRUE ~ as.character(.x)
      )
    )
  ) |> 
  arrange(match(`Primary classification`, row_order),`Secondary classification`) 

