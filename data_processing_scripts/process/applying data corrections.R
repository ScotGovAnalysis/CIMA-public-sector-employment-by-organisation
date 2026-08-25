
# ==============================================================================
# Script Name : q126 manual data edits.R
# Purpose     : remove disputed data from q126 dataset. This is in response to organisations wanting to make corrections. 
                # will remove data  
# Author      : Jackie
# Created     : YYYY-MM-DD
# Updated     : YYYY-MM-DD
# Version     : 1.0.0
# ==============================================================================


# ==============================================================================
# PACKAGES
# ==============================================================================

# Load required packages
library(here)
library(openxlsx2)
library(tidyverse)
# ==============================================================================
# SETUP
# ==============================================================================
source(here("data_processing_scripts/process/disclosure_control.R")) # import data
# note: change variables such as publication date in setup/config.r script

source(here("publication/functions.R"))



# import corrections spreadsheet ----
# nb correcitons wil be marked as 0 - then overwritten in shorthand for final tables
# corrections are from organisations replying to mail merge - cannot correct data - they need to submit to ONS etc. 
# so replacing values with not available shorthand
corrections <-
  read_xlsx(
    path = paste0(
      adm_source_path,
      "code inputs/corrected data from organisations q126.xlsx"
    ),
    sheet = "Sheet2"
  ) 



corrections_x <- corrections|>
  mutate(
    across(
      contains("Quarter"),
      ~ case_when(.x == 0 ~ "[x]",
                  .default = as.character(.x)) # change to character ahead of join
    )
  )
    
  
# ==============================================================================
# Update publication dataset with corrections
# ==============================================================================

# join key
keys <- c("Organisation", "Variable")

# cols to update
update_cols <- intersect(
  setdiff(names(publication_dataset), keys),
  setdiff(names(corrections_x), keys)
)

# update publication dataset with "corrected" data
updated <- publication_dataset %>%
  left_join(
    corrections_x %>%
      rename_with(~ paste0(.x, "_new"), all_of(update_cols)),
    by = keys
  )

updated[update_cols] <- Map(
  coalesce,
  updated[paste0(update_cols, "_new")],
  updated[update_cols]
)

updated <- updated %>%
  select(names(publication_dataset))

# check update was successful
check <- setdiff(updated, publication_dataset)

# replace 0 with [z] etc
publication_dataset <- updated |>  mutate(
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

check <- check |> arrange(match(`Primary classification`, row_order),`Secondary classification`) 
  
corrected_orgs <- unique(check$Organisation) # for publication table notes
# ==============================================================================
# Update datasets for exchequer with corrections
# ==============================================================================

corrections_num <- corrections |> mutate(
  across(
    contains("Quarter"),
    ~ as.numeric(.x) # change to numeric ahead of join
  )
)

# unrounded, low values not suppressed, exclusions removed
exchequer_pse_updated <- exchequer_pse |> 
  left_join(
    corrections_num %>%
      rename_with(~ paste0(.x, "_new"), all_of(update_cols)),
    by = keys
  )

exchequer_pse_updated[update_cols] <- Map(
  coalesce,
  exchequer_pse_updated[paste0(update_cols, "_new")],
  exchequer_pse_updated[update_cols]
)

exchequer_pse_updated <- exchequer_pse_updated %>%
  select(names(exchequer_pse))|> 
  filter(excluded == "No") # remove exclusions


# check update was successful
check <- setdiff(exchequer_pse_updated, exchequer_pse)


# rounded and suppressed e.g. for figures>5 blank cell:
check_prior_zero<- exchequer_suppressed |>
  filter(if_any(contains("Quarter"), ~ .x == 0))

exchequer_suppressed_updated <- exchequer_suppressed |> 
  left_join(
    corrections_num %>%
      rename_with(~ paste0(.x, "_new"), all_of(update_cols)),
    by = keys
  )

exchequer_suppressed_updated[update_cols] <- Map(
  coalesce,
  exchequer_suppressed_updated[paste0(update_cols, "_new")],
  exchequer_suppressed_updated[update_cols]
)

exchequer_suppressed_updated <- exchequer_suppressed_updated %>%
  select(names(exchequer_suppressed)) |> 
  filter(excluded == "No") # remove exclusions


# check update was successful
check <- setdiff(exchequer_suppressed_updated, exchequer_suppressed)


# Create workbook and worksheet


#  list of data frames
df_list <- list(
  full_data_q12026 = exchequer_pse_updated,
  suppressed_rounded_q12026 = exchequer_suppressed_updated
)

wb <- createWorkbook()

for (sheet_name in names(df_list)) {
  
  df <- df_list[[sheet_name]]
  
  addWorksheet(wb, sheet_name)
  
  if (sheet_name == "suppressed_rounded_q12026") {
    
    note_text <- paste(
      "Note: Blank cells indicate values have been suppressed due to counts fewer than 5.",
      "Cells containing 0 indicate zero FTE or headcount OR data not available for that period.",
      "All values have been rounded to the nearest 5.",
      "All excluded organisations are removed."
    )
    
  } else {
    
    note_text <- "Note: No rounding has been applied to the data. Excluded and secondary suppressed organisations are removed"
    
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
    startRow = 4
  )
  
  setColWidths(
    wb,
    sheet = sheet_name,
    cols = seq_len(ncol(df)),
    widths = "auto"
  )
}

saveWorkbook(wb, paste0(adm_source_path, "code outputs/exchequer_psebyorg_q126.xlsx"), overwrite = TRUE) 

# ==============================================================================
# Export final datasets to ADM
# ==============================================================================
# write_dataframe_to_db(server = server,
#                       database = database,
#                       schema = schema,
#                       table_name = paste( "final_dataset", latest_data_short, sep = "_"),
#                       publication_dataset
#                       )
# 
# write_dataframe_to_db(server = server,
#                       database = database,
#                       schema = schema,
#                       table_name = paste( "final_full_dataset_including_exclusions", latest_data_short, sep = "_"),
#                       exchequer_pse_updated
# )

# save corrected data only as indivdual dataframe -to share with orgs
check <- check |> select(-Consent, -excluded) |> 
  mutate(
    across(
      contains("Quarter"),
      ~ case_when(.x == 0 ~ "[x]",
                  .default = as.character(.x)) # change to character ahead of join
    )
  )
org_dfs <- split(check, check$Organisation) 




for (org in names(org_dfs)) {
  
  df <- org_dfs[[org]]
  
  # Clean organisation name for use in file names
  safe_name <- gsub("[^[:alnum:]_ -]", "", org)
  
  wb <- createWorkbook()
  addWorksheet(wb, "Data")
  
  writeData(wb, "Data", df)
  

  # Write note 2 rows below table
  note_row <- nrow(df) + 3
  
  writeData(
    wb,
    sheet = "Data",
    x = "Note: [x] indicates FTE and/or headcount data are not available for these periods as data have been identified as incorrect. Revised figures will be included in future publications.",
    startRow = note_row,
    startCol = 1
  )
  
  # Auto-fit column widths
  setColWidths(
    wb,
    sheet = "Data",
    cols = 1:ncol(df),
    widths = "auto"
  )
  
  saveWorkbook(
    wb,
    file = paste0(adm_source_path, "mail merge/data/corrected data/PSE_employment_statistics_2026_Q1_corrected", safe_name, ".xlsx"),
    overwrite = TRUE
  )
}

