
# ==============================================================================
# Script Name : disclosure_check against main PSE.R
# Purpose     : check suppressed data against aggregate published PSE
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
library(waldo)
# ==============================================================================
# SETUP
# ==============================================================================
source(here("data_processing_scripts/qa/QA checks of raw data against main PSE.R" ))
source(here("data_processing_scripts/process/disclosure_control.R"))
source(here("data_processing_scripts/qa/functions.R"))


# define datasets
# publication_dataset = refusals removed and secondary disclosed data removed as markers - but organisations still included
# exchequer_suppressed = full rounded dataset with consent and excluded included as markers for all organisations. Also includes missing values where data <5
# and 0 where data not available. 


suppressed_data <- exchequer_suppressed |>
  filter(excluded == "No") |>
  mutate(across(contains("Quarter"), as.numeric))

suppressed_fte <- suppressed_data |> 
  filter(Variable == "FTE Total")

suppressed_hc <- suppressed_data |> 
  filter(Variable == "HC Total")
# ==============================================================================
# Calculate totals for categories and total devolved public sector i.e. dataset total 
# ==============================================================================
# get classification totals  
cat_totals <- suppressed_data |>
  group_by(`Primary classification`, Variable) |>
  summarise(
    across(
      contains("Quarter"),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# split for each variable 
hc_tot <- cat_totals |> 
  filter(Variable == "HC Total") |> 
  select(-Variable)

fte_tot <- cat_totals |> 
  filter(Variable == "FTE Total") |> 
  select(-Variable)

# transpose and edit col names to match web tables
hc_tot <- format_totals(hc_tot)
fte_tot <- format_totals(fte_tot)

#calculate devolved totals
fte_all <- create_all_totals(
  data = suppressed_fte ,
  totals_df = fte_tot,
  row_order = row_order,
  round_totals = FALSE
) 

#calculate devolved totals
hc_all <- create_all_totals(
  data = suppressed_hc ,
  totals_df = hc_tot,
  row_order = row_order,
  round_totals = FALSE
)


# ==============================================================================
# Calculate differences between PSE aggregate and suppressed totals
# ==============================================================================


measure_cols <- setdiff(names(published_hc), c("Quarter", "Year"))

compare_hc <- compare_tables(
  agg_data = published_hc,
  disagg_data = hc_all
)

compare_fte <- compare_tables(
  agg_data = published_fte,
  disagg_data = fte_all
)

# format excluded orgs in same way for manual comparison


excluded_orgs <- excluded_updated |> 
  select(Organisation, Variable, classification, contains("Quarter"))|>
  group_by (classification, Variable)

excluded_orgs_hc <- excluded_orgs |> 
  filter(Variable == "HC Total")

excluded_orgs_fte <- excluded_orgs |> 
  filter(Variable == "FTE Total")

excluded_list <- excluded_orgs |> 
  select(Organisation, Variable, classification) 

excluded_orgs_fte <- format_excluded_totals(excluded_orgs_fte) |> 
  mutate(across(!Quarter, as.numeric))
excluded_orgs_hc <- format_excluded_totals(excluded_orgs_hc) |> 
  mutate(across(!Quarter, as.numeric))

compare_diff_fte <- compare_fte |> 
  select(Quarter, Year, contains("_diff"))

compare_diff_hc <- compare_hc |> 
  select(Quarter, Year, contains("_diff"))





excluded_totals <- excluded_orgs |> 
  group_by(classification, Variable) |>
  summarise(
    across(
      contains("Quarter"),
      ~ sum(.x, na.rm = TRUE)
    )
  ) 

excluded_total_fte <- excluded_totals|> 
  filter(Variable == "FTE Total") |> 
  format_excluded_totals()|> 
  mutate(across(!Quarter, as.numeric))

excluded_total_hc <- excluded_totals|> 
  filter(Variable == "HC Total") |> 
  format_excluded_totals()|> 
  mutate(across(!Quarter, as.numeric))


# ==============================================================================
# Export
# ==============================================================================

df_list <- 
  list(
    # "full_data" = pse_org2,
    "Unpublished supp HC" = hc_all,
    "Published HC"   = published_hc,
    "Unpublished supp FTE" = fte_all,
    "Published FTE"   = published_fte,
    "comparison_HC" = compare_hc,
    "differences_fte" = compare_diff_fte,
    "differences_hc" = compare_diff_hc,
    "comparison_FTE" = compare_fte,
    "excluded_list" = excluded_list,
    "exclusions_fte" = excluded_orgs_fte,
    "exclusions_hc" = excluded_orgs_hc,
    "exclusions_totals_fte" = excluded_total_fte,
    "exclusions_total_hc" = excluded_total_hc
  
  )
wb <- createWorkbook()


for (sheet_name in names(df_list)) {
  
  df <- df_list[[sheet_name]]
  
  # Convert non-data.frames
  if (!is.data.frame(df)) {
    df <- data.frame(Organisation = as.character(df))
  }
  
  addWorksheet(wb, sheet_name)
  
  writeData(
    wb,
    sheet = sheet_name,
    x = df
  )
  
  setColWidths(
    wb,
    sheet = sheet_name,
    cols = seq_len(ncol(df)),
    widths = "auto"
  )
}
saveWorkbook(
  wb,
  file.path(
    adm_source_path,
    paste0("code outputs/disclosure/QA check of suppressed data against main PSE ",
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
      ".xlsx"
    )
  ),
  overwrite = FALSE
)



