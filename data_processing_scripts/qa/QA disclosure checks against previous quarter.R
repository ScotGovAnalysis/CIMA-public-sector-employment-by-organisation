
# ==============================================================================
# Script Name : QA checks against previous quarter .R
# Purpose     : QA against previous data  - checking revisions 
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
library(writexl)
library(janitor)
# ==============================================================================
# IMPORT processed new data and old
# ==============================================================================

source(here("data_processing_scripts/process/disclosure_control.R"))
source(here("data_processing_scripts/qa/functions.R"))


#save as a new name 
new_pse <- exchequer_suppressed |>
  filter(excluded == "No") |>
  mutate(across(contains("Quarter"), as.numeric))


# Get previous years 
#note for q32025 no nhs data export - will be same as q126
QA_checks <- "QA"
latest <- FALSE 

source(here("data_processing_scripts/process/process.R"))

old_pse <- pse_org2

# list of excluded organisations - last time 
list_of_exclusions <- c(
  "Caledonian Maritime Assets Ltd",
  "Crown Estate Scotland",
  "National Museums of Scotland",
  "Scottish Parliamentary Corporate Body",
   "Scottish Social Services Council",
  "Audit Scotland",
    "Redress Scotland",
  "Edinburgh Tours"
)

old_pse <- old_pse |> mutate(excluded_last_time =
  case_when(Organisation %in% list_of_exclusions ~ "TRUE",
            updated_name %in% list_of_exclusions ~ "TRUE",
            .default = "FALSE")
) |> 
  filter(excluded_last_time == "FALSE") # remove excluded orgs

# keep excluded orgs values for checks

old_exclusions <- pse_org2 |> 
  filter(Organisation %in% list_of_exclusions | updated_name %in% list_of_exclusions)


new_exclusions <- excluded_updated

#what's changed - new vs old exclusions
exclusions_change <- setdiff(old_exclusions,new_exclusions[, names(new_exclusions) %in% names(old_exclusions)])
# ==============================================================================
# Apply rounding
# ==============================================================================

old_pse<-old_pse %>% 
  mutate(across(where(is.numeric),~ ifelse( is.na(.x), 0, .x)  ))%>%  # replace missing with 0 e.g. no data at that point
  mutate(across(where(is.numeric),~ ifelse( .x < 5 & .x > 0 , NA_integer_, .x)  )) |> # make low value (les than 5) missing - suppressing
  mutate(across(where(is.numeric), ~round(.x/5)*5)) |> 
  rename (`Primary classification` = classification)

old_pse_fte <- old_pse |> 
  filter(Variable == "FTE Total")

old_pse_hc <- old_pse |> 
  filter(Variable == "HC Total")

new_pse <- new_pse |> 
  mutate(across(where(is.numeric),~ ifelse( is.na(.x), 0, .x)  ))%>%  # replace missing with 0 e.g. no data at that point
  mutate(across(where(is.numeric),~ ifelse( .x < 5 & .x > 0 , NA_integer_, .x)  )) |> # make low value (les than 5) missing - suppressing
  mutate(across(where(is.numeric), ~round(.x/5)*5)) 

new_pse_fte <- new_pse |> 
  filter(Variable == "FTE Total")

new_pse_hc <- new_pse |> 
  filter(Variable == "HC Total")
# ==============================================================================
# Get classification totals
# ==============================================================================
cat_totals_old <- old_pse|>
  group_by(`Primary classification`, Variable) |>
  summarise(
    across(
      contains("Quarter"),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# split for each variable 
hc_tot_old <- cat_totals_old |> 
  filter(Variable == "HC Total") |> 
  select(-Variable)

fte_tot_old <- cat_totals_old |> 
  filter(Variable == "FTE Total") |> 
  select(-Variable)


# repeat for new data
cat_totals_new <- new_pse|>
  group_by(`Primary classification`, Variable) |>
  summarise(
    across(
      contains("Quarter"),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# split for each variable 
hc_tot_new <- cat_totals_new|> 
  filter(Variable == "HC Total") |> 
  select(-Variable)

fte_tot_new <- cat_totals_new |> 
  filter(Variable == "FTE Total") |> 
  select(-Variable)

# ==============================================================================
# Get total devolved public sector
# ==============================================================================
#calculate devolved totals
fte_all_old <- 
  fte_tot_old |> 
  summarise(
    across(
      contains("Quarter"),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |> 
  mutate(`Primary classification` = "Total Devolved Public Sector")
  
hc_all_old <- 
  hc_tot_old |> 
  summarise(
    across(
      contains("Quarter"),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |> 
  mutate(`Primary classification` = "Total Devolved Public Sector")

# bind

fte_old <- rbind(fte_all_old, fte_tot_old) |> 
  select(`Primary classification`, everything ())

hc_old <- rbind(hc_all_old, hc_tot_old) |> 
  select(`Primary classification`, everything ())

fte_all_new <- 
  fte_tot_new |> 
  summarise(
    across(
      contains("Quarter"),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |> 
  mutate(`Primary classification` = "Total Devolved Public Sector")


hc_all_new <- 
  hc_tot_new |> 
  summarise(
    across(
      contains("Quarter"),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |> 
  mutate(`Primary classification` = "Total Devolved Public Sector")

fte_new <- rbind(fte_all_new, fte_tot_new) |> 
  select(`Primary classification`, everything ())

hc_new <- rbind(hc_all_new, hc_tot_new) |> 
  select(`Primary classification`, everything ())
# ==============================================================================
# Export
# ==============================================================================



df_list <- 
  list(
    "old_published_data " = old_pse,
    "new_unpublished_data"   = new_pse,
    "old_exclusions" = old_exclusions,
    "new_exclusions"   = new_exclusions,
    "exclusions_change" = exclusions_change,
    "old_category_total_hc" = hc_old,
    "new_category_totals_hc" = hc_new,
    "old_category_totals_fte" = fte_old,
    "new_category_totals_fte" = fte_new
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
    paste0( "code outputs/QA disclosure check against last PSE org data ",
           format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
           ".xlsx"
    )
  ),
  overwrite = FALSE
)






