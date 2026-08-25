
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
latest <- TRUE
source(here("data_processing_scripts/process/process.R"))


#save as a new name 
new_pse <- pse_org2


# change input workbook and rerun to get previous years 
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
)
# ==============================================================================
# Calculate changes 
# ==============================================================================


changes <- old_pse %>%
  full_join(
    new_pse,
    by = c("Source", "Organisation", "Variable", "updated_name", "classification", "classification2"),
    suffix = c("_old", "_new")
  ) %>%
  pivot_longer(
    cols = -c(Source, Organisation, Variable, updated_name, classification, classification2),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(old|new)"
  ) %>%
  filter(old != new) |> # remove cols that haven't changed
  mutate(diff = round(new-old))


# ==============================================================================
# Export
# ==============================================================================

write_xlsx(
 changes,
  path = paste0(
    adm_source_path,
    "code outputs/QA check against last PSE org data ",
    Sys.Date(),
    ".xlsx"
  )
)

