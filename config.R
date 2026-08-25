
# ==============================================================================
# Script Name : config_.R
# Purpose     : set up parameters and packages e.g. quarters
# Author      : Jackie
# Created     : YYYY-MM-DD
# Updated     : YYYY-MM-DD
# Version     : 1.0.0
# ==============================================================================


# ==============================================================================
# PACKAGES
# ==============================================================================

# Load required packages
library(RtoSQLServer)

# Optional package checks
required_packages <- c(
  "dplyr",
  "stringr",
  "glue"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
``
# ==============================================================================
#  config variables 
# ==============================================================================

#latest_data <- "2026 Quarter 1"

# set latest quarter and  previous quarter (for QA checks scripts)
if (exists("QA_checks") && QA_checks == "QA"
    && latest == FALSE) {
  latest_data <- "2025 Quarter 3"
} else {
  latest_data <- "2026 Quarter 1"
}
# for QA scripts


latest_data_short <- gsub("Quarter ", "Q", latest_data )

latest_quarter_title <- "1st Quarter 2026"
latest_q_month <- "March"
publication_date <- "20 August 2026"

# ==============================================================================
# ADM data load set up
# ==============================================================================
#Set database connection details for use in functions:

server <- Sys.getenv("ADM_SERVER")
database <- Sys.getenv("ADM_DATABASE")
schema <- Sys.getenv("ADM_SCHEMA")
adm_source_path <- Sys.getenv("ADM_SOURCE_PATH")


# show_schema_tables(
#   server = server,
#   database = database,
#   schema = schema,
#   include_views = TRUE
# )
ocea_data_import_path <- if (latest) {
  Sys.getenv("OCEA_IMPORT_PATH_LATEST")
} else {
  Sys.getenv("OCEA_IMPORT_PATH_PREVIOUS")
}
# adm_source_path <- "//s0196a/ADM-Strategy and External Affairs-Public Sector Employment - Public Bodies/Public Bodies Quarterly Employment Data/Source/Q1 2026 publication/"
# 
# ocea_data_import_path <- ifelse(latest == TRUE, "C:/Users/U456727/OneDrive - SCOTS Connect/Analytical Data Management - Public Bodies Quarterly Employment Data/Data in from OCEA/Q1 2026/", # latest
#                                 "C:/Users/U456727/OneDrive - SCOTS Connect/Analytical Data Management - Public Bodies Quarterly Employment Data/Data in from OCEA/Q3 2025/") # prev
                                
# ocea_workbook_name = ifelse(latest == TRUE, "Devolved Public Sector Employment data for individual organisations - Q1 2019 to Q1 2026.xlsx", #latest
#                             "Devolved Public Sector Employment data for individual organisations - Q1 2019 onwards - formatted.xlsx") # prev

ocea_data_workbook_name <- if (latest) {
  Sys.getenv("OCEA_WORKBOOK_NAME_LATEST")
} else {
  Sys.getenv("OCEA_WORKBOOK_NAME_PREVIOUS")
}

ocea_sheets <-c(
    "JSW Headcount",
    "JSW FTE",
    #"Simplification return",
    "Civil Service Units",
    "Colleges",
    "Public Bodies",
    "Public Bodies 2",
    "SG collected"
  )

publication_adm_path_import <- Sys.getenv("PUBLICATION_ADM_IMPORT_PATH")

lookup_workbook <- Sys.getenv("LOOKUP_WORKBOOK")
lookup_sheets <- c("organisations",
                   "period format",
                   "orgs in OCEA data",
                   "variables in OCEA data",
                   "last lookup"
                   )
nhs_data_export <- Sys.getenv("NHS_DATA_EXPORT")
nhs_sheets <- c("Headcount",
                "FTE"
)

publication_adm_path_qa <- Sys.getenv("PUBLICATION_ADM_PATH_QA")
qa_workbook <- Sys.getenv("QA_WORKBOOK")
qa_sheets <- c("Table_3", "Table_10")

