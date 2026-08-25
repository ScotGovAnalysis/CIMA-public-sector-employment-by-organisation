
# ==============================================================================
# Script Name : create publication tables.R
# Purpose     : create publication tables
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
library(aftables)
library(openxlsx2)
library(tidyverse)
# ==============================================================================
# SETUP
# ==============================================================================
source(here("data_processing_scripts/process/applying data corrections.R")) # import data
# note: change variables such as publication date in setup/config.r script


source(here("publication/format main PSE for publication.R"))# import totals (aggregate PSE publication)
source(here("publication/functions.R"))

# notes for tables
table_notes <-  data.frame(
  "Organisation" = c(corrected_orgs[1],
                     "UHI North, West and Hebrides",
                     corrected_orgs[-1],
                     list_of_low
                     ),
  
  "Notes" = c("[note 2] This note applies to cell P44",
              "[note 3] This note applies to cells C55:U55",
              "[note 2] This note applies to cell X86",
              "[note 2] This note applies to cells E151 and G151",
              "[note 2] This note applies to cells C160:N160",
              "[note 2] This note applies to cells C183:K183",
              "[note 4] This note applies to cells C141:I141 and U141:AE141",
              "[note 4] This note applies to cells M155:N155",
              "[note 4] This note applies to cells C178:AD178"),
              
  check.names = FALSE
)


# ==============================================================================
# Prep tables - join agg totals and add notes
# ==============================================================================

# remove secondary classification col and rename primary

publication_dataset <- publication_dataset |> 
  select(-`Secondary classification`) |> 
  rename(Classification = `Primary classification`)

total_fte <- total_fte |> select(-`Secondary classification`) |> 
  rename(Classification = `Primary classification`)


total_hc <- total_hc |> select(-`Secondary classification`) |> 
  rename(Classification = `Primary classification`)


# split into two tables fte/hc
pub_fte <- publication_dataset |> filter(Variable == "FTE Total") |> 
 select(-Consent, -Variable, -excluded) 

# match row order  
total_fte <- total_fte |>
  arrange(
    desc(is.na(match(Classification, pub_fte$Classification))),
    match(Classification, pub_fte$Classification)
  )

# bind totals with org data 
pub_fte <- rbind(total_fte, pub_fte) 

pub_fte <- left_join(pub_fte, table_notes, by = "Organisation") |> 
  select(Organisation,
         Classification,
         everything(),
         Notes)

pub_hc <- publication_dataset |> filter (Variable == "HC Total") |> 
  select(-Consent, -Variable, -excluded)


# match row order  
total_hc <- total_hc |>
  arrange(
    desc(is.na(match(Classification, pub_hc$Classification))),
    match(Classification, pub_hc$Classification)
  )

# bind totals with org data 
pub_hc <- rbind(total_hc, pub_hc)



pub_hc <- left_join(pub_hc, table_notes, by = "Organisation") |> 
  select(Organisation,
       Classification,
         everything(),
         Notes)


cover_list <- list(
  "Publication description" = c(paste0("The tables in this workbook present employment figures (headcount and full-time equivalent) for a subset of organisations",
  " in the devolved public sector in Scotland, on a quarterly basis from 2019 Quarter 1 to ", latest_data, "."),
  paste0("Data are consistent at the level of individual organisation with those used to produce the Accredited Official Statistics publication",
  " Public Sector Employment in Scotland for ", latest_quarter_title, ".")),
  "Data source" = paste("The statistics in this workbook are based on administrative records and surveys of individual public sector bodies carried out by the",
                        "Scottish Government and the Office for National Statistics (ONS)."),
  "Time period" = c(paste0("Data covers the calender quarters 2019 Quarter 1 (March) to ", latest_data, " (", latest_q_month, ")."),
                    "Calendar quarters are: Quarter 1 (March), Quarter 2 (June), Quarter 3 (September), Quarter 4 (December)."),
  "Publication date" = paste("This spreadsheet was published on", publication_date),
  "Key information" =read_markdown_for_spreadsheet(here("publication/cover sheet_key information.md")),
  "Further information" = c("Background and quality information is available at:",
    "[About public sector employment in Scotland: employment by organisation](https://www.gov.scot/publications/about-public-sector-employment-by-organisation)",
    "More information on Public Sector Employment in Scotland statistics is available at:",
    "[About public sector employment statistics](https://www.gov.scot/publications/about-public-sector-employment-statistics/)"
  ),
  "Contact" = c("Scottish Government Central Analysis Division",
                "[CIMA@gov.scot](mailto:cima@gov.scot)")
  
)

contents_df <- data.frame(
  "Sheet name" = c("Notes", "Table_1", "Table_2"),
  "Sheet title" = c(
    "Notes used in this workbook",
    "Devolved public sector employment by organisation, headcount",
    "Devolved public sector employment by organisation, full-time equivalent (FTE)"
  ),
  check.names = FALSE
)


notes_df <- data.frame(
 "Note number" = paste0("[note ", 1:4, "]"),
  "Note text" = read_markdown_for_spreadsheet(here("publication/notes sheet.md")),
  check.names = FALSE
)





my_aftable <- aftables::create_aftable(
  tab_titles = c("Cover", "Contents", "Notes", "Table_1", "Table_2"),
  sheet_types = c("cover", "contents", "notes", "tables", "tables"),
  sheet_titles = c(
    paste("Public sector employment in Scotland: employment by organisation for", latest_quarter_title),
    "Table of contents",
    "Notes",
    "Table 1: Devolved public sector employment by organisation, headcount [note 1]",
    "Table 2: Devolved public sector employment by organisation, full-time equivalent (FTE) [note 1]"
  ),
  blank_cells = c(
    rep(NA_character_, 5)
  ),
  custom_rows = list(
    NA_character_,
    NA_character_,
    NA_character_,
    c(
      #"This worksheet contains one table.",
      "When notes are mentioned the note marker is presented in square brackets.",
      "Blank cells in the notes column indicate that there's no note in that row.",
      "Some shorthand has been used in this table: [x] = data not available, [z] = not applicable - organisation did not exist at this period  or organisation was classified under a different category.", 
      "Not seasonally adjusted."),
    c(
      #"This worksheet contains one table.",
      "When notes are mentioned the note marker is presented in square brackets.",
      "Blank cells in the notes column indicate that there's no note in that row.",
      "Some shorthand has been used in this table: [x] = data not available, [z] = not applicable - organisation did not exist at this period  or organisation was classified under a different category.", 
      "Not seasonally adjusted.")),
    
  sources = c(
    rep(NA_character_, 3),
    rep("[Public sector employment in Scotland: employment by organisation](https://www.gov.scot/collections/public-sector-employment-by-organisation-statistics/)",2)), # change link to publication url!!
  tables = list(cover_list, contents_df, notes_df, pub_hc, pub_fte)
)




pse_by_org_wb <- aftables::generate_workbook(my_aftable)




# formatting


# -----------------------------
# Set column widths
# -----------------------------


widths <- list(
  Cover = list(
    cols = 1,
    widths = 120
  ),
  Contents = list(
    cols = 2,
    widths = 105
  ),
  
  Notes = list(
    cols = 2,
    widths = 105
  ),
  
  Table_1 = list(
    cols = 1:ncol(pub_hc),
    widths = c(52, 28, rep(8.5, ncol(pub_hc) - 3), 65)
  ),
  Table_2 = list(
    cols = 1:ncol(pub_fte),
    widths = c(52, 28, rep(8.5, ncol(pub_fte) - 3), 65)
  )
)

for (sheet_name in names(widths)) {
  setColWidths(
    pse_by_org_wb ,
    sheet = sheet_name,
    cols = widths[[sheet_name]]$cols,
    widths = widths[[sheet_name]]$widths
  )

  
}
  
  # -----------------------------
  # Set row heights
  # -----------------------------

setRowHeights(pse_by_org_wb, "Table_1", rows = c(9: nrow(pub_hc)+9),  heights = 15)
setRowHeights(pse_by_org_wb, "Table_1", rows = c(10, 11, 18, 39,58,106,128,184,187),  heights = 25)
setRowHeights(pse_by_org_wb, "Table_2", rows = c(9: nrow(pub_hc)+9),  heights = 15)  
setRowHeights(pse_by_org_wb, "Table_2", rows = c(10, 11, 18, 39,58,106,128,184,187), heights = 25)
  
# -----------------------------
# Remove gridlines
# -----------------------------

sheets <- c("Cover",
            "Contents",
            "Notes",
            "Table_1",
            "Table_2")

for (s in sheets) {
  showGridLines(pse_by_org_wb , sheet = s, showGridLines = FALSE)
}


# -----------------------------
# Overwrite num cols
# -----------------------------

overwrite_num_cols(excel_wb = pse_by_org_wb, sheet = "Table_1", cols =c(4:ncol(pub_hc)-1), rows = c(10:190), df = pub_hc)

overwrite_num_cols(excel_wb = pse_by_org_wb, sheet = "Table_2", cols =c(4:ncol(pub_fte)-1), rows = c(10:190), df = pub_fte)


# -----------------------------
# make totals bold
# -----------------------------

sheets <- c(
  "Table_1",
  "Table_2")
for (s in sheets) {
  addStyle(
    pse_by_org_wb,
    sheet = s,
    style = createStyle(textDecoration = "bold"),
    rows = 10:17,
    cols = 1:32,
    gridExpand = TRUE
  )
}
# ----------


# -----------------------------
# Add border around a table
# -----------------------------
# 
# Define a border style
border_style <- createStyle(
  border = c("top", "bottom"),
  borderColour = "black",
  borderStyle = "thin"
)


# Apply border to a table range
sheets <- c(
            "Table_1",
            "Table_2")

for (s in sheets) {
addStyle(
  pse_by_org_wb,
  sheet = s,
  style = border_style,
  rows = 9,
  cols =  1:ncol(pub_fte),
  gridExpand = TRUE,
  stack = TRUE
)
}

border_style <- createStyle(
    border = c("bottom"),
    borderColour = "black",
    borderStyle = "thin"
  )
# Apply border to a table range
sheets <- c(
  "Table_1",
  "Table_2")

for (s in sheets) {
  addStyle(
    pse_by_org_wb,
    sheet = s,
    style = border_style,
    rows = c(10, 17, 38,57,105,127,183,186,190),
    cols =  1:ncol(pub_fte),
    gridExpand = TRUE,
    stack = TRUE
  )
}


border_style <- createStyle(
  border = c("right"),
  borderColour = "black",
  borderStyle = "thin"
)
# Apply border to a table range
sheets <- c(
  "Table_1",
  "Table_2")

for (s in sheets) {
  addStyle(
    pse_by_org_wb,
    sheet = s,
    style = border_style,
    rows = c(9:190),
    cols =  ncol(pub_fte),
    gridExpand = TRUE,
    stack = TRUE
  )
}


  

#-------------------
# add comma separators
# ----------------------------
comma_style <- createStyle(numFmt = "#,##0")

sheets <- c(
  "Table_1",
  "Table_2")

for (s in sheets) {
addStyle(
  pse_by_org_wb,
  sheet = s,
  style = comma_style,
  rows = c(10:190),
  cols =  c(4:ncol(pub_hc)-1),
  gridExpand = TRUE,
  stack = TRUE
)
}
# -----------------------------
# Export
# -----------------------------


period <- paste(
  rev(strsplit(as.character(latest_data_short), " ")[[1]]),
  collapse = " "
)

file_name <- file.path(
  adm_source_path,
  paste0(
    "publication/Public sector employment in Scotland - employment by organisation ",
    period,
    ".xlsx"
  )
)


openxlsx::saveWorkbook(wb = pse_by_org_wb, file = file_name, overwrite = T)
