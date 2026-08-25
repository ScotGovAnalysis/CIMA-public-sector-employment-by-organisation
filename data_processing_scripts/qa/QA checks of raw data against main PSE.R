
# ==============================================================================
# Script Name : QA checks against main PSE .R
# Purpose     : QA organisation totals against published aggregate totals
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
# IMPORT processed data and published agg tables
# ==============================================================================
latest  <- TRUE
source(here("data_processing_scripts/process/process.R"))
source(here("data_processing_scripts/qa/functions.R"))


# published PSE web table import ----
pub_qa <- 
  lapply(
    qa_sheets,
    function(x) {
      read_xlsx(
        path = paste0(publication_adm_path_qa, qa_workbook),
        sheet = x
      )
    }
  )

names(pub_qa) <- qa_sheets 


pub_qa <- purrr::map(pub_qa, ~ .x |>
                       janitor::row_to_names(row_number = 6) |> # get col headers
                       dplyr::slice(-(1:80)) # keep data from 2019
                     |> select(-starts_with("Revised")
))# trim to column header 

# order dataset to amtch aggregate publication column order
row_order <- c("Civil Service", 
               "Other Public Bodies",
               "Police and Fire Services",
               "NHS",
               "Further Education Colleges",
               "Local Government",
               "Public Corporations")
               #"Not in scope")

pse_org2 <- pse_org2 |> 
  arrange(match(classification, row_order))

pse_hc <- pse_org2 |> filter(Variable == "HC Total")
pse_fte <- pse_org2 |> filter(Variable == "FTE Total")



quarter_cols <- c("Quarter 1",
                  "Quarter 2",
                  "Quarter 3",
                  "Quarter 4")

years <- c(2019,2020,2021,2022,2023, 2024, 2025,2026)

quarter_cols <- unlist(lapply(years, function(i) paste(i, quarter_cols)))



# ==============================================================================
# CALCULATE totals and round data 
# ==============================================================================

hc_tot <- pse_hc |>
  group_by(classification) |>
  summarise(
    across(any_of(quarter_cols), ~ sum(.x, na.rm =T)),
    .groups = "drop"
  )



# round to nearest 100 or 1000 if LG or PC

hc_tot <- hc_tot |>
  mutate(
    across(
      where(is.numeric),
      ~ if_else(
        classification %in% c("Local Government", "Public Corporations"),
        round(.x, -3),  # nearest 1000
        round(.x, -2)   # nearest 100
      )
    )
  )

# transpose and edit col names to match web tables
hc_tot <- format_totals(hc_tot)


# repeat for FTE

fte_tot <- pse_fte |>
  group_by(classification) |>
  summarise(
    across(any_of(quarter_cols), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# # keep Q1 and Q4 from 2019 only
# 
# fte_tot <- fte_tot |> 
#   select(classification, contains(c("Quarter 1", "Quarter 4")))|> 
#   arrange(match(classification, row_order))


# round to nearest 100 or 1000 if LG or PC

fte_tot <- fte_tot |>
  mutate(
    across(
      where(is.numeric),
      ~ if_else(
        classification %in% c("Local Government", "Public Corporations"),
        round(.x, -3),  # nearest 1000
        round(.x, -2)   # nearest 100
      )
    )
  )


# transpose and edit col names to match web tables
fte_tot <- format_totals(fte_tot)


# calculate total devolved PS
fte_all <- create_all_totals(
  data = pse_fte,
  totals_df = fte_tot,
  row_order = row_order,
  round_totals = TRUE
) 


# repeat for headcount
hc_all <- create_all_totals(
  data = pse_hc,
  totals_df = hc_tot,
  row_order = row_order,
  round_totals = TRUE
) 


# format published tables ahead of export

published_hc <- pub_qa$Table_3
names(published_hc) <- names(hc_all)
published_hc <- published_hc |>
  mutate(across(!Quarter, as.numeric))


published_fte <- pub_qa$Table_10
names(published_fte) <- names(fte_all)
published_fte <- published_fte |>
  mutate(across(!Quarter, as.numeric))


 


write_xlsx(
  list(
    # "full_data" = pse_org2,
    "Unpublished HC" = hc_all,
    "Published HC"   = published_hc,
    "Unpublished FTE" = fte_all,
    "Published FTE"   = published_fte
  ),
  path = paste0(
    adm_source_path,
    "code outputs/QA check against published values Q126 ",
    Sys.Date(),
    ".xlsx"
  )
)







# checking lookup
agg_class <- lookup$organisations |> 
  rename(agg_class = classification,
         updated_name = update_name)

check2 <- left_join(pse_org2, agg_class, by = "updated_name")

check_match <- check2 |> filter(classification == agg_class )
check_no <- check2 |> filter(classification != agg_class )

setdiff(prev_look$updated_name, agg_class$`Devolved public sector`)          

check2 <- check2 |> filter(is.na(agg_class))
