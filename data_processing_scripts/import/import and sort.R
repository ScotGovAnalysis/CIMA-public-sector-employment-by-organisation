
# ==============================================================================
# Script Name : import and sort.R
# Purpose     : import OCEA data and sort out using lookups
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
library(readxl)
library(tidyverse)
library(here)

# ==============================================================================
# SETUP
# ==============================================================================
source(here("setup/config.R"))



# ==============================================================================
# Import
# ==============================================================================

# OCEA data -----
ocea_data <- 
  lapply(
    ocea_sheets,
    function(x) {
      read_xlsx(
        path = paste0(ocea_data_import_path, ocea_workbook_name),
        sheet = x
      )
    }
  )

names(ocea_data) <- ocea_sheets

# look up ----
lookup <- 
  lapply(
    lookup_sheets,
    function(x) {
      read_xlsx(
        path = paste0(publication_adm_path_import, lookup_workbook),
        sheet = x
      )
    }
  )

names(lookup) <- lookup_sheets


# NHS ------
nhs_data <- 
  lapply(
    nhs_sheets,
    function(x){
      read_xlsx(
        path = paste0(publication_adm_path_import, nhs_data_export),
        sheet = x
      )
    }
  )

names(nhs_data) <- paste("NHS", nhs_sheets)

# ==============================================================================
# Sort and wrangle
# ==============================================================================

# rename cols-----
# getting variable names
vars <- lookup$`variables in OCEA data`$`Civil Service Units`

# convert column names for non JSW sheets- paste organisation name against variable name
for (i in ocea_sheets[!ocea_sheets %in% c("JSW Headcount", "JSW FTE")]) {
  
  orgs <- names(ocea_data[[i]])[seq(2, ncol(ocea_data[[i]]), by = 4)]
  
  names(ocea_data[[i]]) <- c("Organisation",
    unlist(
      lapply(
        orgs,
        function(org) paste(org, vars)
      )
    )
  )

  
}


# Rename JSW columns to period format used in publication

names(ocea_data$`JSW Headcount`) <- c("Headcount", lookup$`period format`$`Publication tables columns`)
names(ocea_data$`JSW FTE`) <- c("FTE", lookup$`period format`$`Publication tables columns`)


# remove old header
ocea_data <- lapply(ocea_data, function(df) {
  df[-1, ]
})


# replace period formats in other sheets to match publication and JSW
periods <- if (latest) {
  lookup$`period format`$`Publication tables columns`
} else {
  head(lookup$`period format`$`Publication tables columns`, -2)
}

ocea_data[!ocea_sheets %in% c("JSW Headcount", "JSW FTE")] <- map(
  ocea_data[!ocea_sheets %in% c("JSW Headcount", "JSW FTE")],
  ~ .x |>
    mutate(Period = periods) |>
    select(Period, !Organisation)
)
# transpose 
ocea_data[!ocea_sheets %in% c("JSW Headcount", "JSW FTE")]<- lapply(ocea_data[!ocea_sheets %in% c("JSW Headcount", "JSW FTE")], function(df) {
  df <- as.data.frame(t(df))
    
    names(df) <- df[1, ]
    df <- df[-1, ]
    
    df
    
})

# add columns containing organisation name and FTE or HC

ocea_data[!ocea_sheets %in% c("JSW Headcount", "JSW FTE")]  <- map(
  ocea_data[!ocea_sheets %in% c("JSW Headcount", "JSW FTE")] ,
  ~ .x %>%
    
    rownames_to_column("rowname") %>%
    extract(
      rowname,
      into = c("Organisation", "Variable"),
      regex = "(.*)\\s(FTE.*|HC.*)",
      remove = FALSE
    ) |> 
    select(-rowname)
)


 ocea_data["JSW Headcount"]<- map(
   ocea_data["JSW Headcount"] ,
   ~ .x %>%
     mutate(Variable = "HC Total") |>
      select(Organisation = Headcount, Variable, everything() ))
  
 ocea_data["JSW FTE"]<- map(
   ocea_data["JSW FTE"] ,
   ~ .x %>%
     mutate(Variable = "FTE Total") |>
     select(Organisation = FTE , Variable, everything() ))


# bind into one dataframe

 ocea_data_full <- bind_rows(ocea_data, .id = "Source")
 
 
 
 # ==============================================================================
 # Sort and wrangle - NHS
 # ==============================================================================
# change colnames

 new_names <- c(
   "Country",
   "Region",
   "Organisation",
   names(ocea_data_full)[!names(ocea_data_full) %in% c("Organisation", "Variable", "Source")]
 )
 
 if (!latest) {
   new_names <- c(
     new_names,
     "2025 Quarter 4",
     "2026 Quarter 1" # for Q3 2025 where NHS export wasn't available, so reusing 2026 Q1
   )
 }
   
nhs_data <- map(
  nhs_data,
  ~{
    colnames(.x) <- new_names
    
    .x <- .x[-1, ] |> 
      select(-Country, -Region) 
  }
)


nhs_data["NHS Headcount"] <- map(nhs_data["NHS Headcount"],
                              ~.x |> 
   mutate(Variable = "HC Total") |>   
   select(Organisation, Variable, everything() ))
 
 
nhs_data["NHS FTE"] <- map(nhs_data["NHS FTE"],
                             ~.x |> 
                               mutate(Variable = "FTE Total") |>   
                               select(Organisation, Variable, everything() ))

# bind into one dataframe

nhs_data_full <- bind_rows(nhs_data, .id = "Source") 

  if(!latest){
    nhs_data_full <- nhs_data_full |> 
      select(-"2025 Quarter 4",
             -"2026 Quarter 1") # remove so can bind
  }

# combine with ocea
pse_org <- rbind(ocea_data_full, nhs_data_full)

# remove rows with empty quarters

quarter_cols <- grep("Quarter", names(pse_org), value = TRUE)

pse_org <- pse_org %>%
  filter(
    rowSums(
      !is.na(across(all_of(quarter_cols))) &
        across(all_of(quarter_cols)) != ""
    ) > 0
  )
# ==============================================================================
# make Quarters numeric
# ==============================================================================

pse_org <- pse_org %>%
  mutate(across(all_of(quarter_cols), as.numeric))


