
# ==============================================================================
# Script Name : format main PSE for publication.R
# Purpose     : format aggregate published PSE to add to publication tables
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

#FTE first
 # add quarter col to match disagg publication
published_fte <- published_fte |> 
  mutate(q_full = paste(Year, gsub("Q", "Quarter ", Quarter))) |> 
  select(-Year, -Quarter) |> 
  rename(Quarter = q_full)

# transpose dataframe to match disagg publication
published_fte <- as.data.frame(t(published_fte)) 

# rename column names
names(published_fte) <- published_fte["Quarter", ]

# add classification column and other tags to match published table
published_fte <- published_fte[-nrow(published_fte), ] # remove last row


total_fte <- published_fte|> 
  mutate(`Primary classification` =  case_when(
    row.names(published_fte) == "Civil Service" ~ "Devolved Civil Service",
    .default = row.names(published_fte)),
         `Secondary classification` = `Primary classification`,
          Organisation = case_when(
            `Primary classification` == "Total Devolved Public Sector" ~ "TOTAL Devolved Public Sector",
            .default = paste("TOTAL", `Primary classification`) )) |> 
  select(Organisation,
         `Primary classification`,
         `Secondary classification`,
         contains("Quarter"))
         
row.names(total_fte) <- NULL


# repeat for headcount

# add quarter col to match disagg publication
published_hc <- published_hc |> 
  mutate(q_full = paste(Year, gsub("Q", "Quarter ", Quarter))) |> 
  select(-Year, -Quarter) |> 
  rename(Quarter = q_full)

# transpose dataframe to match disagg publication
published_hc <- as.data.frame(t(published_hc)) 

# rename column names
names(published_hc) <- published_hc["Quarter", ]

# add classification column and other tags to match published table
published_hc <- published_hc[-nrow(published_hc), ] # remove last row


total_hc <- published_hc|> 
  mutate(`Primary classification` =  case_when(
    row.names(published_hc) == "Civil Service" ~ "Devolved Civil Service",
    .default = row.names(published_hc)),
    `Secondary classification` = `Primary classification`,
    Organisation = case_when(
      `Primary classification` == "Total Devolved Public Sector" ~ "TOTAL Devolved Public Sector",
      .default = paste("TOTAL", `Primary classification`) )) |> 
  select(Organisation,
         `Primary classification`,
         `Secondary classification`,
         contains("Quarter"))

row.names(total_hc) <- NULL




  
