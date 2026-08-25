library(dplyr)
library(tidyr)
library(tibble)
# order dataset to amtch aggregate publication column order
row_order <- c("Civil Service", 
               "Other Public Bodies",
               "Police and Fire Services",
               "NHS",
               "Further Education Colleges",
               "Local Government",
               "Public Corporations")


format_totals <- function(df) {
  df <- as.data.frame(t(df))
  
  names(df) <- df[1, ]
  df <- df[-1, ]
  
  df |>
    rownames_to_column("period") |>
    extract(
      period,
      into = c("Year", "Quarter"),
      regex = "(\\d{4}) Quarter (\\d)"
    ) |>
    mutate(
      Quarter = paste0("Q", Quarter)
    ) |>
    select(
      Quarter,
      Year,
      `Civil Service` = `Devolved Civil Service`,
      everything()
    )
}

# summing all FTE/HC to get devolved public sector total 

 create_all_totals <- function(data,
                                totals_df,
                                row_order,
                                total_name = "Total Devolved Public Sector",
                                round_totals = FALSE,
                                round_to = -2) {
    
    all_tot <- data |>
      summarise(
        across(any_of(contains ("Quarter")), ~ sum(.x, na.rm = TRUE))
      )
    
    if (round_totals) {
      all_tot <- all_tot |>
        mutate(
          across(
            where(is.numeric),
            ~ round(.x, round_to)
          )
        )
    }
    
    all_tot <- all_tot |>
      pivot_longer(
        cols = everything(),
        names_to = c("Year", "Quarter"),
        names_pattern = "(\\d{4}) Quarter (\\d)",
        values_to = total_name
      ) |>
      mutate(Quarter = paste0("Q", Quarter))
    
    left_join(all_tot, totals_df, by = c("Quarter", "Year")) |>
      select(
        Quarter,
        Year,
        all_of(total_name),
        any_of(row_order)
      ) |>
      mutate(
        across(-Quarter, as.numeric)
      )
 }
 
 
 format_excluded_totals <- function(df) {
   df <- as.data.frame(t(df))
   
  names(df) <- df[1, ]
  df <- df[-1, ]
   
   df |>
     rownames_to_column("period") |>
     extract(
       period,
       into = c("Year", "Quarter"),
       regex = "(\\d{4}) Quarter (\\d)"
     ) |>
     mutate(
       Quarter = paste0("Q", Quarter)
     ) |>
     select(
       Quarter,
       Year,
       everything()
      )
 }

 
 # compare pse aggregate vs suppressed data 
 compare_tables <- function(agg_data,
                            disagg_data,
                            by = c("Quarter", "Year")) {
   
   measure_cols <- setdiff(names(agg_data), by)
   
   agg_data |>
     full_join(
       disagg_data,
       by = by,
       suffix = c("_agg", "_disagg")
     ) |>
     mutate(
       across(
         ends_with("_agg"),
         ~ .x - get(sub("_agg$", "_disagg", cur_column())),
         .names = "{sub('_agg$', '_diff', .col)}"
       )
     ) |>
     select(
       all_of(by),
       unlist(
         lapply(
           measure_cols,
           \(x) c(
             paste0(x, "_agg"),
             paste0(x, "_disagg"),
             paste0(x, "_diff")
           )
         )
       )
     )
 } 
 