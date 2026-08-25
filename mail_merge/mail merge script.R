#extract_emails_semicolon <- function(x) {
#  pattern <- "[A-Za-z0-9.!#$%&'*+/=?^_`{|}~-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"

#  matches <- regmatches(x, gregexpr(pattern, x))

# collapse each element's matches with semicolons
#sapply(matches, function(m) {
#  if (length(m) == 0) "" else paste(m, collapse = ";")
# })
#

#email_addresses$email_address_string<-extract_emails_semicolon(email_addresses$email_contact_string)


group_col <- "Organisation"

out_dir   <- paste0(adm_source_path, "mail merge/data")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Group once
gb <- exchequer_suppressed %>% group_by(across(all_of(group_col))) |> 
  select(Organisation, `Primary Classification`,
         Variable, everything(), -Consent)

# Split and get keys in the same order
grouped_list <- group_split(gb, .keep = TRUE)
group_keys_df <- group_keys(gb)
group_names <- group_keys_df[[group_col]]   # vector of group values in matching order

sanitize <- function(x) {
  x <- as.character(x)
  x <- gsub("[[:space:]]+", "_", x)
  x <- gsub("[^A-Za-z0-9._-]", "", x)
  x <- substr(x, 1, 120)
  ifelse(nzchar(x), x, "NA")
}

# (Optional) set names on the list for clarity
names(grouped_list) <- sanitize(group_names)

record<-NULL
# Write files
for (i in seq_along(grouped_list)) {
  
  org_name <- group_names[i]
  
  if (org_name == "Tayside and Central Scotland Transport Partnership (TACTRAN)") {
    org_name <- "Transport Partnership TACTRAN" # shorter file name to avoid error
  }
  
  file_name <- paste0(
    "PSE_employment_statistics_2026_Q1_",
    sanitize(org_name),
    ".xlsx"
  )
  
  write_xlsx(grouped_list[[i]], path = file.path(out_dir, file_name))
  
  record$org[i] <- group_names[i]
  record$fpath[i] <- normalizePath(file.path(out_dir, file_name))
}
`

# record_out<-as_tibble(record) %>% left_join(rename_and_classify, by=c("org"="updated_name")) %>% select(-c("input_name")) %>% left_join(email_addresses, by=c("org"="Organisation")) %>% select(-c("email_contact_string"))
# 
# write_xlsx(record_out, "mail_merge_secondary_support.xlsx")
