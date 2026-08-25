# ==============================================================================
# Script Name : packages.R
# Purpose     : to check what packages and versiosn are used in the projects
# Author      : Jackie
# Created     : YYYY-MM-DD
# Updated     : YYYY-MM-DD
# Version     : 1.0.0
# ==============================================================================


library(renv)

# Detect dependencies
deps <- renv::dependencies(".")

# Get unique package names
pkgs <- unique(deps$Package)

# Get installed versions
versions <- sapply(pkgs, function(pkg) {
  tryCatch(
    as.character(packageVersion(pkg)),
    error = function(e) NA_character_
  )
})

# Create report
dependency_report <- data.frame(
  Package = pkgs,
  Version = versions,
  row.names = NULL
)

dependency_report[order(dependency_report$Package), ]