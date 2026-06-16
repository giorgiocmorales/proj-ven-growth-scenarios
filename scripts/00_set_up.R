# Set up the project session and required directories.

## Package groups --------------------------------------------------------------
# Keep package requirements grouped by usage so missing dependencies are easier
# to diagnose before running the full pipeline.
project_packages_core <- c("readxl", "dplyr", "jsonlite")
project_packages_app <- c("shiny")
project_packages_dev <- c("testthat")
project_packages_reports <- c("ggplot2", "scales", "quarto")
project_packages_legacy <- c("tidyverse", "janitor", "lubridate", "zoo")

project_packages_all <- unique(c(
  project_packages_core,
  project_packages_app,
  project_packages_dev,
  project_packages_reports,
  project_packages_legacy
))

## Package checks --------------------------------------------------------------
# Fail early with a concise list of packages that must be installed locally.
check_project_packages <- function(packages, error_if_missing = TRUE) {
  missing_packages <- packages[!vapply(
    packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )]

  if (length(missing_packages) > 0 && error_if_missing) {
    stop(
      sprintf(
        "Missing required packages: %s",
        paste(missing_packages, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(missing_packages)
}

missing_packages <- check_project_packages(project_packages_all, error_if_missing = FALSE)

if (length(missing_packages) > 0) {
  stop(
    sprintf(
      "Install these packages before running setup: %s",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

## Session setup ---------------------------------------------------------------
# Attach packages used by downstream scripts when the full pipeline is sourced.
for (package_name in project_packages_all) {
  library(package_name, character.only = TRUE)
}

## Directory setup -------------------------------------------------------------
# Create the writable folders expected by data, figure, and presentation scripts.
dir.create("data/interim", recursive = TRUE, showWarnings = FALSE)
dir.create("data/final", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("reports", recursive = TRUE, showWarnings = FALSE)
dir.create("reports/presentation/assets/images", recursive = TRUE, showWarnings = FALSE)

message("Loaded project packages: ", paste(project_packages_all, collapse = ", "))
message("Prepared project directories: data/interim, data/final, outputs/figures, reports, reports/presentation/assets/images")
