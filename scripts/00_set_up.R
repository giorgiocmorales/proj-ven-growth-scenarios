# Set up the project session and required directories.
project_packages_core <- c("readxl")
project_packages_app <- c("shiny")
project_packages_dev <- c("testthat")
project_packages_legacy <- c("tidyverse", "dplyr", "janitor", "lubridate", "zoo")

project_packages_all <- unique(c(
  project_packages_core,
  project_packages_app,
  project_packages_dev,
  project_packages_legacy
))

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

for (package_name in project_packages_all) {
  library(package_name, character.only = TRUE)
}

dir.create("data/interim", recursive = TRUE, showWarnings = FALSE)
dir.create("data/final", recursive = TRUE, showWarnings = FALSE)
dir.create("reports", recursive = TRUE, showWarnings = FALSE)

message("Loaded project packages: ", paste(project_packages_all, collapse = ", "))
message("Prepared project directories: data/interim, data/final, reports")
