# Download and cache OWID development-indicator source files used by presentation graphs.

## Setup -----------------------------------------------------------------------
# Check packages before downloading OWID Grapher and indicator API files.
required_packages <- c("dplyr", "jsonlite")
missing_packages <- required_packages[!vapply(
  required_packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)]

if (length(missing_packages) > 0) {
  stop(
    sprintf("Missing required packages: %s", paste(missing_packages, collapse = ", ")),
    call. = FALSE
  )
}

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

## Grapher sources -------------------------------------------------------------
# Standard OWID Grapher CSVs used by the development relationship charts.
indicator_sources <- data.frame(
  slug = c(
    "life-expectancy-vs-gdp-per-capita",
    "median-daily-per-capita-expenditure-vs-gdp-per-capita",
    "child-mortality-gdp-per-capita",
    "human-development-index-vs-gdp-per-capita",
    "average-years-of-schooling-vs-gdp-per-capita",
    "energy-use-per-person-vs-gdp-per-capita"
  ),
  csv_url = c(
    "https://ourworldindata.org/grapher/life-expectancy-vs-gdp-per-capita.csv?v=1&csvType=full&useColumnShortNames=true",
    "https://ourworldindata.org/grapher/median-daily-per-capita-expenditure-vs-gdp-per-capita.csv?v=1&csvType=full&useColumnShortNames=true",
    "https://ourworldindata.org/grapher/child-mortality-gdp-per-capita.csv?v=1&csvType=full&useColumnShortNames=true",
    "https://ourworldindata.org/grapher/human-development-index-vs-gdp-per-capita.csv?v=1&csvType=full&useColumnShortNames=true",
    "https://ourworldindata.org/grapher/average-years-of-schooling-vs-gdp-per-capita.csv?v=1&csvType=full&useColumnShortNames=true",
    "https://ourworldindata.org/grapher/energy-use-per-person-vs-gdp-per-capita.csv?v=1&csvType=full&useColumnShortNames=true"
  ),
  stringsAsFactors = FALSE
)

## CSV downloads ---------------------------------------------------------------
# Cache each Grapher source as-is so graph scripts can run offline.
for (row_index in seq_len(nrow(indicator_sources))) {
  raw_path <- file.path("data/raw", sprintf("%s.csv", indicator_sources$slug[[row_index]]))
  utils::download.file(indicator_sources$csv_url[[row_index]], raw_path, mode = "wb", quiet = TRUE)
  message("Wrote ", raw_path)
}

## Democracy API download ------------------------------------------------------
# Democracy comes from the OWID indicator API, which needs a separate entity map.
democracy_variable_id <- 1014800
data_url <- sprintf("https://api.ourworldindata.org/v1/indicators/%s.data.json", democracy_variable_id)
metadata_url <- sprintf("https://api.ourworldindata.org/v1/indicators/%s.metadata.json", democracy_variable_id)
indicator_data <- jsonlite::fromJSON(data_url, simplifyVector = TRUE)
indicator_metadata <- jsonlite::fromJSON(metadata_url, simplifyVector = FALSE)

entity_lookup <- dplyr::bind_rows(lapply(indicator_metadata$dimensions$entities$values, function(entity) {
  data.frame(
    entity_id = entity$id,
    country = entity$name,
    country_code = if (is.null(entity$code)) NA_character_ else entity$code,
    stringsAsFactors = FALSE
  )
}))

## Democracy output ------------------------------------------------------------
# Normalize the API payload to the same country-year shape as the Grapher files.
democracy_raw <- data.frame(
  entity_id = indicator_data$entities,
  year = as.integer(indicator_data$years),
  indicator_value = as.numeric(indicator_data$values),
  stringsAsFactors = FALSE
) |>
  dplyr::left_join(entity_lookup, by = "entity_id") |>
  dplyr::transmute(
    indicator_id = "democracy",
    country = country,
    country_code = country_code,
    year = year,
    indicator_value = indicator_value,
    population = NA_real_
  ) |>
  dplyr::filter(
    !is.na(country_code),
    nchar(country_code) == 3,
    !is.na(indicator_value)
  )

democracy_raw_path <- "data/raw/owid_democracy_index.csv"
utils::write.csv(democracy_raw, democracy_raw_path, row.names = FALSE)
message("Wrote ", democracy_raw_path)
