# Download World Bank WDI real GDP growth for the Maddison country sample.
required_packages <- c("dplyr", "jsonlite", "readxl")
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

maddison_data_path <- "data/raw/mpd2023_web.xlsx"
if (!file.exists(maddison_data_path)) {
  stop(sprintf("Maddison workbook not found at `%s`.", maddison_data_path), call. = FALSE)
}

maddison_countries <- readxl::read_excel(maddison_data_path, sheet = "Full data") |>
  dplyr::transmute(
    country_code = as.character(countrycode),
    maddison_country = as.character(country),
    maddison_region = as.character(region)
  ) |>
  dplyr::filter(!is.na(country_code), nchar(country_code) == 3) |>
  dplyr::distinct(country_code, .keep_all = TRUE) |>
  dplyr::arrange(country_code)

download_wdi_indicator <- function(indicator_id, indicator_name, output_stem) {
  url <- sprintf(
    "https://api.worldbank.org/v2/country/all/indicator/%s?format=json&per_page=20000",
    indicator_id
  )
  raw_json <- jsonlite::fromJSON(url)

  if (length(raw_json) < 2 || is.null(raw_json[[2]]) || nrow(raw_json[[2]]) == 0) {
    stop(sprintf("World Bank returned no rows for %s.", indicator_id), call. = FALSE)
  }

  wdi_raw <- raw_json[[2]] |>
    dplyr::transmute(
      country_code = as.character(countryiso3code),
      country = as.character(country$value),
      indicator_id = as.character(indicator$id),
      indicator = as.character(indicator$value),
      year = as.integer(date),
      value = as.numeric(value),
      unit = as.character(unit),
      obs_status = as.character(obs_status),
      decimal = as.integer(decimal)
    ) |>
    dplyr::filter(!is.na(country_code), country_code %in% maddison_countries$country_code) |>
    dplyr::left_join(maddison_countries, by = "country_code") |>
    dplyr::select(
      country_code,
      country,
      maddison_country,
      maddison_region,
      indicator_id,
      indicator,
      year,
      value,
      unit,
      obs_status,
      decimal
    ) |>
    dplyr::arrange(country_code, year)

  wdi_leftout <- setdiff(maddison_countries$country_code, unique(wdi_raw$country_code))

  wdi_metadata <- data.frame(
    metric = c(
      "indicator_id",
      "indicator_name",
      "maddison_country_count",
      "downloaded_country_count",
      "row_count",
      "year_min",
      "year_max",
      "countries_leftout"
    ),
    value = c(
      indicator_id,
      indicator_name,
      nrow(maddison_countries),
      length(unique(wdi_raw$country_code)),
      nrow(wdi_raw),
      min(wdi_raw$year, na.rm = TRUE),
      max(wdi_raw$year, na.rm = TRUE),
      paste(sort(unique(wdi_leftout)), collapse = ";")
    ),
    stringsAsFactors = FALSE
  )

  data_path <- sprintf("data/raw/%s.csv", output_stem)
  metadata_path <- sprintf("data/raw/%s_metadata.csv", output_stem)
  utils::write.csv(wdi_raw, data_path, row.names = FALSE)
  utils::write.csv(wdi_metadata, metadata_path, row.names = FALSE)

  message("Wrote ", data_path)
  message("Wrote ", metadata_path)
}

download_wdi_indicator(
  indicator_id = "NY.GDP.MKTP.KD.ZG",
  indicator_name = "GDP growth (annual %)",
  output_stem = "wdi_real_gdp_growth_maddison_countries"
)

download_wdi_indicator(
  indicator_id = "NY.GDP.PCAP.KD.ZG",
  indicator_name = "GDP per capita growth (annual %)",
  output_stem = "wdi_real_gdp_per_capita_growth_maddison_countries"
)
