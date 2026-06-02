# Download IMF WEO inputs and derive real GDP per capita growth for the Maddison country sample.
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

read_imf_datamapper_indicator <- function(indicator_id, indicator_name) {
  imf_url <- sprintf("https://www.imf.org/external/datamapper/api/v1/%s", indicator_id)
  imf_json <- jsonlite::fromJSON(imf_url, simplifyVector = FALSE)

  if (is.null(imf_json$values[[indicator_id]])) {
    stop(sprintf("IMF DataMapper response did not include `%s`.", indicator_id), call. = FALSE)
  }

  imf_series <- imf_json$values[[indicator_id]]
  target_codes <- intersect(maddison_countries$country_code, names(imf_series))

  indicator_parts <- lapply(target_codes, function(country_code) {
    country_series <- imf_series[[country_code]]
    years <- names(country_series)

    data.frame(
      country_code = country_code,
      indicator_id = indicator_id,
      indicator = indicator_name,
      year = as.integer(years),
      value = as.numeric(unlist(country_series, use.names = FALSE)),
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(indicator_parts) |>
    dplyr::filter(!is.na(year), !is.na(value))
}

imf_weo_components <- dplyr::bind_rows(
  read_imf_datamapper_indicator("NGDP_RPCH", "Real GDP growth"),
  read_imf_datamapper_indicator("LP", "Population")
) |>
  dplyr::left_join(maddison_countries, by = "country_code") |>
  dplyr::arrange(country_code, indicator_id, year)

imf_weo_wide <- imf_weo_components |>
  dplyr::select(country_code, maddison_country, maddison_region, indicator_id, year, value) |>
  stats::reshape(
    idvar = c("country_code", "maddison_country", "maddison_region", "year"),
    timevar = "indicator_id",
    direction = "wide"
  )

names(imf_weo_wide) <- sub("^value\\.", "", names(imf_weo_wide))

imf_weo_gdp_pc_growth <- imf_weo_wide |>
  dplyr::arrange(country_code, year) |>
  dplyr::group_by(country_code) |>
  dplyr::mutate(
    population_growth = LP / dplyr::lag(LP) - 1,
    gdp_growth = NGDP_RPCH / 100,
    value = ((1 + gdp_growth) / (1 + population_growth) - 1) * 100
  ) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    country_code = country_code,
    maddison_country = maddison_country,
    maddison_region = maddison_region,
    indicator_id = "DERIVED_NGDP_RPCH_LP_PC",
    indicator = "Derived real GDP per capita growth from WEO real GDP growth and population",
    year = year,
    value = value,
    real_gdp_growth = NGDP_RPCH,
    population = LP,
    population_growth = population_growth * 100
  ) |>
  dplyr::filter(!is.na(value)) |>
  dplyr::arrange(country_code, year)

component_countries <- unique(imf_weo_components$country_code)
derived_countries <- unique(imf_weo_gdp_pc_growth$country_code)
leftout_codes <- setdiff(maddison_countries$country_code, derived_countries)

imf_metadata <- data.frame(
  metric = c(
    "source",
    "derived_indicator_id",
    "derived_indicator_name",
    "component_indicators",
    "maddison_country_count",
    "component_country_count",
    "derived_country_count",
    "row_count",
    "year_min",
    "year_max",
    "countries_leftout"
  ),
  value = c(
    "IMF DataMapper / WEO",
    "DERIVED_NGDP_RPCH_LP_PC",
    "Derived real GDP per capita growth from WEO real GDP growth and population",
    "NGDP_RPCH;LP",
    nrow(maddison_countries),
    length(component_countries),
    length(derived_countries),
    nrow(imf_weo_gdp_pc_growth),
    min(imf_weo_gdp_pc_growth$year, na.rm = TRUE),
    max(imf_weo_gdp_pc_growth$year, na.rm = TRUE),
    paste(sort(leftout_codes), collapse = ";")
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  imf_weo_components,
  "data/raw/imf_weo_real_gdp_growth_population_maddison_countries.csv",
  row.names = FALSE
)
utils::write.csv(
  imf_weo_gdp_pc_growth,
  "data/raw/imf_weo_gdp_per_capita_growth_maddison_countries.csv",
  row.names = FALSE
)
utils::write.csv(
  imf_metadata,
  "data/raw/imf_weo_gdp_per_capita_growth_maddison_countries_metadata.csv",
  row.names = FALSE
)

message("Wrote data/raw/imf_weo_real_gdp_growth_population_maddison_countries.csv")
message("Wrote data/raw/imf_weo_gdp_per_capita_growth_maddison_countries.csv")
message("Wrote data/raw/imf_weo_gdp_per_capita_growth_maddison_countries_metadata.csv")
