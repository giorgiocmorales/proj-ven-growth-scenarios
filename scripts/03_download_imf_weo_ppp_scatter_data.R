# Download and cache IMF WEO PPP/nominal GDP data for scatterplot figures.

## Setup ----
# Check packages before querying IMF DataMapper and reading the Maddison workbook.
required_packages <- c("dplyr", "jsonlite", "magrittr", "readxl")
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

`%>%` <- magrittr::`%>%`

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/final", recursive = TRUE, showWarnings = FALSE)

## Country universe ----
# Use Maddison country metadata and flag the LatAm peer group used in charts.
maddison_data_path <- "data/raw/mpd2023_web.xlsx"
if (!file.exists(maddison_data_path)) {
  stop(sprintf("Maddison workbook not found at `%s`.", maddison_data_path), call. = FALSE)
}

maddison_countries <- readxl::read_excel(maddison_data_path, sheet = "Full data") %>%
  dplyr::transmute(
    country_code = as.character(countrycode),
    country = as.character(country),
    maddison_region = as.character(region)
  ) %>%
  dplyr::filter(!is.na(country_code), nchar(country_code) == 3) %>%
  dplyr::distinct(country_code, .keep_all = TRUE)

emerging_latam_codes <- c(
  "ATG", "ARG", "ABW", "BHS", "BRB", "BLZ", "BOL", "BRA", "CHL", "COL",
  "CRI", "DMA", "DOM", "ECU", "SLV", "GRD", "GTM", "GUY", "HTI", "HND",
  "JAM", "MEX", "NIC", "PAN", "PRI", "PRY", "PER", "KNA", "LCA", "VCT",
  "SUR", "TTO", "URY", "VEN"
)

## IMF download helper ----
# Return one DataMapper indicator as a country-year table.
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

  dplyr::bind_rows(indicator_parts) %>%
    dplyr::filter(!is.na(year), !is.na(value))
}

## Component data ----
# Pull PPP, nominal, per-capita, and population components for one scatter table.
imf_ppp_components <- dplyr::bind_rows(
  read_imf_datamapper_indicator("PPPGDP", "GDP, current international dollars, PPP"),
  read_imf_datamapper_indicator("PPPPC", "GDP per capita, current international dollars, PPP"),
  read_imf_datamapper_indicator("NGDPD", "GDP, current U.S. dollars"),
  read_imf_datamapper_indicator("NGDPDPC", "GDP per capita, current U.S. dollars"),
  read_imf_datamapper_indicator("LP", "Population")
) %>%
  dplyr::left_join(maddison_countries, by = "country_code") %>%
  dplyr::arrange(country_code, indicator_id, year)

imf_ppp_wide <- imf_ppp_components %>%
  dplyr::select(country_code, country, maddison_region, indicator_id, year, value) %>%
  stats::reshape(
    idvar = c("country_code", "country", "maddison_region", "year"),
    timevar = "indicator_id",
    direction = "wide"
  )

names(imf_ppp_wide) <- sub("^value\\.", "", names(imf_ppp_wide))

## Scatter table ----
# Keep only complete country-years and add the display highlight group.
imf_ppp_scatter_data <- imf_ppp_wide %>%
  dplyr::transmute(
    country_code = country_code,
    country = country,
    maddison_region = maddison_region,
    year = year,
    gdp_ppp_current_intl_dollars_billions = PPPGDP,
    gdp_per_capita_ppp_current_intl_dollars = PPPPC,
    gdp_nominal_current_usd_billions = NGDPD,
    gdp_per_capita_nominal_current_usd = NGDPDPC,
    population_millions = LP,
    highlight_group = dplyr::case_when(
      country_code == "VEN" ~ "Venezuela",
      country_code %in% emerging_latam_codes ~ "LatAm emergente",
      TRUE ~ "Resto del mundo"
    )
  ) %>%
  dplyr::filter(
    !is.na(gdp_ppp_current_intl_dollars_billions),
    !is.na(gdp_per_capita_ppp_current_intl_dollars),
    !is.na(gdp_nominal_current_usd_billions),
    !is.na(gdp_per_capita_nominal_current_usd),
    !is.na(population_millions)
  ) %>%
    dplyr::arrange(year, highlight_group, country)

## Data outputs ----
# Persist the raw components and the final scatterplot-ready table.
utils::write.csv(
  imf_ppp_components,
  "data/raw/imf_weo_ppp_gdp_population_maddison_countries.csv",
  row.names = FALSE
)
utils::write.csv(
  imf_ppp_scatter_data,
  "data/final/imf_weo_ppp_scatter_data.csv",
  row.names = FALSE
)

message("Wrote data/raw/imf_weo_ppp_gdp_population_maddison_countries.csv")
message("Wrote data/final/imf_weo_ppp_scatter_data.csv")
