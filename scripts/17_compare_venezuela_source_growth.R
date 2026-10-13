# Compare IMF/WDI/BCV Venezuela growth with the project's historical series.
# Rates are annual percentages; gaps are external minus project in percentage points.
# IMF per-capita growth is derived from real GDP growth and population, not PPP GDP.
# The API does not expose actual-outturn flags; prior years may be staff estimates.

## Setup and inputs ----
# Require the existing pipeline dependencies and fail before writing partial results.
required_packages <- c("dplyr", "magrittr", "tidyr")
missing_packages <- required_packages[!vapply(
  required_packages, requireNamespace, logical(1), quietly = TRUE
)]
if (length(missing_packages) > 0) {
  stop(sprintf("Missing required packages: %s", paste(missing_packages, collapse = ", ")), call. = FALSE)
}
`%>%` <- magrittr::`%>%`
input_paths <- c(
  "data/interim/clean_historical_data.csv",
  "data/raw/wdi_real_gdp_growth_venezuela.csv",
  "data/raw/wdi_real_gdp_per_capita_growth_venezuela.csv",
  "data/raw/imf_weo_real_gdp_growth_population_venezuela.csv",
  "data/raw/imf_weo_gdp_per_capita_growth_venezuela.csv",
  "data/raw/wdi_real_gdp_growth_maddison_countries_metadata.csv",
  "data/raw/wdi_real_gdp_per_capita_growth_maddison_countries_metadata.csv",
  "data/raw/imf_weo_gdp_per_capita_growth_maddison_countries_metadata.csv",
  "data/raw/bcv_real_gdp_growth_venezuela.csv"
)
if (any(!file.exists(input_paths))) {
  stop(sprintf("Missing comparison inputs: %s", paste(input_paths[!file.exists(input_paths)], collapse = ", ")), call. = FALSE)
}
comparison_inputs <- lapply(input_paths, utils::read.csv, stringsAsFactors = FALSE)

# Retain the supplied proj-ven-econ snapshot and validate its annual GDP definition.
# Refresh data/raw/bcv_real_gdp_growth_venezuela.csv from that repo when requested.
bcv_growth <- comparison_inputs[[9]]
bcv_columns <- c("year", "value", "base_year", "provisional", "source_id", "source_url", "downloaded_at", "method", "series_label", "frequency", "unit")
if (!all(bcv_columns %in% names(bcv_growth))) {
  stop("BCV snapshot is missing required rate or provenance columns.", call. = FALSE)
}
if (nrow(bcv_growth) == 0L || anyNA(bcv_growth$year) || anyNA(bcv_growth$provisional) ||
    anyNA(bcv_growth[c("source_id", "series_label", "frequency", "unit")]) ||
    any(bcv_growth$source_id != "bcv" | bcv_growth$series_label != "Consolidado" |
        bcv_growth$frequency != "annual" | bcv_growth$unit != "percent_yoy") ||
    any(!is.finite(bcv_growth$value))) {
  stop("BCV snapshot must contain finite annual consolidated GDP growth in percent YoY.", call. = FALSE)
}

## Comparable annual rates ----
# Convert project decimals to percentages and retain unavailable external years.
project_growth <- comparison_inputs[[1]] %>%
  dplyr::select(year, gdp_growth, gdp_pc_growth) %>%
  tidyr::pivot_longer(-year, names_to = "series_id", values_to = "growth_pct") %>%
  dplyr::mutate(
    series_id = dplyr::recode(series_id, gdp_growth = "gdp", gdp_pc_growth = "gdp_per_capita"),
    source = "project",
    growth_pct = growth_pct * 100
  )
external_growth <- dplyr::bind_rows(
  comparison_inputs[[2]] %>% dplyr::transmute(year, series_id = "gdp", source = "world_bank", growth_pct = value),
  comparison_inputs[[3]] %>% dplyr::transmute(year, series_id = "gdp_per_capita", source = "world_bank", growth_pct = value),
  comparison_inputs[[4]] %>% dplyr::filter(indicator_id == "NGDP_RPCH") %>%
    dplyr::transmute(year, series_id = "gdp", source = "imf", growth_pct = value),
  comparison_inputs[[5]] %>% dplyr::transmute(year, series_id = "gdp_per_capita", source = "imf", growth_pct = value),
  bcv_growth %>% dplyr::transmute(year, series_id = "gdp", source = "bcv", growth_pct = value)
)
growth_long <- dplyr::bind_rows(project_growth, external_growth)
if (anyDuplicated(growth_long[c("year", "series_id", "source")]) > 0) {
  stop("Duplicate source-series-year keys in Venezuela comparison.", call. = FALSE)
}
if (any(!is.finite(growth_long$growth_pct) & !is.na(growth_long$growth_pct)) ||
    any(growth_long$growth_pct <= -100, na.rm = TRUE)) {
  stop("Invalid annual growth values in Venezuela comparison.", call. = FALSE)
}

## Common recovery baseline ----
# Normalize at 2012=100 and stop compounding at missing or non-adjacent years.
# This compares real growth paths without mixing source price levels or PPP units.
benchmark_year <- 2012L
index_long <- growth_long %>%
  dplyr::filter(year >= benchmark_year) %>%
  dplyr::arrange(source, series_id, year) %>%
  dplyr::group_by(source, series_id) %>%
  dplyr::mutate(
    index_2012_100 = 100 * cumprod(dplyr::case_when(
      year == benchmark_year ~ 1,
      year - dplyr::lag(year) == 1L ~ 1 + growth_pct / 100,
      TRUE ~ NA_real_
    ))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(year, series_id, source, index_2012_100)
if (nrow(dplyr::filter(index_long, year == benchmark_year)) !=
    nrow(dplyr::distinct(growth_long, source, series_id))) {
  stop("Every available source-series pair must cover the 2012 benchmark.", call. = FALSE)
}

## Comparison and coverage ----
# Keep IMF forward years visible, but exclude them from historical gap summaries.
imf_metadata <- comparison_inputs[[8]]
imf_retrieved_at <- imf_metadata$value[imf_metadata$metric == "retrieved_at_utc"]
if (length(imf_retrieved_at) != 1L) {
  stop("IMF retrieval timestamp missing or duplicated.", call. = FALSE)
}
forecast_start_year <- as.integer(substr(imf_retrieved_at, 1, 4))
source_comparison <- growth_long %>%
  tidyr::pivot_wider(names_from = source, values_from = growth_pct, names_glue = "{source}_growth_pct") %>%
  dplyr::left_join(
    index_long %>% tidyr::pivot_wider(
      names_from = source, values_from = index_2012_100, names_glue = "{source}_index_2012_100"
    ),
    by = c("year", "series_id")
  ) %>%
  dplyr::left_join(
    bcv_growth %>% dplyr::transmute(
      year, series_id = "gdp", bcv_provisional = provisional,
      bcv_base_year = base_year, bcv_method = method
    ),
    by = c("year", "series_id")
  ) %>%
  dplyr::mutate(
    imf_minus_project_pp = imf_growth_pct - project_growth_pct,
    world_bank_minus_project_pp = world_bank_growth_pct - project_growth_pct,
    imf_minus_world_bank_pp = imf_growth_pct - world_bank_growth_pct,
    bcv_minus_project_pp = bcv_growth_pct - project_growth_pct,
    imf_minus_bcv_pp = imf_growth_pct - bcv_growth_pct,
    world_bank_minus_bcv_pp = world_bank_growth_pct - bcv_growth_pct,
    imf_period = dplyr::case_when(
      is.na(imf_growth_pct) ~ "unavailable",
      year >= forecast_start_year ~ "forecast",
      TRUE ~ "historical_or_estimate"
    )
  ) %>%
  dplyr::arrange(series_id, year)
comparison_summary <- external_growth %>%
  dplyr::left_join(
    project_growth %>% dplyr::select(year, series_id, project_growth_pct = growth_pct),
    by = c("year", "series_id")
  ) %>%
  dplyr::filter(year < forecast_start_year, !is.na(growth_pct), !is.na(project_growth_pct)) %>%
  dplyr::group_by(source, series_id) %>%
  dplyr::summarise(
    paired_year_min = min(year), paired_year_max = max(year), paired_year_count = dplyr::n(),
    mean_external_minus_project_pp = mean(growth_pct - project_growth_pct),
    mean_absolute_gap_pp = mean(abs(growth_pct - project_growth_pct)),
    max_absolute_gap_pp = max(abs(growth_pct - project_growth_pct)),
    .groups = "drop"
  )

## Provenance and outputs ----
# Copy source retrieval metadata and record transformations alongside final tables.
comparison_metadata <- dplyr::bind_rows(
  comparison_inputs[[6]] %>% dplyr::mutate(source = "world_bank_gdp"),
  comparison_inputs[[7]] %>% dplyr::mutate(source = "world_bank_gdp_per_capita"),
  imf_metadata %>% dplyr::mutate(source = "imf"),
  data.frame(
    source = "bcv",
    metric = c("snapshot_origin", "snapshot_file", "source_url", "source_downloaded_at_utc", "coverage", "definition", "provisional_years"),
    value = c(
      "../proj-ven-econ/data/final/real_11_gdp_growth_historical_bcv.csv",
      "data/raw/bcv_real_gdp_growth_venezuela.csv",
      paste(unique(bcv_growth$source_url), collapse = ";"),
      paste(unique(bcv_growth$downloaded_at), collapse = ";"),
      sprintf("%s-%s", min(bcv_growth$year), max(bcv_growth$year)),
      "Annual consolidated real GDP YoY percent; within-base historical growth and published recent rates; no per-capita series supplied",
      paste(bcv_growth$year[bcv_growth$provisional], collapse = ";")
    )
  ),
  data.frame(
    source = "comparison",
    metric = c("project_input", "growth_units", "gap_direction", "index_base", "imf_status_rule", "imf_status_caveat"),
    value = c(
      "data/raw/milagros_economicos.xlsx: PIB Historico via scripts/05_build_clean_data.R",
      "Annual percent; project decimal rates multiplied by 100",
      "External minus project in percentage points",
      "2012=100; missing years are not bridged",
      sprintf("Years >= %s are forecasts; earlier years are historical or estimates", forecast_start_year),
      "DataMapper API supplies neither release vintage nor actual-outturn flags; IMF per-capita growth uses rounded LP population"
    )
  )
)
dir.create("data/final", recursive = TRUE, showWarnings = FALSE)
utils::write.csv(source_comparison, "data/final/venezuela_source_growth_comparison.csv", row.names = FALSE)
utils::write.csv(comparison_summary, "data/final/venezuela_source_growth_comparison_summary.csv", row.names = FALSE)
utils::write.csv(comparison_metadata, "data/final/venezuela_source_growth_comparison_metadata.csv", row.names = FALSE)
message("Wrote Venezuela source growth comparison, paired-year summary, and metadata.")
