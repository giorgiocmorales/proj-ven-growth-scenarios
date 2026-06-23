# Build normalized historical series used by the simulator.

## Inputs ----
# Read the cleaned Venezuela history produced by scripts/05_build_clean_data.R.
dir.create("data/interim", recursive = TRUE, showWarnings = FALSE)

clean_data_path <- "data/interim/clean_historical_data.csv"
if (!file.exists(clean_data_path)) {
  stop("Missing cleaned data. Run scripts/05_build_clean_data.R first.", call. = FALSE)
}

clean_data <- utils::read.csv(clean_data_path, stringsAsFactors = FALSE)
clean_data$date <- as.Date(clean_data$date)

## Anchor year ----
# Use the latest available observation as the current recovery anchor.
latest_year <- max(clean_data$year, na.rm = TRUE)
anchor_data <- clean_data[clean_data$year == latest_year, , drop = FALSE]

## Long series ----
# Stack total GDP and GDP per-capita into one long table for common downstream code.
series_outputs <- list()
series_outputs$series_long <- rbind(
  data.frame(
    series_id = "gdp",
    series_label = "Real GDP",
    year = clean_data$year,
    date = clean_data$date,
    growth_rate = clean_data$gdp_growth,
    index_value = clean_data$gdp_index,
    population = clean_data$population,
    stringsAsFactors = FALSE
  ),
  data.frame(
    series_id = "gdp_per_capita",
    series_label = "Real GDP per capita",
    year = clean_data$year,
    date = clean_data$date,
    growth_rate = clean_data$gdp_pc_growth,
    index_value = clean_data$gdp_pc_index,
    population = clean_data$population,
    stringsAsFactors = FALSE
  )
)

series_outputs$series_long$anchor_year <- latest_year
series_outputs$series_long$anchor_index_value <- ifelse(
  series_outputs$series_long$series_id == "gdp",
  anchor_data$gdp_index[[1]],
  anchor_data$gdp_pc_index[[1]]
)
series_outputs$series_long$index_vs_anchor_100 <-
  (series_outputs$series_long$index_value / series_outputs$series_long$anchor_index_value) * 100
series_outputs$series_long$is_anchor_year <- series_outputs$series_long$year == latest_year

## Benchmark table ----
# Turn each historical year into a candidate benchmark for the simulator.
series_outputs$benchmark_table <- series_outputs$series_long[, c(
  "series_id",
  "series_label",
  "year",
  "date",
  "index_value",
  "anchor_year",
  "anchor_index_value",
  "index_vs_anchor_100"
)]
names(series_outputs$benchmark_table)[names(series_outputs$benchmark_table) == "year"] <- "benchmark_year"
names(series_outputs$benchmark_table)[names(series_outputs$benchmark_table) == "date"] <- "benchmark_date"
names(series_outputs$benchmark_table)[names(series_outputs$benchmark_table) == "index_value"] <- "benchmark_index_value"
names(series_outputs$benchmark_table)[names(series_outputs$benchmark_table) == "index_vs_anchor_100"] <- "benchmark_vs_anchor_100"

series_outputs$normalized_by_benchmark <- merge(
  series_outputs$series_long[, c("series_id", "series_label", "year", "date", "growth_rate", "index_value")],
  series_outputs$benchmark_table[, c("series_id", "benchmark_year", "benchmark_date", "benchmark_index_value")],
  by = "series_id",
  all = FALSE
)

series_outputs$normalized_by_benchmark$index_vs_benchmark_100 <-
  (series_outputs$normalized_by_benchmark$index_value /
     series_outputs$normalized_by_benchmark$benchmark_index_value) * 100
series_outputs$normalized_by_benchmark$years_from_benchmark <-
  series_outputs$normalized_by_benchmark$year - series_outputs$normalized_by_benchmark$benchmark_year
series_outputs$normalized_by_benchmark$is_benchmark_year <-
  series_outputs$normalized_by_benchmark$year == series_outputs$normalized_by_benchmark$benchmark_year

## Series summary ----
# Record basic coverage and latest index values for quick validation.
series_outputs$series_summary <- data.frame(
  series_id = c("gdp", "gdp_per_capita"),
  series_label = c("Real GDP", "Real GDP per capita"),
  observation_count = c(
    sum(series_outputs$series_long$series_id == "gdp"),
    sum(series_outputs$series_long$series_id == "gdp_per_capita")
  ),
  first_year = c(min(clean_data$year, na.rm = TRUE), min(clean_data$year, na.rm = TRUE)),
  latest_year = c(latest_year, latest_year),
  latest_index_value = c(anchor_data$gdp_index[[1]], anchor_data$gdp_pc_index[[1]]),
  stringsAsFactors = FALSE
)

## Data outputs ----
# Persist normalized series tables for app, report, and graph scripts.
utils::write.csv(series_outputs$series_long, "data/interim/index_series_long.csv", row.names = FALSE)
utils::write.csv(series_outputs$benchmark_table, "data/interim/benchmark_table.csv", row.names = FALSE)
utils::write.csv(
  series_outputs$normalized_by_benchmark,
  "data/interim/normalized_series_by_benchmark.csv",
  row.names = FALSE
)
utils::write.csv(series_outputs$series_summary, "data/interim/index_series_summary.csv", row.names = FALSE)

message("Wrote data/interim/index_series_long.csv")
message("Wrote data/interim/benchmark_table.csv")
message("Wrote data/interim/normalized_series_by_benchmark.csv")
message("Wrote data/interim/index_series_summary.csv")
