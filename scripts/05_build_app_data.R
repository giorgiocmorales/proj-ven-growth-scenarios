# Assemble final processed CSVs consumed by the app and report.
dir.create("data/final", recursive = TRUE, showWarnings = FALSE)

clean_data_path <- "data/interim/clean_historical_data.csv"
if (!file.exists(clean_data_path)) {
  stop("Missing cleaned data. Run scripts/01_build_clean_data.R first.", call. = FALSE)
}

clean_data <- utils::read.csv(clean_data_path, stringsAsFactors = FALSE)
clean_data$date <- as.Date(clean_data$date)

series_outputs <- list(
  series_long = utils::read.csv("data/interim/index_series_long.csv", stringsAsFactors = FALSE),
  benchmark_table = utils::read.csv("data/interim/benchmark_table.csv", stringsAsFactors = FALSE)
)
episodes <- list(
  episode_summary = utils::read.csv("data/interim/episode_summary.csv", stringsAsFactors = FALSE),
  episode_path = utils::read.csv("data/interim/episode_path.csv", stringsAsFactors = FALSE)
)
simulation <- list(
  summary = utils::read.csv("data/final/simulation_summary.csv", stringsAsFactors = FALSE),
  paths = utils::read.csv("data/final/simulation_paths.csv", stringsAsFactors = FALSE)
)
plausibility <- utils::read.csv("data/final/plausibility_metrics.csv", stringsAsFactors = FALSE)

utils::write.csv(clean_data, "data/final/clean_historical_data.csv", row.names = FALSE)
utils::write.csv(series_outputs$series_long, "data/final/index_series_long.csv", row.names = FALSE)
utils::write.csv(series_outputs$benchmark_table, "data/final/benchmark_table.csv", row.names = FALSE)
utils::write.csv(episodes$episode_summary, "data/final/episode_summary.csv", row.names = FALSE)
utils::write.csv(episodes$episode_path, "data/final/episode_path.csv", row.names = FALSE)
utils::write.csv(simulation$summary, "data/final/simulation_summary.csv", row.names = FALSE)
utils::write.csv(simulation$paths, "data/final/simulation_paths.csv", row.names = FALSE)
utils::write.csv(plausibility, "data/final/plausibility_metrics.csv", row.names = FALSE)

validation <- do.call(
  rbind,
  list(
    data.frame(
      check = "clean_data_has_rows",
      passed = nrow(clean_data) > 0,
      details = sprintf("Rows in clean_data: %s", nrow(clean_data)),
      stringsAsFactors = FALSE
    ),
    data.frame(
      check = "series_outputs_available",
      passed = is.list(series_outputs),
      details = paste("Output type:", paste(class(series_outputs), collapse = "/")),
      stringsAsFactors = FALSE
    ),
    data.frame(
      check = "episodes_available",
      passed = is.list(episodes),
      details = paste("Output type:", paste(class(episodes), collapse = "/")),
      stringsAsFactors = FALSE
    ),
    data.frame(
      check = "simulation_available",
      passed = is.list(simulation),
      details = paste("Output type:", paste(class(simulation), collapse = "/")),
      stringsAsFactors = FALSE
    ),
    data.frame(
      check = "plausibility_available",
      passed = is.data.frame(plausibility),
      details = paste("Output type:", paste(class(plausibility), collapse = "/")),
      stringsAsFactors = FALSE
    )
  )
)

utils::write.csv(validation, "data/final/validation_checks.csv", row.names = FALSE)
message("Wrote app-ready outputs to data/final/")
