# Build plausibility metrics for app consumption.
dir.create("data/final", recursive = TRUE, showWarnings = FALSE)

clean_data_path <- "data/interim/clean_historical_data.csv"
if (!file.exists(clean_data_path)) {
  stop("Missing cleaned data. Run scripts/01_build_clean_data.R first.", call. = FALSE)
}

clean_data <- utils::read.csv(clean_data_path, stringsAsFactors = FALSE)
clean_data$date <- as.Date(clean_data$date)

series_data <- utils::read.csv("data/interim/index_series_long.csv", stringsAsFactors = FALSE)
series_data$date <- as.Date(series_data$date)

benchmark_year <- 2012L
growth_rate <- 0.05
anchor_year <- max(series_data$year, na.rm = TRUE)

series_ids <- unique(series_data$series_id)
simulation_rows <- vector("list", length(series_ids))
path_rows <- vector("list", length(series_ids))

for (i in seq_along(series_ids)) {
  series_id <- series_ids[[i]]
  series_subset <- series_data[series_data$series_id == series_id, , drop = FALSE]
  benchmark_row <- series_subset[series_subset$year == benchmark_year, , drop = FALSE]
  anchor_row <- series_subset[series_subset$year == anchor_year, , drop = FALSE]
  benchmark_index <- benchmark_row$index_value[[1]]
  anchor_index <- anchor_row$index_value[[1]]
  recovery_ratio <- benchmark_index / anchor_index
  already_recovered <- isTRUE(anchor_index >= benchmark_index)

  years_to_recover <- if (already_recovered) {
    0
  } else if (growth_rate <= 0) {
    Inf
  } else {
    ceiling(log(recovery_ratio) / log(1 + growth_rate))
  }

  recovery_year <- if (is.finite(years_to_recover)) anchor_year + years_to_recover else NA_integer_
  path_horizon <- if (is.finite(years_to_recover)) max(1L, years_to_recover) else 50L
  sim_years <- seq.int(anchor_year, anchor_year + path_horizon)
  simulated_index <- anchor_index * (1 + growth_rate)^(sim_years - anchor_year)

  path_rows[[i]] <- data.frame(
    series_id = series_id,
    series_label = series_subset$series_label[[1]],
    year = sim_years,
    simulated_index_value = simulated_index,
    benchmark_year = benchmark_year,
    benchmark_index_value = benchmark_index,
    anchor_year = anchor_year,
    growth_rate = growth_rate,
    stringsAsFactors = FALSE
  )

  simulation_rows[[i]] <- data.frame(
    series_id = series_id,
    series_label = series_subset$series_label[[1]],
    benchmark_year = benchmark_year,
    benchmark_index_value = benchmark_index,
    anchor_year = anchor_year,
    anchor_index_value = anchor_index,
    growth_rate = growth_rate,
    recovery_ratio = recovery_ratio,
    already_recovered = already_recovered,
    years_to_recover = years_to_recover,
    recovery_year = recovery_year,
    stringsAsFactors = FALSE
  )
}

simulation <- list(
  summary = do.call(rbind, simulation_rows),
  paths = do.call(rbind, path_rows)
)
simulation$summary$status <- "recovering"
simulation$summary$status[simulation$summary$already_recovered] <- "already_recovered"
simulation$summary$status[!simulation$summary$already_recovered & simulation$summary$growth_rate <= 0] <- "not_recovering"
simulation$summary$status[is.infinite(simulation$summary$years_to_recover)] <- "not_recovering"

build_plausibility_label <- function(metric_value) {
  if (length(metric_value) != 1 || is.na(metric_value)) {
    return(NA_character_)
  }
  if (metric_value >= 0.20) {
    return("common")
  }
  if (metric_value >= 0.05) {
    return("uncommon")
  }
  "historically_rare"
}

plausibility_rows <- vector("list", nrow(simulation$summary))
for (i in seq_len(nrow(simulation$summary))) {
  row <- simulation$summary[i, , drop = FALSE]
  growth_history <- if (row$series_id[[1]] == "gdp") clean_data$gdp_growth else clean_data$gdp_pc_growth
  valid_growth <- growth_history[!is.na(growth_history)]
  yearly_share <- if (length(valid_growth) == 0) NA_real_ else mean(valid_growth >= row$growth_rate[[1]])

  longest_true_run <- function(values) {
    runs <- rle(values)
    true_lengths <- runs$lengths[runs$values]
    if (length(true_lengths) == 0) {
      return(0)
    }
    max(true_lengths)
  }

  if (!is.finite(row$years_to_recover[[1]]) || row$years_to_recover[[1]] <= 0) {
    streak_share <- NA_real_
    max_streak <- longest_true_run(valid_growth >= row$growth_rate[[1]])
  } else {
    streak_length <- as.integer(row$years_to_recover[[1]])
    pass <- valid_growth >= row$growth_rate[[1]]
    possible_windows <- max(length(valid_growth) - streak_length + 1L, 0L)
    qualifying_windows <- 0L
    if (possible_windows > 0) {
      for (window_start in seq_len(possible_windows)) {
        window_end <- window_start + streak_length - 1L
        if (all(pass[window_start:window_end])) {
          qualifying_windows <- qualifying_windows + 1L
        }
      }
    }
    streak_share <- if (possible_windows > 0) qualifying_windows / possible_windows else 0
    max_streak <- longest_true_run(pass)
  }

  plausibility_rows[[i]] <- data.frame(
    series_id = row$series_id[[1]],
    series_label = row$series_label[[1]],
    growth_rate = row$growth_rate[[1]],
    years_to_recover = row$years_to_recover[[1]],
    yearly_plausibility = yearly_share,
    yearly_label = build_plausibility_label(yearly_share),
    streak_plausibility = streak_share,
    streak_label = build_plausibility_label(streak_share),
    max_historical_streak = max_streak,
    stringsAsFactors = FALSE
  )
}

plausibility <- do.call(rbind, plausibility_rows)

utils::write.csv(plausibility, "data/final/plausibility_metrics.csv", row.names = FALSE)
utils::write.csv(simulation$summary, "data/final/simulation_summary.csv", row.names = FALSE)
utils::write.csv(simulation$paths, "data/final/simulation_paths.csv", row.names = FALSE)
message("Wrote data/final/plausibility_metrics.csv")
