# Build historical boom-bust episode tables.
dir.create("data/interim", recursive = TRUE, showWarnings = FALSE)

clean_data_path <- "data/interim/clean_historical_data.csv"
if (!file.exists(clean_data_path)) {
  stop("Missing cleaned data. Run scripts/01_build_clean_data.R first.", call. = FALSE)
}

clean_data <- utils::read.csv(clean_data_path, stringsAsFactors = FALSE)
clean_data$date <- as.Date(clean_data$date)

build_episode_outputs <- function(year, date, growth_rate, index_value, series_id, series_label) {
  if (length(year) < 2) {
    stop("Episode construction requires at least two observations.", call. = FALSE)
  }

  transition_rows <- seq.int(2L, length(year))
  transition_sign <- ifelse(
    is.na(growth_rate[transition_rows]),
    NA_integer_,
    ifelse(growth_rate[transition_rows] >= 0, 1L, -1L)
  )

  episode_id <- integer(length(transition_sign))
  current_episode <- 0L
  previous_sign <- NA_integer_

  for (i in seq_along(transition_sign)) {
    current_sign <- transition_sign[[i]]
    if (is.na(current_sign)) {
      episode_id[[i]] <- NA_integer_
    } else {
      if (is.na(previous_sign) || current_sign != previous_sign) {
        current_episode <- current_episode + 1L
      }
      episode_id[[i]] <- current_episode
      previous_sign <- current_sign
    }
  }

  valid_rows <- !is.na(episode_id)
  unique_ids <- unique(episode_id[valid_rows])
  episode_path_parts <- vector("list", length(unique_ids))
  episode_summary_parts <- vector("list", length(unique_ids))

  for (j in seq_along(unique_ids)) {
    id <- unique_ids[[j]]
    transition_run_positions <- which(episode_id == id)
    rows <- seq.int(
      transition_rows[transition_run_positions[[1]]] - 1L,
      transition_rows[transition_run_positions[[length(transition_run_positions)]]]
    )
    transition_indices <- rows[-1]
    phase <- if (transition_sign[transition_run_positions[[1]]] >= 0) "expansion" else "contraction"
    episode_label <- sprintf("%s-%s", year[rows[[1]]], year[rows[[length(rows)]]])
    episode_index_100 <- (index_value[rows] / index_value[rows[[1]]]) * 100
    episode_growth_rate <- c(NA_real_, growth_rate[transition_indices])

    episode_path_parts[[j]] <- data.frame(
      series_id = series_id,
      series_label = series_label,
      episode_id = id,
      episode_label = episode_label,
      phase = phase,
      year = year[rows],
      date = date[rows],
      growth_rate = episode_growth_rate,
      index_value = index_value[rows],
      episode_year_number = seq.int(0L, length(rows) - 1L),
      episode_index_100 = episode_index_100,
      overlap_flag = as.integer(rows[[1]] != 1L),
      stringsAsFactors = FALSE
    )

    episode_summary_parts[[j]] <- data.frame(
      series_id = series_id,
      series_label = series_label,
      episode_id = id,
      episode_label = episode_label,
      phase = phase,
      start_year = year[rows[[1]]],
      end_year = year[rows[[length(rows)]]],
      start_date = date[rows[[1]]],
      end_date = date[rows[[length(rows)]]],
      duration_years = length(transition_indices),
      average_growth = mean(growth_rate[transition_indices], na.rm = TRUE),
      cumulative_growth = (index_value[rows[[length(rows)]]] / index_value[rows[[1]]]) - 1,
      start_index_value = index_value[rows[[1]]],
      end_index_value = index_value[rows[[length(rows)]]],
      episode_index_end_100 = episode_index_100[[length(episode_index_100)]],
      stringsAsFactors = FALSE
    )
  }

  list(
    episode_path = do.call(rbind, episode_path_parts),
    episode_summary = do.call(rbind, episode_summary_parts)
  )
}

gdp_outputs <- build_episode_outputs(
  year = clean_data$year,
  date = clean_data$date,
  growth_rate = clean_data$gdp_growth,
  index_value = clean_data$gdp_index,
  series_id = "gdp",
  series_label = "Real GDP"
)

gdp_pc_outputs <- build_episode_outputs(
  year = clean_data$year,
  date = clean_data$date,
  growth_rate = clean_data$gdp_pc_growth,
  index_value = clean_data$gdp_pc_index,
  series_id = "gdp_per_capita",
  series_label = "Real GDP per capita"
)

episode_outputs <- list(
  episode_path = rbind(gdp_outputs$episode_path, gdp_pc_outputs$episode_path),
  episode_summary = rbind(gdp_outputs$episode_summary, gdp_pc_outputs$episode_summary)
)

utils::write.csv(episode_outputs$episode_summary, "data/interim/episode_summary.csv", row.names = FALSE)
utils::write.csv(episode_outputs$episode_path, "data/interim/episode_path.csv", row.names = FALSE)

message("Wrote data/interim/episode_summary.csv")
message("Wrote data/interim/episode_path.csv")
