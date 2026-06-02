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
    episode_cumulative_change <- (episode_index_100 / 100) - 1
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
      episode_year_number = year[rows] - year[rows[[1]]],
      episode_index_100 = episode_index_100,
      episode_cumulative_change = episode_cumulative_change,
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
      duration_years = year[rows[[length(rows)]]] - year[rows[[1]]],
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

maddison_data_path <- "data/raw/mpd2023_web.xlsx"
if (file.exists(maddison_data_path)) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("The `readxl` package is required to build Maddison episode outputs.", call. = FALSE)
  }

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The `dplyr` package is required to build Maddison episode outputs.", call. = FALSE)
  }

  build_maddison_country_episodes <- function(country_data) {
    country_data <- country_data[order(country_data$year), ]
    country_data <- country_data[!is.na(country_data$gdppc), ]

    if (nrow(country_data) < 2) {
      return(NULL)
    }

    year_gap <- country_data$year[-1] - country_data$year[-nrow(country_data)]
    country_data$growth_rate <- c(
      NA_real_,
      (country_data$gdppc[-1] / country_data$gdppc[-nrow(country_data)])^(1 / year_gap) - 1
    )

    country_outputs <- build_episode_outputs(
      year = country_data$year,
      date = as.Date(sprintf("%s-01-01", country_data$year)),
      growth_rate = country_data$growth_rate,
      index_value = country_data$gdppc,
      series_id = "maddison_gdp_per_capita",
      series_label = "Maddison GDP per capita"
    )

    for (output_name in names(country_outputs)) {
      country_outputs[[output_name]]$country_code <- country_data$countrycode[[1]]
      country_outputs[[output_name]]$country <- country_data$country[[1]]
      country_outputs[[output_name]]$region <- country_data$region[[1]]
      country_outputs[[output_name]] <- country_outputs[[output_name]][
        c(
          "country_code",
          "country",
          "region",
          setdiff(names(country_outputs[[output_name]]), c("country_code", "country", "region"))
        )
      ]
    }

    country_outputs
  }

  maddison_full_data <- readxl::read_excel(maddison_data_path, sheet = "Full data") |>
    dplyr::transmute(
      countrycode = as.character(countrycode),
      country = as.character(country),
      region = as.character(region),
      year = as.integer(year),
      gdppc = as.numeric(gdppc)
    ) |>
    dplyr::filter(!is.na(countrycode), !is.na(country), !is.na(year), !is.na(gdppc))

  maddison_country_groups <- split(maddison_full_data, maddison_full_data$countrycode)
  maddison_country_outputs <- lapply(maddison_country_groups, build_maddison_country_episodes)
  maddison_country_outputs <- maddison_country_outputs[!vapply(maddison_country_outputs, is.null, logical(1))]

  maddison_episode_path <- do.call(
    rbind,
    lapply(maddison_country_outputs, `[[`, "episode_path")
  )
  maddison_episode_summary <- do.call(
    rbind,
    lapply(maddison_country_outputs, `[[`, "episode_summary")
  )

  utils::write.csv(
    maddison_episode_summary,
    "data/interim/maddison_gdp_per_capita_episode_summary.csv",
    row.names = FALSE
  )
  utils::write.csv(
    maddison_episode_path,
    "data/interim/maddison_gdp_per_capita_episode_path.csv",
    row.names = FALSE
  )

  message("Wrote data/interim/maddison_gdp_per_capita_episode_summary.csv")
  message("Wrote data/interim/maddison_gdp_per_capita_episode_path.csv")
} else {
  message("Skipping Maddison episode outputs; raw workbook not found at ", maddison_data_path)
}

wdi_real_gdp_growth_path <- "data/raw/wdi_real_gdp_growth_maddison_countries.csv"
if (file.exists(wdi_real_gdp_growth_path)) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The `dplyr` package is required to build WDI episode outputs.", call. = FALSE)
  }

  build_wdi_country_episodes <- function(country_data, series_id, series_label) {
    country_data <- country_data[order(country_data$year), ]
    country_data <- country_data[!is.na(country_data$growth_rate), ]

    if (nrow(country_data) < 2) {
      return(NULL)
    }

    country_data$index_value <- cumprod(1 + country_data$growth_rate) * 100

    country_outputs <- build_episode_outputs(
      year = country_data$year,
      date = as.Date(sprintf("%s-01-01", country_data$year)),
      growth_rate = country_data$growth_rate,
      index_value = country_data$index_value,
      series_id = series_id,
      series_label = series_label
    )

    for (output_name in names(country_outputs)) {
      country_outputs[[output_name]]$country_code <- country_data$country_code[[1]]
      country_outputs[[output_name]]$country <- country_data$country[[1]]
      country_outputs[[output_name]]$maddison_country <- country_data$maddison_country[[1]]
      country_outputs[[output_name]]$region <- country_data$maddison_region[[1]]
      country_outputs[[output_name]] <- country_outputs[[output_name]][
        c(
          "country_code",
          "country",
          "maddison_country",
          "region",
          setdiff(
            names(country_outputs[[output_name]]),
            c("country_code", "country", "maddison_country", "region")
          )
        )
      ]
    }

    country_outputs
  }

  wdi_real_gdp_growth <- utils::read.csv(wdi_real_gdp_growth_path, stringsAsFactors = FALSE) |>
    dplyr::transmute(
      country_code = as.character(country_code),
      country = as.character(country),
      maddison_country = as.character(maddison_country),
      maddison_region = as.character(maddison_region),
      year = as.integer(year),
      growth_rate = as.numeric(value) / 100
    ) |>
    dplyr::filter(!is.na(country_code), !is.na(year), !is.na(growth_rate))

  wdi_country_groups <- split(wdi_real_gdp_growth, wdi_real_gdp_growth$country_code)
  wdi_country_outputs <- lapply(
    wdi_country_groups,
    build_wdi_country_episodes,
    series_id = "wdi_real_gdp_growth",
    series_label = "WDI real GDP growth"
  )
  wdi_country_outputs <- wdi_country_outputs[!vapply(wdi_country_outputs, is.null, logical(1))]

  wdi_episode_path <- do.call(
    rbind,
    lapply(wdi_country_outputs, `[[`, "episode_path")
  )
  wdi_episode_summary <- do.call(
    rbind,
    lapply(wdi_country_outputs, `[[`, "episode_summary")
  )

  utils::write.csv(
    wdi_episode_summary,
    "data/interim/wdi_real_gdp_growth_episode_summary.csv",
    row.names = FALSE
  )
  utils::write.csv(
    wdi_episode_path,
    "data/interim/wdi_real_gdp_growth_episode_path.csv",
    row.names = FALSE
  )

  message("Wrote data/interim/wdi_real_gdp_growth_episode_summary.csv")
  message("Wrote data/interim/wdi_real_gdp_growth_episode_path.csv")
} else {
  message("Skipping WDI real GDP growth episode outputs; raw CSV not found at ", wdi_real_gdp_growth_path)
}

wdi_real_gdp_pc_growth_path <- "data/raw/wdi_real_gdp_per_capita_growth_maddison_countries.csv"
if (file.exists(wdi_real_gdp_pc_growth_path)) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The `dplyr` package is required to build WDI per-capita episode outputs.", call. = FALSE)
  }

  if (!exists("build_wdi_country_episodes")) {
    stop("WDI episode builder is unavailable.", call. = FALSE)
  }

  wdi_real_gdp_pc_growth <- utils::read.csv(wdi_real_gdp_pc_growth_path, stringsAsFactors = FALSE) |>
    dplyr::transmute(
      country_code = as.character(country_code),
      country = as.character(country),
      maddison_country = as.character(maddison_country),
      maddison_region = as.character(maddison_region),
      year = as.integer(year),
      growth_rate = as.numeric(value) / 100
    ) |>
    dplyr::filter(!is.na(country_code), !is.na(year), !is.na(growth_rate))

  wdi_pc_country_groups <- split(wdi_real_gdp_pc_growth, wdi_real_gdp_pc_growth$country_code)
  wdi_pc_country_outputs <- lapply(
    wdi_pc_country_groups,
    build_wdi_country_episodes,
    series_id = "wdi_real_gdp_per_capita_growth",
    series_label = "WDI real GDP per capita growth"
  )
  wdi_pc_country_outputs <- wdi_pc_country_outputs[!vapply(wdi_pc_country_outputs, is.null, logical(1))]

  wdi_pc_episode_path <- do.call(
    rbind,
    lapply(wdi_pc_country_outputs, `[[`, "episode_path")
  )
  wdi_pc_episode_summary <- do.call(
    rbind,
    lapply(wdi_pc_country_outputs, `[[`, "episode_summary")
  )

  utils::write.csv(
    wdi_pc_episode_summary,
    "data/interim/wdi_real_gdp_per_capita_growth_episode_summary.csv",
    row.names = FALSE
  )
  utils::write.csv(
    wdi_pc_episode_path,
    "data/interim/wdi_real_gdp_per_capita_growth_episode_path.csv",
    row.names = FALSE
  )

  message("Wrote data/interim/wdi_real_gdp_per_capita_growth_episode_summary.csv")
  message("Wrote data/interim/wdi_real_gdp_per_capita_growth_episode_path.csv")
} else {
  message("Skipping WDI real GDP per-capita growth episode outputs; raw CSV not found at ", wdi_real_gdp_pc_growth_path)
}

imf_weo_gdp_pc_growth_path <- "data/raw/imf_weo_gdp_per_capita_growth_maddison_countries.csv"
if (file.exists(imf_weo_gdp_pc_growth_path)) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The `dplyr` package is required to build IMF WEO episode outputs.", call. = FALSE)
  }

  build_imf_weo_country_episodes <- function(country_data) {
    country_data <- country_data[order(country_data$year), ]
    country_data <- country_data[!is.na(country_data$growth_rate), ]

    if (nrow(country_data) < 2) {
      return(NULL)
    }

    country_data$index_value <- cumprod(1 + country_data$growth_rate) * 100

    country_outputs <- build_episode_outputs(
      year = country_data$year,
      date = as.Date(sprintf("%s-01-01", country_data$year)),
      growth_rate = country_data$growth_rate,
      index_value = country_data$index_value,
      series_id = "imf_weo_gdp_per_capita_growth",
      series_label = "IMF WEO real GDP per capita growth"
    )

    for (output_name in names(country_outputs)) {
      country_outputs[[output_name]]$country_code <- country_data$country_code[[1]]
      country_outputs[[output_name]]$maddison_country <- country_data$maddison_country[[1]]
      country_outputs[[output_name]]$region <- country_data$maddison_region[[1]]
      country_outputs[[output_name]] <- country_outputs[[output_name]][
        c(
          "country_code",
          "maddison_country",
          "region",
          setdiff(
            names(country_outputs[[output_name]]),
            c("country_code", "maddison_country", "region")
          )
        )
      ]
    }

    country_outputs
  }

  imf_weo_gdp_pc_growth <- utils::read.csv(imf_weo_gdp_pc_growth_path, stringsAsFactors = FALSE) |>
    dplyr::transmute(
      country_code = as.character(country_code),
      maddison_country = as.character(maddison_country),
      maddison_region = as.character(maddison_region),
      year = as.integer(year),
      growth_rate = as.numeric(value) / 100
    ) |>
    dplyr::filter(!is.na(country_code), !is.na(year), !is.na(growth_rate))

  imf_weo_country_groups <- split(imf_weo_gdp_pc_growth, imf_weo_gdp_pc_growth$country_code)
  imf_weo_country_outputs <- lapply(imf_weo_country_groups, build_imf_weo_country_episodes)
  imf_weo_country_outputs <- imf_weo_country_outputs[!vapply(imf_weo_country_outputs, is.null, logical(1))]

  imf_weo_episode_path <- do.call(
    rbind,
    lapply(imf_weo_country_outputs, `[[`, "episode_path")
  )
  imf_weo_episode_summary <- do.call(
    rbind,
    lapply(imf_weo_country_outputs, `[[`, "episode_summary")
  )

  utils::write.csv(
    imf_weo_episode_summary,
    "data/interim/imf_weo_gdp_per_capita_growth_episode_summary.csv",
    row.names = FALSE
  )
  utils::write.csv(
    imf_weo_episode_path,
    "data/interim/imf_weo_gdp_per_capita_growth_episode_path.csv",
    row.names = FALSE
  )

  message("Wrote data/interim/imf_weo_gdp_per_capita_growth_episode_summary.csv")
  message("Wrote data/interim/imf_weo_gdp_per_capita_growth_episode_path.csv")
} else {
  message("Skipping IMF WEO GDP per capita growth episode outputs; raw CSV not found at ", imf_weo_gdp_pc_growth_path)
}
