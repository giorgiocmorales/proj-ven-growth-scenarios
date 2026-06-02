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
maddison_episodes <- NULL
maddison_episode_summary_path <- "data/interim/maddison_gdp_per_capita_episode_summary.csv"
maddison_episode_path_path <- "data/interim/maddison_gdp_per_capita_episode_path.csv"
if (file.exists(maddison_episode_summary_path) && file.exists(maddison_episode_path_path)) {
  maddison_episodes <- list(
    episode_summary = utils::read.csv(maddison_episode_summary_path, stringsAsFactors = FALSE),
    episode_path = utils::read.csv(maddison_episode_path_path, stringsAsFactors = FALSE)
  )
}
wdi_episodes <- NULL
wdi_episode_summary_path <- "data/interim/wdi_real_gdp_growth_episode_summary.csv"
wdi_episode_path_path <- "data/interim/wdi_real_gdp_growth_episode_path.csv"
if (file.exists(wdi_episode_summary_path) && file.exists(wdi_episode_path_path)) {
  wdi_episodes <- list(
    episode_summary = utils::read.csv(wdi_episode_summary_path, stringsAsFactors = FALSE),
    episode_path = utils::read.csv(wdi_episode_path_path, stringsAsFactors = FALSE)
  )
}
wdi_pc_episodes <- NULL
wdi_pc_episode_summary_path <- "data/interim/wdi_real_gdp_per_capita_growth_episode_summary.csv"
wdi_pc_episode_path_path <- "data/interim/wdi_real_gdp_per_capita_growth_episode_path.csv"
if (file.exists(wdi_pc_episode_summary_path) && file.exists(wdi_pc_episode_path_path)) {
  wdi_pc_episodes <- list(
    episode_summary = utils::read.csv(wdi_pc_episode_summary_path, stringsAsFactors = FALSE),
    episode_path = utils::read.csv(wdi_pc_episode_path_path, stringsAsFactors = FALSE)
  )
}
imf_weo_episodes <- NULL
imf_weo_episode_summary_path <- "data/interim/imf_weo_gdp_per_capita_growth_episode_summary.csv"
imf_weo_episode_path_path <- "data/interim/imf_weo_gdp_per_capita_growth_episode_path.csv"
if (file.exists(imf_weo_episode_summary_path) && file.exists(imf_weo_episode_path_path)) {
  imf_weo_episodes <- list(
    episode_summary = utils::read.csv(imf_weo_episode_summary_path, stringsAsFactors = FALSE),
    episode_path = utils::read.csv(imf_weo_episode_path_path, stringsAsFactors = FALSE)
  )
}
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
if (!is.null(maddison_episodes)) {
  utils::write.csv(
    maddison_episodes$episode_summary,
    "data/final/maddison_gdp_per_capita_episode_summary.csv",
    row.names = FALSE
  )
  utils::write.csv(
    maddison_episodes$episode_path,
    "data/final/maddison_gdp_per_capita_episode_path.csv",
    row.names = FALSE
  )
}
if (!is.null(wdi_episodes)) {
  utils::write.csv(
    wdi_episodes$episode_summary,
    "data/final/wdi_real_gdp_growth_episode_summary.csv",
    row.names = FALSE
  )
  utils::write.csv(
    wdi_episodes$episode_path,
    "data/final/wdi_real_gdp_growth_episode_path.csv",
    row.names = FALSE
  )
}
if (!is.null(wdi_pc_episodes)) {
  utils::write.csv(
    wdi_pc_episodes$episode_summary,
    "data/final/wdi_real_gdp_per_capita_growth_episode_summary.csv",
    row.names = FALSE
  )
  utils::write.csv(
    wdi_pc_episodes$episode_path,
    "data/final/wdi_real_gdp_per_capita_growth_episode_path.csv",
    row.names = FALSE
  )
}
if (!is.null(imf_weo_episodes)) {
  utils::write.csv(
    imf_weo_episodes$episode_summary,
    "data/final/imf_weo_gdp_per_capita_growth_episode_summary.csv",
    row.names = FALSE
  )
  utils::write.csv(
    imf_weo_episodes$episode_path,
    "data/final/imf_weo_gdp_per_capita_growth_episode_path.csv",
    row.names = FALSE
  )
}
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
      check = "maddison_episodes_available",
      passed = is.list(maddison_episodes),
      details = if (is.null(maddison_episodes)) {
        "Maddison episode outputs not available"
      } else {
        sprintf(
          "Rows in Maddison episode path: %s; summary: %s",
          nrow(maddison_episodes$episode_path),
          nrow(maddison_episodes$episode_summary)
        )
      },
      stringsAsFactors = FALSE
    ),
    data.frame(
      check = "wdi_real_gdp_growth_episodes_available",
      passed = is.list(wdi_episodes),
      details = if (is.null(wdi_episodes)) {
        "WDI real GDP growth episode outputs not available"
      } else {
        sprintf(
          "Rows in WDI real GDP growth episode path: %s; summary: %s",
          nrow(wdi_episodes$episode_path),
          nrow(wdi_episodes$episode_summary)
        )
      },
      stringsAsFactors = FALSE
    ),
    data.frame(
      check = "wdi_real_gdp_per_capita_growth_episodes_available",
      passed = is.list(wdi_pc_episodes),
      details = if (is.null(wdi_pc_episodes)) {
        "WDI real GDP per capita growth episode outputs not available"
      } else {
        sprintf(
          "Rows in WDI real GDP per capita growth episode path: %s; summary: %s",
          nrow(wdi_pc_episodes$episode_path),
          nrow(wdi_pc_episodes$episode_summary)
        )
      },
      stringsAsFactors = FALSE
    ),
    data.frame(
      check = "imf_weo_gdp_per_capita_growth_episodes_available",
      passed = is.list(imf_weo_episodes),
      details = if (is.null(imf_weo_episodes)) {
        "IMF WEO GDP per capita growth episode outputs not available"
      } else {
        sprintf(
          "Rows in IMF WEO GDP per capita growth episode path: %s; summary: %s",
          nrow(imf_weo_episodes$episode_path),
          nrow(imf_weo_episodes$episode_summary)
        )
      },
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
