# Resolve from the testthat working directory back to the repository root.
project_root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)

testthat::test_that("pipeline outputs exist after running the build scripts", {
  testthat::expect_true(file.exists(file.path(project_root, "data", "interim", "clean_historical_data.csv")))
  testthat::expect_true(file.exists(file.path(project_root, "data", "interim", "index_series_long.csv")))
  testthat::expect_true(file.exists(file.path(project_root, "data", "interim", "episode_summary.csv")))
  testthat::expect_true(file.exists(file.path(
    project_root,
    "data",
    "final",
    "maddison_gdp_per_capita_episode_path.csv"
  )))
  testthat::expect_true(file.exists(file.path(
    project_root,
    "data",
    "final",
    "maddison_gdp_per_capita_episode_summary.csv"
  )))
  testthat::expect_true(file.exists(file.path(
    project_root,
    "data",
    "final",
    "wdi_real_gdp_growth_episode_path.csv"
  )))
  testthat::expect_true(file.exists(file.path(
    project_root,
    "data",
    "final",
    "wdi_real_gdp_growth_episode_summary.csv"
  )))
  testthat::expect_true(file.exists(file.path(
    project_root,
    "data",
    "final",
    "imf_weo_gdp_per_capita_growth_episode_path.csv"
  )))
  testthat::expect_true(file.exists(file.path(
    project_root,
    "data",
    "final",
    "imf_weo_gdp_per_capita_growth_episode_summary.csv"
  )))
  testthat::expect_true(file.exists(file.path(project_root, "data", "final", "plausibility_metrics.csv")))
  testthat::expect_true(file.exists(file.path(project_root, "data", "final", "simulation_summary.csv")))
})

testthat::test_that("pipeline outputs have expected core columns", {
  # Read the core tables once, then assert schema and internal consistency.
  clean_data <- utils::read.csv(
    file.path(project_root, "data", "interim", "clean_historical_data.csv"),
    stringsAsFactors = FALSE
  )
  series_data <- utils::read.csv(
    file.path(project_root, "data", "interim", "index_series_long.csv"),
    stringsAsFactors = FALSE
  )
  episodes <- utils::read.csv(
    file.path(project_root, "data", "interim", "episode_summary.csv"),
    stringsAsFactors = FALSE
  )
  plausibility <- utils::read.csv(
    file.path(project_root, "data", "final", "plausibility_metrics.csv"),
    stringsAsFactors = FALSE
  )
  maddison_episodes <- utils::read.csv(
    file.path(project_root, "data", "final", "maddison_gdp_per_capita_episode_path.csv"),
    stringsAsFactors = FALSE
  )
  wdi_episodes <- utils::read.csv(
    file.path(project_root, "data", "final", "wdi_real_gdp_growth_episode_path.csv"),
    stringsAsFactors = FALSE
  )
  imf_weo_episodes <- utils::read.csv(
    file.path(project_root, "data", "final", "imf_weo_gdp_per_capita_growth_episode_path.csv"),
    stringsAsFactors = FALSE
  )

  testthat::expect_true(all(c("date", "year", "gdp_growth", "gdp_index") %in% names(clean_data)))
  testthat::expect_true(all(c("series_id", "year", "index_value") %in% names(series_data)))
  testthat::expect_true(all(c("series_id", "phase", "start_year", "end_year") %in% names(episodes)))
  testthat::expect_true(all(c("series_id", "yearly_label", "streak_label") %in% names(plausibility)))
  testthat::expect_true(all(c(
    "country_code",
    "country",
    "region",
    "episode_cumulative_change"
  ) %in% names(maddison_episodes)))
  testthat::expect_true(all(c(
    "country_code",
    "country",
    "maddison_country",
    "region",
    "episode_cumulative_change"
  ) %in% names(wdi_episodes)))
  testthat::expect_true(all(c(
    "country_code",
    "maddison_country",
    "region",
    "episode_cumulative_change"
  ) %in% names(imf_weo_episodes)))
  testthat::expect_true(nrow(clean_data) > 0)
  testthat::expect_true(nrow(series_data) > 0)
  testthat::expect_true(nrow(maddison_episodes) > 0)
  testthat::expect_true(nrow(wdi_episodes) > 0)
  testthat::expect_true(nrow(imf_weo_episodes) > 0)
  testthat::expect_equal(
    maddison_episodes$episode_cumulative_change,
    maddison_episodes$episode_index_100 / 100 - 1,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    wdi_episodes$episode_cumulative_change,
    wdi_episodes$episode_index_100 / 100 - 1,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    imf_weo_episodes$episode_cumulative_change,
    imf_weo_episodes$episode_index_100 / 100 - 1,
    tolerance = 1e-12
  )
})
