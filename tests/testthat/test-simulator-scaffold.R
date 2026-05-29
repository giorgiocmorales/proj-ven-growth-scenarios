project_root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)

testthat::test_that("pipeline outputs exist after running the build scripts", {
  testthat::expect_true(file.exists(file.path(project_root, "data", "interim", "clean_historical_data.csv")))
  testthat::expect_true(file.exists(file.path(project_root, "data", "interim", "index_series_long.csv")))
  testthat::expect_true(file.exists(file.path(project_root, "data", "interim", "episode_summary.csv")))
  testthat::expect_true(file.exists(file.path(project_root, "data", "final", "plausibility_metrics.csv")))
  testthat::expect_true(file.exists(file.path(project_root, "data", "final", "simulation_summary.csv")))
})

testthat::test_that("pipeline outputs have expected core columns", {
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

  testthat::expect_true(all(c("date", "year", "gdp_growth", "gdp_index") %in% names(clean_data)))
  testthat::expect_true(all(c("series_id", "year", "index_value") %in% names(series_data)))
  testthat::expect_true(all(c("series_id", "phase", "start_year", "end_year") %in% names(episodes)))
  testthat::expect_true(all(c("series_id", "yearly_label", "streak_label") %in% names(plausibility)))
  testthat::expect_true(nrow(clean_data) > 0)
  testthat::expect_true(nrow(series_data) > 0)
})
