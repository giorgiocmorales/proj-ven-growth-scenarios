# Verify source retention, rate units, and recovery compounding independently.
comparison_root <- normalizePath(file.path("..", ".."), winslash = "/", mustWork = TRUE)

testthat::test_that("Venezuela comparison preserves source rates and missing coverage", {
  comparison <- utils::read.csv(file.path(comparison_root, "data/final/venezuela_source_growth_comparison.csv"))
  project <- utils::read.csv(file.path(comparison_root, "data/interim/clean_historical_data.csv"))
  wdi <- utils::read.csv(file.path(comparison_root, "data/raw/wdi_real_gdp_growth_venezuela.csv"))
  imf <- utils::read.csv(file.path(comparison_root, "data/raw/imf_weo_real_gdp_growth_population_venezuela.csv"))
  gdp <- comparison[comparison$series_id == "gdp", ]
  testthat::expect_false(anyDuplicated(comparison[c("series_id", "year")]) > 0)
  testthat::expect_true(all(wdi$country_code == "VEN"))
  testthat::expect_true(all(imf$country_code == "VEN"))
  testthat::expect_equal(gdp$project_growth_pct[match(project$year, gdp$year)], project$gdp_growth * 100)
  testthat::expect_equal(gdp$world_bank_growth_pct[match(wdi$year, gdp$year)], wdi$value)
  imf_gdp <- imf[imf$indicator_id == "NGDP_RPCH", ]
  testthat::expect_equal(gdp$imf_growth_pct[match(imf_gdp$year, gdp$year)], imf_gdp$value)
  testthat::expect_equal(comparison$imf_minus_project_pp, comparison$imf_growth_pct - comparison$project_growth_pct)
  forecast <- gdp[gdp$imf_period == "forecast", ]
  testthat::expect_gt(nrow(forecast), 0)
  testthat::expect_true(all(is.na(forecast$project_growth_pct)))
  testthat::expect_true(all(is.na(forecast$world_bank_growth_pct)))
})

testthat::test_that("BCV rates retain provenance and have no fabricated per-capita coverage", {
  comparison <- utils::read.csv(file.path(comparison_root, "data/final/venezuela_source_growth_comparison.csv"))
  bcv <- utils::read.csv(file.path(comparison_root, "data/raw/bcv_real_gdp_growth_venezuela.csv"))
  gdp <- comparison[comparison$series_id == "gdp", ]
  bcv_rows <- gdp[match(bcv$year, gdp$year), ]
  testthat::expect_equal(bcv_rows$bcv_growth_pct, bcv$value)
  testthat::expect_identical(bcv_rows$bcv_provisional, bcv$provisional)
  testthat::expect_equal(bcv_rows$bcv_base_year, bcv$base_year)
  testthat::expect_identical(bcv_rows$bcv_method, bcv$method)
  testthat::expect_true(all(is.na(comparison$bcv_growth_pct[comparison$series_id == "gdp_per_capita"])))
  testthat::expect_true(all(is.na(gdp$bcv_growth_pct[!gdp$year %in% bcv$year])))
  recovery <- bcv_rows[bcv_rows$year >= 2012L, ]
  testthat::expect_equal(recovery$bcv_index_2012_100, c(100, 100 * cumprod(1 + recovery$bcv_growth_pct[-1] / 100)))
})

testthat::test_that("2012 recovery indices reconcile with project levels and IMF population", {
  comparison <- utils::read.csv(file.path(comparison_root, "data/final/venezuela_source_growth_comparison.csv"))
  project <- utils::read.csv(file.path(comparison_root, "data/interim/clean_historical_data.csv"))
  gdp <- comparison[comparison$series_id == "gdp" & comparison$year >= 2012 & !is.na(comparison$project_growth_pct), ]
  project_rows <- project[match(gdp$year, project$year), ]
  testthat::expect_equal(gdp$project_index_2012_100, project_rows$gdp_index / project$gdp_index[project$year == 2012] * 100)
  imf <- utils::read.csv(file.path(comparison_root, "data/raw/imf_weo_real_gdp_growth_population_venezuela.csv"))
  population <- imf[imf$indicator_id == "LP", ]
  imf_pc <- comparison[comparison$series_id == "gdp_per_capita" & !is.na(comparison$imf_growth_pct), ]
  current_pop <- population$value[match(imf_pc$year, population$year)]
  previous_pop <- population$value[match(imf_pc$year - 1L, population$year)]
  # Select by indicator before matching so component row order cannot affect rates.
  gdp_component <- imf[imf$indicator_id == "NGDP_RPCH", ]
  imf_gdp <- gdp_component$value[match(imf_pc$year, gdp_component$year)]
  testthat::expect_equal(imf_pc$imf_growth_pct, ((1 + imf_gdp / 100) * previous_pop / current_pop - 1) * 100)
})
