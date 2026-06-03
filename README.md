# Venezuela Economic Recovery Simulator

This repository contains the analytical engine, processed data products, public-facing dashboard scaffold, and report scaffold for a project on Venezuelan economic recovery scenarios.

The project is built around one central question:

How long would it take Venezuela to recover the real GDP or real GDP per capita level of a user-selected benchmark year under a given annual growth rate, and how historically plausible is sustaining that path?

## Project Principles

- The analytical engine lives in `R/`.
- Pipeline entry points live in `scripts/`.
- The app and report consume processed outputs, not raw Excel files.
- Raw inputs are kept in `data/raw/`.
- Historical plausibility is presented as a transparent historical-frequency concept, not as a forecast probability.

## Current Scaffold

The repository is organized into five layers:

1. Raw and processed data in `data/`
2. Reusable analytical functions in `R/`
3. Reproducible pipeline scripts in `scripts/`
4. Presentation layer in `app/`
5. Public article/report in `reports/article-es/`

## Directory Guide

- `data/raw/`: manual source inputs and data notes
- `data/interim/`: cleaned and intermediate historical tables
- `data/final/`: processed CSV outputs for the app and report
- `R/`: reusable analytical functions
- `scripts/`: ordered data-build scripts
- `app/`: dashboard scaffold, modules, static assets, and i18n files
- `reports/article-es/`: Spanish article/report scaffold
- `references/`: source references, including the Tableau prototype
- `tests/testthat/`: unit tests for the analytical engine

## Planned Workflow

1. Read the Excel master file from `data/raw/`
2. Clean and validate the historical series
3. Recompute all derived indicators in R
4. Export app-ready CSV files to `data/final/`
5. Consume those outputs in the dashboard and report

Current pipeline sequence:

- `scripts/00_set_up.R`
- `scripts/01_build_clean_data.R`
- `scripts/02_build_series.R`
- `scripts/02_download_wdi_real_gdp_growth.R`
- `scripts/02_download_imf_weo_gdp_pc_growth.R`
- `scripts/03_build_episodes.R`
- `scripts/04_build_plausibility.R`
- `scripts/05_build_app_data.R`
- `scripts/08_graphs.R`
- `scripts/09_wilks_composite_correlation.R`
- `scripts/10_imf_weo_ppp_scatter.R`
- `scripts/11_owid_development_indicators.R`
- `scripts/06_render_report.R`
- `scripts/07_run_app.R`
- `scripts/99_run_all.R`

## Legacy Material

The original exploratory script in `scripts/01_legacy.R` is preserved for reference during the transition to the new architecture.
