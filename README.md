# Venezuela Economic Recovery Simulator

This repository contains the data pipeline, processed data products, and Quarto presentation for a project on Venezuelan economic recovery scenarios. A public-facing dashboard and Spanish article remain scaffolds.

The project is built around one central question:

How long would it take Venezuela to recover the real GDP or real GDP per capita level of a user-selected benchmark year under a given annual growth rate, and how historically plausible is sustaining that path?

## Project Principles

- Data transformations and pipeline entry points live in `scripts/`.
- The app and report consume processed outputs, not raw Excel files.
- Raw inputs are kept in `data/raw/`.
- Historical plausibility is presented as a transparent historical-frequency concept, not as a forecast probability.

## Current Structure

The repository is organized into four layers:

1. Raw and processed data in `data/`
2. Reproducible pipeline scripts and shared presentation helpers in `scripts/`
3. Quarto presentation in `reports/presentation/`
4. Dashboard and article scaffolds in `app/` and `reports/article-es/`

## Directory Guide

- `data/raw/`: manual source inputs and data notes
- `data/interim/`: cleaned and intermediate historical tables
- `data/final/`: processed CSV outputs for the app and report
- `scripts/`: ordered data-build scripts
- `outputs/figures/`: generated PNG, SVG, and PDF figures in `default/` and `transparent/` subfolders
- `outputs/presentation/`: definitive rendered HTML and PDF presentation outputs
- `app/`: dashboard scaffold, homepage outline, and planned translations
- `reports/presentation/`: presentation source, CSS, and manually curated images
- `reports/article-es/`: Spanish article/report scaffold
- `references/`: source references, including the Tableau prototype
- `tests/testthat/`: processed-output schema and consistency checks

## Reproduction Workflow

Run `scripts/99_run_all.R` from the repository root to download source caches, build historical series and recovery metrics, export final CSVs, generate and audit graphs, and render the HTML and PDF presentation. Downloads require network access; rendering requires Quarto and a working LaTeX runtime.

See [scripts/README.md](scripts/README.md) for the ordered script guide and graph-editing workflow. During visual iteration, rerun only the affected graph scripts, the graph audit, and the presentation renderer.

The build scripts consume `data/interim/`; final CSVs in `data/final/` provide stable inputs for graphs and future app/report consumers. Matching tables across these folders are intentional pipeline products.

Launch the dashboard scaffold separately with `scripts/15_run_app.R`. Run the processed-data checks with `tests/testthat.R` after building the data.
