# Script Editing Guide

This folder is organized as a reproducible pipeline. Use `scripts/99_run_all.R` to run the full non-interactive workflow, or run individual scripts while editing.

## Main Pipeline

- `00_set_up.R`: package checks and project folders.
- `01_build_clean_data.R`: raw historical data cleaning.
- `02_build_series.R`: derived Venezuela index series.
- `02_download_wdi_real_gdp_growth.R`: WDI real GDP growth download.
- `02_download_imf_weo_gdp_pc_growth.R`: IMF WEO GDP per capita growth download.
- `03_build_episodes.R`: growth/contraction episode tables.
- `04_build_plausibility.R`: plausibility metrics.
- `05_build_app_data.R`: final app/report data.
- `06_render_report.R`: Quarto report rendering.
- `07_run_app.R`: interactive app launch.

## Graph Scripts

- `08_graphs.R`: main presentation figure factory for historical series, episodes, recovery scenarios, and simulations.
- `09_wilks_composite_correlation.R`: composite-indicator simulation graph.
- `10_imf_weo_ppp_scatter.R`: IMF WEO scatterplots and Venezuela-relative comparisons.
- `11_owid_development_indicators.R`: OWID development relationship graphs.
- `_presentation_theme.R`: shared colors, captions, axis helpers, and figure saving.

## Manual Editing Notes

- Search for `## Plot construction` to find where graph objects are created.
- Search for `## Family:` to jump to a family of related graphs.
- Search for `# Graph:` to find a specific chart.
- Edit Spanish display text inside `ggplot2::labs()` and Quarto slide headings.
- Keep display text as UTF-8. Do not save scripts as Windows-1252 or ANSI.
- Generated PNG charts live in `reports/presentation/figures/`.
- Manually added presentation images should go in `reports/presentation/assets/images/`.
