# Script Editing Guide

This folder is organized as a reproducible pipeline. Use `scripts/99_run_all.R`
to run the full non-interactive workflow, or run individual scripts while
editing.

## Main Pipeline

- `00_set_up.R`: package checks and project folders.
- `01_download_wdi_real_gdp_growth.R`: World Bank WDI GDP growth cache.
- `02_download_imf_weo_gdp_pc_growth.R`: IMF WEO GDP per capita growth cache.
- `03_download_imf_weo_ppp_scatter_data.R`: IMF WEO/Maddison scatterplot cache.
- `04_download_owid_development_data.R`: OWID development indicator cache.
- `05_build_clean_data.R`: raw historical workbook cleaning.
- `06_build_series.R`: derived Venezuela index series.
- `07_build_episodes.R`: domestic and international growth/contraction episodes.
- `08_build_plausibility.R`: recovery plausibility metrics.
- `09_build_app_data.R`: final app/report CSV assembly and validation.
- `10_graph_historical_recovery.R`: historical series, episodes, recovery scenarios, and simulations.
- `11_graph_wilks_composite_correlation.R`: composite-indicator simulation graph.
- `12_graph_imf_weo_ppp_scatter.R`: IMF WEO scatterplots and Venezuela-relative comparisons.
- `13_graph_owid_development_indicators.R`: OWID development relationship graphs.
- `14_render_presentation.R`: Quarto HTML and Beamer PDF rendering.
- `15_run_app.R`: interactive app launch.
- `99_run_all.R`: full non-interactive pipeline runner.

## Graph Scripts

- Search for `## Plot construction` to find where graph objects are created.
- Search for `## Family:` to jump to a family of related graphs.
- Search for `# Graph:` to find a specific chart.
- Every graph saved through `save_presentation_plot()` is also printed to the
  active R graphics device after the PNG is written.

## Manual Editing Notes

- Edit Spanish display text inside `ggplot2::labs()` and Quarto slide headings.
- Keep display text as UTF-8. Do not save scripts as Windows-1252 or ANSI.
- Generated PNG charts live in `outputs/figures/`.
- Manually added presentation images should go in `reports/presentation/assets/images/`.
- During visual iteration, rerun only graph scripts. Rerun download/cache scripts only when source data should be refreshed.
