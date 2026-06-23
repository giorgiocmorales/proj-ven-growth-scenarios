# Run the full non-interactive reproduction pipeline from setup through report build.

## Setup ----
# Load dependencies and create output directories before running the pipeline.
source("scripts/00_set_up.R")

## Pipeline sequence ----
# Run data downloads, transformations, graph generation, and presentation render.
pipeline_scripts <- c(
  "scripts/01_download_wdi_real_gdp_growth.R",
  "scripts/02_download_imf_weo_gdp_pc_growth.R",
  "scripts/03_download_imf_weo_ppp_scatter_data.R",
  "scripts/04_download_owid_development_data.R",
  "scripts/05_build_clean_data.R",
  "scripts/06_build_series.R",
  "scripts/07_build_episodes.R",
  "scripts/08_build_plausibility.R",
  "scripts/09_build_app_data.R",
  "scripts/10_graph_historical_recovery.R",
  "scripts/11_graph_wilks_composite_correlation.R",
  "scripts/12_graph_imf_weo_ppp_scatter.R",
  "scripts/13_graph_owid_development_indicators.R",
  "scripts/14_render_presentation.R"
)

## Execution ----
# Source each script in order so intermediate objects are recreated from scratch.
for (script_path in pipeline_scripts) {
  message("Running ", script_path, " ...")
  source(script_path)
}

message("Completed reproduction pipeline.")
message("Interactive app launch is excluded; run scripts/15_run_app.R separately when needed.")
