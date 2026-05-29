# Run the full non-interactive reproduction pipeline from setup through report build.
source("scripts/00_set_up.R")

pipeline_scripts <- c(
  "scripts/01_build_clean_data.R",
  "scripts/02_build_series.R",
  "scripts/03_build_episodes.R",
  "scripts/04_build_plausibility.R",
  "scripts/05_build_app_data.R",
  "scripts/06_render_report.R"
)

for (script_path in pipeline_scripts) {
  message("Running ", script_path, " ...")
  source(script_path)
}

message("Completed reproduction pipeline.")
message("Interactive app launch is excluded; run scripts/07_run_app.R separately when needed.")
