# Launch the dashboard entry point.

## Dependency check ------------------------------------------------------------
# Keep app launch failures focused on the missing runtime package.
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("The `shiny` package is required to run the app.", call. = FALSE)
}

## App launch ------------------------------------------------------------------
# Start the Shiny app from the project app directory.
shiny::runApp("app")
