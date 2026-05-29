# Launch the dashboard entry point.
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("The `shiny` package is required to run the app.", call. = FALSE)
}

shiny::runApp("app")
