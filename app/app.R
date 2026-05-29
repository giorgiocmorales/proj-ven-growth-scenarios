# Minimal dashboard entry point scaffold.
if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("The `shiny` package is required to run the app scaffold.", call. = FALSE)
}

ui <- shiny::fluidPage(
  shiny::titlePanel("Venezuela Economic Recovery Simulator"),
  shiny::p("App scaffold in place. Analytical engine and modules still need implementation.")
)

server <- function(input, output, session) {
}

shiny::shinyApp(ui = ui, server = server)
