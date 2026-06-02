# Render report artifacts from processed outputs.
presentation_qmd <- "reports/presentation/index.qmd"

if (!file.exists(presentation_qmd)) {
  stop(sprintf("Missing Quarto presentation at `%s`.", presentation_qmd), call. = FALSE)
}

if (!requireNamespace("quarto", quietly = TRUE)) {
  stop("The `quarto` R package is required to render the presentation from R.", call. = FALSE)
}

quarto::quarto_render(presentation_qmd)
message("Rendered ", presentation_qmd)
