# Render the Quarto presentation to the final HTML and PDF outputs.
#
# Quarto's Beamer format renders directly to PDF through LaTeX/Pandoc. The
# deck reads graphs from `outputs/figures`; this script copies only presentation
# artifacts to `outputs/presentation`.

presentation_qmd <- "reports/presentation/index.qmd"
presentation_dir <- dirname(presentation_qmd)
presentation_slug <- "el-largo-camino-por-delante"
presentation_outputs_dir <- "outputs/presentation"
presentation_html_dir <- file.path(presentation_outputs_dir, "html")
presentation_pdf_dir <- file.path(presentation_outputs_dir, "pdf")
presentation_html_file <- file.path(presentation_html_dir, paste0(presentation_slug, ".html"))
presentation_pdf_file <- file.path(presentation_pdf_dir, paste0(presentation_slug, ".pdf"))
presentation_pending_pdf_file <- file.path(presentation_pdf_dir, paste0(presentation_slug, ".pending.pdf"))
presentation_source_html <- file.path(presentation_dir, "index.html")
presentation_source_pdf <- file.path(presentation_dir, "index.pdf")
source_figure_prefix <- "../../outputs/figures/"
html_figure_prefix <- "../../figures/"

if (!file.exists(presentation_qmd)) {
  stop(sprintf("Missing Quarto presentation at `%s`.", presentation_qmd), call. = FALSE)
}

if (!requireNamespace("quarto", quietly = TRUE)) {
  stop("The `quarto` R package is required to render the presentation from R.", call. = FALSE)
}

source("scripts/_presentation_assets.R")

render_presentation_format <- function(output_format, source_output_file, input_file) {
  # Remove the previous in-place artifact so stale successful renders are obvious.
  if (file.exists(source_output_file)) {
    unlink(source_output_file)
  }

  quarto::quarto_render(
    input = input_file,
    output_format = output_format,
    output_file = basename(source_output_file),
    quiet = FALSE
  )

  if (!file.exists(source_output_file)) {
    stop(sprintf("Quarto did not create `%s`.", source_output_file), call. = FALSE)
  }

  invisible(source_output_file)
}

copy_presentation_html_bundle <- function() {
  # HTML needs its dependency bundle, CSS, and static images. Graphs stay in
  # outputs/figures and are referenced from the copied HTML.
  if (dir.exists(presentation_html_dir)) {
    unlink(presentation_html_dir, recursive = TRUE)
  }
  dir.create(presentation_html_dir, recursive = TRUE, showWarnings = FALSE)

  copied_html <- file.copy(presentation_source_html, presentation_html_file, overwrite = TRUE)
  if (!isTRUE(copied_html)) {
    stop(sprintf("Could not copy HTML to `%s`.", presentation_html_file), call. = FALSE)
  }

  html_lines <- readLines(presentation_html_file, warn = FALSE)
  html_lines <- gsub(source_figure_prefix, html_figure_prefix, html_lines, fixed = TRUE)
  writeLines(html_lines, presentation_html_file, useBytes = TRUE)

  bundle_items <- c("index_files", "title-sweep.css", "assets")
  for (item in bundle_items) {
    source_path <- file.path(presentation_dir, item)
    if (file.exists(source_path)) {
      invisible(file.copy(source_path, presentation_html_dir, recursive = TRUE, overwrite = TRUE))
    }
  }
}

copy_presentation_pdf <- function() {
  dir.create(presentation_pdf_dir, recursive = TRUE, showWarnings = FALSE)

  copied_pending_pdf <- file.copy(
    presentation_source_pdf,
    presentation_pending_pdf_file,
    overwrite = TRUE
  )
  if (!isTRUE(copied_pending_pdf)) {
    stop(
      sprintf("Could not stage the rendered PDF at `%s`.", presentation_pending_pdf_file),
      call. = FALSE
    )
  }

  copied_pdf <- file.copy(presentation_source_pdf, presentation_pdf_file, overwrite = TRUE)
  if (!isTRUE(copied_pdf)) {
    stop(
      sprintf(
        paste0(
          "Could not copy PDF to `%s`. Close any PDF viewer using the file and rerun this script. ",
          "The successful build is preserved at `%s`."
        ),
        presentation_pdf_file,
        presentation_pending_pdf_file
      ),
      call. = FALSE
    )
  }

  unlink(presentation_pending_pdf_file)
  invisible(TRUE)
}

remove_duplicate_figure_dirs <- function() {
  # Quarto and older render scripts may have left copied figure folders behind.
  stale_figure_dirs <- c(
    file.path(presentation_dir, "figures"),
    file.path(presentation_html_dir, "figures"),
    file.path(presentation_pdf_dir, "figures")
  )
  for (stale_dir in stale_figure_dirs) {
    if (dir.exists(stale_dir)) {
      unlink(stale_dir, recursive = TRUE)
    }
  }
}

clean_in_place_presentation_artifacts <- function() {
  # Keep reports/presentation source-only after definitive outputs are copied.
  stale_artifacts <- c(
    presentation_source_html,
    presentation_source_pdf,
    file.path(presentation_dir, "index.tex"),
    file.path(presentation_dir, "index.log"),
    file.path(presentation_dir, "index-pdf.qmd"),
    file.path(presentation_dir, "index-pdf.tex"),
    file.path(presentation_dir, "index-pdf.log"),
    file.path(presentation_dir, "index-pdf_files"),
    file.path(presentation_dir, "index.fdb_latexmk"),
    file.path(presentation_dir, "index_files"),
    file.path(presentation_dir, "figures"),
    file.path(presentation_dir, "outputs")
  )

  for (stale_path in stale_artifacts) {
    if (file.exists(stale_path) || dir.exists(stale_path)) {
      unlink(stale_path, recursive = TRUE)
    }
  }
}

clean_failed_presentation_artifacts <- function() {
  # Remove incomplete outputs while retaining TeX diagnostics for inspection.
  incomplete_artifacts <- c(
    presentation_source_html,
    presentation_source_pdf,
    file.path(presentation_dir, "index-pdf_files"),
    file.path(presentation_dir, "index_files"),
    file.path(presentation_dir, "figures"),
    file.path(presentation_dir, "outputs")
  )

  for (incomplete_path in incomplete_artifacts) {
    if (file.exists(incomplete_path) || dir.exists(incomplete_path)) {
      unlink(incomplete_path, recursive = TRUE)
    }
  }
}

render_succeeded <- FALSE
tryCatch(
  {
    validate_presentation_assets(presentation_qmd, c("svg", "pdf"))
    clean_in_place_presentation_artifacts()

    render_presentation_format("revealjs", presentation_source_html, presentation_qmd)
    copy_presentation_html_bundle()

    # Beamer embeds the validated vector PDF figure variants.
    render_presentation_format("beamer", presentation_source_pdf, presentation_qmd)
    copy_presentation_pdf()
    remove_duplicate_figure_dirs()
    render_succeeded <- TRUE
  },
  finally = {
    if (render_succeeded) {
      clean_in_place_presentation_artifacts()
    } else {
      clean_failed_presentation_artifacts()
    }
  }
)

message("Rendered presentation source: ", presentation_qmd)
message("Wrote definitive HTML: ", presentation_html_file)
message("Wrote definitive PDF: ", presentation_pdf_file)
