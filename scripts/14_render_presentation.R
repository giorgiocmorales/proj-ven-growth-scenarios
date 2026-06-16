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
presentation_source_html <- file.path(presentation_dir, "index.html")
presentation_source_pdf <- file.path(presentation_dir, "index.pdf")
source_figure_prefix <- "../../outputs/figures/"
html_figure_prefix <- "../../figures/"
texlive_2025_repository <- "https://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2025/tlnet-final"

if (!file.exists(presentation_qmd)) {
  stop(sprintf("Missing Quarto presentation at `%s`.", presentation_qmd), call. = FALSE)
}

if (!requireNamespace("quarto", quietly = TRUE)) {
  stop("The `quarto` R package is required to render the presentation from R.", call. = FALSE)
}

ensure_latex_file_available <- function(file_name, package_name) {
  # TinyTeX installs can be minimal. Install specific packages only when missing.
  file_check <- suppressWarnings(system2("kpsewhich", file_name, stdout = TRUE, stderr = TRUE))
  if (length(file_check) > 0 && any(nzchar(file_check))) {
    return(invisible(TRUE))
  }

  message("LaTeX file `", file_name, "` not found; installing `", package_name, "` from TeX Live 2025 final repository.")
  status <- system2(
    "tlmgr",
    c("--repository", texlive_2025_repository, "install", package_name)
  )

  if (!identical(status, 0L)) {
    stop(
      paste(
        sprintf("Could not install the LaTeX `%s` package.", package_name),
        sprintf("Update TinyTeX/TeX Live or install `%s` manually before rendering the PDF.", package_name)
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

ensure_beamer_dependencies <- function() {
  ensure_latex_file_available("beamer.cls", "beamer")
  ensure_latex_file_available("spanish.ldf", "babel-spanish")
  ensure_latex_file_available("loadhyph-es.tex", "hyphen-spanish")
}

render_presentation_format <- function(output_format, source_output_file) {
  # Remove the previous in-place artifact so stale successful renders are obvious.
  if (file.exists(source_output_file)) {
    unlink(source_output_file)
  }

  quarto::quarto_render(
    input = presentation_qmd,
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
  copied_pdf <- file.copy(presentation_source_pdf, presentation_pdf_file, overwrite = TRUE)
  if (!isTRUE(copied_pdf)) {
    stop(
      sprintf(
        "Could not copy PDF to `%s`. Close any PDF viewer using the file and rerun this script.",
        presentation_pdf_file
      ),
      call. = FALSE
    )
  }
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

render_presentation_format("revealjs", presentation_source_html)
copy_presentation_html_bundle()

ensure_beamer_dependencies()
render_presentation_format("beamer", presentation_source_pdf)
copy_presentation_pdf()
remove_duplicate_figure_dirs()

message("Rendered presentation source: ", presentation_qmd)
message("Wrote definitive HTML: ", presentation_html_file)
message("Wrote definitive PDF: ", presentation_pdf_file)
