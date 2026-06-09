# Render report artifacts from processed outputs.
presentation_qmd <- "reports/presentation/index.qmd"
presentation_dir <- dirname(presentation_qmd)
presentation_slug <- "el-largo-camino-por-delante"
presentation_outputs_dir <- "outputs/presentation"
presentation_html_dir <- file.path(presentation_outputs_dir, "html")
presentation_html_file <- file.path(presentation_html_dir, paste0(presentation_slug, ".html"))
presentation_pdf_file <- file.path(presentation_outputs_dir, paste0(presentation_slug, ".pdf"))

if (!file.exists(presentation_qmd)) {
  stop(sprintf("Missing Quarto presentation at `%s`.", presentation_qmd), call. = FALSE)
}

if (!requireNamespace("quarto", quietly = TRUE)) {
  stop("The `quarto` R package is required to render the presentation from R.", call. = FALSE)
}

find_chromium_browser <- function() {
  env_browser <- Sys.getenv("CHROMOTE_CHROME", unset = "")
  if (nzchar(env_browser) && file.exists(env_browser)) {
    return(env_browser)
  }

  common_browser_paths <- c(
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
  )
  browser_path <- common_browser_paths[file.exists(common_browser_paths)][1]
  if (!is.na(browser_path)) {
    return(browser_path)
  }

  if (requireNamespace("chromote", quietly = TRUE)) {
    browser_path <- tryCatch(
      suppressWarnings(chromote::find_chrome()),
      error = function(e) ""
    )
    if (nzchar(browser_path) && file.exists(browser_path)) {
      return(browser_path)
    }
  }

  stop(
    paste(
      "Could not find Chrome, Chromium, or Edge for PDF export.",
      "Install Quarto's chrome-headless-shell with `quarto install chrome-headless-shell`,",
      "or set CHROMOTE_CHROME to a Chromium-based browser executable."
    ),
    call. = FALSE
  )
}

copy_presentation_html_bundle <- function() {
  bundle_items <- c(
    "index.html",
    "title-sweep.css",
    "assets",
    "figures",
    "index_files"
  )

  if (dir.exists(presentation_html_dir)) {
    unlink(presentation_html_dir, recursive = TRUE)
  }
  dir.create(presentation_html_dir, recursive = TRUE, showWarnings = FALSE)

  file.copy(
    from = file.path(presentation_dir, "index.html"),
    to = presentation_html_file,
    overwrite = TRUE
  )

  for (item in setdiff(bundle_items, "index.html")) {
    source_path <- file.path(presentation_dir, item)
    if (file.exists(source_path)) {
      file.copy(
        from = source_path,
        to = presentation_html_dir,
        recursive = TRUE,
        overwrite = TRUE
      )
    }
  }
}

print_presentation_pdf <- function() {
  browser_path <- find_chromium_browser()
  html_path <- utils::shortPathName(normalizePath(
    presentation_html_file,
    winslash = "/",
    mustWork = TRUE
  ))
  pdf_dir <- utils::shortPathName(normalizePath(
    dirname(presentation_pdf_file),
    winslash = "/",
    mustWork = TRUE
  ))
  pdf_path <- file.path(pdf_dir, basename(presentation_pdf_file))
  html_url <- paste0("file:///", html_path, "?print-pdf")

  if (file.exists(presentation_pdf_file)) {
    unlink(presentation_pdf_file)
  }

  browser_args <- c(
      "--headless",
      "--disable-gpu",
      "--no-sandbox",
      "--no-pdf-header-footer",
      paste0("--print-to-pdf=", pdf_path),
      html_url
  )

  status <- system2(
    command = browser_path,
    args = shQuote(browser_args)
  )

  if (!identical(status, 0L)) {
    stop(sprintf("PDF export failed with status %s.", status), call. = FALSE)
  }

  previous_size <- 0
  for (attempt in seq_len(40)) {
    if (file.exists(presentation_pdf_file)) {
      current_size <- file.info(presentation_pdf_file)$size
      if (is.finite(current_size) && current_size > 10000 && current_size == previous_size) {
        return(invisible(presentation_pdf_file))
      }
      previous_size <- current_size
    }
    Sys.sleep(0.25)
  }

  stop(sprintf("PDF export did not create `%s`.", presentation_pdf_file), call. = FALSE)
}

dir.create(presentation_outputs_dir, recursive = TRUE, showWarnings = FALSE)

quarto::quarto_render(presentation_qmd, output_file = "index.html")
copy_presentation_html_bundle()
print_presentation_pdf()

message("Rendered presentation source: ", presentation_qmd)
message("Wrote definitive HTML bundle: ", presentation_html_dir)
message("Wrote definitive PDF: ", presentation_pdf_file)
