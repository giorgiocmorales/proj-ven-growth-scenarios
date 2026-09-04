# Validate local image references used by the Quarto presentation.

extract_presentation_image_references <- function(input_file) {
  # Parse local Markdown image references once for every presentation format.
  input_text <- paste(readLines(input_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  image_pattern <- "!\\[[^]]*\\]\\(([^)]+)\\)"
  image_matches <- regmatches(input_text, gregexpr(image_pattern, input_text, perl = TRUE))[[1]]

  if (length(image_matches) == 0) {
    return(character())
  }

  image_references <- sub(image_pattern, "\\1", image_matches, perl = TRUE)
  image_references <- utils::URLdecode(image_references)
  unique(image_references[!grepl("^(https?|data):", image_references, perl = TRUE)])
}

build_presentation_asset_manifest <- function(input_file, image_extensions) {
  # Resolve extensionless charts per format and preserve explicit image formats.
  image_references <- extract_presentation_image_references(input_file)
  input_dir <- dirname(input_file)

  manifest_rows <- lapply(
    image_references,
    function(image_reference) {
      reference_extension <- tools::file_ext(image_reference)
      resolved_extensions <- if (nzchar(reference_extension)) {
        reference_extension
      } else {
        image_extensions
      }
      resolved_references <- if (nzchar(reference_extension)) {
        image_reference
      } else {
        paste0(image_reference, ".", resolved_extensions)
      }

      data.frame(
        reference = image_reference,
        format = resolved_extensions,
        resolved_reference = resolved_references,
        stringsAsFactors = FALSE
      )
    }
  )
  asset_manifest <- do.call(rbind, manifest_rows)
  asset_manifest$absolute_path <- vapply(
    asset_manifest$resolved_reference,
    function(resolved_reference) {
      normalizePath(
        file.path(input_dir, resolved_reference),
        winslash = "/",
        mustWork = FALSE
      )
    },
    character(1)
  )
  asset_manifest$exists <- file.exists(asset_manifest$absolute_path)

  asset_manifest
}

validate_presentation_assets <- function(input_file, image_extensions) {
  # Fail before rendering when any image referenced by Quarto is unavailable.
  asset_manifest <- build_presentation_asset_manifest(input_file, image_extensions)
  missing_references <- asset_manifest$resolved_reference[!asset_manifest$exists]

  if (length(missing_references) > 0) {
    stop(
      paste(
        "Presentation image preflight failed. Missing referenced files:",
        paste(sprintf("- %s", missing_references), collapse = "\n"),
        sep = "\n"
      ),
      call. = FALSE
    )
  }

  message("Validated presentation image assets: ", nrow(asset_manifest))
  invisible(asset_manifest)
}
