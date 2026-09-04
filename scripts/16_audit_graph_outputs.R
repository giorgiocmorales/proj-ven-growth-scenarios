# Build a structural audit of graph scripts and expected figure outputs.

# Setup ----
# Validate the packages required to inspect graph scripts and their outputs.
required_packages <- c("dplyr", "magrittr", "stringr", "tibble")
missing_packages <- required_packages[!vapply(
  required_packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)]

if (length(missing_packages) > 0) {
  stop(
    sprintf("Missing required packages: %s", paste(missing_packages, collapse = ", ")),
    call. = FALSE
  )
}

`%>%` <- magrittr::`%>%`

# Inputs ----
# Keep the audited scope explicit so graph navigation stays reviewable.
graph_scripts <- c(
  "scripts/10_graph_historical_recovery.R",
  "scripts/11_graph_wilks_composite_correlation.R",
  "scripts/12_graph_imf_weo_ppp_scatter.R",
  "scripts/13_graph_owid_development_indicators.R"
)
presentation_qmd <- "reports/presentation/index.qmd"

missing_scripts <- graph_scripts[!file.exists(graph_scripts)]
if (length(missing_scripts) > 0) {
  stop(
    sprintf("Missing graph scripts: %s", paste(missing_scripts, collapse = ", ")),
    call. = FALSE
  )
}

if (!file.exists(presentation_qmd)) {
  stop(sprintf("Missing Quarto presentation at `%s`.", presentation_qmd), call. = FALSE)
}

source("scripts/_presentation_assets.R")
presentation_asset_manifest <- validate_presentation_assets(
  presentation_qmd,
  c("svg", "pdf")
)

audit_output_path <- "outputs/graph_output_audit.csv"
dir.create(dirname(audit_output_path), recursive = TRUE, showWarnings = FALSE)

# Audit helpers ----
# Parse save calls, presentation references, and image metadata into audit tables.
# Extract indexed graph headings, family headings, and literal PNG output paths.
audit_graph_script <- function(script_path) {
  script_lines <- readLines(script_path, warn = FALSE, encoding = "UTF-8")
  current_family <- NA_character_
  current_graph <- NA_character_
  audit_rows <- list()

  for (line_number in seq_along(script_lines)) {
    line_text <- script_lines[[line_number]]
    family_match <- stringr::str_match(line_text, "^## Family:\\s*(.+?)\\s*----\\s*$")
    graph_match <- stringr::str_match(
      line_text,
      "^### Graph\\s+[0-9]{2}(?:\\s+\\(draft\\))?:\\s*(.+?)\\s*----\\s*$"
    )
    output_match <- stringr::str_match(
      line_text,
      "file\\.path\\(figure_dir,\\s*\"([^\"]+\\.png)\"\\)"
    )

    if (!is.na(family_match[[2]])) {
      current_family <- family_match[[2]]
    }

    if (!is.na(graph_match[[2]])) {
      current_graph <- graph_match[[2]]
    }

    if (!is.na(output_match[[2]])) {
      audit_rows[[length(audit_rows) + 1L]] <- tibble::tibble(
        script_path = script_path,
        line_number = line_number,
        family = current_family,
        graph_label = current_graph,
        output_pattern = file.path("outputs/figures", output_match[[2]])
      )
    }
  }

  if (length(audit_rows) == 0) {
    return(tibble::tibble(
      script_path = character(),
      line_number = integer(),
      family = character(),
      graph_label = character(),
      output_pattern = character()
    ))
  }

  dplyr::bind_rows(audit_rows)
}

# Audit construction ----
# Summarize every graph save call and mark records that need navigation cleanup.
graph_output_audit <- dplyr::bind_rows(lapply(graph_scripts, audit_graph_script)) %>%
  dplyr::group_by(script_path, output_pattern) %>%
  dplyr::slice_min(order_by = line_number, n = 1L, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    has_family = !is.na(family) & nzchar(family),
    has_graph_label = !is.na(graph_label) & nzchar(graph_label),
    has_output_pattern = !is.na(output_pattern) & nzchar(output_pattern),
    output_stem = tools::file_path_sans_ext(output_pattern),
    png_exists = has_output_pattern & file.exists(output_pattern),
    svg_exists = has_output_pattern & file.exists(paste0(output_stem, ".svg")),
    pdf_exists = has_output_pattern & file.exists(paste0(output_stem, ".pdf")),
    output_exists = png_exists & svg_exists & pdf_exists,
    referenced_by_presentation = basename(output_stem) %in%
      basename(presentation_asset_manifest$reference),
    audit_status = dplyr::case_when(
      !has_graph_label ~ "missing_graph_label",
      !has_family ~ "missing_family",
      !has_output_pattern ~ "missing_output_pattern",
      output_exists %in% FALSE ~ "output_not_found",
      TRUE ~ "ok"
    )
  ) %>%
  dplyr::select(
    script_path,
    line_number,
    family,
    graph_label,
    output_pattern,
    png_exists,
    svg_exists,
    pdf_exists,
    referenced_by_presentation,
    audit_status
  )

# Output ----
# Write a compact CSV that can be reviewed after graph edits.
utils::write.csv(graph_output_audit, audit_output_path, row.names = FALSE)

message("Wrote ", audit_output_path)
message("Audited graph save calls: ", nrow(graph_output_audit))
message("Validated presentation image assets: ", nrow(presentation_asset_manifest))
message(
  "Audit statuses: ",
  paste(
    sprintf(
      "%s=%s",
      names(table(graph_output_audit$audit_status)),
      as.integer(table(graph_output_audit$audit_status))
    ),
    collapse = ", "
  )
)
