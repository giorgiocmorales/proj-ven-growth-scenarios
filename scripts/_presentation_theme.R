# Shared visual defaults for the Quarto presentation figures.

presentation_palette <- c(
  navy = "#003a5d",
  red = "#d70036",
  cyan = "#00acc8",
  green = "#157549",
  yellow = "#f1ba0d",
  orange = "#f26122"
)

presentation_ordered_colors <- unname(presentation_palette[c(
  "navy",
  "red",
  "cyan",
  "green",
  "yellow",
  "orange"
)])

presentation_colors <- c(
  primary = presentation_palette[["navy"]],
  secondary = presentation_palette[["red"]],
  venezuela = presentation_palette[["red"]],
  latam = presentation_palette[["navy"]],
  positive = presentation_palette[["navy"]],
  negative = presentation_palette[["red"]],
  reference = presentation_palette[["yellow"]],
  accent = presentation_palette[["orange"]],
  recovery = presentation_palette[["green"]],
  ink = "#2b3033",
  grid = "#d6dde2",
  muted = "#b8c1c8",
  light = "#edf2f5"
)

presentation_plot_width <- 16
presentation_plot_height <- 9
presentation_plot_dpi <- 120
presentation_vector_extensions <- c("png")

presentation_font_family <- "serif"
presentation_base_size <- 19
presentation_small_base_size <- 18
presentation_compact_base_size <- 17
presentation_blank_label <- " "
presentation_default_subtitle <- NULL
presentation_label_text_size <- 3.1
presentation_label_small_text_size <- 2.7
presentation_label_box_linewidth <- 0.15
presentation_label_padding <- ggplot2::unit(0.12, "lines")
presentation_label_radius <- ggplot2::unit(0.08, "lines")
presentation_label_segment_size <- 0.22
presentation_label_box_padding <- 0.45
presentation_label_point_padding <- 0.25

# Shared number labels use Spanish separators and one decimal for compact values.
presentation_number_label <- function(accuracy = 0.1, ...) {
  scales::label_number(
    accuracy = accuracy,
    big.mark = ".",
    decimal.mark = ",",
    trim = TRUE,
    ...
  )
}

presentation_dollar_label <- function(accuracy = 0.1, ...) {
  scales::label_number(
    prefix = "$",
    accuracy = accuracy,
    big.mark = ".",
    decimal.mark = ",",
    trim = TRUE,
    ...
  )
}

presentation_axis_note <- function(log_x = FALSE, log_y = FALSE, extra = NULL) {
  log_parts <- c(
    if (isTRUE(log_x)) "Eje horizontal en escala logarítmica",
    if (isTRUE(log_y)) "Eje vertical en escala logarítmica"
  )
  parts <- c(log_parts, extra)
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (length(parts) == 0) {
    return(NULL)
  }
  paste(parts, collapse = "; ")
}

presentation_note_text <- function(note) {
  if (is.null(note) || length(note) == 0 || is.na(note) || !nzchar(note)) {
    return(NULL)
  }

  note <- gsub("[\r\n]+", " ", note)
  note <- gsub("\\s+", " ", trimws(note))
  if (!nzchar(note)) {
    return(NULL)
  }

  first_letter <- substr(note, 1, 1)
  note <- paste0(toupper(first_letter), substr(note, 2, nchar(note)))
  if (!grepl("[.!?]$", note)) {
    note <- paste0(note, ".")
  }
  note
}

# Keep text geoms aligned with the presentation theme even when a chart adds
# labels with geom_text(), geom_label(), or annotate("text", ...).
ggplot2::update_geom_defaults("text", list(family = presentation_font_family))
ggplot2::update_geom_defaults("label", list(family = presentation_font_family))

historical_event_references <- data.frame(
  event = c(
    "Guerra Federal",
    "WWI",
    "Reventón Barroso 2",
    "WWII",
    "Nacionalización petróleo",
    "Viernes Negro",
    "Caracazo",
    "Paro Petrolero",
    "Sanciones Petroleras"
  ),
  start_year = c(1859, 1914, 1922, 1939, 1976, 1983, 1989, 2002, 2019),
  end_year = c(1863, 1918, NA, 1945, NA, NA, NA, NA, NA),
  stringsAsFactors = FALSE
)

historical_event_label_map <- c(
  "1859" = "Guerra Federal",
  "1914" = "WWI",
  "1922" = "Barroso 2",
  "1939" = "WWII",
  "1976" = "Nac. petr\u00f3leo",
  "1983" = "Viernes Negro",
  "1989" = "Caracazo",
  "2002" = "Paro petrolero",
  "2019" = "Sanc. petroleras"
)

build_priority_color_map <- function(labels, palette = presentation_ordered_colors) {
  labels <- unique(as.character(labels))
  stats::setNames(rep(palette, length.out = length(labels)), labels)
}

build_venezuela_highlight_color_map <- function(
    labels,
    venezuela_label = "Venezuela",
    neutral_labels = character()) {
  labels <- unique(as.character(labels))
  palette_labels <- setdiff(labels, c(venezuela_label, neutral_labels))
  color_map <- build_priority_color_map(palette_labels)

  if (venezuela_label %in% labels) {
    color_map <- c(color_map, stats::setNames(presentation_colors[["venezuela"]], venezuela_label))
  }

  if (length(neutral_labels) > 0) {
    present_neutral_labels <- intersect(neutral_labels, labels)
    color_map <- c(
      color_map,
      stats::setNames(rep(presentation_colors[["muted"]], length(present_neutral_labels)), present_neutral_labels)
    )
  }

  color_map[labels]
}

presentation_breaks_include_limits <- function(n = 6, base_breaks = scales::breaks_extended(n = n)) {
  function(limits) {
    if (length(limits) != 2 || any(is.na(limits)) || any(!is.finite(limits))) {
      return(base_breaks(limits))
    }

    nice_breaks <- base_breaks(limits)
    nice_breaks <- nice_breaks[is.finite(nice_breaks)]
    nice_breaks[nice_breaks >= limits[1] & nice_breaks <= limits[2]]
  }
}

presentation_year_breaks <- function(n = 7) {
  function(limits) {
    if (length(limits) != 2 || any(is.na(limits)) || any(!is.finite(limits))) {
      return(scales::breaks_width(25)(limits))
    }

    span <- diff(limits)
    width <- if (span <= 15) {
      2
    } else if (span <= 35) {
      5
    } else if (span <= 70) {
      10
    } else if (span <= 130) {
      20
    } else {
      25
    }
    breaks <- scales::breaks_width(width)(limits)
    breaks[breaks >= limits[[1]] & breaks <= limits[[2]]]
  }
}

presentation_year_axis <- function(start_year, end_year, tick_interval, expand_right = 0) {
  ggplot2::scale_x_continuous(
    limits = c(start_year, end_year),
    breaks = seq(start_year, end_year, by = tick_interval),
    expand = ggplot2::expansion(mult = c(0, expand_right))
  )
}

presentation_full_history_year_axis <- function() {
  presentation_year_axis(1830, 2030, 10)
}

presentation_recent_year_axis <- function(start_year) {
  presentation_year_axis(start_year, 2030, 5)
}

presentation_recovery_year_axis <- function(end_year, expand_right = 0.035) {
  presentation_year_axis(1920, end_year, 10, expand_right = expand_right)
}

presentation_percent_breaks <- function(n = 6) {
  presentation_breaks_include_limits(n = n)
}

presentation_log10_axis <- function(values, break_candidates, max_breaks = 7) {
  positive_values <- values[!is.na(values) & is.finite(values) & values > 0]
  if (length(positive_values) == 0) {
    stop("Log-axis values must include at least one positive finite value.", call. = FALSE)
  }

  axis_limits <- c(
    max(break_candidates[break_candidates <= min(positive_values)]),
    min(break_candidates[break_candidates >= max(positive_values)])
  )
  axis_breaks <- break_candidates[
    break_candidates >= axis_limits[[1]] & break_candidates <= axis_limits[[2]]
  ]

  if (length(axis_breaks) > max_breaks) {
    interior_breaks <- axis_breaks[-c(1, length(axis_breaks))]
    keep_count <- max(0, max_breaks - 2)
    if (keep_count > 0) {
      keep_positions <- unique(round(seq(1, length(interior_breaks), length.out = keep_count)))
      axis_breaks <- c(axis_breaks[[1]], interior_breaks[keep_positions], axis_breaks[[length(axis_breaks)]])
    } else {
      axis_breaks <- axis_breaks[c(1, length(axis_breaks))]
    }
  }

  list(limits = axis_limits, breaks = axis_breaks)
}

historical_event_reference_layers <- function(
    events = historical_event_references,
    label_events = TRUE,
    label_y = Inf) {
  band_events <- events[!is.na(events$end_year), , drop = FALSE]
  line_events <- events[is.na(events$end_year), , drop = FALSE]
  event_layers <- list()

  if (nrow(band_events) > 0) {
    event_layers <- c(
      event_layers,
      list(
      ggplot2::geom_rect(
        data = band_events,
        ggplot2::aes(xmin = start_year, xmax = end_year, ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE,
        fill = presentation_colors[["light"]],
        alpha = 0.12
      ),
      ggplot2::geom_vline(
        data = band_events,
        ggplot2::aes(xintercept = start_year),
        inherit.aes = FALSE,
        color = presentation_colors[["muted"]],
        linewidth = 0.25,
        linetype = "dashed"
      )
      )
    )
  }

  if (nrow(line_events) > 0) {
    event_layers <- c(
      event_layers,
      list(
      ggplot2::geom_vline(
        data = line_events,
        ggplot2::aes(xintercept = start_year),
        inherit.aes = FALSE,
        color = presentation_colors[["muted"]],
        linewidth = 0.25,
        linetype = "dashed"
      )
      )
    )
  }

  if (isTRUE(label_events) && nrow(events) > 0) {
    label_data <- events
    label_data$event_label <- historical_event_label_map[as.character(label_data$start_year)]
    label_data$event_label[is.na(label_data$event_label)] <- label_data$event[is.na(label_data$event_label)]
    label_data$label_year <- ifelse(
      is.na(label_data$end_year),
      label_data$start_year + 0.85,
      (label_data$start_year + label_data$end_year) / 2
    )

    event_layers <- c(
      event_layers,
      list(
      ggplot2::geom_text(
        data = label_data,
        ggplot2::aes(x = label_year, y = label_y, label = event_label),
        inherit.aes = FALSE,
        angle = 90,
        hjust = 1.03,
        vjust = 0.5,
        family = presentation_font_family,
        size = 3.8,
        color = presentation_colors[["ink"]],
        alpha = 0.95,
        check_overlap = TRUE
      )
      )
    )
  }

  event_layers
}

add_historical_event_references <- function(
    plot,
    events = historical_event_references,
    label_events = TRUE,
    label_y = Inf) {
  plot + historical_event_reference_layers(
    events = events,
    label_events = label_events,
    label_y = label_y
  )
}

apply_presentation_axis_theme <- function(plot = NULL) {
  axis_theme <- ggplot2::theme(
    axis.line.x = ggplot2::element_line(color = presentation_colors[["ink"]], linewidth = 0.35),
    axis.line.y = ggplot2::element_line(color = presentation_colors[["ink"]], linewidth = 0.35),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = presentation_colors[["grid"]], linewidth = 0.3),
    panel.grid.minor.y = ggplot2::element_blank()
  )

  if (is.null(plot)) {
    return(axis_theme)
  }

  plot + axis_theme
}

build_source_caption <- function(source, calculations = TRUE, note = NULL) {
  source_text <- if (calculations) {
    sprintf("Fuente: %s; cálculos propios.", source)
  } else {
    sprintf("Fuente: %s.", source)
  }

  note <- presentation_note_text(note)
  if (!is.null(note)) {
    return(sprintf("%s\nNota: %s", source_text, note))
  }

  sprintf("%s\n%s", source_text, presentation_blank_label)
}

first_present_label <- function(...) {
  values <- list(...)
  for (value in values) {
    if (!is.null(value) && length(value) > 0 && !is.na(value) && nzchar(value)) {
      return(value)
    }
  }
  NULL
}

prepare_presentation_plot <- function(
    plot,
    source_caption,
    subtitle = NULL,
    note = NULL,
    reserve_subtitle = TRUE,
    reserve_caption = TRUE) {
  current_subtitle <- plot$labels$subtitle
  current_caption <- plot$labels$caption
  current_x <- plot$labels$x

  # Prefer explicit labels from the plot, while reserving layout space when absent.
  final_subtitle <- first_present_label(
    subtitle,
    current_subtitle,
    if (reserve_subtitle) presentation_default_subtitle else NULL
  )
  final_caption <- first_present_label(
    current_caption,
    source_caption,
    if (reserve_caption) presentation_blank_label else NULL
  )
  final_x <- first_present_label(current_x, presentation_blank_label)

  # Notes are appended to the caption so individual graphs can explain bins or caveats.
  note <- presentation_note_text(note)
  if (!is.null(note)) {
    if (grepl("\nNota: ", final_caption, fixed = TRUE)) {
      final_caption <- sprintf("%s %s", final_caption, note)
    } else {
      final_caption <- sprintf("%s\nNota: %s", final_caption, note)
    }
  }

  # Remove default ggplot padding only when the plot did not request axis limits.
  should_remove_coordinate_padding <- inherits(plot$coordinates, "CoordCartesian") &&
    is.null(plot$coordinates$limits$x) &&
    is.null(plot$coordinates$limits$y)

  if (should_remove_coordinate_padding) {
    plot <- plot + ggplot2::coord_cartesian(expand = FALSE)
  }

  # Apply shared labels, single-row legends, axis styling, and presentation typography.
  plot +
    ggplot2::labs(
      x = final_x,
      subtitle = final_subtitle,
      caption = final_caption
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(nrow = 1, byrow = TRUE),
      fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE),
      shape = ggplot2::guide_legend(nrow = 1, byrow = TRUE),
      linetype = ggplot2::guide_legend(nrow = 1, byrow = TRUE),
      size = ggplot2::guide_legend(nrow = 1, byrow = TRUE)
    ) +
    apply_presentation_axis_theme() +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(12, 32, 12, 18),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 23,
        family = presentation_font_family,
        margin = ggplot2::margin(b = 3)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 16,
        family = presentation_font_family,
        color = presentation_colors[["ink"]],
        margin = ggplot2::margin(b = 8)
      ),
      axis.title.x = ggplot2::element_text(
        family = presentation_font_family,
        margin = ggplot2::margin(t = 7, b = 2)
      ),
      axis.title.y = ggplot2::element_text(family = presentation_font_family),
      axis.text = ggplot2::element_text(family = presentation_font_family),
      strip.text = ggplot2::element_text(family = presentation_font_family),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = 12.5,
        face = "italic",
        family = presentation_font_family,
        color = presentation_colors[["ink"]],
        lineheight = 1.15,
        margin = ggplot2::margin(t = 8)
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.title = ggplot2::element_text(size = 14, family = presentation_font_family),
      legend.text = ggplot2::element_text(size = 14, family = presentation_font_family),
      legend.key.width = grid::unit(1.1, "lines"),
      legend.spacing.x = grid::unit(0.45, "lines")
    )
}

save_presentation_plot <- function(filename, plot, source_caption, subtitle = NULL, note = NULL) {
  # Prepare once, then save the same graph to raster and vector outputs.
  prepared_plot <- prepare_presentation_plot(
    plot = plot,
    source_caption = source_caption,
    subtitle = subtitle,
    note = note
  )

  # Save the raster version used by the HTML deck.
  ggplot2::ggsave(
    filename = filename,
    plot = prepared_plot,
    width = presentation_plot_width,
    height = presentation_plot_height,
    dpi = presentation_plot_dpi
  )

  # Save a matching vector PDF so the Beamer deck can use sharper figures.
  file_extension <- tolower(tools::file_ext(filename))
  if (file_extension %in% presentation_vector_extensions) {
    vector_filename <- sub(sprintf("\\.%s$", file_extension), ".pdf", filename, ignore.case = TRUE)
    ggplot2::ggsave(
      filename = vector_filename,
      plot = prepared_plot,
      width = presentation_plot_width,
      height = presentation_plot_height,
      device = grDevices::cairo_pdf
    )
  }

  invisible(prepared_plot)
}

save_and_preview_plot <- function(filename, plot, source_caption, subtitle = NULL, note = NULL) {
  # Make the preview step explicit at the graph call site.
  prepared_plot <- save_presentation_plot(
    filename = filename,
    plot = plot,
    source_caption = source_caption,
    subtitle = subtitle,
    note = note
  )
  if (interactive() || grDevices::dev.cur() > 1) {
    print(prepared_plot)
  }
  invisible(prepared_plot)
}
