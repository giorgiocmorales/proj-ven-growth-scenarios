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
presentation_font_family <- "serif"
presentation_base_size <- 19
presentation_small_base_size <- 18
presentation_compact_base_size <- 17
presentation_blank_label <- " "
presentation_label_text_size <- 3.1
presentation_label_small_text_size <- 2.7
presentation_label_box_linewidth <- 0.15
presentation_label_padding <- ggplot2::unit(0.12, "lines")
presentation_label_radius <- ggplot2::unit(0.08, "lines")
presentation_label_segment_size <- 0.22
# Dark dashed connectors distinguish annotations from solid historical series.
presentation_label_segment_color <- "#667085"
presentation_label_segment_linetype <- "dashed"
presentation_label_box_padding <- 0.45
presentation_label_point_padding <- 0.25
presentation_point_stroke <- 0.12

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
  if (is.null(note) || length(note) == 0 || all(is.na(note))) {
    return(NULL)
  }

  note_parts <- note[!is.na(note) & nzchar(note)]
  note_parts <- gsub("[\r\n]+", " ", note_parts)
  note_parts <- gsub("\\s+", " ", trimws(note_parts))
  note_parts <- note_parts[nzchar(note_parts)]
  if (length(note_parts) == 0) {
    return(NULL)
  }

  note_parts <- vapply(
    note_parts,
    function(note_part) {
      paste0(toupper(substr(note_part, 1, 1)), substr(note_part, 2, nchar(note_part)))
    },
    character(1)
  )
  note <- paste(note_parts, collapse = "; ")
  if (!nzchar(note)) {
    return(NULL)
  }
  if (!grepl("[.!?]$", note)) {
    note <- paste0(note, ".")
  }
  note
}

append_caption_note <- function(caption, note) {
  note <- presentation_note_text(note)
  if (is.null(note)) {
    return(caption)
  }

  blank_caption_suffix <- sprintf("\n%s", presentation_blank_label)
  if (endsWith(caption, blank_caption_suffix)) {
    caption <- substr(caption, 1, nchar(caption) - nchar(blank_caption_suffix))
  }
  if (grepl("\nNota: ", caption, fixed = TRUE)) {
    caption <- sub("([.!?])$", "", caption)
    return(sprintf("%s; %s", caption, note))
  }

  sprintf("%s\nNota: %s", caption, note)
}

# Keep text geoms aligned with the presentation theme even when a chart adds
# labels with geom_text(), geom_label(), or annotate("text", ...).
ggplot2::update_geom_defaults("text", list(family = presentation_font_family))
ggplot2::update_geom_defaults("label", list(family = presentation_font_family))
ggplot2::update_geom_defaults("point", list(stroke = presentation_point_stroke))

historical_event_references <- data.frame(
  event = c(
    "WWI",
    "Reventón Barroso 2",
    "WWII",
    "Nacionalización petróleo",
    "Viernes Negro",
    "Caracazo",
    "Paro Petrolero",
    "Sanciones Petroleras",
    "Pandemia de COVID-19",
    "Terremotos de San Juan"
  ),
  # Fractional years retain day-level placement on the continuous timeline.
  start_year = c(
    1914, 1922, 1939, 1976, 1983, 1989, 2002, 2019,
    2020 + 29 / 366,
    2026 + 174 / 365
  ),
  end_year = c(
    1918, NA, 1945, NA, NA, NA, NA, NA,
    2021 + 363 / 365,
    NA
  ),
  stringsAsFactors = FALSE
)

historical_event_label_map <- c(
  "WWI" = "WWI",
  "Reventón Barroso 2" = "Barroso 2",
  "WWII" = "WWII",
  "Nacionalización petróleo" = "Nac. petr\u00f3leo",
  "Viernes Negro" = "Viernes Negro",
  "Caracazo" = "Caracazo",
  "Paro Petrolero" = "Paro petrolero",
  "Sanciones Petroleras" = "Sanc. petroleras",
  "Pandemia de COVID-19" = "COVID-19",
  "Terremotos de San Juan" = "Terremotos de San Juan"
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
        alpha = 0.35
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
    label_data$event_label <- historical_event_label_map[as.character(label_data$event)]
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

  # Tag contextual bands and reference lines so they can be drawn beneath data marks.
  lapply(event_layers, function(layer) {
    if (
      inherits(layer$geom, "GeomRect") ||
        inherits(layer$geom, "GeomVline")
    ) {
      attr(layer, "presentation_context_layer") <- TRUE
    }

    layer
  })
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
    sprintf("Fuente: %s; cálculos propios; elaborado por @giorgiocmorales.", source)
  } else {
    sprintf("Fuente: %s.", source)
  }

  caption <- append_caption_note(source_text, note)
  if (!identical(caption, source_text)) {
    return(caption)
  }

  sprintf("%s\n%s", source_text, presentation_blank_label)
}

apply_presentation_plot_style <- function(plot) {
  # Keep contextual historical shading and reference lines behind every chart's data.
  context_layers <- vapply(
    plot$layers,
    function(layer) isTRUE(attr(layer, "presentation_context_layer")),
    logical(1)
  )

  if (any(context_layers)) {
    plot$layers <- c(plot$layers[context_layers], plot$layers[!context_layers])
  }

  # Remove default ggplot padding only when the plot did not request axis limits.
  should_remove_coordinate_padding <- inherits(plot$coordinates, "CoordCartesian") &&
    is.null(plot$coordinates$limits$x) &&
    is.null(plot$coordinates$limits$y)

  if (should_remove_coordinate_padding) {
    plot <- plot + ggplot2::coord_cartesian(expand = FALSE)
  }

  # Apply only universal legend, axis, and typography settings.
  # Titles, subtitles, axes, legends, and captions belong in each graph's labs() block.
  plot +
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

avoid_presentation_label_overlaps <- function(plot_build) {
  # Add invisible, empty-label obstacles to ggrepel's built data. Using scaled
  # coordinates preserves log axes, facets, original anchors, and chart values.
  label_layers <- which(vapply(
    plot_build$plot$layers,
    function(layer) inherits(layer$geom, c("GeomLabelRepel", "GeomTextRepel")),
    logical(1)
  ))
  if (length(label_layers) == 0) {
    return(plot_build)
  }

  # Points reserve clearance around their rendered bubble sizes; paths are
  # sampled between vertices so labels also avoid the middle of a line.
  obstacle_parts <- list()
  for (layer_index in seq_along(plot_build$data)) {
    layer <- plot_build$plot$layers[[layer_index]]
    layer_data <- plot_build$data[[layer_index]]
    is_point <- inherits(layer$geom, "GeomPoint")
    is_path <- inherits(layer$geom, c("GeomLine", "GeomPath", "GeomSmooth"))
    if ((!is_point && !is_path) || nrow(layer_data) == 0) {
      next
    }

    # Retain gaps rather than connecting across missing or out-of-range values.
    layer_data$path_piece <- cumsum(!is.finite(layer_data$x) | !is.finite(layer_data$y))
    layer_data <- layer_data[is.finite(layer_data$x) & is.finite(layer_data$y), , drop = FALSE]
    if (is_point) {
      obstacle_parts[[length(obstacle_parts) + 1L]] <- data.frame(
        x = layer_data$x, y = layer_data$y, PANEL = layer_data$PANEL,
        point.size = layer_data$size * 2
      )
      next
    }

    path_groups <- split(layer_data, interaction(
      layer_data$PANEL, layer_data$group, layer_data$path_piece, drop = TRUE
    ))
    for (path_data in path_groups) {
      if (nrow(path_data) < 2) {
        next
      }
      panel_index <- as.integer(path_data$PANEL[[1]])
      panel_data <- plot_build$layout$coord$transform(
        path_data, plot_build$layout$panel_params[[panel_index]]
      )
      path_distance <- c(0, cumsum(sqrt(diff(panel_data$x)^2 + diff(panel_data$y)^2)))
      if (max(path_distance) == 0) {
        next
      }
      # Faint background trajectories retain their observed vertices; dense
      # sampling is reserved for visible comparison lines to keep exports fast.
      sample_distance <- if (all(is.na(path_data$alpha) | path_data$alpha >= 0.4)) {
        unique(c(path_distance, seq(0, max(path_distance), by = 0.008)))
      } else {
        path_distance
      }
      obstacle_parts[[length(obstacle_parts) + 1L]] <- data.frame(
        x = stats::approx(path_distance, path_data$x, xout = sample_distance, ties = "ordered")$y,
        y = stats::approx(path_distance, path_data$y, xout = sample_distance, ties = "ordered")$y,
        PANEL = path_data$PANEL[[1]],
        point.size = 2
      )
    }
  }
  if (length(obstacle_parts) == 0) {
    return(plot_build)
  }
  obstacles <- dplyr::distinct(dplyr::bind_rows(obstacle_parts))

  # Keep every visible label and its original style. Empty strings participate
  # in ggrepel's collision solver without adding text, points, or legend entries.
  for (layer_index in label_layers) {
    labels <- plot_build$data[[layer_index]]
    if (nrow(labels) == 0) {
      next
    }
    panel_obstacles <- obstacles[obstacles$PANEL %in% labels$PANEL, , drop = FALSE]
    template_rows <- match(panel_obstacles$PANEL, labels$PANEL)
    obstacle_labels <- labels[template_rows, , drop = FALSE]
    obstacle_labels$x <- panel_obstacles$x
    obstacle_labels$y <- panel_obstacles$y
    obstacle_labels$label <- ""
    obstacle_labels$point.size <- panel_obstacles$point.size
    for (axis in c("x", "y")) {
      # ggrepel's nudge position stores original anchors in these optional fields.
      for (position_column in intersect(c(paste0(axis, "_orig"), paste0("nudge_", axis)), names(labels))) {
        obstacle_labels[[position_column]] <- panel_obstacles[[axis]]
      }
    }
    plot_build$data[[layer_index]] <- dplyr::bind_rows(labels, obstacle_labels)

    # Let crowded charts move labels farther from their anchors while retaining
    # leader lines and a fixed seed. Solve by iteration count for repeatable exports.
    layer <- plot_build$plot$layers[[layer_index]]
    label_parameters <- list(
      force = max(layer$geom_params$force, 20),
      force_pull = 0,
      max.time = Inf,
      max.iter = 10000L,
      max.overlaps = Inf,
      point.padding = grid::unit(0.8, "lines"),
      min.segment.length = grid::unit(0, "lines")
    )
    # ggplot_build has already cached draw parameters; update both representations.
    layer$geom_params <- utils::modifyList(layer$geom_params, label_parameters)
    layer$computed_geom_params <- utils::modifyList(layer$computed_geom_params, label_parameters)
    plot_build$plot$layers[[layer_index]] <- layer
  }
  plot_build
}

connect_presentation_labels_to_points <- function(grob) {
  # Keep the resolved label positions and collision padding. Only extend each
  # connector to its own original anchor, inside the corresponding point marker.
  if (inherits(grob, c("labelrepeltree", "textrepeltree"))) {
    for (child_index in seq_along(grob$children)) {
      connector <- grob$children[[child_index]]
      if (!grepl("^segmentrepelgrob[0-9]+$", connector$name)) {
        next
      }
      anchor_index <- as.integer(sub("^segmentrepelgrob", "", connector$name))
      anchor <- grob$data[anchor_index, , drop = FALSE]
      if (nrow(anchor) != 1 || !is.finite(anchor$x) || !is.finite(anchor$y)) {
        stop("A presentation label connector has no finite original anchor.", call. = FALSE)
      }
      # Revert only the connector curve so grid redraws its endpoint; the parent
      # remains frozen, preventing another label-layout calculation on export.
      grob$children[[child_index]] <- grid::editGrob(
        grid::grid.revert(connector),
        x2 = grid::unit(anchor$x, "native"),
        y2 = grid::unit(anchor$y, "native")
      )
    }
    return(grob)
  }

  # Resolved grid trees draw their children; untouched tables use their grobs.
  if (length(grob$children) > 0) {
    for (child_index in seq_along(grob$children)) {
      grob$children[[child_index]] <- connect_presentation_labels_to_points(grob$children[[child_index]])
    }
  } else if (length(grob$grobs) > 0) {
    grob$grobs <- lapply(grob$grobs, connect_presentation_labels_to_points)
  }
  grob
}

save_plot_variants <- function(filename, plot, width, height, dpi) {
  # Export matching default and transparent charts in PNG, SVG, and PDF formats.
  if (!identical(tolower(tools::file_ext(filename)), "png")) {
    stop("Presentation plot filenames must use the `.png` extension.", call. = FALSE)
  }

  if (!identical(basename(dirname(filename)), "default")) {
    stop("Presentation plots must be saved in the `default` figure directory.", call. = FALSE)
  }

  figure_root <- dirname(dirname(filename))
  output_stem <- tools::file_path_sans_ext(basename(filename))
  output_devices <- list(png = "png", svg = svglite::svglite, pdf = grDevices::cairo_pdf)
  output_backgrounds <- c(default = "white", transparent = "transparent")

  # Remove canvas fills while preserving white country-label boxes and data marks.
  # The default exports restore their white canvas through the graphics device.
  plot <- plot + ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.box.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.key = ggplot2::element_rect(fill = "transparent", colour = NA)
  )

  # Build stateful layers such as ggrepel once so every device gets identical geometry.
  plot_build <- avoid_presentation_label_overlaps(ggplot2::ggplot_build(plot))
  plot_grob <- ggplot2::ggplot_gtable(plot_build)
  # Resolve deferred label layout on a device with the final export dimensions.
  # Freeze the grobs so PNG, SVG, and PDF cannot independently reposition labels.
  plot_grob <- grid::grid.grabExpr(
    {
      grid::grid.draw(plot_grob)
      grid::grid.force()
    },
    width = width,
    height = height
  )
  plot_grob <- connect_presentation_labels_to_points(plot_grob)

  # Reuse the frozen layout for both backgrounds so label positions cannot drift.
  output_files <- character()
  for (background_name in names(output_backgrounds)) {
    output_dir <- file.path(figure_root, background_name)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    for (extension in names(output_devices)) {
      output_file <- file.path(output_dir, paste0(output_stem, ".", extension))
      ggplot2::ggsave(
        filename = output_file,
        plot = plot_grob,
        device = output_devices[[extension]],
        width = width,
        height = height,
        dpi = dpi,
        bg = output_backgrounds[[background_name]]
      )
      output_files <- c(output_files, output_file)
    }
  }

  invisible(output_files)
}
