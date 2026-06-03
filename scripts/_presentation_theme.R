# Shared visual defaults for the Quarto presentation figures.

presentation_palette <- c(
  navy = "#003a5d",
  red = "#d70036",
  cyan = "#00acc8",
  green = "#157549",
  yellow = "#f1ba0d",
  orange = "#f26122"
)

presentation_colors <- c(
  primary = presentation_palette[["navy"]],
  secondary = presentation_palette[["cyan"]],
  venezuela = presentation_palette[["red"]],
  latam = presentation_palette[["cyan"]],
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

presentation_plot_width <- 11
presentation_plot_height <- 6.2
presentation_plot_dpi <- 160

presentation_blank_label <- " "

presentation_breaks_include_limits <- function(n = 5, base_breaks = scales::breaks_extended(n = n)) {
  function(limits) {
    if (length(limits) != 2 || any(is.na(limits)) || any(!is.finite(limits))) {
      return(base_breaks(limits))
    }

    middle_breaks <- base_breaks(limits)
    middle_breaks <- middle_breaks[is.finite(middle_breaks)]
    middle_breaks <- middle_breaks[middle_breaks > limits[1] & middle_breaks < limits[2]]
    unique(c(limits[1], middle_breaks, limits[2]))
  }
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
    sprintf("Fuente: %s; calculos propios.", source)
  } else {
    sprintf("Fuente: %s.", source)
  }

  if (!is.null(note) && nzchar(note)) {
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

  final_subtitle <- first_present_label(
    subtitle,
    current_subtitle,
    if (reserve_subtitle) presentation_blank_label else NULL
  )
  final_caption <- first_present_label(
    current_caption,
    source_caption,
    if (reserve_caption) presentation_blank_label else NULL
  )
  final_x <- first_present_label(current_x, presentation_blank_label)

  if (!is.null(note) && nzchar(note)) {
    final_caption <- sprintf("%s\nNota: %s", final_caption, note)
  }

  should_remove_coordinate_padding <- inherits(plot$coordinates, "CoordCartesian") &&
    is.null(plot$coordinates$limits$x) &&
    is.null(plot$coordinates$limits$y)

  if (should_remove_coordinate_padding) {
    plot <- plot + ggplot2::coord_cartesian(expand = FALSE)
  }

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
        size = 14,
        margin = ggplot2::margin(b = 3)
      ),
      plot.subtitle = ggplot2::element_text(
        size = 10,
        color = presentation_colors[["ink"]],
        margin = ggplot2::margin(b = 8)
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 7, b = 2)
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = 8,
        face = "italic",
        color = presentation_colors[["ink"]],
        lineheight = 1.15,
        margin = ggplot2::margin(t = 8)
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.title = ggplot2::element_text(size = 9),
      legend.text = ggplot2::element_text(size = 9),
      legend.key.width = grid::unit(1.1, "lines"),
      legend.spacing.x = grid::unit(0.45, "lines")
    )
}

save_presentation_plot <- function(filename, plot, source_caption, subtitle = NULL, note = NULL) {
  ggplot2::ggsave(
    filename = filename,
    plot = prepare_presentation_plot(
      plot = plot,
      source_caption = source_caption,
      subtitle = subtitle,
      note = note
    ),
    width = presentation_plot_width,
    height = presentation_plot_height,
    dpi = presentation_plot_dpi
  )
}
