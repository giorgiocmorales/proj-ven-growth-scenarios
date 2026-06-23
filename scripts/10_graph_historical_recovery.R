# Build first-pass ggplot figures for the Quarto presentation.

## Setup ----
# Check packages used by this plotting script.
required_packages <- c("dplyr", "ggplot2", "ggrepel", "magrittr", "scales")
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

source("scripts/_presentation_theme.R")

## Inputs ----
# Require final data products created by the earlier pipeline scripts.
required_files <- c(
  index_series = "data/final/index_series_long.csv",
  episode_path = "data/final/episode_path.csv",
  episode_summary = "data/final/episode_summary.csv",
  maddison_episode_path = "data/final/maddison_gdp_per_capita_episode_path.csv",
  maddison_episode_summary = "data/final/maddison_gdp_per_capita_episode_summary.csv",
  wdi_episode_path = "data/final/wdi_real_gdp_growth_episode_path.csv",
  wdi_episode_summary = "data/final/wdi_real_gdp_growth_episode_summary.csv",
  wdi_pc_episode_path = "data/final/wdi_real_gdp_per_capita_growth_episode_path.csv",
  wdi_pc_episode_summary = "data/final/wdi_real_gdp_per_capita_growth_episode_summary.csv",
  imf_weo_episode_path = "data/final/imf_weo_gdp_per_capita_growth_episode_path.csv",
  imf_weo_episode_summary = "data/final/imf_weo_gdp_per_capita_growth_episode_summary.csv",
  simulation_summary = "data/final/simulation_summary.csv",
  simulation_paths = "data/final/simulation_paths.csv",
  plausibility = "data/final/plausibility_metrics.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    sprintf(
      "Missing required final outputs: %s. Run scripts/09_build_app_data.R first.",
      paste(unname(missing_files), collapse = ", ")
    ),
    call. = FALSE
  )
}

figure_dir <- "outputs/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

## Plot constants ----
# Shared colors, labels, and captions used across graph families.
growth_colors <- c(
  positive = presentation_colors[["positive"]],
  negative = presentation_colors[["negative"]]
)

phase_colors <- c(
  expansion = growth_colors[["positive"]],
  contraction = growth_colors[["negative"]]
)

phase_labels <- c(
  expansion = "Expansión",
  contraction = "Contracción"
)

growth_labels <- c(
  positive = "Expansión",
  negative = "Contracción"
)

mix_with_white <- function(color, white_share = 0.45) {
  rgb_value <- grDevices::col2rgb(color)
  mixed_value <- rgb_value * (1 - white_share) + 255 * white_share
  grDevices::rgb(mixed_value[1, ], mixed_value[2, ], mixed_value[3, ], maxColorValue = 255)
}

annual_growth_y_axis <- function() {
  list(
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = seq(-0.40, 0.40, by = 0.10)
    ),
    ggplot2::coord_cartesian(ylim = c(-0.40, 0.40), expand = FALSE)
  )
}

compound_growth_y_axis <- function() {
  list(
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = seq(-0.05, 0.40, by = 0.05)
    ),
    ggplot2::coord_cartesian(ylim = c(-0.05, 0.40), expand = FALSE)
  )
}

presentation_source_caption <- build_source_caption(
  "Baptista (2008); Garay (2019); BCV (2016); Focus Economics (2026); Maddison Project (2023); Banco Mundial WDI (2025); FMI WEO (2025)",
  calculations = TRUE
)

scenario_colors <- c(
  "15%" = presentation_ordered_colors[[1]],
  "10%" = presentation_ordered_colors[[2]],
  "7%" = presentation_ordered_colors[[3]],
  "5%" = presentation_ordered_colors[[4]],
  "2%" = presentation_ordered_colors[[5]]
)

window_colors <- c(
  "3 años" = presentation_ordered_colors[[1]],
  "5 años" = presentation_ordered_colors[[2]],
  "7 años" = presentation_ordered_colors[[3]],
  "10 años" = presentation_ordered_colors[[4]],
  "15 años" = presentation_ordered_colors[[5]],
  "20 años" = presentation_ordered_colors[[6]]
)

series_order <- c("gdp", "gdp_per_capita")
series_labels <- c(
  gdp = "PIB real",
  gdp_per_capita = "PIB real per cápita"
)

# Standardize series names before plotting.
format_series <- function(series_id) {
  factor(series_id, levels = series_order, labels = series_labels)
}


episode_axis_limits <- function(selected_series, selected_phase = NULL) {
  upper <- if (identical(selected_series, series_labels[["gdp"]])) 5 else 3
  if (identical(selected_phase, "expansion")) {
    c(0, upper)
  } else if (identical(selected_phase, "contraction")) {
    c(-1, 0)
  } else {
    c(-1, upper)
  }
}

episode_axis_breaks <- function(selected_series, selected_phase = NULL) {
  upper <- if (identical(selected_series, series_labels[["gdp"]])) 5 else 3
  if (identical(selected_phase, "expansion")) {
    seq(0, upper, by = 0.5)
  } else if (identical(selected_phase, "contraction")) {
    seq(-1, 0, by = 0.1)
  } else {
    seq(-1, upper, by = 1)
  }
}

build_episode_top_label_data <- function(data, selected_series, selected_phase, top_n = 5) {
  ranked_episodes <- data %>%
    dplyr::filter(series_id == selected_series, phase == selected_phase) %>%
    dplyr::group_by(episode_group, episode_label) %>%
    dplyr::summarise(
      final_change = dplyr::last(episode_cumulative_change[order(year)]),
      final_year = max(year, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(sort_value = if (identical(selected_phase, "expansion")) -final_change else final_change) %>%
    dplyr::arrange(sort_value) %>%
    dplyr::slice_head(n = top_n)

  data %>%
    dplyr::inner_join(ranked_episodes %>% dplyr::select(episode_group, episode_label), by = c("episode_group", "episode_label")) %>%
    dplyr::group_by(episode_group, episode_label) %>%
    dplyr::filter(year == max(year, na.rm = TRUE)) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(if (identical(selected_phase, "expansion")) -episode_cumulative_change else episode_cumulative_change) %>%
    dplyr::mutate(
      label_rank = dplyr::row_number(),
      label = sprintf("(%s)", episode_label)
    )
}

## Data loading ----
# Read Venezuela series, episode summaries, simulation outputs, and plausibility data.
index_series <- utils::read.csv(required_files[["index_series"]], stringsAsFactors = FALSE) %>%
  dplyr::mutate(
    growth_direction = dplyr::if_else(growth_rate >= 0, "positive", "negative"),
    growth_direction = factor(growth_direction, levels = names(growth_colors)),
    series_id = format_series(series_id)
  )

episodes <- utils::read.csv(required_files[["episode_path"]], stringsAsFactors = FALSE) %>%
  dplyr::mutate(
    growth_direction = dplyr::if_else(phase == "expansion", "positive", "negative"),
    growth_direction = factor(growth_direction, levels = names(growth_colors)),
    episode_group = paste(series_id, episode_label, sep = "_"),
    series_id = format_series(series_id)
  )

episode_summary <- utils::read.csv(required_files[["episode_summary"]], stringsAsFactors = FALSE) %>%
  dplyr::mutate(
    phase = factor(phase, levels = names(phase_colors)),
    series_id = format_series(series_id)
  )

simulation_summary <- utils::read.csv(required_files[["simulation_summary"]], stringsAsFactors = FALSE) %>%
  dplyr::mutate(series_id = format_series(series_id))

simulation_paths <- utils::read.csv(required_files[["simulation_paths"]], stringsAsFactors = FALSE) %>%
  dplyr::mutate(series_id = format_series(series_id))

plausibility <- utils::read.csv(required_files[["plausibility"]], stringsAsFactors = FALSE) %>%
  dplyr::mutate(series_id = format_series(series_id))

maddison_episode_path <- utils::read.csv(
  required_files[["maddison_episode_path"]],
  stringsAsFactors = FALSE
)

maddison_episode_summary <- utils::read.csv(
  required_files[["maddison_episode_summary"]],
  stringsAsFactors = FALSE
)

wdi_episode_path <- utils::read.csv(
  required_files[["wdi_episode_path"]],
  stringsAsFactors = FALSE
)

wdi_episode_summary <- utils::read.csv(
  required_files[["wdi_episode_summary"]],
  stringsAsFactors = FALSE
)

wdi_pc_episode_path <- utils::read.csv(
  required_files[["wdi_pc_episode_path"]],
  stringsAsFactors = FALSE
)

wdi_pc_episode_summary <- utils::read.csv(
  required_files[["wdi_pc_episode_summary"]],
  stringsAsFactors = FALSE
)

imf_weo_episode_path <- utils::read.csv(
  required_files[["imf_weo_episode_path"]],
  stringsAsFactors = FALSE
)

imf_weo_episode_summary <- utils::read.csv(
  required_files[["imf_weo_episode_summary"]],
  stringsAsFactors = FALSE
)

## Chart helpers ----
# Reusable ggplot builders used by the plot construction section below.

### Axis helpers ----
historical_index_y_axis <- function(selected_series) {
  # Use fixed historical-index scales so GDP and per-capita charts are comparable across reruns.
  if (identical(as.character(selected_series), as.character(series_labels[["gdp"]]))) {
    return(ggplot2::scale_y_continuous(
      labels = presentation_number_label(accuracy = 0.1),
      limits = c(0, 150000),
      breaks = seq(0, 150000, by = 25000)
    ))
  }

  if (identical(as.character(selected_series), as.character(series_labels[["gdp_per_capita"]]))) {
    return(ggplot2::scale_y_continuous(
      labels = presentation_number_label(accuracy = 0.1),
      limits = c(0, 5000),
      breaks = seq(0, 5000, by = 1000)
    ))
  }

  ggplot2::scale_y_continuous(
    labels = presentation_number_label(accuracy = 0.1),
    breaks = presentation_breaks_include_limits()
  )
}

### Domestic history charts ----
build_growth_bar_chart <- function(data, selected_series, title) {
  ggplot2::ggplot(
    data[data$series_id == selected_series, ],
    ggplot2::aes(x = year, y = growth_rate, fill = growth_direction)
  ) +
    ggplot2::geom_col(width = 0.9) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = presentation_colors[["ink"]]) +
    ggplot2::scale_fill_manual(values = growth_colors, labels = growth_labels, name = NULL) +
    presentation_full_history_year_axis() +
    annual_growth_y_axis() +
    ggplot2::labs(title = title, x = NULL, y = "Tasa de crecimiento anual") +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    historical_event_reference_layers()
}

build_index_line_chart <- function(data, selected_series, title) {
  legend_label <- paste(selected_series)
  series_data <- data[data$series_id == selected_series, ]
  base_year <- min(series_data$year, na.rm = TRUE)
  latest_year <- max(series_data$year, na.rm = TRUE)
  latest_index <- series_data$index_value[series_data$year == latest_year][[1]]
  reference_year <- if (identical(as.character(selected_series), "PIB real")) {
    1967
  } else if (identical(as.character(selected_series), "PIB real per cápita")) {
    1943
  } else {
    NA_real_
  }
  closest_prior_data <- if (is.finite(reference_year) && reference_year %in% series_data$year) {
    series_data %>%
      dplyr::filter(year == reference_year)
  } else {
    series_data %>%
      dplyr::filter(year < latest_year, is.finite(index_value)) %>%
      dplyr::mutate(distance_to_latest = abs(index_value - latest_index)) %>%
      dplyr::slice_min(distance_to_latest, n = 1, with_ties = FALSE)
  }
  closest_prior_year <- closest_prior_data$year[[1]]
  closest_label_at_right_edge <- closest_prior_year > latest_year - 10
  reference_level_color <- presentation_colors[["accent"]]
  reference_year_color <- presentation_colors[["primary"]]
  closest_label_data <- closest_prior_data %>%
    dplyr::mutate(
      label_x = if (closest_label_at_right_edge) year - 6 else year + 6,
      label_y = index_value + latest_index * 0.24
    )

  ggplot2::ggplot(
    series_data,
    ggplot2::aes(x = year, y = index_value)
  ) +
    ggplot2::geom_hline(
      yintercept = latest_index,
      color = reference_level_color,
      linewidth = 0.65,
      linetype = "dashed"
    ) +
    ggplot2::geom_vline(
      data = closest_prior_data,
      ggplot2::aes(xintercept = year),
      color = reference_year_color,
      linewidth = 0.45,
      linetype = "dotted"
    ) +
    ggplot2::geom_line(ggplot2::aes(color = legend_label), linewidth = 0.55) +
    ggplot2::geom_point(
      data = closest_prior_data,
      shape = 21,
      fill = presentation_colors[["venezuela"]],
      color = presentation_colors[["ink"]],
      stroke = 0.35,
      size = 3.6
    ) +
    ggplot2::geom_segment(
      data = closest_label_data,
      ggplot2::aes(x = year, y = index_value, xend = label_x, yend = label_y),
      inherit.aes = FALSE,
      color = presentation_colors[["muted"]],
      linewidth = presentation_label_segment_size
    ) +
    ggplot2::geom_label(
      data = closest_label_data,
      ggplot2::aes(x = label_x, y = label_y, label = year),
      inherit.aes = FALSE,
      color = presentation_colors[["ink"]],
      fill = "white",
      linewidth = presentation_label_box_linewidth,
      label.padding = presentation_label_padding,
      family = presentation_font_family,
      fontface = "bold",
      size = presentation_label_text_size,
      hjust = 0.5
    ) +
    ggplot2::scale_color_manual(
      values = stats::setNames(presentation_colors[["ink"]], legend_label),
      name = NULL
    ) +
    presentation_full_history_year_axis() +
    historical_index_y_axis(selected_series) +
    ggplot2::labs(
      title = sprintf("%s: nivel de %s cercano a %s", title, latest_year, closest_prior_year),
      x = NULL,
      y = sprintf("Índice histórico (%s = 100)", base_year)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    ggplot2::theme(plot.margin = ggplot2::margin(12, 42, 12, 18)) +
    historical_event_reference_layers()
}

build_anchor_line_chart <- function(data, selected_series, title) {
  ggplot2::ggplot(
    data[data$series_id == selected_series, ],
    ggplot2::aes(x = year, y = index_vs_anchor_100)
  ) +
    ggplot2::geom_hline(yintercept = 100, linewidth = 0.3, color = presentation_colors[["ink"]]) +
    ggplot2::geom_line(color = presentation_colors[["ink"]], linewidth = 0.55) +
    presentation_full_history_year_axis() +
    ggplot2::labs(title = title, x = NULL, y = "Índice vs. último año = 100") +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    historical_event_reference_layers()
}

build_episode_line_chart <- function(data, selected_series, title) {
  ggplot2::ggplot(
    data[data$series_id == selected_series, ],
    ggplot2::aes(
      x = year,
      y = episode_cumulative_change,
      group = episode_group,
      color = growth_direction
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, color = presentation_colors[["ink"]]) +
    ggplot2::geom_line(linewidth = 0.85, alpha = 0.68) +
    ggplot2::scale_color_manual(values = growth_colors, labels = growth_labels, name = NULL) +
    presentation_full_history_year_axis() +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      limits = episode_axis_limits(selected_series),
      breaks = episode_axis_breaks(selected_series)
    ) +
    ggplot2::labs(title = title, x = NULL, y = "Variación acumulada") +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    historical_event_reference_layers()
}

build_episode_duration_chart <- function(data, selected_series, title) {
  ggplot2::ggplot(
    data[data$series_id == selected_series, ],
    ggplot2::aes(x = duration_years, fill = phase)
  ) +
    ggplot2::geom_bar(width = 0.85) +
    ggplot2::scale_fill_manual(values = phase_colors, labels = phase_labels, name = NULL) +
    ggplot2::labs(title = title, x = "Duración del episodio, años", y = "Número de episodios") +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family)
}

build_growth_distribution_chart <- function(data, selected_series, title) {
  plot_data <- data[!is.na(data$growth_rate) & data$series_id == selected_series, ]
  bin_width <- 0.01

  ggplot2::ggplot(plot_data, ggplot2::aes(x = growth_rate)) +
    ggplot2::geom_histogram(
      ggplot2::aes(fill = "Años"),
      binwidth = bin_width,
      boundary = 0,
      color = "white",
      linewidth = 0.2
    ) +
    ggplot2::geom_density(
      ggplot2::aes(y = ggplot2::after_stat(count * bin_width), color = "Densidad"),
      linewidth = 0.8,
      adjust = 1.1
    ) +
    ggplot2::geom_vline(xintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.35) +
    ggplot2::scale_fill_manual(values = c("Años" = presentation_colors[["muted"]]), name = NULL) +
    ggplot2::scale_color_manual(values = c("Densidad" = presentation_colors[["primary"]]), name = NULL) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = scales::breaks_width(0.05)
    ) +
    ggplot2::scale_y_continuous(breaks = seq(0, 16, by = 2)) +
    ggplot2::coord_cartesian(xlim = c(-0.4, 0.4), ylim = c(0, 16), expand = FALSE) +
    ggplot2::labs(
      title = title,
      subtitle = "Barras agrupadas en intervalos de 1 punto porcentual.",
      x = "Tasa de crecimiento anual",
      y = "Número de años"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family)
}

build_episode_phase_faceted_chart <- function(data, selected_phase, title) {
  phase_data <- data[data$phase == selected_phase, ]

  ggplot2::ggplot(
    phase_data,
    ggplot2::aes(
      x = year,
      y = episode_cumulative_change,
      group = episode_group,
      color = growth_direction
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = presentation_colors[["ink"]]) +
    ggplot2::geom_line(linewidth = 0.75, alpha = 0.65) +
    ggplot2::facet_wrap(ggplot2::vars(series_id), ncol = 1) +
    ggplot2::scale_color_manual(values = growth_colors, labels = growth_labels, name = NULL) +
    presentation_full_history_year_axis() +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), breaks = presentation_breaks_include_limits()) +
    ggplot2::labs(title = title, x = NULL, y = "Cambio acumulado desde el inicio") +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    historical_event_reference_layers()
}

build_episode_phase_chart <- function(data, selected_series, selected_phase, title) {
  phase_data <- data[data$series_id == selected_series & data$phase == selected_phase, ]
  label_data <- build_episode_top_label_data(data, selected_series, selected_phase, top_n = 5)

  ggplot2::ggplot(
    phase_data,
    ggplot2::aes(
      x = year,
      y = episode_cumulative_change,
      group = episode_group,
      color = growth_direction
    )
    ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, color = presentation_colors[["ink"]]) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.72) +
    ggrepel::geom_label_repel(
      data = label_data,
      ggplot2::aes(x = year, y = episode_cumulative_change, label = label),
      inherit.aes = FALSE,
      color = presentation_colors[["ink"]],
      family = presentation_font_family,
      size = presentation_label_text_size,
      fill = "white",
      label.size = presentation_label_box_linewidth,
      label.padding = presentation_label_padding,
      label.r = presentation_label_radius,
      min.segment.length = 0,
      segment.color = presentation_colors[["muted"]],
      segment.size = presentation_label_segment_size,
      box.padding = presentation_label_box_padding,
      point.padding = presentation_label_point_padding,
      max.overlaps = Inf,
      seed = 1234,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(values = growth_colors, labels = growth_labels, name = NULL) +
    presentation_full_history_year_axis() +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      limits = episode_axis_limits(selected_series, selected_phase),
      breaks = episode_axis_breaks(selected_series, selected_phase)
    ) +
    ggplot2::coord_cartesian(clip = "off", expand = FALSE) +
    ggplot2::labs(title = title, x = NULL, y = "Variación acumulada") +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    ggplot2::theme(plot.margin = ggplot2::margin(5.5, 78, 5.5, 5.5)) +
    historical_event_reference_layers()
}

### International episode labels ----
build_negative_episode_labels <- function(plot_data, highlighted_summary, venezuela_plot_data) {
  other_labels <- highlighted_summary %>%
    dplyr::transmute(
      episode_key = episode_key,
      label_group = "Top 10 episodios",
      label = sprintf(
        "%s (%s-%s)",
        country_code,
        start_year,
        end_year
      )
    ) %>%
    dplyr::inner_join(
      plot_data %>%
        dplyr::group_by(episode_key) %>%
        dplyr::filter(year == max(year, na.rm = TRUE)) %>%
        dplyr::slice_tail(n = 1) %>%
        dplyr::ungroup() %>%
        dplyr::select(episode_key, year, episode_cumulative_change),
      by = "episode_key"
    )

  venezuela_labels <- venezuela_plot_data %>%
    dplyr::group_by(episode_group) %>%
    dplyr::summarise(
      start_year = min(year, na.rm = TRUE),
      end_year = max(year, na.rm = TRUE),
      episode_cumulative_change = episode_cumulative_change[which.max(year)],
      cumulative_growth = min(episode_cumulative_change, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::transmute(
      episode_key = episode_group,
      year = end_year,
      episode_cumulative_change = episode_cumulative_change,
      label_group = "Venezuela",
      label = sprintf(
        "VEN (%s-%s)",
        start_year,
        end_year
      )
    )

  dplyr::bind_rows(other_labels, venezuela_labels)
}

### International negative episode charts ----
build_maddison_negative_episode_chart <- function(path_data, summary_data, venezuela_path_data, top_n = 10) {
  negative_summary <- summary_data %>%
    dplyr::filter(country_code != "VEN", phase == "contraction", end_year >= 1950, start_year <= 2026) %>%
    dplyr::mutate(episode_key = paste(country_code, episode_id, sep = "_"))

  worst_summary <- negative_summary %>%
    dplyr::slice_min(cumulative_growth, n = top_n, with_ties = FALSE) %>%
    dplyr::arrange(cumulative_growth)

  top_episodes <- worst_summary$episode_key

  plot_data <- path_data %>%
    dplyr::filter(country_code != "VEN", phase == "contraction", year >= 1950, year <= 2026) %>%
    dplyr::mutate(
      episode_key = paste(country_code, episode_id, sep = "_"),
      highlight_group = dplyr::case_when(
        episode_key %in% top_episodes ~ "top_10",
        TRUE ~ "baseline"
      )
    )

  venezuela_plot_data <- venezuela_path_data %>%
    dplyr::filter(
      as.character(series_id) == as.character(series_labels[["gdp_per_capita"]]),
      phase == "contraction",
      year >= 1950,
      year <= 2026
    ) %>%
    dplyr::group_by(episode_group) %>%
    dplyr::mutate(episode_min_change = min(episode_cumulative_change, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(episode_min_change == min(episode_min_change, na.rm = TRUE))

  label_data <- build_negative_episode_labels(plot_data, worst_summary, venezuela_plot_data)
  top_label_data <- label_data %>% dplyr::filter(label_group == "Top 10 episodios")
  venezuela_label_data <- label_data %>% dplyr::filter(label_group == "Venezuela")

  ggplot2::ggplot() +
    # Reference lines establish no-change and the -15% disaster threshold.
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = presentation_colors[["ink"]]) +
    ggplot2::geom_hline(yintercept = -0.15, linewidth = 0.55, color = presentation_colors[["accent"]], linetype = "dashed") +
    ggplot2::annotate(
      "text",
      x = 1951,
      y = -0.15,
      label = "-15%",
      color = presentation_colors[["accent"]],
      family = presentation_font_family,
      fontface = "bold",
      size = 3.4,
      vjust = -0.45
    ) +
    # Background episodes stay faint so the top-10 contractions remain readable.
    ggplot2::geom_line(
      data = plot_data[plot_data$highlight_group == "baseline", ],
      ggplot2::aes(
        x = year,
        y = episode_cumulative_change,
        group = episode_key,
        color = "Otros episodios"
      ),
      linewidth = 0.25,
      alpha = 0.65
    ) +
    # Highlight the worst non-Venezuela episodes in dark blue.
    ggplot2::geom_line(
      data = plot_data[plot_data$highlight_group == "top_10", ],
      ggplot2::aes(
        x = year,
        y = episode_cumulative_change,
        group = episode_key,
        color = "Top 10 episodios"
      ),
      linewidth = 0.75,
      alpha = 0.95
    ) +
    # Draw Venezuela on top so its collapse is never hidden by the comparison set.
    ggplot2::geom_line(
      data = venezuela_plot_data,
      ggplot2::aes(
        x = year,
        y = episode_cumulative_change,
        group = episode_group,
        color = "Venezuela"
      ),
      linewidth = 1.1,
      alpha = 0.95
    ) +
    # Mark each labelled episode endpoint to anchor the callout leader lines.
    ggplot2::geom_point(
      data = top_label_data,
      ggplot2::aes(x = year, y = episode_cumulative_change),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.4,
      stroke = 0.35,
      color = presentation_colors[["ink"]],
      fill = presentation_colors[["primary"]]
    ) +
    ggplot2::geom_point(
      data = venezuela_label_data,
      ggplot2::aes(x = year, y = episode_cumulative_change),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.8,
      stroke = 0.35,
      color = presentation_colors[["ink"]],
      fill = presentation_colors[["venezuela"]]
    ) +
    # Use the shared label style so episode labels match OWID and scatter plots.
    ggrepel::geom_label_repel(
      data = label_data,
      ggplot2::aes(x = year, y = episode_cumulative_change, label = label),
      inherit.aes = FALSE,
      color = presentation_colors[["ink"]],
      family = presentation_font_family,
      size = presentation_label_small_text_size,
      fill = "white",
      label.size = presentation_label_box_linewidth,
      label.padding = presentation_label_padding,
      label.r = presentation_label_radius,
      box.padding = presentation_label_box_padding,
      point.padding = presentation_label_point_padding,
      segment.color = presentation_colors[["muted"]],
      segment.size = presentation_label_segment_size,
      min.segment.length = 0,
      seed = 1234,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Otros episodios" = presentation_colors[["grid"]],
        "Top 10 episodios" = presentation_colors[["primary"]],
        "Venezuela" = presentation_colors[["venezuela"]]
      ),
      breaks = c("Otros episodios", "Top 10 episodios", "Venezuela"),
      name = NULL
    ) +
    presentation_recent_year_axis(1950) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      limits = c(-1, 0),
      breaks = seq(-1, 0, by = 0.25)
    ) +
    ggplot2::coord_cartesian(clip = "off", expand = FALSE) +
    ggplot2::labs(
      title = "Episodios negativos de PIB real per cápita (Maddison)",
      x = NULL,
      y = "Cambio acumulado desde el inicio"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    ggplot2::theme(plot.margin = ggplot2::margin(5.5, 70, 5.5, 5.5))
}

build_wdi_negative_episode_chart <- function(path_data, summary_data, venezuela_path_data, top_n = 10) {
  negative_summary <- summary_data %>%
    dplyr::filter(country_code != "VEN", phase == "contraction", end_year >= 1965, start_year <= 2026) %>%
    dplyr::mutate(episode_key = paste(country_code, episode_id, sep = "_"))

  worst_summary <- negative_summary %>%
    dplyr::slice_min(cumulative_growth, n = top_n, with_ties = FALSE) %>%
    dplyr::arrange(cumulative_growth)

  top_episodes <- worst_summary$episode_key

  plot_data <- path_data %>%
    dplyr::filter(country_code != "VEN", phase == "contraction", year >= 1965, year <= 2026) %>%
    dplyr::mutate(
      episode_key = paste(country_code, episode_id, sep = "_"),
      highlight_group = dplyr::case_when(
        episode_key %in% top_episodes ~ "top_10",
        TRUE ~ "baseline"
      )
    )

  venezuela_plot_data <- venezuela_path_data %>%
    dplyr::filter(
      as.character(series_id) == as.character(series_labels[["gdp_per_capita"]]),
      phase == "contraction",
      year >= 1965,
      year <= 2026
    ) %>%
    dplyr::group_by(episode_group) %>%
    dplyr::mutate(episode_min_change = min(episode_cumulative_change, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(episode_min_change == min(episode_min_change, na.rm = TRUE))

  label_data <- build_negative_episode_labels(plot_data, worst_summary, venezuela_plot_data)
  top_label_data <- label_data %>% dplyr::filter(label_group == "Top 10 episodios")
  venezuela_label_data <- label_data %>% dplyr::filter(label_group == "Venezuela")

  ggplot2::ggplot() +
    # Reference lines make the no-change and -15% thresholds explicit.
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = presentation_colors[["ink"]]) +
    ggplot2::geom_hline(yintercept = -0.15, linewidth = 0.55, color = presentation_colors[["accent"]], linetype = "dashed") +
    ggplot2::annotate(
      "text",
      x = 1966,
      y = -0.15,
      label = "-15%",
      color = presentation_colors[["accent"]],
      family = presentation_font_family,
      fontface = "bold",
      size = 3.4,
      vjust = -0.45
    ) +
    # Baseline WDI episodes remain light so highlighted contractions dominate.
    ggplot2::geom_line(
      data = plot_data[plot_data$highlight_group == "baseline", ],
      ggplot2::aes(x = year, y = episode_cumulative_change, group = episode_key, color = "Otros episodios"),
      linewidth = 0.25,
      alpha = 0.65
    ) +
    # Top-10 non-Venezuela contractions provide the benchmark set.
    ggplot2::geom_line(
      data = plot_data[plot_data$highlight_group == "top_10", ],
      ggplot2::aes(x = year, y = episode_cumulative_change, group = episode_key, color = "Top 10 episodios"),
      linewidth = 0.75,
      alpha = 0.95
    ) +
    # Venezuela is layered last and colored red for consistency across the deck.
    ggplot2::geom_line(
      data = venezuela_plot_data,
      ggplot2::aes(x = year, y = episode_cumulative_change, group = episode_group, color = "Venezuela"),
      linewidth = 1.1,
      alpha = 0.95
    ) +
    # Endpoint dots provide clear anchors for the repelled labels.
    ggplot2::geom_point(
      data = top_label_data,
      ggplot2::aes(x = year, y = episode_cumulative_change),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.4,
      stroke = 0.35,
      color = presentation_colors[["ink"]],
      fill = presentation_colors[["primary"]]
    ) +
    ggplot2::geom_point(
      data = venezuela_label_data,
      ggplot2::aes(x = year, y = episode_cumulative_change),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.8,
      stroke = 0.35,
      color = presentation_colors[["ink"]],
      fill = presentation_colors[["venezuela"]]
    ) +
    # ISO-year labels follow the same callout style as other scatter annotations.
    ggrepel::geom_label_repel(
      data = label_data,
      ggplot2::aes(x = year, y = episode_cumulative_change, label = label),
      inherit.aes = FALSE,
      color = presentation_colors[["ink"]],
      family = presentation_font_family,
      size = presentation_label_small_text_size,
      fill = "white",
      label.size = presentation_label_box_linewidth,
      label.padding = presentation_label_padding,
      label.r = presentation_label_radius,
      box.padding = presentation_label_box_padding,
      point.padding = presentation_label_point_padding,
      segment.color = presentation_colors[["muted"]],
      segment.size = presentation_label_segment_size,
      min.segment.length = 0,
      seed = 1234,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Otros episodios" = presentation_colors[["grid"]],
        "Top 10 episodios" = presentation_colors[["primary"]],
        "Venezuela" = presentation_colors[["venezuela"]]
      ),
      breaks = c("Otros episodios", "Top 10 episodios", "Venezuela"),
      name = NULL
    ) +
    presentation_recent_year_axis(1960) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      limits = c(-1, 0),
      breaks = seq(-1, 0, by = 0.25)
    ) +
    ggplot2::coord_cartesian(clip = "off", expand = FALSE) +
    ggplot2::labs(
      title = "Episodios negativos de PIB real per cápita (WDI)",
      x = NULL,
      y = "Cambio acumulado desde el inicio"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    ggplot2::theme(plot.margin = ggplot2::margin(5.5, 70, 5.5, 5.5))
}

build_imf_weo_negative_episode_chart <- function(path_data, summary_data, venezuela_path_data, top_n = 10) {
  negative_summary <- summary_data %>%
    dplyr::filter(country_code != "VEN", phase == "contraction", end_year >= 1981, start_year <= 2026) %>%
    dplyr::mutate(episode_key = paste(country_code, episode_id, sep = "_"))

  worst_summary <- negative_summary %>%
    dplyr::slice_min(cumulative_growth, n = top_n, with_ties = FALSE) %>%
    dplyr::arrange(cumulative_growth)

  top_episodes <- worst_summary$episode_key

  plot_data <- path_data %>%
    dplyr::filter(country_code != "VEN", phase == "contraction", year >= 1981, year <= 2026) %>%
    dplyr::mutate(
      episode_key = paste(country_code, episode_id, sep = "_"),
      highlight_group = dplyr::case_when(
        episode_key %in% top_episodes ~ "top_10",
        TRUE ~ "baseline"
      )
    )

  venezuela_plot_data <- venezuela_path_data %>%
    dplyr::filter(
      as.character(series_id) == as.character(series_labels[["gdp_per_capita"]]),
      phase == "contraction",
      year >= 1981,
      year <= 2026
    ) %>%
    dplyr::group_by(episode_group) %>%
    dplyr::mutate(episode_min_change = min(episode_cumulative_change, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(episode_min_change == min(episode_min_change, na.rm = TRUE))

  label_data <- build_negative_episode_labels(plot_data, worst_summary, venezuela_plot_data)
  top_label_data <- label_data %>% dplyr::filter(label_group == "Top 10 episodios")
  venezuela_label_data <- label_data %>% dplyr::filter(label_group == "Venezuela")

  ggplot2::ggplot() +
    # IMF-WEO chart follows the same threshold and visual hierarchy as WDI/Maddison.
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = presentation_colors[["ink"]]) +
    ggplot2::geom_hline(yintercept = -0.15, linewidth = 0.55, color = presentation_colors[["accent"]], linetype = "dashed") +
    ggplot2::annotate(
      "text",
      x = 1982,
      y = -0.15,
      label = "-15%",
      color = presentation_colors[["accent"]],
      family = presentation_font_family,
      fontface = "bold",
      size = 3.4,
      vjust = -0.45
    ) +
    # Other international contractions provide the faint background context.
    ggplot2::geom_line(
      data = plot_data[plot_data$highlight_group == "baseline", ],
      ggplot2::aes(x = year, y = episode_cumulative_change, group = episode_key, color = "Otros episodios"),
      linewidth = 0.25,
      alpha = 0.65
    ) +
    # Top-10 IMF-WEO contractions are emphasized in the shared dark-blue style.
    ggplot2::geom_line(
      data = plot_data[plot_data$highlight_group == "top_10", ],
      ggplot2::aes(x = year, y = episode_cumulative_change, group = episode_key, color = "Top 10 episodios"),
      linewidth = 0.75,
      alpha = 0.95
    ) +
    # Venezuela stays in red and is drawn over all comparison paths.
    ggplot2::geom_line(
      data = venezuela_plot_data,
      ggplot2::aes(x = year, y = episode_cumulative_change, group = episode_group, color = "Venezuela"),
      linewidth = 1.1,
      alpha = 0.95
    ) +
    # Endpoint markers make the callout labels easier to trace back to a path.
    ggplot2::geom_point(
      data = top_label_data,
      ggplot2::aes(x = year, y = episode_cumulative_change),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.4,
      stroke = 0.35,
      color = presentation_colors[["ink"]],
      fill = presentation_colors[["primary"]]
    ) +
    ggplot2::geom_point(
      data = venezuela_label_data,
      ggplot2::aes(x = year, y = episode_cumulative_change),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.8,
      stroke = 0.35,
      color = presentation_colors[["ink"]],
      fill = presentation_colors[["venezuela"]]
    ) +
    # Repelled labels use ISO code plus years, matching the other disaster charts.
    ggrepel::geom_label_repel(
      data = label_data,
      ggplot2::aes(x = year, y = episode_cumulative_change, label = label),
      inherit.aes = FALSE,
      color = presentation_colors[["ink"]],
      family = presentation_font_family,
      size = presentation_label_small_text_size,
      fill = "white",
      label.size = presentation_label_box_linewidth,
      label.padding = presentation_label_padding,
      label.r = presentation_label_radius,
      box.padding = presentation_label_box_padding,
      point.padding = presentation_label_point_padding,
      segment.color = presentation_colors[["muted"]],
      segment.size = presentation_label_segment_size,
      min.segment.length = 0,
      seed = 1234,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Otros episodios" = presentation_colors[["grid"]],
        "Top 10 episodios" = presentation_colors[["primary"]],
        "Venezuela" = presentation_colors[["venezuela"]]
      ),
      breaks = c("Otros episodios", "Top 10 episodios", "Venezuela"),
      name = NULL
    ) +
    presentation_recent_year_axis(1980) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      limits = c(-1, 0),
      breaks = seq(-1, 0, by = 0.25)
    ) +
    ggplot2::coord_cartesian(clip = "off", expand = FALSE) +
    ggplot2::labs(
      title = "Episodios negativos de PIB real per cápita (FMI WEO)",
      x = NULL,
      y = "Cambio acumulado desde el inicio"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    ggplot2::theme(plot.margin = ggplot2::margin(5.5, 70, 5.5, 5.5))
}

### Episode comparison summaries ----
build_venezuela_episode_summary_for_source <- function(
  venezuela_summary_data,
  source_name,
  source_label,
  selected_series,
  period_start
) {
  venezuela_summary_data %>%
    dplyr::filter(
      series_id == selected_series,
      start_year >= period_start,
      start_year <= 2026
    ) %>%
    dplyr::mutate(
      source = source_name,
      source_label = source_label,
      country_code = "VEN",
      country = "Venezuela",
      episode_key = paste(source_name, country_code, episode_id, sep = "_"),
      cagr = (1 + cumulative_growth)^(1 / pmax(duration_years, 1)) - 1,
      is_venezuela = TRUE
    )
}

build_source_episode_summary <- function(
  summary_data,
  source_name,
  source_label,
  period_start,
  country_name_column = "country"
) {
  filtered_data <- summary_data %>%
    dplyr::filter(
      country_code != "VEN",
      start_year >= period_start,
      start_year <= 2026
    )

  country_names <- if (country_name_column %in% names(filtered_data)) {
    filtered_data[[country_name_column]]
  } else if ("maddison_country" %in% names(filtered_data)) {
    filtered_data[["maddison_country"]]
  } else {
    filtered_data[["country_code"]]
  }

  filtered_data %>%
    dplyr::mutate(
      source = source_name,
      source_label = source_label,
      country = country_names,
      episode_key = paste(source_name, country_code, episode_id, sep = "_"),
      cagr = (1 + cumulative_growth)^(1 / pmax(duration_years, 1)) - 1,
      is_venezuela = FALSE
    )
}

build_international_episode_summary <- function(
  maddison_summary_data,
  wdi_summary_data,
  imf_weo_summary_data,
  venezuela_summary_data
) {
  dplyr::bind_rows(
    build_source_episode_summary(
      maddison_summary_data,
      source_name = "maddison",
      source_label = "Maddison (PIB per cápita)",
      period_start = 1950,
      country_name_column = "country"
    ),
    build_venezuela_episode_summary_for_source(
      venezuela_summary_data,
      source_name = "maddison",
      source_label = "Maddison (PIB per cápita)",
      selected_series = series_labels[["gdp_per_capita"]],
      period_start = 1950
    ),
    build_source_episode_summary(
      wdi_summary_data,
      source_name = "wdi",
      source_label = "WDI (PIB per cápita)",
      period_start = 1965,
      country_name_column = "country"
    ),
    build_venezuela_episode_summary_for_source(
      venezuela_summary_data,
      source_name = "wdi",
      source_label = "WDI (PIB per cápita)",
      selected_series = series_labels[["gdp_per_capita"]],
      period_start = 1965
    ),
    build_source_episode_summary(
      imf_weo_summary_data,
      source_name = "imf_weo",
      source_label = "FMI WEO (PIB per cápita)",
      period_start = 1981,
      country_name_column = "maddison_country"
    ),
    build_venezuela_episode_summary_for_source(
      venezuela_summary_data,
      source_name = "imf_weo",
      source_label = "FMI WEO (PIB per cápita)",
      selected_series = series_labels[["gdp_per_capita"]],
      period_start = 1981
    )
  ) %>%
    dplyr::mutate(
      country_episode = sprintf("%s (%s-%s)", country_code, start_year, end_year),
      highlight = dplyr::if_else(is_venezuela, "Venezuela", "Otros países")
    )
}

select_ranked_episode_rows <- function(
  data,
  metric,
  phase_value,
  n_per_source = 12,
  largest = FALSE,
  min_duration_years = 0
) {
  phase_data <- data %>%
    dplyr::filter(phase == phase_value, duration_years >= min_duration_years) %>%
    dplyr::mutate(metric_value = .data[[metric]])

  phase_data$rank_metric <- if (largest) {
    -1 * phase_data$metric_value
  } else {
    phase_data$metric_value
  }

  top_rows <- phase_data %>%
    dplyr::filter(!is_venezuela) %>%
    dplyr::group_by(source_label) %>%
    dplyr::slice_min(
      rank_metric,
      n = n_per_source,
      with_ties = FALSE
    ) %>%
    dplyr::ungroup()

  venezuela_rows <- phase_data %>%
    dplyr::filter(is_venezuela) %>%
    dplyr::group_by(source_label) %>%
    dplyr::slice_min(
      rank_metric,
      n = 1,
      with_ties = FALSE
    ) %>%
    dplyr::ungroup()

  ranked_data <- dplyr::bind_rows(top_rows, venezuela_rows) %>%
    dplyr::distinct(source_label, episode_key, .keep_all = TRUE) %>%
    dplyr::group_by(source_label) %>%
    dplyr::mutate(
      rank_value = rank(rank_metric, ties.method = "first"),
      plot_label = country_episode
    ) %>%
    dplyr::arrange(source_label, rank_metric, .by_group = TRUE) %>%
    dplyr::ungroup()

  ranked_data$plot_label <- stats::reorder(ranked_data$plot_label, ranked_data$metric_value)
  ranked_data
}

### Episode comparison charts ----
build_episode_comparison_chart <- function(
  comparison_data,
  metric,
  phase_value,
  title,
  x_label,
  value_labels,
  n_per_source = 12,
  largest = FALSE,
  min_duration_years = 0,
  x_limits = NULL,
  selected_source_label = NULL
) {
  plot_data <- select_ranked_episode_rows(
    comparison_data,
    metric = metric,
    phase_value = phase_value,
    n_per_source = n_per_source,
    largest = largest,
    min_duration_years = min_duration_years
  )

  if (!is.null(selected_source_label)) {
    plot_data <- plot_data[plot_data$source_label == selected_source_label, ]
  }

  chart <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = metric_value, y = plot_label, fill = highlight)
  ) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::scale_fill_manual(
      values = c(
        "Venezuela" = presentation_colors[["venezuela"]],
        "Otros países" = presentation_colors[["primary"]]
      ),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(labels = value_labels, limits = x_limits) +
    ggplot2::labs(title = title, x = x_label, y = NULL) +
    ggplot2::theme_minimal(base_size = presentation_compact_base_size, base_family = presentation_font_family) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 9.8, family = presentation_font_family),
      strip.text = ggplot2::element_text(face = "bold", family = presentation_font_family)
    )

  if (is.null(selected_source_label)) {
    chart <- chart + ggplot2::facet_wrap(ggplot2::vars(source_label), ncol = 1, scales = "free_y")
  }

  if (phase_value == "contraction" && metric == "cumulative_growth") {
    chart <- chart +
      ggplot2::geom_vline(xintercept = -0.15, color = presentation_colors[["negative"]], linewidth = 0.55, linetype = "dashed") +
      ggplot2::annotate(
        "text",
        x = -0.15,
        y = Inf,
        label = "Crisis (-15%)",
        color = presentation_colors[["negative"]],
        family = presentation_font_family,
        size = 3.6,
        angle = 90,
        hjust = 1.05,
        vjust = -0.35
      ) +
      ggplot2::geom_vline(xintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.3)
  }

  chart
}

build_source_series_from_episode_path <- function(
  path_data,
  source_name,
  source_label,
  period_start,
  country_name_column = "country"
) {
  country_names <- if (country_name_column %in% names(path_data)) {
    path_data[[country_name_column]]
  } else if ("maddison_country" %in% names(path_data)) {
    path_data[["maddison_country"]]
  } else {
    path_data[["country_code"]]
  }

  path_data %>%
    dplyr::mutate(
      source = source_name,
      source_label = source_label,
      country = country_names
    ) %>%
    dplyr::filter(country_code != "VEN", year >= period_start, year <= 2026) %>%
    dplyr::distinct(source, source_label, country_code, country, year, index_value)
}

build_venezuela_series_for_source <- function(
  data,
  source_name,
  source_label,
  selected_series,
  period_start
) {
  data %>%
    dplyr::filter(series_id == selected_series, year >= period_start, year <= 2026) %>%
    dplyr::transmute(
      source = source_name,
      source_label = source_label,
      country_code = "VEN",
      country = "Venezuela",
      year = year,
      index_value = index_value
    )
}

build_international_series_for_recovery <- function(
  maddison_path_data,
  wdi_path_data,
  imf_weo_path_data,
  venezuela_index_data
) {
  dplyr::bind_rows(
    build_source_series_from_episode_path(
      maddison_path_data,
      source_name = "maddison",
      source_label = "Maddison (PIB per cápita)",
      period_start = 1950,
      country_name_column = "country"
    ),
    build_venezuela_series_for_source(
      venezuela_index_data,
      source_name = "maddison",
      source_label = "Maddison (PIB per cápita)",
      selected_series = series_labels[["gdp_per_capita"]],
      period_start = 1950
    ),
    build_source_series_from_episode_path(
      wdi_path_data,
      source_name = "wdi",
      source_label = "WDI (PIB per cápita)",
      period_start = 1965,
      country_name_column = "country"
    ),
    build_venezuela_series_for_source(
      venezuela_index_data,
      source_name = "wdi",
      source_label = "WDI (PIB per cápita)",
      selected_series = series_labels[["gdp_per_capita"]],
      period_start = 1965
    ),
    build_source_series_from_episode_path(
      imf_weo_path_data,
      source_name = "imf_weo",
      source_label = "FMI WEO (PIB per cápita)",
      period_start = 1981,
      country_name_column = "maddison_country"
    ),
    build_venezuela_series_for_source(
      venezuela_index_data,
      source_name = "imf_weo",
      source_label = "FMI WEO (PIB per cápita)",
      selected_series = series_labels[["gdp_per_capita"]],
      period_start = 1981
    )
  )
}

### Disaster recovery data ----
build_disaster_recovery_data <- function(
  comparison_data,
  series_data,
  disaster_threshold = -0.15,
  max_years_after_start = 35L
) {
  disasters <- comparison_data %>%
    dplyr::filter(phase == "contraction", cumulative_growth <= disaster_threshold) %>%
    dplyr::mutate(
      disaster_key = paste(source, country_code, episode_id, sep = "_"),
      disaster_label = sprintf("%s (%s-%s)", country, start_year, end_year)
    )

  disaster_rows <- lapply(seq_len(nrow(disasters)), function(i) {
    disaster <- disasters[i, , drop = FALSE]
    country_series <- series_data %>%
      dplyr::filter(
        source == disaster$source[[1]],
        country_code == disaster$country_code[[1]],
        year >= disaster$start_year[[1]],
        year <= disaster$start_year[[1]] + max_years_after_start
      ) %>%
      dplyr::arrange(year)

    if (nrow(country_series) == 0 || !any(country_series$year == disaster$start_year[[1]])) {
      return(NULL)
    }

    start_index <- country_series$index_value[country_series$year == disaster$start_year[[1]]][[1]]
    country_series$index_start_100 <- country_series$index_value / start_index * 100
    country_series$years_since_start <- country_series$year - disaster$start_year[[1]]

    recovery_candidates <- country_series %>%
      dplyr::filter(year > disaster$end_year[[1]], index_start_100 >= 100)
    recovery_year <- if (nrow(recovery_candidates) > 0) {
      recovery_candidates$year[[1]]
    } else {
      NA_integer_
    }

    country_series %>%
      dplyr::mutate(
        disaster_key = disaster$disaster_key[[1]],
        disaster_label = disaster$disaster_label[[1]],
        source_label = disaster$source_label[[1]],
        cumulative_growth = disaster$cumulative_growth[[1]],
        duration_years = disaster$duration_years[[1]],
        start_year = disaster$start_year[[1]],
        end_year = disaster$end_year[[1]],
        recovery_year = recovery_year,
        years_to_recover = ifelse(is.na(recovery_year), NA_real_, recovery_year - disaster$start_year[[1]]),
        is_venezuela = disaster$is_venezuela[[1]],
        highlight = dplyr::if_else(disaster$is_venezuela[[1]], "Venezuela", "Otros países")
      )
  })

  dplyr::bind_rows(disaster_rows)
}

build_disaster_recovery_path_chart <- function(disaster_data, top_n = 10) {
  deepest_disasters <- disaster_data %>%
    dplyr::filter(!is_venezuela) %>%
    dplyr::distinct(source_label, disaster_key, cumulative_growth) %>%
    dplyr::group_by(source_label) %>%
    dplyr::slice_min(cumulative_growth, n = top_n, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::pull(disaster_key)

  plot_data <- disaster_data %>%
    dplyr::mutate(
      path_group = dplyr::case_when(
        is_venezuela ~ "Venezuela",
        disaster_key %in% deepest_disasters ~ "Peores desastres",
        TRUE ~ "Otros desastres"
      )
    )

  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 100, color = presentation_colors[["ink"]], linewidth = 0.3) +
    ggplot2::geom_line(
      data = plot_data[plot_data$path_group == "Otros desastres", ],
      ggplot2::aes(x = years_since_start, y = index_start_100, group = disaster_key),
      color = presentation_colors[["muted"]],
      linewidth = 0.25,
      alpha = 0.45
    ) +
    ggplot2::geom_line(
      data = plot_data[plot_data$path_group == "Peores desastres", ],
      ggplot2::aes(x = years_since_start, y = index_start_100, group = disaster_key),
      color = presentation_colors[["primary"]],
      linewidth = 0.55,
      alpha = 0.85
    ) +
    ggplot2::geom_line(
      data = plot_data[plot_data$path_group == "Venezuela", ],
      ggplot2::aes(x = years_since_start, y = index_start_100, group = disaster_key),
      color = presentation_colors[["venezuela"]],
      linewidth = 1,
      alpha = 0.95
    ) +
    ggplot2::facet_wrap(ggplot2::vars(source_label), ncol = 1) +
    ggplot2::coord_cartesian(xlim = c(0, 35), ylim = c(0, 160), expand = FALSE) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(5)) +
    ggplot2::scale_y_continuous(labels = presentation_number_label(accuracy = 0.1), breaks = scales::breaks_width(40)) +
    ggplot2::labs(
      title = "Recuperación tras desastres de al menos -15%",
      x = "Años desde el inicio de la caída",
      y = "Índice, inicio del desastre = 100"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family)
}

build_disaster_recovery_time_chart <- function(disaster_data, selected_source_label = NULL) {
  recovery_summary <- disaster_data %>%
    dplyr::distinct(
      source_label,
      disaster_key,
      disaster_label,
      cumulative_growth,
      duration_years,
      years_to_recover,
      recovery_year,
      is_venezuela
    ) %>%
    dplyr::mutate(
      recovery_status = dplyr::if_else(is.na(years_to_recover), "Sin recuperar", "Recuperado"),
      recovery_status = factor(recovery_status, levels = c("Recuperado", "Sin recuperar")),
      fall_magnitude = -cumulative_growth,
      highlight = dplyr::case_when(
        is_venezuela ~ "Venezuela",
        recovery_status == "Sin recuperar" ~ "Sin recuperar",
        TRUE ~ "Otros países"
      ),
      highlight = factor(highlight, levels = c("Otros países", "Sin recuperar", "Venezuela")),
      y_value = dplyr::if_else(is.na(years_to_recover), 37, years_to_recover),
      venezuela_label = dplyr::if_else(is_venezuela, paste("VEN", gsub("Venezuela ", "", disaster_label)), "")
    )

  if (!is.null(selected_source_label)) {
    recovery_summary <- recovery_summary[recovery_summary$source_label == selected_source_label, ]
  }

  chart <- ggplot2::ggplot(
    recovery_summary,
    ggplot2::aes(x = fall_magnitude, y = y_value, color = highlight, shape = recovery_status)
  ) +
    ggplot2::annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = 35.2,
      ymax = Inf,
      fill = presentation_colors[["light"]],
      alpha = 0.8
    ) +
    ggplot2::geom_hline(yintercept = 35.2, color = presentation_colors[["muted"]], linewidth = 0.3, linetype = "dashed") +
    ggplot2::geom_point(
      position = ggplot2::position_jitter(width = 0.0025, height = 0.25, seed = 12),
      alpha = 0.7,
      size = 2
    ) +
    ggplot2::geom_point(
      data = recovery_summary[recovery_summary$is_venezuela, ],
      size = 3.3,
      alpha = 0.95
    ) +
    ggplot2::geom_text(
      data = recovery_summary[recovery_summary$is_venezuela, ],
      ggplot2::aes(label = venezuela_label),
      color = presentation_colors[["venezuela"]],
      size = 3,
      hjust = -0.05,
      vjust = -0.2,
      show.legend = FALSE
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0.02, 0.08))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 40),
      breaks = c(seq(0, 30, by = 5), 37),
      labels = c("0", "5", "10", "15", "20", "25", "30", "Sin recuperar")
    ) +
    ggplot2::coord_cartesian(xlim = c(0.15, NA), expand = FALSE) +
    ggplot2::scale_color_manual(
      values = c(
        "Venezuela" = presentation_colors[["venezuela"]],
        "Otros países" = presentation_colors[["primary"]],
        "Sin recuperar" = presentation_colors[["ink"]]
      ),
      name = NULL
    ) +
    ggplot2::scale_shape_manual(
      values = c("Recuperado" = 16, "Sin recuperar" = 4),
      guide = "none"
    ) +
    ggplot2::labs(
      title = "Profundidad del desastre y años hasta recuperar",
      x = "Magnitud de la caída acumulada del episodio",
      y = "Años hasta volver al nivel inicial"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )

  if (is.null(selected_source_label)) {
    chart <- chart + ggplot2::facet_wrap(ggplot2::vars(source_label), ncol = 1)
  }

  chart
}

build_disaster_recovery_heatmap_chart <- function(disaster_data) {
  fall_breaks <- seq(0.10, 1.00, by = 0.10)
  fall_labels <- paste0(
    scales::percent(utils::head(fall_breaks, -1), accuracy = 1),
    "-",
    scales::percent(utils::tail(fall_breaks, -1), accuracy = 1)
  )
  recovery_breaks <- c(0, 5, 10, 15, 20, 25, 30, 35, Inf)
  recovery_labels <- c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30", "31-35", "Sin recuperar")
  source_levels <- c("Maddison (PIB per cápita)", "WDI (PIB per cápita)", "FMI WEO (PIB per cápita)")

  recovery_summary <- disaster_data %>%
    dplyr::distinct(
      source_label,
      disaster_key,
      country_code,
      cumulative_growth,
      years_to_recover,
      is_venezuela
    ) %>%
    dplyr::mutate(
      source_label = factor(source_label, levels = source_levels),
      fall_magnitude = pmin(-cumulative_growth, 0.999),
      fall_bin = cut(
        fall_magnitude,
        breaks = fall_breaks,
        labels = fall_labels,
        include.lowest = TRUE,
        right = FALSE
      ),
      recovery_bin = cut(
        dplyr::if_else(is.na(years_to_recover), Inf, years_to_recover),
        breaks = recovery_breaks,
        labels = recovery_labels,
        include.lowest = TRUE,
        right = TRUE
      )
    ) %>%
    dplyr::filter(!is.na(source_label), !is.na(fall_bin), !is.na(recovery_bin))

  heatmap_counts <- recovery_summary %>%
    dplyr::count(source_label, fall_bin, recovery_bin, name = "episodes") %>%
    dplyr::mutate(
      source_label = factor(source_label, levels = source_levels),
      fall_bin = factor(fall_bin, levels = fall_labels),
      recovery_bin = factor(recovery_bin, levels = rev(recovery_labels))
    )

  heatmap_grid <- expand.grid(
    source_label = factor(source_levels, levels = source_levels),
    fall_bin = factor(fall_labels, levels = fall_labels),
    recovery_bin = factor(rev(recovery_labels), levels = rev(recovery_labels)),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(
      heatmap_counts,
      by = c("source_label", "fall_bin", "recovery_bin")
    ) %>%
    dplyr::mutate(
      episodes = dplyr::coalesce(episodes, 0L),
      label = dplyr::if_else(episodes > 0L, as.character(episodes), ""),
      text_color = dplyr::if_else(episodes >= 4L, "white", presentation_colors[["ink"]])
    )

  ggplot2::ggplot(
    heatmap_grid,
    ggplot2::aes(x = fall_bin, y = recovery_bin, fill = episodes)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = label, color = text_color),
      family = presentation_font_family,
      fontface = "bold",
      size = 4.6
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_gradient(
      low = presentation_colors[["light"]],
      high = presentation_colors[["primary"]],
      breaks = presentation_breaks_include_limits(n = 5),
      name = "Episodios"
    ) +
    ggplot2::facet_wrap(ggplot2::vars(source_label), ncol = 1) +
    ggplot2::labs(
      title = "Severidad del desastre y tiempo de recuperación",
      subtitle = "Cada celda cuenta episodios según caída acumulada y años hasta recuperar el nivel inicial.",
      x = "Caída acumulada del episodio",
      y = "Años hasta recuperar"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size + 1, base_family = presentation_font_family) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, family = presentation_font_family),
      strip.text = ggplot2::element_text(face = "bold", size = 15, family = presentation_font_family)
    )
}

build_disaster_horizon_data <- function(disaster_data, horizons = c(5L, 10L, 15L, 20L, 25L, 30L)) {
  episode_metadata <- disaster_data %>%
    dplyr::distinct(
      source_label,
      disaster_key,
      disaster_label,
      country_code,
      country,
      start_year,
      end_year,
      cumulative_growth,
      duration_years,
      is_venezuela
    )

  horizon_parts <- lapply(horizons, function(horizon) {
    horizon_rows <- disaster_data %>%
      dplyr::distinct(disaster_key, end_year, .keep_all = TRUE) %>%
      dplyr::transmute(
        disaster_key = disaster_key,
        horizon_years_after_end = horizon,
        endpoint_year = end_year + horizon
      ) %>%
      dplyr::inner_join(
        disaster_data %>%
          dplyr::select(disaster_key, endpoint_year = year, horizon_index_start_100 = index_start_100),
        by = c("disaster_key", "endpoint_year")
      )

    if (nrow(horizon_rows) == 0) {
      return(NULL)
    }

    episode_metadata %>%
      dplyr::inner_join(horizon_rows, by = "disaster_key") %>%
      dplyr::mutate(
        horizon_years = horizon,
        horizon_label = sprintf("%s años después", horizon),
        fall_magnitude = -cumulative_growth,
        horizon_ratio_to_start = horizon_index_start_100 / 100,
        total_years_from_start = endpoint_year - start_year,
        horizon_cagr = horizon_ratio_to_start^(1 / total_years_from_start) - 1,
        post_disaster_ratio_to_end = horizon_ratio_to_start / (1 + cumulative_growth),
        post_disaster_cagr = post_disaster_ratio_to_end^(1 / horizon_years_after_end) - 1,
        highlight = dplyr::if_else(is_venezuela, "Venezuela", "Otros países")
      )
  })

  dplyr::bind_rows(horizon_parts) %>%
    dplyr::mutate(
      horizon_label = factor(horizon_label, levels = sprintf("%s años después", horizons)),
      highlight = factor(highlight, levels = c("Otros países", "Venezuela"))
    )
}

add_source_reference_disasters <- function(horizon_data, horizons = c(5L, 10L, 15L, 20L, 25L, 30L)) {
  reference_keys <- horizon_data %>%
    dplyr::filter(!is_venezuela) %>%
    dplyr::distinct(source_label, disaster_key, disaster_label, cumulative_growth, horizon_years) %>%
    dplyr::group_by(source_label, disaster_key, disaster_label, cumulative_growth) %>%
    dplyr::summarise(
      horizon_count = length(unique(horizon_years)),
      .groups = "drop"
    ) %>%
    dplyr::filter(horizon_count == length(horizons)) %>%
    dplyr::group_by(source_label) %>%
    dplyr::slice_min(cumulative_growth, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(reference_label = sprintf("%s", disaster_label)) %>%
    dplyr::select(source_label, disaster_key, reference_label)

  horizon_data %>%
    dplyr::left_join(reference_keys, by = c("source_label", "disaster_key")) %>%
    dplyr::mutate(
      is_source_reference = !is.na(reference_label),
      reference_label = dplyr::if_else(is_source_reference, reference_label, "")
    )
}

build_disaster_horizon_level_chart <- function(horizon_data) {
  ggplot2::ggplot(
    horizon_data,
    ggplot2::aes(x = fall_magnitude, y = horizon_ratio_to_start, color = highlight)
  ) +
    ggplot2::geom_hline(yintercept = 1, color = presentation_colors[["ink"]], linewidth = 0.3, linetype = "dashed") +
    ggplot2::geom_point(
      position = ggplot2::position_jitter(width = 0.002, height = 0, seed = 18),
      alpha = 0.58,
      size = 1.7
    ) +
    ggplot2::geom_point(
      data = horizon_data[horizon_data$is_venezuela, ],
      size = 2.9,
      alpha = 0.95
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(source_label),
      cols = ggplot2::vars(horizon_label)
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0.03, 0.08))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = seq(0, 2, by = 0.5)
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Venezuela" = presentation_colors[["venezuela"]],
        "Otros países" = presentation_colors[["primary"]]
      ),
      name = NULL
    ) +
    ggplot2::coord_cartesian(xlim = c(0.15, NA), ylim = c(0, 2), expand = FALSE) +
    ggplot2::labs(
      title = "Nivel alcanzado después del desastre",
      x = "Magnitud de la caída acumulada",
      y = "PIB per cápita relativo al inicio (=100)"
    ) +
    ggplot2::theme_minimal(base_size = presentation_compact_base_size, base_family = presentation_font_family) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, family = presentation_font_family)
    )
}

build_single_disaster_horizon_level_chart <- function(horizon_data, selected_horizon) {
  fixed_x_limits <- c(
    0.15,
    ceiling(max(horizon_data$fall_magnitude, na.rm = TRUE) * 20) / 20
  )

  plot_data <- horizon_data %>%
    dplyr::filter(horizon_years == selected_horizon) %>%
    dplyr::mutate(source_label = factor(source_label, levels = c("Maddison (PIB per cápita)", "WDI (PIB per cápita)", "FMI WEO (PIB per cápita)")))

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = fall_magnitude, y = horizon_ratio_to_start)
  ) +
    ggplot2::geom_hline(yintercept = 1, color = presentation_colors[["ink"]], linewidth = 0.35, linetype = "dashed") +
    ggplot2::geom_point(
      data = plot_data[!plot_data$is_venezuela, ],
      ggplot2::aes(color = source_label),
      position = ggplot2::position_jitter(width = 0.002, height = 0, seed = 18 + selected_horizon),
      alpha = 0.58,
      size = 2
    ) +
    ggplot2::geom_point(
      data = plot_data[plot_data$is_venezuela, ],
      color = presentation_colors[["venezuela"]],
      size = 3.2,
      alpha = 0.95
    ) +
    ggplot2::geom_point(
      data = plot_data[plot_data$is_source_reference, ],
      color = presentation_colors[["ink"]],
      fill = presentation_colors[["reference"]],
      shape = 23,
      size = 3.4,
      alpha = 0.95
    ) +
    ggplot2::geom_text(
      data = plot_data[plot_data$is_venezuela, ],
      ggplot2::aes(label = sprintf("VEN %s-%s", start_year, end_year)),
      color = presentation_colors[["venezuela"]],
      size = 3,
      hjust = -0.05,
      vjust = -0.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = plot_data[plot_data$is_source_reference, ],
      ggplot2::aes(label = reference_label),
      color = presentation_colors[["ink"]],
      size = 2.8,
      hjust = -0.05,
      vjust = 1.2,
      show.legend = FALSE
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0.03, 0.12))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = seq(0, 2, by = 0.5)
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Maddison (PIB per cápita)" = presentation_colors[["primary"]],
        "WDI (PIB per cápita)" = presentation_colors[["venezuela"]],
        "FMI WEO (PIB per cápita)" = presentation_palette[["cyan"]]
      ),
      name = NULL
    ) +
    ggplot2::coord_cartesian(xlim = fixed_x_limits, ylim = c(0, 2), expand = FALSE) +
    ggplot2::labs(
      title = sprintf("Nivel posterior al desastre: %s años después del final", selected_horizon),
      x = "Magnitud de la caída acumulada",
      y = "PIB per cápita relativo al inicio (=100)"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}

build_disaster_horizon_cagr_chart <- function(horizon_data) {
  ggplot2::ggplot(
    horizon_data,
    ggplot2::aes(x = fall_magnitude, y = post_disaster_cagr, color = highlight)
  ) +
    ggplot2::geom_hline(yintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.3, linetype = "dashed") +
    ggplot2::geom_point(
      position = ggplot2::position_jitter(width = 0.002, height = 0, seed = 19),
      alpha = 0.58,
      size = 1.7
    ) +
    ggplot2::geom_point(
      data = horizon_data[horizon_data$is_venezuela, ],
      size = 2.9,
      alpha = 0.95
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(source_label),
      cols = ggplot2::vars(horizon_label)
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0.03, 0.08))
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), breaks = seq(-0.1, 0.4, by = 0.05)) +
    ggplot2::scale_color_manual(
      values = c(
        "Venezuela" = presentation_colors[["venezuela"]],
        "Otros países" = presentation_colors[["primary"]]
      ),
      name = NULL
    ) +
    ggplot2::coord_cartesian(xlim = c(0.15, NA), ylim = c(-0.1, 0.4), expand = FALSE) +
    ggplot2::labs(
      title = "Crecimiento compuesto después del desastre",
      subtitle = "La TCAC se calcula desde el final de la caída hasta cada horizonte posterior; mide solo la recuperación.",
      x = "Magnitud de la caída acumulada",
      y = "TCAC posterior a la caída"
    ) +
    ggplot2::theme_minimal(base_size = presentation_compact_base_size, base_family = presentation_font_family) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, family = presentation_font_family)
    )
}

build_single_disaster_horizon_cagr_chart <- function(horizon_data, selected_horizon) {
  fixed_x_limits <- c(
    0.15,
    ceiling(max(horizon_data$fall_magnitude, na.rm = TRUE) * 20) / 20
  )

  plot_data <- horizon_data %>%
    dplyr::filter(horizon_years == selected_horizon) %>%
    dplyr::mutate(source_label = factor(source_label, levels = c("Maddison (PIB per cápita)", "WDI (PIB per cápita)", "FMI WEO (PIB per cápita)")))

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = fall_magnitude, y = post_disaster_cagr)
  ) +
    ggplot2::geom_hline(yintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.35, linetype = "dashed") +
    ggplot2::geom_point(
      data = plot_data[!plot_data$is_venezuela, ],
      ggplot2::aes(color = source_label),
      position = ggplot2::position_jitter(width = 0.002, height = 0, seed = 19 + selected_horizon),
      alpha = 0.58,
      size = 2
    ) +
    ggplot2::geom_point(
      data = plot_data[plot_data$is_venezuela, ],
      color = presentation_colors[["venezuela"]],
      size = 3.2,
      alpha = 0.95
    ) +
    ggplot2::geom_point(
      data = plot_data[plot_data$is_source_reference, ],
      color = presentation_colors[["ink"]],
      fill = presentation_colors[["reference"]],
      shape = 23,
      size = 3.4,
      alpha = 0.95
    ) +
    ggplot2::geom_text(
      data = plot_data[plot_data$is_venezuela, ],
      ggplot2::aes(label = sprintf("VEN %s-%s", start_year, end_year)),
      color = presentation_colors[["venezuela"]],
      size = 3,
      hjust = -0.05,
      vjust = -0.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = plot_data[plot_data$is_source_reference, ],
      ggplot2::aes(label = reference_label),
      color = presentation_colors[["ink"]],
      size = 2.8,
      hjust = -0.05,
      vjust = 1.2,
      show.legend = FALSE
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(accuracy = 1),
      expand = ggplot2::expansion(mult = c(0.03, 0.12))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = seq(-0.1, 0.4, by = 0.05)
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Maddison (PIB per cápita)" = presentation_colors[["primary"]],
        "WDI (PIB per cápita)" = presentation_colors[["venezuela"]],
        "FMI WEO (PIB per cápita)" = presentation_palette[["cyan"]]
      ),
      name = NULL
    ) +
    ggplot2::coord_cartesian(xlim = fixed_x_limits, ylim = c(-0.1, 0.4), expand = FALSE) +
    ggplot2::labs(
      title = sprintf("Crecimiento compuesto posterior al desastre: %s años después del final", selected_horizon),
      subtitle = "La TCAC se mide desde el final de la caída hasta el horizonte indicado; excluye la contracción inicial.",
      x = "Magnitud de la caída acumulada",
      y = "TCAC posterior a la caída"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}


build_disaster_horizon_cagr_distribution_chart <- function(horizon_data) {
  plot_data <- horizon_data %>%
    dplyr::mutate(
      horizon_label = factor(horizon_label, levels = sprintf("%s años después", c(5L, 10L, 15L, 20L, 25L, 30L)))
    )
  horizon_colors <- build_priority_color_map(levels(plot_data$horizon_label))

  ggplot2::ggplot(plot_data, ggplot2::aes(x = post_disaster_cagr, color = horizon_label)) +
    ggplot2::geom_vline(xintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.35, linetype = "dashed") +
    ggplot2::geom_density(linewidth = 1.05, adjust = 1.05, na.rm = TRUE) +
    ggplot2::facet_wrap(ggplot2::vars(source_label), ncol = 1) +
    ggplot2::scale_color_manual(values = horizon_colors, name = "Horizonte") +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1), breaks = scales::breaks_width(0.05)) +
    ggplot2::labs(
      title = "Distribución de TCAC post-desastre por horizonte",
      subtitle = "Cada línea mide la TCAC desde el final de la caída hasta el horizonte posterior; excluye la contracción inicial.",
      x = "TCAC posterior a la caída",
      y = "Densidad de episodios"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    ggplot2::theme(legend.position = "bottom")
}
### Recovery projection charts ----
build_recovery_projection_data <- function(data, scenario_rates, start_year = 1900L, average_end_year = 2013L) {
  latest_year <- max(data$year, na.rm = TRUE)
  series_ids_raw <- unique(as.character(data$series_id))

  historical_parts <- vector("list", length(series_ids_raw))
  projection_parts <- vector("list", length(series_ids_raw))

  for (i in seq_along(series_ids_raw)) {
    selected_series <- series_ids_raw[[i]]
    series_data <- data[data$series_id == selected_series, , drop = FALSE]
    peak_index <- max(series_data$index_value, na.rm = TRUE)
    latest_index <- series_data$index_value[series_data$year == latest_year][[1]]

    historical_parts[[i]] <- data.frame(
      series_id = selected_series,
      year = series_data$year,
      index_peak_100 = series_data$index_value / peak_index * 100,
      path_type = "Histórico",
      scenario_label = "Histórico",
      stringsAsFactors = FALSE
    )

    start_row <- series_data[series_data$year == start_year, , drop = FALSE]
    average_end_row <- series_data[series_data$year == average_end_year, , drop = FALSE]
    historical_average <- if (nrow(start_row) == 1 && nrow(average_end_row) == 1) {
      (average_end_row$index_value[[1]] / start_row$index_value[[1]])^(1 / (average_end_year - start_year)) - 1
    } else {
      NA_real_
    }

    scenario_rates <- sort(scenario_rates, decreasing = TRUE)
    rates <- c(scenario_rates, historical_average)
    labels <- c(
      scales::percent(scenario_rates, accuracy = 1),
      sprintf("TCAC %s-%s (%s)", start_year, average_end_year, scales::percent(historical_average, accuracy = 0.1))
    )
    valid_rates <- !is.na(rates)
    rates <- rates[valid_rates]
    labels <- labels[valid_rates]

    projection_years <- seq.int(latest_year, latest_year + 80L)
    scenario_parts <- vector("list", length(rates))
    for (j in seq_along(rates)) {
      projected_index <- latest_index * (1 + rates[[j]])^(projection_years - latest_year)
      projected_index_peak_100 <- projected_index / peak_index * 100
      recovery_positions <- which(projected_index_peak_100 >= 100)
      keep_until <- if (length(recovery_positions) > 0) recovery_positions[[1]] else length(projection_years)
      scenario_years <- projection_years[seq_len(keep_until)]
      scenario_parts[[j]] <- data.frame(
        series_id = selected_series,
        year = scenario_years,
        index_peak_100 = projected_index_peak_100[seq_len(keep_until)],
        path_type = "Proyección",
        scenario_label = labels[[j]],
        growth_rate = rates[[j]],
        stringsAsFactors = FALSE
      )
    }
    projection_parts[[i]] <- do.call(rbind, scenario_parts)
  }

  list(
    historical = do.call(rbind, historical_parts),
    projections = do.call(rbind, projection_parts)
  )
}

build_recovery_projection_chart <- function(data, scenario_rates, title, selected_series = NULL) {
  if (!is.null(selected_series)) {
    data <- data[data$series_id == selected_series, ]
  }

  projection_data <- build_recovery_projection_data(data, scenario_rates, start_year = 1920L)
  last_historical_year <- max(projection_data$historical$year, na.rm = TRUE)
  max_projection_year <- max(projection_data$projections$year, na.rm = TRUE)
  axis_end_year <- 2100
  projection_labels <- projection_data$projections %>%
    dplyr::distinct(scenario_label, growth_rate) %>%
    dplyr::arrange(dplyr::desc(growth_rate)) %>%
    dplyr::pull(scenario_label) %>%
    unique()
  projection_data$projections$scenario_label <- factor(
    projection_data$projections$scenario_label,
    levels = projection_labels
  )
  projection_colors <- build_priority_color_map(projection_labels)

  target_crossings <- projection_data$projections %>%
    dplyr::filter(index_peak_100 >= 100) %>%
    dplyr::arrange(series_id, scenario_label, year) %>%
    dplyr::group_by(series_id, scenario_label) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cross_label = as.character(year))

  chart <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 100, color = presentation_colors[["ink"]], linewidth = 0.45) +
    ggplot2::geom_line(
      data = projection_data$historical,
      ggplot2::aes(x = year, y = index_peak_100),
      color = presentation_colors[["ink"]],
      linewidth = 0.9
    ) +
    ggplot2::geom_line(
      data = projection_data$projections,
      ggplot2::aes(x = year, y = index_peak_100, color = scenario_label),
      linewidth = 1.1
    ) +
    ggplot2::geom_point(
      data = target_crossings,
      ggplot2::aes(x = year, y = index_peak_100, color = scenario_label),
      size = 3.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = target_crossings,
      ggplot2::aes(x = year, y = index_peak_100, label = cross_label, color = scenario_label),
      family = presentation_font_family,
      size = 3.8,
      nudge_y = 7,
      check_overlap = TRUE,
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(xintercept = last_historical_year, color = presentation_colors[["muted"]], linewidth = 0.35, linetype = "dashed") +
    presentation_recovery_year_axis(axis_end_year) +
    ggplot2::scale_y_continuous(labels = presentation_number_label(accuracy = 0.1), breaks = seq(0, 125, by = 25)) +
    ggplot2::scale_color_manual(values = projection_colors) +
    ggplot2::coord_cartesian(ylim = c(0, 125), clip = "off", expand = FALSE) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = "Índice, pico histórico = 100",
      color = "Crecimiento anual compuesto"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.margin = ggplot2::margin(12, 42, 12, 18)
    )

  if (is.null(selected_series)) {
    chart <- chart + ggplot2::facet_wrap(ggplot2::vars(series_id), ncol = 1)
  }

  add_historical_event_references(chart)
}
build_compound_growth_explainer_chart <- function() {
  annual_growth <- c(
    0.08, 0.045, -0.025, 0.065, 0.032,
    -0.045, 0.095, 0.028, -0.018, 0.055
  )
  years <- seq_along(annual_growth)
  compound_rate <- prod(1 + annual_growth)^(1 / length(annual_growth)) - 1

  bar_data <- rbind(
    data.frame(
      year = years,
      rate = annual_growth,
      series = "Crecimiento anual observado",
      stringsAsFactors = FALSE
    ),
    data.frame(
      year = years,
      rate = rep(compound_rate, length(years)),
      series = "Crecimiento compuesto equivalente",
      stringsAsFactors = FALSE
    )
  )
  bar_data$series <- factor(
    bar_data$series,
    levels = c("Crecimiento anual observado", "Crecimiento compuesto equivalente")
  )

  cumulative_data <- rbind(
    data.frame(
      year = c(0, years),
      index_value = c(100, 100 * cumprod(1 + annual_growth)),
      series = "Trayectoria observada",
      stringsAsFactors = FALSE
    ),
    data.frame(
      year = c(0, years),
      index_value = 100 * (1 + compound_rate)^(c(0, years)),
      series = "Trayectoria compuesta equivalente",
      stringsAsFactors = FALSE
    )
  )

  ggplot2::ggplot() +
    ggplot2::geom_col(
      data = bar_data,
      ggplot2::aes(x = year, y = rate, fill = series),
      position = ggplot2::position_dodge(width = 0.75),
      width = 0.68,
      alpha = 0.78
    ) +
    ggplot2::geom_hline(yintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.35) +
    ggplot2::geom_line(
      data = cumulative_data,
      ggplot2::aes(x = year, y = (index_value - 100) / 100, color = series),
      linewidth = 0.9
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Crecimiento anual observado" = mix_with_white(presentation_colors[["primary"]], 0.38),
        "Crecimiento compuesto equivalente" = mix_with_white(presentation_colors[["venezuela"]], 0.42)
      )
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Trayectoria observada" = presentation_colors[["primary"]],
        "Trayectoria compuesta equivalente" = presentation_colors[["venezuela"]]
      )
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2), limits = c(0, 11)) +
    compound_growth_y_axis() +
    ggplot2::labs(
      title = "Crecimiento anual y crecimiento compuesto",
      x = "Año de la simulación",
      y = "Tasa anual / cambio acumulado desde el inicio",
      fill = NULL,
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    ggplot2::theme(legend.position = "bottom")
}

build_recovery_heatmap_data <- function(data, selected_series, n_reference_years = 15L) {
  series_data <- data[data$series_id == selected_series, , drop = FALSE]
  latest_year <- max(series_data$year, na.rm = TRUE)
  latest_index <- series_data$index_value[series_data$year == latest_year][[1]]
  peak_index <- max(series_data$index_value, na.rm = TRUE)
  peak_year <- series_data$year[which.max(series_data$index_value)]

  forced_years <- unique(c(1999L, 2013L, peak_year, latest_year))
  eligible_years <- series_data$year[
    series_data$year >= 1900 &
      series_data$year <= latest_year &
      (series_data$index_value >= latest_index | series_data$year %in% forced_years)
  ]
  eligible_years <- sort(unique(eligible_years))
  optional_years <- setdiff(eligible_years, forced_years)
  optional_slots <- max(n_reference_years - length(intersect(forced_years, eligible_years)), 0L)
  optional_reference_years <- if (optional_slots > 0 && length(optional_years) > 0) {
    optional_years[unique(round(seq(1, length(optional_years), length.out = optional_slots)))]
  } else {
    integer(0)
  }
  reference_years <- sort(unique(c(intersect(forced_years, eligible_years), optional_reference_years)))
  reference_data <- series_data[match(reference_years, series_data$year), , drop = FALSE]
  reference_data$index_peak_100 <- reference_data$index_value / peak_index * 100
  reference_data$column_label <- sprintf("%s\n%s", reference_data$year, scales::number(reference_data$index_peak_100, accuracy = 0.1))

  rates <- seq(0.01, 0.15, by = 0.01)
  heatmap_data <- merge(
    data.frame(growth_rate = rates, stringsAsFactors = FALSE),
    reference_data[, c("year", "index_peak_100", "column_label")],
    all = TRUE
  )
  heatmap_data$latest_index_peak_100 <- latest_index / peak_index * 100
  heatmap_data$years_to_recover <- ifelse(
    heatmap_data$index_peak_100 <= heatmap_data$latest_index_peak_100,
    0,
    ceiling(log(heatmap_data$index_peak_100 / heatmap_data$latest_index_peak_100) / log(1 + heatmap_data$growth_rate))
  )
  heatmap_data$rate_label <- scales::percent(heatmap_data$growth_rate, accuracy = 1)
  heatmap_data$rate_label <- factor(heatmap_data$rate_label, levels = rev(scales::percent(rates, accuracy = 1)))
  heatmap_data$column_label <- factor(heatmap_data$column_label, levels = reference_data$column_label)
  heatmap_data
}

build_recovery_heatmap_chart <- function(data, selected_series, title) {
  heatmap_data <- build_recovery_heatmap_data(data, selected_series) %>%
    dplyr::mutate(text_color = dplyr::if_else(growth_rate <= 0.02, "white", presentation_colors[["ink"]]))

  ggplot2::ggplot(
    heatmap_data,
    ggplot2::aes(x = column_label, y = rate_label, fill = years_to_recover)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35) +
    ggplot2::geom_text(
      ggplot2::aes(label = years_to_recover, color = text_color),
      size = 5,
      fontface = "bold"
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_gradient(
      low = presentation_colors[["light"]],
      high = presentation_colors[["primary"]],
      name = "Años"
    ) +
    ggplot2::labs(
      title = title,
      x = "Año de referencia e Índice (pico = 100)",
      y = "TCAC anual"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size + 1, base_family = presentation_font_family) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 13.5, family = presentation_font_family),
      axis.text.y = ggplot2::element_text(size = 14, family = presentation_font_family),
      panel.grid = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}
### Rolling TCAC charts ----
build_rolling_cagr_data <- function(data, windows = c(5L, 10L, 15L, 20L)) {
  series_values <- split(data, data$series_id)
  rolling_parts <- list()

  for (series_name in names(series_values)) {
    series_data <- series_values[[series_name]]
    series_data <- series_data[order(series_data$year), , drop = FALSE]

    for (window_length in windows) {
      end_years <- series_data$year[(series_data$year - window_length) %in% series_data$year]
      if (length(end_years) == 0) {
        next
      }

      window_rows <- lapply(end_years, function(end_year) {
        start_year <- end_year - window_length
        start_index <- series_data$index_value[series_data$year == start_year][[1]]
        end_index <- series_data$index_value[series_data$year == end_year][[1]]

        data.frame(
          series_id = series_name,
          start_year = start_year,
          end_year = end_year,
          window_years = window_length,
          cagr = (end_index / start_index)^(1 / window_length) - 1,
          stringsAsFactors = FALSE
        )
      })

      rolling_parts[[paste(series_name, window_length, sep = "_")]] <- do.call(rbind, window_rows)
    }
  }

  do.call(rbind, rolling_parts)
}

build_momentum_example_chart <- function(
  data,
  selected_series = series_labels[["gdp_per_capita"]],
  example_windows = data.frame(
    start_year = c(1903L, 2003L),
    end_year = c(1913L, 2013L),
    stringsAsFactors = FALSE
  )
) {
  series_data <- data[data$series_id == selected_series, , drop = FALSE]
  window_summaries <- example_windows %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      start_index = series_data$index_value[series_data$year == start_year][[1]],
      end_index = series_data$index_value[series_data$year == end_year][[1]],
      cagr = (end_index / start_index)^(1 / (end_year - start_year)) - 1,
      window_label = sprintf("%s-%s", start_year, end_year),
      label = sprintf("%s\nTCAC %s", window_label, scales::percent(cagr, accuracy = 0.1))
    ) %>%
    dplyr::ungroup()
  window_colors <- stats::setNames(
    presentation_ordered_colors[seq_len(nrow(window_summaries))],
    window_summaries$window_label
  )
  window_data <- lapply(seq_len(nrow(window_summaries)), function(i) {
    window_row <- window_summaries[i, ]
    segment_data <- series_data[
      series_data$year >= window_row$start_year &
        series_data$year <= window_row$end_year,
      ,
      drop = FALSE
    ]
    segment_data$window_label <- window_row$window_label
    segment_data
  }) %>%
    dplyr::bind_rows()
  endpoint_data <- window_data %>%
    dplyr::filter(year %in% c(min(year), max(year)), .by = window_label)
  label_data <- window_data %>%
    dplyr::filter(year == max(year), .by = window_label) %>%
    dplyr::left_join(window_summaries[, c("window_label", "label")], by = "window_label") %>%
    dplyr::mutate(
      label_nudge_y = dplyr::if_else(
        window_label == window_summaries$window_label[[1]],
        450,
        0
      )
    )
  early_window_data <- window_data[window_data$window_label == window_summaries$window_label[[1]], , drop = FALSE]
  recent_window_data <- window_data[window_data$window_label == window_summaries$window_label[[2]], , drop = FALSE]
  early_endpoint_data <- endpoint_data[endpoint_data$window_label == window_summaries$window_label[[1]], , drop = FALSE]
  recent_endpoint_data <- endpoint_data[endpoint_data$window_label == window_summaries$window_label[[2]], , drop = FALSE]

  ggplot2::ggplot(series_data, ggplot2::aes(x = year, y = index_value)) +
    ggplot2::geom_line(color = presentation_colors[["muted"]], linewidth = 0.55) +
    ggplot2::geom_line(
      data = early_window_data,
      color = window_colors[[1]],
      linewidth = 1
    ) +
    ggplot2::geom_line(
      data = recent_window_data,
      color = window_colors[[2]],
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = early_endpoint_data,
      color = window_colors[[1]],
      size = 2.8,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = recent_endpoint_data,
      color = window_colors[[2]],
      size = 2.8,
      show.legend = FALSE
    ) +
    ggrepel::geom_label_repel(
      data = label_data,
      ggplot2::aes(x = year, y = index_value, label = label),
      inherit.aes = FALSE,
      family = presentation_font_family,
      color = presentation_colors[["ink"]],
      size = presentation_label_text_size,
      fill = "white",
      label.size = presentation_label_box_linewidth,
      label.padding = presentation_label_padding,
      label.r = presentation_label_radius,
      box.padding = presentation_label_box_padding,
      point.padding = presentation_label_point_padding,
      segment.color = presentation_colors[["muted"]],
      segment.size = presentation_label_segment_size,
      min.segment.length = 0,
      nudge_y = label_data$label_nudge_y,
      seed = 1234,
      show.legend = FALSE
    ) +
    presentation_full_history_year_axis() +
    historical_index_y_axis(selected_series) +
    ggplot2::labs(
      title = "Crecimiento compuesto en ventanas móviles",
      subtitle = "Cada ventana calcul el crecimiento compuesto entre el nivel inicial y final de un período consecutivo.",
      x = NULL,
      y = "Índice histórico (1830 = 100)"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    historical_event_reference_layers()
}

build_rolling_cagr_lines_chart <- function(rolling_data, selected_series = NULL, title = "Crecimiento compuesto en ventanas móviles") {
  line_data <- rolling_data %>%
    dplyr::filter(window_years %in% c(5L, 10L, 15L, 20L)) %>%
    dplyr::mutate(window_label = factor(
      paste0(window_years, " años"),
      levels = paste0(c(5L, 10L, 15L, 20L), " años")
    ))
  if (!is.null(selected_series)) {
    line_data <- line_data[line_data$series_id == selected_series, ]
  }
  line_colors <- build_priority_color_map(line_data$window_label)

  chart <- ggplot2::ggplot(line_data, ggplot2::aes(x = end_year, y = cagr, color = window_label)) +
    ggplot2::geom_hline(yintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.65, alpha = 0.9) +
    ggplot2::scale_color_manual(values = line_colors) +
    presentation_full_history_year_axis() +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(accuracy = 1),
      limits = c(-0.25, 0.25),
      breaks = seq(-0.25, 0.25, by = 0.05)
    ) +
    ggplot2::labs(
      title = "Crecimiento compuesto en ventanas móviles",
      x = "Año final de la ventana",
      y = "TCAC de la ventana",
      color = "Ventana"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    ggplot2::theme(legend.position = "bottom")

  chart <- chart + ggplot2::labs(title = title)

  if (is.null(selected_series)) {
    chart <- chart + ggplot2::facet_wrap(ggplot2::vars(series_id), ncol = 1)
  }

  add_historical_event_references(chart)
}

build_rolling_cagr_heatmap_chart <- function(rolling_data, selected_series, title) {
  thresholds <- c(0.02, 0.05, 0.07, 0.10, 0.15)
  windows <- c(3L, 5L, 7L, 10L, 15L, 20L)
  selected_data <- rolling_data %>%
    dplyr::filter(series_id == selected_series, window_years %in% windows)

  heatmap_parts <- lapply(windows, function(window_length) {
    window_data <- selected_data[selected_data$window_years == window_length, , drop = FALSE]
    data.frame(
      window_years = window_length,
      threshold = thresholds,
      share = vapply(thresholds, function(threshold) mean(window_data$cagr >= threshold, na.rm = TRUE), numeric(1)),
      stringsAsFactors = FALSE
    )
  })

  heatmap_data <- do.call(rbind, heatmap_parts) %>%
    dplyr::mutate(
      window_label = factor(paste0(window_years, " años"), levels = paste0(rev(windows), " años")),
      threshold_label = factor(scales::percent(threshold, accuracy = 1), levels = scales::percent(thresholds, accuracy = 1)),
      text_color = dplyr::if_else(share >= 0.3, "white", presentation_colors[["ink"]])
    )

  ggplot2::ggplot(heatmap_data, ggplot2::aes(x = threshold_label, y = window_label, fill = share)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(share, accuracy = 1), color = text_color),
      size = 5.4,
      fontface = "bold"
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_gradient(
      low = presentation_colors[["light"]],
      high = presentation_colors[["primary"]],
      labels = scales::label_percent(accuracy = 1),
      name = "Frecuencia"
    ) +
    ggplot2::labs(
      title = title,
      x = "TCAC mínimo",
      y = "Duración consecutiva"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size + 2, base_family = presentation_font_family) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "bottom")
}
build_rolling_cagr_distribution_chart <- function(
  rolling_data,
  selected_series = NULL,
  title = "Distribución de TCACs en ventanas móviles"
) {
  distribution_data <- rolling_data %>%
    dplyr::filter(window_years %in% c(3L, 5L, 7L, 10L, 15L, 20L)) %>%
    dplyr::mutate(window_label = factor(
      paste0(window_years, " años"),
      levels = paste0(c(3L, 5L, 7L, 10L, 15L, 20L), " años")
    ))
  if (!is.null(selected_series)) {
    distribution_data <- distribution_data[distribution_data$series_id == selected_series, ]
  }
  distribution_colors <- build_priority_color_map(distribution_data$window_label)
  threshold_data <- data.frame(threshold = c(0.02, 0.05, 0.07, 0.10, 0.15))
  density_y_limits <- NULL
  density_y_breaks <- ggplot2::waiver()

  # Fix density ranges for the single-series TCAC distribution charts.
  if (identical(as.character(selected_series), as.character(series_labels[["gdp"]]))) {
    density_y_limits <- c(0, 16)
    density_y_breaks <- seq(0, 16, by = 4)
  } else if (identical(as.character(selected_series), as.character(series_labels[["gdp_per_capita"]]))) {
    density_y_limits <- c(0, 20)
    density_y_breaks <- seq(0, 20, by = 5)
  }

  chart <- ggplot2::ggplot(distribution_data, ggplot2::aes(x = cagr, fill = window_label)) +
    ggplot2::geom_vline(
      data = threshold_data,
      ggplot2::aes(xintercept = threshold),
      inherit.aes = FALSE,
      color = presentation_colors[["muted"]],
      linewidth = 0.25,
      linetype = "dashed"
    ) +
    ggplot2::geom_density(alpha = 0.18, linewidth = 0.65, color = NA) +
    ggplot2::geom_density(ggplot2::aes(color = window_label), fill = NA, linewidth = 0.75) +
    ggplot2::scale_fill_manual(values = distribution_colors) +
    ggplot2::scale_color_manual(values = distribution_colors) +
    ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
    ggplot2::scale_y_continuous(breaks = density_y_breaks) +
    ggplot2::labs(
      title = "Distribución de TCACs en ventanas móviles",
      x = "TCAC de la ventana",
      y = "Densidad",
      fill = "Ventana",
      color = "Ventana"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    ggplot2::theme(legend.position = "bottom")

  chart <- chart + ggplot2::labs(title = title)

  if (is.null(selected_series)) {
    chart <- chart + ggplot2::facet_wrap(ggplot2::vars(series_id), ncol = 1)
  } else if (!is.null(density_y_limits)) {
    chart <- chart + ggplot2::coord_cartesian(ylim = density_y_limits, expand = FALSE)
  }

  chart
}

## Derived analysis tables ----
# Build rolling TCAC windows used by the momentum graphs.
rolling_cagr <- build_rolling_cagr_data(
  index_series,
  windows = c(3L, 5L, 7L, 10L, 15L, 20L)
)

# Combine international episode summaries into one comparison table.
international_episode_comparison <- build_international_episode_summary(
  maddison_episode_summary,
  wdi_pc_episode_summary,
  imf_weo_episode_summary,
  episode_summary
)

international_recovery_series <- build_international_series_for_recovery(
  maddison_episode_path,
  wdi_pc_episode_path,
  imf_weo_episode_path,
  index_series
)

# Summarize deep contractions and recovery time after each disaster episode.
international_disaster_recovery <- build_disaster_recovery_data(
  international_episode_comparison,
  international_recovery_series,
  disaster_threshold = -0.15,
  max_years_after_start = 35L
)

# Measure post-disaster levels and TCACs at fixed horizons.
international_disaster_horizons <- build_disaster_horizon_data(
  international_disaster_recovery,
  horizons = c(5L, 10L, 15L, 20L, 25L, 30L)
)

international_disaster_horizons <- add_source_reference_disasters(
  international_disaster_horizons,
  horizons = c(5L, 10L, 15L, 20L, 25L, 30L)
)

## Derived data outputs ----
# Persist the recovery comparison tables for later review and reporting.
utils::write.csv(
  international_disaster_recovery %>%
    dplyr::distinct(
      source_label,
      disaster_key,
      disaster_label,
      country_code,
      country,
      start_year,
      end_year,
      cumulative_growth,
      duration_years,
      recovery_year,
      years_to_recover,
      is_venezuela
    ) %>%
    dplyr::arrange(source_label, cumulative_growth),
  "data/final/international_disaster_recovery_summary.csv",
  row.names = FALSE
)

utils::write.csv(
  international_disaster_horizons %>%
    dplyr::arrange(source_label, horizon_years, cumulative_growth),
  "data/final/international_disaster_horizon_summary.csv",
  row.names = FALSE
)

## Plot construction ----
# Each graph below is built, saved, and previewed in place for easier review.

density_note <- "Densidad indica la concentración suavizada de episodios o años; el área bajo cada curva se normaliza a uno."

save_historical_plot <- function(plot_name, plot, subtitle = NULL, note = NULL) {
  save_and_preview_plot(
    filename = file.path(figure_dir, paste0(plot_name, ".png")),
    plot = plot,
    source_caption = presentation_source_caption,
    subtitle = subtitle,
    note = note
  )
  message("Wrote ", file.path(figure_dir, paste0(plot_name, ".png")))
  invisible(plot)
}

## Family: annual growth bars and distributions
# Graph: Crecimiento del PIB real
gdp_growth_bars <- build_growth_bar_chart(
    index_series,
    "PIB real",
    "Crecimiento anual del PIB real"
  )

save_historical_plot(
  "gdp_growth_bars",
  gdp_growth_bars,
  subtitle = "Muestra las variaciones anuales positivas y negativas del PIB real desde 1830."
)

# Graph: Crecimiento del PIB real per cápita
gdp_per_capita_growth_bars <- build_growth_bar_chart(
    index_series,
    "PIB real per cápita",
    "Crecimiento anual del PIB real per cápita"
  )

save_historical_plot(
  "gdp_per_capita_growth_bars",
  gdp_per_capita_growth_bars,
  subtitle = "Muestra las variaciones anuales positivas y negativas del PIB real por habitante desde 1830."
)

# Graph: Crecimiento del PIB real y per cápita
growth_bars_faceted <- ggplot2::ggplot(
    index_series,
    ggplot2::aes(x = year, y = growth_rate, fill = growth_direction)
  ) +
    ggplot2::geom_col(width = 0.9) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = presentation_colors[["ink"]]) +
    ggplot2::facet_wrap(ggplot2::vars(series_id), ncol = 1) +
    ggplot2::scale_fill_manual(values = growth_colors, labels = growth_labels, name = NULL) +
    presentation_full_history_year_axis() +
    annual_growth_y_axis() +
    ggplot2::labs(title = "Crecimiento anual", x = NULL, y = "Tasa de crecimiento anual") +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    historical_event_reference_layers()

save_historical_plot(
  "growth_bars_faceted",
  growth_bars_faceted,
  subtitle = "Compara el crecimiento anual total y per cápita con la misma escala vertical."
)

# Graph: Distribución anual del crecimiento
growth_rate_distribution <- ggplot2::ggplot(
    index_series[!is.na(index_series$growth_rate), ],
    ggplot2::aes(x = growth_rate)
  ) +
    ggplot2::geom_histogram(
      ggplot2::aes(fill = "Años"),
      binwidth = 0.01,
      boundary = 0,
      color = "white",
      linewidth = 0.2
    ) +
    ggplot2::geom_density(
      ggplot2::aes(y = ggplot2::after_stat(count * 0.01), color = "Densidad"),
      linewidth = 0.8,
      adjust = 1.1
    ) +
    ggplot2::geom_vline(xintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.35) +
    ggplot2::facet_wrap(ggplot2::vars(series_id), ncol = 1) +
    ggplot2::scale_fill_manual(values = c("Años" = presentation_colors[["muted"]]), name = NULL) +
    ggplot2::scale_color_manual(values = c("Densidad" = presentation_colors[["primary"]]), name = NULL) +
    ggplot2::scale_x_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = scales::breaks_width(0.05)
    ) +
    ggplot2::scale_y_continuous(breaks = presentation_breaks_include_limits()) +
    ggplot2::labs(
      title = "Distribución histórica del crecimiento anual",
      x = "Tasa de crecimiento anual",
      y = "Número de años"
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family)

save_historical_plot(
  "growth_rate_distribution",
  growth_rate_distribution,
  subtitle = "Resume la frecuencia histórica de tasas anuales para PIB real y PIB real per cápita.",
  note = density_note
)

# Graph: Distribución anual del PIB real
gdp_growth_rate_distribution <- build_growth_distribution_chart(
    index_series,
    series_labels[["gdp"]],
    "Distribución del crecimiento anual del PIB real"
  )

save_historical_plot(
  "gdp_growth_rate_distribution",
  gdp_growth_rate_distribution,
  subtitle = "Cuenta años históricos por intervalo de crecimiento anual del PIB real.",
  note = density_note
)

# Graph: Distribución anual per cápita
gdp_per_capita_growth_rate_distribution <- build_growth_distribution_chart(
    index_series,
    series_labels[["gdp_per_capita"]],
    "Distribución del crecimiento anual del PIB real per cápita"
  )

save_historical_plot(
  "gdp_per_capita_growth_rate_distribution",
  gdp_per_capita_growth_rate_distribution,
  subtitle = "Cuenta años históricos por intervalo de crecimiento anual del PIB real per cápita.",
  note = density_note
)

## Family: historical index and anchor lines
# Graph: Índice del PIB real
gdp_index_line <- build_index_line_chart(
    index_series,
    "PIB real",
    "Índice histórico del PIB real"
  )

save_historical_plot(
  "gdp_index_line",
  gdp_index_line,
  subtitle = "Ubica el nivel reciente del PIB real frente a referencias históricas comparables."
)

# Graph: Índice del PIB real per cápita
gdp_per_capita_index_line <- build_index_line_chart(
    index_series,
    "PIB real per cápita",
    "Índice histórico del PIB real per cápita"
  )

save_historical_plot(
  "gdp_per_capita_index_line",
  gdp_per_capita_index_line,
  subtitle = "Ubica el nivel reciente del PIB real per cápita frente a referencias históricas comparables."
)

# Graph: Índice del PIB real y per cápita
index_lines_faceted <- ggplot2::ggplot(index_series, ggplot2::aes(x = year, y = index_value)) +
    ggplot2::geom_line(color = presentation_colors[["ink"]], linewidth = 0.55) +
    ggplot2::facet_wrap(ggplot2::vars(series_id), ncol = 1, scales = "free_y") +
    presentation_full_history_year_axis() +
    ggplot2::scale_y_continuous(labels = presentation_number_label(accuracy = 0.1), breaks = presentation_breaks_include_limits()) +
    ggplot2::labs(title = "Índices históricos", x = NULL, y = "Índice histórico") +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    historical_event_reference_layers()

save_historical_plot(
  "index_lines_faceted",
  index_lines_faceted,
  subtitle = "Compara las trayectorias históricas del PIB real total y por habitante."
)

# Graph: Anclaje del PIB real
gdp_anchor_line <- build_anchor_line_chart(
    index_series,
    "PIB real",
    "PIB real relativo al último año"
  )

save_historical_plot(
  "gdp_anchor_line",
  gdp_anchor_line,
  subtitle = "Reescala la serie para mostrar cada año relativo al último nivel observado."
)

# Graph: Anclaje per cápita
gdp_per_capita_anchor_line <- build_anchor_line_chart(
    index_series,
    "PIB real per cápita",
    "PIB real per cápita relativo al último año"
  )

save_historical_plot(
  "gdp_per_capita_anchor_line",
  gdp_per_capita_anchor_line,
  subtitle = "Reescala la serie per cápita para mostrar cada año relativo al último nivel observado."
)

## Family: domestic growth episodes
# Graph: Episodios del PIB real
gdp_episode_lines <- build_episode_line_chart(
    episodes,
    "PIB real",
    "Episodios históricos del PIB real"
  )

save_historical_plot(
  "gdp_episode_lines",
  gdp_episode_lines,
  subtitle = "Cada línea sigue un episodio acumulado de expansión o contracción del PIB real."
)

# Graph: Episodios per cápita
gdp_per_capita_episode_lines <- build_episode_line_chart(
    episodes,
    "PIB real per cápita",
    "Episodios históricos del PIB real per cápita"
  )

save_historical_plot(
  "gdp_per_capita_episode_lines",
  gdp_per_capita_episode_lines,
  subtitle = "Cada línea sigue un episodio acumulado de expansión o contracción del PIB real per cápita."
)

# Graph: Episodios del PIB real y per cápita
episode_lines_faceted <- ggplot2::ggplot(
    episodes,
    ggplot2::aes(
      x = year,
      y = episode_cumulative_change,
      group = episode_group,
      color = growth_direction
    )
  ) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = presentation_colors[["ink"]]) +
    ggplot2::geom_line(linewidth = 0.75, alpha = 0.62) +
    ggplot2::facet_wrap(ggplot2::vars(series_id), ncol = 1) +
    ggplot2::scale_color_manual(values = growth_colors, labels = growth_labels, name = NULL) +
    presentation_full_history_year_axis() +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), breaks = presentation_breaks_include_limits()) +
    ggplot2::labs(title = "Episodios históricos", x = NULL, y = "Cambio acumulado desde el inicio") +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    historical_event_reference_layers()

save_historical_plot(
  "episode_lines_faceted",
  episode_lines_faceted,
  subtitle = "Compara episodios acumulados del PIB real y del PIB real per cápita."
)

# Graph: Episodios positivos
positive_episode_lines_faceted <- build_episode_phase_faceted_chart(
    episodes,
    "expansion",
    "Episodios históricos de crecimiento positivo"
  )

save_historical_plot(
  "positive_episode_lines_faceted",
  positive_episode_lines_faceted,
  subtitle = "Aísla expansiones históricas para comparar duración y magnitud acumulada."
)

# Graph: Episodios negativos
negative_episode_lines_faceted <- build_episode_phase_faceted_chart(
    episodes,
    "contraction",
    "Episodios históricos de crecimiento negativo"
  )

save_historical_plot(
  "negative_episode_lines_faceted",
  negative_episode_lines_faceted,
  subtitle = "Aísla contracciones históricas para comparar duración y magnitud acumulada."
)

# Graph: Episodios positivos del PIB real
gdp_positive_episode_lines <- build_episode_phase_chart(
    episodes,
    "PIB real",
    "expansion",
    "Episodios positivos del PIB real"
  )

save_historical_plot(
  "gdp_positive_episode_lines",
  gdp_positive_episode_lines,
  subtitle = "Destaca las mayores expansiones acumuladas del PIB real."
)

# Graph: Episodios negativos del PIB real
gdp_negative_episode_lines <- build_episode_phase_chart(
    episodes,
    "PIB real",
    "contraction",
    "Episodios negativos del PIB real"
  )

save_historical_plot(
  "gdp_negative_episode_lines",
  gdp_negative_episode_lines,
  subtitle = "Destaca las mayores contracciones acumuladas del PIB real."
)

# Graph: Episodios positivos per cápita
gdp_per_capita_positive_episode_lines <- build_episode_phase_chart(
    episodes,
    "PIB real per cápita",
    "expansion",
    "Episodios positivos del PIB real per cápita"
  )

save_historical_plot(
  "gdp_per_capita_positive_episode_lines",
  gdp_per_capita_positive_episode_lines,
  subtitle = "Destaca las mayores expansiones acumuladas del PIB real per cápita."
)

# Graph: Episodios negativos per cápita
gdp_per_capita_negative_episode_lines <- build_episode_phase_chart(
    episodes,
    "PIB real per cápita",
    "contraction",
    "Episodios negativos del PIB real per cápita"
  )

save_historical_plot(
  "gdp_per_capita_negative_episode_lines",
  gdp_per_capita_negative_episode_lines,
  subtitle = "Destaca las mayores contracciones acumuladas del PIB real per cápita."
)

## Family: international negative episodes and episode comparisons
# Graph: Episodios negativos internacionales Maddison
maddison_negative_episode_lines <- build_maddison_negative_episode_chart(
    maddison_episode_path,
    maddison_episode_summary,
    episodes,
    top_n = 10
  )

save_historical_plot(
  "maddison_negative_episode_lines",
  maddison_negative_episode_lines,
  subtitle = "Compara a Venezuela con las peores contracciones internacionales de Maddison desde 1950."
)

# Graph: Episodios negativos internacionales WDI
wdi_negative_episode_lines <- build_wdi_negative_episode_chart(
    wdi_pc_episode_path,
    wdi_pc_episode_summary,
    episodes,
    top_n = 10
  )

save_historical_plot(
  "wdi_negative_episode_lines",
  wdi_negative_episode_lines,
  subtitle = "Compara a Venezuela con las peores contracciones internacionales de WDI desde 1965."
)

# Graph: Episodios negativos internacionales FMI WEO
imf_weo_negative_episode_lines <- build_imf_weo_negative_episode_chart(
    imf_weo_episode_path,
    imf_weo_episode_summary,
    episodes,
    top_n = 10
  )

save_historical_plot(
  "imf_weo_negative_episode_lines",
  imf_weo_negative_episode_lines,
  subtitle = "Compara a Venezuela con las peores contracciones internacionales de FMI WEO desde 1980."
)

# Graph: Profundidad de contracciones internacionales
international_contraction_depth <- build_episode_comparison_chart(
    international_episode_comparison,
    metric = "cumulative_growth",
    phase_value = "contraction",
    title = "Peores episodios de contracción: Venezuela frente al mundo",
    x_label = "Cambio acumulado en el episodio",
    value_labels = scales::label_percent(accuracy = 1),
    n_per_source = 12
  )

save_historical_plot(
  "international_contraction_depth",
  international_contraction_depth,
  subtitle = "Ordena las contracciones internacionales por pérdida acumulada del episodio."
)

# Graph: Profundidad de contracciones Maddison
international_contraction_depth_maddison <- build_episode_comparison_chart(
    international_episode_comparison,
    metric = "cumulative_growth",
    phase_value = "contraction",
    title = "Peores episodios de contracción (Maddison)",
    x_label = "Cambio acumulado en el episodio",
    value_labels = scales::label_percent(accuracy = 1),
    n_per_source = 12,
    selected_source_label = "Maddison (PIB per cápita)"
  )

save_historical_plot(
  "international_contraction_depth_maddison",
  international_contraction_depth_maddison,
  subtitle = "Ordena las mayores contracciones acumuladas en la base Maddison."
)

# Graph: Profundidad de contracciones WDI
international_contraction_depth_wdi <- build_episode_comparison_chart(
    international_episode_comparison,
    metric = "cumulative_growth",
    phase_value = "contraction",
    title = "Peores episodios de contracción (WDI)",
    x_label = "Cambio acumulado en el episodio",
    value_labels = scales::label_percent(accuracy = 1),
    n_per_source = 12,
    selected_source_label = "WDI (PIB per cápita)"
  )

save_historical_plot(
  "international_contraction_depth_wdi",
  international_contraction_depth_wdi,
  subtitle = "Ordena las mayores contracciones acumuladas en WDI per cápita."
)

# Graph: Profundidad de contracciones FMI WEO
international_contraction_depth_imf_weo <- build_episode_comparison_chart(
    international_episode_comparison,
    metric = "cumulative_growth",
    phase_value = "contraction",
    title = "Peores episodios de contracción (FMI WEO)",
    x_label = "Cambio acumulado en el episodio",
    value_labels = scales::label_percent(accuracy = 1),
    n_per_source = 12,
    selected_source_label = "FMI WEO (PIB per cápita)"
  )

save_historical_plot(
  "international_contraction_depth_imf_weo",
  international_contraction_depth_imf_weo,
  subtitle = "Ordena las mayores contracciones acumuladas en FMI WEO per cápita."
)

# Graph: Duración de contracciones internacionales
international_contraction_duration <- build_episode_comparison_chart(
    international_episode_comparison,
    metric = "duration_years",
    phase_value = "contraction",
    title = "Episodios de contracción más prolongados",
    x_label = "Duración del episodio, años",
    value_labels = presentation_number_label(accuracy = 0.1),
    n_per_source = 12,
    largest = TRUE
  )

save_historical_plot(
  "international_contraction_duration",
  international_contraction_duration,
  subtitle = "Ordena los episodios de contracción por años consecutivos de duración."
)

# Graph: Duración de contracciones Maddison
international_contraction_duration_maddison <- build_episode_comparison_chart(
    international_episode_comparison,
    metric = "duration_years",
    phase_value = "contraction",
    title = "Episodios de contracción más prolongados (Maddison)",
    x_label = "Duración del episodio, años",
    value_labels = presentation_number_label(accuracy = 0.1),
    n_per_source = 12,
    largest = TRUE,
    selected_source_label = "Maddison (PIB per cápita)"
  )

save_historical_plot(
  "international_contraction_duration_maddison",
  international_contraction_duration_maddison,
  subtitle = "Muestra las contracciones más largas dentro de Maddison."
)

# Graph: Duración de contracciones WDI
international_contraction_duration_wdi <- build_episode_comparison_chart(
    international_episode_comparison,
    metric = "duration_years",
    phase_value = "contraction",
    title = "Episodios de contracción más prolongados (WDI)",
    x_label = "Duración del episodio, años",
    value_labels = presentation_number_label(accuracy = 0.1),
    n_per_source = 12,
    largest = TRUE,
    selected_source_label = "WDI (PIB per cápita)"
  )

save_historical_plot(
  "international_contraction_duration_wdi",
  international_contraction_duration_wdi,
  subtitle = "Muestra las contracciones más largas dentro de WDI per cápita."
)

# Graph: Duración de contracciones FMI WEO
international_contraction_duration_imf_weo <- build_episode_comparison_chart(
    international_episode_comparison,
    metric = "duration_years",
    phase_value = "contraction",
    title = "Episodios de contracción más prolongados (FMI WEO)",
    x_label = "Duración del episodio, años",
    value_labels = presentation_number_label(accuracy = 0.1),
    n_per_source = 12,
    largest = TRUE,
    selected_source_label = "FMI WEO (PIB per cápita)"
  )

save_historical_plot(
  "international_contraction_duration_imf_weo",
  international_contraction_duration_imf_weo,
  subtitle = "Muestra las contracciones más largas dentro de FMI WEO per cápita."
)

# Graph: TCAC de expansiones internacionales
international_expansion_cagr <- build_episode_comparison_chart(
    international_episode_comparison,
    metric = "cagr",
    phase_value = "expansion",
    title = "Expansiones sostenidas con mayor crecimiento compuesto",
    x_label = "TCAC del episodio",
    value_labels = scales::label_percent(accuracy = 1),
    n_per_source = 12,
    largest = TRUE,
    min_duration_years = 5
  )

save_historical_plot(
  "international_expansion_cagr",
  international_expansion_cagr,
  subtitle = "Ordena expansiones de al menos cinco años por crecimiento compuesto anual."
)

# Graph: TCAC de expansiones Maddison
international_expansion_cagr_maddison <- build_episode_comparison_chart(
    international_episode_comparison,
    metric = "cagr",
    phase_value = "expansion",
    title = "Expansiones sostenidas con mayor crecimiento compuesto (Maddison)",
    x_label = "TCAC del episodio",
    value_labels = scales::label_percent(accuracy = 1),
    n_per_source = 12,
    largest = TRUE,
    min_duration_years = 5,
    selected_source_label = "Maddison (PIB per cápita)"
  )

save_historical_plot(
  "international_expansion_cagr_maddison",
  international_expansion_cagr_maddison,
  subtitle = "Muestra las expansiones sostenidas de mayor TCAC en Maddison."
)

# Graph: TCAC de expansiones WDI
international_expansion_cagr_wdi <- build_episode_comparison_chart(
    international_episode_comparison,
    metric = "cagr",
    phase_value = "expansion",
    title = "Expansiones sostenidas con mayor crecimiento compuesto (WDI)",
    x_label = "TCAC del episodio",
    value_labels = scales::label_percent(accuracy = 1),
    n_per_source = 12,
    largest = TRUE,
    min_duration_years = 5,
    selected_source_label = "WDI (PIB per cápita)"
  )

save_historical_plot(
  "international_expansion_cagr_wdi",
  international_expansion_cagr_wdi,
  subtitle = "Muestra las expansiones sostenidas de mayor TCAC en WDI per cápita."
)

# Graph: TCAC de expansiones FMI WEO
international_expansion_cagr_imf_weo <- build_episode_comparison_chart(
    international_episode_comparison,
    metric = "cagr",
    phase_value = "expansion",
    title = "Expansiones sostenidas con mayor crecimiento compuesto (FMI WEO)",
    x_label = "TCAC del episodio",
    value_labels = scales::label_percent(accuracy = 1),
    n_per_source = 12,
    largest = TRUE,
    min_duration_years = 5,
    selected_source_label = "FMI WEO (PIB per cápita)"
  )

save_historical_plot(
  "international_expansion_cagr_imf_weo",
  international_expansion_cagr_imf_weo,
  subtitle = "Muestra las expansiones sostenidas de mayor TCAC en FMI WEO per cápita."
)

## Family: post-disaster recovery benchmarks
# Graph: Años para recuperar después del desastre
international_disaster_recovery_time <- build_disaster_recovery_time_chart(
    international_disaster_recovery
  )

save_historical_plot(
  "international_disaster_recovery_time",
  international_disaster_recovery_time,
  subtitle = "Relaciona profundidad de caída y años necesarios para volver al nivel inicial."
)

# Graph: Severidad y recuperación post-desastre
international_disaster_recovery_heatmap <- build_disaster_recovery_heatmap_chart(
    international_disaster_recovery
  )

save_historical_plot(
  "international_disaster_recovery_heatmap",
  international_disaster_recovery_heatmap,
  subtitle = "Cuenta episodios por severidad de caída y tiempo de recuperación."
)

# Graph: Años para recuperar Maddison
international_disaster_recovery_time_maddison <- build_disaster_recovery_time_chart(
    international_disaster_recovery,
    selected_source_label = "Maddison (PIB per cápita)"
  )

save_historical_plot(
  "international_disaster_recovery_time_maddison",
  international_disaster_recovery_time_maddison,
  subtitle = "Relaciona severidad y recuperación para episodios Maddison."
)

# Graph: Años para recuperar WDI
international_disaster_recovery_time_wdi <- build_disaster_recovery_time_chart(
    international_disaster_recovery,
    selected_source_label = "WDI (PIB per cápita)"
  )

save_historical_plot(
  "international_disaster_recovery_time_wdi",
  international_disaster_recovery_time_wdi,
  subtitle = "Relaciona severidad y recuperación para episodios WDI."
)

# Graph: Años para recuperar FMI WEO
international_disaster_recovery_time_imf_weo <- build_disaster_recovery_time_chart(
    international_disaster_recovery,
    selected_source_label = "FMI WEO (PIB per cápita)"
  )

save_historical_plot(
  "international_disaster_recovery_time_imf_weo",
  international_disaster_recovery_time_imf_weo,
  subtitle = "Relaciona severidad y recuperación para episodios FMI WEO."
)

# Graph: Nivel post-desastre 5 años
international_disaster_horizon_level_05 <- build_single_disaster_horizon_level_chart(
    international_disaster_horizons,
    selected_horizon = 5
  )

save_historical_plot(
  "international_disaster_horizon_level_05",
  international_disaster_horizon_level_05,
  subtitle = "Compara el nivel acumulado cinco años después del final de la caída."
)

# Graph: Nivel post-desastre 10 años
international_disaster_horizon_level_10 <- build_single_disaster_horizon_level_chart(
    international_disaster_horizons,
    selected_horizon = 10
  )

save_historical_plot(
  "international_disaster_horizon_level_10",
  international_disaster_horizon_level_10,
  subtitle = "Compara el nivel acumulado diez años después del final de la caída."
)

# Graph: Nivel post-desastre 15 años
international_disaster_horizon_level_15 <- build_single_disaster_horizon_level_chart(
    international_disaster_horizons,
    selected_horizon = 15
  )

save_historical_plot(
  "international_disaster_horizon_level_15",
  international_disaster_horizon_level_15,
  subtitle = "Compara el nivel acumulado quince años después del final de la caída."
)

# Graph: Nivel post-desastre 20 años
international_disaster_horizon_level_20 <- build_single_disaster_horizon_level_chart(
    international_disaster_horizons,
    selected_horizon = 20
  )

save_historical_plot(
  "international_disaster_horizon_level_20",
  international_disaster_horizon_level_20,
  subtitle = "Compara el nivel acumulado veinte años después del final de la caída."
)

# Graph: Nivel post-desastre 25 años
international_disaster_horizon_level_25 <- build_single_disaster_horizon_level_chart(
    international_disaster_horizons,
    selected_horizon = 25
  )

save_historical_plot(
  "international_disaster_horizon_level_25",
  international_disaster_horizon_level_25,
  subtitle = "Compara el nivel acumulado veinticinco años después del final de la caída."
)

# Graph: Nivel post-desastre 30 años
international_disaster_horizon_level_30 <- build_single_disaster_horizon_level_chart(
    international_disaster_horizons,
    selected_horizon = 30
  )

save_historical_plot(
  "international_disaster_horizon_level_30",
  international_disaster_horizon_level_30,
  subtitle = "Compara el nivel acumulado treinta años después del final de la caída."
)

# Graph: TCAC post-desastre 5 años
# Graph: Distribución de TCAC post-desastre
international_disaster_horizon_cagr_distribution <- build_disaster_horizon_cagr_distribution_chart(
    international_disaster_horizons
  )

save_historical_plot(
  "international_disaster_horizon_cagr_distribution",
  international_disaster_horizon_cagr_distribution,
  subtitle = "Compara la distribución de TCAC posterior a la caída por horizonte de recuperación.",
  note = density_note
)

international_disaster_horizon_cagr_05 <- build_single_disaster_horizon_cagr_chart(
    international_disaster_horizons,
    selected_horizon = 5
  )

save_historical_plot(
  "international_disaster_horizon_cagr_05",
  international_disaster_horizon_cagr_05,
  subtitle = "Mide la TCAC de recuperación durante los cinco años posteriores a la caída."
)

# Graph: TCAC post-desastre 10 años
international_disaster_horizon_cagr_10 <- build_single_disaster_horizon_cagr_chart(
    international_disaster_horizons,
    selected_horizon = 10
  )

save_historical_plot(
  "international_disaster_horizon_cagr_10",
  international_disaster_horizon_cagr_10,
  subtitle = "Mide la TCAC de recuperación durante los diez años posteriores a la caída."
)

# Graph: TCAC post-desastre 15 años
international_disaster_horizon_cagr_15 <- build_single_disaster_horizon_cagr_chart(
    international_disaster_horizons,
    selected_horizon = 15
  )

save_historical_plot(
  "international_disaster_horizon_cagr_15",
  international_disaster_horizon_cagr_15,
  subtitle = "Mide la TCAC de recuperación durante los quince años posteriores a la caída."
)

# Graph: TCAC post-desastre 20 años
international_disaster_horizon_cagr_20 <- build_single_disaster_horizon_cagr_chart(
    international_disaster_horizons,
    selected_horizon = 20
  )

save_historical_plot(
  "international_disaster_horizon_cagr_20",
  international_disaster_horizon_cagr_20,
  subtitle = "Mide la TCAC de recuperación durante los veinte años posteriores a la caída."
)

# Graph: TCAC post-desastre 25 años
international_disaster_horizon_cagr_25 <- build_single_disaster_horizon_cagr_chart(
    international_disaster_horizons,
    selected_horizon = 25
  )

save_historical_plot(
  "international_disaster_horizon_cagr_25",
  international_disaster_horizon_cagr_25,
  subtitle = "Mide la TCAC de recuperación durante los veinticinco años posteriores a la caída."
)

# Graph: TCAC post-desastre 30 años
international_disaster_horizon_cagr_30 <- build_single_disaster_horizon_cagr_chart(
    international_disaster_horizons,
    selected_horizon = 30
  )

save_historical_plot(
  "international_disaster_horizon_cagr_30",
  international_disaster_horizon_cagr_30,
  subtitle = "Mide la TCAC de recuperación durante los treinta años posteriores a la caída."
)

## Family: episode durations
# Graph: Duración de episodios del PIB real
gdp_episode_durations <- build_episode_duration_chart(
    episode_summary,
    "PIB real",
    "Duración de episodios del PIB real"
  )

save_historical_plot(
  "gdp_episode_durations",
  gdp_episode_durations,
  subtitle = "Cuenta cuántas expansiones y contracciones duraron cada cantidad de años."
)

# Graph: Duración de episodios per cápita
gdp_per_capita_episode_durations <- build_episode_duration_chart(
    episode_summary,
    "PIB real per cápita",
    "Duración de episodios del PIB real per cápita"
  )

save_historical_plot(
  "gdp_per_capita_episode_durations",
  gdp_per_capita_episode_durations,
  subtitle = "Cuenta duraciones de expansiones y contracciones en términos per cápita."
)

## Family: recovery scenarios
# Graph: Trayectoria de recuperación
compound_growth_explainer <- build_compound_growth_explainer_chart()

save_historical_plot(
  "compound_growth_explainer",
  compound_growth_explainer,
  subtitle = "Contrasta tasas anuales con el índice acumulado de dos ventanas históricas."
)

# Graph: Recuperación al 15 por ciento
recovery_path_15 <- build_recovery_projection_chart(
    index_series,
    scenario_rates = c(0.15),
    title = "Trayectoria de recuperación con 15% de crecimiento anual"
  )

save_historical_plot(
  "recovery_path_15",
  recovery_path_15,
  subtitle = "Proyecta el nivel desde 2013 con una tasa anual fija de 15%."
)

# Graph: Recuperación del PIB real al 15 por ciento
recovery_path_15_gdp <- build_recovery_projection_chart(
    index_series,
    scenario_rates = c(0.15),
    title = "Trayectoria de recuperación del PIB real al 15%",
    selected_series = series_labels[["gdp"]]
  )

save_historical_plot(
  "recovery_path_15_gdp",
  recovery_path_15_gdp,
  subtitle = "Muestra cuánto tardaría el PIB real en recuperar referencias históricas con 15% anual."
)

# Graph: Recuperación per cápita al 15 por ciento
recovery_path_15_gdp_per_capita <- build_recovery_projection_chart(
    index_series,
    scenario_rates = c(0.15),
    title = "Trayectoria de recuperación per cápita al 15%",
    selected_series = series_labels[["gdp_per_capita"]]
  )

save_historical_plot(
  "recovery_path_15_gdp_per_capita",
  recovery_path_15_gdp_per_capita,
  subtitle = "Muestra cuánto tardaría el PIB real per cápita en recuperar referencias históricas con 15% anual."
)

# Graph: Escenarios de recuperación
recovery_path_scenarios <- build_recovery_projection_chart(
    index_series,
    scenario_rates = c(0.10, 0.07, 0.05, 0.02),
    title = "Trayectorias de recuperación bajo distintos ritmos de crecimiento"
  )

save_historical_plot(
  "recovery_path_scenarios",
  recovery_path_scenarios,
  subtitle = "Compara trayectorias de recuperación bajo ritmos anuales alternativos."
)

# Graph: Escenarios del PIB real
recovery_path_scenarios_gdp <- build_recovery_projection_chart(
    index_series,
    scenario_rates = c(0.10, 0.07, 0.05, 0.02),
    title = "Escenarios de recuperación del PIB real",
    selected_series = series_labels[["gdp"]]
  )

save_historical_plot(
  "recovery_path_scenarios_gdp",
  recovery_path_scenarios_gdp,
  subtitle = "Compara escenarios de recuperación del PIB real con tasas anuales fijas."
)

# Graph: Escenarios per cápita
recovery_path_scenarios_gdp_per_capita <- build_recovery_projection_chart(
    index_series,
    scenario_rates = c(0.10, 0.07, 0.05, 0.02),
    title = "Escenarios de recuperación per cápita",
    selected_series = series_labels[["gdp_per_capita"]]
  )

save_historical_plot(
  "recovery_path_scenarios_gdp_per_capita",
  recovery_path_scenarios_gdp_per_capita,
  subtitle = "Compara escenarios de recuperación per cápita con tasas anuales fijas."
)

# Graph: Años para recuperar el PIB real
recovery_heatmap_gdp <- build_recovery_heatmap_chart(
    index_series,
    selected_series = series_labels[["gdp"]],
    title = "Años para recuperar niveles históricos del PIB real"
  )

save_historical_plot(
  "recovery_heatmap_gdp",
  recovery_heatmap_gdp,
  subtitle = "Calcula años necesarios para recuperar niveles históricos del PIB real por tasa anual."
)

# Graph: Años para recuperar per cápita
recovery_heatmap_gdp_per_capita <- build_recovery_heatmap_chart(
    index_series,
    selected_series = series_labels[["gdp_per_capita"]],
    title = "Años para recuperar niveles históricos del PIB real per cápita"
  )

save_historical_plot(
  "recovery_heatmap_gdp_per_capita",
  recovery_heatmap_gdp_per_capita,
  subtitle = "Calcula años necesarios para recuperar niveles históricos per cápita por tasa anual."
)

## Family: rolling TCAC
# Graph: Ventanas móviles de TCAC
rolling_cagr_example <- build_momentum_example_chart(index_series)

save_historical_plot(
  "rolling_cagr_example",
  rolling_cagr_example,
  subtitle = "Contrasta dos ventanas móviles para mostrar cómo se calcula el crecimiento compuesto."
)

# Graph: TCAC móvil del PIB real y per cápita
rolling_cagr_lines <- build_rolling_cagr_lines_chart(rolling_cagr)

save_historical_plot(
  "rolling_cagr_lines",
  rolling_cagr_lines,
  subtitle = "Traza TCACs móviles para comparar persistencia de crecimiento por ventana temporal."
)

# Graph: TCAC móvil del PIB real
rolling_cagr_lines_gdp <- build_rolling_cagr_lines_chart(
    rolling_cagr,
    selected_series = series_labels[["gdp"]],
    title = "Crecimiento sostenido en ventanas móviles del PIB real"
  )

save_historical_plot(
  "rolling_cagr_lines_gdp",
  rolling_cagr_lines_gdp,
  subtitle = "Traza TCACs móviles del PIB real por longitud de ventana."
)

# Graph: TCAC móvil per cápita
rolling_cagr_lines_gdp_per_capita <- build_rolling_cagr_lines_chart(
    rolling_cagr,
    selected_series = series_labels[["gdp_per_capita"]],
    title = "Crecimiento sostenido en ventanas móviles per cápita"
  )

save_historical_plot(
  "rolling_cagr_lines_gdp_per_capita",
  rolling_cagr_lines_gdp_per_capita,
  subtitle = "Traza TCACs móviles per cápita por longitud de ventana."
)

# Graph: Distribución TCAC
rolling_cagr_distribution <- build_rolling_cagr_distribution_chart(rolling_cagr)

save_historical_plot(
  "rolling_cagr_distribution",
  rolling_cagr_distribution,
  subtitle = "Resume la distribución de TCACs móviles por serie y ventana.",
  note = density_note
)

# Graph: Distribución TCAC PIB real
rolling_cagr_distribution_gdp <- build_rolling_cagr_distribution_chart(
    rolling_cagr,
    selected_series = series_labels[["gdp"]],
    title = "Distribución de crecimiento sostenido del PIB real"
  )

save_historical_plot(
  "rolling_cagr_distribution_gdp",
  rolling_cagr_distribution_gdp,
  subtitle = "Resume la distribución de TCACs móviles del PIB real.",
  note = density_note
)

# Graph: Distribución TCAC per cápita
rolling_cagr_distribution_gdp_per_capita <- build_rolling_cagr_distribution_chart(
    rolling_cagr,
    selected_series = series_labels[["gdp_per_capita"]],
    title = "Distribución de crecimiento sostenido per cápita"
  )

save_historical_plot(
  "rolling_cagr_distribution_gdp_per_capita",
  rolling_cagr_distribution_gdp_per_capita,
  subtitle = "Resume la distribución de TCACs móviles del PIB real per cápita.",
  note = density_note
)

# Graph: Frecuencia TCAC PIB real
rolling_cagr_heatmap_gdp <- build_rolling_cagr_heatmap_chart(
    rolling_cagr,
    selected_series = series_labels[["gdp"]],
    title = "Frecuencia histórica de crecimiento sostenido del PIB real"
  )

save_historical_plot(
  "rolling_cagr_heatmap_gdp",
  rolling_cagr_heatmap_gdp,
  subtitle = "Cuenta la frecuencia de TCACs móviles del PIB real por rango y ventana."
)

# Graph: Frecuencia TCAC per cápita
rolling_cagr_heatmap_gdp_per_capita <- build_rolling_cagr_heatmap_chart(
    rolling_cagr,
    selected_series = series_labels[["gdp_per_capita"]],
    title = "Frecuencia histórica de crecimiento sostenido per cápita"
  )

save_historical_plot(
  "rolling_cagr_heatmap_gdp_per_capita",
  rolling_cagr_heatmap_gdp_per_capita,
  subtitle = "Cuenta la frecuencia de TCACs móviles per cápita por rango y ventana."
)

## Family: simulation and plausibility checks
# Graph: Trayectoria simulada de recuperación
simulation_paths <- ggplot2::ggplot(
    simulation_paths,
    ggplot2::aes(x = year, y = simulated_index_value)
  ) +
    ggplot2::geom_line(color = presentation_colors[["primary"]], linewidth = 0.65) +
    ggplot2::geom_hline(
      data = simulation_summary,
      ggplot2::aes(yintercept = benchmark_index_value),
      color = presentation_colors[["venezuela"]],
      linewidth = 0.45,
      linetype = "dashed"
    ) +
    ggplot2::facet_wrap(ggplot2::vars(series_id), ncol = 1, scales = "free_y") +
    ggplot2::scale_y_continuous(labels = presentation_number_label(accuracy = 0.1), breaks = presentation_breaks_include_limits()) +
    ggplot2::labs(title = "Trayectoria de recuperación con 5% de crecimiento anual", x = NULL, y = "Índice simulado") +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family)

save_historical_plot(
  "simulation_paths",
  simulation_paths,
  subtitle = "Proyecta índices simulados con crecimiento constante de 5% anual."
)

# Graph: Años de recuperación simulada
recovery_years <- ggplot2::ggplot(
    simulation_summary,
    ggplot2::aes(x = series_id, y = years_to_recover)
  ) +
    ggplot2::geom_col(fill = presentation_colors[["primary"]], width = 0.55) +
    ggplot2::labs(title = "Años para recuperar el nivel de 2012 con 5% de crecimiento", x = NULL, y = "Años") +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family)

save_historical_plot(
  "recovery_years",
  recovery_years,
  subtitle = "Compara años requeridos para recuperar el nivel de referencia bajo 5% anual."
)

# Graph: Plausibilidad anual
yearly_plausibility <- ggplot2::ggplot(
    plausibility,
    ggplot2::aes(x = series_id, y = yearly_plausibility)
  ) +
    ggplot2::geom_col(fill = presentation_colors[["primary"]], width = 0.55) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), breaks = presentation_breaks_include_limits(), limits = c(0, 1)) +
    ggplot2::labs(title = "Frecuencia histórica de crecimiento de al menos 5%", x = NULL, y = "Proporción de años") +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family)

save_historical_plot(
  "yearly_plausibility",
  yearly_plausibility,
  subtitle = "Mide qué proporción de años históricos alcanzó al menos 5% de crecimiento."
)

# Graph: Racha maxima de crecimiento
max_growth_streak <- ggplot2::ggplot(
    plausibility,
    ggplot2::aes(x = series_id, y = max_historical_streak)
  ) +
    ggplot2::geom_col(fill = presentation_colors[["primary"]], width = 0.55) +
    ggplot2::labs(title = "Racha histórica más larga de crecimiento de al menos 5%", x = NULL, y = "Años") +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family)

save_historical_plot(
  "max_growth_streak",
  max_growth_streak,
  subtitle = "Muestra la racha histórica más larga con crecimiento anual de al menos 5%."
)

