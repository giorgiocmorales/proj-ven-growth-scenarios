# Build first-pass ggplot figures for the Quarto presentation.
required_packages <- c("dplyr", "ggplot2", "scales")
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

required_files <- c(
  index_series = "data/final/index_series_long.csv",
  episode_path = "data/final/episode_path.csv",
  episode_summary = "data/final/episode_summary.csv",
  simulation_summary = "data/final/simulation_summary.csv",
  simulation_paths = "data/final/simulation_paths.csv",
  plausibility = "data/final/plausibility_metrics.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    sprintf(
      "Missing required final outputs: %s. Run scripts/05_build_app_data.R first.",
      paste(unname(missing_files), collapse = ", ")
    ),
    call. = FALSE
  )
}

presentation_dir <- "reports/presentation"
figure_dir <- file.path(presentation_dir, "figures")
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

growth_colors <- c(
  positive = "#1f77b4",
  negative = "#d62728"
)

phase_colors <- c(
  expansion = growth_colors[["positive"]],
  contraction = growth_colors[["negative"]]
)

series_order <- c("gdp", "gdp_per_capita")
series_labels <- c(
  gdp = "Real GDP",
  gdp_per_capita = "Real GDP per capita"
)

format_series <- function(series_id) {
  factor(series_id, levels = series_order, labels = series_labels)
}

index_series <- utils::read.csv(required_files[["index_series"]], stringsAsFactors = FALSE) |>
  dplyr::mutate(
    growth_direction = dplyr::if_else(growth_rate >= 0, "positive", "negative"),
    growth_direction = factor(growth_direction, levels = names(growth_colors)),
    series_id = format_series(series_id)
  )

episodes <- utils::read.csv(required_files[["episode_path"]], stringsAsFactors = FALSE) |>
  dplyr::mutate(
    growth_direction = dplyr::if_else(phase == "expansion", "positive", "negative"),
    growth_direction = factor(growth_direction, levels = names(growth_colors)),
    episode_group = paste(series_id, episode_label, sep = "_"),
    series_id = format_series(series_id)
  )

episode_summary <- utils::read.csv(required_files[["episode_summary"]], stringsAsFactors = FALSE) |>
  dplyr::mutate(
    phase = factor(phase, levels = names(phase_colors)),
    series_id = format_series(series_id)
  )

simulation_summary <- utils::read.csv(required_files[["simulation_summary"]], stringsAsFactors = FALSE) |>
  dplyr::mutate(series_id = format_series(series_id))

simulation_paths <- utils::read.csv(required_files[["simulation_paths"]], stringsAsFactors = FALSE) |>
  dplyr::mutate(series_id = format_series(series_id))

plausibility <- utils::read.csv(required_files[["plausibility"]], stringsAsFactors = FALSE) |>
  dplyr::mutate(series_id = format_series(series_id))

build_growth_bar_chart <- function(data, selected_series, title) {
  ggplot2::ggplot(
    data[data$series_id == selected_series, ],
    ggplot2::aes(x = year, y = growth_rate, fill = growth_direction)
  ) +
    ggplot2::geom_col(width = 0.9) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey35") +
    ggplot2::scale_fill_manual(values = growth_colors, guide = "none") +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    ggplot2::labs(title = title, x = NULL, y = "Annual growth rate") +
    ggplot2::theme_minimal(base_size = 12)
}

build_index_line_chart <- function(data, selected_series, title) {
  ggplot2::ggplot(
    data[data$series_id == selected_series, ],
    ggplot2::aes(x = year, y = index_value)
  ) +
    ggplot2::geom_line(color = "#2f2f2f", linewidth = 0.55) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
    ggplot2::labs(title = title, x = NULL, y = "Historical index") +
    ggplot2::theme_minimal(base_size = 12)
}

build_anchor_line_chart <- function(data, selected_series, title) {
  ggplot2::ggplot(
    data[data$series_id == selected_series, ],
    ggplot2::aes(x = year, y = index_vs_anchor_100)
  ) +
    ggplot2::geom_hline(yintercept = 100, linewidth = 0.3, color = "grey35") +
    ggplot2::geom_line(color = "#2f2f2f", linewidth = 0.55) +
    ggplot2::labs(title = title, x = NULL, y = "Index vs. latest year = 100") +
    ggplot2::theme_minimal(base_size = 12)
}

build_episode_line_chart <- function(data, selected_series, title) {
  ggplot2::ggplot(
    data[data$series_id == selected_series, ],
    ggplot2::aes(
      x = year,
      y = episode_index_100,
      group = episode_group,
      color = growth_direction
    )
  ) +
    ggplot2::geom_hline(yintercept = 100, linewidth = 0.3, color = "grey35") +
    ggplot2::geom_line(linewidth = 0.45, alpha = 0.55) +
    ggplot2::scale_color_manual(values = growth_colors, guide = "none") +
    ggplot2::scale_x_continuous(limits = c(1830, 2025), breaks = seq(1850, 2025, 25)) +
    ggplot2::labs(title = title, x = NULL, y = "Episode index, start = 100") +
    ggplot2::theme_minimal(base_size = 12)
}

build_episode_duration_chart <- function(data, selected_series, title) {
  ggplot2::ggplot(
    data[data$series_id == selected_series, ],
    ggplot2::aes(x = duration_years, fill = phase)
  ) +
    ggplot2::geom_bar(width = 0.85) +
    ggplot2::scale_fill_manual(values = phase_colors, name = NULL) +
    ggplot2::labs(title = title, x = "Episode duration, years", y = "Episode count") +
    ggplot2::theme_minimal(base_size = 12)
}

plots <- list(
  gdp_growth_bars = build_growth_bar_chart(
    index_series,
    "Real GDP",
    "Real GDP growth by year"
  ),
  gdp_per_capita_growth_bars = build_growth_bar_chart(
    index_series,
    "Real GDP per capita",
    "Real GDP per capita growth by year"
  ),
  growth_bars_faceted = ggplot2::ggplot(
    index_series,
    ggplot2::aes(x = year, y = growth_rate, fill = growth_direction)
  ) +
    ggplot2::geom_col(width = 0.9) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.3, color = "grey35") +
    ggplot2::facet_wrap(ggplot2::vars(series_id), ncol = 1) +
    ggplot2::scale_fill_manual(values = growth_colors, guide = "none") +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
    ggplot2::labs(title = "Growth by year", x = NULL, y = "Annual growth rate") +
    ggplot2::theme_minimal(base_size = 12),
  gdp_index_line = build_index_line_chart(
    index_series,
    "Real GDP",
    "Real GDP historical index"
  ),
  gdp_per_capita_index_line = build_index_line_chart(
    index_series,
    "Real GDP per capita",
    "Real GDP per capita historical index"
  ),
  index_lines_faceted = ggplot2::ggplot(index_series, ggplot2::aes(x = year, y = index_value)) +
    ggplot2::geom_line(color = "#2f2f2f", linewidth = 0.55) +
    ggplot2::facet_wrap(ggplot2::vars(series_id), ncol = 1, scales = "free_y") +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
    ggplot2::labs(title = "Historical indexes", x = NULL, y = "Historical index") +
    ggplot2::theme_minimal(base_size = 12),
  gdp_anchor_line = build_anchor_line_chart(
    index_series,
    "Real GDP",
    "Real GDP relative to latest year"
  ),
  gdp_per_capita_anchor_line = build_anchor_line_chart(
    index_series,
    "Real GDP per capita",
    "Real GDP per capita relative to latest year"
  ),
  gdp_episode_lines = build_episode_line_chart(
    episodes,
    "Real GDP",
    "Real GDP historical episodes"
  ),
  gdp_per_capita_episode_lines = build_episode_line_chart(
    episodes,
    "Real GDP per capita",
    "Real GDP per capita historical episodes"
  ),
  episode_lines_faceted = ggplot2::ggplot(
    episodes,
    ggplot2::aes(
      x = year,
      y = episode_index_100,
      group = episode_group,
      color = growth_direction
    )
  ) +
    ggplot2::geom_hline(yintercept = 100, linewidth = 0.3, color = "grey35") +
    ggplot2::geom_line(linewidth = 0.45, alpha = 0.55) +
    ggplot2::facet_wrap(ggplot2::vars(series_id), ncol = 1) +
    ggplot2::scale_color_manual(values = growth_colors, guide = "none") +
    ggplot2::scale_x_continuous(limits = c(1830, 2025), breaks = seq(1850, 2025, 25)) +
    ggplot2::labs(title = "Historical episodes", x = NULL, y = "Episode index, start = 100") +
    ggplot2::theme_minimal(base_size = 12),
  gdp_episode_durations = build_episode_duration_chart(
    episode_summary,
    "Real GDP",
    "Real GDP episode durations"
  ),
  gdp_per_capita_episode_durations = build_episode_duration_chart(
    episode_summary,
    "Real GDP per capita",
    "Real GDP per capita episode durations"
  ),
  simulation_paths = ggplot2::ggplot(
    simulation_paths,
    ggplot2::aes(x = year, y = simulated_index_value)
  ) +
    ggplot2::geom_line(color = "#1f77b4", linewidth = 0.65) +
    ggplot2::geom_hline(
      data = simulation_summary,
      ggplot2::aes(yintercept = benchmark_index_value),
      color = "#d62728",
      linewidth = 0.45,
      linetype = "dashed"
    ) +
    ggplot2::facet_wrap(ggplot2::vars(series_id), ncol = 1, scales = "free_y") +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
    ggplot2::labs(title = "Recovery path at 5% annual growth", x = NULL, y = "Simulated index") +
    ggplot2::theme_minimal(base_size = 12),
  recovery_years = ggplot2::ggplot(
    simulation_summary,
    ggplot2::aes(x = series_id, y = years_to_recover)
  ) +
    ggplot2::geom_col(fill = "#1f77b4", width = 0.55) +
    ggplot2::labs(title = "Years to recover 2012 benchmark at 5% growth", x = NULL, y = "Years") +
    ggplot2::theme_minimal(base_size = 12),
  yearly_plausibility = ggplot2::ggplot(
    plausibility,
    ggplot2::aes(x = series_id, y = yearly_plausibility)
  ) +
    ggplot2::geom_col(fill = "#1f77b4", width = 0.55) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, 1)) +
    ggplot2::labs(title = "Historical frequency of at least 5% growth", x = NULL, y = "Share of years") +
    ggplot2::theme_minimal(base_size = 12),
  max_growth_streak = ggplot2::ggplot(
    plausibility,
    ggplot2::aes(x = series_id, y = max_historical_streak)
  ) +
    ggplot2::geom_col(fill = "#1f77b4", width = 0.55) +
    ggplot2::labs(title = "Longest historical streak of at least 5% growth", x = NULL, y = "Years") +
    ggplot2::theme_minimal(base_size = 12)
)

figure_paths <- file.path(figure_dir, paste0(names(plots), ".png"))

for (plot_name in names(plots)) {
  ggplot2::ggsave(
    filename = file.path(figure_dir, paste0(plot_name, ".png")),
    plot = plots[[plot_name]],
    width = 11,
    height = 6.2,
    dpi = 160
  )
}

message("Wrote first-pass presentation figures:")
for (figure_path in figure_paths) {
  message("- ", figure_path)
}
