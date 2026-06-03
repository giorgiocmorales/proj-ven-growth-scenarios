# Recreate the Wilks-style composite convergence simulation discussed by Cremieux.

## Setup -----------------------------------------------------------------------
# Check packages and load the shared presentation theme.
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

source("scripts/_presentation_theme.R")

figure_dir <- "reports/presentation/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

## Plot constants --------------------------------------------------------------
# Shared caption and simulation parameters.
presentation_source_caption <- build_source_caption(
  "replicacion propia de la simulacion de Cremieux Recueil (2024)",
  calculations = TRUE,
  note = "cada punto es una nueva extraccion de pesos aleatorios de media positiva aplicados a conjuntos disjuntos de items"
)

set.seed(20260530)

items_per_composite <- c(2L, 3L, 5L, 8L, 12L, 20L, 35L, 60L, 100L, 200L)
draws_per_size <- 450L
sample_size <- 200L
target_average_correlation <- 0.50

## Simulation helper -----------------------------------------------------------
# Simulate the correlation between two weighted composites with disjoint items.
simulate_composite_correlation <- function(items_n) {
  factor_loading <- sqrt(target_average_correlation)
  unique_loading <- sqrt(1 - target_average_correlation)

  common_factor <- stats::rnorm(sample_size)
  item_errors <- matrix(stats::rnorm(sample_size * items_n * 2L), nrow = sample_size)
  items <- factor_loading * common_factor + unique_loading * item_errors

  l1_items <- items[, seq_len(items_n), drop = FALSE]
  l2_items <- items[, items_n + seq_len(items_n), drop = FALSE]

  # Positive-mean weights allow occasional weak or negative weights, matching the
  # broad dispersion in the reference simulation.
  l1_weights <- stats::rnorm(items_n, mean = 1, sd = 1)
  l2_weights <- stats::rnorm(items_n, mean = 1, sd = 1)

  l1 <- as.vector(l1_items %*% l1_weights)
  l2 <- as.vector(l2_items %*% l2_weights)
  stats::cor(l1, l2)
}

## Simulation data -------------------------------------------------------------
# Run repeated draws for each composite size.
simulation_data <- do.call(
  rbind,
  lapply(
    items_per_composite,
    function(items_n) {
      data.frame(
        items_per_composite = items_n,
        composite_correlation = replicate(draws_per_size, simulate_composite_correlation(items_n)),
        stringsAsFactors = FALSE
      )
    }
  )
)

## Summary data ----------------------------------------------------------------
# Collapse draws into mean, median, and uncertainty bands for plotting.
summary_data <- simulation_data |>
  dplyr::group_by(items_per_composite) |>
  dplyr::summarise(
    lower = stats::quantile(composite_correlation, 0.05, na.rm = TRUE),
    upper = stats::quantile(composite_correlation, 0.95, na.rm = TRUE),
    mean = mean(composite_correlation, na.rm = TRUE),
    median = stats::median(composite_correlation, na.rm = TRUE),
    .groups = "drop"
  )

## Plot construction -----------------------------------------------------------
# Graph: Por qué el PIB aparece en todas partes
wilks_plot <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    data = summary_data,
    ggplot2::aes(x = items_per_composite, ymin = lower, ymax = upper),
    fill = "grey70",
    alpha = 0.55
  ) +
  ggplot2::geom_point(
    data = simulation_data,
    ggplot2::aes(x = items_per_composite, y = composite_correlation),
    color = presentation_colors[["ink"]],
    alpha = 0.12,
    size = 0.85,
    stroke = 0
  ) +
  ggplot2::geom_line(
    data = summary_data,
    ggplot2::aes(x = items_per_composite, y = median, color = "Mediana"),
    linewidth = 1.05
  ) +
  ggplot2::geom_line(
    data = summary_data,
    ggplot2::aes(x = items_per_composite, y = mean, color = "Media"),
    linewidth = 1.05,
    linetype = "longdash"
  ) +
  ggplot2::scale_x_log10(
    breaks = items_per_composite,
    labels = items_per_composite
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01),
    breaks = c(-0.05, seq(0, 1, by = 0.25), 1.05)
  ) +
  ggplot2::coord_cartesian(ylim = c(-0.05, 1.05), expand = FALSE) +
  ggplot2::scale_color_manual(
    values = c(
      "Mediana" = presentation_colors[["primary"]],
      "Media" = presentation_colors[["negative"]]
    )
  ) +
  ggplot2::labs(
    title = "Convergencia entre indicadores compuestos con ítems disjuntos",
    subtitle = "Dos índices construidos con ítems distintos, pero todos comparten correlación promedio cercana a 0.5",
    x = "Ítems por indicador compuesto, log(n)",
    y = "Corr(L?, L?)",
    color = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank()
  )

## Figure output ---------------------------------------------------------------
# Save the finished simulation graph.
save_presentation_plot(
  filename = file.path(figure_dir, "wilks_composite_correlation.png"),
  plot = wilks_plot,
  source_caption = presentation_source_caption
)

message("Wrote ", file.path(figure_dir, "wilks_composite_correlation.png"))
