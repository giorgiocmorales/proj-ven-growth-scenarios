# -*- coding: UTF-8 -*-
# Recreate the Wilks-style composite convergence simulation discussed by Cremieux.

# Setup ----
# Validate and attach the packages required to run this script independently.
required_packages <- c(
  "dplyr",
  "ggplot2",
  "magrittr",
  "purrr",
  "scales",
  "svglite",
  "tidyr"
)
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

for (package_name in required_packages) {
  library(package_name, character.only = TRUE)
}

# Load shared presentation styling and export helpers.
source("scripts/_presentation_theme.R")

figure_dir <- "outputs/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Plot constants ----
# Shared caption and simulation parameters.
presentation_source_caption <- build_source_caption(
  "Replicación propia de Cremieux Recueil (2024)",
  calculations = TRUE,
  note = "cada punto es una nueva extracción de pesos aleatorios de media positiva aplicados a elementos disjuntos"
)

set.seed(20260530)

items_per_composite <- c(2L, 3L, 5L, 8L, 12L, 20L, 35L, 60L, 100L, 200L)
draws_per_size <- 450L
sample_size <- 200L
target_average_correlation <- 0.50

# Graphs ----
# Keep simulation preparation, plotting, formatting, and export in one graph block.
## Family: composite indicator simulation ----

### Graph 01: Por qué el PIB aparece en todas partes ----
# Simulate correlations between weighted composites built from disjoint items.
simulation_data <- expand_grid(
  items_per_composite = items_per_composite,
  draw = seq_len(draws_per_size)
) %>%
  mutate(
    composite_correlation = map_dbl(
      items_per_composite,
      function(items_n) {
        factor_loading <- sqrt(target_average_correlation)
        unique_loading <- sqrt(1 - target_average_correlation)
        common_factor <- stats::rnorm(sample_size)
        item_errors <- matrix(
          stats::rnorm(sample_size * items_n * 2L),
          nrow = sample_size
        )
        items <- factor_loading * common_factor + unique_loading * item_errors
        l1_items <- items[, seq_len(items_n), drop = FALSE]
        l2_items <- items[, items_n + seq_len(items_n), drop = FALSE]

        # Positive-mean weights reproduce the broad reference dispersion.
        l1_weights <- stats::rnorm(items_n, mean = 1, sd = 1)
        l2_weights <- stats::rnorm(items_n, mean = 1, sd = 1)
        l1 <- as.vector(l1_items %*% l1_weights)
        l2 <- as.vector(l2_items %*% l2_weights)

        stats::cor(l1, l2)
      }
    )
  ) %>%
  select(-draw)

# Summarize the simulation draws used for the ribbon and central lines.
summary_data <- simulation_data %>%
  group_by(items_per_composite) %>%
  summarise(
    lower = stats::quantile(composite_correlation, 0.05, na.rm = TRUE),
    upper = stats::quantile(composite_correlation, 0.95, na.rm = TRUE),
    mean = mean(composite_correlation, na.rm = TRUE),
    median = stats::median(composite_correlation, na.rm = TRUE),
    .groups = "drop"
  )

# Spread simulation points horizontally without changing their correlations.
set.seed(20260727)
wilks_point_data <- simulation_data %>%
  mutate(
    items_per_composite_jittered = items_per_composite * exp(
      stats::runif(
        n(),
        min = log(0.965),
        max = log(1.035)
      )
    )
  )

# Build the graph from the simulation draws and their summary statistics.
wilks_plot <- ggplot() +
  geom_ribbon(
    data = summary_data,
    aes(x = items_per_composite, ymin = lower, ymax = upper),
    fill = presentation_colors[["muted"]],
    alpha = 0.55
  ) +
  geom_point(
    data = wilks_point_data,
    aes(x = items_per_composite_jittered, y = composite_correlation),
    color = presentation_colors[["ink"]],
    alpha = 0.16,
    size = 2.2,
    stroke = presentation_point_stroke
  ) +
  geom_line(
    data = summary_data,
    aes(x = items_per_composite, y = median, color = "Mediana"),
    linewidth = 1.05
  ) +
  geom_line(
    data = summary_data,
    aes(x = items_per_composite, y = mean, color = "Media"),
    linewidth = 1.05,
    linetype = "longdash"
  ) +
  scale_x_log10(
    breaks = items_per_composite,
    labels = items_per_composite
  ) +
  scale_y_continuous(
    labels = presentation_number_label(accuracy = 0.01),
    breaks = breaks_width(0.25)
  ) +
  coord_cartesian(
    xlim = c(1.75, 220),
    ylim = c(-0.05, 1.05),
    expand = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Mediana" = presentation_colors[["primary"]],
      "Media" = presentation_colors[["negative"]]
    )
  ) +
  labs(
    title = "Convergencia entre indicadores con componentes disjuntos",
    subtitle = "Cada punto resume la correlación entre dos índices L1 y L2 construidos de forma independiente con componentes distintos.",
    x = "Componentes por indicador compuesto (escala logarítmica)",
    y = "Correlación (L1, L2)",
    color = NULL,
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = TRUE, log_y = FALSE, extra = NULL))
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Apply universal presentation formatting without hiding graph-specific settings.
wilks_plot <- apply_presentation_plot_style(wilks_plot)

# Save the graph and print the same final object for IDE preview.
save_plot_variants(
  filename = file.path(figure_dir, "wilks_composite_correlation.png"),
  plot = wilks_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(wilks_plot)

message("Wrote ", file.path(figure_dir, "wilks_composite_correlation.png"))
