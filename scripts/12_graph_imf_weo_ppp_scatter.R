# -*- coding: UTF-8 -*-
# Build IMF WEO PPP GDP scatterplots for selected cross-sections.

# Setup ----
# Validate and attach the packages required to run this script independently.
required_packages <- c(
  "dplyr",
  "ggplot2",
  "ggrepel",
  "magrittr",
  "scales",
  "svglite"
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

# Load shared presentation styling, scale utilities, and export helpers.
source("scripts/_presentation_theme.R")

figure_dir <- "outputs/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Plot constants ----
# Shared caption, country groups, labels, and axis limits.
presentation_source_caption <- build_source_caption(
  "FMI WEO (2025) y Maddison Project (2023)",
  calculations = TRUE
)

emerging_latam_codes <- c(
  "ATG", "ARG", "ABW", "BHS", "BRB", "BLZ", "BOL", "BRA", "CHL", "COL",
  "CRI", "DMA", "DOM", "ECU", "SLV", "GRD", "GTM", "GUY", "HTI", "HND",
  "JAM", "MEX", "NIC", "PAN", "PRI", "PRY", "PER", "KNA", "LCA", "VCT",
  "SUR", "TTO", "URY", "VEN"
)
tracked_latam_codes <- c("COL", "ECU", "BRA", "ARG", "PER", "BOL", "CHL")
final_year_ratio_label_codes <- c("VEN", tracked_latam_codes, "ESP", "USA")
scatter_highlight_colors <- c(
  "Resto del mundo" = presentation_colors[["muted"]],
  "LatAm emergente" = presentation_colors[["latam"]],
  "Venezuela" = presentation_colors[["venezuela"]]
)
scatter_highlight_labels <- c(
  "Resto del mundo" = "Resto del mundo",
  "LatAm emergente" = "LatAm",
  "Venezuela" = "Venezuela"
)

ratio_x_limits <- c(1997, 2027)
ratio_x_breaks <- c(1999, 2005, 2010, 2015, 2020, 2025)
ratio_y_limits <- c(0.01, 100)
ratio_y_breaks <- c(0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30, 100)
gdp_pc_break_candidates <- c(1, 3, 10, 30, 100, 300, 1000, 3000, 10000, 30000, 100000, 300000)
gdp_size_break_candidates <- c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)
scatter_gdp_pc_axis <- list(
  limits = c(300, 300000),
  breaks = c(300, 1000, 3000, 10000, 30000, 100000, 300000)
)
scatter_gdp_size_axis <- list(
  limits = c(0.1, 100000),
  breaks = c(0.1, 1, 10, 100, 1000, 10000, 100000)
)

# Data input ----
# Use the cached scatterplot table. Run scripts/03_download_imf_weo_ppp_scatter_data.R
# only when the underlying IMF/Maddison data need to be refreshed.
imf_ppp_scatter_path <- "data/final/imf_weo_ppp_scatter_data.csv"
if (!file.exists(imf_ppp_scatter_path)) {
  stop(
    sprintf(
      "Missing `%s`. Run scripts/03_download_imf_weo_ppp_scatter_data.R once before graphing.",
      imf_ppp_scatter_path
    ),
    call. = FALSE
  )
}

imf_ppp_scatter_data <- utils::read.csv(imf_ppp_scatter_path, stringsAsFactors = FALSE)

# Data preparation ----
# Use common limits so individual cross-sections remain directly comparable.

single_years <- c(1999L, 2008L, 2013L, 2018L, 2025L)
single_year_plot_data <- imf_ppp_scatter_data %>%
  filter(year %in% single_years)

# Use common limits so year-by-year scatterplots are comparable.
fixed_x_limits <- range(single_year_plot_data$gdp_per_capita_ppp_current_intl_dollars, na.rm = TRUE)
fixed_y_limits <- range(single_year_plot_data$gdp_ppp_current_intl_dollars_billions, na.rm = TRUE)
fixed_nominal_x_limits <- range(single_year_plot_data$gdp_per_capita_nominal_current_usd, na.rm = TRUE)
fixed_nominal_y_limits <- range(single_year_plot_data$gdp_nominal_current_usd_billions, na.rm = TRUE)
fixed_ppp_x_axis <- scatter_gdp_pc_axis
fixed_ppp_y_axis <- scatter_gdp_size_axis
fixed_nominal_x_axis <- presentation_log10_axis(
  single_year_plot_data$gdp_per_capita_nominal_current_usd,
  gdp_pc_break_candidates
)
fixed_nominal_y_axis <- presentation_log10_axis(
  single_year_plot_data$gdp_nominal_current_usd_billions,
  gdp_size_break_candidates
)
if (10 >= fixed_nominal_x_axis$limits[[1]] && 10 <= fixed_nominal_x_axis$limits[[2]]) {
  fixed_nominal_x_axis$breaks <- sort(unique(c(fixed_nominal_x_axis$breaks, 10)))
}

if (10 >= fixed_nominal_y_axis$limits[[1]] && 10 <= fixed_nominal_y_axis$limits[[2]]) {
  fixed_nominal_y_axis$breaks <- sort(unique(c(fixed_nominal_y_axis$breaks, 10)))
}

# Graphs ----
# Each graph block contains its data preparation, plot definition, formatting, and export.
## Family: individual-year PPP and nominal scatterplots ----

### Graph 01: PIB per cápita PPP, 1999 ----
ppp_1999_data <- imf_ppp_scatter_data %>%
  filter(year == 1999L) %>%
  mutate(
    highlight_group = factor(
      highlight_group,
      levels = c("Resto del mundo", "LatAm emergente", "Venezuela")
    ),
    population_size_index = if_else(
      !is.na(population_millions) & population_millions > 0,
      pmax(population_millions, 1)^0.35,
      NA_real_
    )
  )

ppp_1999_background <- ppp_1999_data %>%
  filter(highlight_group == "Resto del mundo")
ppp_1999_latam <- ppp_1999_data %>%
  filter(highlight_group == "LatAm emergente")
ppp_1999_venezuela <- ppp_1999_data %>%
  filter(highlight_group == "Venezuela")
ppp_1999_labels <- ppp_1999_data %>%
  filter(
    country_code %in% c("VEN", tracked_latam_codes),
    !is.na(gdp_per_capita_ppp_current_intl_dollars),
    !is.na(gdp_ppp_current_intl_dollars_billions),
    gdp_per_capita_ppp_current_intl_dollars >= fixed_ppp_x_axis$limits[[1]],
    gdp_per_capita_ppp_current_intl_dollars <= fixed_ppp_x_axis$limits[[2]],
    gdp_ppp_current_intl_dollars_billions >= fixed_ppp_y_axis$limits[[1]],
    gdp_ppp_current_intl_dollars_billions <= fixed_ppp_y_axis$limits[[2]]
  ) %>%
  arrange(match(country_code, c("VEN", tracked_latam_codes))) %>%
  mutate(country_label = country_code)

ppp_1999_plot <- ggplot() +
  geom_point(
    data = ppp_1999_background,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.45
  ) +
  geom_point(
    data = ppp_1999_latam,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.8
  ) +
  geom_point(
    data = ppp_1999_venezuela,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.95
  ) +
  geom_label_repel(
    data = ppp_1999_labels,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, label = country_label),
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
    seed = 1234
  ) +
  scale_x_log10(
    labels = presentation_number_label(accuracy = 1),
    limits = fixed_ppp_x_axis$limits,
    breaks = fixed_ppp_x_axis$breaks
  ) +
  scale_y_log10(
    labels = presentation_number_label(accuracy = 0.1),
    limits = fixed_ppp_y_axis$limits,
    breaks = fixed_ppp_y_axis$breaks
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1, 10, 100, 1000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  scale_fill_manual(
    values = scatter_highlight_colors,
    labels = scatter_highlight_labels,
    name = NULL
  ) +
  labs(
    title = "Tamaño económico total y per cápita (1999)",
    subtitle = "Cada punto compara ingreso per cápita y tamaño económico en un mismo año.",
    x = "PIBpc PPP",
    y = "PIB PPP (millardos)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(
      log_x = TRUE,
      log_y = TRUE,
      extra = "Valores PPP expresados en dólares internacionales corrientes."
      ))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  )
ppp_1999_plot <- apply_presentation_plot_style(ppp_1999_plot)
save_plot_variants(
  filename = file.path(figure_dir, "imf_weo_ppp_scatter_1999.png"),
  plot = ppp_1999_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(ppp_1999_plot)

### Graph 02: PIB per cápita nominal, 1999 ----
nominal_1999_data <- imf_ppp_scatter_data %>%
  filter(year == 1999L) %>%
  mutate(
    highlight_group = factor(
      highlight_group,
      levels = c("Resto del mundo", "LatAm emergente", "Venezuela")
    ),
    population_size_index = if_else(
      !is.na(population_millions) & population_millions > 0,
      pmax(population_millions, 1)^0.35,
      NA_real_
    )
  )

nominal_1999_background <- nominal_1999_data %>%
  filter(highlight_group == "Resto del mundo")
nominal_1999_latam <- nominal_1999_data %>%
  filter(highlight_group == "LatAm emergente")
nominal_1999_venezuela <- nominal_1999_data %>%
  filter(highlight_group == "Venezuela")
nominal_1999_labels <- nominal_1999_data %>%
  filter(
    country_code %in% c("VEN", tracked_latam_codes),
    !is.na(gdp_per_capita_nominal_current_usd),
    !is.na(gdp_nominal_current_usd_billions),
    gdp_per_capita_nominal_current_usd >= fixed_nominal_x_axis$limits[[1]],
    gdp_per_capita_nominal_current_usd <= fixed_nominal_x_axis$limits[[2]],
    gdp_nominal_current_usd_billions >= fixed_nominal_y_axis$limits[[1]],
    gdp_nominal_current_usd_billions <= fixed_nominal_y_axis$limits[[2]]
  ) %>%
  arrange(match(country_code, c("VEN", tracked_latam_codes))) %>%
  mutate(country_label = country_code)

nominal_1999_plot <- ggplot() +
  geom_point(
    data = nominal_1999_background,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.45
  ) +
  geom_point(
    data = nominal_1999_latam,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.8
  ) +
  geom_point(
    data = nominal_1999_venezuela,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.95
  ) +
  geom_label_repel(
    data = nominal_1999_labels,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, label = country_label),
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
    seed = 1234
  ) +
  scale_x_log10(
    labels = presentation_number_label(accuracy = 1),
    limits = fixed_nominal_x_axis$limits,
    breaks = fixed_nominal_x_axis$breaks
  ) +
  scale_y_log10(
    labels = presentation_number_label(accuracy = 0.1),
    limits = fixed_nominal_y_axis$limits,
    breaks = fixed_nominal_y_axis$breaks
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1, 10, 100, 1000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  scale_fill_manual(
    values = scatter_highlight_colors,
    labels = scatter_highlight_labels,
    name = NULL
  ) +
  labs(
    title = "PIB per cápita y PIB total nominal (1999)",
    subtitle = "Comparación de tamaño económico agregado y por habitante para un año.",
    x = "PIBpc (USD)",
    y = "PIB (USD millardos)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = TRUE, log_y = TRUE, extra = NULL))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  )
nominal_1999_plot <- apply_presentation_plot_style(nominal_1999_plot)
save_plot_variants(
  filename = file.path(figure_dir, "imf_weo_nominal_scatter_1999.png"),
  plot = nominal_1999_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(nominal_1999_plot)

### Graph 03: PIB per cápita PPP, 2008 ----
ppp_2008_data <- imf_ppp_scatter_data %>%
  filter(year == 2008L) %>%
  mutate(
    highlight_group = factor(
      highlight_group,
      levels = c("Resto del mundo", "LatAm emergente", "Venezuela")
    ),
    population_size_index = if_else(
      !is.na(population_millions) & population_millions > 0,
      pmax(population_millions, 1)^0.35,
      NA_real_
    )
  )

ppp_2008_background <- ppp_2008_data %>%
  filter(highlight_group == "Resto del mundo")
ppp_2008_latam <- ppp_2008_data %>%
  filter(highlight_group == "LatAm emergente")
ppp_2008_venezuela <- ppp_2008_data %>%
  filter(highlight_group == "Venezuela")
ppp_2008_labels <- ppp_2008_data %>%
  filter(
    country_code %in% c("VEN", tracked_latam_codes),
    !is.na(gdp_per_capita_ppp_current_intl_dollars),
    !is.na(gdp_ppp_current_intl_dollars_billions),
    gdp_per_capita_ppp_current_intl_dollars >= fixed_ppp_x_axis$limits[[1]],
    gdp_per_capita_ppp_current_intl_dollars <= fixed_ppp_x_axis$limits[[2]],
    gdp_ppp_current_intl_dollars_billions >= fixed_ppp_y_axis$limits[[1]],
    gdp_ppp_current_intl_dollars_billions <= fixed_ppp_y_axis$limits[[2]]
  ) %>%
  arrange(match(country_code, c("VEN", tracked_latam_codes))) %>%
  mutate(country_label = country_code)

ppp_2008_plot <- ggplot() +
  geom_point(
    data = ppp_2008_background,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.45
  ) +
  geom_point(
    data = ppp_2008_latam,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.8
  ) +
  geom_point(
    data = ppp_2008_venezuela,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.95
  ) +
  geom_label_repel(
    data = ppp_2008_labels,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, label = country_label),
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
    seed = 1234
  ) +
  scale_x_log10(
    labels = presentation_number_label(accuracy = 1),
    limits = fixed_ppp_x_axis$limits,
    breaks = fixed_ppp_x_axis$breaks
  ) +
  scale_y_log10(
    labels = presentation_number_label(accuracy = 0.1),
    limits = fixed_ppp_y_axis$limits,
    breaks = fixed_ppp_y_axis$breaks
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1, 10, 100, 1000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  scale_fill_manual(
    values = scatter_highlight_colors,
    labels = scatter_highlight_labels,
    name = NULL
  ) +
  labs(
    title = "PIB per cápita y PIB total PPP (2008)",
    subtitle = "Comparación de tamaño económico agregado y por habitante para un año.",
    x = "PIBpc PPP",
    y = "PIB PPP (millardos)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(
      log_x = TRUE,
      log_y = TRUE,
      extra = "Valores PPP expresados en dólares internacionales corrientes."
      ))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  )
ppp_2008_plot <- apply_presentation_plot_style(ppp_2008_plot)
save_plot_variants(
  filename = file.path(figure_dir, "imf_weo_ppp_scatter_2008.png"),
  plot = ppp_2008_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(ppp_2008_plot)

### Graph 04: PIB per cápita nominal, 2008 ----
nominal_2008_data <- imf_ppp_scatter_data %>%
  filter(year == 2008L) %>%
  mutate(
    highlight_group = factor(
      highlight_group,
      levels = c("Resto del mundo", "LatAm emergente", "Venezuela")
    ),
    population_size_index = if_else(
      !is.na(population_millions) & population_millions > 0,
      pmax(population_millions, 1)^0.35,
      NA_real_
    )
  )

nominal_2008_background <- nominal_2008_data %>%
  filter(highlight_group == "Resto del mundo")
nominal_2008_latam <- nominal_2008_data %>%
  filter(highlight_group == "LatAm emergente")
nominal_2008_venezuela <- nominal_2008_data %>%
  filter(highlight_group == "Venezuela")
nominal_2008_labels <- nominal_2008_data %>%
  filter(
    country_code %in% c("VEN", tracked_latam_codes),
    !is.na(gdp_per_capita_nominal_current_usd),
    !is.na(gdp_nominal_current_usd_billions),
    gdp_per_capita_nominal_current_usd >= fixed_nominal_x_axis$limits[[1]],
    gdp_per_capita_nominal_current_usd <= fixed_nominal_x_axis$limits[[2]],
    gdp_nominal_current_usd_billions >= fixed_nominal_y_axis$limits[[1]],
    gdp_nominal_current_usd_billions <= fixed_nominal_y_axis$limits[[2]]
  ) %>%
  arrange(match(country_code, c("VEN", tracked_latam_codes))) %>%
  mutate(country_label = country_code)

nominal_2008_plot <- ggplot() +
  geom_point(
    data = nominal_2008_background,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.45
  ) +
  geom_point(
    data = nominal_2008_latam,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.8
  ) +
  geom_point(
    data = nominal_2008_venezuela,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.95
  ) +
  geom_label_repel(
    data = nominal_2008_labels,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, label = country_label),
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
    seed = 1234
  ) +
  scale_x_log10(
    labels = presentation_number_label(accuracy = 1),
    limits = fixed_nominal_x_axis$limits,
    breaks = fixed_nominal_x_axis$breaks
  ) +
  scale_y_log10(
    labels = presentation_number_label(accuracy = 0.1),
    limits = fixed_nominal_y_axis$limits,
    breaks = fixed_nominal_y_axis$breaks
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1, 10, 100, 1000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  scale_fill_manual(
    values = scatter_highlight_colors,
    labels = scatter_highlight_labels,
    name = NULL
  ) +
  labs(
    title = "PIB per cápita y PIB total nominal (2008)",
    subtitle = "Comparación de tamaño económico agregado y por habitante para un año.",
    x = "PIBpc (USD)",
    y = "PIB (USD millardos)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = TRUE, log_y = TRUE, extra = NULL))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  )
nominal_2008_plot <- apply_presentation_plot_style(nominal_2008_plot)
save_plot_variants(
  filename = file.path(figure_dir, "imf_weo_nominal_scatter_2008.png"),
  plot = nominal_2008_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(nominal_2008_plot)

### Graph 05: PIB per cápita PPP, 2013 ----
ppp_2013_data <- imf_ppp_scatter_data %>%
  filter(year == 2013L) %>%
  mutate(
    highlight_group = factor(
      highlight_group,
      levels = c("Resto del mundo", "LatAm emergente", "Venezuela")
    ),
    population_size_index = if_else(
      !is.na(population_millions) & population_millions > 0,
      pmax(population_millions, 1)^0.35,
      NA_real_
    )
  )

ppp_2013_background <- ppp_2013_data %>%
  filter(highlight_group == "Resto del mundo")
ppp_2013_latam <- ppp_2013_data %>%
  filter(highlight_group == "LatAm emergente")
ppp_2013_venezuela <- ppp_2013_data %>%
  filter(highlight_group == "Venezuela")
ppp_2013_labels <- ppp_2013_data %>%
  filter(
    country_code %in% c("VEN", tracked_latam_codes),
    !is.na(gdp_per_capita_ppp_current_intl_dollars),
    !is.na(gdp_ppp_current_intl_dollars_billions),
    gdp_per_capita_ppp_current_intl_dollars >= fixed_ppp_x_axis$limits[[1]],
    gdp_per_capita_ppp_current_intl_dollars <= fixed_ppp_x_axis$limits[[2]],
    gdp_ppp_current_intl_dollars_billions >= fixed_ppp_y_axis$limits[[1]],
    gdp_ppp_current_intl_dollars_billions <= fixed_ppp_y_axis$limits[[2]]
  ) %>%
  arrange(match(country_code, c("VEN", tracked_latam_codes))) %>%
  mutate(country_label = country_code)

ppp_2013_plot <- ggplot() +
  geom_point(
    data = ppp_2013_background,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.45
  ) +
  geom_point(
    data = ppp_2013_latam,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.8
  ) +
  geom_point(
    data = ppp_2013_venezuela,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.95
  ) +
  geom_label_repel(
    data = ppp_2013_labels,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, label = country_label),
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
    seed = 1234
  ) +
  scale_x_log10(
    labels = presentation_number_label(accuracy = 1),
    limits = fixed_ppp_x_axis$limits,
    breaks = fixed_ppp_x_axis$breaks
  ) +
  scale_y_log10(
    labels = presentation_number_label(accuracy = 0.1),
    limits = fixed_ppp_y_axis$limits,
    breaks = fixed_ppp_y_axis$breaks
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1, 10, 100, 1000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  scale_fill_manual(
    values = scatter_highlight_colors,
    labels = scatter_highlight_labels,
    name = NULL
  ) +
  labs(
    title = "PIB per cápita y PIB total PPP (2013)",
    subtitle = "Comparación de tamaño económico agregado y por habitante para un año.",
    x = "PIBpc PPP",
    y = "PIB PPP (millardos)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(
      log_x = TRUE,
      log_y = TRUE,
      extra = "Valores PPP expresados en dólares internacionales corrientes."
      ))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  )
ppp_2013_plot <- apply_presentation_plot_style(ppp_2013_plot)
save_plot_variants(
  filename = file.path(figure_dir, "imf_weo_ppp_scatter_2013.png"),
  plot = ppp_2013_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(ppp_2013_plot)

### Graph 06: PIB per cápita nominal, 2013 ----
nominal_2013_data <- imf_ppp_scatter_data %>%
  filter(year == 2013L) %>%
  mutate(
    highlight_group = factor(
      highlight_group,
      levels = c("Resto del mundo", "LatAm emergente", "Venezuela")
    ),
    population_size_index = if_else(
      !is.na(population_millions) & population_millions > 0,
      pmax(population_millions, 1)^0.35,
      NA_real_
    )
  )

nominal_2013_background <- nominal_2013_data %>%
  filter(highlight_group == "Resto del mundo")
nominal_2013_latam <- nominal_2013_data %>%
  filter(highlight_group == "LatAm emergente")
nominal_2013_venezuela <- nominal_2013_data %>%
  filter(highlight_group == "Venezuela")
nominal_2013_labels <- nominal_2013_data %>%
  filter(
    country_code %in% c("VEN", tracked_latam_codes),
    !is.na(gdp_per_capita_nominal_current_usd),
    !is.na(gdp_nominal_current_usd_billions),
    gdp_per_capita_nominal_current_usd >= fixed_nominal_x_axis$limits[[1]],
    gdp_per_capita_nominal_current_usd <= fixed_nominal_x_axis$limits[[2]],
    gdp_nominal_current_usd_billions >= fixed_nominal_y_axis$limits[[1]],
    gdp_nominal_current_usd_billions <= fixed_nominal_y_axis$limits[[2]]
  ) %>%
  arrange(match(country_code, c("VEN", tracked_latam_codes))) %>%
  mutate(country_label = country_code)

nominal_2013_plot <- ggplot() +
  geom_point(
    data = nominal_2013_background,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.45
  ) +
  geom_point(
    data = nominal_2013_latam,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.8
  ) +
  geom_point(
    data = nominal_2013_venezuela,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.95
  ) +
  geom_label_repel(
    data = nominal_2013_labels,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, label = country_label),
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
    seed = 1234
  ) +
  scale_x_log10(
    labels = presentation_number_label(accuracy = 1),
    limits = fixed_nominal_x_axis$limits,
    breaks = fixed_nominal_x_axis$breaks
  ) +
  scale_y_log10(
    labels = presentation_number_label(accuracy = 0.1),
    limits = fixed_nominal_y_axis$limits,
    breaks = fixed_nominal_y_axis$breaks
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1, 10, 100, 1000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  scale_fill_manual(
    values = scatter_highlight_colors,
    labels = scatter_highlight_labels,
    name = NULL
  ) +
  labs(
    title = "PIB per cápita y PIB total nominal (2013)",
    subtitle = "Comparación de tamaño económico agregado y por habitante para un año.",
    x = "PIBpc (USD)",
    y = "PIB (USD millardos)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = TRUE, log_y = TRUE, extra = NULL))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  )
nominal_2013_plot <- apply_presentation_plot_style(nominal_2013_plot)
save_plot_variants(
  filename = file.path(figure_dir, "imf_weo_nominal_scatter_2013.png"),
  plot = nominal_2013_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(nominal_2013_plot)

### Graph 07: PIB per cápita PPP, 2018 ----
ppp_2018_data <- imf_ppp_scatter_data %>%
  filter(year == 2018L) %>%
  mutate(
    highlight_group = factor(
      highlight_group,
      levels = c("Resto del mundo", "LatAm emergente", "Venezuela")
    ),
    population_size_index = if_else(
      !is.na(population_millions) & population_millions > 0,
      pmax(population_millions, 1)^0.35,
      NA_real_
    )
  )

ppp_2018_background <- ppp_2018_data %>%
  filter(highlight_group == "Resto del mundo")
ppp_2018_latam <- ppp_2018_data %>%
  filter(highlight_group == "LatAm emergente")
ppp_2018_venezuela <- ppp_2018_data %>%
  filter(highlight_group == "Venezuela")
ppp_2018_labels <- ppp_2018_data %>%
  filter(
    country_code %in% c("VEN", tracked_latam_codes),
    !is.na(gdp_per_capita_ppp_current_intl_dollars),
    !is.na(gdp_ppp_current_intl_dollars_billions),
    gdp_per_capita_ppp_current_intl_dollars >= fixed_ppp_x_axis$limits[[1]],
    gdp_per_capita_ppp_current_intl_dollars <= fixed_ppp_x_axis$limits[[2]],
    gdp_ppp_current_intl_dollars_billions >= fixed_ppp_y_axis$limits[[1]],
    gdp_ppp_current_intl_dollars_billions <= fixed_ppp_y_axis$limits[[2]]
  ) %>%
  arrange(match(country_code, c("VEN", tracked_latam_codes))) %>%
  mutate(country_label = country_code)

ppp_2018_plot <- ggplot() +
  geom_point(
    data = ppp_2018_background,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.45
  ) +
  geom_point(
    data = ppp_2018_latam,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.8
  ) +
  geom_point(
    data = ppp_2018_venezuela,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.95
  ) +
  geom_label_repel(
    data = ppp_2018_labels,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, label = country_label),
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
    seed = 1234
  ) +
  scale_x_log10(
    labels = presentation_number_label(accuracy = 1),
    limits = fixed_ppp_x_axis$limits,
    breaks = fixed_ppp_x_axis$breaks
  ) +
  scale_y_log10(
    labels = presentation_number_label(accuracy = 0.1),
    limits = fixed_ppp_y_axis$limits,
    breaks = fixed_ppp_y_axis$breaks
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1, 10, 100, 1000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  scale_fill_manual(
    values = scatter_highlight_colors,
    labels = scatter_highlight_labels,
    name = NULL
  ) +
  labs(
    title = "PIB per cápita y PIB total PPP (2018)",
    subtitle = "Comparación de tamaño económico agregado y por habitante para un año.",
    x = "PIBpc PPP",
    y = "PIB PPP (millardos)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(
      log_x = TRUE,
      log_y = TRUE,
      extra = "Valores PPP expresados en dólares internacionales corrientes."
      ))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  )
ppp_2018_plot <- apply_presentation_plot_style(ppp_2018_plot)
save_plot_variants(
  filename = file.path(figure_dir, "imf_weo_ppp_scatter_2018.png"),
  plot = ppp_2018_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(ppp_2018_plot)

### Graph 08: PIB per cápita nominal, 2018 ----
nominal_2018_data <- imf_ppp_scatter_data %>%
  filter(year == 2018L) %>%
  mutate(
    highlight_group = factor(
      highlight_group,
      levels = c("Resto del mundo", "LatAm emergente", "Venezuela")
    ),
    population_size_index = if_else(
      !is.na(population_millions) & population_millions > 0,
      pmax(population_millions, 1)^0.35,
      NA_real_
    )
  )

nominal_2018_background <- nominal_2018_data %>%
  filter(highlight_group == "Resto del mundo")
nominal_2018_latam <- nominal_2018_data %>%
  filter(highlight_group == "LatAm emergente")
nominal_2018_venezuela <- nominal_2018_data %>%
  filter(highlight_group == "Venezuela")
nominal_2018_labels <- nominal_2018_data %>%
  filter(
    country_code %in% c("VEN", tracked_latam_codes),
    !is.na(gdp_per_capita_nominal_current_usd),
    !is.na(gdp_nominal_current_usd_billions),
    gdp_per_capita_nominal_current_usd >= fixed_nominal_x_axis$limits[[1]],
    gdp_per_capita_nominal_current_usd <= fixed_nominal_x_axis$limits[[2]],
    gdp_nominal_current_usd_billions >= fixed_nominal_y_axis$limits[[1]],
    gdp_nominal_current_usd_billions <= fixed_nominal_y_axis$limits[[2]]
  ) %>%
  arrange(match(country_code, c("VEN", tracked_latam_codes))) %>%
  mutate(country_label = country_code)

nominal_2018_plot <- ggplot() +
  geom_point(
    data = nominal_2018_background,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.45
  ) +
  geom_point(
    data = nominal_2018_latam,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.8
  ) +
  geom_point(
    data = nominal_2018_venezuela,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.95
  ) +
  geom_label_repel(
    data = nominal_2018_labels,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, label = country_label),
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
    seed = 1234
  ) +
  scale_x_log10(
    labels = presentation_number_label(accuracy = 1),
    limits = fixed_nominal_x_axis$limits,
    breaks = fixed_nominal_x_axis$breaks
  ) +
  scale_y_log10(
    labels = presentation_number_label(accuracy = 0.1),
    limits = fixed_nominal_y_axis$limits,
    breaks = fixed_nominal_y_axis$breaks
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1, 10, 100, 1000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  scale_fill_manual(
    values = scatter_highlight_colors,
    labels = scatter_highlight_labels,
    name = NULL
  ) +
  labs(
    title = "PIB per cápita y PIB total nominal (2018)",
    subtitle = "Comparación de tamaño económico agregado y por habitante para un año.",
    x = "PIBpc (USD)",
    y = "PIB (USD millardos)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = TRUE, log_y = TRUE, extra = NULL))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  )
nominal_2018_plot <- apply_presentation_plot_style(nominal_2018_plot)
save_plot_variants(
  filename = file.path(figure_dir, "imf_weo_nominal_scatter_2018.png"),
  plot = nominal_2018_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(nominal_2018_plot)

### Graph 09: PIB per cápita PPP, 2025 ----
ppp_2025_data <- imf_ppp_scatter_data %>%
  filter(year == 2025L) %>%
  mutate(
    highlight_group = factor(
      highlight_group,
      levels = c("Resto del mundo", "LatAm emergente", "Venezuela")
    ),
    population_size_index = if_else(
      !is.na(population_millions) & population_millions > 0,
      pmax(population_millions, 1)^0.35,
      NA_real_
    )
  )

ppp_2025_background <- ppp_2025_data %>%
  filter(highlight_group == "Resto del mundo")
ppp_2025_latam <- ppp_2025_data %>%
  filter(highlight_group == "LatAm emergente")
ppp_2025_venezuela <- ppp_2025_data %>%
  filter(highlight_group == "Venezuela")
ppp_2025_labels <- ppp_2025_data %>%
  filter(
    country_code %in% c("VEN", tracked_latam_codes),
    !is.na(gdp_per_capita_ppp_current_intl_dollars),
    !is.na(gdp_ppp_current_intl_dollars_billions),
    gdp_per_capita_ppp_current_intl_dollars >= fixed_ppp_x_axis$limits[[1]],
    gdp_per_capita_ppp_current_intl_dollars <= fixed_ppp_x_axis$limits[[2]],
    gdp_ppp_current_intl_dollars_billions >= fixed_ppp_y_axis$limits[[1]],
    gdp_ppp_current_intl_dollars_billions <= fixed_ppp_y_axis$limits[[2]]
  ) %>%
  arrange(match(country_code, c("VEN", tracked_latam_codes))) %>%
  mutate(country_label = country_code)

ppp_2025_plot <- ggplot() +
  geom_point(
    data = ppp_2025_background,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.45
  ) +
  geom_point(
    data = ppp_2025_latam,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.8
  ) +
  geom_point(
    data = ppp_2025_venezuela,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.95
  ) +
  geom_label_repel(
    data = ppp_2025_labels,
    aes(x = gdp_per_capita_ppp_current_intl_dollars, y = gdp_ppp_current_intl_dollars_billions, label = country_label),
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
    seed = 1234
  ) +
  scale_x_log10(
    labels = presentation_number_label(accuracy = 1),
    limits = fixed_ppp_x_axis$limits,
    breaks = fixed_ppp_x_axis$breaks
  ) +
  scale_y_log10(
    labels = presentation_number_label(accuracy = 0.1),
    limits = fixed_ppp_y_axis$limits,
    breaks = fixed_ppp_y_axis$breaks
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1, 10, 100, 1000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  scale_fill_manual(
    values = scatter_highlight_colors,
    labels = scatter_highlight_labels,
    name = NULL
  ) +
  labs(
    title = "PIB per cápita y PIB total PPP (2025)",
    subtitle = "Cada punto compara ingreso per cápita y tamaño económico en un mismo año.",
    x = "PIBpc PPP",
    y = "PIB PPP (millardos)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(
      log_x = TRUE,
      log_y = TRUE,
      extra = "Valores PPP expresados en dólares internacionales corrientes."
      ))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  )
ppp_2025_plot <- apply_presentation_plot_style(ppp_2025_plot)
save_plot_variants(
  filename = file.path(figure_dir, "imf_weo_ppp_scatter_2025.png"),
  plot = ppp_2025_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(ppp_2025_plot)

### Graph 10: PIB per cápita nominal, 2025 ----
nominal_2025_data <- imf_ppp_scatter_data %>%
  filter(year == 2025L) %>%
  mutate(
    highlight_group = factor(
      highlight_group,
      levels = c("Resto del mundo", "LatAm emergente", "Venezuela")
    ),
    population_size_index = if_else(
      !is.na(population_millions) & population_millions > 0,
      pmax(population_millions, 1)^0.35,
      NA_real_
    )
  )

nominal_2025_background <- nominal_2025_data %>%
  filter(highlight_group == "Resto del mundo")
nominal_2025_latam <- nominal_2025_data %>%
  filter(highlight_group == "LatAm emergente")
nominal_2025_venezuela <- nominal_2025_data %>%
  filter(highlight_group == "Venezuela")
nominal_2025_labels <- nominal_2025_data %>%
  filter(
    country_code %in% c("VEN", tracked_latam_codes),
    !is.na(gdp_per_capita_nominal_current_usd),
    !is.na(gdp_nominal_current_usd_billions),
    gdp_per_capita_nominal_current_usd >= fixed_nominal_x_axis$limits[[1]],
    gdp_per_capita_nominal_current_usd <= fixed_nominal_x_axis$limits[[2]],
    gdp_nominal_current_usd_billions >= fixed_nominal_y_axis$limits[[1]],
    gdp_nominal_current_usd_billions <= fixed_nominal_y_axis$limits[[2]]
  ) %>%
  arrange(match(country_code, c("VEN", tracked_latam_codes))) %>%
  mutate(country_label = country_code)

nominal_2025_plot <- ggplot() +
  geom_point(
    data = nominal_2025_background,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.45
  ) +
  geom_point(
    data = nominal_2025_latam,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.8
  ) +
  geom_point(
    data = nominal_2025_venezuela,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, fill = highlight_group, size = population_size_index),
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke, alpha = 0.95
  ) +
  geom_label_repel(
    data = nominal_2025_labels,
    aes(x = gdp_per_capita_nominal_current_usd, y = gdp_nominal_current_usd_billions, label = country_label),
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
    seed = 1234
  ) +
  scale_x_log10(
    labels = presentation_number_label(accuracy = 1),
    limits = fixed_nominal_x_axis$limits,
    breaks = fixed_nominal_x_axis$breaks
  ) +
  scale_y_log10(
    labels = presentation_number_label(accuracy = 0.1),
    limits = fixed_nominal_y_axis$limits,
    breaks = fixed_nominal_y_axis$breaks
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1, 10, 100, 1000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  scale_fill_manual(
    values = scatter_highlight_colors,
    labels = scatter_highlight_labels,
    name = NULL
  ) +
  labs(
    title = "PIB per cápita y PIB total nominal (2025)",
    subtitle = "Cada punto compara ingreso per cápita y tamaño económico en un mismo año.",
    x = "PIBpc (USD)",
    y = "PIB (USD millardos)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = TRUE, log_y = TRUE, extra = NULL))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  )
nominal_2025_plot <- apply_presentation_plot_style(nominal_2025_plot)
save_plot_variants(
  filename = file.path(figure_dir, "imf_weo_nominal_scatter_2025.png"),
  plot = nominal_2025_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(nominal_2025_plot)

## Family: GDP per cápita relative to Venezuela ----

### Graph 11: PIB per cápita PPP relativo a Venezuela ----
gdp_pc_ppp_ratio_venezuela_reference <- imf_ppp_scatter_data %>%
  filter(country_code == "VEN") %>%
  select(year, venezuela_value = gdp_per_capita_ppp_current_intl_dollars)

gdp_pc_ppp_ratio_data <- imf_ppp_scatter_data %>%
  left_join(gdp_pc_ppp_ratio_venezuela_reference, by = "year") %>%
  filter(year >= 1999L, year <= 2025L, !is.na(gdp_per_capita_ppp_current_intl_dollars), !is.na(venezuela_value), venezuela_value > 0) %>%
  mutate(
    ratio_to_venezuela = gdp_per_capita_ppp_current_intl_dollars / venezuela_value,
    highlight_group = case_when(
      country_code == "VEN" ~ "Venezuela",
      country_code %in% emerging_latam_codes ~ "LatAm emergente",
      TRUE ~ "Resto del mundo"
    ),
    highlight_group = factor(highlight_group, levels = c("Resto del mundo", "LatAm emergente", "Venezuela")),
    ratio_label = year %in% c(1999L, 2025L) & country_code %in% final_year_ratio_label_codes
  )

gdp_pc_ppp_ratio_background <- gdp_pc_ppp_ratio_data %>% filter(highlight_group == "Resto del mundo", !ratio_label)
gdp_pc_ppp_ratio_latam <- gdp_pc_ppp_ratio_data %>% filter(highlight_group == "LatAm emergente", !ratio_label)
gdp_pc_ppp_ratio_venezuela <- gdp_pc_ppp_ratio_data %>% filter(highlight_group == "Venezuela", !ratio_label)
gdp_pc_ppp_ratio_labels <- gdp_pc_ppp_ratio_data %>%
  filter(ratio_label) %>%
  mutate(
    label_nudge_x = if_else(year == 1999L, -0.75, 0.75)
  )
gdp_pc_ppp_ratio_median <- gdp_pc_ppp_ratio_data %>%
  filter(country_code != "VEN") %>%
  group_by(year) %>%
  summarise(ratio_to_venezuela = stats::median(ratio_to_venezuela, na.rm = TRUE), .groups = "drop")

gdp_pc_ppp_ratio_plot <- ggplot() +
  geom_hline(yintercept = 1, color = presentation_colors[["ink"]], linewidth = 0.35) +
  geom_line(
    data = gdp_pc_ppp_ratio_median,
    aes(x = year, y = ratio_to_venezuela, color = "Mediana"),
    linewidth = 1
  ) +
  geom_jitter(
    data = gdp_pc_ppp_ratio_background,
    aes(x = year, y = ratio_to_venezuela, fill = highlight_group),
    width = 0.18, height = 0, alpha = 0.32, size = 1.6,
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke
  ) +
  geom_jitter(
    data = gdp_pc_ppp_ratio_latam,
    aes(x = year, y = ratio_to_venezuela, fill = highlight_group),
    width = 0.18, height = 0, alpha = 0.7, size = 2.1,
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke
  ) +
  geom_point(
    data = gdp_pc_ppp_ratio_venezuela,
    aes(x = year, y = ratio_to_venezuela, fill = highlight_group),
    size = 2.6, alpha = 0.95, shape = 21,
    color = presentation_colors[["ink"]], stroke = presentation_point_stroke
  ) +
  geom_point(
    data = gdp_pc_ppp_ratio_labels,
    aes(x = year, y = ratio_to_venezuela, fill = highlight_group),
    size = 2.6, alpha = 0.95, shape = 21,
    color = presentation_colors[["ink"]], stroke = presentation_point_stroke
  ) +
  geom_label_repel(
    data = gdp_pc_ppp_ratio_labels,
    aes(x = year, y = ratio_to_venezuela, label = country_code),
    color = presentation_colors[["ink"]], family = presentation_font_family,
    size = presentation_label_text_size, fill = "white",
    label.size = presentation_label_box_linewidth, label.padding = presentation_label_padding,
    label.r = presentation_label_radius, min.segment.length = 0,
    segment.color = presentation_colors[["muted"]], segment.size = presentation_label_segment_size,
    box.padding = 0.55, point.padding = 0.45,
    nudge_x = gdp_pc_ppp_ratio_labels$label_nudge_x,
    direction = "y", force = 1.5, max.time = 4,
    max.overlaps = Inf, seed = 1234
  ) +
  scale_y_log10(
    labels = presentation_number_label(accuracy = 0.01),
    breaks = ratio_y_breaks, limits = ratio_y_limits
  ) +
  scale_fill_manual(
    values = scatter_highlight_colors, labels = scatter_highlight_labels, name = NULL
  ) +
  scale_color_manual(
    values = c("Mediana" = presentation_colors[["accent"]]),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = ratio_x_breaks,
    limits = ratio_x_limits,
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    title = "PIB per cápita (PPP) relativo a Venezuela",
    subtitle = "Ratio de producto interno bruto por habitante.",
    x = NULL,
    y = "PIBpc PPP / PIBpc PPP (VEN)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = FALSE, log_y = TRUE, extra = NULL))
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family)

gdp_pc_ppp_ratio_plot <- apply_presentation_plot_style(gdp_pc_ppp_ratio_plot)
save_plot_variants(
  filename = file.path(figure_dir, "imf_weo_gdp_pc_ppp_ratio_to_venezuela.png"),
  plot = gdp_pc_ppp_ratio_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(gdp_pc_ppp_ratio_plot)

### Graph 12: PIB per cápita nominal relativo a Venezuela ----
gdp_pc_nominal_ratio_venezuela_reference <- imf_ppp_scatter_data %>%
  filter(country_code == "VEN") %>%
  select(year, venezuela_value = gdp_per_capita_nominal_current_usd)

gdp_pc_nominal_ratio_data <- imf_ppp_scatter_data %>%
  left_join(gdp_pc_nominal_ratio_venezuela_reference, by = "year") %>%
  filter(year >= 1999L, year <= 2025L, !is.na(gdp_per_capita_nominal_current_usd), !is.na(venezuela_value), venezuela_value > 0) %>%
  mutate(
    ratio_to_venezuela = gdp_per_capita_nominal_current_usd / venezuela_value,
    highlight_group = case_when(
      country_code == "VEN" ~ "Venezuela",
      country_code %in% emerging_latam_codes ~ "LatAm emergente",
      TRUE ~ "Resto del mundo"
    ),
    highlight_group = factor(highlight_group, levels = c("Resto del mundo", "LatAm emergente", "Venezuela")),
    ratio_label = year %in% c(1999L, 2025L) & country_code %in% final_year_ratio_label_codes
  )

gdp_pc_nominal_ratio_background <- gdp_pc_nominal_ratio_data %>% filter(highlight_group == "Resto del mundo", !ratio_label)
gdp_pc_nominal_ratio_latam <- gdp_pc_nominal_ratio_data %>% filter(highlight_group == "LatAm emergente", !ratio_label)
gdp_pc_nominal_ratio_venezuela <- gdp_pc_nominal_ratio_data %>% filter(highlight_group == "Venezuela", !ratio_label)
gdp_pc_nominal_ratio_labels <- gdp_pc_nominal_ratio_data %>%
  filter(ratio_label) %>%
  mutate(
    label_nudge_x = if_else(year == 1999L, -0.75, 0.75)
  )
gdp_pc_nominal_ratio_median <- gdp_pc_nominal_ratio_data %>%
  filter(country_code != "VEN") %>%
  group_by(year) %>%
  summarise(ratio_to_venezuela = stats::median(ratio_to_venezuela, na.rm = TRUE), .groups = "drop")

gdp_pc_nominal_ratio_plot <- ggplot() +
  geom_hline(yintercept = 1, color = presentation_colors[["ink"]], linewidth = 0.35) +
  geom_line(
    data = gdp_pc_nominal_ratio_median,
    aes(x = year, y = ratio_to_venezuela, color = "Mediana"),
    linewidth = 1
  ) +
  geom_jitter(
    data = gdp_pc_nominal_ratio_background,
    aes(x = year, y = ratio_to_venezuela, fill = highlight_group),
    width = 0.18, height = 0, alpha = 0.32, size = 1.6,
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke
  ) +
  geom_jitter(
    data = gdp_pc_nominal_ratio_latam,
    aes(x = year, y = ratio_to_venezuela, fill = highlight_group),
    width = 0.18, height = 0, alpha = 0.7, size = 2.1,
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke
  ) +
  geom_point(
    data = gdp_pc_nominal_ratio_venezuela,
    aes(x = year, y = ratio_to_venezuela, fill = highlight_group),
    size = 2.6, alpha = 0.95, shape = 21,
    color = presentation_colors[["ink"]], stroke = presentation_point_stroke
  ) +
  geom_point(
    data = gdp_pc_nominal_ratio_labels,
    aes(x = year, y = ratio_to_venezuela, fill = highlight_group),
    size = 2.6, alpha = 0.95, shape = 21,
    color = presentation_colors[["ink"]], stroke = presentation_point_stroke
  ) +
  geom_label_repel(
    data = gdp_pc_nominal_ratio_labels,
    aes(x = year, y = ratio_to_venezuela, label = country_code),
    color = presentation_colors[["ink"]], family = presentation_font_family,
    size = presentation_label_text_size, fill = "white",
    label.size = presentation_label_box_linewidth, label.padding = presentation_label_padding,
    label.r = presentation_label_radius, min.segment.length = 0,
    segment.color = presentation_colors[["muted"]], segment.size = presentation_label_segment_size,
    box.padding = 0.55, point.padding = 0.45,
    nudge_x = gdp_pc_nominal_ratio_labels$label_nudge_x,
    direction = "y", force = 1.5, max.time = 4,
    max.overlaps = Inf, seed = 1234
  ) +
  scale_y_log10(
    labels = presentation_number_label(accuracy = 0.01),
    breaks = ratio_y_breaks, limits = ratio_y_limits
  ) +
  scale_fill_manual(
    values = scatter_highlight_colors, labels = scatter_highlight_labels, name = NULL
  ) +
  scale_color_manual(
    values = c("Mediana" = presentation_colors[["accent"]]),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = ratio_x_breaks,
    limits = ratio_x_limits,
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    title = "PIB per cápita nominal relativo a Venezuela",
    subtitle = "Ratio de producto interno bruto por habitante.",
    x = NULL,
    y = "PIBpc / PIBpc (VEN)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = FALSE, log_y = TRUE, extra = NULL))
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family)

gdp_pc_nominal_ratio_plot <- apply_presentation_plot_style(gdp_pc_nominal_ratio_plot)
save_plot_variants(
  filename = file.path(figure_dir, "imf_weo_gdp_pc_nominal_ratio_to_venezuela.png"),
  plot = gdp_pc_nominal_ratio_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(gdp_pc_nominal_ratio_plot)

### Graph 13: PIB per cápita PPP relativo (años clave) ----
gdp_pc_ppp_ratio_selected_years_venezuela_reference <- imf_ppp_scatter_data %>%
  filter(country_code == "VEN") %>%
  select(year, venezuela_value = gdp_per_capita_ppp_current_intl_dollars)

selected_ratio_years <- c(1999L, 2007L, 2013L, 2019L, 2025L)

gdp_pc_ppp_ratio_selected_years_data <- imf_ppp_scatter_data %>%
  left_join(gdp_pc_ppp_ratio_selected_years_venezuela_reference, by = "year") %>%
  filter(year %in% selected_ratio_years, !is.na(gdp_per_capita_ppp_current_intl_dollars), !is.na(venezuela_value), venezuela_value > 0) %>%
  mutate(
    year = factor(year, levels = selected_ratio_years),
    year_position = as.numeric(year),
    ratio_to_venezuela = gdp_per_capita_ppp_current_intl_dollars / venezuela_value,
    highlight_group = case_when(
      country_code == "VEN" ~ "Venezuela",
      country_code %in% emerging_latam_codes ~ "LatAm emergente",
      TRUE ~ "Resto del mundo"
    ),
    highlight_group = factor(highlight_group, levels = c("Resto del mundo", "LatAm emergente", "Venezuela")),
    ratio_label = as.integer(as.character(year)) %in% c(1999L, 2025L) &
      country_code %in% final_year_ratio_label_codes
  )

gdp_pc_ppp_ratio_selected_years_background <- gdp_pc_ppp_ratio_selected_years_data %>%
  filter(highlight_group == "Resto del mundo", !ratio_label)

gdp_pc_ppp_ratio_selected_years_latam <- gdp_pc_ppp_ratio_selected_years_data %>%
  filter(highlight_group == "LatAm emergente", !ratio_label)

gdp_pc_ppp_ratio_selected_years_venezuela <- gdp_pc_ppp_ratio_selected_years_data %>%
  filter(highlight_group == "Venezuela", !ratio_label)

gdp_pc_ppp_ratio_selected_years_median <- gdp_pc_ppp_ratio_selected_years_data %>%
  filter(country_code != "VEN") %>%
  group_by(year_position) %>%
  summarise(
    median_ratio = stats::median(ratio_to_venezuela, na.rm = TRUE),
    .groups = "drop"
  )

gdp_pc_ppp_ratio_selected_years_labels <- gdp_pc_ppp_ratio_selected_years_data %>%
  filter(ratio_label) %>%
  mutate(
    label_nudge_x = if_else(year_position == 1, -0.42, 0.42)
  )

gdp_pc_ppp_ratio_selected_years_plot <- ggplot() +
  geom_hline(yintercept = 1, color = presentation_colors[["ink"]], linewidth = 0.35) +
  geom_segment(
    data = gdp_pc_ppp_ratio_selected_years_median,
    aes(
      x = year_position - 0.24,
      xend = year_position + 0.24,
      y = median_ratio,
      yend = median_ratio,
      color = "Mediana de la muestra"
    ),
    linewidth = 1.1
  ) +
  geom_jitter(
    data = gdp_pc_ppp_ratio_selected_years_background,
    aes(x = year_position, y = ratio_to_venezuela, fill = highlight_group),
    width = 0.16, height = 0, alpha = 0.4, size = 2.3,
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke
  ) +
  geom_jitter(
    data = gdp_pc_ppp_ratio_selected_years_latam,
    aes(x = year_position, y = ratio_to_venezuela, fill = highlight_group),
    width = 0.16, height = 0, alpha = 0.75, size = 3,
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke
  ) +
  geom_point(
    data = gdp_pc_ppp_ratio_selected_years_venezuela,
    aes(x = year_position, y = ratio_to_venezuela, fill = highlight_group),
    size = 3.5, alpha = 0.95, shape = 21,
    color = presentation_colors[["ink"]], stroke = presentation_point_stroke
  ) +
  geom_point(
    data = gdp_pc_ppp_ratio_selected_years_labels,
    aes(x = year_position, y = ratio_to_venezuela, fill = highlight_group),
    size = 3.5, alpha = 0.95, shape = 21,
    color = presentation_colors[["ink"]], stroke = presentation_point_stroke
  ) +
  geom_label_repel(
    data = gdp_pc_ppp_ratio_selected_years_labels,
    aes(x = year_position, y = ratio_to_venezuela, label = country_code),
    color = presentation_colors[["ink"]], family = presentation_font_family,
    size = presentation_label_text_size, fill = "white",
    label.size = presentation_label_box_linewidth, label.padding = presentation_label_padding,
    label.r = presentation_label_radius, min.segment.length = 0,
    segment.color = presentation_colors[["muted"]], segment.size = presentation_label_segment_size,
    box.padding = 0.60, point.padding = 0.50,
    nudge_x = gdp_pc_ppp_ratio_selected_years_labels$label_nudge_x,
    direction = "y", force = 1.8, max.time = 4,
    max.overlaps = Inf, seed = 1234
  ) +
  scale_y_log10(
    labels = presentation_number_label(accuracy = 0.01),
    breaks = ratio_y_breaks, limits = ratio_y_limits
  ) +
  scale_fill_manual(
    values = scatter_highlight_colors, labels = scatter_highlight_labels, name = NULL
  ) +
  scale_color_manual(
    values = c("Mediana de la muestra" = presentation_colors[["accent"]]),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = seq_along(selected_ratio_years),
    labels = selected_ratio_years,
    limits = c(0.25, 5.75),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(clip = "off", expand = FALSE) +
  labs(
    title = "PIB per cápita (PPP) relativo a Venezuela",
    subtitle = "Ratio de producto interno bruto por habitante para años selectos.",
    x = NULL,
    y = "PIBpc PPP / PIBpc PPP (VEN)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = FALSE, log_y = TRUE, extra = NULL))
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(plot.margin = margin(5.5, 30, 5.5, 5.5))

gdp_pc_ppp_ratio_selected_years_plot <- apply_presentation_plot_style(gdp_pc_ppp_ratio_selected_years_plot)
save_plot_variants(
  filename = file.path(figure_dir, "imf_weo_gdp_pc_ppp_ratio_selected_years_to_venezuela.png"),
  plot = gdp_pc_ppp_ratio_selected_years_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(gdp_pc_ppp_ratio_selected_years_plot)

### Graph 14: PIB per cápita nominal relativo (años clave) ----
gdp_pc_nominal_ratio_selected_years_venezuela_reference <- imf_ppp_scatter_data %>%
  filter(country_code == "VEN") %>%
  select(year, venezuela_value = gdp_per_capita_nominal_current_usd)

selected_ratio_years <- c(1999L, 2007L, 2013L, 2019L, 2025L)
gdp_pc_nominal_ratio_selected_years_data <- imf_ppp_scatter_data %>%
  left_join(gdp_pc_nominal_ratio_selected_years_venezuela_reference, by = "year") %>%
  filter(year %in% selected_ratio_years, !is.na(gdp_per_capita_nominal_current_usd), !is.na(venezuela_value), venezuela_value > 0) %>%
  mutate(
    year = factor(year, levels = selected_ratio_years),
    year_position = as.numeric(year),
    ratio_to_venezuela = gdp_per_capita_nominal_current_usd / venezuela_value,
    highlight_group = case_when(
      country_code == "VEN" ~ "Venezuela",
      country_code %in% emerging_latam_codes ~ "LatAm emergente",
      TRUE ~ "Resto del mundo"
    ),
    highlight_group = factor(highlight_group, levels = c("Resto del mundo", "LatAm emergente", "Venezuela")),
    ratio_label = as.integer(as.character(year)) %in% c(1999L, 2025L) &
      country_code %in% final_year_ratio_label_codes
  )

gdp_pc_nominal_ratio_selected_years_background <- gdp_pc_nominal_ratio_selected_years_data %>%
  filter(highlight_group == "Resto del mundo", !ratio_label)

gdp_pc_nominal_ratio_selected_years_latam <- gdp_pc_nominal_ratio_selected_years_data %>%
  filter(highlight_group == "LatAm emergente", !ratio_label)

gdp_pc_nominal_ratio_selected_years_venezuela <- gdp_pc_nominal_ratio_selected_years_data %>%
  filter(highlight_group == "Venezuela", !ratio_label)

gdp_pc_nominal_ratio_selected_years_median <- gdp_pc_nominal_ratio_selected_years_data %>%
  filter(country_code != "VEN") %>%
  group_by(year_position) %>%
  summarise(
    median_ratio = stats::median(ratio_to_venezuela, na.rm = TRUE),
    .groups = "drop"
  )

gdp_pc_nominal_ratio_selected_years_labels <- gdp_pc_nominal_ratio_selected_years_data %>%
  filter(ratio_label) %>%
  mutate(
    label_nudge_x = if_else(year_position == 1, -0.42, 0.42)
  )

gdp_pc_nominal_ratio_selected_years_plot <- ggplot() +
  geom_hline(yintercept = 1, color = presentation_colors[["ink"]], linewidth = 0.35) +
  geom_segment(
    data = gdp_pc_nominal_ratio_selected_years_median,
    aes(
      x = year_position - 0.24,
      xend = year_position + 0.24,
      y = median_ratio,
      yend = median_ratio,
      color = "Mediana de la muestra"
    ),
    linewidth = 1.1
  ) +
  geom_jitter(
    data = gdp_pc_nominal_ratio_selected_years_background,
    aes(x = year_position, y = ratio_to_venezuela, fill = highlight_group),
    width = 0.16, height = 0, alpha = 0.4, size = 2.3,
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke
  ) +
  geom_jitter(
    data = gdp_pc_nominal_ratio_selected_years_latam,
    aes(x = year_position, y = ratio_to_venezuela, fill = highlight_group),
    width = 0.16, height = 0, alpha = 0.75, size = 3,
    shape = 21, color = presentation_colors[["ink"]], stroke = presentation_point_stroke
  ) +
  geom_point(
    data = gdp_pc_nominal_ratio_selected_years_venezuela,
    aes(x = year_position, y = ratio_to_venezuela, fill = highlight_group),
    size = 3.5, alpha = 0.95, shape = 21,
    color = presentation_colors[["ink"]], stroke = presentation_point_stroke
  ) +
  geom_point(
    data = gdp_pc_nominal_ratio_selected_years_labels,
    aes(x = year_position, y = ratio_to_venezuela, fill = highlight_group),
    size = 3.5, alpha = 0.95, shape = 21,
    color = presentation_colors[["ink"]], stroke = presentation_point_stroke
  ) +
  geom_label_repel(
    data = gdp_pc_nominal_ratio_selected_years_labels,
    aes(x = year_position, y = ratio_to_venezuela, label = country_code),
    color = presentation_colors[["ink"]], family = presentation_font_family,
    size = presentation_label_text_size, fill = "white",
    label.size = presentation_label_box_linewidth, label.padding = presentation_label_padding,
    label.r = presentation_label_radius, min.segment.length = 0,
    segment.color = presentation_colors[["muted"]], segment.size = presentation_label_segment_size,
    box.padding = 0.60, point.padding = 0.50,
    nudge_x = gdp_pc_nominal_ratio_selected_years_labels$label_nudge_x,
    direction = "y", force = 1.8, max.time = 4,
    max.overlaps = Inf, seed = 1234
  ) +
  scale_y_log10(
    labels = presentation_number_label(accuracy = 0.01),
    breaks = ratio_y_breaks, limits = ratio_y_limits
  ) +
  scale_fill_manual(
    values = scatter_highlight_colors, labels = scatter_highlight_labels, name = NULL
  ) +
  scale_color_manual(
    values = c("Mediana de la muestra" = presentation_colors[["accent"]]),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = seq_along(selected_ratio_years),
    labels = selected_ratio_years,
    limits = c(0.25, 5.75),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(clip = "off", expand = FALSE) +
  labs(
    title = "PIB per cápita nominal relativo a Venezuela",
    subtitle = "Ratio de producto interno bruto por habitante para años selectos.",
    x = NULL,
    y = "PIBpc / PIBpc (VEN)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = FALSE, log_y = TRUE, extra = NULL))
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(plot.margin = margin(5.5, 30, 5.5, 5.5))

gdp_pc_nominal_ratio_selected_years_plot <- apply_presentation_plot_style(gdp_pc_nominal_ratio_selected_years_plot)
save_plot_variants(
  filename = file.path(figure_dir, "imf_weo_gdp_pc_nominal_ratio_selected_years_to_venezuela.png"),
  plot = gdp_pc_nominal_ratio_selected_years_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(gdp_pc_nominal_ratio_selected_years_plot)
