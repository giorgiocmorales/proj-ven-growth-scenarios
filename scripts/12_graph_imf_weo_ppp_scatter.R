# Build IMF WEO PPP GDP scatterplots for selected cross-sections.

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

figure_dir <- "outputs/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

## Plot constants --------------------------------------------------------------
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
final_year_ratio_label_codes <- c("USA", "PER", "COL", "BRA", "ESP", "CHL", "ECU", "ARG")
ratio_x_limits <- c(1998.5, 2025.5)
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

## Data input ------------------------------------------------------------------
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

## Selected-year plot data ------------------------------------------------------
# Split selected cross-sections into background, LatAm, and Venezuela layers.
selected_years <- c(1999L, 2009L, 2013L, 2018L, 2021L, 2025L)
plot_data <- imf_ppp_scatter_data |>
  dplyr::filter(year %in% selected_years) |>
  dplyr::mutate(
    year = factor(year, levels = selected_years),
    highlight_group = factor(highlight_group, levels = c("Resto del mundo", "LatAm emergente", "Venezuela"))
  )

plot_data_background <- plot_data |>
  dplyr::filter(highlight_group == "Resto del mundo")
plot_data_latam <- plot_data |>
  dplyr::filter(highlight_group == "LatAm emergente")
plot_data_venezuela <- plot_data |>
  dplyr::filter(highlight_group == "Venezuela")

selected_ppp_x_axis <- scatter_gdp_pc_axis
selected_ppp_y_axis <- scatter_gdp_size_axis

## Family: selected-year PPP scatterplots
# Graph: PIB per cápita PPP, años seleccionados
imf_ppp_scatter <- ggplot2::ggplot() +
  ggplot2::geom_point(
    data = plot_data_background,
    ggplot2::aes(
      x = gdp_per_capita_ppp_current_intl_dollars,
      y = gdp_ppp_current_intl_dollars_billions,
      size = population_millions,
      color = highlight_group
    ),
    alpha = 0.45
  ) +
  ggplot2::geom_point(
    data = plot_data_latam,
    ggplot2::aes(
      x = gdp_per_capita_ppp_current_intl_dollars,
      y = gdp_ppp_current_intl_dollars_billions,
      size = population_millions,
      color = highlight_group
    ),
    alpha = 0.75
  ) +
  ggplot2::geom_point(
    data = plot_data_venezuela,
    ggplot2::aes(
      x = gdp_per_capita_ppp_current_intl_dollars,
      y = gdp_ppp_current_intl_dollars_billions,
      size = population_millions,
      color = highlight_group
    ),
    alpha = 0.95
  ) +
  ggplot2::geom_text(
    data = plot_data_venezuela,
    ggplot2::aes(
      x = gdp_per_capita_ppp_current_intl_dollars,
      y = gdp_ppp_current_intl_dollars_billions,
      label = "VEN"
    ),
    color = presentation_colors[["ink"]],
    nudge_y = 0.18,
    size = 3.3,
    fontface = "bold",
    check_overlap = TRUE
  ) +
  ggplot2::facet_wrap(ggplot2::vars(year), ncol = 3) +
  ggplot2::scale_x_log10(
    labels = scales::label_dollar(prefix = "$", accuracy = 1),
    limits = selected_ppp_x_axis$limits,
    breaks = selected_ppp_x_axis$breaks
  ) +
  ggplot2::scale_y_log10(
    labels = scales::label_number(big.mark = ",", accuracy = 1),
    limits = selected_ppp_y_axis$limits,
    breaks = selected_ppp_y_axis$breaks
  ) +
  ggplot2::scale_size_area(max_size = 5.5, labels = scales::label_number(suffix = " M"), name = "Población") +
  ggplot2::scale_color_manual(
    values = c(
      "Resto del mundo" = presentation_colors[["muted"]],
      "LatAm emergente" = presentation_colors[["latam"]],
      "Venezuela" = presentation_colors[["venezuela"]]
    ),
    name = NULL
  ) +
  ggplot2::labs(
    title = "PIB per cápita y tamaño económico en paridad de poder de compra",
    subtitle = "Cada punto ubica a un país por ingreso per cápita y tamaño económico.",
    x = "PIB per cápita PPP, dólares internacionales corrientes",
    y = "PIB total PPP, miles de millones"
  ) +
  ggplot2::theme_minimal(base_size = presentation_small_base_size, base_family = presentation_font_family) +
  ggplot2::theme(legend.position = "bottom")

save_presentation_plot(
  filename = file.path(figure_dir, "imf_weo_ppp_scatter_selected_years.png"),
  plot = imf_ppp_scatter,
  source_caption = presentation_source_caption
)

## Chart helper ----------------------------------------------------------------
# Build one fixed-axis scatterplot for a selected year and GDP measure.
build_single_year_scatter <- function(
    selected_year,
    x_var,
    y_var,
    x_limits,
    y_limits,
    x_breaks,
    y_breaks,
    title_prefix,
    x_label,
    y_label) {
  single_year_data <- imf_ppp_scatter_data |>
    dplyr::filter(year == selected_year) |>
    dplyr::mutate(
      highlight_group = factor(highlight_group, levels = c("Resto del mundo", "LatAm emergente", "Venezuela"))
    )

  single_year_background <- single_year_data |>
    dplyr::filter(highlight_group == "Resto del mundo")
  single_year_latam <- single_year_data |>
    dplyr::filter(highlight_group == "LatAm emergente")
  single_year_venezuela <- single_year_data |>
    dplyr::filter(highlight_group == "Venezuela")

  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = single_year_background,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]],
        color = highlight_group
      ),
      alpha = 0.45,
      size = 1.5
    ) +
    ggplot2::geom_point(
      data = single_year_latam,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]],
        color = highlight_group
      ),
      alpha = 0.8,
      size = 2.2
    ) +
    ggplot2::geom_point(
      data = single_year_venezuela,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]],
        color = highlight_group
      ),
      alpha = 0.95,
      size = 4.5
    ) +
    ggplot2::geom_text(
      data = single_year_data |> dplyr::filter(country_code %in% c("VEN", tracked_latam_codes)),
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]],
        label = country_code
      ),
      color = presentation_colors[["ink"]],
      nudge_y = 0.22,
      size = 3.6,
      fontface = "bold",
      check_overlap = TRUE
    ) +
    ggplot2::scale_x_log10(
      labels = scales::label_dollar(prefix = "$", accuracy = 1),
      limits = x_limits,
      breaks = x_breaks
    ) +
    ggplot2::scale_y_log10(
      labels = scales::label_number(big.mark = ",", accuracy = 1),
      breaks = y_breaks,
      limits = y_limits
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Resto del mundo" = presentation_colors[["muted"]],
        "LatAm emergente" = presentation_colors[["latam"]],
        "Venezuela" = presentation_colors[["venezuela"]]
      ),
      name = NULL
    ) +
    ggplot2::labs(
      title = sprintf("%s (%s)", title_prefix, selected_year),
      subtitle = "Cada punto compara ingreso per cápita y tamaño económico en un mismo año.",
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family)
}

single_years <- c(1999L, 2008L, 2013L, 2018L, 2025L)
single_year_plot_data <- imf_ppp_scatter_data |>
  dplyr::filter(year %in% single_years)

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

## Family: individual-year PPP and nominal scatterplots
for (single_year in single_years) {
  # Graph: PIB per cápita PPP, año individual
  single_year_plot <- build_single_year_scatter(
    single_year,
    x_var = "gdp_per_capita_ppp_current_intl_dollars",
    y_var = "gdp_ppp_current_intl_dollars_billions",
    x_limits = fixed_ppp_x_axis$limits,
    y_limits = fixed_ppp_y_axis$limits,
    x_breaks = fixed_ppp_x_axis$breaks,
    y_breaks = fixed_ppp_y_axis$breaks,
    title_prefix = "PIB per cápita y tamaño económico",
    x_label = "PIB per cápita PPP, dólares internacionales corrientes",
    y_label = "PIB total PPP, miles de millones"
  )
  save_presentation_plot(
    filename = file.path(figure_dir, sprintf("imf_weo_ppp_scatter_%s.png", single_year)),
    plot = single_year_plot,
    source_caption = presentation_source_caption
  )

  # Graph: PIB per cápita nominal, año individual
  single_year_nominal_plot <- build_single_year_scatter(
    single_year,
    x_var = "gdp_per_capita_nominal_current_usd",
    y_var = "gdp_nominal_current_usd_billions",
    x_limits = fixed_nominal_x_axis$limits,
    y_limits = fixed_nominal_y_axis$limits,
    x_breaks = fixed_nominal_x_axis$breaks,
    y_breaks = fixed_nominal_y_axis$breaks,
    title_prefix = "PIB nominal per cápita y tamaño económico",
    x_label = "PIB nominal per cápita, dólares corrientes",
    y_label = "PIB nominal total, miles de millones de dólares"
  )
  save_presentation_plot(
    filename = file.path(figure_dir, sprintf("imf_weo_nominal_scatter_%s.png", single_year)),
    plot = single_year_nominal_plot,
    source_caption = presentation_source_caption
  )
}

## Family: GDP per cápita relative to Venezuela
# Build ratio tables and plots using Venezuela as the yearly baseline.
build_venezuela_ratio_data <- function(data, value_var) {
  venezuela_reference <- data |>
    dplyr::filter(country_code == "VEN") |>
    dplyr::select(year, venezuela_value = dplyr::all_of(value_var))

  data |>
    dplyr::left_join(venezuela_reference, by = "year") |>
    dplyr::filter(!is.na(.data[[value_var]]), !is.na(venezuela_value), venezuela_value > 0) |>
    dplyr::mutate(
      ratio_to_venezuela = .data[[value_var]] / venezuela_value,
      highlight_group = dplyr::case_when(
        country_code == "VEN" ~ "Venezuela",
        country_code %in% emerging_latam_codes ~ "LatAm emergente",
        TRUE ~ "Resto del mundo"
      ),
      highlight_group = factor(highlight_group, levels = c("Resto del mundo", "LatAm emergente", "Venezuela"))
    )
}

build_venezuela_ratio_plot <- function(data, value_var, title, y_label) {
  ratio_data <- build_venezuela_ratio_data(data, value_var)
  ratio_data <- ratio_data |>
    dplyr::filter(year >= 1999, year <= 2025)
  background_data <- ratio_data |>
    dplyr::filter(highlight_group == "Resto del mundo")
  emerging_latam_data <- ratio_data |>
    dplyr::filter(highlight_group == "LatAm emergente")
  venezuela_data <- ratio_data |>
    dplyr::filter(highlight_group == "Venezuela")

  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 1, color = presentation_colors[["ink"]], linewidth = 0.35) +
    ggplot2::geom_jitter(
      data = background_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela, color = highlight_group),
      width = 0.18,
      height = 0,
      alpha = 0.32,
      size = 1
    ) +
    ggplot2::geom_jitter(
      data = emerging_latam_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela, color = highlight_group),
      width = 0.18,
      height = 0,
      alpha = 0.7,
      size = 1.4
    ) +
    ggplot2::geom_point(
      data = venezuela_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela, color = highlight_group),
      size = 1.8,
      alpha = 0.95
    ) +
    ggplot2::scale_y_log10(
      labels = scales::label_number(accuracy = 0.01),
      breaks = ratio_y_breaks,
      limits = ratio_y_limits
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Resto del mundo" = presentation_colors[["muted"]],
        "LatAm emergente" = presentation_colors[["latam"]],
        "Venezuela" = presentation_colors[["venezuela"]]
      ),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(
      breaks = ratio_x_breaks,
      limits = ratio_x_limits,
      expand = ggplot2::expansion(mult = c(0, 0.015))
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Cada punto mide cuantas veces el ingreso per cápita supera al de Venezuela.",
      x = NULL,
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family)
}

build_selected_year_venezuela_ratio_plot <- function(data, value_var, title, y_label) {
  selected_ratio_years <- c(2001L, 2007L, 2013L, 2019L, 2025L)
  ratio_data <- build_venezuela_ratio_data(data, value_var) |>
    dplyr::filter(year %in% selected_ratio_years) |>
    dplyr::mutate(year = factor(year, levels = selected_ratio_years))

  background_data <- ratio_data |>
    dplyr::filter(highlight_group == "Resto del mundo")
  emerging_latam_data <- ratio_data |>
    dplyr::filter(highlight_group == "LatAm emergente")
  venezuela_data <- ratio_data |>
    dplyr::filter(highlight_group == "Venezuela")
  label_data <- ratio_data |>
    dplyr::filter(year == factor(2025L, levels = selected_ratio_years), country_code %in% final_year_ratio_label_codes)

  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 1, color = presentation_colors[["ink"]], linewidth = 0.35) +
    ggplot2::geom_jitter(
      data = background_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela, color = highlight_group),
      width = 0.14,
      height = 0,
      alpha = 0.4,
      size = 1.5
    ) +
    ggplot2::geom_jitter(
      data = emerging_latam_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela, color = highlight_group),
      width = 0.14,
      height = 0,
      alpha = 0.75,
      size = 2
    ) +
    ggplot2::geom_point(
      data = venezuela_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela, color = highlight_group),
      size = 2.4,
      alpha = 0.95
    ) +
    ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela, label = country_code),
      color = presentation_colors[["ink"]],
      nudge_x = -0.26,
      hjust = 1,
      size = 3.7,
      fontface = "bold",
      check_overlap = TRUE
    ) +
    ggplot2::scale_y_log10(
      labels = scales::label_number(accuracy = 0.01),
      breaks = ratio_y_breaks,
      limits = ratio_y_limits
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Resto del mundo" = presentation_colors[["muted"]],
        "LatAm emergente" = presentation_colors[["latam"]],
        "Venezuela" = presentation_colors[["venezuela"]]
      ),
      name = NULL
    ) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(0.8, 0.6))) +
    ggplot2::coord_cartesian(clip = "off", expand = FALSE) +
    ggplot2::labs(
      title = title,
      subtitle = "Los años clave comparan la distancia relativa frente al ingreso per cápita venezolano.",
      x = NULL,
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    ggplot2::theme(plot.margin = ggplot2::margin(5.5, 30, 5.5, 5.5))
}

## Ratio plot construction -----------------------------------------------------
# Graph: PIB per cápita PPP relativo a Venezuela
gdp_pc_ppp_ratio_plot <- build_venezuela_ratio_plot(
  imf_ppp_scatter_data,
  value_var = "gdp_per_capita_ppp_current_intl_dollars",
  title = "PIB per cápita PPP relativo a Venezuela",
  y_label = "PIB per cápita PPP / Venezuela"
)

# Graph: PIB per cápita nominal relativo a Venezuela
gdp_pc_nominal_ratio_plot <- build_venezuela_ratio_plot(
  imf_ppp_scatter_data,
  value_var = "gdp_per_capita_nominal_current_usd",
  title = "PIB nominal per cápita relativo a Venezuela",
  y_label = "PIB nominal per cápita / Venezuela"
)

## Ratio figure outputs --------------------------------------------------------
# Save ratio graphs for full time series and selected years.
save_presentation_plot(
  filename = file.path(figure_dir, "imf_weo_gdp_pc_ppp_ratio_to_venezuela.png"),
  plot = gdp_pc_ppp_ratio_plot,
  source_caption = presentation_source_caption
)
save_presentation_plot(
  filename = file.path(figure_dir, "imf_weo_gdp_pc_nominal_ratio_to_venezuela.png"),
  plot = gdp_pc_nominal_ratio_plot,
  source_caption = presentation_source_caption
)

# Graph: PIB per cápita PPP relativo (años clave)
gdp_pc_ppp_ratio_selected_years_plot <- build_selected_year_venezuela_ratio_plot(
  imf_ppp_scatter_data,
  value_var = "gdp_per_capita_ppp_current_intl_dollars",
  title = "PIB per cápita PPP relativo a Venezuela (años seleccionados)",
  y_label = "PIB per cápita PPP / Venezuela"
)

# Graph: PIB per cápita nominal relativo (años clave)
gdp_pc_nominal_ratio_selected_years_plot <- build_selected_year_venezuela_ratio_plot(
  imf_ppp_scatter_data,
  value_var = "gdp_per_capita_nominal_current_usd",
  title = "PIB nominal per cápita relativo a Venezuela (años seleccionados)",
  y_label = "PIB nominal per cápita / Venezuela"
)

save_presentation_plot(
  filename = file.path(figure_dir, "imf_weo_gdp_pc_ppp_ratio_selected_years_to_venezuela.png"),
  plot = gdp_pc_ppp_ratio_selected_years_plot,
  source_caption = presentation_source_caption
)
save_presentation_plot(
  filename = file.path(figure_dir, "imf_weo_gdp_pc_nominal_ratio_selected_years_to_venezuela.png"),
  plot = gdp_pc_nominal_ratio_selected_years_plot,
  source_caption = presentation_source_caption
)

message("Wrote ", file.path(figure_dir, "imf_weo_ppp_scatter_selected_years.png"))
for (single_year in single_years) {
  message("Wrote ", file.path(figure_dir, sprintf("imf_weo_ppp_scatter_%s.png", single_year)))
  message("Wrote ", file.path(figure_dir, sprintf("imf_weo_nominal_scatter_%s.png", single_year)))
}
message("Wrote ", file.path(figure_dir, "imf_weo_gdp_pc_ppp_ratio_to_venezuela.png"))
message("Wrote ", file.path(figure_dir, "imf_weo_gdp_pc_nominal_ratio_to_venezuela.png"))
message("Wrote ", file.path(figure_dir, "imf_weo_gdp_pc_ppp_ratio_selected_years_to_venezuela.png"))
message("Wrote ", file.path(figure_dir, "imf_weo_gdp_pc_nominal_ratio_selected_years_to_venezuela.png"))
