# Build IMF WEO PPP GDP scatterplots for selected cross-sections.

## Setup ----
# Check packages and load the shared presentation theme.
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

figure_dir <- "outputs/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

## Plot constants ----
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

## Data input ----
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

## Chart helpers ----
# Use the same moderated population sizing as the OWID scatterplots.
add_population_size_index <- function(data) {
  data %>%
    dplyr::mutate(
      population_size_index = dplyr::if_else(
        !is.na(population_millions) & population_millions > 0,
        pmax(population_millions, 1)^0.35,
        NA_real_
      )
    )
}

# Label the same LatAm countries every year so their trajectories are easy to track.
build_scatter_label_data <- function(data, x_var, y_var, x_limits, y_limits) {
  data %>%
    dplyr::filter(
      country_code %in% c("VEN", tracked_latam_codes),
      !is.na(.data[[x_var]]),
      !is.na(.data[[y_var]]),
      .data[[x_var]] >= x_limits[[1]],
      .data[[x_var]] <= x_limits[[2]],
      .data[[y_var]] >= y_limits[[1]],
      .data[[y_var]] <= y_limits[[2]]
    ) %>%
    dplyr::arrange(year, match(country_code, c("VEN", tracked_latam_codes))) %>%
    dplyr::mutate(country_label = country_code)
}

add_scatter_labels <- function(chart, label_data, x_var, y_var) {
  chart +
    ggrepel::geom_label_repel(
      data = label_data,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]],
        label = country_label
      ),
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
    )
}

## Selected-year plot data ----
# Split selected cross-sections into background, LatAm, and Venezuela layers.
selected_years <- c(1999L, 2009L, 2013L, 2018L, 2021L, 2025L)
plot_data <- imf_ppp_scatter_data %>%
  dplyr::filter(year %in% selected_years) %>%
  dplyr::mutate(
    year = factor(year, levels = selected_years),
    highlight_group = factor(highlight_group, levels = c("Resto del mundo", "LatAm emergente", "Venezuela"))
  ) %>%
  add_population_size_index()

plot_data_background <- plot_data %>%
  dplyr::filter(highlight_group == "Resto del mundo")
plot_data_latam <- plot_data %>%
  dplyr::filter(highlight_group == "LatAm emergente")
plot_data_venezuela <- plot_data %>%
  dplyr::filter(highlight_group == "Venezuela")

selected_ppp_x_axis <- scatter_gdp_pc_axis
selected_ppp_y_axis <- scatter_gdp_size_axis
selected_ppp_label_data <- build_scatter_label_data(
  plot_data,
  x_var = "gdp_per_capita_ppp_current_intl_dollars",
  y_var = "gdp_ppp_current_intl_dollars_billions",
  x_limits = selected_ppp_x_axis$limits,
  y_limits = selected_ppp_y_axis$limits
)

## Family: selected-year PPP scatterplots
# Graph: PIB per cápita PPP, años seleccionados
imf_ppp_scatter <- ggplot2::ggplot() +
  # Plot the rest of the world first as a subdued comparison field.
  ggplot2::geom_point(
    data = plot_data_background,
    ggplot2::aes(
      x = gdp_per_capita_ppp_current_intl_dollars,
      y = gdp_ppp_current_intl_dollars_billions,
      size = population_size_index,
      fill = highlight_group
    ),
    shape = 21,
    color = presentation_colors[["ink"]],
    stroke = 0.25,
    alpha = 0.45
  ) +
  # LatAm peers use stronger opacity for regional comparison.
  ggplot2::geom_point(
    data = plot_data_latam,
    ggplot2::aes(
      x = gdp_per_capita_ppp_current_intl_dollars,
      y = gdp_ppp_current_intl_dollars_billions,
      size = population_size_index,
      fill = highlight_group
    ),
    shape = 21,
    color = presentation_colors[["ink"]],
    stroke = 0.28,
    alpha = 0.75
  ) +
  # Venezuela is drawn last to keep it visible in every selected year facet.
  ggplot2::geom_point(
    data = plot_data_venezuela,
    ggplot2::aes(
      x = gdp_per_capita_ppp_current_intl_dollars,
      y = gdp_ppp_current_intl_dollars_billions,
      size = population_size_index,
      fill = highlight_group
    ),
    shape = 21,
    color = presentation_colors[["ink"]],
    stroke = 0.32,
    alpha = 0.95
  ) +
  # Label the same LatAm countries across years for traceable comparison.
  ggrepel::geom_label_repel(
    data = selected_ppp_label_data,
    ggplot2::aes(
      x = gdp_per_capita_ppp_current_intl_dollars,
      y = gdp_ppp_current_intl_dollars_billions,
      label = country_label
    ),
    color = presentation_colors[["ink"]],
    family = presentation_font_family,
    size = presentation_label_small_text_size,
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
  # Keep one facet per selected year to compare relative position over time.
  ggplot2::facet_wrap(ggplot2::vars(year), ncol = 3) +
  # Both axes are logged because GDP per-capita and total GDP span orders of magnitude.
  ggplot2::scale_x_log10(
    labels = presentation_dollar_label(accuracy = 0.1),
    limits = selected_ppp_x_axis$limits,
    breaks = selected_ppp_x_axis$breaks
  ) +
  ggplot2::scale_y_log10(
    labels = presentation_number_label(accuracy = 0.1),
    limits = selected_ppp_y_axis$limits,
    breaks = selected_ppp_y_axis$breaks
  ) +
  # Population size is compressed to preserve visible differences without dwarfing small countries.
  ggplot2::scale_size_continuous(
    range = c(1.3, 5.8),
    breaks = c(1, 10, 100, 1000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  ggplot2::scale_fill_manual(
    values = scatter_highlight_colors,
    labels = scatter_highlight_labels,
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

save_and_preview_plot(
  filename = file.path(figure_dir, "imf_weo_ppp_scatter_selected_years.png"),
  plot = imf_ppp_scatter,
  source_caption = presentation_source_caption,
  note = presentation_axis_note(log_x = TRUE, log_y = TRUE)
)

## Chart helper ----
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
  single_year_data <- imf_ppp_scatter_data %>%
    dplyr::filter(year == selected_year) %>%
    dplyr::mutate(
      highlight_group = factor(highlight_group, levels = c("Resto del mundo", "LatAm emergente", "Venezuela"))
    ) %>%
    add_population_size_index()
  single_year_label_data <- build_scatter_label_data(
    single_year_data,
    x_var = x_var,
    y_var = y_var,
    x_limits = x_limits,
    y_limits = y_limits
  )

  # Split the year into the same three visual layers used by the faceted chart.
  single_year_background <- single_year_data %>%
    dplyr::filter(highlight_group == "Resto del mundo")
  single_year_latam <- single_year_data %>%
    dplyr::filter(highlight_group == "LatAm emergente")
  single_year_venezuela <- single_year_data %>%
    dplyr::filter(highlight_group == "Venezuela")

  ggplot2::ggplot() +
    # Draw global background points first.
    ggplot2::geom_point(
      data = single_year_background,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]],
        fill = highlight_group,
        size = population_size_index
      ),
      shape = 21,
      color = presentation_colors[["ink"]],
      stroke = 0.25,
      alpha = 0.45
    ) +
    # Draw LatAm peers with more contrast.
    ggplot2::geom_point(
      data = single_year_latam,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]],
        fill = highlight_group,
        size = population_size_index
      ),
      shape = 21,
      color = presentation_colors[["ink"]],
      stroke = 0.28,
      alpha = 0.8
    ) +
    # Put Venezuela on top of the point stack.
    ggplot2::geom_point(
      data = single_year_venezuela,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]],
        fill = highlight_group,
        size = population_size_index
      ),
      shape = 21,
      color = presentation_colors[["ink"]],
      stroke = 0.32,
      alpha = 0.95
    ) +
    # Keep labelled countries consistent across the year-specific scatterplots.
    ggrepel::geom_label_repel(
      data = single_year_label_data,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]],
        label = country_label
      ),
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
    # Use fixed log scales so every year is visually comparable.
    ggplot2::scale_x_log10(
      labels = presentation_dollar_label(accuracy = 0.1),
      limits = x_limits,
      breaks = x_breaks
    ) +
    ggplot2::scale_y_log10(
      labels = presentation_number_label(accuracy = 0.1),
      breaks = y_breaks,
      limits = y_limits
    ) +
    # Size legend remains in population units even though dots use transformed values.
    ggplot2::scale_size_continuous(
      range = c(1.8, 9.2),
      breaks = c(1, 10, 100, 1000)^0.35,
      labels = c("1M", "10M", "100M", "1B"),
      name = "Población"
    ) +
    ggplot2::scale_fill_manual(
      values = scatter_highlight_colors,
      labels = scatter_highlight_labels,
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
single_year_plot_data <- imf_ppp_scatter_data %>%
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
  save_and_preview_plot(
    filename = file.path(figure_dir, sprintf("imf_weo_ppp_scatter_%s.png", single_year)),
    plot = single_year_plot,
    source_caption = presentation_source_caption,
    note = presentation_axis_note(log_x = TRUE, log_y = TRUE)
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
  save_and_preview_plot(
    filename = file.path(figure_dir, sprintf("imf_weo_nominal_scatter_%s.png", single_year)),
    plot = single_year_nominal_plot,
    source_caption = presentation_source_caption,
    note = presentation_axis_note(log_x = TRUE, log_y = TRUE)
  )
}

## Family: GDP per cápita relative to Venezuela
# Build ratio tables and plots using Venezuela as the yearly baseline.
build_venezuela_ratio_data <- function(data, value_var) {
  venezuela_reference <- data %>%
    dplyr::filter(country_code == "VEN") %>%
    dplyr::select(year, venezuela_value = dplyr::all_of(value_var))

  data %>%
    dplyr::left_join(venezuela_reference, by = "year") %>%
    dplyr::filter(!is.na(.data[[value_var]]), !is.na(venezuela_value), venezuela_value > 0) %>%
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
  ratio_data <- ratio_data %>%
    dplyr::filter(year >= 1999, year <= 2025)

  # Split groups so the plot can layer background, LatAm peers, and Venezuela.
  background_data <- ratio_data %>%
    dplyr::filter(highlight_group == "Resto del mundo")
  emerging_latam_data <- ratio_data %>%
    dplyr::filter(highlight_group == "LatAm emergente")
  venezuela_data <- ratio_data %>%
    dplyr::filter(highlight_group == "Venezuela")
  label_data <- ratio_data %>%
    dplyr::filter(year %in% c(1999L, 2025L), country_code %in% c("VEN", tracked_latam_codes))

  ggplot2::ggplot() +
    # Ratio of 1 marks parity with Venezuela.
    ggplot2::geom_hline(yintercept = 1, color = presentation_colors[["ink"]], linewidth = 0.35) +
    # Jitter dense country-year clouds just enough to reveal overlapping points.
    ggplot2::geom_jitter(
      data = background_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela, fill = highlight_group),
      width = 0.18,
      height = 0,
      alpha = 0.32,
      size = 1.1,
      shape = 21,
      color = presentation_colors[["ink"]],
      stroke = 0.18
    ) +
    # LatAm peers are darker to make regional comparisons easy to scan.
    ggplot2::geom_jitter(
      data = emerging_latam_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela, fill = highlight_group),
      width = 0.18,
      height = 0,
      alpha = 0.7,
      size = 1.5,
      shape = 21,
      color = presentation_colors[["ink"]],
      stroke = 0.22
    ) +
    # Venezuela appears at parity and anchors the interpretation of the ratio.
    ggplot2::geom_point(
      data = venezuela_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela, fill = highlight_group),
      size = 1.8,
      alpha = 0.95,
      shape = 21,
      color = presentation_colors[["ink"]],
      stroke = 0.25
    ) +
    # Label tracked countries at the first and last selected years.
    ggrepel::geom_label_repel(
      data = label_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela, label = country_code),
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
    # Log scale keeps parity, small gaps, and very large gaps visible together.
    ggplot2::scale_y_log10(
      labels = presentation_number_label(accuracy = 0.01),
      breaks = ratio_y_breaks,
      limits = ratio_y_limits
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Resto del mundo" = presentation_colors[["muted"]],
        "LatAm emergente" = presentation_colors[["latam"]],
        "Venezuela" = presentation_colors[["venezuela"]]
      ),
      labels = scatter_highlight_labels,
      name = NULL
    ) +
    ggplot2::scale_x_continuous(
      breaks = ratio_x_breaks,
      limits = ratio_x_limits,
      expand = ggplot2::expansion(mult = c(0.01, 0.02))
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
  selected_ratio_years <- c(1999L, 2007L, 2013L, 2019L, 2025L)
  ratio_data <- build_venezuela_ratio_data(data, value_var) %>%
    dplyr::filter(year %in% selected_ratio_years) %>%
    dplyr::mutate(year = factor(year, levels = selected_ratio_years))

  background_data <- ratio_data %>%
    dplyr::filter(highlight_group == "Resto del mundo")
  emerging_latam_data <- ratio_data %>%
    dplyr::filter(highlight_group == "LatAm emergente")
  venezuela_data <- ratio_data %>%
    dplyr::filter(highlight_group == "Venezuela")
  label_data <- ratio_data %>%
    dplyr::filter(year %in% factor(c(1999L, 2025L), levels = selected_ratio_years), country_code %in% c("VEN", tracked_latam_codes))

  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 1, color = presentation_colors[["ink"]], linewidth = 0.35) +
    ggplot2::geom_jitter(
      data = background_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela, fill = highlight_group),
      width = 0.14,
      height = 0,
      alpha = 0.4,
      size = 1.5,
      shape = 21,
      color = presentation_colors[["ink"]],
      stroke = 0.18
    ) +
    ggplot2::geom_jitter(
      data = emerging_latam_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela, fill = highlight_group),
      width = 0.14,
      height = 0,
      alpha = 0.75,
      size = 2,
      shape = 21,
      color = presentation_colors[["ink"]],
      stroke = 0.22
    ) +
    ggplot2::geom_point(
      data = venezuela_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela, fill = highlight_group),
      size = 2.4,
      alpha = 0.95,
      shape = 21,
      color = presentation_colors[["ink"]],
      stroke = 0.25
    ) +
    ggrepel::geom_label_repel(
      data = label_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela, label = country_code),
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
    ggplot2::scale_y_log10(
      labels = presentation_number_label(accuracy = 0.01),
      breaks = ratio_y_breaks,
      limits = ratio_y_limits
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Resto del mundo" = presentation_colors[["muted"]],
        "LatAm emergente" = presentation_colors[["latam"]],
        "Venezuela" = presentation_colors[["venezuela"]]
      ),
      labels = scatter_highlight_labels,
      name = NULL
    ) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = c(1.4, 1.4))) +
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

## Ratio plot construction ----
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

## Ratio figure outputs ----
# Save ratio graphs for full time series and selected years.
save_and_preview_plot(
  filename = file.path(figure_dir, "imf_weo_gdp_pc_ppp_ratio_to_venezuela.png"),
  plot = gdp_pc_ppp_ratio_plot,
  source_caption = presentation_source_caption,
  note = presentation_axis_note(log_y = TRUE)
)
save_and_preview_plot(
  filename = file.path(figure_dir, "imf_weo_gdp_pc_nominal_ratio_to_venezuela.png"),
  plot = gdp_pc_nominal_ratio_plot,
  source_caption = presentation_source_caption,
  note = presentation_axis_note(log_y = TRUE)
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

save_and_preview_plot(
  filename = file.path(figure_dir, "imf_weo_gdp_pc_ppp_ratio_selected_years_to_venezuela.png"),
  plot = gdp_pc_ppp_ratio_selected_years_plot,
  source_caption = presentation_source_caption,
  note = presentation_axis_note(log_y = TRUE)
)
save_and_preview_plot(
  filename = file.path(figure_dir, "imf_weo_gdp_pc_nominal_ratio_selected_years_to_venezuela.png"),
  plot = gdp_pc_nominal_ratio_selected_years_plot,
  source_caption = presentation_source_caption,
  note = presentation_axis_note(log_y = TRUE)
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
