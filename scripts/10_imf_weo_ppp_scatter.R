# Build IMF WEO PPP GDP scatterplots for selected cross-sections.
required_packages <- c("dplyr", "ggplot2", "jsonlite", "readxl", "scales")
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

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
figure_dir <- "reports/presentation/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

maddison_data_path <- "data/raw/mpd2023_web.xlsx"
if (!file.exists(maddison_data_path)) {
  stop(sprintf("Maddison workbook not found at `%s`.", maddison_data_path), call. = FALSE)
}

maddison_countries <- readxl::read_excel(maddison_data_path, sheet = "Full data") |>
  dplyr::transmute(
    country_code = as.character(countrycode),
    country = as.character(country),
    maddison_region = as.character(region)
  ) |>
  dplyr::filter(!is.na(country_code), nchar(country_code) == 3) |>
  dplyr::distinct(country_code, .keep_all = TRUE)

emerging_latam_codes <- c(
  "ATG", "ARG", "ABW", "BHS", "BRB", "BLZ", "BOL", "BRA", "CHL", "COL",
  "CRI", "DMA", "DOM", "ECU", "SLV", "GRD", "GTM", "GUY", "HTI", "HND",
  "JAM", "MEX", "NIC", "PAN", "PRI", "PRY", "PER", "KNA", "LCA", "VCT",
  "SUR", "TTO", "URY", "VEN"
)
tracked_latam_codes <- c("COL", "ECU", "BRA", "ARG", "PER", "BOL", "CHL")
final_year_ratio_label_codes <- c("USA", "PER", "COL", "BRA", "ESP", "CHL", "ECU", "ARG")

read_imf_datamapper_indicator <- function(indicator_id, indicator_name) {
  imf_url <- sprintf("https://www.imf.org/external/datamapper/api/v1/%s", indicator_id)
  imf_json <- jsonlite::fromJSON(imf_url, simplifyVector = FALSE)

  if (is.null(imf_json$values[[indicator_id]])) {
    stop(sprintf("IMF DataMapper response did not include `%s`.", indicator_id), call. = FALSE)
  }

  imf_series <- imf_json$values[[indicator_id]]
  target_codes <- intersect(maddison_countries$country_code, names(imf_series))

  indicator_parts <- lapply(target_codes, function(country_code) {
    country_series <- imf_series[[country_code]]
    years <- names(country_series)

    data.frame(
      country_code = country_code,
      indicator_id = indicator_id,
      indicator = indicator_name,
      year = as.integer(years),
      value = as.numeric(unlist(country_series, use.names = FALSE)),
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(indicator_parts) |>
    dplyr::filter(!is.na(year), !is.na(value))
}

imf_ppp_components <- dplyr::bind_rows(
  read_imf_datamapper_indicator("PPPGDP", "GDP, current international dollars, PPP"),
  read_imf_datamapper_indicator("PPPPC", "GDP per capita, current international dollars, PPP"),
  read_imf_datamapper_indicator("NGDPD", "GDP, current U.S. dollars"),
  read_imf_datamapper_indicator("NGDPDPC", "GDP per capita, current U.S. dollars"),
  read_imf_datamapper_indicator("LP", "Population")
) |>
  dplyr::left_join(maddison_countries, by = "country_code") |>
  dplyr::arrange(country_code, indicator_id, year)

imf_ppp_wide <- imf_ppp_components |>
  dplyr::select(country_code, country, maddison_region, indicator_id, year, value) |>
  stats::reshape(
    idvar = c("country_code", "country", "maddison_region", "year"),
    timevar = "indicator_id",
    direction = "wide"
  )

names(imf_ppp_wide) <- sub("^value\\.", "", names(imf_ppp_wide))

imf_ppp_scatter_data <- imf_ppp_wide |>
  dplyr::transmute(
    country_code = country_code,
    country = country,
    maddison_region = maddison_region,
    year = year,
    gdp_ppp_current_intl_dollars_billions = PPPGDP,
    gdp_per_capita_ppp_current_intl_dollars = PPPPC,
    gdp_nominal_current_usd_billions = NGDPD,
    gdp_per_capita_nominal_current_usd = NGDPDPC,
    population_millions = LP,
    highlight_group = dplyr::case_when(
      country_code == "VEN" ~ "Venezuela",
      country_code %in% emerging_latam_codes ~ "LatAm emergente",
      TRUE ~ "Resto del mundo"
    )
  ) |>
  dplyr::filter(
    !is.na(gdp_ppp_current_intl_dollars_billions),
    !is.na(gdp_per_capita_ppp_current_intl_dollars),
    !is.na(gdp_nominal_current_usd_billions),
    !is.na(gdp_per_capita_nominal_current_usd),
    !is.na(population_millions)
  ) |>
  dplyr::arrange(year, highlight_group, country)

utils::write.csv(
  imf_ppp_components,
  "data/raw/imf_weo_ppp_gdp_population_maddison_countries.csv",
  row.names = FALSE
)
utils::write.csv(
  imf_ppp_scatter_data,
  "data/final/imf_weo_ppp_scatter_data.csv",
  row.names = FALSE
)

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

imf_ppp_scatter <- ggplot2::ggplot() +
  ggplot2::geom_point(
    data = plot_data_background,
    ggplot2::aes(
      x = gdp_per_capita_ppp_current_intl_dollars,
      y = gdp_ppp_current_intl_dollars_billions,
      size = population_millions
    ),
    color = "grey70",
    alpha = 0.45
  ) +
  ggplot2::geom_point(
    data = plot_data_latam,
    ggplot2::aes(
      x = gdp_per_capita_ppp_current_intl_dollars,
      y = gdp_ppp_current_intl_dollars_billions,
      size = population_millions
    ),
    color = "#1f77b4",
    alpha = 0.75
  ) +
  ggplot2::geom_point(
    data = plot_data_venezuela,
    ggplot2::aes(
      x = gdp_per_capita_ppp_current_intl_dollars,
      y = gdp_ppp_current_intl_dollars_billions,
      size = population_millions
    ),
    color = "#d62728",
    alpha = 0.95
  ) +
  ggplot2::geom_text(
    data = plot_data_venezuela,
    ggplot2::aes(
      x = gdp_per_capita_ppp_current_intl_dollars,
      y = gdp_ppp_current_intl_dollars_billions,
      label = "VEN"
    ),
    color = "#d62728",
    nudge_y = 0.10,
    size = 3.3,
    fontface = "bold",
    check_overlap = TRUE
  ) +
  ggplot2::facet_wrap(ggplot2::vars(year), ncol = 3) +
  ggplot2::scale_x_log10(labels = scales::label_dollar(prefix = "$", accuracy = 1)) +
  ggplot2::scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  ggplot2::scale_size_area(max_size = 5.5, labels = scales::label_number(suffix = " M"), name = "Población") +
  ggplot2::labs(
    title = "PIB per cápita y tamaño económico en paridad de poder de compra",
    subtitle = "FMI WEO. Venezuela en rojo; LatAm emergente en azul; resto del mundo en gris.",
    x = "PIB per cápita PPP, dólares internacionales corrientes",
    y = "PIB total PPP, miles de millones"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  filename = file.path(figure_dir, "imf_weo_ppp_scatter_selected_years.png"),
  plot = imf_ppp_scatter,
  width = 11,
  height = 6.2,
  dpi = 160
)

build_single_year_scatter <- function(selected_year, x_var, y_var, x_limits, y_limits, title_prefix, x_label, y_label) {
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
        y = .data[[y_var]]
      ),
      color = "grey70",
      alpha = 0.45,
      size = 1.5
    ) +
    ggplot2::geom_point(
      data = single_year_latam,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]]
      ),
      color = "#1f77b4",
      alpha = 0.8,
      size = 2.2
    ) +
    ggplot2::geom_point(
      data = single_year_venezuela,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]]
      ),
      color = "#d62728",
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
      color = "#2f2f2f",
      nudge_y = 0.12,
      size = 3.6,
      fontface = "bold"
    ) +
    ggplot2::scale_x_log10(
      labels = scales::label_dollar(prefix = "$", accuracy = 1),
      limits = x_limits
    ) +
    ggplot2::scale_y_log10(
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      limits = y_limits
    ) +
    ggplot2::labs(
      title = sprintf("%s, %s", title_prefix, selected_year),
      subtitle = "FMI WEO. Venezuela en rojo; LatAm emergente en azul; resto del mundo en gris.",
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

single_years <- c(1999L, 2008L, 2013L, 2018L, 2025L)
single_year_plot_data <- imf_ppp_scatter_data |>
  dplyr::filter(year %in% single_years)
fixed_x_limits <- range(single_year_plot_data$gdp_per_capita_ppp_current_intl_dollars, na.rm = TRUE)
fixed_y_limits <- range(single_year_plot_data$gdp_ppp_current_intl_dollars_billions, na.rm = TRUE)
fixed_nominal_x_limits <- range(single_year_plot_data$gdp_per_capita_nominal_current_usd, na.rm = TRUE)
fixed_nominal_y_limits <- range(single_year_plot_data$gdp_nominal_current_usd_billions, na.rm = TRUE)

for (single_year in single_years) {
  single_year_plot <- build_single_year_scatter(
    single_year,
    x_var = "gdp_per_capita_ppp_current_intl_dollars",
    y_var = "gdp_ppp_current_intl_dollars_billions",
    x_limits = fixed_x_limits,
    y_limits = fixed_y_limits,
    title_prefix = "PIB per cápita y tamaño económico",
    x_label = "PIB per cápita PPP, dólares internacionales corrientes",
    y_label = "PIB total PPP, miles de millones"
  )
  ggplot2::ggsave(
    filename = file.path(figure_dir, sprintf("imf_weo_ppp_scatter_%s.png", single_year)),
    plot = single_year_plot,
    width = 11,
    height = 6.2,
    dpi = 160
  )

  single_year_nominal_plot <- build_single_year_scatter(
    single_year,
    x_var = "gdp_per_capita_nominal_current_usd",
    y_var = "gdp_nominal_current_usd_billions",
    x_limits = fixed_nominal_x_limits,
    y_limits = fixed_nominal_y_limits,
    title_prefix = "PIB nominal per cápita y tamaño económico",
    x_label = "PIB nominal per cápita, dólares corrientes",
    y_label = "PIB nominal total, miles de millones de dólares"
  )
  ggplot2::ggsave(
    filename = file.path(figure_dir, sprintf("imf_weo_nominal_scatter_%s.png", single_year)),
    plot = single_year_nominal_plot,
    width = 11,
    height = 6.2,
    dpi = 160
  )
}

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
    ggplot2::geom_hline(yintercept = 1, color = "grey45", linewidth = 0.35) +
    ggplot2::geom_jitter(
      data = background_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela),
      width = 0.18,
      height = 0,
      color = "grey72",
      alpha = 0.32,
      size = 1
    ) +
    ggplot2::geom_jitter(
      data = emerging_latam_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela),
      width = 0.18,
      height = 0,
      color = "#1f77b4",
      alpha = 0.7,
      size = 1.4
    ) +
    ggplot2::geom_point(
      data = venezuela_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela),
      color = "#d62728",
      size = 1.8,
      alpha = 0.95
    ) +
    ggplot2::scale_y_log10(
      labels = scales::label_number(accuracy = 0.1),
      breaks = c(0.1, 0.25, 0.5, 1, 2, 5, 10, 25)
    ) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2025, 5), limits = c(1999, 2025)) +
    ggplot2::coord_cartesian(ylim = c(0.1, 25)) +
    ggplot2::labs(
      title = title,
      subtitle = "Cada punto es un país-año. Venezuela = 1; LatAm emergente en azul; resto del mundo en gris.",
      x = NULL,
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = 12)
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
    dplyr::filter(year %in% factor(c(2001L, 2025L), levels = selected_ratio_years), country_code %in% final_year_ratio_label_codes) |>
    dplyr::mutate(label_nudge = dplyr::if_else(as.character(year) == "2001", -0.18, 0.18))

  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 1, color = "grey45", linewidth = 0.35) +
    ggplot2::geom_jitter(
      data = background_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela),
      width = 0.14,
      height = 0,
      color = "grey72",
      alpha = 0.4,
      size = 1.5
    ) +
    ggplot2::geom_jitter(
      data = emerging_latam_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela),
      width = 0.14,
      height = 0,
      color = "#1f77b4",
      alpha = 0.75,
      size = 2
    ) +
    ggplot2::geom_point(
      data = venezuela_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela),
      color = "#d62728",
      size = 2.4,
      alpha = 0.95
    ) +
    ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(x = year, y = ratio_to_venezuela, label = country_code),
      color = "#2f2f2f",
      nudge_x = label_data$label_nudge,
      size = 3.7,
      fontface = "bold",
      check_overlap = TRUE
    ) +
    ggplot2::scale_y_log10(
      labels = scales::label_number(accuracy = 0.1),
      breaks = c(0.1, 0.25, 0.5, 1, 2, 5, 10, 25)
    ) +
    ggplot2::coord_cartesian(ylim = c(0.1, 25), clip = "off") +
    ggplot2::labs(
      title = title,
      subtitle = "Años seleccionados. Venezuela = 1; etiquetas solo en 2025.",
      x = NULL,
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.margin = ggplot2::margin(5.5, 30, 5.5, 5.5))
}

gdp_pc_ppp_ratio_plot <- build_venezuela_ratio_plot(
  imf_ppp_scatter_data,
  value_var = "gdp_per_capita_ppp_current_intl_dollars",
  title = "PIB per cápita PPP relativo a Venezuela",
  y_label = "PIB per cápita PPP / Venezuela"
)

gdp_pc_nominal_ratio_plot <- build_venezuela_ratio_plot(
  imf_ppp_scatter_data,
  value_var = "gdp_per_capita_nominal_current_usd",
  title = "PIB nominal per cápita relativo a Venezuela",
  y_label = "PIB nominal per cápita / Venezuela"
)

ggplot2::ggsave(
  filename = file.path(figure_dir, "imf_weo_gdp_pc_ppp_ratio_to_venezuela.png"),
  plot = gdp_pc_ppp_ratio_plot,
  width = 11,
  height = 6.2,
  dpi = 160
)
ggplot2::ggsave(
  filename = file.path(figure_dir, "imf_weo_gdp_pc_nominal_ratio_to_venezuela.png"),
  plot = gdp_pc_nominal_ratio_plot,
  width = 11,
  height = 6.2,
  dpi = 160
)

gdp_pc_ppp_ratio_selected_years_plot <- build_selected_year_venezuela_ratio_plot(
  imf_ppp_scatter_data,
  value_var = "gdp_per_capita_ppp_current_intl_dollars",
  title = "PIB per cápita PPP relativo a Venezuela, años seleccionados",
  y_label = "PIB per cápita PPP / Venezuela"
)

gdp_pc_nominal_ratio_selected_years_plot <- build_selected_year_venezuela_ratio_plot(
  imf_ppp_scatter_data,
  value_var = "gdp_per_capita_nominal_current_usd",
  title = "PIB nominal per cápita relativo a Venezuela, años seleccionados",
  y_label = "PIB nominal per cápita / Venezuela"
)

ggplot2::ggsave(
  filename = file.path(figure_dir, "imf_weo_gdp_pc_ppp_ratio_selected_years_to_venezuela.png"),
  plot = gdp_pc_ppp_ratio_selected_years_plot,
  width = 11,
  height = 6.2,
  dpi = 160
)
ggplot2::ggsave(
  filename = file.path(figure_dir, "imf_weo_gdp_pc_nominal_ratio_selected_years_to_venezuela.png"),
  plot = gdp_pc_nominal_ratio_selected_years_plot,
  width = 11,
  height = 6.2,
  dpi = 160
)

message("Wrote data/raw/imf_weo_ppp_gdp_population_maddison_countries.csv")
message("Wrote data/final/imf_weo_ppp_scatter_data.csv")
message("Wrote ", file.path(figure_dir, "imf_weo_ppp_scatter_selected_years.png"))
for (single_year in single_years) {
  message("Wrote ", file.path(figure_dir, sprintf("imf_weo_ppp_scatter_%s.png", single_year)))
  message("Wrote ", file.path(figure_dir, sprintf("imf_weo_nominal_scatter_%s.png", single_year)))
}
message("Wrote ", file.path(figure_dir, "imf_weo_gdp_pc_ppp_ratio_to_venezuela.png"))
message("Wrote ", file.path(figure_dir, "imf_weo_gdp_pc_nominal_ratio_to_venezuela.png"))
message("Wrote ", file.path(figure_dir, "imf_weo_gdp_pc_ppp_ratio_selected_years_to_venezuela.png"))
message("Wrote ", file.path(figure_dir, "imf_weo_gdp_pc_nominal_ratio_selected_years_to_venezuela.png"))
