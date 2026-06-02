# Replicate selected OWID development indicator relationships for the presentation.
required_packages <- c("dplyr", "ggplot2", "jsonlite", "scales")
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
dir.create("data/final", recursive = TRUE, showWarnings = FALSE)
figure_dir <- "reports/presentation/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

imf_gdp_path <- "data/final/imf_weo_ppp_scatter_data.csv"
if (!file.exists(imf_gdp_path)) {
  stop(sprintf("Missing `%s`. Run scripts/10_imf_weo_ppp_scatter.R first.", imf_gdp_path), call. = FALSE)
}

emerging_latam_codes <- c(
  "ATG", "ARG", "ABW", "BHS", "BRB", "BLZ", "BOL", "BRA", "CHL", "COL",
  "CRI", "DMA", "DOM", "ECU", "SLV", "GRD", "GTM", "GUY", "HTI", "HND",
  "JAM", "MEX", "NIC", "PAN", "PRI", "PRY", "PER", "KNA", "LCA", "VCT",
  "SUR", "TTO", "URY", "VEN"
)
label_codes <- c("VEN", "COL", "ECU", "BRA", "ARG", "PER", "CHL", "USA", "ESP")

apply_presentation_axis_theme <- function(plot = NULL) {
  axis_theme <- ggplot2::theme(
    axis.line.x = ggplot2::element_line(color = "grey35", linewidth = 0.35),
    axis.line.y = ggplot2::element_line(color = "grey35", linewidth = 0.35),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank()
  )

  if (is.null(plot)) {
    return(axis_theme)
  }

  plot + axis_theme
}

indicator_specs <- list(
  list(
    id = "life_expectancy",
    slug = "life-expectancy-un-vs-gdp-per-capita-wb",
    y_column = "Life expectancy",
    title = "Esperanza de vida y PIB per capita",
    subtitle = "Esperanza de vida: OWID/UN. PIB per capita PPP: FMI WEO.",
    y_label = "Esperanza de vida al nacer",
    y_limits = c(45, 90),
    y_breaks = seq(45, 90, 5),
    file_stem = "owid_life_expectancy_gdp_per_capita"
  ),
  list(
    id = "daily_income",
    slug = "mean-daily-per-capita-expenditure-vs-gdp-per-capita",
    y_column = "Mean income or consumption per day",
    title = "Ingreso diario y PIB per capita",
    subtitle = "Ingreso o consumo diario: OWID/PIP. PIB per capita PPP: FMI WEO.",
    y_label = "Ingreso o consumo diario per capita",
    y_limits = c(0, 80),
    y_breaks = seq(0, 80, 10),
    file_stem = "owid_daily_income_gdp_per_capita"
  ),
  list(
    id = "child_mortality",
    slug = "child-mortality-vs-level-of-prosperity-regression",
    y_column = "Child mortality",
    title = "Mortalidad infantil y PIB pc",
    subtitle = "Mortalidad infantil: OWID/UN IGME. PIB per capita PPP: FMI WEO.",
    y_label = "Muertes menores de 5 por 100 nacidos vivos",
    y_limits = c(0, 15),
    y_breaks = seq(0, 15, 2.5),
    file_stem = "owid_child_mortality_gdp_per_capita"
  ),
  list(
    id = "hdi",
    slug = "human-development-index-vs-gdp-per-capita",
    y_column = "Human Development Index",
    title = "IDH y PIB per capita",
    subtitle = "Indice de Desarrollo Humano: OWID/UNDP. PIB per capita PPP: FMI WEO.",
    y_label = "Indice de Desarrollo Humano",
    y_limits = c(0.35, 1),
    y_breaks = seq(0.4, 1, 0.1),
    file_stem = "owid_hdi_gdp_per_capita"
  ),
  list(
    id = "schooling",
    slug = "average-years-of-schooling-vs-gdp-per-capita",
    y_column = "Both genders",
    title = "Escolaridad y PIB per capita",
    subtitle = "Anios promedio de escolaridad: OWID/UNDP. PIB per capita PPP: FMI WEO.",
    y_label = "Anios promedio de escolaridad",
    y_limits = c(0, 16),
    y_breaks = seq(0, 16, 2),
    file_stem = "owid_schooling_gdp_per_capita"
  )
)

democracy_spec <- list(
  id = "democracy",
  variable_id = 1014800,
  title = "Democracia y PIB per capita",
  subtitle = "Democracy Index: OWID/EIU. PIB per capita PPP: FMI WEO.",
  y_label = "Democracy Index (0-10)",
  y_limits = c(0, 10),
  y_breaks = seq(0, 10, 2),
  file_stem = "owid_democracy_gdp_per_capita"
)

imf_gdp <- utils::read.csv(imf_gdp_path, stringsAsFactors = FALSE) |>
  dplyr::transmute(
    country_code = country_code,
    year = as.integer(year),
    gdp_per_capita = as.numeric(gdp_per_capita_ppp_current_intl_dollars),
    gdp_population = as.numeric(population_millions) * 1e6
  ) |>
  dplyr::filter(!is.na(gdp_per_capita), gdp_per_capita > 0)

build_highlight_group <- function(country_code) {
  dplyr::case_when(
    country_code == "VEN" ~ "Venezuela",
    country_code %in% emerging_latam_codes ~ "LatAm emergente",
    TRUE ~ "Resto del mundo"
  )
}

read_grapher_indicator <- function(spec) {
  raw_path <- file.path("data/raw", sprintf("%s.csv", spec$slug))
  utils::download.file(
    sprintf("https://ourworldindata.org/grapher/%s.csv", spec$slug),
    raw_path,
    mode = "wb",
    quiet = TRUE
  )

  utils::read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE) |>
    dplyr::transmute(
      indicator_id = spec$id,
      country = Entity,
      country_code = Code,
      year = as.integer(Year),
      indicator_value = as.numeric(.data[[spec$y_column]]),
      population = as.numeric(Population)
    ) |>
    dplyr::filter(
      !is.na(country_code),
      nchar(country_code) == 3,
      !is.na(indicator_value)
    )
}

read_democracy_indicator <- function(spec) {
  data_url <- sprintf("https://api.ourworldindata.org/v1/indicators/%s.data.json", spec$variable_id)
  metadata_url <- sprintf("https://api.ourworldindata.org/v1/indicators/%s.metadata.json", spec$variable_id)
  indicator_data <- jsonlite::fromJSON(data_url, simplifyVector = TRUE)
  indicator_metadata <- jsonlite::fromJSON(metadata_url, simplifyVector = FALSE)

  entity_lookup <- dplyr::bind_rows(lapply(indicator_metadata$dimensions$entities$values, function(entity) {
    data.frame(
      entity_id = entity$id,
      country = entity$name,
      country_code = if (is.null(entity$code)) NA_character_ else entity$code,
      stringsAsFactors = FALSE
    )
  }))

  data.frame(
    entity_id = indicator_data$entities,
    year = as.integer(indicator_data$years),
    indicator_value = as.numeric(indicator_data$values),
    stringsAsFactors = FALSE
  ) |>
    dplyr::left_join(entity_lookup, by = "entity_id") |>
    dplyr::transmute(
      indicator_id = spec$id,
      country = country,
      country_code = country_code,
      year = year,
      indicator_value = indicator_value,
      population = NA_real_
    ) |>
    dplyr::filter(
      !is.na(country_code),
      nchar(country_code) == 3,
      !is.na(indicator_value)
    )
}

build_plot_data <- function(indicator_data) {
  selected_year <- min(
    max(indicator_data$year[indicator_data$country_code == "VEN"], na.rm = TRUE),
    max(imf_gdp$year[imf_gdp$country_code == "VEN"], na.rm = TRUE)
  )

  indicator_data |>
    dplyr::filter(year == selected_year) |>
    dplyr::left_join(
      imf_gdp |>
        dplyr::filter(year == selected_year) |>
        dplyr::select(country_code, gdp_per_capita, gdp_population),
      by = "country_code"
    ) |>
    dplyr::mutate(
      population = dplyr::coalesce(population, gdp_population),
      highlight_group = build_highlight_group(country_code),
      highlight_group = factor(highlight_group, levels = c("Resto del mundo", "LatAm emergente", "Venezuela"))
    ) |>
    dplyr::filter(!is.na(gdp_per_capita), !is.na(population))
}

build_relationship_chart <- function(plot_data, spec) {
  selected_year <- unique(plot_data$year)
  background_data <- plot_data |>
    dplyr::filter(highlight_group == "Resto del mundo")
  latam_data <- plot_data |>
    dplyr::filter(highlight_group == "LatAm emergente")
  venezuela_data <- plot_data |>
    dplyr::filter(highlight_group == "Venezuela")
  label_data <- plot_data |>
    dplyr::filter(country_code %in% label_codes)

  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = background_data,
      ggplot2::aes(x = gdp_per_capita, y = indicator_value, size = population),
      color = "grey72",
      alpha = 0.38
    ) +
    ggplot2::geom_point(
      data = latam_data,
      ggplot2::aes(x = gdp_per_capita, y = indicator_value, size = population),
      color = "#1f77b4",
      alpha = 0.78
    ) +
    ggplot2::geom_point(
      data = venezuela_data,
      ggplot2::aes(x = gdp_per_capita, y = indicator_value, size = population),
      color = "#d62728",
      alpha = 0.95
    ) +
    ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(x = gdp_per_capita, y = indicator_value, label = country_code),
      color = "#2f2f2f",
      nudge_y = diff(spec$y_limits) * 0.02,
      size = 3.6,
      fontface = "bold",
      check_overlap = TRUE
    ) +
    ggplot2::scale_x_log10(labels = scales::label_dollar(prefix = "$", accuracy = 1)) +
    ggplot2::scale_y_continuous(limits = spec$y_limits, breaks = spec$y_breaks) +
    ggplot2::scale_size_area(
      max_size = 7,
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      name = "Poblacion"
    ) +
    ggplot2::labs(
      title = sprintf("%s, %s", spec$title, selected_year),
      subtitle = spec$subtitle,
      x = "PIB per capita, PPP",
      y = spec$y_label
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom") +
    apply_presentation_axis_theme()
}

all_specs <- c(indicator_specs, list(democracy_spec))
final_parts <- list()

for (spec in indicator_specs) {
  indicator_data <- read_grapher_indicator(spec)
  plot_data <- build_plot_data(indicator_data)
  final_parts[[spec$id]] <- plot_data
  ggplot2::ggsave(
    filename = file.path(figure_dir, sprintf("%s.png", spec$file_stem)),
    plot = build_relationship_chart(plot_data, spec),
    width = 11,
    height = 6.2,
    dpi = 160
  )
}

democracy_data <- read_democracy_indicator(democracy_spec)
democracy_plot_data <- build_plot_data(democracy_data)
final_parts[[democracy_spec$id]] <- democracy_plot_data
ggplot2::ggsave(
  filename = file.path(figure_dir, sprintf("%s.png", democracy_spec$file_stem)),
  plot = build_relationship_chart(democracy_plot_data, democracy_spec),
  width = 11,
  height = 6.2,
  dpi = 160
)

final_data <- dplyr::bind_rows(final_parts)
utils::write.csv(final_data, "data/final/owid_development_relationships.csv", row.names = FALSE)

# Keep the previous single-indicator path for compatibility with earlier deck builds.
utils::write.csv(
  final_parts[["life_expectancy"]],
  "data/final/owid_life_expectancy_gdp_per_capita.csv",
  row.names = FALSE
)

for (spec in all_specs) {
  message("Wrote ", file.path(figure_dir, sprintf("%s.png", spec$file_stem)))
}
message("Wrote data/final/owid_development_relationships.csv")
