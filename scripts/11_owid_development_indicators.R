# Replicate selected OWID development indicator relationships for the presentation.

## Setup -----------------------------------------------------------------------
# Check packages and load the shared presentation theme.
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

source("scripts/_presentation_theme.R")

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/final", recursive = TRUE, showWarnings = FALSE)
figure_dir <- "reports/presentation/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

## Plot constants --------------------------------------------------------------
# Shared caption, country groups, and OWID region colors.
presentation_source_caption <- build_source_caption(
  "Our World in Data Grapher (2024-2025) y FMI World Economic Outlook Database (2025)",
  calculations = TRUE
)

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
owid_region_colors <- c(
  África = presentation_colors[["reference"]],
  Asia = presentation_colors[["accent"]],
  Europa = presentation_colors[["primary"]],
  `América del Norte` = presentation_colors[["latam"]],
  Oceanía = presentation_colors[["recovery"]],
  `América del Sur` = presentation_colors[["venezuela"]]
)

owid_region_labels <- c(
  Africa = "África",
  Asia = "Asia",
  Europe = "Europa",
  `North America` = "América del Norte",
  Oceania = "Oceanía",
  `South America` = "América del Sur"
)

## Family: OWID development relationship specifications
indicator_specs <- list(
  # Graph: Esperanza de vida y PIB pc
  list(
    id = "life_expectancy",
    slug = "life-expectancy-vs-gdp-per-capita",
    csv_url = "https://ourworldindata.org/grapher/life-expectancy-vs-gdp-per-capita.csv?v=1&csvType=full&useColumnShortNames=true",
    y_column = "life_expectancy_0",
    x_column = "gdp_per_capita",
    population_column = "population_historical",
    title = "Esperanza de vida y PIB per capita",
    subtitle = "Esperanza de vida y PIB per capita: OWID.",
    y_label = "Esperanza de vida al nacer",
    y_limits = c(40, 90),
    y_breaks = c(40, 50, 60, 70, 80, 90),
    y_log = FALSE,
    file_stem = "owid_life_expectancy_gdp_per_capita"
  ),
  # Graph: Ingreso diario y PIB pc
  list(
    id = "daily_income",
    slug = "median-daily-per-capita-expenditure-vs-gdp-per-capita",
    csv_url = "https://ourworldindata.org/grapher/median-daily-per-capita-expenditure-vs-gdp-per-capita.csv?v=1&csvType=full&useColumnShortNames=true",
    y_column = "median__ppp_version_2021__welfare_type_income_or_consumption__period_day__table_income_or_consumption_consolidated__survey_comparability_no_spells",
    x_column = "ny_gdp_pcap_pp_kd",
    population_column = "population_historical",
    title = "Ingreso mediano diario y PIB per capita",
    subtitle = "Ingreso o consumo mediano diario y PIB per capita: OWID/PIP.",
    y_label = "Ingreso o consumo mediano diario per capita",
    y_limits = c(1, 100),
    y_breaks = c(1, 2, 5, 10, 20, 50, 100),
    y_log = TRUE,
    latest_per_country = TRUE,
    fallback_imf_gdp = TRUE,
    file_stem = "owid_daily_income_gdp_per_capita"
  ),
  # Graph: Mortalidad infantil y PIB pc
  list(
    id = "child_mortality",
    slug = "child-mortality-gdp-per-capita",
    csv_url = "https://ourworldindata.org/grapher/child-mortality-gdp-per-capita.csv?v=1&csvType=full&useColumnShortNames=true",
    y_column = "child_mortality_rate",
    x_column = "gdp_per_capita",
    population_column = "population_historical",
    title = "Mortalidad infantil y PIB pc",
    subtitle = "Mortalidad infantil y PIB per capita: OWID.",
    y_label = "Muertes menores de 5 por 100 nacidos vivos",
    y_limits = c(0.2, 100),
    y_breaks = c(0.2, 0.5, 1, 2, 5, 10, 20, 50, 100),
    y_log = TRUE,
    y_reverse = TRUE,
    file_stem = "owid_child_mortality_gdp_per_capita"
  ),
  # Graph: IDH y PIB per capita
  list(
    id = "hdi",
    slug = "human-development-index-vs-gdp-per-capita",
    csv_url = "https://ourworldindata.org/grapher/human-development-index-vs-gdp-per-capita.csv?v=1&csvType=full&useColumnShortNames=true",
    y_column = "hdi__sex_total",
    x_column = "ny_gdp_pcap_pp_kd",
    population_column = "population_historical",
    title = "IDH y PIB per capita",
    subtitle = "Indice de Desarrollo Humano y PIB per capita: OWID/UNDP.",
    y_label = "Indice de Desarrollo Humano",
    y_limits = c(0.35, 1),
    y_breaks = c(0.35, seq(0.4, 1, 0.1)),
    y_log = FALSE,
    file_stem = "owid_hdi_gdp_per_capita"
  ),
  # Graph: Escolaridad y PIB pc
  list(
    id = "schooling",
    slug = "average-years-of-schooling-vs-gdp-per-capita",
    csv_url = "https://ourworldindata.org/grapher/average-years-of-schooling-vs-gdp-per-capita.csv?v=1&csvType=full&useColumnShortNames=true",
    y_column = "mys__sex_total",
    x_column = "ny_gdp_pcap_pp_kd",
    population_column = "population_historical",
    title = "Escolaridad y PIB per capita",
    subtitle = "Anios promedio de escolaridad y PIB per capita: OWID/UNDP.",
    y_label = "Anios promedio de escolaridad",
    y_limits = c(1, 20),
    y_breaks = c(1, 2, 5, 10, 15, 20),
    y_log = FALSE,
    file_stem = "owid_schooling_gdp_per_capita"
  )
)

# Graph: Democracia y PIB pc
democracy_spec <- list(
  id = "democracy",
  variable_id = 1014800,
  title = "Democracia y PIB per capita",
  subtitle = "Democracy Index: OWID/EIU. PIB per capita PPP: FMI WEO.",
  y_label = "Democracy Index (0-10)",
  y_limits = c(0.2, 10),
  y_breaks = c(0.2, 0.5, 1, 2, 3, 5, 7, 10),
  y_log = TRUE,
  file_stem = "owid_democracy_gdp_per_capita"
)

## Dependency data -------------------------------------------------------------
# Read IMF GDP per capita data used as the common x-axis.
imf_gdp <- utils::read.csv(imf_gdp_path, stringsAsFactors = FALSE) |>
  dplyr::transmute(
    country_code = country_code,
    year = as.integer(year),
    gdp_per_capita = as.numeric(gdp_per_capita_ppp_current_intl_dollars),
    gdp_population = as.numeric(population_millions) * 1e6
  ) |>
  dplyr::filter(!is.na(gdp_per_capita), gdp_per_capita > 0)

owid_x_limits <- range(imf_gdp$gdp_per_capita, na.rm = TRUE)
owid_x_breaks <- c(500, 1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000)
owid_x_breaks <- owid_x_breaks[owid_x_breaks >= owid_x_limits[[1]] & owid_x_breaks <= owid_x_limits[[2]]]

build_highlight_group <- function(country_code) {
  dplyr::case_when(
    country_code == "VEN" ~ "Venezuela",
    country_code %in% emerging_latam_codes ~ "LatAm emergente",
    TRUE ~ "Resto del mundo"
  )
}

## Data helpers ----------------------------------------------------------------
# Download and normalize one OWID Grapher indicator.
read_grapher_indicator <- function(spec) {
  raw_path <- file.path("data/raw", sprintf("%s.csv", spec$slug))
  utils::download.file(
    spec$csv_url,
    raw_path,
    mode = "wb",
    quiet = TRUE
  )

  grapher_raw <- utils::read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE)

  grapher_raw |>
    dplyr::transmute(
      indicator_id = spec$id,
      country = entity,
      country_code = code,
      year = as.integer(year),
      indicator_value = as.numeric(.data[[spec$y_column]]),
      gdp_per_capita = as.numeric(.data[[spec$x_column]]),
      population = as.numeric(.data[[spec$population_column]]),
      owid_region = owid_region_labels[owid_region]
    ) |>
    dplyr::filter(
      !is.na(country_code),
      nchar(country_code) == 3,
      !is.na(indicator_value),
      is.na(gdp_per_capita) | gdp_per_capita > 0
    )
}

# Read the OWID API format used by the Democracy Index.
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

# Select the year or latest available observation used for each indicator.
build_plot_data <- function(indicator_data, spec = NULL) {
  selected_year <- if (!is.null(spec$selected_year)) {
    as.integer(spec$selected_year)
  } else if (isTRUE(spec$latest_per_country)) {
    NA_integer_
  } else {
    max(indicator_data$year[!is.na(indicator_data$gdp_per_capita)], na.rm = TRUE)
  }

  selected_data <- if (isTRUE(spec$latest_per_country)) {
    latest_data <- indicator_data |>
      dplyr::arrange(country_code, dplyr::desc(year)) |>
      dplyr::group_by(country_code) |>
      dplyr::slice(1) |>
      dplyr::ungroup()

    if (isTRUE(spec$fallback_imf_gdp)) {
      latest_data |>
        dplyr::left_join(
          imf_gdp |>
            dplyr::select(country_code, year, imf_gdp_per_capita = gdp_per_capita),
          by = c("country_code", "year")
        ) |>
        dplyr::mutate(gdp_per_capita = dplyr::coalesce(gdp_per_capita, imf_gdp_per_capita)) |>
        dplyr::select(-imf_gdp_per_capita)
    } else {
      latest_data
    }
  } else {
    indicator_data |>
      dplyr::filter(year == selected_year)
  }

  selected_data |>
    dplyr::mutate(
      owid_region = factor(owid_region, levels = names(owid_region_colors))
    ) |>
    dplyr::filter(!is.na(gdp_per_capita), gdp_per_capita > 0)
}

# Round log-axis limits to clean powers and candidate breaks.
nice_log10_limits <- function(values, breaks) {
  positive_values <- values[!is.na(values) & values > 0]
  data_min <- min(positive_values)
  data_max <- max(positive_values)
  c(
    max(breaks[breaks <= data_min]),
    min(breaks[breaks >= data_max])
  )
}

## Chart helper ----------------------------------------------------------------
# Build one OWID relationship chart from a prepared indicator table.
build_relationship_chart <- function(plot_data, spec, x_limits, x_breaks) {
  selected_year <- if (isTRUE(spec$latest_per_country)) {
    sprintf(
      "último dato disponible, %s-%s",
      min(plot_data$year, na.rm = TRUE),
      max(plot_data$year, na.rm = TRUE)
    )
  } else {
    unique(plot_data$year)
  }
  plot_data <- plot_data |>
    dplyr::filter(gdp_per_capita >= x_limits[[1]], gdp_per_capita <= x_limits[[2]])

  if (isTRUE(spec$y_log)) {
    plot_data <- plot_data |>
      dplyr::filter(indicator_value > 0)
  }
  if (isTRUE(spec$y_reverse)) {
    plot_data <- plot_data |>
      dplyr::mutate(plot_indicator_value = 1 / indicator_value)
    y_limits <- 1 / rev(spec$y_limits)
    y_breaks <- 1 / rev(spec$y_breaks)
    y_labels <- scales::label_number(accuracy = 0.1)(rev(spec$y_breaks))
  } else {
    plot_data <- plot_data |>
      dplyr::mutate(plot_indicator_value = indicator_value)
    y_limits <- spec$y_limits
    y_breaks <- spec$y_breaks
    y_labels <- scales::label_number(accuracy = 1)(spec$y_breaks)
  }

  chart <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_point(
      ggplot2::aes(x = gdp_per_capita, y = plot_indicator_value, color = owid_region),
      alpha = 0.72,
      size = 2.7
    ) +
    ggplot2::scale_x_log10(
      limits = x_limits,
      breaks = x_breaks,
      labels = scales::label_dollar(prefix = "$", accuracy = 1),
      expand = ggplot2::expansion(mult = c(0, 0.025))
    ) +
    ggplot2::scale_color_manual(
      values = owid_region_colors,
      breaks = names(owid_region_colors),
      drop = FALSE,
      name = NULL
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

  if (isTRUE(spec$y_log)) {
    chart <- chart +
      ggplot2::scale_y_log10(
        limits = y_limits,
        breaks = y_breaks,
        labels = y_labels,
        expand = c(0, 0)
      )
  } else {
    if (isTRUE(spec$y_reverse)) {
      chart <- chart +
        ggplot2::scale_y_reverse(
          limits = y_limits,
          breaks = y_breaks,
          expand = c(0, 0)
        )
    } else {
      chart <- chart +
        ggplot2::scale_y_continuous(
        limits = y_limits,
        breaks = y_breaks,
        expand = c(0, 0)
      )
    }
  }

  chart
}

all_specs <- c(indicator_specs, list(democracy_spec))
final_parts <- list()

## Indicator data --------------------------------------------------------------
# Download and prepare all standard OWID indicators.
for (spec in indicator_specs) {
  indicator_data <- read_grapher_indicator(spec)
  plot_data <- build_plot_data(indicator_data, spec)
  final_parts[[spec$id]] <- plot_data
}

# Reuse OWID region assignments for the separate democracy API response.
owid_region_lookup <- dplyr::bind_rows(final_parts) |>
  dplyr::filter(!is.na(owid_region)) |>
  dplyr::select(country_code, owid_region) |>
  dplyr::distinct(country_code, .keep_all = TRUE)

democracy_data <- read_democracy_indicator(democracy_spec)

# Join democracy data to IMF GDP per capita for the selected common year.
democracy_selected_year <- min(
  max(democracy_data$year, na.rm = TRUE),
  max(imf_gdp$year, na.rm = TRUE)
)
democracy_plot_data <- democracy_data |>
  dplyr::filter(year == democracy_selected_year) |>
  dplyr::left_join(
    imf_gdp |>
      dplyr::filter(year == democracy_selected_year) |>
      dplyr::select(country_code, gdp_per_capita, gdp_population),
    by = "country_code"
  ) |>
  dplyr::left_join(owid_region_lookup, by = "country_code") |>
  dplyr::mutate(
    population = gdp_population,
    owid_region = factor(owid_region, levels = names(owid_region_colors))
  ) |>
  dplyr::filter(!is.na(gdp_per_capita), gdp_per_capita > 0, !is.na(owid_region))
final_parts[[democracy_spec$id]] <- democracy_plot_data

owid_x_break_candidates <- c(
  200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000, 500000
)
owid_x_limits <- nice_log10_limits(
  unlist(lapply(final_parts, function(part) part$gdp_per_capita)),
  owid_x_break_candidates
)
owid_x_breaks <- owid_x_break_candidates
owid_x_breaks <- owid_x_breaks[owid_x_breaks >= owid_x_limits[[1]] & owid_x_breaks <= owid_x_limits[[2]]]

## Family: OWID development relationship graphs
for (spec in indicator_specs) {
  # Graph: relationship chart named by spec$file_stem
  save_presentation_plot(
    filename = file.path(figure_dir, sprintf("%s.png", spec$file_stem)),
    plot = build_relationship_chart(final_parts[[spec$id]], spec, owid_x_limits, owid_x_breaks),
    source_caption = presentation_source_caption
  )
}

save_presentation_plot(
  filename = file.path(figure_dir, sprintf("%s.png", democracy_spec$file_stem)),
  plot = build_relationship_chart(democracy_plot_data, democracy_spec, owid_x_limits, owid_x_breaks),
  source_caption = presentation_source_caption
)

final_data <- dplyr::bind_rows(final_parts)

## Data outputs ----------------------------------------------------------------
# Persist the combined OWID relationship table for review.
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
