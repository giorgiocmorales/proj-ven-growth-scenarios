# Replicate selected OWID development indicator relationships for the presentation.

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

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/final", recursive = TRUE, showWarnings = FALSE)
figure_dir <- "outputs/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

## Plot constants ----
# Shared caption, country groups, and OWID region colors.
presentation_source_caption <- build_source_caption(
  "Our World in Data Grapher (2024-2025) y FMI WEO (2025)",
  calculations = TRUE
)
eci_source_caption <- build_source_caption(
  "Growth Lab Atlas of Economic Complexity (2024) y FMI WEO (2025)",
  calculations = TRUE
)

imf_gdp_path <- "data/final/imf_weo_ppp_scatter_data.csv"
if (!file.exists(imf_gdp_path)) {
  stop(sprintf("Missing `%s`. Run scripts/03_download_imf_weo_ppp_scatter_data.R first.", imf_gdp_path), call. = FALSE)
}

emerging_latam_codes <- c(
  "ATG", "ARG", "ABW", "BHS", "BRB", "BLZ", "BOL", "BRA", "CHL", "COL",
  "CRI", "DMA", "DOM", "ECU", "SLV", "GRD", "GTM", "GUY", "HTI", "HND",
  "JAM", "MEX", "NIC", "PAN", "PRI", "PRY", "PER", "KNA", "LCA", "VCT",
  "SUR", "TTO", "URY", "VEN"
)
owid_region_colors <- c(
  build_priority_color_map(c(
    "África",
    "Asia",
    "Europa",
    "América del Norte",
    "Oceanía",
    "América del Sur"
  ))
)

owid_region_labels <- c(
  Africa = "África",
  Asia = "Asia",
  Europe = "Europa",
  `North America` = "América del Norte",
  Oceania = "Oceanía",
  `South America` = "América del Sur"
)

map_wb_region_to_owid_region <- function(wb_region) {
  dplyr::case_when(
    wb_region %in% c("Sub-Saharan Africa (WB)") ~ "África",
    wb_region %in% c(
      "East Asia and Pacific (WB)",
      "South Asia (WB)",
      "Middle East, North Africa, Afghanistan and Pakistan (WB)"
    ) ~ "Asia",
    wb_region %in% c("Europe and Central Asia (WB)") ~ "Europa",
    wb_region %in% c("North America (WB)") ~ "América del Norte",
    wb_region %in% c("Latin America and Caribbean (WB)") ~ "América del Sur",
    TRUE ~ NA_character_
  )
}

## Dependency data ----
# Read IMF GDP per cápita data used as the common x-axis.
imf_gdp <- utils::read.csv(imf_gdp_path, stringsAsFactors = FALSE) %>%
  dplyr::transmute(
    country_code = country_code,
    country = country,
    year = as.integer(year),
    gdp_per_capita = as.numeric(gdp_per_capita_ppp_current_intl_dollars),
    gdp_population = as.numeric(population_millions) * 1e6
  ) %>%
  dplyr::filter(!is.na(gdp_per_capita), gdp_per_capita > 0)

# Latest IMF population values backfill OWID indicators without a population field.
imf_population_lookup <- imf_gdp %>%
  dplyr::filter(!is.na(gdp_population), gdp_population > 0) %>%
  dplyr::arrange(country_code, dplyr::desc(year)) %>%
  dplyr::group_by(country_code) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(country_code, imf_population = gdp_population)

# Use IMF WEO PPP GDP per capita for Venezuela in the same year as the OWID indicator point.
venezuela_imf_gdp_by_year <- imf_gdp %>%
  dplyr::filter(country_code == "VEN") %>%
  dplyr::transmute(
    country_code,
    year,
    venezuela_imf_gdp_per_capita = gdp_per_capita,
    venezuela_imf_population = gdp_population
  )

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

## Data helpers ----
# Read and normalize one cached OWID Grapher indicator.
read_grapher_indicator <- function(indicator_id, slug, y_column, x_column, population_column = NULL) {
  raw_path <- file.path("data/raw", sprintf("%s.csv", slug))
  if (!file.exists(raw_path)) {
    stop(
      sprintf(
        "Missing `%s`. Run scripts/04_download_owid_development_data.R once before graphing.",
        raw_path
      ),
      call. = FALSE
    )
  }

  grapher_raw <- utils::read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE)
  region_values <- if ("owid_region" %in% names(grapher_raw)) {
    unname(owid_region_labels[grapher_raw[["owid_region"]]])
  } else if ("wb_region" %in% names(grapher_raw)) {
    map_wb_region_to_owid_region(grapher_raw[["wb_region"]])
  } else {
    rep(NA_character_, nrow(grapher_raw))
  }

  grapher_raw %>%
    dplyr::mutate(.mapped_owid_region = region_values) %>%
    dplyr::transmute(
      indicator_id = indicator_id,
      country = entity,
      country_code = code,
      year = as.integer(year),
      indicator_value = as.numeric(.data[[y_column]]),
      gdp_per_capita = as.numeric(.data[[x_column]]),
      population = if (is.null(population_column)) NA_real_ else as.numeric(.data[[population_column]]),
      owid_region = .mapped_owid_region
    ) %>%
    dplyr::filter(
      !is.na(country_code),
      nchar(country_code) == 3,
      !is.na(indicator_value),
      is.na(gdp_per_capita) | gdp_per_capita > 0
    )
}

# Read the cached OWID Democracy Index table.
read_democracy_indicator <- function(spec = NULL) {
  raw_path <- "data/raw/owid_democracy_index.csv"
  if (!file.exists(raw_path)) {
    stop(
      sprintf(
        "Missing `%s`. Run scripts/04_download_owid_development_data.R once before graphing.",
        raw_path
      ),
      call. = FALSE
    )
  }

  utils::read.csv(raw_path, stringsAsFactors = FALSE)
}

# Read cached economic-complexity rankings and join them to GDP per capita.
read_eci_indicator <- function() {
  raw_path <- "data/raw/growth_proj_eci_rankings.csv"
  if (!file.exists(raw_path)) {
    stop(sprintf("Missing `%s`.", raw_path), call. = FALSE)
  }

  utils::read.csv(raw_path, stringsAsFactors = FALSE) %>%
    dplyr::transmute(
      indicator_id = "economic_complexity",
      country_code = country_iso3_code,
      year = as.integer(year),
      indicator_value = dplyr::coalesce(
        as.numeric(eci_hs12),
        as.numeric(eci_hs92),
        as.numeric(eci_sitc)
      )
    ) %>%
    dplyr::filter(!is.na(country_code), nchar(country_code) == 3, !is.na(indicator_value))
}

# Select the year or latest available observation used for each indicator.
build_plot_data <- function(
    indicator_data,
    selected_year = NULL,
    latest_per_country = FALSE,
    fallback_imf_gdp = FALSE) {
  selected_year <- if (!is.null(selected_year)) {
    as.integer(selected_year)
  } else if (isTRUE(latest_per_country)) {
    NA_integer_
  } else {
    max(indicator_data$year[!is.na(indicator_data$gdp_per_capita)], na.rm = TRUE)
  }

  selected_data <- if (isTRUE(latest_per_country)) {
    latest_data <- indicator_data %>%
      dplyr::arrange(country_code, dplyr::desc(year)) %>%
      dplyr::group_by(country_code) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    if (isTRUE(fallback_imf_gdp)) {
      latest_data %>%
        dplyr::left_join(
          imf_gdp %>%
            dplyr::select(country_code, year, imf_gdp_per_capita = gdp_per_capita),
          by = c("country_code", "year")
        ) %>%
        dplyr::mutate(gdp_per_capita = dplyr::coalesce(gdp_per_capita, imf_gdp_per_capita)) %>%
        dplyr::select(-imf_gdp_per_capita)
    } else {
      latest_data
    }
  } else {
    indicator_data %>%
      dplyr::filter(year == selected_year)
  }

  selected_data %>%
    dplyr::left_join(imf_population_lookup, by = "country_code") %>%
    dplyr::left_join(venezuela_imf_gdp_by_year, by = c("country_code", "year")) %>%
    dplyr::mutate(
      use_imf_venezuela_gdp = country_code == "VEN" &
        !is.na(venezuela_imf_gdp_per_capita),
      gdp_per_capita = dplyr::if_else(use_imf_venezuela_gdp, venezuela_imf_gdp_per_capita, gdp_per_capita),
      population = dplyr::coalesce(population, imf_population),
      population = dplyr::if_else(
        country_code == "VEN" & !is.na(venezuela_imf_population),
        venezuela_imf_population,
        population
      ),
      gdp_source = dplyr::if_else(
        use_imf_venezuela_gdp,
        sprintf("IMF WEO PPP GDP per capita matched to %s", year),
        "OWID Grapher"
      ),
      owid_region = factor(owid_region, levels = names(owid_region_colors))
    ) %>%
    dplyr::select(-imf_population, -venezuela_imf_gdp_per_capita, -venezuela_imf_population, -use_imf_venezuela_gdp) %>%
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

## Chart helper ----
# Label the largest countries in each non-Oceania region to keep scatterplots readable.
build_regional_label_data <- function(plot_data, labels_per_region = 5L) {
  regional_labels <- plot_data %>%
    dplyr::filter(
      !is.na(owid_region),
      owid_region != "OceanÃ­a",
      !is.na(population),
      population > 0
    ) %>%
    dplyr::arrange(owid_region, dplyr::desc(population), country_code) %>%
    dplyr::group_by(owid_region) %>%
    dplyr::slice_head(n = labels_per_region) %>%
    dplyr::ungroup()

  venezuela_label <- plot_data %>%
    dplyr::filter(country_code == "VEN")

  dplyr::bind_rows(regional_labels, venezuela_label) %>%
    dplyr::distinct(country_code, .keep_all = TRUE) %>%
    dplyr::mutate(country_label = country_code)
}

# Build one OWID relationship chart from a prepared indicator table.
build_relationship_chart <- function(
    plot_data,
    title,
    subtitle,
    y_label,
    y_limits,
    y_breaks,
    x_limits,
    x_breaks,
    y_log = FALSE,
    y_reverse = FALSE,
    latest_per_country = FALSE,
    y_dollar = FALSE) {
  selected_year <- if (isTRUE(latest_per_country)) {
    sprintf(
      "último dato disponible, %s-%s",
      min(plot_data$year, na.rm = TRUE),
      max(plot_data$year, na.rm = TRUE)
    )
  } else {
    unique(plot_data$year)
  }
  plot_data <- plot_data %>%
    dplyr::filter(gdp_per_capita >= x_limits[[1]], gdp_per_capita <= x_limits[[2]])

  if (isTRUE(y_log)) {
    plot_data <- plot_data %>%
      dplyr::filter(indicator_value > 0)
  }
  if (isTRUE(y_reverse)) {
    plot_data <- plot_data %>%
      dplyr::mutate(plot_indicator_value = 1 / indicator_value)
    original_y_breaks <- y_breaks
    y_limits <- 1 / rev(y_limits)
    y_breaks <- 1 / rev(original_y_breaks)
    y_labels <- presentation_number_label(accuracy = 0.1)(rev(original_y_breaks))
  } else {
    plot_data <- plot_data %>%
      dplyr::mutate(plot_indicator_value = indicator_value)
    y_limits <- y_limits
    y_breaks <- y_breaks
    y_label_accuracy <- 0.1
    y_labels <- if (isTRUE(y_dollar)) {
      presentation_dollar_label(accuracy = y_label_accuracy)(y_breaks)
    } else {
      presentation_number_label(accuracy = y_label_accuracy)(y_breaks)
    }
  }

  # Compress population moderately so smaller countries remain visible without flattening all size differences.
  plot_data <- plot_data %>%
    dplyr::mutate(
      population_size_index = dplyr::if_else(
        !is.na(population) & population > 0,
        pmax(population, 1e6)^0.35,
        NA_real_
      )
    )
  visible_plot_data <- plot_data %>%
    dplyr::filter(
      !is.na(plot_indicator_value),
      plot_indicator_value >= min(y_limits, na.rm = TRUE),
      plot_indicator_value <= max(y_limits, na.rm = TRUE)
    )
  label_data <- build_regional_label_data(visible_plot_data)

  chart <- ggplot2::ggplot(plot_data) +
    # Add one simple log-income trend line to communicate the broad association.
    ggplot2::geom_smooth(
      ggplot2::aes(x = gdp_per_capita, y = plot_indicator_value),
      method = "lm",
      formula = y ~ log10(x),
      se = FALSE,
      color = presentation_colors[["ink"]],
      linewidth = 0.45,
      alpha = 0.85
    ) +
    # Size dots by transformed population and use region fill for the main hierarchy.
    ggplot2::geom_point(
      ggplot2::aes(
        x = gdp_per_capita,
        y = plot_indicator_value,
        fill = owid_region,
        size = population_size_index
      ),
      shape = 21,
      color = presentation_colors[["ink"]],
      stroke = 0.28,
      alpha = 0.72
    ) +
    # Label representative large countries by ISO code with leader lines.
    ggrepel::geom_label_repel(
      data = label_data,
      ggplot2::aes(
        x = gdp_per_capita,
        y = plot_indicator_value,
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
      show.legend = FALSE,
      seed = 1234
    ) +
    # Keep the shared GDP-per-capita axis consistent across OWID charts.
    ggplot2::scale_x_log10(
      limits = x_limits,
      breaks = x_breaks,
      labels = presentation_dollar_label(accuracy = 0.1),
      expand = ggplot2::expansion(mult = c(0, 0.025))
    ) +
    # Preserve the existing regional color order and legend labels.
    ggplot2::scale_fill_manual(
      values = owid_region_colors,
      breaks = names(owid_region_colors),
      drop = FALSE,
      name = NULL
    ) +
    # Use transformed population breaks so the legend remains interpretable in people.
    ggplot2::scale_size_continuous(
      range = c(1.8, 9.2),
      breaks = c(1e6, 1e7, 1e8, 1e9)^0.35,
      labels = c("1M", "10M", "100M", "1B"),
      name = "Poblaci\u00f3n"
    ) +
    ggplot2::labs(
      title = sprintf("%s (%s)", title, selected_year),
      subtitle = subtitle,
      x = "PIB per cápita, PPP",
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
    ggplot2::theme(legend.position = "bottom") +
    apply_presentation_axis_theme()

  if (isTRUE(y_log)) {
    chart <- chart +
      ggplot2::scale_y_log10(
        limits = y_limits,
        breaks = y_breaks,
        labels = y_labels,
        expand = c(0, 0)
      )
  } else {
    if (isTRUE(y_reverse)) {
      chart <- chart +
        ggplot2::scale_y_reverse(
          limits = y_limits,
          breaks = y_breaks,
          labels = y_labels,
          expand = c(0, 0)
        )
    } else {
      chart <- chart +
        ggplot2::scale_y_continuous(
        limits = y_limits,
        breaks = y_breaks,
        labels = y_labels,
        expand = c(0, 0)
      )
    }
  }

  chart
}

final_parts <- list()
figure_files <- character()

# Graph 1: Life expectancy and GDP per capita ----
life_expectancy_data <- read_grapher_indicator(
  indicator_id = "life_expectancy",
  slug = "life-expectancy-vs-gdp-per-capita",
  y_column = "life_expectancy_0",
  x_column = "gdp_per_capita",
  population_column = "population_historical"
)
life_expectancy_plot_data <- build_plot_data(life_expectancy_data)
final_parts[["life_expectancy"]] <- life_expectancy_plot_data

# Graph 2: Median daily income and GDP per capita ----
daily_income_data <- read_grapher_indicator(
  indicator_id = "daily_income",
  slug = "median-daily-per-capita-expenditure-vs-gdp-per-capita",
  y_column = "median__ppp_version_2021__welfare_type_income_or_consumption__period_day__table_income_or_consumption_consolidated__survey_comparability_no_spells",
  x_column = "ny_gdp_pcap_pp_kd",
  population_column = "population_historical"
)
daily_income_plot_data <- build_plot_data(
  daily_income_data,
  latest_per_country = TRUE,
  fallback_imf_gdp = TRUE
)
final_parts[["daily_income"]] <- daily_income_plot_data

# Graph 3: Bottom-decile income and GDP per capita ----
p10_income_data <- read_grapher_indicator(
  indicator_id = "p10_income",
  slug = "p10-vs-gdp-per-capita",
  y_column = "thr__ppp_version_2021__welfare_type_income_or_consumption__decile_1__period_day__table_income_or_consumption_consolidated__survey_comparability_no_spells",
  x_column = "ny_gdp_pcap_pp_kd",
  population_column = "population"
)
p10_income_plot_data <- build_plot_data(
  p10_income_data,
  latest_per_country = TRUE,
  fallback_imf_gdp = TRUE
)
final_parts[["p10_income"]] <- p10_income_plot_data

# Graph 4: Child mortality and GDP per capita ----
child_mortality_data <- read_grapher_indicator(
  indicator_id = "child_mortality",
  slug = "child-mortality-gdp-per-capita",
  y_column = "child_mortality_rate",
  x_column = "gdp_per_capita",
  population_column = "population_historical"
)
child_mortality_plot_data <- build_plot_data(child_mortality_data)
final_parts[["child_mortality"]] <- child_mortality_plot_data

# Graph 5: Human Development Index and GDP per capita ----
hdi_data <- read_grapher_indicator(
  indicator_id = "hdi",
  slug = "human-development-index-vs-gdp-per-capita",
  y_column = "hdi__sex_total",
  x_column = "ny_gdp_pcap_pp_kd",
  population_column = "population_historical"
)
hdi_plot_data <- build_plot_data(hdi_data)
final_parts[["hdi"]] <- hdi_plot_data

# Graph 6: Schooling and GDP per capita ----
schooling_data <- read_grapher_indicator(
  indicator_id = "schooling",
  slug = "average-years-of-schooling-vs-gdp-per-capita",
  y_column = "mys__sex_total",
  x_column = "ny_gdp_pcap_pp_kd",
  population_column = "population_historical"
)
schooling_plot_data <- build_plot_data(schooling_data)
final_parts[["schooling"]] <- schooling_plot_data

# Graph 7: Learning outcomes and GDP per capita ----
learning_outcomes_data <- read_grapher_indicator(
  indicator_id = "learning_outcomes",
  slug = "learning-outcomes-vs-gdp-per-capita",
  y_column = "harmonized_test_scores__sex_all_students",
  x_column = "ny_gdp_pcap_pp_kd",
  population_column = "population_historical"
)
learning_outcomes_plot_data <- build_plot_data(learning_outcomes_data)
final_parts[["learning_outcomes"]] <- learning_outcomes_plot_data

# Graph 8: Energy use and GDP per capita ----
energy_use_data <- read_grapher_indicator(
  indicator_id = "energy_use",
  slug = "energy-use-per-person-vs-gdp-per-capita",
  y_column = "primary_energy_consumption_per_capita__kwh",
  x_column = "ny_gdp_pcap_pp_kd"
)
energy_use_plot_data <- build_plot_data(energy_use_data)
final_parts[["energy_use"]] <- energy_use_plot_data

# Shared OWID x-axis after all regular indicators are prepared ----
owid_region_lookup <- dplyr::bind_rows(final_parts) %>%
  dplyr::filter(!is.na(owid_region)) %>%
  dplyr::select(country_code, owid_region) %>%
  dplyr::distinct(country_code, .keep_all = TRUE)

# Graph 9: Democracy and GDP per capita ----
democracy_data <- read_democracy_indicator()
democracy_selected_year <- min(
  max(democracy_data$year, na.rm = TRUE),
  max(imf_gdp$year, na.rm = TRUE)
)
democracy_plot_data <- democracy_data %>%
  dplyr::filter(year == democracy_selected_year) %>%
  dplyr::left_join(
    imf_gdp %>%
      dplyr::filter(year == democracy_selected_year) %>%
      dplyr::select(country_code, gdp_per_capita, gdp_population),
    by = "country_code"
  ) %>%
  dplyr::left_join(owid_region_lookup, by = "country_code") %>%
  dplyr::mutate(
    population = gdp_population,
    gdp_source = "IMF WEO PPP GDP per capita matched to 2024",
    owid_region = factor(owid_region, levels = names(owid_region_colors))
  ) %>%
  dplyr::filter(!is.na(gdp_per_capita), gdp_per_capita > 0, !is.na(owid_region))
final_parts[["democracy"]] <- democracy_plot_data

# Graph 10: Economic complexity and GDP per capita ----
eci_data <- read_eci_indicator()
eci_selected_year <- min(
  max(eci_data$year, na.rm = TRUE),
  max(imf_gdp$year, na.rm = TRUE)
)
eci_plot_data <- eci_data %>%
  dplyr::filter(year == eci_selected_year) %>%
  dplyr::left_join(
    imf_gdp %>%
      dplyr::filter(year == eci_selected_year) %>%
      dplyr::select(country_code, country, gdp_per_capita, gdp_population),
    by = "country_code"
  ) %>%
  dplyr::left_join(owid_region_lookup, by = "country_code") %>%
  dplyr::mutate(
    country = dplyr::coalesce(country, country_code),
    population = gdp_population,
    gdp_source = "IMF WEO PPP GDP per capita matched to 2024",
    owid_region = factor(owid_region, levels = names(owid_region_colors))
  ) %>%
  dplyr::filter(
    !is.na(gdp_per_capita),
    gdp_per_capita > 0,
    !is.na(indicator_value),
    !is.na(owid_region)
  )
final_parts[["economic_complexity"]] <- eci_plot_data

# Shared OWID x-axis after all graph data are available ----
owid_x_break_candidates <- c(
  200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000, 500000
)
owid_x_limits <- nice_log10_limits(
  unlist(lapply(final_parts, function(part) part$gdp_per_capita)),
  owid_x_break_candidates
)
owid_x_breaks <- owid_x_break_candidates
owid_x_breaks <- owid_x_breaks[owid_x_breaks >= owid_x_limits[[1]] & owid_x_breaks <= owid_x_limits[[2]]]

# Save and preview Graph 1 ----
life_expectancy_plot <- build_relationship_chart(
  life_expectancy_plot_data,
  title = "Esperanza de vida y PIB per capita",
  subtitle = "Cada punto compara longevidad e ingreso per capita entre paises.",
  y_label = "Esperanza de vida al nacer",
  y_limits = c(40, 90),
  y_breaks = c(40, 50, 60, 70, 80, 90),
  x_limits = owid_x_limits,
  x_breaks = owid_x_breaks
)
figure_files <- c(figure_files, file.path(figure_dir, "owid_life_expectancy_gdp_per_capita.png"))
save_and_preview_plot(
  figure_files[[length(figure_files)]],
  life_expectancy_plot,
  presentation_source_caption,
  note = presentation_axis_note(log_x = TRUE)
)

# Save and preview Graph 2 ----
daily_income_plot <- build_relationship_chart(
  daily_income_plot_data,
  title = "Ingreso mediano diario y PIB per capita",
  subtitle = "Cada punto compara ingreso diario de los hogares e ingreso per capita.",
  y_label = "Dólares diarios",
  y_limits = c(1, 100),
  y_breaks = c(1, 2, 5, 10, 20, 50, 100),
  x_limits = owid_x_limits,
  x_breaks = owid_x_breaks,
  y_log = TRUE,
  latest_per_country = TRUE,
  y_dollar = TRUE
)
figure_files <- c(figure_files, file.path(figure_dir, "owid_daily_income_gdp_per_capita.png"))
save_and_preview_plot(
  figure_files[[length(figure_files)]],
  daily_income_plot,
  presentation_source_caption,
  note = presentation_axis_note(log_x = TRUE, log_y = TRUE)
)

# Save and preview Graph 3 ----
p10_income_plot <- build_relationship_chart(
  p10_income_plot_data,
  title = "Ingreso del 10% mas pobre y PIB per capita",
  subtitle = "Cada punto compara ingreso diario del decil inferior e ingreso per capita.",
  y_label = "Dólares diarios",
  y_limits = c(0.2, 50),
  y_breaks = c(0.2, 0.5, 1, 2, 5, 10, 20, 50),
  x_limits = owid_x_limits,
  x_breaks = owid_x_breaks,
  y_log = TRUE,
  latest_per_country = TRUE,
  y_dollar = TRUE
)
figure_files <- c(figure_files, file.path(figure_dir, "owid_p10_income_gdp_per_capita.png"))
save_and_preview_plot(
  figure_files[[length(figure_files)]],
  p10_income_plot,
  presentation_source_caption,
  note = presentation_axis_note(log_x = TRUE, log_y = TRUE)
)

# Save and preview Graph 4 ----
child_mortality_plot <- build_relationship_chart(
  child_mortality_plot_data,
  title = "Mortalidad infantil y PIB per capita",
  subtitle = "Cada punto compara mortalidad infantil e ingreso per capita entre paises.",
  y_label = "Muertes menores de 5 años por 100 nacidos vivos",
  y_limits = c(0.1, 100),
  y_breaks = c(0.1, 0.5, 1, 2, 5, 10, 20, 50, 100),
  x_limits = owid_x_limits,
  x_breaks = owid_x_breaks,
  y_log = TRUE,
  y_reverse = TRUE
)
figure_files <- c(figure_files, file.path(figure_dir, "owid_child_mortality_gdp_per_capita.png"))
save_and_preview_plot(
  figure_files[[length(figure_files)]],
  child_mortality_plot,
  presentation_source_caption,
  note = presentation_axis_note(log_x = TRUE)
)

# Save and preview Graph 5 ----
hdi_plot <- build_relationship_chart(
  hdi_plot_data,
  title = "IDH y PIB per capita",
  subtitle = "Cada punto compara desarrollo humano e ingreso per capita entre paises.",
  y_label = "Indice de Desarrollo Humano (IDH)",
  y_limits = c(0.20, 1),
  y_breaks = c(0.20, seq(0.2, 1, 0.1)),
  x_limits = owid_x_limits,
  x_breaks = owid_x_breaks
)
figure_files <- c(figure_files, file.path(figure_dir, "owid_hdi_gdp_per_capita.png"))
save_and_preview_plot(
  figure_files[[length(figure_files)]],
  hdi_plot,
  presentation_source_caption,
  note = presentation_axis_note(log_x = TRUE)
)

# Save and preview Graph 6 ----
schooling_plot <- build_relationship_chart(
  schooling_plot_data,
  title = "Escolaridad y PIB per capita",
  subtitle = "Cada punto compara escolaridad promedio e ingreso per capita entre paises.",
  y_label = "Años promedio de escolaridad",
  y_limits = c(0, 18),
  y_breaks = c(0, 3, 6, 9, 12, 15, 18),
  x_limits = owid_x_limits,
  x_breaks = owid_x_breaks
)
figure_files <- c(figure_files, file.path(figure_dir, "owid_schooling_gdp_per_capita.png"))
save_and_preview_plot(
  figure_files[[length(figure_files)]],
  schooling_plot,
  presentation_source_caption,
  note = presentation_axis_note(log_x = TRUE)
)

# Save and preview Graph 7 ----
learning_outcomes_plot <- build_relationship_chart(
  learning_outcomes_plot_data,
  title = "Resultados de aprendizaje y PIB per capita",
  subtitle = "Cada punto compara puntajes armonizados de pruebas estandarizadas e ingreso per capita.",
  y_label = "Puntaje armonizado de aprendizaje",
  y_limits = c(250, 600),
  y_breaks = seq(250, 600, by = 50),
  x_limits = owid_x_limits,
  x_breaks = owid_x_breaks
)
figure_files <- c(figure_files, file.path(figure_dir, "owid_learning_outcomes_gdp_per_capita.png"))
save_and_preview_plot(
  figure_files[[length(figure_files)]],
  learning_outcomes_plot,
  presentation_source_caption,
  note = presentation_axis_note(log_x = TRUE)
)

# Save and preview Graph 8 ----
energy_use_plot <- build_relationship_chart(
  energy_use_plot_data,
  title = "Uso de energia y PIB per capita",
  subtitle = "Cada punto compara consumo energetico individual e ingreso per capita entre paises.",
  y_label = "Energia primaria per capita, kWh",
  y_limits = c(100, 300000),
  y_breaks = c(100, 300, 1000, 3000, 10000, 30000, 100000, 300000),
  x_limits = owid_x_limits,
  x_breaks = owid_x_breaks,
  y_log = TRUE
)
figure_files <- c(figure_files, file.path(figure_dir, "owid_energy_use_gdp_per_capita.png"))
save_and_preview_plot(
  figure_files[[length(figure_files)]],
  energy_use_plot,
  presentation_source_caption,
  note = presentation_axis_note(log_x = TRUE, log_y = TRUE)
)

# Save and preview Graph 9 ----
democracy_plot <- build_relationship_chart(
  democracy_plot_data,
  title = "Democracia y PIB per capita",
  subtitle = "Cada punto compara calidad democratica e ingreso per capita entre paises.",
  y_label = "Democracy Index (0-10)",
  y_limits = c(0.1, 10),
  y_breaks = c(0.1, 0.5, 1, 2, 3, 5, 7, 10),
  x_limits = owid_x_limits,
  x_breaks = owid_x_breaks,
  y_log = TRUE
)
figure_files <- c(figure_files, file.path(figure_dir, "owid_democracy_gdp_per_capita.png"))
save_and_preview_plot(
  figure_files[[length(figure_files)]],
  democracy_plot,
  presentation_source_caption,
  note = presentation_axis_note(log_x = TRUE, log_y = TRUE)
)

# Save and preview Graph 10 ----
economic_complexity_plot <- build_relationship_chart(
  eci_plot_data,
  title = "Complejidad economica y PIB per capita",
  subtitle = "Cada punto compara sofisticacion productiva e ingreso per capita entre paises.",
  y_label = "Indice de complejidad economica",
  y_limits = c(-3.0, 3.0),
  y_breaks = seq(-3.0, 3.0, by = 1.0),
  x_limits = owid_x_limits,
  x_breaks = owid_x_breaks
)
figure_files <- c(figure_files, file.path(figure_dir, "economic_complexity_gdp_per_capita.png"))
save_and_preview_plot(
  figure_files[[length(figure_files)]],
  economic_complexity_plot,
  eci_source_caption,
  note = presentation_axis_note(log_x = TRUE)
)
final_data <- dplyr::bind_rows(final_parts)

## Data outputs ----
# Persist the combined OWID relationship table for review.
utils::write.csv(final_data, "data/final/owid_development_relationships.csv", row.names = FALSE)

# Keep the previous single-indicator path for compatibility with earlier deck builds.
utils::write.csv(
  final_parts[["life_expectancy"]],
  "data/final/owid_life_expectancy_gdp_per_capita.csv",
  row.names = FALSE
)

for (figure_file in figure_files) {
  message("Wrote ", figure_file)
}
message("Wrote data/final/owid_development_relationships.csv")
