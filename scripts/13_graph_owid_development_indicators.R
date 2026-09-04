# -*- coding: UTF-8 -*-
# Replicate selected OWID development indicator relationships for the presentation.

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

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/final", recursive = TRUE, showWarnings = FALSE)
figure_dir <- "outputs/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Plot constants ----
# Shared caption, country groups, and OWID region colors.
presentation_source_caption <- build_source_caption(
  "Our World in Data Grapher (2024-2025) y FMI WEO (2025)",
  calculations = TRUE
)
eci_source_caption <- build_source_caption(
  "Growth Lab Atlas of Economic Complexity (2024) y FMI WEO (2025)",
  calculations = TRUE
)
maddison_ppp_unit_note <- "PIB per cápita PPP de OWID expresado en dólares internacionales constantes de 2011."
world_bank_ppp_unit_note <- "PIB per cápita PPP de OWID expresado en dólares internacionales constantes de 2021."
imf_ppp_unit_note <- "PIB per cápita PPP del FMI WEO expresado en dólares internacionales corrientes."

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
  "África" = presentation_palette[["yellow"]],
  "Asia" = presentation_palette[["red"]],
  "Europa" = presentation_palette[["navy"]],
  "América del Norte" = presentation_palette[["cyan"]],
  "Oceanía" = presentation_palette[["orange"]],
  "América del Sur" = presentation_palette[["green"]]
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
  case_when(
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

# Dependency data ----
# Read IMF GDP per cápita data used as the common x-axis.
imf_gdp <- utils::read.csv(imf_gdp_path, stringsAsFactors = FALSE) %>%
  transmute(
    country_code = country_code,
    country = country,
    year = as.integer(year),
    gdp_per_capita = as.numeric(gdp_per_capita_ppp_current_intl_dollars),
    gdp_population = as.numeric(population_millions) * 1e6
  ) %>%
  filter(!is.na(gdp_per_capita), gdp_per_capita > 0)

# Latest IMF population values backfill OWID indicators without a population field.
imf_population_lookup <- imf_gdp %>%
  filter(!is.na(gdp_population), gdp_population > 0) %>%
  arrange(country_code, desc(year)) %>%
  group_by(country_code) %>%
  slice(1) %>%
  ungroup() %>%
  select(country_code, imf_population = gdp_population)

# Use IMF WEO PPP GDP per capita for Venezuela in the same year as the OWID indicator point.
venezuela_imf_gdp_by_year <- imf_gdp %>%
  filter(country_code == "VEN") %>%
  transmute(
    country_code,
    year,
    venezuela_imf_gdp_per_capita = gdp_per_capita,
    venezuela_imf_population = gdp_population
  )

owid_x_limits <- range(imf_gdp$gdp_per_capita, na.rm = TRUE)
owid_x_breaks <- c(500, 1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000)
owid_x_breaks <- owid_x_breaks[owid_x_breaks >= owid_x_limits[[1]] & owid_x_breaks <= owid_x_limits[[2]]]

# Data helpers ----
# Read and normalize one cached OWID Grapher indicator.
read_grapher_indicator <- function(indicator_id, slug, y_column, x_column, population_column) {
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
    mutate(.mapped_owid_region = region_values) %>%
    transmute(
      indicator_id = indicator_id,
      country = entity,
      country_code = code,
      year = as.integer(year),
      indicator_value = as.numeric(.data[[y_column]]),
      gdp_per_capita = as.numeric(.data[[x_column]]),
      population = if (is.null(population_column)) NA_real_ else as.numeric(.data[[population_column]]),
      owid_region = .mapped_owid_region
    ) %>%
    filter(
      !is.na(country_code),
      nchar(country_code) == 3,
      !is.na(indicator_value),
      is.na(gdp_per_capita) | gdp_per_capita > 0
    )
}

# Select the year or latest available observation used for each indicator.
build_plot_data <- function(
  indicator_data,
  selected_year,
  latest_per_country,
  fallback_imf_gdp
) {
  selected_year <- if (!is.null(selected_year)) {
    as.integer(selected_year)
  } else if (isTRUE(latest_per_country)) {
    NA_integer_
  } else {
    max(indicator_data$year[!is.na(indicator_data$gdp_per_capita)], na.rm = TRUE)
  }

  selected_data <- if (isTRUE(latest_per_country)) {
    latest_data <- indicator_data %>%
      arrange(country_code, desc(year)) %>%
      group_by(country_code) %>%
      slice(1) %>%
      ungroup()

    if (isTRUE(fallback_imf_gdp)) {
      latest_data %>%
        left_join(
          imf_gdp %>%
            select(country_code, year, imf_gdp_per_capita = gdp_per_capita),
          by = c("country_code", "year")
        ) %>%
        mutate(gdp_per_capita = coalesce(gdp_per_capita, imf_gdp_per_capita)) %>%
        select(-imf_gdp_per_capita)
    } else {
      latest_data
    }
  } else {
    indicator_data %>%
      filter(year == selected_year)
  }

  selected_data %>%
    left_join(imf_population_lookup, by = "country_code") %>%
    left_join(venezuela_imf_gdp_by_year, by = c("country_code", "year")) %>%
    mutate(
      use_imf_venezuela_gdp = country_code == "VEN" &
        !is.na(venezuela_imf_gdp_per_capita),
      gdp_per_capita = if_else(use_imf_venezuela_gdp, venezuela_imf_gdp_per_capita, gdp_per_capita),
      population = coalesce(population, imf_population),
      population = if_else(
        country_code == "VEN" & !is.na(venezuela_imf_population),
        venezuela_imf_population,
        population
      ),
      gdp_source = if_else(
        use_imf_venezuela_gdp,
        sprintf("IMF WEO PPP GDP per capita matched to %s", year),
        "OWID Grapher"
      ),
      owid_region = factor(owid_region, levels = names(owid_region_colors))
    ) %>%
    select(-imf_population, -venezuela_imf_gdp_per_capita, -venezuela_imf_population, -use_imf_venezuela_gdp) %>%
    filter(!is.na(gdp_per_capita), gdp_per_capita > 0)
}

## Indicator data: Life expectancy ----
life_expectancy_data <- read_grapher_indicator(
  indicator_id = "life_expectancy",
  slug = "life-expectancy-vs-gdp-per-capita",
  y_column = "life_expectancy_0",
  x_column = "gdp_per_capita",
  population_column = "population_historical"
)
life_expectancy_plot_data <- build_plot_data(
  life_expectancy_data,
  selected_year = NULL,
  latest_per_country = FALSE,
  fallback_imf_gdp = FALSE
)

## Indicator data: Median daily income ----
daily_income_data <- read_grapher_indicator(
  indicator_id = "daily_income",
  slug = "median-daily-per-capita-expenditure-vs-gdp-per-capita",
  y_column = "median__ppp_version_2021__welfare_type_income_or_consumption__period_day__table_income_or_consumption_consolidated__survey_comparability_no_spells",
  x_column = "ny_gdp_pcap_pp_kd",
  population_column = "population_historical"
)
daily_income_plot_data <- build_plot_data(
  daily_income_data,
  selected_year = NULL,
  latest_per_country = TRUE,
  fallback_imf_gdp = TRUE
)

## Indicator data: Bottom-decile income ----
p10_income_data <- read_grapher_indicator(
  indicator_id = "p10_income",
  slug = "p10-vs-gdp-per-capita",
  y_column = "thr__ppp_version_2021__welfare_type_income_or_consumption__decile_1__period_day__table_income_or_consumption_consolidated__survey_comparability_no_spells",
  x_column = "ny_gdp_pcap_pp_kd",
  population_column = "population"
)
p10_income_plot_data <- build_plot_data(
  p10_income_data,
  selected_year = NULL,
  latest_per_country = TRUE,
  fallback_imf_gdp = TRUE
)

## Indicator data: Child mortality ----
child_mortality_data <- read_grapher_indicator(
  indicator_id = "child_mortality",
  slug = "child-mortality-gdp-per-capita",
  y_column = "child_mortality_rate",
  x_column = "gdp_per_capita",
  population_column = "population_historical"
)
child_mortality_plot_data <- build_plot_data(
  child_mortality_data,
  selected_year = NULL,
  latest_per_country = FALSE,
  fallback_imf_gdp = FALSE
)

## Indicator data: Human Development Index ----
hdi_data <- read_grapher_indicator(
  indicator_id = "hdi",
  slug = "human-development-index-vs-gdp-per-capita",
  y_column = "hdi__sex_total",
  x_column = "ny_gdp_pcap_pp_kd",
  population_column = "population_historical"
)
hdi_plot_data <- build_plot_data(
  hdi_data,
  selected_year = NULL,
  latest_per_country = FALSE,
  fallback_imf_gdp = FALSE
)

## Indicator data: Schooling ----
schooling_data <- read_grapher_indicator(
  indicator_id = "schooling",
  slug = "average-years-of-schooling-vs-gdp-per-capita",
  y_column = "mys__sex_total",
  x_column = "ny_gdp_pcap_pp_kd",
  population_column = "population_historical"
)
schooling_plot_data <- build_plot_data(
  schooling_data,
  selected_year = NULL,
  latest_per_country = FALSE,
  fallback_imf_gdp = FALSE
)

## Indicator data: Learning outcomes ----
learning_outcomes_data <- read_grapher_indicator(
  indicator_id = "learning_outcomes",
  slug = "learning-outcomes-vs-gdp-per-capita",
  y_column = "harmonized_test_scores__sex_all_students",
  x_column = "ny_gdp_pcap_pp_kd",
  population_column = "population_historical"
)
learning_outcomes_plot_data <- build_plot_data(
  learning_outcomes_data,
  selected_year = NULL,
  latest_per_country = FALSE,
  fallback_imf_gdp = FALSE
)

## Indicator data: Energy use ----
energy_use_data <- read_grapher_indicator(
  indicator_id = "energy_use",
  slug = "energy-use-per-person-vs-gdp-per-capita",
  y_column = "primary_energy_consumption_per_capita__kwh",
  x_column = "ny_gdp_pcap_pp_kd",
  population_column = NULL
)
energy_use_plot_data <- build_plot_data(
  energy_use_data,
  selected_year = NULL,
  latest_per_country = FALSE,
  fallback_imf_gdp = FALSE
)

## Shared x-axis: Regular indicators ----
# Build the common GDP axis after preparing the regular OWID indicators.
owid_regular_plot_data <- bind_rows(
  life_expectancy_plot_data,
  daily_income_plot_data,
  p10_income_plot_data,
  child_mortality_plot_data,
  hdi_plot_data,
  schooling_plot_data,
  learning_outcomes_plot_data,
  energy_use_plot_data
)

owid_region_lookup <- owid_regular_plot_data %>%
  filter(!is.na(owid_region)) %>%
  select(country_code, owid_region) %>%
  distinct(country_code, .keep_all = TRUE)

## Indicator data: Democracy ----
democracy_path <- "data/raw/owid_democracy_index.csv"
if (!file.exists(democracy_path)) {
  stop(
    sprintf(
      "Missing `%s`. Run scripts/04_download_owid_development_data.R once before graphing.",
      democracy_path
    ),
    call. = FALSE
  )
}

democracy_data <- utils::read.csv(democracy_path, stringsAsFactors = FALSE)
democracy_selected_year <- min(
  max(democracy_data$year, na.rm = TRUE),
  max(imf_gdp$year, na.rm = TRUE)
)
democracy_plot_data <- democracy_data %>%
  filter(year == democracy_selected_year) %>%
  left_join(
    imf_gdp %>%
      filter(year == democracy_selected_year) %>%
      select(country_code, gdp_per_capita, gdp_population),
    by = "country_code"
  ) %>%
  left_join(owid_region_lookup, by = "country_code") %>%
  mutate(
    population = gdp_population,
    gdp_source = "IMF WEO PPP GDP per capita matched to 2024",
    owid_region = factor(owid_region, levels = names(owid_region_colors))
  ) %>%
  filter(!is.na(gdp_per_capita), gdp_per_capita > 0, !is.na(owid_region))

## Indicator data: Economic complexity ----
eci_path <- "data/raw/growth_proj_eci_rankings.csv"
if (!file.exists(eci_path)) {
  stop(sprintf("Missing `%s`.", eci_path), call. = FALSE)
}

eci_data <- utils::read.csv(eci_path, stringsAsFactors = FALSE) %>%
  transmute(
    indicator_id = "economic_complexity",
    country_code = country_iso3_code,
    year = as.integer(year),
    indicator_value = coalesce(
      as.numeric(eci_hs12),
      as.numeric(eci_hs92),
      as.numeric(eci_sitc)
    )
  ) %>%
  filter(!is.na(country_code), nchar(country_code) == 3, !is.na(indicator_value))
eci_selected_year <- min(
  max(eci_data$year, na.rm = TRUE),
  max(imf_gdp$year, na.rm = TRUE)
)
eci_plot_data <- eci_data %>%
  filter(year == eci_selected_year) %>%
  left_join(
    imf_gdp %>%
      filter(year == eci_selected_year) %>%
      select(country_code, country, gdp_per_capita, gdp_population),
    by = "country_code"
  ) %>%
  left_join(owid_region_lookup, by = "country_code") %>%
  mutate(
    country = coalesce(country, country_code),
    population = gdp_population,
    gdp_source = "IMF WEO PPP GDP per capita matched to 2024",
    owid_region = factor(owid_region, levels = names(owid_region_colors))
  ) %>%
  filter(
    !is.na(gdp_per_capita),
    gdp_per_capita > 0,
    !is.na(indicator_value),
    !is.na(owid_region)
  )

## Shared x-axis: All indicators ----
# Finalize common GDP limits after adding democracy and economic complexity.
final_data <- bind_rows(
  owid_regular_plot_data,
  democracy_plot_data,
  eci_plot_data
)
owid_x_break_candidates <- c(
  200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000, 500000
)
owid_positive_gdp <- final_data$gdp_per_capita[
  !is.na(final_data$gdp_per_capita) & final_data$gdp_per_capita > 0
]
if (length(owid_positive_gdp) == 0) {
  stop("OWID graph data must contain positive GDP per capita values.", call. = FALSE)
}

owid_x_limits <- c(
  max(owid_x_break_candidates[owid_x_break_candidates <= min(owid_positive_gdp)]),
  min(owid_x_break_candidates[owid_x_break_candidates >= max(owid_positive_gdp)])
)
owid_x_breaks <- owid_x_break_candidates
owid_x_breaks <- owid_x_breaks[owid_x_breaks >= owid_x_limits[[1]] & owid_x_breaks <= owid_x_limits[[2]]]

# Graphs ----
# Each graph block contains its data preparation, plot definition, formatting, and export.
## Family: OWID development indicator relationships ----

### Graph 01: Esperanza de vida y PIB per cápita ----
# Prepare the observations, population sizing, and labels used only by this graph.
life_expectancy_data <- life_expectancy_plot_data %>%
  filter(
    gdp_per_capita >= owid_x_limits[[1]],
    gdp_per_capita <= owid_x_limits[[2]]
  ) %>%
  mutate(
    plot_indicator_value = indicator_value,
    population_size_index = if_else(
      !is.na(population) & population > 0,
      pmax(population, 1000000)^0.35,
      NA_real_
    )
  )

life_expectancy_label_data <- life_expectancy_data %>%
  filter(
    !is.na(plot_indicator_value),
    plot_indicator_value >= min(c(40, 90)),
    plot_indicator_value <= max(c(40, 90)),
    !is.na(owid_region),
    owid_region != "Oceanía",
    !is.na(population),
    population > 0
  ) %>%
  arrange(owid_region, desc(population), country_code) %>%
  group_by(owid_region) %>%
  slice_head(n = 5L) %>%
  ungroup() %>%
  bind_rows(life_expectancy_data %>% filter(country_code == "VEN")) %>%
  distinct(country_code, .keep_all = TRUE) %>%
  mutate(country_label = country_code)

life_expectancy_year_label <- unique(life_expectancy_data$year)

# Build the complete graph with all graph-specific scales directly visible.
life_expectancy_plot <- life_expectancy_data %>%
  ggplot() +
  geom_smooth(
    aes(x = gdp_per_capita, y = plot_indicator_value),
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = presentation_colors[["ink"]],
    linewidth = 0.45,
    alpha = 0.85
  ) +
  geom_point(
    aes(
      x = gdp_per_capita,
      y = plot_indicator_value,
      fill = owid_region,
      size = population_size_index
    ),
    shape = 21,
    color = presentation_colors[["ink"]],
    stroke = presentation_point_stroke,
    alpha = 0.72
  ) +
  geom_label_repel(
    data = life_expectancy_label_data,
    aes(
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
  scale_x_log10(
    limits = owid_x_limits,
    breaks = owid_x_breaks,
    labels = presentation_number_label(accuracy = 1),
    expand = expansion(mult = c(0, 0.025))
  ) +
  scale_y_continuous(
    limits = c(40, 90),
    breaks = c(40, 50, 60, 70, 80, 90),
    labels = presentation_number_label(accuracy = 0.1)(c(40, 50, 60, 70, 80, 90)),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = owid_region_colors,
    breaks = names(owid_region_colors),
    drop = FALSE,
    name = NULL
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1000000, 10000000, 100000000, 1000000000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  labs(
    title = sprintf("Esperanza de vida y PIB per cápita (%s)", life_expectancy_year_label),
    subtitle = "Comparación de longevidad y producto por habitante.",
    x = "PIBpc PPP",
    y = "Esperanza de vida al nacer (Años)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = TRUE, log_y = FALSE, extra = maddison_ppp_unit_note))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  ) +
  theme(legend.position = "bottom") +
  apply_presentation_axis_theme()

# Apply universal presentation formatting, save, and preview the final object.
life_expectancy_plot <- apply_presentation_plot_style(life_expectancy_plot)

life_expectancy_figure_file <- file.path(figure_dir, "owid_life_expectancy_gdp_per_capita.png")
save_plot_variants(
  filename = life_expectancy_figure_file,
  plot = life_expectancy_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(life_expectancy_plot)
message("Wrote ", life_expectancy_figure_file)


### Graph 02: Ingreso mediano diario y PIB per cápita ----
# Prepare the observations, population sizing, and labels used only by this graph.
daily_income_data <- daily_income_plot_data %>%
  filter(
    gdp_per_capita >= owid_x_limits[[1]],
    gdp_per_capita <= owid_x_limits[[2]],
    indicator_value > 0
  ) %>%
  mutate(
    plot_indicator_value = indicator_value,
    population_size_index = if_else(
      !is.na(population) & population > 0,
      pmax(population, 1000000)^0.35,
      NA_real_
    )
  )

daily_income_label_data <- daily_income_data %>%
  filter(
    !is.na(plot_indicator_value),
    plot_indicator_value >= min(c(1, 100)),
    plot_indicator_value <= max(c(1, 100)),
    !is.na(owid_region),
    owid_region != "Oceanía",
    !is.na(population),
    population > 0
  ) %>%
  arrange(owid_region, desc(population), country_code) %>%
  group_by(owid_region) %>%
  slice_head(n = 5L) %>%
  ungroup() %>%
  bind_rows(daily_income_data %>% filter(country_code == "VEN")) %>%
  distinct(country_code, .keep_all = TRUE) %>%
  mutate(country_label = country_code)

daily_income_year_label <- sprintf("último dato disponible, %s-%s", min(daily_income_data$year, na.rm = TRUE), max(daily_income_data$year, na.rm = TRUE))

# Build the complete graph with all graph-specific scales directly visible.
daily_income_plot <- daily_income_data %>%
  ggplot() +
  geom_smooth(
    aes(x = gdp_per_capita, y = plot_indicator_value),
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = presentation_colors[["ink"]],
    linewidth = 0.45,
    alpha = 0.85
  ) +
  geom_point(
    aes(
      x = gdp_per_capita,
      y = plot_indicator_value,
      fill = owid_region,
      size = population_size_index
    ),
    shape = 21,
    color = presentation_colors[["ink"]],
    stroke = presentation_point_stroke,
    alpha = 0.72
  ) +
  geom_label_repel(
    data = daily_income_label_data,
    aes(
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
  scale_x_log10(
    limits = owid_x_limits,
    breaks = owid_x_breaks,
    labels = presentation_number_label(accuracy = 1),
    expand = expansion(mult = c(0, 0.025))
  ) +
  scale_y_log10(
    limits = c(1, 100),
    breaks = c(1, 2, 5, 10, 20, 50, 100),
    labels = presentation_dollar_label(accuracy = 0.1)(c(1, 2, 5, 10, 20, 50, 100)),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = owid_region_colors,
    breaks = names(owid_region_colors),
    drop = FALSE,
    name = NULL
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1000000, 10000000, 100000000, 1000000000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  labs(
    title = sprintf("Ingreso mediano diario y PIB per cápita (%s)", daily_income_year_label),
    subtitle = "Comparación de ingreso diario de los hogares y producto per cápita.",
    x = "PIBpc PPP",
    y = "Dólares diarios",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = TRUE, log_y = TRUE, extra = world_bank_ppp_unit_note))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  ) +
  theme(legend.position = "bottom") +
  apply_presentation_axis_theme()

# Apply universal presentation formatting, save, and preview the final object.
daily_income_plot <- apply_presentation_plot_style(daily_income_plot)

daily_income_figure_file <- file.path(figure_dir, "owid_daily_income_gdp_per_capita.png")
save_plot_variants(
  filename = daily_income_figure_file,
  plot = daily_income_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(daily_income_plot)
message("Wrote ", daily_income_figure_file)


### Graph 03: Ingreso del 10% más pobre y PIB per cápita ----
# Prepare the observations, population sizing, and labels used only by this graph.
p10_income_data <- p10_income_plot_data %>%
  filter(
    gdp_per_capita >= owid_x_limits[[1]],
    gdp_per_capita <= owid_x_limits[[2]],
    indicator_value > 0
  ) %>%
  mutate(
    plot_indicator_value = indicator_value,
    population_size_index = if_else(
      !is.na(population) & population > 0,
      pmax(population, 1000000)^0.35,
      NA_real_
    )
  )

p10_income_label_data <- p10_income_data %>%
  filter(
    !is.na(plot_indicator_value),
    plot_indicator_value >= min(c(0.2, 50)),
    plot_indicator_value <= max(c(0.2, 50)),
    !is.na(owid_region),
    owid_region != "Oceanía",
    !is.na(population),
    population > 0
  ) %>%
  arrange(owid_region, desc(population), country_code) %>%
  group_by(owid_region) %>%
  slice_head(n = 5L) %>%
  ungroup() %>%
  bind_rows(p10_income_data %>% filter(country_code == "VEN")) %>%
  distinct(country_code, .keep_all = TRUE) %>%
  mutate(country_label = country_code)

p10_income_year_label <- sprintf("último dato disponible, %s-%s", min(p10_income_data$year, na.rm = TRUE), max(p10_income_data$year, na.rm = TRUE))

# Build the complete graph with all graph-specific scales directly visible.
p10_income_plot <- p10_income_data %>%
  ggplot() +
  geom_smooth(
    aes(x = gdp_per_capita, y = plot_indicator_value),
    method = "lm",
    formula = y ~ log10(x),
    se = FALSE,
    color = presentation_colors[["ink"]],
    linewidth = 0.45,
    alpha = 0.85
  ) +
  geom_point(
    aes(
      x = gdp_per_capita,
      y = plot_indicator_value,
      fill = owid_region,
      size = population_size_index
    ),
    shape = 21,
    color = presentation_colors[["ink"]],
    stroke = presentation_point_stroke,
    alpha = 0.72
  ) +
  geom_label_repel(
    data = p10_income_label_data,
    aes(
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
  scale_x_log10(
    limits = owid_x_limits,
    breaks = owid_x_breaks,
    labels = presentation_number_label(accuracy = 1),
    expand = expansion(mult = c(0, 0.025))
  ) +
  scale_y_log10(
    limits = c(0.2, 50),
    breaks = c(0.2, 0.5, 1, 2, 5, 10, 20, 50),
    labels = presentation_dollar_label(accuracy = 0.1)(c(0.2, 0.5, 1, 2, 5, 10, 20, 50)),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = owid_region_colors,
    breaks = names(owid_region_colors),
    drop = FALSE,
    name = NULL
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1000000, 10000000, 100000000, 1000000000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  labs(
    title = sprintf("Ingreso del 10%% más pobre y PIB per cápita (%s)", p10_income_year_label),
    subtitle = "Comparación ingreso diario correspondiente al decil inferior y producto per habitante.",
    x = "PIBpc PPP",
    y = "Dólares diarios",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = TRUE, log_y = TRUE, extra = world_bank_ppp_unit_note))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  ) +
  theme(legend.position = "bottom") +
  apply_presentation_axis_theme()

# Apply universal presentation formatting, save, and preview the final object.
p10_income_plot <- apply_presentation_plot_style(p10_income_plot)

p10_income_figure_file <- file.path(figure_dir, "owid_p10_income_gdp_per_capita.png")
save_plot_variants(
  filename = p10_income_figure_file,
  plot = p10_income_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(p10_income_plot)
message("Wrote ", p10_income_figure_file)


### Graph 04: Supervivencia infantil y PIB per cápita ----
# Prepare the observations, population sizing, and labels used only by this graph.
child_survival_data <- child_mortality_plot_data %>%
  filter(
    gdp_per_capita >= owid_x_limits[[1]],
    gdp_per_capita <= owid_x_limits[[2]],
    !is.na(indicator_value),
    indicator_value >= 0,
    indicator_value <= 100
  ) %>%
  mutate(
    plot_indicator_value = 100 - indicator_value,
    population_size_index = if_else(
      !is.na(population) & population > 0,
      pmax(population, 1000000)^0.35,
      NA_real_
    )
  )

child_survival_label_data <- child_survival_data %>%
  filter(
    !is.na(plot_indicator_value),
    plot_indicator_value >= 60,
    plot_indicator_value <= 100,
    !is.na(owid_region),
    owid_region != "Oceanía",
    !is.na(population),
    population > 0
  ) %>%
  arrange(owid_region, desc(population), country_code) %>%
  group_by(owid_region) %>%
  slice_head(n = 5L) %>%
  ungroup() %>%
  bind_rows(child_survival_data %>% filter(country_code == "VEN")) %>%
  distinct(country_code, .keep_all = TRUE) %>%
  mutate(country_label = country_code)

child_survival_year_label <- unique(child_survival_data$year)

# Use the mortality complement to spread high-survival observations without reversing the relationship.
child_survival_transform <- new_transform(
  name = "child_survival_complement_log10",
  transform = function(x) -log10(pmax(100 - x, 0.1)),
  inverse = function(x) 100 - 10^(-x),
  domain = c(-Inf, 100)
)

# Build the complete graph with all graph-specific scales directly visible.
child_survival_plot <- child_survival_data %>%
  ggplot() +
  geom_smooth(
    aes(x = gdp_per_capita, y = plot_indicator_value),
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    color = presentation_colors[["ink"]],
    linewidth = 0.45,
    alpha = 0.85
  ) +
  geom_point(
    aes(
      x = gdp_per_capita,
      y = plot_indicator_value,
      fill = owid_region,
      size = population_size_index
    ),
    shape = 21,
    color = presentation_colors[["ink"]],
    stroke = presentation_point_stroke,
    alpha = 0.72
  ) +
  geom_label_repel(
    data = child_survival_label_data,
    aes(
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
  scale_x_log10(
    limits = owid_x_limits,
    breaks = owid_x_breaks,
    labels = presentation_number_label(accuracy = 1),
    expand = expansion(mult = c(0, 0.025))
  ) +
  scale_y_continuous(
    transform = child_survival_transform,
    limits = c(50, 100),
    breaks = c(50, 80, 90, 95, 98, 99, 99.5, 99.8),
    labels = presentation_number_label(accuracy = 0.1),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(
    values = owid_region_colors,
    breaks = names(owid_region_colors),
    drop = FALSE,
    name = NULL
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1000000, 10000000, 100000000, 1000000000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  labs(
    title = sprintf("Supervivencia infantil y PIB per cápita (%s)", child_survival_year_label),
    subtitle = "Comnprción de supervivencia hasta los cinco años y producto por habitante.",
    x = "PIBpc PPP",
    y = "Niños sobrevivientes a los 5 años por 100 nacidos",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(
      log_x = TRUE,
      log_y = TRUE,
      extra = maddison_ppp_unit_note
      ))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  ) +
  theme(legend.position = "bottom") +
  apply_presentation_axis_theme()

# Apply universal presentation formatting, save, and preview the final object.
child_survival_plot <- apply_presentation_plot_style(child_survival_plot)

child_survival_figure_file <- file.path(figure_dir, "owid_child_survival_gdp_per_capita.png")
save_plot_variants(
  filename = child_survival_figure_file,
  plot = child_survival_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(child_survival_plot)
message("Wrote ", child_survival_figure_file)


### Graph 05: IDH y PIB per cápita ----
# Prepare the observations, population sizing, and labels used only by this graph.
hdi_data <- hdi_plot_data %>%
  filter(
    gdp_per_capita >= owid_x_limits[[1]],
    gdp_per_capita <= owid_x_limits[[2]]
  ) %>%
  mutate(
    plot_indicator_value = indicator_value,
    population_size_index = if_else(
      !is.na(population) & population > 0,
      pmax(population, 1000000)^0.35,
      NA_real_
    )
  )

hdi_label_data <- hdi_data %>%
  filter(
    !is.na(plot_indicator_value),
    plot_indicator_value >= min(c(0.20, 1)),
    plot_indicator_value <= max(c(0.20, 1)),
    !is.na(owid_region),
    owid_region != "Oceanía",
    !is.na(population),
    population > 0
  ) %>%
  arrange(owid_region, desc(population), country_code) %>%
  group_by(owid_region) %>%
  slice_head(n = 5L) %>%
  ungroup() %>%
  bind_rows(hdi_data %>% filter(country_code == "VEN")) %>%
  distinct(country_code, .keep_all = TRUE) %>%
  mutate(country_label = country_code)

hdi_year_label <- unique(hdi_data$year)

# Build the complete graph with all graph-specific scales directly visible.
hdi_plot <- hdi_data %>%
  ggplot() +
  geom_smooth(
    aes(x = gdp_per_capita, y = plot_indicator_value),
    method = "lm",
    formula = y ~ log10(x),
    se = FALSE,
    color = presentation_colors[["ink"]],
    linewidth = 0.45,
    alpha = 0.85
  ) +
  geom_point(
    aes(
      x = gdp_per_capita,
      y = plot_indicator_value,
      fill = owid_region,
      size = population_size_index
    ),
    shape = 21,
    color = presentation_colors[["ink"]],
    stroke = presentation_point_stroke,
    alpha = 0.72
  ) +
  geom_label_repel(
    data = hdi_label_data,
    aes(
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
  scale_x_log10(
    limits = owid_x_limits,
    breaks = owid_x_breaks,
    labels = presentation_number_label(accuracy = 1),
    expand = expansion(mult = c(0, 0.025))
  ) +
  scale_y_continuous(
    limits = c(0.20, 1),
    breaks = c(0.20, seq(0.2, 1, 0.1)),
    labels = presentation_number_label(accuracy = 0.1)(c(0.20, seq(0.2, 1, 0.1))),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = owid_region_colors,
    breaks = names(owid_region_colors),
    drop = FALSE,
    name = NULL
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1000000, 10000000, 100000000, 1000000000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  labs(
    title = sprintf("Desarrollo humano y PIB per cápita (%s)", hdi_year_label),
    subtitle = "Comparación de índice de desarrollo humano y producto por habitante.",
    x = "PIBpc PPP",
    y = "Índice de Desarrollo Humano (IDH)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = TRUE, log_y = FALSE, extra = world_bank_ppp_unit_note))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  ) +
  theme(legend.position = "bottom") +
  apply_presentation_axis_theme()

# Apply universal presentation formatting, save, and preview the final object.
hdi_plot <- apply_presentation_plot_style(hdi_plot)

hdi_figure_file <- file.path(figure_dir, "owid_hdi_gdp_per_capita.png")
save_plot_variants(
  filename = hdi_figure_file,
  plot = hdi_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(hdi_plot)
message("Wrote ", hdi_figure_file)


### Graph 06: Escolaridad y PIB per cápita ----
# Prepare the observations, population sizing, and labels used only by this graph.
schooling_data <- schooling_plot_data %>%
  filter(
    gdp_per_capita >= owid_x_limits[[1]],
    gdp_per_capita <= owid_x_limits[[2]]
  ) %>%
  mutate(
    plot_indicator_value = indicator_value,
    population_size_index = if_else(
      !is.na(population) & population > 0,
      pmax(population, 1000000)^0.35,
      NA_real_
    )
  )

schooling_label_data <- schooling_data %>%
  filter(
    !is.na(plot_indicator_value),
    plot_indicator_value >= min(c(0, 18)),
    plot_indicator_value <= max(c(0, 18)),
    !is.na(owid_region),
    owid_region != "Oceanía",
    !is.na(population),
    population > 0
  ) %>%
  arrange(owid_region, desc(population), country_code) %>%
  group_by(owid_region) %>%
  slice_head(n = 5L) %>%
  ungroup() %>%
  bind_rows(schooling_data %>% filter(country_code == "VEN")) %>%
  distinct(country_code, .keep_all = TRUE) %>%
  mutate(country_label = country_code)

schooling_year_label <- unique(schooling_data$year)

# Build the complete graph with all graph-specific scales directly visible.
schooling_plot <- schooling_data %>%
  ggplot() +
  geom_smooth(
    aes(x = gdp_per_capita, y = plot_indicator_value),
    method = "lm",
    formula = y ~ log10(x),
    se = FALSE,
    color = presentation_colors[["ink"]],
    linewidth = 0.45,
    alpha = 0.85
  ) +
  geom_point(
    aes(
      x = gdp_per_capita,
      y = plot_indicator_value,
      fill = owid_region,
      size = population_size_index
    ),
    shape = 21,
    color = presentation_colors[["ink"]],
    stroke = presentation_point_stroke,
    alpha = 0.72
  ) +
  geom_label_repel(
    data = schooling_label_data,
    aes(
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
  scale_x_log10(
    limits = owid_x_limits,
    breaks = owid_x_breaks,
    labels = presentation_number_label(accuracy = 1),
    expand = expansion(mult = c(0, 0.025))
  ) +
  scale_y_continuous(
    limits = c(0, 18),
    breaks = c(0, 3, 6, 9, 12, 15, 18),
    labels = presentation_number_label(accuracy = 0.1)(c(0, 3, 6, 9, 12, 15, 18)),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = owid_region_colors,
    breaks = names(owid_region_colors),
    drop = FALSE,
    name = NULL
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1000000, 10000000, 100000000, 1000000000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  labs(
    title = sprintf("Escolaridad y PIB per cápita (%s)", schooling_year_label),
    subtitle = "Comparción de años de escolaridad promedio y producto per cápita.",
    x = "PIBpc PPP",
    y = "Años promedio de escolaridad",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = TRUE, log_y = FALSE, extra = world_bank_ppp_unit_note))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  ) +
  theme(legend.position = "bottom") +
  apply_presentation_axis_theme()

# Apply universal presentation formatting, save, and preview the final object.
schooling_plot <- apply_presentation_plot_style(schooling_plot)

schooling_figure_file <- file.path(figure_dir, "owid_schooling_gdp_per_capita.png")
save_plot_variants(
  filename = schooling_figure_file,
  plot = schooling_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(schooling_plot)
message("Wrote ", schooling_figure_file)


### Graph 07: Resultados de aprendizaje y PIB per cápita ----
# Prepare the observations, population sizing, and labels used only by this graph.
learning_outcomes_data <- learning_outcomes_plot_data %>%
  filter(
    gdp_per_capita >= owid_x_limits[[1]],
    gdp_per_capita <= owid_x_limits[[2]]
  ) %>%
  mutate(
    plot_indicator_value = indicator_value,
    population_size_index = if_else(
      !is.na(population) & population > 0,
      pmax(population, 1000000)^0.35,
      NA_real_
    )
  )

learning_outcomes_label_data <- learning_outcomes_data %>%
  filter(
    !is.na(plot_indicator_value),
    plot_indicator_value >= min(c(250, 600)),
    plot_indicator_value <= max(c(250, 600)),
    !is.na(owid_region),
    owid_region != "Oceanía",
    !is.na(population),
    population > 0
  ) %>%
  arrange(owid_region, desc(population), country_code) %>%
  group_by(owid_region) %>%
  slice_head(n = 5L) %>%
  ungroup() %>%
  bind_rows(learning_outcomes_data %>% filter(country_code == "VEN")) %>%
  distinct(country_code, .keep_all = TRUE) %>%
  mutate(country_label = country_code)

learning_outcomes_year_label <- unique(learning_outcomes_data$year)

# Build the complete graph with all graph-specific scales directly visible.
learning_outcomes_plot <- learning_outcomes_data %>%
  ggplot() +
  geom_smooth(
    aes(x = gdp_per_capita, y = plot_indicator_value),
    method = "lm",
    formula = y ~ log10(x),
    se = FALSE,
    color = presentation_colors[["ink"]],
    linewidth = 0.45,
    alpha = 0.85
  ) +
  geom_point(
    aes(
      x = gdp_per_capita,
      y = plot_indicator_value,
      fill = owid_region,
      size = population_size_index
    ),
    shape = 21,
    color = presentation_colors[["ink"]],
    stroke = presentation_point_stroke,
    alpha = 0.72
  ) +
  geom_label_repel(
    data = learning_outcomes_label_data,
    aes(
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
  scale_x_log10(
    limits = owid_x_limits,
    breaks = owid_x_breaks,
    labels = presentation_number_label(accuracy = 1),
    expand = expansion(mult = c(0, 0.025))
  ) +
  scale_y_continuous(
    limits = c(250, 600),
    breaks = seq(250, 600, by = 50),
    labels = presentation_number_label(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = owid_region_colors,
    breaks = names(owid_region_colors),
    drop = FALSE,
    name = NULL
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1000000, 10000000, 100000000, 1000000000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  labs(
    title = sprintf("Resultados de aprendizaje y PIB per cápita (%s)", learning_outcomes_year_label),
    subtitle = "Comparación de puntajes armonizados en pruebas estandarizadas y producto por habitante.",
    x = "PIBpc PPP",
    y = "Puntaje armonizado de aprendizaje",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = TRUE, log_y = FALSE, extra = world_bank_ppp_unit_note))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  ) +
  theme(legend.position = "bottom") +
  apply_presentation_axis_theme()

# Apply universal presentation formatting, save, and preview the final object.
learning_outcomes_plot <- apply_presentation_plot_style(learning_outcomes_plot)

learning_outcomes_figure_file <- file.path(figure_dir, "owid_learning_outcomes_gdp_per_capita.png")
save_plot_variants(
  filename = learning_outcomes_figure_file,
  plot = learning_outcomes_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(learning_outcomes_plot)
message("Wrote ", learning_outcomes_figure_file)


### Graph 08: Uso de energía y PIB per cápita ----
# Prepare the observations, population sizing, and labels used only by this graph.
energy_use_data <- energy_use_plot_data %>%
  filter(
    gdp_per_capita >= owid_x_limits[[1]],
    gdp_per_capita <= owid_x_limits[[2]],
    indicator_value > 0
  ) %>%
  mutate(
    plot_indicator_value = indicator_value,
    population_size_index = if_else(
      !is.na(population) & population > 0,
      pmax(population, 1000000)^0.35,
      NA_real_
    )
  )

energy_use_label_data <- energy_use_data %>%
  filter(
    !is.na(plot_indicator_value),
    plot_indicator_value >= min(c(100, 300000)),
    plot_indicator_value <= max(c(100, 300000)),
    !is.na(owid_region),
    owid_region != "Oceanía",
    !is.na(population),
    population > 0
  ) %>%
  arrange(owid_region, desc(population), country_code) %>%
  group_by(owid_region) %>%
  slice_head(n = 5L) %>%
  ungroup() %>%
  bind_rows(energy_use_data %>% filter(country_code == "VEN")) %>%
  distinct(country_code, .keep_all = TRUE) %>%
  mutate(country_label = country_code)

energy_use_year_label <- unique(energy_use_data$year)

# Build the complete graph with all graph-specific scales directly visible.
energy_use_plot <- energy_use_data %>%
  ggplot() +
  geom_smooth(
    aes(x = gdp_per_capita, y = plot_indicator_value),
    method = "lm",
    formula = y ~ log10(x),
    se = FALSE,
    color = presentation_colors[["ink"]],
    linewidth = 0.45,
    alpha = 0.85
  ) +
  geom_point(
    aes(
      x = gdp_per_capita,
      y = plot_indicator_value,
      fill = owid_region,
      size = population_size_index
    ),
    shape = 21,
    color = presentation_colors[["ink"]],
    stroke = presentation_point_stroke,
    alpha = 0.72
  ) +
  geom_label_repel(
    data = energy_use_label_data,
    aes(
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
  scale_x_log10(
    limits = owid_x_limits,
    breaks = owid_x_breaks,
    labels = presentation_number_label(accuracy = 1),
    expand = expansion(mult = c(0, 0.025))
  ) +
  scale_y_log10(
    limits = c(100, 300000),
    breaks = c(100, 300, 1000, 3000, 10000, 30000, 100000, 300000),
    labels = presentation_number_label(accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = owid_region_colors,
    breaks = names(owid_region_colors),
    drop = FALSE,
    name = NULL
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1000000, 10000000, 100000000, 1000000000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  labs(
    title = sprintf("Uso de energía y PIB per cápita (%s)", energy_use_year_label),
    subtitle = "Comparación de consumo energético individual y producto por habitante.",
    x = "PIBpc PPP",
    y = "Energía primaria per cápita (kWh)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = TRUE, log_y = TRUE, extra = world_bank_ppp_unit_note))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  ) +
  theme(legend.position = "bottom") +
  apply_presentation_axis_theme()

# Apply universal presentation formatting, save, and preview the final object.
energy_use_plot <- apply_presentation_plot_style(energy_use_plot)

energy_use_figure_file <- file.path(figure_dir, "owid_energy_use_gdp_per_capita.png")
save_plot_variants(
  filename = energy_use_figure_file,
  plot = energy_use_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(energy_use_plot)
message("Wrote ", energy_use_figure_file)


### Graph 09: Democracia y PIB per cápita ----
# Prepare the observations, population sizing, and labels used only by this graph.
democracy_data <- democracy_plot_data %>%
  filter(
    gdp_per_capita >= owid_x_limits[[1]],
    gdp_per_capita <= owid_x_limits[[2]],
    indicator_value > 0
  ) %>%
  mutate(
    plot_indicator_value = indicator_value,
    population_size_index = if_else(
      !is.na(population) & population > 0,
      pmax(population, 1000000)^0.35,
      NA_real_
    )
  )

democracy_label_data <- democracy_data %>%
  filter(
    !is.na(plot_indicator_value),
    plot_indicator_value >= min(c(0.1, 10)),
    plot_indicator_value <= max(c(0.1, 10)),
    !is.na(owid_region),
    owid_region != "Oceanía",
    !is.na(population),
    population > 0
  ) %>%
  arrange(owid_region, desc(population), country_code) %>%
  group_by(owid_region) %>%
  slice_head(n = 5L) %>%
  ungroup() %>%
  bind_rows(democracy_data %>% filter(country_code == "VEN")) %>%
  distinct(country_code, .keep_all = TRUE) %>%
  mutate(country_label = country_code)

democracy_year_label <- unique(democracy_data$year)

# Build the complete graph with all graph-specific scales directly visible.
democracy_plot <- democracy_data %>%
  ggplot() +
  geom_smooth(
    aes(x = gdp_per_capita, y = plot_indicator_value),
    method = "lm",
    formula = y ~ log10(x),
    se = FALSE,
    color = presentation_colors[["ink"]],
    linewidth = 0.45,
    alpha = 0.85
  ) +
  geom_point(
    aes(
      x = gdp_per_capita,
      y = plot_indicator_value,
      fill = owid_region,
      size = population_size_index
    ),
    shape = 21,
    color = presentation_colors[["ink"]],
    stroke = presentation_point_stroke,
    alpha = 0.72
  ) +
  geom_label_repel(
    data = democracy_label_data,
    aes(
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
  scale_x_log10(
    limits = owid_x_limits,
    breaks = owid_x_breaks,
    labels = presentation_number_label(accuracy = 1),
    expand = expansion(mult = c(0, 0.025))
  ) +
  scale_y_log10(
    limits = c(0.1, 10),
    breaks = c(0.1, 0.5, 1, 2, 3, 5, 7, 10),
    labels = presentation_number_label(accuracy = 0.1)(c(0.1, 0.5, 1, 2, 3, 5, 7, 10)),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = owid_region_colors,
    breaks = names(owid_region_colors),
    drop = FALSE,
    name = NULL
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1000000, 10000000, 100000000, 1000000000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  labs(
    title = sprintf("Democracia y PIB per cápita (%s)", democracy_year_label),
    subtitle = "Comparación de calidad democrática y producto per cápita.",
    x = "PIBpc PPP",
    y = "Índice de democracia (0–10)",
    caption = append_caption_note(presentation_source_caption, presentation_axis_note(log_x = TRUE, log_y = TRUE, extra = imf_ppp_unit_note))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  ) +
  theme(legend.position = "bottom") +
  apply_presentation_axis_theme()

# Apply universal presentation formatting, save, and preview the final object.
democracy_plot <- apply_presentation_plot_style(democracy_plot)

democracy_figure_file <- file.path(figure_dir, "owid_democracy_gdp_per_capita.png")
save_plot_variants(
  filename = democracy_figure_file,
  plot = democracy_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(democracy_plot)
message("Wrote ", democracy_figure_file)


### Graph 10: Complejidad económica y PIB per cápita ----
# Prepare the observations, population sizing, and labels used only by this graph.
economic_complexity_data <- eci_plot_data %>%
  filter(
    gdp_per_capita >= owid_x_limits[[1]],
    gdp_per_capita <= owid_x_limits[[2]]
  ) %>%
  mutate(
    plot_indicator_value = indicator_value,
    population_size_index = if_else(
      !is.na(population) & population > 0,
      pmax(population, 1000000)^0.35,
      NA_real_
    )
  )

economic_complexity_label_data <- economic_complexity_data %>%
  filter(
    !is.na(plot_indicator_value),
    plot_indicator_value >= min(c(-3.0, 3.0)),
    plot_indicator_value <= max(c(-3.0, 3.0)),
    !is.na(owid_region),
    owid_region != "Oceanía",
    !is.na(population),
    population > 0
  ) %>%
  arrange(owid_region, desc(population), country_code) %>%
  group_by(owid_region) %>%
  slice_head(n = 5L) %>%
  ungroup() %>%
  bind_rows(economic_complexity_data %>% filter(country_code == "VEN")) %>%
  distinct(country_code, .keep_all = TRUE) %>%
  mutate(country_label = country_code)

economic_complexity_year_label <- unique(economic_complexity_data$year)

# Build the complete graph with all graph-specific scales directly visible.
economic_complexity_plot <- economic_complexity_data %>%
  ggplot() +
  geom_smooth(
    aes(x = gdp_per_capita, y = plot_indicator_value),
    method = "lm",
    formula = y ~ log10(x),
    se = FALSE,
    color = presentation_colors[["ink"]],
    linewidth = 0.45,
    alpha = 0.85
  ) +
  geom_point(
    aes(
      x = gdp_per_capita,
      y = plot_indicator_value,
      fill = owid_region,
      size = population_size_index
    ),
    shape = 21,
    color = presentation_colors[["ink"]],
    stroke = presentation_point_stroke,
    alpha = 0.72
  ) +
  geom_label_repel(
    data = economic_complexity_label_data,
    aes(
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
  scale_x_log10(
    limits = owid_x_limits,
    breaks = owid_x_breaks,
    labels = presentation_number_label(accuracy = 1),
    expand = expansion(mult = c(0, 0.025))
  ) +
  scale_y_continuous(
    limits = c(-3.0, 3.0),
    breaks = seq(-3.0, 3.0, by = 1.0),
    labels = presentation_number_label(accuracy = 0.1)(seq(-3.0, 3.0, by = 1.0)),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    values = owid_region_colors,
    breaks = names(owid_region_colors),
    drop = FALSE,
    name = NULL
  ) +
  scale_size_continuous(
    range = c(1.8, 9.2),
    breaks = c(1000000, 10000000, 100000000, 1000000000)^0.35,
    labels = c("1M", "10M", "100M", "1B"),
    name = "Población"
  ) +
  labs(
    title = sprintf("Complejidad económica y PIB per cápita (%s)", economic_complexity_year_label),
    subtitle = "Compara sofisticación productiva de sector exportador y producto por habitante.",
    x = "PIBpc PPP",
    y = "Índice de complejidad económica",
    caption = append_caption_note(eci_source_caption, presentation_axis_note(log_x = TRUE, log_y = FALSE, extra = imf_ppp_unit_note))
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  ) +
  theme(legend.position = "bottom") +
  apply_presentation_axis_theme()

# Apply universal presentation formatting, save, and preview the final object.
economic_complexity_plot <- apply_presentation_plot_style(economic_complexity_plot)

economic_complexity_figure_file <- file.path(figure_dir, "economic_complexity_gdp_per_capita.png")
save_plot_variants(
  filename = economic_complexity_figure_file,
  plot = economic_complexity_plot,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)
print(economic_complexity_plot)
message("Wrote ", economic_complexity_figure_file)

# Data outputs ----
# Persist the combined OWID relationship table for review.
utils::write.csv(final_data, "data/final/owid_development_relationships.csv", row.names = FALSE)

# Keep the previous single-indicator path for compatibility with earlier deck builds.
utils::write.csv(
  life_expectancy_plot_data,
  "data/final/owid_life_expectancy_gdp_per_capita.csv",
  row.names = FALSE
)
message("Wrote data/final/owid_development_relationships.csv")
