# -*- coding: UTF-8 -*-
# Build first-pass ggplot figures for the Quarto presentation.

# Setup ----
# Validate and attach the packages required to run this script independently.
required_packages <- c(
  "dplyr",
  "ggplot2",
  "ggrepel",
  "magrittr",
  "readxl",
  "scales",
  "svglite",
  "tibble",
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

# Load shared presentation styling, labels, historical references, and export helpers.
source("scripts/_presentation_theme.R")

# Inputs ----
# Require final data products created by the earlier pipeline scripts.
required_files <- c(
  index_series = "data/final/index_series_long.csv",
  episode_path = "data/final/episode_path.csv",
  episode_summary = "data/final/episode_summary.csv",
  maddison_episode_path = "data/final/maddison_gdp_per_capita_episode_path.csv",
  maddison_episode_summary = "data/final/maddison_gdp_per_capita_episode_summary.csv",
  maddison_gdp_episode_path = "data/final/maddison_gdp_episode_path.csv",
  maddison_gdp_episode_summary = "data/final/maddison_gdp_episode_summary.csv",
  wdi_episode_path = "data/final/wdi_real_gdp_growth_episode_path.csv",
  wdi_episode_summary = "data/final/wdi_real_gdp_growth_episode_summary.csv",
  wdi_pc_episode_path = "data/final/wdi_real_gdp_per_capita_growth_episode_path.csv",
  wdi_pc_episode_summary = "data/final/wdi_real_gdp_per_capita_growth_episode_summary.csv",
  imf_weo_episode_path = "data/final/imf_weo_gdp_per_capita_growth_episode_path.csv",
  imf_weo_episode_summary = "data/final/imf_weo_gdp_per_capita_growth_episode_summary.csv",
  imf_weo_gdp_episode_path = "data/final/imf_weo_gdp_growth_episode_path.csv",
  imf_weo_gdp_episode_summary = "data/final/imf_weo_gdp_growth_episode_summary.csv",
  presidential_periods = "data/raw/venezuela_presidential_periods_fixed.xlsx",
  civil_war_timeline = "data/raw/venezuelan_civil_wars_conflict_dates.xlsx",
  simulation_summary = "data/final/simulation_summary.csv",
  simulation_paths = "data/final/simulation_paths.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    sprintf(
      "Missing required inputs: %s. Run upstream scripts and restore raw workbooks before plotting.",
      paste(unname(missing_files), collapse = ", ")
    ),
    call. = FALSE
  )
}

figure_dir <- "outputs/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Plot constants ----
# Shared colors, labels, and captions used across graph families.
growth_colors <- c(
  positive = presentation_colors[["positive"]],
  negative = presentation_colors[["negative"]]
)

phase_colors <- c(
  expansion = growth_colors[["positive"]],
  contraction = growth_colors[["negative"]]
)

phase_labels <- c(
  expansion = "Expansión",
  contraction = "Contracción"
)

growth_labels <- c(
  positive = "Expansión",
  negative = "Contracción"
)

mix_with_white <- function(color, white_share) {
  rgb_value <- grDevices::col2rgb(color)
  mixed_value <- rgb_value * (1 - white_share) + 255 * white_share
  grDevices::rgb(mixed_value[1, ], mixed_value[2, ], mixed_value[3, ], maxColorValue = 255)
}

presentation_source_caption <- build_source_caption(
  "Baptista (2008); Garay (2019); BCV (2016); Focus Economics (2026); Maddison Project (2023); Banco Mundial WDI (2025); FMI WEO (2025)",
  calculations = TRUE
)

scenario_colors <- c(
  "15%" = presentation_ordered_colors[[1]],
  "10%" = presentation_ordered_colors[[2]],
  "7%" = presentation_ordered_colors[[3]],
  "5%" = presentation_ordered_colors[[4]],
  "2%" = presentation_ordered_colors[[5]]
)

window_colors <- c(
  "3 años" = presentation_ordered_colors[[1]],
  "5 años" = presentation_ordered_colors[[2]],
  "7 años" = presentation_ordered_colors[[3]],
  "10 años" = presentation_ordered_colors[[4]],
  "15 años" = presentation_ordered_colors[[5]],
  "20 años" = presentation_ordered_colors[[6]]
)

series_order <- c("gdp", "gdp_per_capita")
series_labels <- c(
  gdp = "PIB real",
  gdp_per_capita = "PIB real per cápita"
)

build_episode_top_label_data <- function(data, selected_series, selected_phase, top_n) {
  ranked_episodes <- data %>%
    filter(series_id == selected_series, phase == selected_phase) %>%
    group_by(episode_group, episode_label) %>%
    summarise(
      final_change = last(episode_cumulative_change[order(year)]),
      final_year = max(year, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(sort_value = if (identical(selected_phase, "expansion")) -final_change else final_change) %>%
    arrange(sort_value) %>%
    slice_head(n = top_n)

  data %>%
    inner_join(ranked_episodes %>% select(episode_group, episode_label), by = c("episode_group", "episode_label")) %>%
    group_by(episode_group, episode_label) %>%
    filter(year == max(year, na.rm = TRUE)) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    arrange(if (identical(selected_phase, "expansion")) -episode_cumulative_change else episode_cumulative_change) %>%
    mutate(
      label_rank = row_number(),
      label = sprintf("(%s)", episode_label)
    )
}

# Data loading ----
# Read Venezuela series, episode summaries, and simulation outputs.
index_series <- utils::read.csv(required_files[["index_series"]], stringsAsFactors = FALSE) %>%
  mutate(
    growth_direction = if_else(growth_rate >= 0, "positive", "negative"),
    growth_direction = factor(growth_direction, levels = names(growth_colors)),
    series_id = factor(series_id, levels = series_order, labels = series_labels)
  )

episodes <- utils::read.csv(required_files[["episode_path"]], stringsAsFactors = FALSE) %>%
  mutate(
    growth_direction = if_else(phase == "expansion", "positive", "negative"),
    growth_direction = factor(growth_direction, levels = names(growth_colors)),
    episode_group = paste(series_id, episode_label, sep = "_"),
    series_id = factor(series_id, levels = series_order, labels = series_labels)
  )

episode_summary <- utils::read.csv(required_files[["episode_summary"]], stringsAsFactors = FALSE) %>%
  mutate(
    phase = factor(phase, levels = names(phase_colors)),
    series_id = factor(series_id, levels = series_order, labels = series_labels)
  )

simulation_summary <- utils::read.csv(required_files[["simulation_summary"]], stringsAsFactors = FALSE) %>%
  mutate(series_id = factor(series_id, levels = series_order, labels = series_labels))

simulation_paths <- utils::read.csv(required_files[["simulation_paths"]], stringsAsFactors = FALSE) %>%
  mutate(series_id = factor(series_id, levels = series_order, labels = series_labels))

maddison_episode_path <- utils::read.csv(
  required_files[["maddison_episode_path"]],
  stringsAsFactors = FALSE
)

maddison_episode_summary <- utils::read.csv(
  required_files[["maddison_episode_summary"]],
  stringsAsFactors = FALSE
)

maddison_gdp_episode_path <- utils::read.csv(
  required_files[["maddison_gdp_episode_path"]],
  stringsAsFactors = FALSE
)

maddison_gdp_episode_summary <- utils::read.csv(
  required_files[["maddison_gdp_episode_summary"]],
  stringsAsFactors = FALSE
)

wdi_episode_path <- utils::read.csv(
  required_files[["wdi_episode_path"]],
  stringsAsFactors = FALSE
)

wdi_episode_summary <- utils::read.csv(
  required_files[["wdi_episode_summary"]],
  stringsAsFactors = FALSE
)

wdi_pc_episode_path <- utils::read.csv(
  required_files[["wdi_pc_episode_path"]],
  stringsAsFactors = FALSE
)

wdi_pc_episode_summary <- utils::read.csv(
  required_files[["wdi_pc_episode_summary"]],
  stringsAsFactors = FALSE
)

imf_weo_episode_path <- utils::read.csv(
  required_files[["imf_weo_episode_path"]],
  stringsAsFactors = FALSE
)

imf_weo_episode_summary <- utils::read.csv(
  required_files[["imf_weo_episode_summary"]],
  stringsAsFactors = FALSE
)

imf_weo_gdp_episode_path <- utils::read.csv(
  required_files[["imf_weo_gdp_episode_path"]],
  stringsAsFactors = FALSE
)

imf_weo_gdp_episode_summary <- utils::read.csv(
  required_files[["imf_weo_gdp_episode_summary"]],
  stringsAsFactors = FALSE
)

# Read presidential terms used to assign annual GDP observations by year-end officeholder.
presidential_periods <- read_excel(
  required_files[["presidential_periods"]],
  sheet = "Presidential periods"
) %>%
  transmute(
    officeholder = `Officeholder`,
    officeholding_category = `Officeholding category`,
    start_date = as.Date(`Start date (ISO text)`),
    end_date = as.Date(`End date (ISO text)`)
  ) %>%
  filter(officeholding_category != "Parallel, partially recognized")

# Select the long, well-separated civil conflicts used as historical background references.
civil_war_events_to_display <- data.frame(
  sequence = c(4, 7, 16),
  display_label = c(
    "Guerra civil 1848-49",
    "Guerra Federal",
    "Rev. Libertadora"
  ),
  stringsAsFactors = FALSE
)

# Import the selected conflicts from the raw chronology and retain exact date placement.
civil_war_event_references <- read_excel(
  required_files[["civil_war_timeline"]],
  sheet = "Civil conflicts"
) %>%
  transmute(
    sequence = as.integer(`Sequence`),
    event = `Conflict`,
    start_date = as.Date(`Start date (ISO text)`),
    end_date = as.Date(`End date (ISO text)`)
  ) %>%
  inner_join(civil_war_events_to_display, by = "sequence")

if (
  nrow(civil_war_event_references) != nrow(civil_war_events_to_display) ||
    anyNA(civil_war_event_references$event) ||
    anyNA(civil_war_event_references$start_date) ||
    anyNA(civil_war_event_references$end_date)
) {
  stop(
    "The civil-war chronology must contain complete rows for display sequences 4, 7, and 16.",
    call. = FALSE
  )
}

civil_war_event_references <- civil_war_event_references %>%
  mutate(
    start_calendar_year = as.integer(format(start_date, "%Y")),
    end_calendar_year = as.integer(format(end_date, "%Y")),
    start_year = start_calendar_year +
      (as.integer(format(start_date, "%j")) - 1) /
        as.integer(format(as.Date(sprintf("%s-12-31", start_calendar_year)), "%j")),
    end_year = end_calendar_year +
      (as.integer(format(end_date, "%j")) - 1) /
        as.integer(format(as.Date(sprintf("%s-12-31", end_calendar_year)), "%j"))
  )

# Extend the shared historical layer with source-backed civil-war intervals and labels.
historical_event_references <- bind_rows(
  civil_war_event_references %>% select(event, start_year, end_year),
  historical_event_references
) %>%
  arrange(start_year)

historical_event_label_map <- c(
  historical_event_label_map,
  stats::setNames(
    civil_war_event_references$display_label,
    civil_war_event_references$event
  )
)

# Chart helpers ----
# Keep only transformations reused by multiple graph blocks in this section.

## Helper group: International episode labels ----
# Build endpoint labels shared by the three international contraction charts.
build_negative_episode_labels <- function(plot_data, highlighted_summary, venezuela_plot_data) {
  other_labels <- highlighted_summary %>%
    transmute(
      episode_key = episode_key,
      label_group = "Top 10 episodios",
      label = sprintf(
        "%s (%s-%s)",
        country_code,
        start_year,
        end_year
      )
    ) %>%
    inner_join(
      plot_data %>%
        group_by(episode_key) %>%
        filter(year == max(year, na.rm = TRUE)) %>%
        slice_tail(n = 1) %>%
        ungroup() %>%
        select(episode_key, year, episode_cumulative_change),
      by = "episode_key"
    )

  venezuela_labels <- venezuela_plot_data %>%
    group_by(episode_group) %>%
    summarise(
      start_year = min(year, na.rm = TRUE),
      end_year = max(year, na.rm = TRUE),
      episode_cumulative_change = episode_cumulative_change[which.max(year)],
      cumulative_growth = min(episode_cumulative_change, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    transmute(
      episode_key = episode_group,
      year = end_year,
      episode_cumulative_change = episode_cumulative_change,
      label_group = "Venezuela",
      label = sprintf(
        "VEN (%s-%s)",
        start_year,
        end_year
      )
    )

  bind_rows(other_labels, venezuela_labels)
}

## Helper group: Episode comparison summaries ----
# Normalize domestic and international episode summaries to one comparison schema.
build_venezuela_episode_summary_for_source <- function(
  venezuela_summary_data,
  source_name,
  source_label,
  selected_series,
  period_start
) {
  venezuela_summary_data %>%
    filter(
      series_id == selected_series,
      start_year >= period_start,
      start_year <= 2026
    ) %>%
    mutate(
      source = source_name,
      source_label = source_label,
      country_code = "VEN",
      country = "Venezuela",
      episode_key = paste(source_name, country_code, episode_id, sep = "_"),
      cagr = (1 + cumulative_growth)^(1 / pmax(duration_years, 1)) - 1,
      is_venezuela = TRUE
    )
}

build_source_episode_summary <- function(
  summary_data,
  source_name,
  source_label,
  period_start,
  country_name_column = "country"
) {
  filtered_data <- summary_data %>%
    filter(
      country_code != "VEN",
      start_year >= period_start,
      start_year <= 2026
    )

  country_names <- if (country_name_column %in% names(filtered_data)) {
    filtered_data[[country_name_column]]
  } else if ("maddison_country" %in% names(filtered_data)) {
    filtered_data[["maddison_country"]]
  } else {
    filtered_data[["country_code"]]
  }

  filtered_data %>%
    mutate(
      source = source_name,
      source_label = source_label,
      country = country_names,
      episode_key = paste(source_name, country_code, episode_id, sep = "_"),
      cagr = (1 + cumulative_growth)^(1 / pmax(duration_years, 1)) - 1,
      is_venezuela = FALSE
    )
}

build_international_episode_summary <- function(
  maddison_summary_data,
  wdi_summary_data,
  imf_weo_summary_data,
  venezuela_summary_data
) {
  bind_rows(
    build_source_episode_summary(
      maddison_summary_data,
      source_name = "maddison",
      source_label = "Maddison (PIB per cápita)",
      period_start = 1919,
      country_name_column = "country"
    ),
    build_venezuela_episode_summary_for_source(
      venezuela_summary_data,
      source_name = "maddison",
      source_label = "Maddison (PIB per cápita)",
      selected_series = series_labels[["gdp_per_capita"]],
      period_start = 1919
    ),
    build_source_episode_summary(
      wdi_summary_data,
      source_name = "wdi",
      source_label = "WDI (PIB per cápita)",
      period_start = 1965,
      country_name_column = "country"
    ),
    build_venezuela_episode_summary_for_source(
      venezuela_summary_data,
      source_name = "wdi",
      source_label = "WDI (PIB per cápita)",
      selected_series = series_labels[["gdp_per_capita"]],
      period_start = 1965
    ),
    build_source_episode_summary(
      imf_weo_summary_data,
      source_name = "imf_weo",
      source_label = "FMI WEO (PIB per cápita)",
      period_start = 1981,
      country_name_column = "maddison_country"
    ),
    build_venezuela_episode_summary_for_source(
      venezuela_summary_data,
      source_name = "imf_weo",
      source_label = "FMI WEO (PIB per cápita)",
      selected_series = series_labels[["gdp_per_capita"]],
      period_start = 1981
    )
  ) %>%
    mutate(
      country_episode = sprintf("%s (%s-%s)", country_code, start_year, end_year),
      highlight = if_else(is_venezuela, "Venezuela", "Otros países")
    )
}

## Helper group: Episode comparison series ----
# Normalize source-specific episode paths before recovery comparisons are calculated.
build_source_series_from_episode_path <- function(
  path_data,
  source_name,
  source_label,
  period_start,
  country_name_column = "country"
) {
  country_names <- if (country_name_column %in% names(path_data)) {
    path_data[[country_name_column]]
  } else if ("maddison_country" %in% names(path_data)) {
    path_data[["maddison_country"]]
  } else {
    path_data[["country_code"]]
  }

  path_data %>%
    mutate(
      source = source_name,
      source_label = source_label,
      country = country_names
    ) %>%
    filter(country_code != "VEN", year >= period_start, year <= 2026) %>%
    distinct(source, source_label, country_code, country, year, index_value)
}

build_venezuela_series_for_source <- function(
  data,
  source_name,
  source_label,
  selected_series,
  period_start
) {
  data %>%
    filter(series_id == selected_series, year >= period_start, year <= 2026) %>%
    transmute(
      source = source_name,
      source_label = source_label,
      country_code = "VEN",
      country = "Venezuela",
      year = year,
      index_value = index_value
    )
}

build_international_series_for_recovery <- function(
  maddison_path_data,
  wdi_path_data,
  imf_weo_path_data,
  venezuela_index_data
) {
  bind_rows(
    build_source_series_from_episode_path(
      maddison_path_data,
      source_name = "maddison",
      source_label = "Maddison (PIB per cápita)",
      period_start = 1919,
      country_name_column = "country"
    ),
    build_venezuela_series_for_source(
      venezuela_index_data,
      source_name = "maddison",
      source_label = "Maddison (PIB per cápita)",
      selected_series = series_labels[["gdp_per_capita"]],
      period_start = 1919
    ),
    build_source_series_from_episode_path(
      wdi_path_data,
      source_name = "wdi",
      source_label = "WDI (PIB per cápita)",
      period_start = 1965,
      country_name_column = "country"
    ),
    build_venezuela_series_for_source(
      venezuela_index_data,
      source_name = "wdi",
      source_label = "WDI (PIB per cápita)",
      selected_series = series_labels[["gdp_per_capita"]],
      period_start = 1965
    ),
    build_source_series_from_episode_path(
      imf_weo_path_data,
      source_name = "imf_weo",
      source_label = "FMI WEO (PIB per cápita)",
      period_start = 1981,
      country_name_column = "maddison_country"
    ),
    build_venezuela_series_for_source(
      venezuela_index_data,
      source_name = "imf_weo",
      source_label = "FMI WEO (PIB per cápita)",
      selected_series = series_labels[["gdp_per_capita"]],
      period_start = 1981
    )
  )
}

## Helper group: Disaster recovery tables ----
# Derive recovery paths and fixed-horizon summaries from normalized episode data.
build_disaster_recovery_data <- function(
  comparison_data,
  series_data,
  disaster_threshold = -0.15,
  max_years_after_end = 30L
) {
  disasters <- comparison_data %>%
    filter(phase == "contraction", cumulative_growth <= disaster_threshold) %>%
    mutate(
      disaster_key = paste(source, country_code, episode_id, sep = "_"),
      disaster_label = sprintf("%s (%s-%s)", country, start_year, end_year)
    )

  disaster_rows <- lapply(seq_len(nrow(disasters)), function(i) {
    disaster <- disasters[i, , drop = FALSE]
    country_series <- series_data %>%
      filter(
        source == disaster$source[[1]],
        country_code == disaster$country_code[[1]],
        year >= disaster$start_year[[1]],
        year <= disaster$end_year[[1]] + max_years_after_end
      ) %>%
      arrange(year)

    if (nrow(country_series) == 0 || !any(country_series$year == disaster$start_year[[1]])) {
      return(NULL)
    }

    start_index <- country_series$index_value[country_series$year == disaster$start_year[[1]]][[1]]
    country_series$index_start_100 <- country_series$index_value / start_index * 100
    country_series$years_since_start <- country_series$year - disaster$start_year[[1]]

    recovery_candidates <- country_series %>%
      filter(year > disaster$end_year[[1]], index_start_100 >= 100)
    recovery_year <- if (nrow(recovery_candidates) > 0) {
      recovery_candidates$year[[1]]
    } else {
      NA_integer_
    }

    country_series %>%
      mutate(
        disaster_key = disaster$disaster_key[[1]],
        disaster_label = disaster$disaster_label[[1]],
        source_label = disaster$source_label[[1]],
        cumulative_growth = disaster$cumulative_growth[[1]],
        duration_years = disaster$duration_years[[1]],
        start_year = disaster$start_year[[1]],
        end_year = disaster$end_year[[1]],
        recovery_year = recovery_year,
        years_to_recover = ifelse(is.na(recovery_year), NA_real_, recovery_year - disaster$end_year[[1]]),
        is_venezuela = disaster$is_venezuela[[1]],
        highlight = if_else(disaster$is_venezuela[[1]], "Venezuela", "Otros países")
      )
  })

  bind_rows(disaster_rows)
}

build_disaster_horizon_data <- function(disaster_data, horizons) {
  episode_metadata <- disaster_data %>%
    distinct(
      source_label,
      disaster_key,
      disaster_label,
      country_code,
      country,
      start_year,
      end_year,
      cumulative_growth,
      duration_years,
      is_venezuela
    )

  horizon_parts <- lapply(horizons, function(horizon) {
    horizon_rows <- disaster_data %>%
      distinct(disaster_key, end_year, .keep_all = TRUE) %>%
      transmute(
        disaster_key = disaster_key,
        horizon_years_after_end = horizon,
        endpoint_year = end_year + horizon
      ) %>%
      inner_join(
        disaster_data %>%
          select(disaster_key, endpoint_year = year, horizon_index_start_100 = index_start_100),
        by = c("disaster_key", "endpoint_year")
      )

    if (nrow(horizon_rows) == 0) {
      return(NULL)
    }

    episode_metadata %>%
      inner_join(horizon_rows, by = "disaster_key") %>%
      mutate(
        horizon_years = horizon,
        horizon_label = sprintf("%s años después", horizon),
        fall_magnitude = -cumulative_growth,
        horizon_ratio_to_start = horizon_index_start_100 / 100,
        total_years_from_start = endpoint_year - start_year,
        horizon_cagr = horizon_ratio_to_start^(1 / total_years_from_start) - 1,
        post_disaster_ratio_to_end = horizon_ratio_to_start / (1 + cumulative_growth),
        post_disaster_cagr = post_disaster_ratio_to_end^(1 / horizon_years_after_end) - 1,
        highlight = if_else(is_venezuela, "Venezuela", "Otros países")
      )
  })

  bind_rows(horizon_parts) %>%
    mutate(
      horizon_label = factor(horizon_label, levels = sprintf("%s años después", horizons)),
      highlight = factor(highlight, levels = c("Otros países", "Venezuela"))
    )
}

add_source_reference_disasters <- function(horizon_data, horizons) {
  reference_keys <- horizon_data %>%
    filter(!is_venezuela) %>%
    distinct(source_label, disaster_key, disaster_label, cumulative_growth, horizon_years) %>%
    group_by(source_label, disaster_key, disaster_label, cumulative_growth) %>%
    summarise(
      horizon_count = length(unique(horizon_years)),
      .groups = "drop"
    ) %>%
    filter(horizon_count == length(horizons)) %>%
    group_by(source_label) %>%
    slice_min(cumulative_growth, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(reference_label = sprintf("%s", disaster_label)) %>%
    select(source_label, disaster_key, reference_label)

  horizon_data %>%
    left_join(reference_keys, by = c("source_label", "disaster_key")) %>%
    mutate(
      is_source_reference = !is.na(reference_label),
      reference_label = if_else(is_source_reference, reference_label, "")
    )
}

## Helper group: Recovery projections ----
# Build scenario paths, crossing labels, and heatmap inputs reused by recovery graphs.
build_recovery_projection_data <- function(data, scenario_rates, start_year, average_end_year) {
  latest_year <- max(data$year, na.rm = TRUE)
  series_ids_raw <- unique(as.character(data$series_id))

  historical_parts <- vector("list", length(series_ids_raw))
  projection_parts <- vector("list", length(series_ids_raw))

  for (i in seq_along(series_ids_raw)) {
    selected_series <- series_ids_raw[[i]]
    series_data <- data[data$series_id == selected_series, , drop = FALSE]
    peak_index <- max(series_data$index_value, na.rm = TRUE)
    latest_index <- series_data$index_value[series_data$year == latest_year][[1]]

    historical_parts[[i]] <- data.frame(
      series_id = selected_series,
      year = series_data$year,
      index_peak_100 = series_data$index_value / peak_index * 100,
      path_type = "Histórico",
      scenario_label = "Histórico",
      stringsAsFactors = FALSE
    )

    start_row <- series_data[series_data$year == start_year, , drop = FALSE]
    average_end_row <- series_data[series_data$year == average_end_year, , drop = FALSE]
    historical_average <- if (nrow(start_row) == 1 && nrow(average_end_row) == 1) {
      (average_end_row$index_value[[1]] / start_row$index_value[[1]])^(1 / (average_end_year - start_year)) - 1
    } else {
      NA_real_
    }

    scenario_rates <- sort(scenario_rates, decreasing = TRUE)
    rates <- c(scenario_rates, historical_average)
    labels <- c(
      percent(scenario_rates, accuracy = 1),
      sprintf("TCAC %s-%s (%s)", start_year, average_end_year, percent(historical_average, accuracy = 0.1))
    )
    valid_rates <- !is.na(rates)
    rates <- rates[valid_rates]
    labels <- labels[valid_rates]

    projection_years <- seq.int(latest_year, latest_year + 80L)
    scenario_parts <- vector("list", length(rates))
    for (j in seq_along(rates)) {
      projected_index <- latest_index * (1 + rates[[j]])^(projection_years - latest_year)
      projected_index_peak_100 <- projected_index / peak_index * 100
      recovery_positions <- which(projected_index_peak_100 >= 100)
      keep_until <- if (length(recovery_positions) > 0) recovery_positions[[1]] else length(projection_years)
      scenario_years <- projection_years[seq_len(keep_until)]
      scenario_parts[[j]] <- data.frame(
        series_id = selected_series,
        year = scenario_years,
        index_peak_100 = projected_index_peak_100[seq_len(keep_until)],
        path_type = "Proyección",
        scenario_label = labels[[j]],
        growth_rate = rates[[j]],
        stringsAsFactors = FALSE
      )
    }
    projection_parts[[i]] <- do.call(rbind, scenario_parts)
  }

  list(
    historical = do.call(rbind, historical_parts),
    projections = do.call(rbind, projection_parts)
  )
}

build_recovery_heatmap_data <- function(data, selected_series, n_reference_years) {
  series_data <- data[data$series_id == selected_series, , drop = FALSE]
  latest_year <- max(series_data$year, na.rm = TRUE)
  latest_index <- series_data$index_value[series_data$year == latest_year][[1]]
  peak_index <- max(series_data$index_value, na.rm = TRUE)
  peak_year <- series_data$year[which.max(series_data$index_value)]

  forced_years <- unique(c(1999L, 2013L, peak_year, latest_year))
  eligible_years <- series_data$year[
    series_data$year >= 1900 &
      series_data$year <= latest_year &
      (series_data$index_value >= latest_index | series_data$year %in% forced_years)
  ]
  eligible_years <- sort(unique(eligible_years))
  optional_years <- setdiff(eligible_years, forced_years)
  optional_slots <- max(n_reference_years - length(intersect(forced_years, eligible_years)), 0L)
  optional_reference_years <- if (optional_slots > 0 && length(optional_years) > 0) {
    optional_years[unique(round(seq(1, length(optional_years), length.out = optional_slots)))]
  } else {
    integer(0)
  }
  reference_years <- sort(unique(c(intersect(forced_years, eligible_years), optional_reference_years)))
  reference_data <- series_data[match(reference_years, series_data$year), , drop = FALSE]
  reference_data$index_peak_100 <- reference_data$index_value / peak_index * 100
  reference_data$column_label <- sprintf("%s\n%s", reference_data$year, number(reference_data$index_peak_100, accuracy = 0.1))

  rates <- seq(0.01, 0.15, by = 0.01)
  heatmap_data <- merge(
    data.frame(growth_rate = rates, stringsAsFactors = FALSE),
    reference_data[, c("year", "index_peak_100", "column_label")],
    all = TRUE
  )
  heatmap_data$latest_index_peak_100 <- latest_index / peak_index * 100
  heatmap_data$years_to_recover <- ifelse(
    heatmap_data$index_peak_100 <= heatmap_data$latest_index_peak_100,
    0,
    ceiling(log(heatmap_data$index_peak_100 / heatmap_data$latest_index_peak_100) / log(1 + heatmap_data$growth_rate))
  )
  heatmap_data$rate_label <- percent(heatmap_data$growth_rate, accuracy = 1)
  heatmap_data$rate_label <- factor(heatmap_data$rate_label, levels = rev(percent(rates, accuracy = 1)))
  heatmap_data$column_label <- factor(heatmap_data$column_label, levels = reference_data$column_label)
  heatmap_data
}

## Helper group: Rolling CAGR calculations ----
# Calculate comparable rolling compound-growth windows for both historical series.
build_rolling_cagr_data <- function(data, windows) {
  series_values <- split(data, data$series_id)
  rolling_parts <- list()

  for (series_name in names(series_values)) {
    series_data <- series_values[[series_name]]
    series_data <- series_data[order(series_data$year), , drop = FALSE]

    for (window_length in windows) {
      end_years <- series_data$year[(series_data$year - window_length) %in% series_data$year]
      if (length(end_years) == 0) {
        next
      }

      window_rows <- lapply(end_years, function(end_year) {
        start_year <- end_year - window_length
        start_index <- series_data$index_value[series_data$year == start_year][[1]]
        end_index <- series_data$index_value[series_data$year == end_year][[1]]

        data.frame(
          series_id = series_name,
          start_year = start_year,
          end_year = end_year,
          window_years = window_length,
          cagr = (end_index / start_index)^(1 / window_length) - 1,
          stringsAsFactors = FALSE
        )
      })

      rolling_parts[[paste(series_name, window_length, sep = "_")]] <- do.call(rbind, window_rows)
    }
  }

  do.call(rbind, rolling_parts)
}

# Derived analysis tables ----
# Build rolling TCAC windows used by the momentum graphs.
rolling_cagr <- build_rolling_cagr_data(
  index_series,
  windows = c(3L, 5L, 7L, 10L, 15L, 20L)
)

# Combine international episode summaries into one comparison table.
international_episode_comparison <- build_international_episode_summary(
  maddison_episode_summary,
  wdi_pc_episode_summary,
  imf_weo_episode_summary,
  episode_summary
)

international_recovery_series <- build_international_series_for_recovery(
  maddison_episode_path,
  wdi_pc_episode_path,
  imf_weo_episode_path,
  index_series
)

# Summarize deep contractions and recovery time after each disaster episode.
international_disaster_recovery <- build_disaster_recovery_data(
  international_episode_comparison,
  international_recovery_series,
  disaster_threshold = -0.15,
  max_years_after_end = 30L
)

# Measure post-disaster levels and TCACs at fixed horizons.
international_disaster_horizons <- build_disaster_horizon_data(
  international_disaster_recovery,
  horizons = c(5L, 10L, 15L, 20L, 25L, 30L)
)

international_disaster_horizons <- add_source_reference_disasters(
  international_disaster_horizons,
  horizons = c(5L, 10L, 15L, 20L, 25L, 30L)
)

# Derived data outputs ----
# Persist the recovery comparison tables for later review and reporting.
utils::write.csv(
  international_disaster_recovery %>%
    distinct(
      source_label,
      disaster_key,
      disaster_label,
      country_code,
      country,
      start_year,
      end_year,
      cumulative_growth,
      duration_years,
      recovery_year,
      years_to_recover,
      is_venezuela
    ) %>%
    arrange(source_label, cumulative_growth),
  "data/final/international_disaster_recovery_summary.csv",
  row.names = FALSE
)

utils::write.csv(
  international_disaster_horizons %>%
    arrange(source_label, horizon_years, cumulative_growth),
  "data/final/international_disaster_horizon_summary.csv",
  row.names = FALSE
)

# Graphs ----
# Each graph below is built, saved, and previewed in place for easier review.

density_note <- "Densidad indica la concentración suavizada de episodios o años; el área bajo cada curva se normaliza a uno."

## Family: annual growth bars and distributions ----


# A light keyline separates adjacent annual columns without competing with their fill.
annual_growth_bar_outline_color <- presentation_colors[["light"]]
annual_growth_bar_outline_linewidth <- 0.12

### Graph 01: Crecimiento del PIB real ----
gdp_growth_bars_data <- index_series %>%
  filter(series_id == "PIB real")

gdp_growth_bars <- gdp_growth_bars_data %>%
  ggplot(aes(x = year, y = growth_rate, fill = growth_direction)) +
  geom_col(
    width = 0.9,
    color = annual_growth_bar_outline_color,
    linewidth = annual_growth_bar_outline_linewidth
  ) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = presentation_colors[["ink"]]) +
  scale_fill_manual(values = growth_colors, labels = growth_labels, name = NULL) +
  presentation_full_history_year_axis() +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    breaks = seq(-0.40, 0.40, by = 0.10)
  ) +
  coord_cartesian(ylim = c(-0.40, 0.40), expand = FALSE) +
  labs(
    title = "Crecimiento anual del PIB real",
    x = NULL,
    y = "Variación interanual",
    subtitle = "Variaciones interanuales positivas y negativas del PIB real desde 1830.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  historical_event_reference_layers()

gdp_growth_bars <- apply_presentation_plot_style(gdp_growth_bars)

save_plot_variants(
  filename = file.path(figure_dir, "gdp_growth_bars.png"),
  plot = gdp_growth_bars,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_growth_bars)
message("Wrote ", file.path(figure_dir, "gdp_growth_bars.png"))

### Graph 02: Crecimiento del PIB real per cápita ----
gdp_per_capita_growth_bars_data <- index_series %>%
  filter(series_id == "PIB real per cápita")

gdp_per_capita_growth_bars <- gdp_per_capita_growth_bars_data %>%
  ggplot(aes(x = year, y = growth_rate, fill = growth_direction)) +
  geom_col(
    width = 0.9,
    color = annual_growth_bar_outline_color,
    linewidth = annual_growth_bar_outline_linewidth
  ) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = presentation_colors[["ink"]]) +
  scale_fill_manual(values = growth_colors, labels = growth_labels, name = NULL) +
  presentation_full_history_year_axis() +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    breaks = seq(-0.40, 0.40, by = 0.10)
  ) +
  coord_cartesian(ylim = c(-0.40, 0.40), expand = FALSE) +
  labs(
    title = "Crecimiento anual del PIB real per cápita",
    x = NULL,
    y = "Variación interanual",
    subtitle = "Variaciones interanuales positivas y negativas del PIB real desde 1830.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  historical_event_reference_layers()

gdp_per_capita_growth_bars <- apply_presentation_plot_style(gdp_per_capita_growth_bars)

save_plot_variants(
  filename = file.path(figure_dir, "gdp_per_capita_growth_bars.png"),
  plot = gdp_per_capita_growth_bars,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_per_capita_growth_bars)
message("Wrote ", file.path(figure_dir, "gdp_per_capita_growth_bars.png"))

### Graph 03: Distribución anual del PIB real ----
gdp_growth_rate_distribution_data <- index_series %>%
  filter(!is.na(growth_rate), series_id == series_labels[["gdp"]])

gdp_growth_rate_distribution <- gdp_growth_rate_distribution_data %>%
  ggplot(aes(x = growth_rate)) +
  geom_histogram(aes(fill = "Años"), binwidth = 0.01, boundary = 0, color = "white", linewidth = 0.2) +
  geom_density(aes(y = after_stat(count * 0.01), color = "Densidad"), linewidth = 0.8, adjust = 1.1) +
  geom_vline(xintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.35) +
  scale_fill_manual(values = c("Años" = presentation_colors[["muted"]]), name = NULL) +
  scale_color_manual(values = c("Densidad" = presentation_colors[["primary"]]), name = NULL) +
  scale_x_continuous(labels = label_percent(accuracy = 1), breaks = breaks_width(0.05)) +
  scale_y_continuous(breaks = seq(0, 16, by = 2)) +
  coord_cartesian(xlim = c(-0.4, 0.4), ylim = c(0, 16), expand = FALSE) +
  labs(
    title = "Distribución del crecimiento anual del PIB real",
    x = "Variación interanual",
    y = "Número de años",
    subtitle = "Cuenta años históricos por intervalo de crecimiento anual del PIB real.",
    caption = append_caption_note(presentation_source_caption, density_note)
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family)

gdp_growth_rate_distribution <- apply_presentation_plot_style(gdp_growth_rate_distribution)

save_plot_variants(
  filename = file.path(figure_dir, "gdp_growth_rate_distribution.png"),
  plot = gdp_growth_rate_distribution,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_growth_rate_distribution)
message("Wrote ", file.path(figure_dir, "gdp_growth_rate_distribution.png"))

### Graph 04: Distribución anual per cápita ----
gdp_per_capita_growth_rate_distribution_data <- index_series %>%
  filter(!is.na(growth_rate), series_id == series_labels[["gdp_per_capita"]])

gdp_per_capita_growth_rate_distribution <- gdp_per_capita_growth_rate_distribution_data %>%
  ggplot(aes(x = growth_rate)) +
  geom_histogram(aes(fill = "Años"), binwidth = 0.01, boundary = 0, color = "white", linewidth = 0.2) +
  geom_density(aes(y = after_stat(count * 0.01), color = "Densidad"), linewidth = 0.8, adjust = 1.1) +
  geom_vline(xintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.35) +
  scale_fill_manual(values = c("Años" = presentation_colors[["muted"]]), name = NULL) +
  scale_color_manual(values = c("Densidad" = presentation_colors[["primary"]]), name = NULL) +
  scale_x_continuous(labels = label_percent(accuracy = 1), breaks = breaks_width(0.05)) +
  scale_y_continuous(breaks = seq(0, 16, by = 2)) +
  coord_cartesian(xlim = c(-0.4, 0.4), ylim = c(0, 16), expand = FALSE) +
  labs(
    title = "Distribución del crecimiento anual del PIB real per cápita",
    x = "Variación interanual",
    y = "Número de años",
    subtitle = "Cuenta años históricos por intervalo de crecimiento anual del PIB real per cápita.",
    caption = append_caption_note(presentation_source_caption, density_note)
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family)

gdp_per_capita_growth_rate_distribution <- apply_presentation_plot_style(gdp_per_capita_growth_rate_distribution)

save_plot_variants(
  filename = file.path(figure_dir, "gdp_per_capita_growth_rate_distribution.png"),
  plot = gdp_per_capita_growth_rate_distribution,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_per_capita_growth_rate_distribution)
message("Wrote ", file.path(figure_dir, "gdp_per_capita_growth_rate_distribution.png"))

## Family: historical index and anchor lines ----

### Graph 05: Índice del PIB real ----
gdp_index_line_data <- index_series %>% filter(series_id == "PIB real")
gdp_index_line_base_year <- min(gdp_index_line_data$year, na.rm = TRUE)
gdp_index_line_latest_year <- max(gdp_index_line_data$year, na.rm = TRUE)
gdp_index_line_latest_value <- gdp_index_line_data %>%
  filter(year == gdp_index_line_latest_year) %>%
  pull(index_value) %>%
  first()
gdp_index_line_reference <- gdp_index_line_data %>% filter(year == 1967)
gdp_index_line_label <- gdp_index_line_reference %>% mutate(label_x = year + 6, label_y = index_value + gdp_index_line_latest_value * 0.24)
gdp_index_line <- gdp_index_line_data %>% ggplot(aes(x = year, y = index_value)) +
  geom_hline(yintercept = gdp_index_line_latest_value, color = presentation_colors[["accent"]], linewidth = 0.65, linetype = "dashed") +
  geom_vline(data = gdp_index_line_reference, aes(xintercept = year), color = presentation_colors[["primary"]], linewidth = 0.45, linetype = "dotted") +
  geom_line(aes(color = "PIB real"), linewidth = 0.55) +
  geom_point(data = gdp_index_line_reference, shape = 21, fill = presentation_colors[["venezuela"]], color = presentation_colors[["ink"]], stroke = presentation_point_stroke, size = 3.6) +
  geom_segment(data = gdp_index_line_label, aes(x = year, y = index_value, xend = label_x, yend = label_y), inherit.aes = FALSE, color = presentation_colors[["muted"]], linewidth = presentation_label_segment_size) +
  geom_label(data = gdp_index_line_label, aes(x = label_x, y = label_y, label = year), inherit.aes = FALSE, color = presentation_colors[["ink"]], fill = "white", linewidth = presentation_label_box_linewidth, label.padding = presentation_label_padding, family = presentation_font_family, fontface = "bold", size = presentation_label_text_size, hjust = 0.5) +
  scale_color_manual(values = c("PIB real" = presentation_colors[["ink"]]), name = NULL) +
  presentation_full_history_year_axis() +
  scale_y_continuous(labels = presentation_number_label(accuracy = 1), limits = c(0, 150000), breaks = seq(0, 150000, by = 25000)) +
  labs(
    title = sprintf("Índice histórico del PIB real: nivel de %s cercano a 1967", gdp_index_line_latest_year),
    x = NULL,
    y = sprintf("Índice histórico (%s = 100)", gdp_index_line_base_year),
    subtitle = "Ubica el nivel reciente del PIB real frente a referencias históricas comparables.",
    caption = presentation_source_caption
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(plot.margin = margin(12, 42, 12, 18)) +
  historical_event_reference_layers()

gdp_index_line <- apply_presentation_plot_style(gdp_index_line)

save_plot_variants(
  filename = file.path(figure_dir, "gdp_index_line.png"),
  plot = gdp_index_line,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_index_line)
message("Wrote ", file.path(figure_dir, "gdp_index_line.png"))

### Graph 06: Índice del PIB real per cápita ----
gdp_per_capita_index_line_data <- index_series %>% filter(series_id == "PIB real per cápita")
gdp_per_capita_index_line_base_year <- min(gdp_per_capita_index_line_data$year, na.rm = TRUE)
gdp_per_capita_index_line_latest_year <- max(gdp_per_capita_index_line_data$year, na.rm = TRUE)
gdp_per_capita_index_line_latest_value <- gdp_per_capita_index_line_data %>%
  filter(year == gdp_per_capita_index_line_latest_year) %>%
  pull(index_value) %>%
  first()
gdp_per_capita_index_line_reference <- gdp_per_capita_index_line_data %>% filter(year == 1943)
gdp_per_capita_index_line_label <- gdp_per_capita_index_line_reference %>% mutate(label_x = year + 6, label_y = index_value + gdp_per_capita_index_line_latest_value * 0.24)
gdp_per_capita_index_line <- gdp_per_capita_index_line_data %>% ggplot(aes(x = year, y = index_value)) +
  geom_hline(yintercept = gdp_per_capita_index_line_latest_value, color = presentation_colors[["accent"]], linewidth = 0.65, linetype = "dashed") +
  geom_vline(data = gdp_per_capita_index_line_reference, aes(xintercept = year), color = presentation_colors[["primary"]], linewidth = 0.45, linetype = "dotted") +
  geom_line(aes(color = "PIB real per cápita"), linewidth = 0.55) +
  geom_point(data = gdp_per_capita_index_line_reference, shape = 21, fill = presentation_colors[["venezuela"]], color = presentation_colors[["ink"]], stroke = presentation_point_stroke, size = 3.6) +
  geom_segment(data = gdp_per_capita_index_line_label, aes(x = year, y = index_value, xend = label_x, yend = label_y), inherit.aes = FALSE, color = presentation_colors[["muted"]], linewidth = presentation_label_segment_size) +
  geom_label(data = gdp_per_capita_index_line_label, aes(x = label_x, y = label_y, label = year), inherit.aes = FALSE, color = presentation_colors[["ink"]], fill = "white", linewidth = presentation_label_box_linewidth, label.padding = presentation_label_padding, family = presentation_font_family, fontface = "bold", size = presentation_label_text_size, hjust = 0.5) +
  scale_color_manual(values = c("PIB real per cápita" = presentation_colors[["ink"]]), name = NULL) +
  presentation_full_history_year_axis() +
  scale_y_continuous(labels = presentation_number_label(accuracy = 1), limits = c(0, 5000), breaks = seq(0, 5000, by = 1000)) +
  labs(
    title = sprintf("Índice histórico del PIB real per cápita: nivel de %s cercano a 1943", gdp_per_capita_index_line_latest_year),
    x = NULL,
    y = sprintf("Índice histórico (%s = 100)", gdp_per_capita_index_line_base_year),
    subtitle = "Ubica el nivel reciente del PIB real per cápita frente a referencias históricas comparables.",
    caption = presentation_source_caption
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(plot.margin = margin(12, 42, 12, 18)) +
  historical_event_reference_layers()

gdp_per_capita_index_line <- apply_presentation_plot_style(gdp_per_capita_index_line)

save_plot_variants(
  filename = file.path(figure_dir, "gdp_per_capita_index_line.png"),
  plot = gdp_per_capita_index_line,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_per_capita_index_line)
message("Wrote ", file.path(figure_dir, "gdp_per_capita_index_line.png"))

### Graph 07: Anclaje del PIB real ----
gdp_anchor_line_data <- index_series %>%
  filter(series_id == "PIB real")

gdp_anchor_line <- gdp_anchor_line_data %>%
  ggplot(aes(x = year, y = index_vs_anchor_100)) +
  geom_hline(yintercept = 100, linewidth = 0.3, color = presentation_colors[["ink"]]) +
  geom_line(color = presentation_colors[["ink"]], linewidth = 0.55) +
  presentation_full_history_year_axis() +
  labs(
    title = "PIB real relativo al último año",
    x = NULL,
    y = "Índice vs. último año = 100",
    subtitle = "Reescala la serie para mostrar cada año relativo al último nivel observado.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  historical_event_reference_layers()

gdp_anchor_line <- apply_presentation_plot_style(gdp_anchor_line)

save_plot_variants(
  filename = file.path(figure_dir, "gdp_anchor_line.png"),
  plot = gdp_anchor_line,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_anchor_line)
message("Wrote ", file.path(figure_dir, "gdp_anchor_line.png"))

### Graph 08: Anclaje per cápita ----
gdp_per_capita_anchor_line_data <- index_series %>%
  filter(series_id == "PIB real per cápita")

gdp_per_capita_anchor_line <- gdp_per_capita_anchor_line_data %>%
  ggplot(aes(x = year, y = index_vs_anchor_100)) +
  geom_hline(yintercept = 100, linewidth = 0.3, color = presentation_colors[["ink"]]) +
  geom_line(color = presentation_colors[["ink"]], linewidth = 0.55) +
  presentation_full_history_year_axis() +
  labs(
    title = "PIB real per cápita relativo al último año",
    x = NULL,
    y = "Índice vs. último año = 100",
    subtitle = "Reescala la serie per cápita para mostrar cada año relativo al último nivel observado.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  historical_event_reference_layers()

gdp_per_capita_anchor_line <- apply_presentation_plot_style(gdp_per_capita_anchor_line)

save_plot_variants(
  filename = file.path(figure_dir, "gdp_per_capita_anchor_line.png"),
  plot = gdp_per_capita_anchor_line,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_per_capita_anchor_line)
message("Wrote ", file.path(figure_dir, "gdp_per_capita_anchor_line.png"))

# Shared styling keeps Venezuelan episode paths comparable across graph variants.
domestic_episode_linewidth <- 0.85
domestic_episode_line_alpha <- 0.68
domestic_episode_marker_size <- 2.8
domestic_episode_start_fill <- alpha("white", alpha = 0.55)
domestic_episode_combined_y_limits <- c(-1, 5)
domestic_episode_combined_y_breaks <- seq(-1, 5, by = 1)
domestic_episode_positive_y_limits <- c(-0.1, 5)
domestic_episode_positive_y_breaks <- seq(0, 5, by = 1)

## Family: presidential-period cumulative change ----


# Match each annual GDP observation to the officeholder in place on 31 December.
presidential_period_year_end_matches <- index_series %>%
  select(series_id, year, index_value) %>%
  arrange(series_id, year) %>%
  group_by(series_id) %>%
  mutate(previous_index_value = lag(index_value)) %>%
  ungroup() %>%
  mutate(year_end_date = as.Date(sprintf("%s-12-31", year))) %>%
  cross_join(presidential_periods) %>%
  filter(
    year_end_date >= start_date,
    is.na(end_date) | year_end_date <= end_date
  ) %>%
  # Treat Gómez's rule as one continuous administration for the presidential-period charts.
  mutate(
    officeholder = if_else(
      year >= 1908 & year <= 1935,
      "Juan Vicente Gómez",
      officeholder
    )
  )

# Stop if the chronology leaves an annual observation unmatched or assigns it twice.
presidential_period_match_counts <- presidential_period_year_end_matches %>%
  count(series_id, year, name = "match_count")

if (
  nrow(presidential_period_match_counts) != nrow(index_series) ||
    any(presidential_period_match_counts$match_count != 1)
) {
  stop(
    "The presidential chronology must assign exactly one year-end officeholder to every annual GDP observation.",
    call. = FALSE
  )
}

# Calculate each administration's cumulative change from the prior year-end level.
presidential_period_lines_data <- presidential_period_year_end_matches %>%
  arrange(series_id, year) %>%
  group_by(series_id) %>%
  mutate(
    presidency_sequence = cumsum(officeholder != lag(officeholder, default = ""))
  ) %>%
  group_by(series_id, presidency_sequence, officeholder) %>%
  mutate(
    presidency_start_year = min(year),
    presidency_end_year = max(year),
    term_cumulative_change = index_value / first(
      coalesce(previous_index_value, index_value)
    ) - 1,
    term_final_change = last(term_cumulative_change),
    term_direction = if_else(term_final_change >= 0, "positive", "negative"),
    presidency_label = sprintf("%s (%s-%s)", officeholder, presidency_start_year, presidency_end_year)
  ) %>%
  ungroup()

# Add an explicit zero marker at the start of every presidency's cumulative path.
presidential_period_lines_starts <- presidential_period_lines_data %>%
  group_by(series_id, presidency_sequence, officeholder) %>%
  slice_min(year, n = 1, with_ties = FALSE) %>%
  transmute(
    series_id,
    presidency_sequence,
    officeholder,
    officeholding_category,
    year = if_else(year == min(index_series$year), year, year - 1),
    term_cumulative_change = 0,
    term_direction,
    presidency_label
  ) %>%
  ungroup()

# Identify the final annual observation for each presidency.
presidential_period_lines_ends <- presidential_period_lines_data %>%
  group_by(series_id, presidency_sequence, officeholder) %>%
  mutate(term_observations = n()) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup()

presidential_period_lines_plot_data <- bind_rows(
  presidential_period_lines_data %>%
    select(
      series_id,
      presidency_sequence,
      officeholder,
      officeholding_category,
      year,
      term_cumulative_change,
      term_direction,
      presidency_label
    ),
  presidential_period_lines_starts
) %>%
  arrange(series_id, presidency_sequence, year) %>%
  mutate(
    term_direction = factor(term_direction, levels = names(growth_colors))
  )

presidential_period_source_caption <- build_source_caption(
  "Baptista (2008); Garay (2019); BCV (2016); Focus Economics (2026)",
  calculations = TRUE
)

### Graph 09: PIB real acumulado por período presidencial ----
gdp_presidential_period_lines_data <- presidential_period_lines_plot_data %>%
  filter(series_id == "PIB real")

gdp_presidential_period_lines_starts <- presidential_period_lines_starts %>%
  filter(series_id == "PIB real")

gdp_presidential_period_lines_ends <- presidential_period_lines_ends %>%
  filter(series_id == "PIB real")

gdp_presidential_period_lines_labels <- gdp_presidential_period_lines_ends %>%
  filter(
    term_observations >= 2,
    abs(term_final_change) >= 0.20 |
      (presidency_start_year >= 1900 & presidency_start_year < 2000) |
      (presidency_start_year < 1900 & term_observations >= 5) |
      grepl("Chávez|Maduro", officeholder)
  ) %>%
  mutate(
    label = sprintf(
      "%s (%s-%s)",
      if_else(
        grepl("Pérez Jiménez", officeholder),
        "Pérez Jiménez",
        sub("^.* ", "", officeholder)
      ),
      presidency_start_year,
      presidency_end_year
    )
  )

gdp_presidential_period_lines_positive_labels <- gdp_presidential_period_lines_labels %>%
  filter(term_final_change >= 0.09, term_final_change < 1.5)

gdp_presidential_period_lines_high_positive_labels <- gdp_presidential_period_lines_labels %>%
  filter(term_final_change >= 1.5)

gdp_presidential_period_lines_negative_labels <- gdp_presidential_period_lines_labels %>%
  filter(term_final_change < 0.09, term_final_change > -0.50)

gdp_presidential_period_lines_deep_negative_labels <- gdp_presidential_period_lines_labels %>%
  filter(term_final_change <= -0.50)

gdp_presidential_period_lines <- gdp_presidential_period_lines_data %>%
  ggplot(
    aes(
      x = year,
      y = term_cumulative_change,
      group = presidency_sequence,
      color = term_direction
    )
  ) +
  historical_event_reference_layers() +
  geom_hline(yintercept = 0, linewidth = 0.4, color = presentation_colors[["ink"]]) +
  geom_line(linewidth = domestic_episode_linewidth, alpha = domestic_episode_line_alpha) +
  geom_point(
    data = gdp_presidential_period_lines_starts,
    shape = 21,
    fill = domestic_episode_start_fill,
    stroke = presentation_point_stroke,
    size = domestic_episode_marker_size,
    show.legend = FALSE
  ) +
  geom_point(
    data = gdp_presidential_period_lines_ends,
    shape = 16,
    size = domestic_episode_marker_size,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = gdp_presidential_period_lines_positive_labels,
    aes(label = label),
    family = presentation_font_family,
    size = presentation_label_small_text_size,
    color = presentation_colors[["ink"]],
    fill = "white",
    label.size = presentation_label_box_linewidth,
    label.padding = presentation_label_padding,
    label.r = presentation_label_radius,
    box.padding = presentation_label_box_padding,
    point.padding = presentation_label_point_padding,
    min.segment.length = 0,
    nudge_y = 0.8,
    segment.color = presentation_colors[["muted"]],
    segment.size = presentation_label_segment_size,
    max.overlaps = Inf,
    max.time = 4,
    seed = 1234,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = gdp_presidential_period_lines_high_positive_labels,
    aes(label = label),
    family = presentation_font_family,
    size = presentation_label_small_text_size,
    color = presentation_colors[["ink"]],
    fill = "white",
    label.size = presentation_label_box_linewidth,
    label.padding = presentation_label_padding,
    label.r = presentation_label_radius,
    box.padding = presentation_label_box_padding,
    point.padding = presentation_label_point_padding,
    min.segment.length = 0,
    nudge_y = -0.20,
    segment.color = presentation_colors[["muted"]],
    segment.size = presentation_label_segment_size,
    max.overlaps = Inf,
    max.time = 4,
    seed = 1234,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = gdp_presidential_period_lines_negative_labels,
    aes(label = label),
    family = presentation_font_family,
    size = presentation_label_small_text_size,
    color = presentation_colors[["ink"]],
    fill = "white",
    label.size = presentation_label_box_linewidth,
    label.padding = presentation_label_padding,
    label.r = presentation_label_radius,
    box.padding = presentation_label_box_padding,
    point.padding = presentation_label_point_padding,
    min.segment.length = 0,
    nudge_y = -0.55,
    segment.color = presentation_colors[["muted"]],
    segment.size = presentation_label_segment_size,
    max.overlaps = Inf,
    max.time = 4,
    seed = 1234,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = gdp_presidential_period_lines_deep_negative_labels,
    aes(label = label),
    family = presentation_font_family,
    size = presentation_label_small_text_size,
    color = presentation_colors[["ink"]],
    fill = "white",
    label.size = presentation_label_box_linewidth,
    label.padding = presentation_label_padding,
    label.r = presentation_label_radius,
    box.padding = presentation_label_box_padding,
    point.padding = presentation_label_point_padding,
    min.segment.length = 0,
    nudge_y = -0.15,
    segment.color = presentation_colors[["muted"]],
    segment.size = presentation_label_segment_size,
    max.overlaps = Inf,
    max.time = 4,
    seed = 1234,
    show.legend = FALSE
  ) +
  scale_color_manual(values = growth_colors, labels = growth_labels, name = NULL) +
  presentation_full_history_year_axis() +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(-1, 2.5),
    breaks = seq(-1, 2.5, by = 0.5)
  ) +
  labs(
    title = "Variación del PIB real acumulada por período presidencial",
    x = NULL,
    y = "Variación acumulada",
    subtitle = "Crecimiento/caída desde el cierre previo al primer año de cada administracion.",
    caption = append_caption_note(presidential_period_source_caption, "Cada año se asigna a quién ocupaba el Ejecutivo el 31 de diciembre; transiciones que cierran el año se mantienen como administraciones separadas.")
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family)

gdp_presidential_period_lines <- apply_presentation_plot_style(gdp_presidential_period_lines)

save_plot_variants(
  filename = file.path(figure_dir, "gdp_presidential_period_lines.png"),
  plot = gdp_presidential_period_lines,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_presidential_period_lines)
message("Wrote ", file.path(figure_dir, "gdp_presidential_period_lines.png"))

### Graph 10: PIB real acumulado por período presidencial (escala pseudo-logarítmica) ----
gdp_presidential_period_lines_pseudo_log_maduro_label <- gdp_presidential_period_lines_ends %>%
  filter(grepl("Maduro", officeholder)) %>%
  mutate(label = sprintf("Maduro (%s-%s)", presidency_start_year, presidency_end_year))

gdp_presidential_period_lines_pseudo_log <- gdp_presidential_period_lines +
  scale_y_continuous(
    trans = pseudo_log_trans(sigma = 0.10, base = 10),
    labels = label_percent(accuracy = 1),
    limits = c(-1, 7),
    breaks = c(-1, -0.50, -0.25, 0, 0.25, 0.50, 1, 2.5, 5, 7)
  ) +
  labs(
    subtitle = "Crecimiento/caída desde el cierre previo al primer año de cada administracion.",
    y = "Variación acumulada (escala pseudo-logarítmica)",
    caption = append_caption_note(
      presidential_period_source_caption,
      "Cada año se asigna a quién ocupaba el Ejecutivo el 31 de diciembre; transiciones que cierran el año se mantienen como administraciones separadas."
    )
  ) +
  geom_label_repel(
    data = gdp_presidential_period_lines_pseudo_log_maduro_label,
    aes(label = label),
    family = presentation_font_family,
    size = presentation_label_small_text_size,
    color = presentation_colors[["ink"]],
    fill = "white",
    label.size = presentation_label_box_linewidth,
    label.padding = presentation_label_padding,
    label.r = presentation_label_radius,
    box.padding = presentation_label_box_padding,
    point.padding = presentation_label_point_padding,
    min.segment.length = 0,
    nudge_y = 0.30,
    segment.color = presentation_colors[["muted"]],
    segment.size = presentation_label_segment_size,
    max.overlaps = Inf,
    max.time = 4,
    seed = 1234,
    show.legend = FALSE
  )

save_plot_variants(
  filename = file.path(figure_dir, "gdp_presidential_period_lines_pseudo_log.png"),
  plot = gdp_presidential_period_lines_pseudo_log,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_presidential_period_lines_pseudo_log)
message("Wrote ", file.path(figure_dir, "gdp_presidential_period_lines_pseudo_log.png"))

### Graph 11: PIB real per cápita acumulado por período presidencial ----
gdp_per_capita_presidential_period_lines_data <- presidential_period_lines_plot_data %>%
  filter(series_id == series_labels[["gdp_per_capita"]])

gdp_per_capita_presidential_period_lines_starts <- presidential_period_lines_starts %>%
  filter(series_id == series_labels[["gdp_per_capita"]])

gdp_per_capita_presidential_period_lines_ends <- presidential_period_lines_ends %>%
  filter(series_id == series_labels[["gdp_per_capita"]])

gdp_per_capita_presidential_period_lines_labels <- gdp_per_capita_presidential_period_lines_ends %>%
  filter(
    term_observations >= 2,
    abs(term_final_change) >= 0.20 |
      (presidency_start_year >= 1900 & presidency_start_year < 2000) |
      (presidency_start_year < 1900 & term_observations >= 5) |
      grepl("Chávez|Maduro", officeholder)
  ) %>%
  mutate(
    label = sprintf(
      "%s (%s-%s)",
      if_else(
        grepl("Pérez Jiménez", officeholder),
        "Pérez Jiménez",
        sub("^.* ", "", officeholder)
      ),
      presidency_start_year,
      presidency_end_year
    )
  )

gdp_per_capita_presidential_period_lines_positive_labels <- gdp_per_capita_presidential_period_lines_labels %>%
  filter(term_final_change >= 0.09, term_final_change < 1.5)

gdp_per_capita_presidential_period_lines_high_positive_labels <- gdp_per_capita_presidential_period_lines_labels %>%
  filter(term_final_change >= 1.5)

gdp_per_capita_presidential_period_lines_negative_labels <- gdp_per_capita_presidential_period_lines_labels %>%
  filter(term_final_change < 0.09, term_final_change > -0.50)

gdp_per_capita_presidential_period_lines_deep_negative_labels <- gdp_per_capita_presidential_period_lines_labels %>%
  filter(term_final_change <= -0.50)

gdp_per_capita_presidential_period_lines <- gdp_per_capita_presidential_period_lines_data %>%
  ggplot(
    aes(
      x = year,
      y = term_cumulative_change,
      group = presidency_sequence,
      color = term_direction
    )
  ) +
  historical_event_reference_layers() +
  geom_hline(yintercept = 0, linewidth = 0.4, color = presentation_colors[["ink"]]) +
  geom_line(linewidth = domestic_episode_linewidth, alpha = domestic_episode_line_alpha) +
  geom_point(
    data = gdp_per_capita_presidential_period_lines_starts,
    shape = 21,
    fill = domestic_episode_start_fill,
    stroke = presentation_point_stroke,
    size = domestic_episode_marker_size,
    show.legend = FALSE
  ) +
  geom_point(
    data = gdp_per_capita_presidential_period_lines_ends,
    shape = 16,
    size = domestic_episode_marker_size,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = gdp_per_capita_presidential_period_lines_positive_labels,
    aes(label = label),
    family = presentation_font_family,
    size = presentation_label_small_text_size,
    color = presentation_colors[["ink"]],
    fill = "white",
    label.size = presentation_label_box_linewidth,
    label.padding = presentation_label_padding,
    label.r = presentation_label_radius,
    box.padding = presentation_label_box_padding,
    point.padding = presentation_label_point_padding,
    min.segment.length = 0,
    nudge_y = 0.8,
    segment.color = presentation_colors[["muted"]],
    segment.size = presentation_label_segment_size,
    max.overlaps = Inf,
    max.time = 4,
    seed = 1234,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = gdp_per_capita_presidential_period_lines_high_positive_labels,
    aes(label = label),
    family = presentation_font_family,
    size = presentation_label_small_text_size,
    color = presentation_colors[["ink"]],
    fill = "white",
    label.size = presentation_label_box_linewidth,
    label.padding = presentation_label_padding,
    label.r = presentation_label_radius,
    box.padding = presentation_label_box_padding,
    point.padding = presentation_label_point_padding,
    min.segment.length = 0,
    nudge_y = -0.20,
    segment.color = presentation_colors[["muted"]],
    segment.size = presentation_label_segment_size,
    max.overlaps = Inf,
    max.time = 4,
    seed = 1234,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = gdp_per_capita_presidential_period_lines_negative_labels,
    aes(label = label),
    family = presentation_font_family,
    size = presentation_label_small_text_size,
    color = presentation_colors[["ink"]],
    fill = "white",
    label.size = presentation_label_box_linewidth,
    label.padding = presentation_label_padding,
    label.r = presentation_label_radius,
    box.padding = presentation_label_box_padding,
    point.padding = presentation_label_point_padding,
    min.segment.length = 0,
    nudge_y = -0.55,
    segment.color = presentation_colors[["muted"]],
    segment.size = presentation_label_segment_size,
    max.overlaps = Inf,
    max.time = 4,
    seed = 1234,
    show.legend = FALSE
  ) +
  geom_label_repel(
    data = gdp_per_capita_presidential_period_lines_deep_negative_labels,
    aes(label = label),
    family = presentation_font_family,
    size = presentation_label_small_text_size,
    color = presentation_colors[["ink"]],
    fill = "white",
    label.size = presentation_label_box_linewidth,
    label.padding = presentation_label_padding,
    label.r = presentation_label_radius,
    box.padding = presentation_label_box_padding,
    point.padding = presentation_label_point_padding,
    min.segment.length = 0,
    nudge_y = -0.15,
    segment.color = presentation_colors[["muted"]],
    segment.size = presentation_label_segment_size,
    max.overlaps = Inf,
    max.time = 4,
    seed = 1234,
    show.legend = FALSE
  ) +
  scale_color_manual(values = growth_colors, labels = growth_labels, name = NULL) +
  presentation_full_history_year_axis() +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(-1, 2.5),
    breaks = seq(-1, 2.5, by = 0.5)
  ) +
  labs(
    title = "Varición del PIB real per cápita acumulada por período presidencial",
    x = NULL,
    y = "Variación acumulada",
    subtitle = "Crecimiento/caída desde el cierre previo al primer año de cada administracion.",
    caption = append_caption_note(presidential_period_source_caption, "Cada año se asigna a quién ocupaba el Ejecutivo el 31 de diciembre; transiciones que cierran el año se mantienen como administraciones separadas.")
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family)

gdp_per_capita_presidential_period_lines <- apply_presentation_plot_style(gdp_per_capita_presidential_period_lines)

save_plot_variants(
  filename = file.path(figure_dir, "gdp_per_capita_presidential_period_lines.png"),
  plot = gdp_per_capita_presidential_period_lines,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_per_capita_presidential_period_lines)
message("Wrote ", file.path(figure_dir, "gdp_per_capita_presidential_period_lines.png"))

### Graph 12: PIB real per cápita acumulado por período presidencial (escala pseudo-logarítmica) ----
gdp_per_capita_presidential_period_lines_pseudo_log_maduro_label <- gdp_per_capita_presidential_period_lines_ends %>%
  filter(grepl("Maduro", officeholder)) %>%
  mutate(label = sprintf("Maduro (%s-%s)", presidency_start_year, presidency_end_year))

gdp_per_capita_presidential_period_lines_pseudo_log <- gdp_per_capita_presidential_period_lines +
  scale_y_continuous(
    trans = pseudo_log_trans(sigma = 0.10, base = 10),
    labels = label_percent(accuracy = 1),
    limits = c(-1, 7),
    breaks = c(-1, -0.50, -0.25, 0, 0.25, 0.50, 1, 2.5, 5, 7)
  ) +
  labs(
    subtitle = "Crecimiento/caída desde el cierre previo al primer año de cada administracion.",
    y = "Variación acumulada (escala pseudo-logarítmica)",
    caption = append_caption_note(
      presidential_period_source_caption,
      "Cada año se asigna a quién ocupaba el Ejecutivo el 31 de diciembre; transiciones que cierran el año se mantienen como administraciones separadas."
    )
  ) +
  geom_label_repel(
    data = gdp_per_capita_presidential_period_lines_pseudo_log_maduro_label,
    aes(label = label),
    family = presentation_font_family,
    size = presentation_label_small_text_size,
    color = presentation_colors[["ink"]],
    fill = "white",
    label.size = presentation_label_box_linewidth,
    label.padding = presentation_label_padding,
    label.r = presentation_label_radius,
    box.padding = presentation_label_box_padding,
    point.padding = presentation_label_point_padding,
    min.segment.length = 0,
    nudge_y = 0.30,
    segment.color = presentation_colors[["muted"]],
    segment.size = presentation_label_segment_size,
    max.overlaps = Inf,
    max.time = 4,
    seed = 1234,
    show.legend = FALSE
  )

save_plot_variants(
  filename = file.path(figure_dir, "gdp_per_capita_presidential_period_lines_pseudo_log.png"),
  plot = gdp_per_capita_presidential_period_lines_pseudo_log,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_per_capita_presidential_period_lines_pseudo_log)
message("Wrote ", file.path(figure_dir, "gdp_per_capita_presidential_period_lines_pseudo_log.png"))

## Family: domestic growth episodes ----
### Graph 13: Episodios del PIB real ----
gdp_episode_lines_data <- episodes %>%
  filter(series_id == "PIB real")

gdp_episode_lines <- gdp_episode_lines_data %>%
  ggplot(aes(x = year, y = episode_cumulative_change, group = episode_group, color = growth_direction)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = presentation_colors[["ink"]]) +
  geom_line(linewidth = domestic_episode_linewidth, alpha = domestic_episode_line_alpha) +
  scale_color_manual(values = growth_colors, labels = growth_labels, name = NULL) +
  presentation_full_history_year_axis() +
  scale_y_continuous(labels = label_percent(accuracy = 1), limits = domestic_episode_combined_y_limits, breaks = domestic_episode_combined_y_breaks) +
  labs(
    title = "Episodios históricos de expansión y contracción económica (PIB real)",
    x = NULL,
    y = "Variación acumulada",
    subtitle = "Cada línea sigue un episodio acumulado de expansión o contracción continua del PIB real.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  historical_event_reference_layers()

gdp_episode_lines <- apply_presentation_plot_style(gdp_episode_lines)

save_plot_variants(
  filename = file.path(figure_dir, "gdp_episode_lines.png"),
  plot = gdp_episode_lines,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_episode_lines)
message("Wrote ", file.path(figure_dir, "gdp_episode_lines.png"))

### Graph 14: Episodios per cápita ----
gdp_per_capita_episode_lines_data <- episodes %>%
  filter(series_id == "PIB real per cápita")

gdp_per_capita_episode_lines <- gdp_per_capita_episode_lines_data %>%
  ggplot(aes(x = year, y = episode_cumulative_change, group = episode_group, color = growth_direction)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = presentation_colors[["ink"]]) +
  geom_line(linewidth = domestic_episode_linewidth, alpha = domestic_episode_line_alpha) +
  scale_color_manual(values = growth_colors, labels = growth_labels, name = NULL) +
  presentation_full_history_year_axis() +
  scale_y_continuous(labels = label_percent(accuracy = 1), limits = domestic_episode_combined_y_limits, breaks = domestic_episode_combined_y_breaks) +
  labs(
    title = "Episodios históricos de expansión y contracción económica (PIB pc. real)",
    x = NULL,
    y = "Variación acumulada",
    subtitle = "Cada línea sigue un episodio acumulado de expansión o contracción continua del PIB real per cápita.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  historical_event_reference_layers()

gdp_per_capita_episode_lines <- apply_presentation_plot_style(gdp_per_capita_episode_lines)

save_plot_variants(
  filename = file.path(figure_dir, "gdp_per_capita_episode_lines.png"),
  plot = gdp_per_capita_episode_lines,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_per_capita_episode_lines)
message("Wrote ", file.path(figure_dir, "gdp_per_capita_episode_lines.png"))

### Graph 15: Episodios del PIB real y per cápita ----
episode_lines_faceted <- ggplot(
  episodes,
  aes(
    x = year,
    y = episode_cumulative_change,
    group = episode_group,
    color = growth_direction
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = presentation_colors[["ink"]]) +
  geom_line(linewidth = domestic_episode_linewidth, alpha = domestic_episode_line_alpha) +
  facet_wrap(vars(series_id), ncol = 1) +
  scale_color_manual(values = growth_colors, labels = growth_labels, name = NULL) +
  presentation_full_history_year_axis() +
  scale_y_continuous(labels = label_percent(accuracy = 1), limits = domestic_episode_combined_y_limits, breaks = domestic_episode_combined_y_breaks) +
  labs(
    title = "Episodios históricos",
    x = NULL,
    y = "Variación acumulada",
    subtitle = "Compara episodios acumulados del PIB real y del PIB real per cápita.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  historical_event_reference_layers()

episode_lines_faceted <- apply_presentation_plot_style(episode_lines_faceted)

save_plot_variants(
  filename = file.path(figure_dir, "episode_lines_faceted.png"),
  plot = episode_lines_faceted,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(episode_lines_faceted)
message("Wrote ", file.path(figure_dir, "episode_lines_faceted.png"))

### Graph 16: Episodios positivos del PIB real ----
gdp_positive_episode_lines_data <- episodes %>% filter(series_id == "PIB real", phase == "expansion")
gdp_positive_episode_lines_starts <- gdp_positive_episode_lines_data %>%
  group_by(episode_group) %>%
  slice_min(year, n = 1, with_ties = FALSE) %>%
  ungroup()
gdp_positive_episode_lines_ends <- gdp_positive_episode_lines_data %>%
  group_by(episode_group) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup()
gdp_positive_episode_lines_2000s_label <- gdp_positive_episode_lines_data %>%
  group_by(episode_group, episode_label) %>%
  summarise(
    start_year = min(year, na.rm = TRUE),
    end_year = max(year, na.rm = TRUE),
    final_change = episode_cumulative_change[which.max(year)],
    .groups = "drop"
  ) %>%
  filter(start_year >= 2000, start_year < 2010) %>%
  slice_max(order_by = final_change, n = 1, with_ties = FALSE) %>%
  inner_join(
    gdp_positive_episode_lines_ends %>%
      select(episode_group, year, episode_cumulative_change),
    by = "episode_group"
  ) %>%
  mutate(label = sprintf("(%s–%s)", start_year, end_year))
gdp_positive_episode_lines_labels <- bind_rows(
  build_episode_top_label_data(episodes, "PIB real", "expansion", top_n = 5),
  gdp_positive_episode_lines_2000s_label
) %>%
  distinct(episode_group, .keep_all = TRUE)
gdp_positive_episode_lines <- gdp_positive_episode_lines_data %>%
  ggplot(aes(x = year, y = episode_cumulative_change, group = episode_group, color = growth_direction)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = presentation_colors[["ink"]]) +
  geom_line(linewidth = domestic_episode_linewidth, alpha = domestic_episode_line_alpha) +
  geom_point(data = gdp_positive_episode_lines_starts, shape = 21, fill = domestic_episode_start_fill, stroke = presentation_point_stroke, size = domestic_episode_marker_size, show.legend = FALSE) +
  geom_point(data = gdp_positive_episode_lines_ends, shape = 16, size = domestic_episode_marker_size, show.legend = FALSE) +
  geom_label_repel(data = gdp_positive_episode_lines_labels, aes(x = year, y = episode_cumulative_change, label = label), inherit.aes = FALSE, color = presentation_colors[["ink"]], family = presentation_font_family, size = presentation_label_text_size, fill = "white", label.size = presentation_label_box_linewidth, label.padding = presentation_label_padding, label.r = presentation_label_radius, min.segment.length = 0, segment.color = presentation_colors[["muted"]], segment.size = presentation_label_segment_size, box.padding = presentation_label_box_padding, point.padding = presentation_label_point_padding, max.overlaps = Inf, seed = 1234, show.legend = FALSE) +
  scale_color_manual(values = growth_colors, labels = growth_labels, name = NULL) +
  presentation_full_history_year_axis() +
  scale_y_continuous(labels = label_percent(accuracy = 1), breaks = domestic_episode_positive_y_breaks) +
  coord_cartesian(ylim = domestic_episode_positive_y_limits, clip = "off", expand = FALSE) +
  labs(
    title = "Episodios de crecimiento económico (PIB real)",
    x = NULL,
    y = "Variación acumulada",
    subtitle = "Periodos de expansión ininterrumpida.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(plot.margin = margin(5.5, 78, 5.5, 5.5)) +
  historical_event_reference_layers()

gdp_positive_episode_lines <- apply_presentation_plot_style(gdp_positive_episode_lines)

save_plot_variants(
  filename = file.path(figure_dir, "gdp_positive_episode_lines.png"),
  plot = gdp_positive_episode_lines,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_positive_episode_lines)
message("Wrote ", file.path(figure_dir, "gdp_positive_episode_lines.png"))

### Graph 17: Episodios negativos del PIB real ----
gdp_negative_episode_lines_data <- episodes %>% filter(series_id == "PIB real", phase == "contraction")
gdp_negative_episode_lines_starts <- gdp_negative_episode_lines_data %>%
  group_by(episode_group) %>%
  slice_min(year, n = 1, with_ties = FALSE) %>%
  ungroup()
gdp_negative_episode_lines_ends <- gdp_negative_episode_lines_data %>%
  group_by(episode_group) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup()
gdp_negative_episode_lines_labels <- build_episode_top_label_data(episodes, "PIB real", "contraction", top_n = 5)
gdp_negative_episode_lines <- gdp_negative_episode_lines_data %>%
  ggplot(aes(x = year, y = episode_cumulative_change, group = episode_group, color = growth_direction)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = presentation_colors[["ink"]]) +
  geom_line(linewidth = domestic_episode_linewidth, alpha = domestic_episode_line_alpha) +
  geom_point(data = gdp_negative_episode_lines_starts, shape = 21, fill = domestic_episode_start_fill, stroke = presentation_point_stroke, size = domestic_episode_marker_size, show.legend = FALSE) +
  geom_point(data = gdp_negative_episode_lines_ends, shape = 16, size = domestic_episode_marker_size, show.legend = FALSE) +
  geom_label_repel(data = gdp_negative_episode_lines_labels, aes(x = year, y = episode_cumulative_change, label = label), inherit.aes = FALSE, color = presentation_colors[["ink"]], family = presentation_font_family, size = presentation_label_text_size, fill = "white", label.size = presentation_label_box_linewidth, label.padding = presentation_label_padding, label.r = presentation_label_radius, min.segment.length = 0, segment.color = presentation_colors[["muted"]], segment.size = presentation_label_segment_size, box.padding = presentation_label_box_padding, point.padding = presentation_label_point_padding, max.overlaps = Inf, seed = 1234, show.legend = FALSE) +
  scale_color_manual(values = growth_colors, labels = growth_labels, name = NULL) +
  presentation_full_history_year_axis() +
  scale_y_continuous(labels = label_percent(accuracy = 1), limits = c(-1, 0.10), breaks = seq(-1, 0.10, by = 0.1)) +
  coord_cartesian(clip = "off", expand = FALSE) +
  labs(
    title = "Episodios de contracción económica (PIB real)",
    x = NULL,
    y = "Variación acumulada",
    subtitle = "Periodos de contracción ininterrumpida.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(plot.margin = margin(5.5, 78, 5.5, 5.5)) +
  historical_event_reference_layers()

gdp_negative_episode_lines <- apply_presentation_plot_style(gdp_negative_episode_lines)

save_plot_variants(
  filename = file.path(figure_dir, "gdp_negative_episode_lines.png"),
  plot = gdp_negative_episode_lines,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_negative_episode_lines)
message("Wrote ", file.path(figure_dir, "gdp_negative_episode_lines.png"))

### Graph 18: Episodios positivos per cápita ----
gdp_per_capita_positive_episode_lines_data <- episodes %>% filter(series_id == "PIB real per cápita", phase == "expansion")
gdp_per_capita_positive_episode_lines_starts <- gdp_per_capita_positive_episode_lines_data %>%
  group_by(episode_group) %>%
  slice_min(year, n = 1, with_ties = FALSE) %>%
  ungroup()
gdp_per_capita_positive_episode_lines_ends <- gdp_per_capita_positive_episode_lines_data %>%
  group_by(episode_group) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup()
gdp_per_capita_positive_episode_lines_2000s_label <- gdp_per_capita_positive_episode_lines_data %>%
  group_by(episode_group, episode_label) %>%
  summarise(
    start_year = min(year, na.rm = TRUE),
    end_year = max(year, na.rm = TRUE),
    final_change = episode_cumulative_change[which.max(year)],
    .groups = "drop"
  ) %>%
  filter(start_year >= 2000, start_year < 2010) %>%
  slice_max(order_by = final_change, n = 1, with_ties = FALSE) %>%
  inner_join(
    gdp_per_capita_positive_episode_lines_ends %>%
      select(episode_group, year, episode_cumulative_change),
    by = "episode_group"
  ) %>%
  mutate(label = sprintf("(%s–%s)", start_year, end_year))
gdp_per_capita_positive_episode_lines_labels <- bind_rows(
  build_episode_top_label_data(episodes, "PIB real per cápita", "expansion", top_n = 5),
  gdp_per_capita_positive_episode_lines_2000s_label
) %>%
  distinct(episode_group, .keep_all = TRUE)
gdp_per_capita_positive_episode_lines <- gdp_per_capita_positive_episode_lines_data %>%
  ggplot(aes(x = year, y = episode_cumulative_change, group = episode_group, color = growth_direction)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = presentation_colors[["ink"]]) +
  geom_line(linewidth = domestic_episode_linewidth, alpha = domestic_episode_line_alpha) +
  geom_point(data = gdp_per_capita_positive_episode_lines_starts, shape = 21, fill = domestic_episode_start_fill, stroke = presentation_point_stroke, size = domestic_episode_marker_size, show.legend = FALSE) +
  geom_point(data = gdp_per_capita_positive_episode_lines_ends, shape = 16, size = domestic_episode_marker_size, show.legend = FALSE) +
  geom_label_repel(data = gdp_per_capita_positive_episode_lines_labels, aes(x = year, y = episode_cumulative_change, label = label), inherit.aes = FALSE, color = presentation_colors[["ink"]], family = presentation_font_family, size = presentation_label_text_size, fill = "white", label.size = presentation_label_box_linewidth, label.padding = presentation_label_padding, label.r = presentation_label_radius, min.segment.length = 0, segment.color = presentation_colors[["muted"]], segment.size = presentation_label_segment_size, box.padding = presentation_label_box_padding, point.padding = presentation_label_point_padding, max.overlaps = Inf, seed = 1234, show.legend = FALSE) +
  scale_color_manual(values = growth_colors, labels = growth_labels, name = NULL) +
  presentation_full_history_year_axis() +
  scale_y_continuous(labels = label_percent(accuracy = 1), breaks = domestic_episode_positive_y_breaks) +
  coord_cartesian(ylim = domestic_episode_positive_y_limits, clip = "off", expand = FALSE) +
  labs(
    title = "Episodios de crecimiento económico (PIB per cápita real)",
    x = NULL,
    y = "Variación acumulada",
    subtitle = "Periodos de expansión ininterrumpida.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(plot.margin = margin(5.5, 78, 5.5, 5.5)) +
  historical_event_reference_layers()

gdp_per_capita_positive_episode_lines <- apply_presentation_plot_style(gdp_per_capita_positive_episode_lines)

save_plot_variants(
  filename = file.path(figure_dir, "gdp_per_capita_positive_episode_lines.png"),
  plot = gdp_per_capita_positive_episode_lines,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_per_capita_positive_episode_lines)
message("Wrote ", file.path(figure_dir, "gdp_per_capita_positive_episode_lines.png"))

### Graph 19: Episodios negativos per cápita ----
gdp_per_capita_negative_episode_lines_data <- episodes %>% filter(series_id == "PIB real per cápita", phase == "contraction")
gdp_per_capita_negative_episode_lines_starts <- gdp_per_capita_negative_episode_lines_data %>%
  group_by(episode_group) %>%
  slice_min(year, n = 1, with_ties = FALSE) %>%
  ungroup()
gdp_per_capita_negative_episode_lines_ends <- gdp_per_capita_negative_episode_lines_data %>%
  group_by(episode_group) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup()
gdp_per_capita_negative_episode_lines_labels <- build_episode_top_label_data(episodes, "PIB real per cápita", "contraction", top_n = 5)
gdp_per_capita_negative_episode_lines <- gdp_per_capita_negative_episode_lines_data %>%
  ggplot(aes(x = year, y = episode_cumulative_change, group = episode_group, color = growth_direction)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = presentation_colors[["ink"]]) +
  geom_line(linewidth = domestic_episode_linewidth, alpha = domestic_episode_line_alpha) +
  geom_point(data = gdp_per_capita_negative_episode_lines_starts, shape = 21, fill = domestic_episode_start_fill, stroke = presentation_point_stroke, size = domestic_episode_marker_size, show.legend = FALSE) +
  geom_point(data = gdp_per_capita_negative_episode_lines_ends, shape = 16, size = domestic_episode_marker_size, show.legend = FALSE) +
  geom_label_repel(data = gdp_per_capita_negative_episode_lines_labels, aes(x = year, y = episode_cumulative_change, label = label), inherit.aes = FALSE, color = presentation_colors[["ink"]], family = presentation_font_family, size = presentation_label_text_size, fill = "white", label.size = presentation_label_box_linewidth, label.padding = presentation_label_padding, label.r = presentation_label_radius, min.segment.length = 0, segment.color = presentation_colors[["muted"]], segment.size = presentation_label_segment_size, box.padding = presentation_label_box_padding, point.padding = presentation_label_point_padding, max.overlaps = Inf, seed = 1234, show.legend = FALSE) +
  scale_color_manual(values = growth_colors, labels = growth_labels, name = NULL) +
  presentation_full_history_year_axis() +
  scale_y_continuous(labels = label_percent(accuracy = 1), limits = c(-1, 0.10), breaks = seq(-1, 0.10, by = 0.1)) +
  coord_cartesian(clip = "off", expand = FALSE) +
  labs(
    title = "Episodios de contracción económica (PIB per cápita real)",
    x = NULL,
    y = "Variación acumulada",
    subtitle = "Periodos de contracción ininterrumpida.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(plot.margin = margin(5.5, 78, 5.5, 5.5)) +
  historical_event_reference_layers()

gdp_per_capita_negative_episode_lines <- apply_presentation_plot_style(gdp_per_capita_negative_episode_lines)

save_plot_variants(
  filename = file.path(figure_dir, "gdp_per_capita_negative_episode_lines.png"),
  plot = gdp_per_capita_negative_episode_lines,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(gdp_per_capita_negative_episode_lines)
message("Wrote ", file.path(figure_dir, "gdp_per_capita_negative_episode_lines.png"))

## Family: international negative episodes and episode comparisons ----


# Shared international-collapse styling preserves the same hierarchy across sources.
international_negative_y_limits <- c(-1, 0.1)
international_negative_y_breaks <- seq(-1, 0.1, by = 0.1)
international_negative_baseline_endpoint_size <- 1.4
international_negative_top_line_alpha <- 0.78
international_negative_top_endpoint_alpha <- 0.85

### Graph 20: Episodios negativos internacionales Maddison ----
maddison_negative_summary <- maddison_episode_summary %>%
  filter(country_code != "VEN", phase == "contraction", start_year >= 1919, start_year <= 2026) %>%
  mutate(episode_key = paste(country_code, episode_id, sep = "_"))

maddison_post_1950_worst_summary <- maddison_negative_summary %>%
  filter(start_year >= 1950) %>%
  slice_min(cumulative_growth, n = 10, with_ties = FALSE) %>%
  arrange(cumulative_growth)

maddison_pre_1950_worst_summary <- maddison_negative_summary %>%
  filter(start_year < 1950) %>%
  slice_min(cumulative_growth, n = 10, with_ties = FALSE) %>%
  arrange(cumulative_growth)

maddison_japan_wwii_summary <- maddison_negative_summary %>%
  filter(country_code == "JPN", start_year == 1941, end_year == 1947)

if (nrow(maddison_japan_wwii_summary) != 1) {
  stop("The Maddison Japan 1941-1947 contraction could not be identified.")
}

maddison_pre_1950_highlight_summary <- bind_rows(
  maddison_pre_1950_worst_summary,
  maddison_japan_wwii_summary
) %>%
  distinct(episode_key, .keep_all = TRUE) %>%
  arrange(cumulative_growth)

maddison_post_1950_top_episodes <- maddison_post_1950_worst_summary$episode_key
maddison_pre_1950_top_episodes <- maddison_pre_1950_highlight_summary$episode_key

maddison_negative_episode_lines_data <- maddison_episode_path %>%
  filter(country_code != "VEN", phase == "contraction", year >= 1919, year <= 2026) %>%
  mutate(
    episode_key = paste(country_code, episode_id, sep = "_"),
    highlight_group = case_when(
      episode_key %in% maddison_post_1950_top_episodes ~ "post_1950_top_10",
      episode_key %in% maddison_pre_1950_top_episodes ~ "pre_1950_top_10",
      TRUE ~ "baseline"
    )
  )

maddison_negative_baseline_endpoints <- maddison_negative_episode_lines_data %>%
  filter(highlight_group == "baseline") %>%
  group_by(episode_key) %>%
  slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
  ungroup()

maddison_venezuela_plot_data <- episodes %>%
  filter(
    as.character(series_id) == as.character(series_labels[["gdp_per_capita"]]),
    phase == "contraction",
    year >= 1950,
    year <= 2026
  ) %>%
  group_by(episode_group) %>%
  mutate(episode_min_change = min(episode_cumulative_change, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(episode_min_change == min(episode_min_change, na.rm = TRUE))

maddison_label_data <- build_negative_episode_labels(
  maddison_negative_episode_lines_data,
  maddison_post_1950_worst_summary,
  maddison_venezuela_plot_data
) %>%
  filter(label_group == "Venezuela") %>%
  bind_rows(
    build_negative_episode_labels(
      maddison_negative_episode_lines_data,
      maddison_post_1950_worst_summary,
      maddison_venezuela_plot_data
    ) %>% filter(label_group == "Top 10 episodios"),
    build_negative_episode_labels(
      maddison_negative_episode_lines_data,
      maddison_pre_1950_highlight_summary,
      maddison_venezuela_plot_data
    ) %>% filter(label_group == "Top 10 episodios")
  ) %>%
  mutate(
    label_nudge_y = case_when(
      grepl("^(MNE|AZE|MDA)", label) ~ 0.03,
      grepl("^(IRQ|GEO|LBR)", label) ~ -0.03,
      TRUE ~ 0
    )
  )
maddison_post_1950_label_data <- maddison_label_data %>% filter(episode_key %in% maddison_post_1950_top_episodes)
maddison_pre_1950_label_data <- maddison_label_data %>% filter(episode_key %in% maddison_pre_1950_top_episodes)
maddison_venezuela_label_data <- maddison_label_data %>% filter(label_group == "Venezuela")

maddison_negative_episode_lines <- ggplot() +
  # Reference lines establish no-change and the -15% disaster threshold.
  geom_hline(yintercept = 0, linewidth = 0.3, color = presentation_colors[["ink"]]) +
  geom_hline(yintercept = -0.15, linewidth = 0.55, color = presentation_colors[["accent"]], linetype = "dashed") +
  geom_vline(xintercept = c(1939, 1945), linewidth = 0.35, color = presentation_colors[["ink"]], alpha = 0.55, linetype = "dashed") +
  annotate(
    "text",
    x = -Inf, y = -0.15, label = "-15%",
    color = presentation_colors[["accent"]], family = presentation_font_family,
    fontface = "bold", size = 3.4, hjust = -0.1, vjust = -0.45
  ) +
  geom_line(
    data = maddison_negative_episode_lines_data[maddison_negative_episode_lines_data$highlight_group == "baseline", ],
    aes(x = year, y = episode_cumulative_change, group = episode_key, color = "Otros episodios"),
    linewidth = 0.25, alpha = 0.65
  ) +
  geom_point(
    data = maddison_negative_baseline_endpoints,
    aes(x = year, y = episode_cumulative_change),
    inherit.aes = FALSE,
    color = presentation_colors[["grid"]],
    alpha = 0.65,
    size = international_negative_baseline_endpoint_size,
    show.legend = FALSE
  ) +
  geom_line(
    data = maddison_negative_episode_lines_data[maddison_negative_episode_lines_data$highlight_group == "post_1950_top_10", ],
    aes(x = year, y = episode_cumulative_change, group = episode_key, color = "Top 10 desde 1950"),
    linewidth = 0.75, alpha = international_negative_top_line_alpha
  ) +
  geom_line(
    data = maddison_negative_episode_lines_data[maddison_negative_episode_lines_data$highlight_group == "pre_1950_top_10", ],
    aes(x = year, y = episode_cumulative_change, group = episode_key, color = "Top 10 antes de 1950"),
    linewidth = 0.75, alpha = international_negative_top_line_alpha
  ) +
  geom_line(
    data = maddison_venezuela_plot_data,
    aes(x = year, y = episode_cumulative_change, group = episode_group, color = "Venezuela"),
    linewidth = 1.1, alpha = 0.95
  ) +
  geom_point(
    data = maddison_post_1950_label_data,
    aes(x = year, y = episode_cumulative_change),
    inherit.aes = FALSE, shape = 21, size = 2.4, stroke = presentation_point_stroke,
    color = presentation_colors[["ink"]], fill = presentation_colors[["primary"]],
    alpha = international_negative_top_endpoint_alpha
  ) +
  geom_point(
    data = maddison_pre_1950_label_data,
    aes(x = year, y = episode_cumulative_change),
    inherit.aes = FALSE, shape = 21, size = 2.4, stroke = presentation_point_stroke,
    color = presentation_colors[["ink"]], fill = presentation_palette[["cyan"]],
    alpha = international_negative_top_endpoint_alpha
  ) +
  geom_point(
    data = maddison_venezuela_label_data,
    aes(x = year, y = episode_cumulative_change),
    inherit.aes = FALSE, shape = 21, size = 2.8, stroke = presentation_point_stroke,
    color = presentation_colors[["ink"]], fill = presentation_colors[["venezuela"]]
  ) +
  geom_label_repel(
    data = maddison_label_data,
    aes(x = year, y = episode_cumulative_change, label = label),
    inherit.aes = FALSE, color = presentation_colors[["ink"]], family = presentation_font_family,
    size = presentation_label_small_text_size, fill = "white", label.size = presentation_label_box_linewidth,
    label.padding = presentation_label_padding, label.r = presentation_label_radius,
    box.padding = 0.60, point.padding = 0.55,
    segment.color = presentation_colors[["muted"]], segment.size = presentation_label_segment_size,
    min.segment.length = 0, nudge_x = 0.8, nudge_y = maddison_label_data$label_nudge_y,
    force = 3, max.overlaps = Inf, seed = 1234, show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Otros episodios" = presentation_colors[["grid"]],
      "Top 10 desde 1950" = presentation_colors[["primary"]],
      "Top 10 antes de 1950" = presentation_palette[["cyan"]],
      "Venezuela" = presentation_colors[["venezuela"]]
    ),
    breaks = c("Otros episodios", "Top 10 desde 1950", "Top 10 antes de 1950", "Venezuela"),
    labels = c(
      "Otros episodios" = "Otros episodios",
      "Top 10 desde 1950" = "Peores episodios post-1950",
      "Top 10 antes de 1950" = "Peores episodios pre-1950",
      "Venezuela" = "Venezuela"
    ),
    name = NULL
  ) +
  presentation_recent_year_axis(1919) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = international_negative_y_limits,
    breaks = international_negative_y_breaks
  ) +
  coord_cartesian(clip = "off", expand = FALSE) +
  labs(
    title = "Episodios negativos de PIB real per cápita (Maddison)",
    x = NULL,
    y = "Variación acumulada",
    subtitle = "Compara a Venezuela con las peores contracciones internacionales de Maddison desde 1919; las líneas verticales delimitan la Segunda Guerra Mundial.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(plot.margin = margin(5.5, 70, 5.5, 5.5))

maddison_negative_episode_lines <- apply_presentation_plot_style(maddison_negative_episode_lines)

save_plot_variants(
  filename = file.path(figure_dir, "international_negative_episode_lines_maddison.png"),
  plot = maddison_negative_episode_lines,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(maddison_negative_episode_lines)
message("Wrote ", file.path(figure_dir, "international_negative_episode_lines_maddison.png"))

### Graph 21: Episodios negativos internacionales WDI ----
wdi_negative_summary <- wdi_pc_episode_summary %>%
  filter(country_code != "VEN", phase == "contraction", end_year >= 1965, start_year <= 2026) %>%
  mutate(episode_key = paste(country_code, episode_id, sep = "_"))

wdi_worst_summary <- wdi_negative_summary %>%
  slice_min(cumulative_growth, n = 10, with_ties = FALSE) %>%
  arrange(cumulative_growth)

wdi_top_episodes <- wdi_worst_summary$episode_key

wdi_negative_episode_plot_data <- wdi_pc_episode_path %>%
  filter(country_code != "VEN", phase == "contraction", year >= 1965, year <= 2026) %>%
  mutate(
    episode_key = paste(country_code, episode_id, sep = "_"),
    highlight_group = case_when(
      episode_key %in% wdi_top_episodes ~ "top_10",
      TRUE ~ "baseline"
    )
  )

wdi_negative_baseline_endpoints <- wdi_negative_episode_plot_data %>%
  filter(highlight_group == "baseline") %>%
  group_by(episode_key) %>%
  slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
  ungroup()

wdi_venezuela_plot_data <- episodes %>%
  filter(
    as.character(series_id) == as.character(series_labels[["gdp_per_capita"]]),
    phase == "contraction",
    year >= 1965,
    year <= 2026
  ) %>%
  group_by(episode_group) %>%
  mutate(episode_min_change = min(episode_cumulative_change, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(episode_min_change == min(episode_min_change, na.rm = TRUE))

wdi_negative_episode_label_data <- build_negative_episode_labels(
  wdi_negative_episode_plot_data,
  wdi_worst_summary,
  wdi_venezuela_plot_data
) %>%
  mutate(
    label_nudge_y = case_when(
      grepl("^(MDA|SDN|SAU)", label) ~ 0.025,
      grepl("^(LBR|GEO|COD)", label) ~ -0.025,
      TRUE ~ 0
    )
  )
wdi_top_label_data <- wdi_negative_episode_label_data %>%
  filter(label_group == "Top 10 episodios")
wdi_venezuela_label_data <- wdi_negative_episode_label_data %>%
  filter(label_group == "Venezuela")

wdi_negative_episode_lines <- ggplot() +
  # Reference lines make the no-change and -15% thresholds explicit.
  geom_hline(yintercept = 0, linewidth = 0.3, color = presentation_colors[["ink"]]) +
  geom_hline(yintercept = -0.15, linewidth = 0.55, color = presentation_colors[["accent"]], linetype = "dashed") +
  annotate(
    "text",
    x = -Inf,
    y = -0.15,
    label = "-15%",
    color = presentation_colors[["accent"]],
    family = presentation_font_family,
    fontface = "bold",
    size = 3.4,
    hjust = -0.1,
    vjust = -0.45
  ) +
  # Baseline WDI episodes remain light so highlighted contractions dominate.
  geom_line(
    data = wdi_negative_episode_plot_data[wdi_negative_episode_plot_data$highlight_group == "baseline", ],
    aes(x = year, y = episode_cumulative_change, group = episode_key, color = "Otros episodios"),
    linewidth = 0.25,
    alpha = 0.65
  ) +
  geom_point(
    data = wdi_negative_baseline_endpoints,
    aes(x = year, y = episode_cumulative_change),
    inherit.aes = FALSE,
    color = presentation_colors[["grid"]],
    alpha = 0.65,
    size = international_negative_baseline_endpoint_size,
    show.legend = FALSE
  ) +
  # Top-10 non-Venezuela contractions provide the benchmark set.
  geom_line(
    data = wdi_negative_episode_plot_data[wdi_negative_episode_plot_data$highlight_group == "top_10", ],
    aes(x = year, y = episode_cumulative_change, group = episode_key, color = "Top 10 episodios"),
    linewidth = 0.75,
    alpha = international_negative_top_line_alpha
  ) +
  # Venezuela is layered last and colored red for consistency across the deck.
  geom_line(
    data = wdi_venezuela_plot_data,
    aes(x = year, y = episode_cumulative_change, group = episode_group, color = "Venezuela"),
    linewidth = 1.1,
    alpha = 0.95
  ) +
  # Endpoint dots provide clear anchors for the repelled labels.
  geom_point(
    data = wdi_top_label_data,
    aes(x = year, y = episode_cumulative_change),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.4,
    stroke = presentation_point_stroke,
    color = presentation_colors[["ink"]],
    fill = presentation_colors[["primary"]],
    alpha = international_negative_top_endpoint_alpha
  ) +
  geom_point(
    data = wdi_venezuela_label_data,
    aes(x = year, y = episode_cumulative_change),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.8,
    stroke = presentation_point_stroke,
    color = presentation_colors[["ink"]],
    fill = presentation_colors[["venezuela"]]
  ) +
  # ISO-year labels follow the same callout style as other scatter annotations.
  geom_label_repel(
    data = wdi_negative_episode_label_data,
    aes(x = year, y = episode_cumulative_change, label = label),
    inherit.aes = FALSE,
    color = presentation_colors[["ink"]],
    family = presentation_font_family,
    size = presentation_label_small_text_size,
    fill = "white",
    label.size = presentation_label_box_linewidth,
    label.padding = presentation_label_padding,
    label.r = presentation_label_radius,
    box.padding = 0.60,
    point.padding = 0.55,
    segment.color = presentation_colors[["muted"]],
    segment.size = presentation_label_segment_size,
    min.segment.length = 0,
    nudge_x = 0.8,
    nudge_y = wdi_negative_episode_label_data$label_nudge_y,
    force = 3,
    max.overlaps = Inf,
    seed = 1234,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Otros episodios" = presentation_colors[["grid"]],
      "Top 10 episodios" = presentation_colors[["primary"]],
      "Venezuela" = presentation_colors[["venezuela"]]
    ),
    breaks = c("Otros episodios", "Top 10 episodios", "Venezuela"),
    name = NULL
  ) +
  presentation_recent_year_axis(1960) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = international_negative_y_limits,
    breaks = international_negative_y_breaks
  ) +
  coord_cartesian(clip = "off", expand = FALSE) +
  labs(
    title = "Episodios negativos de PIB real per cápita (WDI)",
    x = NULL,
    y = "Variación acumulada",
    subtitle = "Compara a Venezuela con las peores contracciones internacionales de WDI desde 1965.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(plot.margin = margin(5.5, 70, 5.5, 5.5))

wdi_negative_episode_lines <- apply_presentation_plot_style(wdi_negative_episode_lines)

save_plot_variants(
  filename = file.path(figure_dir, "international_negative_episode_lines_wdi.png"),
  plot = wdi_negative_episode_lines,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(wdi_negative_episode_lines)
message("Wrote ", file.path(figure_dir, "international_negative_episode_lines_wdi.png"))

### Graph 22: Episodios negativos internacionales FMI WEO ----
imf_weo_negative_summary <- imf_weo_episode_summary %>%
  filter(country_code != "VEN", phase == "contraction", end_year >= 1981, start_year <= 2026) %>%
  mutate(episode_key = paste(country_code, episode_id, sep = "_"))

imf_weo_worst_summary <- imf_weo_negative_summary %>%
  slice_min(cumulative_growth, n = 10, with_ties = FALSE) %>%
  arrange(cumulative_growth)

imf_weo_top_episodes <- imf_weo_worst_summary$episode_key

imf_weo_negative_episode_plot_data <- imf_weo_episode_path %>%
  filter(country_code != "VEN", phase == "contraction", year >= 1981, year <= 2026) %>%
  mutate(
    episode_key = paste(country_code, episode_id, sep = "_"),
    highlight_group = case_when(
      episode_key %in% imf_weo_top_episodes ~ "top_10",
      TRUE ~ "baseline"
    )
  )

imf_weo_negative_baseline_endpoints <- imf_weo_negative_episode_plot_data %>%
  filter(highlight_group == "baseline") %>%
  group_by(episode_key) %>%
  slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
  ungroup()

imf_weo_venezuela_plot_data <- episodes %>%
  filter(
    as.character(series_id) == as.character(series_labels[["gdp_per_capita"]]),
    phase == "contraction",
    year >= 1981,
    year <= 2026
  ) %>%
  group_by(episode_group) %>%
  mutate(episode_min_change = min(episode_cumulative_change, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(episode_min_change == min(episode_min_change, na.rm = TRUE))

imf_weo_negative_episode_label_data <- build_negative_episode_labels(
  imf_weo_negative_episode_plot_data,
  imf_weo_worst_summary,
  imf_weo_venezuela_plot_data
)

imf_weo_top_label_data <- imf_weo_negative_episode_label_data %>%
  filter(label_group == "Top 10 episodios")

imf_weo_venezuela_label_data <- imf_weo_negative_episode_label_data %>%
  filter(label_group == "Venezuela")

imf_weo_negative_episode_lines <- ggplot() +
  # IMF-WEO chart follows the same threshold and visual hierarchy as WDI/Maddison.
  geom_hline(yintercept = 0, linewidth = 0.3, color = presentation_colors[["ink"]]) +
  geom_hline(yintercept = -0.15, linewidth = 0.55, color = presentation_colors[["accent"]], linetype = "dashed") +
  annotate(
    "text",
    x = -Inf,
    y = -0.15,
    label = "-15%",
    color = presentation_colors[["accent"]],
    family = presentation_font_family,
    fontface = "bold",
    size = 3.4,
    hjust = -0.1,
    vjust = -0.45
  ) +
  # Other international contractions provide the faint background context.
  geom_line(
    data = imf_weo_negative_episode_plot_data[imf_weo_negative_episode_plot_data$highlight_group == "baseline", ],
    aes(x = year, y = episode_cumulative_change, group = episode_key, color = "Otros episodios"),
    linewidth = 0.25,
    alpha = 0.65
  ) +
  geom_point(
    data = imf_weo_negative_baseline_endpoints,
    aes(x = year, y = episode_cumulative_change),
    inherit.aes = FALSE,
    color = presentation_colors[["grid"]],
    alpha = 0.65,
    size = international_negative_baseline_endpoint_size,
    show.legend = FALSE
  ) +
  # Top-10 IMF-WEO contractions are emphasized in the shared dark-blue style.
  geom_line(
    data = imf_weo_negative_episode_plot_data[imf_weo_negative_episode_plot_data$highlight_group == "top_10", ],
    aes(x = year, y = episode_cumulative_change, group = episode_key, color = "Top 10 episodios"),
    linewidth = 0.75,
    alpha = international_negative_top_line_alpha
  ) +
  # Venezuela stays in red and is drawn over all comparison paths.
  geom_line(
    data = imf_weo_venezuela_plot_data,
    aes(x = year, y = episode_cumulative_change, group = episode_group, color = "Venezuela"),
    linewidth = 1.1,
    alpha = 0.95
  ) +
  # Endpoint markers make the callout labels easier to trace back to a path.
  geom_point(
    data = imf_weo_top_label_data,
    aes(x = year, y = episode_cumulative_change),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.4,
    stroke = presentation_point_stroke,
    color = presentation_colors[["ink"]],
    fill = presentation_colors[["primary"]],
    alpha = international_negative_top_endpoint_alpha
  ) +
  geom_point(
    data = imf_weo_venezuela_label_data,
    aes(x = year, y = episode_cumulative_change),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.8,
    stroke = presentation_point_stroke,
    color = presentation_colors[["ink"]],
    fill = presentation_colors[["venezuela"]]
  ) +
  # Repelled labels use ISO code plus years, matching the other disaster charts.
  geom_label_repel(
    data = imf_weo_negative_episode_label_data,
    aes(x = year, y = episode_cumulative_change, label = label),
    inherit.aes = FALSE,
    color = presentation_colors[["ink"]],
    family = presentation_font_family,
    size = presentation_label_small_text_size,
    fill = "white",
    label.size = presentation_label_box_linewidth,
    label.padding = presentation_label_padding,
    label.r = presentation_label_radius,
    box.padding = 0.60,
    point.padding = 0.55,
    segment.color = presentation_colors[["muted"]],
    segment.size = presentation_label_segment_size,
    min.segment.length = 0,
    nudge_x = 0.8,
    force = 3,
    max.overlaps = Inf,
    seed = 1234,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Otros episodios" = presentation_colors[["grid"]],
      "Top 10 episodios" = presentation_colors[["primary"]],
      "Venezuela" = presentation_colors[["venezuela"]]
    ),
    breaks = c("Otros episodios", "Top 10 episodios", "Venezuela"),
    name = NULL
  ) +
  presentation_recent_year_axis(1980) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = international_negative_y_limits,
    breaks = international_negative_y_breaks
  ) +
  coord_cartesian(clip = "off", expand = FALSE) +
  labs(
    title = "Episodios negativos de PIB real per cápita (FMI WEO)",
    x = NULL,
    y = "Variación acumulada",
    subtitle = "Compara a Venezuela con las peores contracciones internacionales de FMI WEO desde 1980.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(plot.margin = margin(5.5, 70, 5.5, 5.5))

imf_weo_negative_episode_lines <- apply_presentation_plot_style(imf_weo_negative_episode_lines)

save_plot_variants(
  filename = file.path(figure_dir, "international_negative_episode_lines_imf_weo.png"),
  plot = imf_weo_negative_episode_lines,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(imf_weo_negative_episode_lines)
message("Wrote ", file.path(figure_dir, "international_negative_episode_lines_imf_weo.png"))

## Family: post-disaster recovery benchmarks ----


### Graph 23: Distribución de TCAC post-desastre ----
international_disaster_horizon_cagr_distribution_data <- international_disaster_horizons %>%
  mutate(horizon_label = factor(horizon_label, levels = sprintf("%s años después", c(5L, 10L, 15L, 20L, 25L, 30L))))
international_disaster_horizon_cagr_distribution_colors <- build_priority_color_map(levels(international_disaster_horizon_cagr_distribution_data$horizon_label))
international_disaster_horizon_cagr_distribution <- international_disaster_horizon_cagr_distribution_data %>%
  ggplot(aes(x = post_disaster_cagr, color = horizon_label)) +
  geom_vline(xintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.35, linetype = "dashed") +
  geom_density(linewidth = 1.05, adjust = 1.05, na.rm = TRUE) +
  facet_wrap(vars(source_label), ncol = 1) +
  scale_color_manual(values = international_disaster_horizon_cagr_distribution_colors, name = "Horizonte") +
  scale_x_continuous(labels = label_percent(accuracy = 1), breaks = breaks_width(0.05)) +
  labs(
    title = "Distribución de TCAC post-desastre por horizonte",
    x = "TCAC posterior a la caída",
    y = "Densidad de episodios",
    subtitle = "Compara la distribución de TCAC posterior a la caída por horizonte de recuperación.",
    caption = append_caption_note(presentation_source_caption, density_note)
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(legend.position = "bottom")

international_disaster_horizon_cagr_distribution <- apply_presentation_plot_style(international_disaster_horizon_cagr_distribution)

save_plot_variants(
  filename = file.path(figure_dir, "international_disaster_horizon_cagr_distribution.png"),
  plot = international_disaster_horizon_cagr_distribution,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(international_disaster_horizon_cagr_distribution)
message("Wrote ", file.path(figure_dir, "international_disaster_horizon_cagr_distribution.png"))

## Family: post-collapse recovery paths ----


## Shared graph preparation: International recovery paths ----
# Build the reusable path, label, and plotting logic used by the three source-specific graphs.
build_international_disaster_recovery_paths <- function(
  recovery_source_label,
  recovery_source_name,
  recovery_source_caption,
  output_filename
) {
  international_disaster_recovery_paths_data <- international_disaster_recovery %>%
    filter(
      source_label == recovery_source_label,
      year >= end_year
    ) %>%
    group_by(disaster_key) %>%
    mutate(
      years_since_end = year - end_year,
      path_end_year = if_else(
        !is.na(recovery_year),
        recovery_year,
        max(year, na.rm = TRUE)
      )
    ) %>%
    filter(
      year <= path_end_year,
      years_since_end <= 30
    ) %>%
    ungroup()

  international_disaster_recovery_summary_grid <- international_disaster_recovery_paths_data %>%
    filter(!is_venezuela) %>%
    distinct(
      disaster_key,
      recovery_year,
      end_year
    ) %>%
    crossing(years_since_end = 0:30) %>%
    left_join(
      international_disaster_recovery_paths_data %>%
        select(
          disaster_key,
          years_since_end,
          index_start_100
        ),
      by = c("disaster_key", "years_since_end")
    ) %>%
    mutate(
      recovery_horizon = recovery_year - end_year,
      recovery_progress = case_when(
        !is.na(index_start_100) ~ pmin(index_start_100, 100),
        !is.na(recovery_horizon) & years_since_end > recovery_horizon ~ 100,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(recovery_gap = recovery_progress / 100 - 1)

  international_disaster_recovery_summary <- international_disaster_recovery_summary_grid %>%
    group_by(years_since_end) %>%
    summarise(
      lower_quartile = stats::quantile(
        recovery_gap,
        probs = 0.25,
        na.rm = TRUE,
        names = FALSE
      ),
      median_recovery_gap = stats::median(recovery_gap, na.rm = TRUE),
      upper_quartile = stats::quantile(
        recovery_gap,
        probs = 0.75,
        na.rm = TRUE,
        names = FALSE
      ),
      observed_episodes = sum(!is.na(recovery_gap)),
      .groups = "drop"
    )

  international_disaster_recovery_paths_starts <- international_disaster_recovery_paths_data %>%
    group_by(disaster_key) %>%
    slice_min(order_by = years_since_end, n = 1, with_ties = FALSE) %>%
    ungroup()

  international_disaster_recovery_paths_ends <- international_disaster_recovery_paths_data %>%
    group_by(disaster_key) %>%
    slice_max(order_by = years_since_end, n = 1, with_ties = FALSE) %>%
    ungroup()

  # Highlight the ten deepest international contractions without overloading labels.
  international_disaster_recovery_pre_1950_worst_keys <- international_disaster_recovery_paths_starts %>%
    filter(!is_venezuela, recovery_source_name == "Maddison", start_year < 1950) %>%
    slice_min(
      order_by = index_start_100,
      n = 10,
      with_ties = FALSE
    ) %>%
    pull(disaster_key)

  if (identical(recovery_source_name, "Maddison")) {
    international_disaster_recovery_japan_wwii_key <- international_disaster_recovery_paths_starts %>%
      filter(country_code == "JPN", start_year == 1941, end_year == 1947) %>%
      pull(disaster_key)

    if (length(international_disaster_recovery_japan_wwii_key) != 1) {
      stop("The Japan 1941-1947 recovery path could not be identified.")
    }

    international_disaster_recovery_pre_1950_worst_keys <- union(
      international_disaster_recovery_pre_1950_worst_keys,
      international_disaster_recovery_japan_wwii_key
    )
  }

  international_disaster_recovery_post_1950_worst_keys <- international_disaster_recovery_paths_starts %>%
    filter(
      !is_venezuela,
      recovery_source_name != "Maddison" | start_year >= 1950
    ) %>%
    slice_min(
      order_by = index_start_100,
      n = 10,
      with_ties = FALSE
    ) %>%
    pull(disaster_key)

  venezuela_latest_disaster_key <- international_disaster_recovery_paths_data %>%
    filter(is_venezuela) %>%
    distinct(disaster_key, end_year) %>%
    slice_max(order_by = end_year, n = 1, with_ties = FALSE) %>%
    pull(disaster_key)

  international_disaster_recovery_paths_plot_data <- international_disaster_recovery_paths_data %>%
    mutate(
      recovery_gap = pmin(index_start_100, 100) / 100 - 1,
      path_group = case_when(
        disaster_key == venezuela_latest_disaster_key ~ "Venezuela",
        disaster_key %in% international_disaster_recovery_pre_1950_worst_keys ~ "10 colapsos pre-1950",
        disaster_key %in% international_disaster_recovery_post_1950_worst_keys ~ "10 colapsos desde 1950",
        TRUE ~ "Experiencia internacional"
      )
    )

  international_disaster_recovery_paths_plot_starts <- international_disaster_recovery_paths_starts %>%
    mutate(
      recovery_gap = pmin(index_start_100, 100) / 100 - 1,
      path_group = case_when(
        disaster_key == venezuela_latest_disaster_key ~ "Venezuela",
        disaster_key %in% international_disaster_recovery_pre_1950_worst_keys ~ "10 colapsos pre-1950",
        disaster_key %in% international_disaster_recovery_post_1950_worst_keys ~ "10 colapsos desde 1950",
        TRUE ~ "Experiencia internacional"
      )
    )

  international_disaster_recovery_paths_plot_ends <- international_disaster_recovery_paths_ends %>%
    mutate(
      recovery_gap = pmin(index_start_100, 100) / 100 - 1,
      path_group = case_when(
        disaster_key == venezuela_latest_disaster_key ~ "Venezuela",
        disaster_key %in% international_disaster_recovery_pre_1950_worst_keys ~ "10 colapsos pre-1950",
        disaster_key %in% international_disaster_recovery_post_1950_worst_keys ~ "10 colapsos desde 1950",
        TRUE ~ "Experiencia internacional"
      )
    )

  international_disaster_recovery_median_stop_year <- international_disaster_recovery_summary %>%
    filter(median_recovery_gap >= 0) %>%
    slice_min(order_by = years_since_end, n = 1, with_ties = FALSE) %>%
    pull(years_since_end)

  if (length(international_disaster_recovery_median_stop_year) != 1) {
    stop("The international recovery median does not reach the pre-collapse level within 30 years.")
  }

  international_disaster_recovery_quartile_recovery_years <- bind_rows(
    international_disaster_recovery_summary %>%
      filter(lower_quartile >= 0) %>%
      slice_min(order_by = years_since_end, n = 1, with_ties = FALSE) %>%
      transmute(years_since_end, quartile = "lower"),
    international_disaster_recovery_summary %>%
      filter(upper_quartile >= 0) %>%
      slice_min(order_by = years_since_end, n = 1, with_ties = FALSE) %>%
      transmute(years_since_end, quartile = "upper")
  )

  if (nrow(international_disaster_recovery_quartile_recovery_years) == 0) {
    stop("Neither recovery-path quartile reaches the pre-collapse level within 30 years.")
  }

  international_disaster_recovery_ribbon_stop_year <- max(
    international_disaster_recovery_quartile_recovery_years$years_since_end
  )

  international_disaster_recovery_ribbon <- international_disaster_recovery_summary %>%
    filter(years_since_end <= international_disaster_recovery_ribbon_stop_year)

  international_disaster_recovery_summary <- international_disaster_recovery_summary %>%
    filter(years_since_end <= international_disaster_recovery_median_stop_year) %>%
    mutate(summary_group = "Mediana internacional")

  international_disaster_recovery_paths <- international_disaster_recovery_paths_plot_data %>%
    ggplot(aes(
      x = years_since_end,
      y = recovery_gap,
      group = disaster_key,
      color = path_group
    )) +
    geom_hline(
      yintercept = 0,
      color = presentation_colors[["ink"]],
      linewidth = 0.55
    ) +
    geom_ribbon(
      data = international_disaster_recovery_ribbon,
      aes(
        x = years_since_end,
        ymin = lower_quartile,
        ymax = upper_quartile
      ),
      inherit.aes = FALSE,
      fill = presentation_colors[["recovery"]],
      alpha = 0.13,
      color = NA,
      show.legend = FALSE
    ) +
    geom_line(
      data = international_disaster_recovery_paths_plot_data %>%
        filter(path_group == "Experiencia internacional"),
      linewidth = 0.35,
      alpha = 0.28
    ) +
    geom_line(
      data = international_disaster_recovery_paths_plot_data %>%
        filter(path_group == "10 colapsos pre-1950"),
      linewidth = 0.75,
      alpha = 0.68
    ) +
    geom_line(
      data = international_disaster_recovery_paths_plot_data %>%
        filter(path_group == "10 colapsos desde 1950"),
      linewidth = 0.75,
      alpha = 0.68
    ) +
    geom_line(
      data = international_disaster_recovery_summary,
      aes(
        x = years_since_end,
        y = median_recovery_gap,
        color = summary_group
      ),
      inherit.aes = FALSE,
      linewidth = 0.95
    ) +
    geom_line(
      data = international_disaster_recovery_paths_plot_data %>%
        filter(path_group == "Venezuela"),
      linewidth = 1.1
    ) +
    geom_point(
      data = international_disaster_recovery_paths_plot_starts,
      shape = 21,
      fill = "white",
      size = 1.8,
      stroke = presentation_point_stroke
    ) +
    geom_point(
      data = international_disaster_recovery_paths_plot_ends %>%
        filter(path_group == "Experiencia internacional"),
      shape = 21,
      color = presentation_colors[["muted"]],
      fill = presentation_colors[["muted"]],
      size = 1.8,
      stroke = presentation_point_stroke
    ) +
    geom_point(
      data = international_disaster_recovery_paths_plot_ends %>%
        filter(path_group == "10 colapsos pre-1950"),
      shape = 21,
      color = presentation_palette[["cyan"]],
      fill = presentation_palette[["cyan"]],
      size = 2,
      stroke = presentation_point_stroke
    ) +
    geom_point(
      data = international_disaster_recovery_paths_plot_ends %>%
        filter(path_group == "10 colapsos desde 1950"),
      shape = 21,
      color = presentation_colors[["primary"]],
      fill = presentation_colors[["primary"]],
      size = 2,
      stroke = presentation_point_stroke
    ) +
    geom_point(
      data = international_disaster_recovery_paths_plot_ends %>%
        filter(path_group == "Venezuela"),
      shape = 21,
      color = presentation_colors[["venezuela"]],
      fill = presentation_colors[["venezuela"]],
      size = 1.8,
      stroke = presentation_point_stroke
    ) +
    geom_label_repel(
      data = international_disaster_recovery_paths_plot_ends %>%
        filter(path_group == "Venezuela"),
      aes(
        x = years_since_end,
        y = recovery_gap,
        label = "VEN"
      ),
      inherit.aes = FALSE,
      family = presentation_font_family,
      color = presentation_colors[["ink"]],
      fill = "white",
      size = presentation_label_text_size,
      label.size = presentation_label_box_linewidth,
      label.padding = presentation_label_padding,
      label.r = presentation_label_radius,
      box.padding = presentation_label_box_padding,
      point.padding = presentation_label_point_padding,
      segment.color = presentation_colors[["muted"]],
      segment.size = presentation_label_segment_size,
      min.segment.length = 0,
      max.overlaps = Inf,
      seed = 20260804,
      show.legend = FALSE
    ) +
    geom_label_repel(
      data = international_disaster_recovery_paths_plot_ends %>%
        filter(path_group %in% c("10 colapsos pre-1950", "10 colapsos desde 1950")),
      aes(
        x = years_since_end,
        y = recovery_gap,
        label = country_code
      ),
      inherit.aes = FALSE,
      family = presentation_font_family,
      color = presentation_colors[["ink"]],
      fill = "white",
      size = presentation_label_text_size,
      label.size = presentation_label_box_linewidth,
      label.padding = presentation_label_padding,
      label.r = presentation_label_radius,
      box.padding = presentation_label_box_padding,
      point.padding = presentation_label_point_padding,
      segment.color = presentation_colors[["muted"]],
      segment.size = presentation_label_segment_size,
      min.segment.length = 0,
      max.overlaps = Inf,
      seed = 20260804,
      show.legend = FALSE
    ) +
    scale_color_manual(
      values = c(
        "Experiencia internacional" = presentation_colors[["muted"]],
        "10 colapsos pre-1950" = presentation_palette[["cyan"]],
        "10 colapsos desde 1950" = presentation_colors[["primary"]],
        "Mediana internacional" = presentation_colors[["recovery"]],
        "Venezuela" = presentation_colors[["venezuela"]]
      ),
      breaks = c(
        "Experiencia internacional",
        "10 colapsos pre-1950",
        "10 colapsos desde 1950",
        "Mediana internacional",
        "Venezuela"
      ),
      labels = c(
        "Experiencia internacional" = "Experiencia internacional",
        "10 colapsos pre-1950" = "Peores episodios pre-1950",
        "10 colapsos desde 1950" = "Peores episodios post-1950",
        "Mediana internacional" = "Mediana internacional",
        "Venezuela" = "Venezuela"
      )
    ) +
    scale_x_continuous(
      limits = c(0, 30),
      breaks = seq(0, 30, by = 5),
      expand = expansion(add = c(0.4, 0.4))
    ) +
    scale_y_continuous(
      labels = label_percent(
        accuracy = 1,
        decimal.mark = ","
      ),
      breaks = seq(-1, 0.1, by = 0.1),
      limits = c(-1, 0.1)
    ) +
    labs(
      title = paste0(
        "Trayectorias internacionales después de grandes colapsos (",
        recovery_source_name,
        ")"
        ),
      subtitle = paste(
        "Cada línea muestra la brecha pendiente frente al nivel previo a la caída;",
        "0% indica recuperación."
        ),
      x = "Años desde el final de la contracción",
      y = "Brecha frente al nivel previo a la caída",
      color = NULL,
      caption = build_source_caption(
        source = recovery_source_caption,
        calculations = TRUE
        )
    ) +
    theme_minimal(
      base_size = presentation_base_size,
      base_family = presentation_font_family
    ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.justification = "center",
      panel.grid.minor = element_blank()
    )

  international_disaster_recovery_paths <- apply_presentation_plot_style(
    international_disaster_recovery_paths
  ) +
    coord_cartesian(expand = TRUE)

  save_plot_variants(
    filename = file.path(figure_dir, output_filename),
    plot = international_disaster_recovery_paths,
    width = presentation_plot_width,
    height = presentation_plot_height,
    dpi = presentation_plot_dpi
  )

  print(international_disaster_recovery_paths)
  message("Wrote ", file.path(figure_dir, output_filename))

  international_disaster_recovery_paths
}

### Graph 24: Trayectorias internacionales después de grandes colapsos — Maddison ----
international_disaster_recovery_paths <- build_international_disaster_recovery_paths(
  recovery_source_label = "Maddison (PIB per cápita)",
  recovery_source_name = "Maddison",
  recovery_source_caption = "Maddison Project Database (2023)",
  output_filename = "international_disaster_recovery_paths.png"
)

### Graph 25: Trayectorias internacionales después de grandes colapsos — Banco Mundial ----
international_disaster_recovery_paths_wdi <- build_international_disaster_recovery_paths(
  recovery_source_label = "WDI (PIB per cápita)",
  recovery_source_name = "Banco Mundial",
  recovery_source_caption = "Banco Mundial WDI (2025)",
  output_filename = "international_disaster_recovery_paths_wdi.png"
)

### Graph 26: Trayectorias internacionales después de grandes colapsos — FMI WEO ----
international_disaster_recovery_paths_imf_weo <- build_international_disaster_recovery_paths(
  recovery_source_label = "FMI WEO (PIB per cápita)",
  recovery_source_name = "FMI WEO",
  recovery_source_caption = "FMI WEO (2025)",
  output_filename = "international_disaster_recovery_paths_imf_weo.png"
)

## Shared graph preparation: International recovery depth-duration matrix ----
# Build the reusable matrix data and plotting logic used by the three source-specific drafts.
international_recovery_matrix_episodes <- international_disaster_recovery %>%
  distinct(
    source_label,
    disaster_key,
    country_code,
    cumulative_growth,
    start_year,
    end_year,
    recovery_year,
    is_venezuela
  ) %>%
  left_join(
    international_disaster_recovery %>%
      group_by(source_label, disaster_key) %>%
      summarise(last_observed_year = max(year, na.rm = TRUE), .groups = "drop"),
    by = c("source_label", "disaster_key")
  ) %>%
  mutate(
    fall_magnitude = -cumulative_growth,
    recovery_duration = if_else(
      !is.na(recovery_year),
      recovery_year - end_year,
      last_observed_year - end_year
    ),
    depth_band = case_when(
      fall_magnitude < 0.30 ~ "15–29%",
      fall_magnitude < 0.50 ~ "30–49%",
      fall_magnitude < 0.70 ~ "50–69%",
      TRUE ~ "70% o más"
    ),
    duration_band = case_when(
      is.na(recovery_year) ~ "No recuperado",
      recovery_duration <= 5 ~ "0–5 años",
      recovery_duration <= 10 ~ "6–10 años",
      recovery_duration <= 20 ~ "11–20 años",
      recovery_duration <= 30 ~ "21–30 años",
      TRUE ~ "Más de 30 años"
    )
  )

international_recovery_matrix_counts <- bind_rows(
  international_recovery_matrix_episodes %>%
    count(source_label, depth_band, duration_band, name = "episode_count"),
  international_recovery_matrix_episodes %>%
    count(source_label, depth_band, name = "episode_count") %>%
    mutate(duration_band = "Total"),
  international_recovery_matrix_episodes %>%
    count(source_label, duration_band, name = "episode_count") %>%
    mutate(depth_band = "Total"),
  international_recovery_matrix_episodes %>%
    count(source_label, name = "episode_count") %>%
    mutate(
      depth_band = "Total",
      duration_band = "Total"
    )
)

# Mark only the current, deepest Venezuelan collapse in each source panel.
international_recovery_matrix_venezuela <- international_recovery_matrix_episodes %>%
  filter(is_venezuela) %>%
  group_by(source_label) %>%
  slice_max(
    order_by = fall_magnitude,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  transmute(
    source_label,
    depth_band,
    duration_band,
    venezuela_label = sprintf("VEN %s–%s", start_year, end_year)
  )

international_recovery_matrix_data <- international_recovery_matrix_counts %>%
  full_join(
    international_recovery_matrix_venezuela,
    by = c("source_label", "depth_band", "duration_band")
  ) %>%
  complete(
    source_label = c(
      "Maddison (PIB per cápita)",
      "WDI (PIB per cápita)",
      "FMI WEO (PIB per cápita)"
    ),
    depth_band = c(
      "15–29%",
      "30–49%",
      "50–69%",
      "70% o más",
      "Total"
    ),
    duration_band = c(
      "0–5 años",
      "6–10 años",
      "11–20 años",
      "21–30 años",
      "Más de 30 años",
      "No recuperado",
      "Total"
    ),
    fill = list(episode_count = 0L, venezuela_label = NA_character_)
  ) %>%
  mutate(
    source_label = factor(
      source_label,
      levels = c(
        "Maddison (PIB per cápita)",
        "WDI (PIB per cápita)",
        "FMI WEO (PIB per cápita)"
      ),
      labels = c("Maddison", "Banco Mundial", "FMI WEO")
    ),
    depth_band = factor(
      depth_band,
      levels = c("70% o más", "50–69%", "30–49%", "15–29%", "Total")
    ),
    duration_band = factor(
      duration_band,
      levels = c(
        "0–5 años",
        "6–10 años",
        "11–20 años",
        "21–30 años",
        "Más de 30 años",
        "No recuperado",
        "Total"
      )
    ),
    is_total = depth_band == "Total" | duration_band == "Total",
    fill_count = if_else(is_total, NA_integer_, episode_count),
    text_color = if_else(
      !is_total & episode_count >= max(episode_count[!is_total], na.rm = TRUE) * 0.55,
      "white",
      presentation_colors[["ink"]]
    )
  )

build_international_recovery_depth_duration_matrix <- function(
  matrix_source_data,
  matrix_source_name,
  matrix_source_caption,
  output_filename
) {
  international_recovery_depth_duration_matrix <- matrix_source_data %>%
    ggplot(aes(
      x = duration_band,
      y = depth_band,
      fill = fill_count
    )) +
    geom_tile(
      data = matrix_source_data %>%
        filter(!is_total),
      color = "white",
      linewidth = 0.55
    ) +
    geom_tile(
      data = matrix_source_data %>%
        filter(is_total),
      fill = presentation_colors[["light"]],
      color = "white",
      linewidth = 0.55,
      show.legend = FALSE
    ) +
    geom_tile(
      data = matrix_source_data %>%
        filter(!is.na(venezuela_label)),
      fill = NA,
      color = presentation_colors[["venezuela"]],
      linewidth = 1.1,
      show.legend = FALSE
    ) +
    geom_text(
      aes(
        label = if_else(episode_count > 0, as.character(episode_count), ""),
        color = text_color
      ),
      vjust = -0.35,
      size = 4.2,
      fontface = "bold"
    ) +
    geom_text(
      data = matrix_source_data %>%
        filter(!is.na(venezuela_label)),
      aes(label = venezuela_label),
      color = presentation_colors[["venezuela"]],
      vjust = 1.15,
      size = 3.1,
      fontface = "bold"
    ) +
    scale_color_identity() +
    scale_fill_gradient(
      low = presentation_colors[["light"]],
      high = presentation_colors[["primary"]],
      breaks = breaks_pretty(n = 4),
      name = "Episodios",
      na.value = presentation_colors[["light"]]
    ) +
    scale_x_discrete(
      labels = c(
        "0–5 años" = "0–5",
        "6–10 años" = "6–10",
        "11–20 años" = "11–20",
        "21–30 años" = "21–30",
        "Más de 30 años" = ">30",
        "No recuperado" = "No recuperado",
        "Total" = "Total"
      )
    ) +
    scale_y_discrete(
      limits = rev(levels(matrix_source_data$depth_band))
    ) +
    geom_hline(
      yintercept = 1.5,
      color = alpha(presentation_colors[["ink"]], alpha = 0.55),
      linewidth = 0.8
    ) +
    geom_vline(
      xintercept = 6.5,
      color = alpha(presentation_colors[["ink"]], alpha = 0.55),
      linewidth = 0.8
    ) +
    labs(
      title = paste0(
        "Profundidad de los colapsos y tiempo de recuperación (",
        matrix_source_name,
        ")"
        ),
      subtitle = "Cada celda cuenta episodios; el borde rojo ubica el colapso venezolano más profundo.",
      x = "Años",
      y = "Magnitud acumulada de la caída",
      caption = append_caption_note(
        build_source_caption(
        source = matrix_source_caption,
        calculations = TRUE
        ),
        paste(
        "No recuperado = el nivel previo a la caída no se recuperó antes del último dato disponible.",
        "Los episodios venezolanos provienen de la serie propia del proyecto."
        )
        )
    ) +
    theme_minimal(
      base_size = presentation_base_size,
      base_family = presentation_font_family
    ) +
    theme(
      panel.grid = element_blank(),
      legend.position = "bottom",
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        size = presentation_base_size * 0.8
      )
    )

  international_recovery_depth_duration_matrix <- apply_presentation_plot_style(
    international_recovery_depth_duration_matrix
  )

  save_plot_variants(
    filename = file.path(figure_dir, output_filename),
    plot = international_recovery_depth_duration_matrix,
    width = presentation_plot_width,
    height = presentation_plot_height,
    dpi = presentation_plot_dpi
  )

  print(international_recovery_depth_duration_matrix)
  message("Wrote ", file.path(figure_dir, output_filename))

  international_recovery_depth_duration_matrix
}

### Graph 27 (draft): Matriz internacional de profundidad y duración de la recuperación — Maddison ----
international_recovery_depth_duration_matrix_maddison <- build_international_recovery_depth_duration_matrix(
  matrix_source_data = international_recovery_matrix_data %>%
    filter(as.character(source_label) == "Maddison"),
  matrix_source_name = "Maddison",
  matrix_source_caption = "Maddison Project Database (2023); Venezuela: serie propia",
  output_filename = "draft_international_recovery_depth_duration_matrix.png"
)

### Graph 28 (draft): Matriz internacional de profundidad y duración de la recuperación — Banco Mundial ----
international_recovery_depth_duration_matrix_wdi <- build_international_recovery_depth_duration_matrix(
  matrix_source_data = international_recovery_matrix_data %>%
    filter(as.character(source_label) == "Banco Mundial"),
  matrix_source_name = "Banco Mundial",
  matrix_source_caption = "Banco Mundial WDI (2025); Venezuela: serie propia",
  output_filename = "draft_international_recovery_depth_duration_matrix_wdi.png"
)

### Graph 29 (draft): Matriz internacional de profundidad y duración de la recuperación — FMI WEO ----
international_recovery_depth_duration_matrix_imf_weo <- build_international_recovery_depth_duration_matrix(
  matrix_source_data = international_recovery_matrix_data %>%
    filter(as.character(source_label) == "FMI WEO"),
  matrix_source_name = "FMI WEO",
  matrix_source_caption = "FMI WEO (2025); Venezuela: serie propia",
  output_filename = "draft_international_recovery_depth_duration_matrix_imf_weo.png"
)

## Family: Historical-frequency recovery scenarios ----

# Each pre-2013 ten-year rolling CAGR receives equal empirical weight. This is a
# historical scenario distribution, not a forecast of Venezuelan growth.
historical_frequency_window_years <- 10L
historical_frequency_benchmark_year <- 2012L
historical_frequency_anchor_year <- max(index_series$year, na.rm = TRUE)
historical_frequency_horizon_years <- 75L
historical_frequency_series_ids <- c(
  series_labels[["gdp"]],
  series_labels[["gdp_per_capita"]]
)

historical_frequency_reference_levels <- index_series %>%
  filter(
    series_id %in% historical_frequency_series_ids,
    year %in% c(historical_frequency_benchmark_year, historical_frequency_anchor_year)
  ) %>%
  select(series_id, series_label, year, index_value) %>%
  pivot_wider(
    names_from = year,
    values_from = index_value,
    names_prefix = "index_"
  ) %>%
  rename(
    benchmark_index = !!paste0("index_", historical_frequency_benchmark_year),
    anchor_index = !!paste0("index_", historical_frequency_anchor_year)
  )

if (nrow(historical_frequency_reference_levels) != length(historical_frequency_series_ids) ||
  anyNA(historical_frequency_reference_levels$benchmark_index) ||
  anyNA(historical_frequency_reference_levels$anchor_index)) {
  stop("Missing 2012 benchmark or latest anchor level for historical-frequency recovery scenarios.")
}

historical_frequency_cagr_rates <- rolling_cagr %>%
  filter(
    series_id %in% historical_frequency_series_ids,
    window_years == historical_frequency_window_years,
    end_year <= historical_frequency_benchmark_year
  ) %>%
  inner_join(
    historical_frequency_reference_levels,
    by = c("series_id")
  ) %>%
  mutate(
    recovery_years = if_else(
      cagr > 0,
      ceiling(log(benchmark_index / anchor_index) / log1p(cagr)),
      Inf
    )
  )

if (nrow(historical_frequency_cagr_rates) == 0 || any(historical_frequency_cagr_rates$cagr <= -1)) {
  stop("Historical-frequency recovery scenarios require valid pre-2013 ten-year CAGR observations.")
}

historical_frequency_recovery_paths <- historical_frequency_cagr_rates %>%
  mutate(scenario_id = row_number()) %>%
  crossing(years_since_anchor = 0:historical_frequency_horizon_years) %>%
  mutate(
    year = historical_frequency_anchor_year + years_since_anchor,
    index_2012_100 = pmin(
      anchor_index * (1 + cagr)^years_since_anchor / benchmark_index * 100,
      100
    )
  )

historical_frequency_recovery_summary <- historical_frequency_recovery_paths %>%
  group_by(series_id, series_label, year, years_since_anchor) %>%
  summarise(
    lower_quartile = stats::quantile(index_2012_100, probs = 0.25, na.rm = FALSE, names = FALSE, type = 7),
    median_index = stats::quantile(index_2012_100, probs = 0.50, na.rm = FALSE, names = FALSE, type = 7),
    upper_quartile = stats::quantile(index_2012_100, probs = 0.75, na.rm = FALSE, names = FALSE, type = 7),
    .groups = "drop"
  )

historical_frequency_recovery_history <- index_series %>%
  filter(
    series_id %in% historical_frequency_series_ids,
    year >= 1920,
    year <= historical_frequency_anchor_year
  ) %>%
  inner_join(
    historical_frequency_reference_levels %>%
      select(series_id, benchmark_index),
    by = "series_id"
  ) %>%
  mutate(index_2012_100 = index_value / benchmark_index * 100)

historical_frequency_recovery_metrics <- historical_frequency_cagr_rates %>%
  group_by(series_id, series_label) %>%
  summarise(
    cagr_window_years = historical_frequency_window_years,
    benchmark_year = historical_frequency_benchmark_year,
    anchor_year = historical_frequency_anchor_year,
    n_historical_windows = n(),
    probability_recover_within_10_years = mean(recovery_years <= 10),
    probability_recover_within_20_years = mean(recovery_years <= 20),
    probability_recover_within_30_years = mean(recovery_years <= 30),
    probability_nonpositive_cagr = mean(!is.finite(recovery_years)),
    conditional_recovery_year_p25 = stats::quantile(recovery_years[is.finite(recovery_years)], probs = 0.25, na.rm = FALSE, names = FALSE, type = 7),
    conditional_recovery_year_median = stats::quantile(recovery_years[is.finite(recovery_years)], probs = 0.50, na.rm = FALSE, names = FALSE, type = 7),
    conditional_recovery_year_p75 = stats::quantile(recovery_years[is.finite(recovery_years)], probs = 0.75, na.rm = FALSE, names = FALSE, type = 7),
    .groups = "drop"
  )

utils::write.csv(
  historical_frequency_recovery_metrics,
  "data/final/historical_frequency_recovery_metrics.csv",
  row.names = FALSE
)

### Graph 30: Escenario frecuentista de recuperación del PIB real ----
historical_frequency_recovery_gdp_summary <- historical_frequency_recovery_summary %>%
  filter(series_id == series_labels[["gdp"]])

historical_frequency_recovery_gdp_history <- historical_frequency_recovery_history %>%
  filter(series_id == series_labels[["gdp"]])

historical_frequency_recovery_gdp <- historical_frequency_recovery_gdp_summary %>%
  ggplot(aes(x = year)) +
  geom_hline(
    yintercept = 100,
    color = presentation_colors[["ink"]],
    linewidth = 0.55
  ) +
  geom_line(
    data = historical_frequency_recovery_gdp_history,
    aes(y = index_2012_100),
    color = presentation_colors[["ink"]],
    linewidth = 0.9
  ) +
  geom_ribbon(
    aes(ymin = lower_quartile, ymax = upper_quartile),
    fill = presentation_colors[["primary"]],
    alpha = 0.18
  ) +
  geom_line(
    aes(y = median_index),
    color = presentation_colors[["primary"]],
    linewidth = 1.05
  ) +
  geom_point(
    data = historical_frequency_recovery_gdp_summary %>%
      filter(year == historical_frequency_anchor_year),
    aes(y = median_index),
    shape = 21,
    color = presentation_colors[["primary"]],
    fill = "white",
    size = 2.6,
    stroke = presentation_point_stroke
  ) +
  scale_y_continuous(
    limits = c(0, 125),
    breaks = seq(0, 125, by = 25),
    labels = presentation_number_label(accuracy = 1)
  ) +
  presentation_recovery_year_axis(2100) +
  coord_cartesian(ylim = c(0, 125), clip = "off", expand = FALSE) +
  labs(
    title = "Recuperación frecuentista del PIB real",
    subtitle = "La banda comienza en 2025 y resume TCACs móviles de 10 años observados hasta 2012.",
    x = NULL,
    y = "Índice (2012 = 100)",
    caption = append_caption_note(presentation_source_caption, "Escenario frecuentista, no pronóstico. La banda muestra P25-P75 y la línea azul, la mediana; las trayectorias se detienen al recuperar el nivel de 2012.")
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  ) +
  theme(panel.grid.minor = element_blank()) +
  historical_event_reference_layers()

historical_frequency_recovery_gdp <- apply_presentation_plot_style(historical_frequency_recovery_gdp)

save_plot_variants(
  filename = file.path(figure_dir, "historical_frequency_recovery_gdp.png"),
  plot = historical_frequency_recovery_gdp,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(historical_frequency_recovery_gdp)
message("Wrote ", file.path(figure_dir, "historical_frequency_recovery_gdp.png"))

### Graph 31: Escenario frecuentista de recuperación del PIB real per cápita ----
historical_frequency_recovery_gdp_per_capita_summary <- historical_frequency_recovery_summary %>%
  filter(series_id == series_labels[["gdp_per_capita"]])

historical_frequency_recovery_gdp_per_capita_history <- historical_frequency_recovery_history %>%
  filter(series_id == series_labels[["gdp_per_capita"]])

historical_frequency_recovery_gdp_per_capita <- historical_frequency_recovery_gdp_per_capita_summary %>%
  ggplot(aes(x = year)) +
  geom_hline(
    yintercept = 100,
    color = presentation_colors[["ink"]],
    linewidth = 0.55
  ) +
  geom_line(
    data = historical_frequency_recovery_gdp_per_capita_history,
    aes(y = index_2012_100),
    color = presentation_colors[["ink"]],
    linewidth = 0.9
  ) +
  geom_ribbon(
    aes(ymin = lower_quartile, ymax = upper_quartile),
    fill = presentation_colors[["primary"]],
    alpha = 0.18
  ) +
  geom_line(
    aes(y = median_index),
    color = presentation_colors[["primary"]],
    linewidth = 1.05
  ) +
  geom_point(
    data = historical_frequency_recovery_gdp_per_capita_summary %>%
      filter(year == historical_frequency_anchor_year),
    aes(y = median_index),
    shape = 21,
    color = presentation_colors[["primary"]],
    fill = "white",
    size = 2.6,
    stroke = presentation_point_stroke
  ) +
  scale_y_continuous(
    limits = c(0, 125),
    breaks = seq(0, 125, by = 25),
    labels = presentation_number_label(accuracy = 1)
  ) +
  presentation_recovery_year_axis(2100) +
  coord_cartesian(ylim = c(0, 125), clip = "off", expand = FALSE) +
  labs(
    title = "Recuperación frecuentista del PIB real per cápita",
    subtitle = "La banda comienza en 2025 y resume TCACs móviles de 10 años observados hasta 2012.",
    x = NULL,
    y = "Índice (2012 = 100)",
    caption = append_caption_note(presentation_source_caption, "Escenario frecuentista, no pronóstico. La banda muestra P25-P75 y la línea azul, la mediana; las trayectorias se detienen al recuperar el nivel de 2012.")
  ) +
  theme_minimal(
    base_size = presentation_base_size,
    base_family = presentation_font_family
  ) +
  theme(panel.grid.minor = element_blank()) +
  historical_event_reference_layers()

historical_frequency_recovery_gdp_per_capita <- apply_presentation_plot_style(historical_frequency_recovery_gdp_per_capita)

save_plot_variants(
  filename = file.path(figure_dir, "historical_frequency_recovery_gdp_per_capita.png"),
  plot = historical_frequency_recovery_gdp_per_capita,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(historical_frequency_recovery_gdp_per_capita)
message("Wrote ", file.path(figure_dir, "historical_frequency_recovery_gdp_per_capita.png"))

## Family: recovery scenarios ----

### Graph 32: Trayectoria de recuperación ----
compound_growth_explainer_annual_growth <- c(0.08, 0.045, -0.025, 0.065, 0.032, -0.045, 0.095, 0.028, -0.018, 0.055)
compound_growth_explainer_years <- seq_along(compound_growth_explainer_annual_growth)
compound_growth_explainer_rate <- prod(1 + compound_growth_explainer_annual_growth)^(1 / length(compound_growth_explainer_annual_growth)) - 1
compound_growth_explainer_bar_data <- bind_rows(
  tibble(year = compound_growth_explainer_years, rate = compound_growth_explainer_annual_growth, series = "Crecimiento anual observado"),
  tibble(year = compound_growth_explainer_years, rate = rep(compound_growth_explainer_rate, length(compound_growth_explainer_years)), series = "Crecimiento compuesto equivalente")
) %>% mutate(series = factor(series, levels = c("Crecimiento anual observado", "Crecimiento compuesto equivalente")))
compound_growth_explainer_cumulative_data <- bind_rows(
  tibble(year = c(0, compound_growth_explainer_years), index_value = c(100, 100 * cumprod(1 + compound_growth_explainer_annual_growth)), series = "Trayectoria observada"),
  tibble(year = c(0, compound_growth_explainer_years), index_value = 100 * (1 + compound_growth_explainer_rate)^c(0, compound_growth_explainer_years), series = "Trayectoria compuesta equivalente")
)
compound_growth_explainer <- ggplot() +
  geom_col(data = compound_growth_explainer_bar_data, aes(x = year, y = rate, fill = series), position = position_dodge(width = 0.75), width = 0.68, alpha = 0.78) +
  geom_hline(yintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.35) +
  geom_line(data = compound_growth_explainer_cumulative_data, aes(x = year, y = (index_value - 100) / 100, color = series), linewidth = 0.9) +
  scale_fill_manual(values = c("Crecimiento anual observado" = mix_with_white(presentation_colors[["primary"]], 0.38), "Crecimiento compuesto equivalente" = mix_with_white(presentation_colors[["venezuela"]], 0.42))) +
  scale_color_manual(values = c("Trayectoria observada" = presentation_colors[["primary"]], "Trayectoria compuesta equivalente" = presentation_colors[["venezuela"]])) +
  scale_x_continuous(breaks = breaks_width(2), limits = c(0, 11)) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    breaks = seq(-0.05, 0.40, by = 0.05)
  ) +
  coord_cartesian(ylim = c(-0.05, 0.40), expand = FALSE) +
  labs(
    title = "Crecimiento anual y crecimiento compuesto",
    x = "Año de la simulación",
    y = "Variación",
    fill = NULL,
    color = NULL,
    subtitle = "Contrasta tasas anuales con el índice acumulado de dos ventanas históricas.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(legend.position = "bottom")

compound_growth_explainer <- apply_presentation_plot_style(compound_growth_explainer)

save_plot_variants(
  filename = file.path(figure_dir, "compound_growth_explainer.png"),
  plot = compound_growth_explainer,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(compound_growth_explainer)
message("Wrote ", file.path(figure_dir, "compound_growth_explainer.png"))

### Graph 33: Recuperación del PIB real al 15 por ciento ----

recovery_path_15_gdp_source_data <- index_series %>% filter(series_id == series_labels[["gdp"]])
recovery_path_15_gdp_data <- build_recovery_projection_data(recovery_path_15_gdp_source_data, scenario_rates = c(0.15), start_year = 1920L, average_end_year = 2013L)
recovery_path_15_gdp_labels <- recovery_path_15_gdp_data$projections %>%
  distinct(scenario_label, growth_rate) %>%
  arrange(desc(growth_rate)) %>%
  pull(scenario_label) %>%
  unique()
recovery_path_15_gdp_data$projections$scenario_label <- factor(recovery_path_15_gdp_data$projections$scenario_label, levels = recovery_path_15_gdp_labels)
recovery_path_15_gdp_colors <- build_priority_color_map(recovery_path_15_gdp_labels)
recovery_path_15_gdp_crossings <- recovery_path_15_gdp_data$projections %>%
  filter(index_peak_100 >= 100) %>%
  arrange(series_id, scenario_label, year) %>%
  group_by(series_id, scenario_label) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(cross_label = as.character(year))
recovery_path_15_gdp <- ggplot() +
  geom_hline(yintercept = 100, color = presentation_colors[["ink"]], linewidth = 0.45) +
  geom_line(data = recovery_path_15_gdp_data$historical, aes(x = year, y = index_peak_100), color = presentation_colors[["ink"]], linewidth = 0.9) +
  geom_line(data = recovery_path_15_gdp_data$projections, aes(x = year, y = index_peak_100, color = scenario_label), linewidth = 1.1) +
  geom_point(data = recovery_path_15_gdp_crossings, aes(x = year, y = index_peak_100, color = scenario_label), size = 3.2, show.legend = FALSE) +
  geom_text(data = recovery_path_15_gdp_crossings, aes(x = year, y = index_peak_100, label = cross_label, color = scenario_label), family = presentation_font_family, size = 3.8, nudge_y = 7, check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = max(recovery_path_15_gdp_data$historical$year, na.rm = TRUE), color = presentation_colors[["muted"]], linewidth = 0.35, linetype = "dashed") +
  presentation_recovery_year_axis(2100) +
  scale_y_continuous(labels = presentation_number_label(accuracy = 0.1), breaks = seq(0, 125, by = 25)) +
  scale_color_manual(values = recovery_path_15_gdp_colors) +
  coord_cartesian(ylim = c(0, 125), clip = "off", expand = FALSE) +
  labs(
    title = "Trayectoria de recuperación del PIB real al 15%",
    x = NULL,
    y = "Índice (pico histórico = 100)",
    color = "Crecimiento anual compuesto",
    subtitle = "Muestra cuánto tardaría el PIB real en recuperar referencias históricas con 15% anual.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(legend.position = "bottom", plot.margin = margin(12, 42, 12, 18)) +
  historical_event_reference_layers()

recovery_path_15_gdp <- apply_presentation_plot_style(recovery_path_15_gdp)

save_plot_variants(
  filename = file.path(figure_dir, "recovery_path_15_gdp.png"),
  plot = recovery_path_15_gdp,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(recovery_path_15_gdp)
message("Wrote ", file.path(figure_dir, "recovery_path_15_gdp.png"))

### Graph 34: Recuperación per cápita al 15 por ciento ----
recovery_path_15_gdp_per_capita_source_data <- index_series %>% filter(series_id == series_labels[["gdp_per_capita"]])
recovery_path_15_gdp_per_capita_data <- build_recovery_projection_data(recovery_path_15_gdp_per_capita_source_data, scenario_rates = c(0.15), start_year = 1920L, average_end_year = 2013L)
recovery_path_15_gdp_per_capita_labels <- recovery_path_15_gdp_per_capita_data$projections %>%
  distinct(scenario_label, growth_rate) %>%
  arrange(desc(growth_rate)) %>%
  pull(scenario_label) %>%
  unique()
recovery_path_15_gdp_per_capita_data$projections$scenario_label <- factor(recovery_path_15_gdp_per_capita_data$projections$scenario_label, levels = recovery_path_15_gdp_per_capita_labels)
recovery_path_15_gdp_per_capita_colors <- build_priority_color_map(recovery_path_15_gdp_per_capita_labels)
recovery_path_15_gdp_per_capita_crossings <- recovery_path_15_gdp_per_capita_data$projections %>%
  filter(index_peak_100 >= 100) %>%
  arrange(series_id, scenario_label, year) %>%
  group_by(series_id, scenario_label) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(cross_label = as.character(year))
recovery_path_15_gdp_per_capita <- ggplot() +
  geom_hline(yintercept = 100, color = presentation_colors[["ink"]], linewidth = 0.45) +
  geom_line(data = recovery_path_15_gdp_per_capita_data$historical, aes(x = year, y = index_peak_100), color = presentation_colors[["ink"]], linewidth = 0.9) +
  geom_line(data = recovery_path_15_gdp_per_capita_data$projections, aes(x = year, y = index_peak_100, color = scenario_label), linewidth = 1.1) +
  geom_point(data = recovery_path_15_gdp_per_capita_crossings, aes(x = year, y = index_peak_100, color = scenario_label), size = 3.2, show.legend = FALSE) +
  geom_text(data = recovery_path_15_gdp_per_capita_crossings, aes(x = year, y = index_peak_100, label = cross_label, color = scenario_label), family = presentation_font_family, size = 3.8, nudge_y = 7, check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = max(recovery_path_15_gdp_per_capita_data$historical$year, na.rm = TRUE), color = presentation_colors[["muted"]], linewidth = 0.35, linetype = "dashed") +
  presentation_recovery_year_axis(2100) +
  scale_y_continuous(labels = presentation_number_label(accuracy = 0.1), breaks = seq(0, 125, by = 25)) +
  scale_color_manual(values = recovery_path_15_gdp_per_capita_colors) +
  coord_cartesian(ylim = c(0, 125), clip = "off", expand = FALSE) +
  labs(
    title = "Trayectoria de recuperación per cápita al 15%",
    x = NULL,
    y = "Índice (pico histórico = 100)",
    color = "Crecimiento anual compuesto",
    subtitle = "Muestra cuánto tardaría el PIB real per cápita en recuperar referencias históricas con 15% anual.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(legend.position = "bottom", plot.margin = margin(12, 42, 12, 18)) +
  historical_event_reference_layers()

recovery_path_15_gdp_per_capita <- apply_presentation_plot_style(recovery_path_15_gdp_per_capita)

save_plot_variants(
  filename = file.path(figure_dir, "recovery_path_15_gdp_per_capita.png"),
  plot = recovery_path_15_gdp_per_capita,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(recovery_path_15_gdp_per_capita)
message("Wrote ", file.path(figure_dir, "recovery_path_15_gdp_per_capita.png"))

### Graph 35: Escenarios del PIB real ----
recovery_path_scenarios_gdp_source_data <- index_series %>% filter(series_id == series_labels[["gdp"]])
recovery_path_scenarios_gdp_data <- build_recovery_projection_data(recovery_path_scenarios_gdp_source_data, scenario_rates = c(0.10, 0.07, 0.05, 0.02), start_year = 1920L, average_end_year = 2013L)
recovery_path_scenarios_gdp_labels <- recovery_path_scenarios_gdp_data$projections %>%
  distinct(scenario_label, growth_rate) %>%
  arrange(desc(growth_rate)) %>%
  pull(scenario_label) %>%
  unique()
recovery_path_scenarios_gdp_data$projections$scenario_label <- factor(recovery_path_scenarios_gdp_data$projections$scenario_label, levels = recovery_path_scenarios_gdp_labels)
recovery_path_scenarios_gdp_colors <- build_priority_color_map(recovery_path_scenarios_gdp_labels)
recovery_path_scenarios_gdp_crossings <- recovery_path_scenarios_gdp_data$projections %>%
  filter(index_peak_100 >= 100) %>%
  arrange(series_id, scenario_label, year) %>%
  group_by(series_id, scenario_label) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(cross_label = as.character(year))
recovery_path_scenarios_gdp <- ggplot() +
  geom_hline(yintercept = 100, color = presentation_colors[["ink"]], linewidth = 0.45) +
  geom_line(data = recovery_path_scenarios_gdp_data$historical, aes(x = year, y = index_peak_100), color = presentation_colors[["ink"]], linewidth = 0.9) +
  geom_line(data = recovery_path_scenarios_gdp_data$projections, aes(x = year, y = index_peak_100, color = scenario_label), linewidth = 1.1) +
  geom_point(data = recovery_path_scenarios_gdp_crossings, aes(x = year, y = index_peak_100, color = scenario_label), size = 3.2, show.legend = FALSE) +
  geom_text(data = recovery_path_scenarios_gdp_crossings, aes(x = year, y = index_peak_100, label = cross_label, color = scenario_label), family = presentation_font_family, size = 3.8, nudge_y = 7, check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = max(recovery_path_scenarios_gdp_data$historical$year, na.rm = TRUE), color = presentation_colors[["muted"]], linewidth = 0.35, linetype = "dashed") +
  presentation_recovery_year_axis(2100) +
  scale_y_continuous(labels = presentation_number_label(accuracy = 0.1), breaks = seq(0, 125, by = 25)) +
  scale_color_manual(values = recovery_path_scenarios_gdp_colors) +
  coord_cartesian(ylim = c(0, 125), clip = "off", expand = FALSE) +
  labs(
    title = "Escenarios de recuperación del PIB real",
    x = NULL,
    y = "Índice (pico histórico = 100)",
    color = "Crecimiento anual compuesto",
    subtitle = "Compara escenarios de recuperación del PIB real con tasas anuales fijas.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(legend.position = "bottom", plot.margin = margin(12, 42, 12, 18)) +
  historical_event_reference_layers()

recovery_path_scenarios_gdp <- apply_presentation_plot_style(recovery_path_scenarios_gdp)

save_plot_variants(
  filename = file.path(figure_dir, "recovery_path_scenarios_gdp.png"),
  plot = recovery_path_scenarios_gdp,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(recovery_path_scenarios_gdp)
message("Wrote ", file.path(figure_dir, "recovery_path_scenarios_gdp.png"))

### Graph 36: Escenarios per cápita ----
recovery_path_scenarios_gdp_per_capita_source_data <- index_series %>% filter(series_id == series_labels[["gdp_per_capita"]])
recovery_path_scenarios_gdp_per_capita_data <- build_recovery_projection_data(recovery_path_scenarios_gdp_per_capita_source_data, scenario_rates = c(0.10, 0.07, 0.05, 0.02), start_year = 1920L, average_end_year = 2013L)
recovery_path_scenarios_gdp_per_capita_labels <- recovery_path_scenarios_gdp_per_capita_data$projections %>%
  distinct(scenario_label, growth_rate) %>%
  arrange(desc(growth_rate)) %>%
  pull(scenario_label) %>%
  unique()
recovery_path_scenarios_gdp_per_capita_data$projections$scenario_label <- factor(recovery_path_scenarios_gdp_per_capita_data$projections$scenario_label, levels = recovery_path_scenarios_gdp_per_capita_labels)
recovery_path_scenarios_gdp_per_capita_colors <- build_priority_color_map(recovery_path_scenarios_gdp_per_capita_labels)
recovery_path_scenarios_gdp_per_capita_crossings <- recovery_path_scenarios_gdp_per_capita_data$projections %>%
  filter(index_peak_100 >= 100) %>%
  arrange(series_id, scenario_label, year) %>%
  group_by(series_id, scenario_label) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(cross_label = as.character(year))
recovery_path_scenarios_gdp_per_capita <- ggplot() +
  geom_hline(yintercept = 100, color = presentation_colors[["ink"]], linewidth = 0.45) +
  geom_line(data = recovery_path_scenarios_gdp_per_capita_data$historical, aes(x = year, y = index_peak_100), color = presentation_colors[["ink"]], linewidth = 0.9) +
  geom_line(data = recovery_path_scenarios_gdp_per_capita_data$projections, aes(x = year, y = index_peak_100, color = scenario_label), linewidth = 1.1) +
  geom_point(data = recovery_path_scenarios_gdp_per_capita_crossings, aes(x = year, y = index_peak_100, color = scenario_label), size = 3.2, show.legend = FALSE) +
  geom_text(data = recovery_path_scenarios_gdp_per_capita_crossings, aes(x = year, y = index_peak_100, label = cross_label, color = scenario_label), family = presentation_font_family, size = 3.8, nudge_y = 7, check_overlap = TRUE, show.legend = FALSE) +
  geom_vline(xintercept = max(recovery_path_scenarios_gdp_per_capita_data$historical$year, na.rm = TRUE), color = presentation_colors[["muted"]], linewidth = 0.35, linetype = "dashed") +
  presentation_recovery_year_axis(2100) +
  scale_y_continuous(labels = presentation_number_label(accuracy = 0.1), breaks = seq(0, 125, by = 25)) +
  scale_color_manual(values = recovery_path_scenarios_gdp_per_capita_colors) +
  coord_cartesian(ylim = c(0, 125), clip = "off", expand = FALSE) +
  labs(
    title = "Escenarios de recuperación per cápita",
    x = NULL,
    y = "Índice (pico histórico = 100)",
    color = "Crecimiento anual compuesto",
    subtitle = "Compara escenarios de recuperación per cápita con tasas anuales fijas.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(legend.position = "bottom", plot.margin = margin(12, 42, 12, 18)) +
  historical_event_reference_layers()

recovery_path_scenarios_gdp_per_capita <- apply_presentation_plot_style(recovery_path_scenarios_gdp_per_capita)

save_plot_variants(
  filename = file.path(figure_dir, "recovery_path_scenarios_gdp_per_capita.png"),
  plot = recovery_path_scenarios_gdp_per_capita,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(recovery_path_scenarios_gdp_per_capita)
message("Wrote ", file.path(figure_dir, "recovery_path_scenarios_gdp_per_capita.png"))

### Graph 37: Años para recuperar el PIB real ----
recovery_heatmap_gdp_data <- build_recovery_heatmap_data(index_series, selected_series = series_labels[["gdp"]], n_reference_years = 15L) %>% mutate(text_color = if_else(years_to_recover >= 75, "white", presentation_colors[["ink"]]))
recovery_heatmap_gdp <- recovery_heatmap_gdp_data %>% ggplot(aes(x = column_label, y = rate_label, fill = years_to_recover)) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_text(aes(label = years_to_recover, color = text_color), size = 5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradient(low = presentation_colors[["light"]], high = presentation_colors[["primary"]], name = "Años") +
  labs(
    title = "Años para recuperar niveles históricos del PIB real",
    x = "Año de referencia e Índice (pico = 100)",
    y = "TCAC anual",
    subtitle = "Calcula años necesarios para recuperar niveles históricos del PIB real por tasa anual.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size + 1, base_family = presentation_font_family) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 13.5, family = presentation_font_family), axis.text.y = element_text(size = 14, family = presentation_font_family), panel.grid = element_blank(), legend.position = "bottom")

recovery_heatmap_gdp <- apply_presentation_plot_style(recovery_heatmap_gdp)

save_plot_variants(
  filename = file.path(figure_dir, "recovery_heatmap_gdp.png"),
  plot = recovery_heatmap_gdp,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(recovery_heatmap_gdp)
message("Wrote ", file.path(figure_dir, "recovery_heatmap_gdp.png"))

### Graph 38: Años para recuperar per cápita ----
recovery_heatmap_gdp_per_capita_data <- build_recovery_heatmap_data(index_series, selected_series = series_labels[["gdp_per_capita"]], n_reference_years = 15L) %>% mutate(text_color = if_else(years_to_recover >= 75, "white", presentation_colors[["ink"]]))
recovery_heatmap_gdp_per_capita <- recovery_heatmap_gdp_per_capita_data %>% ggplot(aes(x = column_label, y = rate_label, fill = years_to_recover)) +
  geom_tile(color = "white", linewidth = 0.35) +
  geom_text(aes(label = years_to_recover, color = text_color), size = 5, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradient(low = presentation_colors[["light"]], high = presentation_colors[["primary"]], name = "Años") +
  labs(
    title = "Años para recuperar niveles históricos del PIB real per cápita",
    x = "Año de referencia e Índice (pico = 100)",
    y = "TCAC anual",
    subtitle = "Calcula años necesarios para recuperar niveles históricos per cápita por tasa anual.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size + 1, base_family = presentation_font_family) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 13.5, family = presentation_font_family), axis.text.y = element_text(size = 14, family = presentation_font_family), panel.grid = element_blank(), legend.position = "bottom")

recovery_heatmap_gdp_per_capita <- apply_presentation_plot_style(recovery_heatmap_gdp_per_capita)

save_plot_variants(
  filename = file.path(figure_dir, "recovery_heatmap_gdp_per_capita.png"),
  plot = recovery_heatmap_gdp_per_capita,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(recovery_heatmap_gdp_per_capita)
message("Wrote ", file.path(figure_dir, "recovery_heatmap_gdp_per_capita.png"))

## Family: rolling TCAC ----

### Graph 39: Ventanas móviles de TCAC ----
rolling_cagr_example_series_data <- index_series %>%
  filter(series_id == series_labels[["gdp_per_capita"]])

rolling_cagr_example_windows <- tibble(
  start_year = c(1903L, 2003L),
  end_year = c(1913L, 2013L)
)

rolling_cagr_example_window_summaries <- rolling_cagr_example_windows %>%
  rowwise() %>%
  mutate(
    start_index = rolling_cagr_example_series_data$index_value[rolling_cagr_example_series_data$year == start_year][[1]],
    end_index = rolling_cagr_example_series_data$index_value[rolling_cagr_example_series_data$year == end_year][[1]],
    cagr = (end_index / start_index)^(1 / (end_year - start_year)) - 1,
    window_label = sprintf("%s-%s", start_year, end_year),
    label = sprintf("%s\nTCAC %s", window_label, percent(cagr, accuracy = 0.1))
  ) %>%
  ungroup()

rolling_cagr_example_colors <- stats::setNames(
  presentation_ordered_colors[seq_len(nrow(rolling_cagr_example_window_summaries))],
  rolling_cagr_example_window_summaries$window_label
)

rolling_cagr_example_window_data <- rolling_cagr_example_windows %>%
  mutate(window_label = sprintf("%s-%s", start_year, end_year)) %>%
  crossing(rolling_cagr_example_series_data) %>%
  filter(year >= start_year, year <= end_year)

rolling_cagr_example_endpoint_data <- rolling_cagr_example_window_data %>%
  filter(year %in% c(min(year), max(year)), .by = window_label)

rolling_cagr_example_label_data <- rolling_cagr_example_window_data %>%
  filter(year == max(year), .by = window_label) %>%
  left_join(
    rolling_cagr_example_window_summaries %>% select(window_label, label),
    by = "window_label"
  ) %>%
  mutate(
    label_nudge_y = if_else(
      window_label == rolling_cagr_example_window_summaries$window_label[[1]],
      450,
      0
    )
  )

rolling_cagr_example_early_window_data <- rolling_cagr_example_window_data %>%
  filter(window_label == rolling_cagr_example_window_summaries$window_label[[1]])
rolling_cagr_example_recent_window_data <- rolling_cagr_example_window_data %>%
  filter(window_label == rolling_cagr_example_window_summaries$window_label[[2]])
rolling_cagr_example_early_endpoint_data <- rolling_cagr_example_endpoint_data %>%
  filter(window_label == rolling_cagr_example_window_summaries$window_label[[1]])
rolling_cagr_example_recent_endpoint_data <- rolling_cagr_example_endpoint_data %>%
  filter(window_label == rolling_cagr_example_window_summaries$window_label[[2]])

rolling_cagr_example <- rolling_cagr_example_series_data %>%
  ggplot(aes(x = year, y = index_value)) +
  geom_line(color = presentation_colors[["muted"]], linewidth = 0.55) +
  geom_line(data = rolling_cagr_example_early_window_data, color = rolling_cagr_example_colors[[1]], linewidth = 1) +
  geom_line(data = rolling_cagr_example_recent_window_data, color = rolling_cagr_example_colors[[2]], linewidth = 1) +
  geom_point(data = rolling_cagr_example_early_endpoint_data, color = rolling_cagr_example_colors[[1]], size = 2.8, show.legend = FALSE) +
  geom_point(data = rolling_cagr_example_recent_endpoint_data, color = rolling_cagr_example_colors[[2]], size = 2.8, show.legend = FALSE) +
  geom_label_repel(
    data = rolling_cagr_example_label_data,
    aes(x = year, y = index_value, label = label),
    inherit.aes = FALSE,
    family = presentation_font_family,
    color = presentation_colors[["ink"]],
    size = presentation_label_text_size,
    fill = "white",
    label.size = presentation_label_box_linewidth,
    label.padding = presentation_label_padding,
    label.r = presentation_label_radius,
    box.padding = presentation_label_box_padding,
    point.padding = presentation_label_point_padding,
    segment.color = presentation_colors[["muted"]],
    segment.size = presentation_label_segment_size,
    min.segment.length = 0,
    nudge_y = rolling_cagr_example_label_data$label_nudge_y,
    seed = 1234,
    show.legend = FALSE
  ) +
  presentation_full_history_year_axis() +
  scale_y_continuous(
    labels = presentation_number_label(accuracy = 0.1),
    limits = c(0, 5000),
    breaks = seq(0, 5000, by = 1000)
  ) +
  labs(
    title = "Crecimiento compuesto en ventanas móviles",
    x = NULL,
    y = "Índice histórico (1830 = 100)",
    subtitle = "Contrasta dos ventanas móviles para mostrar cómo se calcula el crecimiento compuesto.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  historical_event_reference_layers()

rolling_cagr_example <- apply_presentation_plot_style(rolling_cagr_example) +
  # Reserve the same bottom band occupied by legends in companion TCAC graphs.
  theme(
    plot.caption = element_text(
      margin = margin(t = 34)
    )
  )

save_plot_variants(
  filename = file.path(figure_dir, "rolling_cagr_example.png"),
  plot = rolling_cagr_example,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(rolling_cagr_example)
message("Wrote ", file.path(figure_dir, "rolling_cagr_example.png"))

### Graph 40: TCAC móvil del PIB real ----
rolling_cagr_lines_gdp_data <- rolling_cagr %>%
  filter(series_id == series_labels[["gdp"]], window_years %in% c(5L, 10L, 15L, 20L)) %>%
  mutate(window_label = factor(paste0(window_years, " años"), levels = paste0(c(5L, 10L, 15L, 20L), " años")))
rolling_cagr_lines_gdp_colors <- build_priority_color_map(rolling_cagr_lines_gdp_data$window_label)
rolling_cagr_lines_gdp <- rolling_cagr_lines_gdp_data %>% ggplot(aes(x = end_year, y = cagr, color = window_label)) +
  geom_hline(yintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.3) +
  geom_line(linewidth = 0.65, alpha = 0.9) +
  scale_color_manual(values = rolling_cagr_lines_gdp_colors) +
  presentation_full_history_year_axis() +
  scale_y_continuous(labels = label_percent(accuracy = 1), limits = c(-0.25, 0.25), breaks = seq(-0.25, 0.25, by = 0.05)) +
  labs(
    title = "Crecimiento sostenido en ventanas móviles del PIB real",
    x = "Año final de la ventana",
    y = "TCAC de la ventana",
    color = "Ventana",
    subtitle = "Traza TCACs móviles del PIB real por longitud de ventana.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(legend.position = "bottom") +
  historical_event_reference_layers()

rolling_cagr_lines_gdp <- apply_presentation_plot_style(rolling_cagr_lines_gdp)

save_plot_variants(
  filename = file.path(figure_dir, "rolling_cagr_lines_gdp.png"),
  plot = rolling_cagr_lines_gdp,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(rolling_cagr_lines_gdp)
message("Wrote ", file.path(figure_dir, "rolling_cagr_lines_gdp.png"))

### Graph 41: TCAC móvil per cápita ----
rolling_cagr_lines_gdp_per_capita_data <- rolling_cagr %>%
  filter(series_id == series_labels[["gdp_per_capita"]], window_years %in% c(5L, 10L, 15L, 20L)) %>%
  mutate(window_label = factor(paste0(window_years, " años"), levels = paste0(c(5L, 10L, 15L, 20L), " años")))
rolling_cagr_lines_gdp_per_capita_colors <- build_priority_color_map(rolling_cagr_lines_gdp_per_capita_data$window_label)
rolling_cagr_lines_gdp_per_capita <- rolling_cagr_lines_gdp_per_capita_data %>% ggplot(aes(x = end_year, y = cagr, color = window_label)) +
  geom_hline(yintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.3) +
  geom_line(linewidth = 0.65, alpha = 0.9) +
  scale_color_manual(values = rolling_cagr_lines_gdp_per_capita_colors) +
  presentation_full_history_year_axis() +
  scale_y_continuous(labels = label_percent(accuracy = 1), limits = c(-0.25, 0.25), breaks = seq(-0.25, 0.25, by = 0.05)) +
  labs(
    title = "Crecimiento sostenido en ventanas móviles per cápita",
    x = "Año final de la ventana",
    y = "TCAC de la ventana",
    color = "Ventana",
    subtitle = "Traza TCACs móviles per cápita por longitud de ventana.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(legend.position = "bottom") +
  historical_event_reference_layers()

rolling_cagr_lines_gdp_per_capita <- apply_presentation_plot_style(rolling_cagr_lines_gdp_per_capita)

save_plot_variants(
  filename = file.path(figure_dir, "rolling_cagr_lines_gdp_per_capita.png"),
  plot = rolling_cagr_lines_gdp_per_capita,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(rolling_cagr_lines_gdp_per_capita)
message("Wrote ", file.path(figure_dir, "rolling_cagr_lines_gdp_per_capita.png"))

### Graph 42: Distribución TCAC ----
rolling_cagr_distribution_data <- rolling_cagr %>%
  filter(window_years %in% c(3L, 5L, 7L, 10L, 15L, 20L)) %>%
  mutate(window_label = factor(paste0(window_years, " años"), levels = paste0(c(3L, 5L, 7L, 10L, 15L, 20L), " años")))
rolling_cagr_distribution_colors <- build_priority_color_map(rolling_cagr_distribution_data$window_label)
rolling_cagr_distribution <- rolling_cagr_distribution_data %>% ggplot(aes(x = cagr, fill = window_label)) +
  geom_vline(xintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.35) +
  geom_density(alpha = 0.18, linewidth = 0.65, color = NA) +
  geom_density(aes(color = window_label), fill = NA, linewidth = 0.75) +
  scale_fill_manual(values = rolling_cagr_distribution_colors) +
  scale_color_manual(values = rolling_cagr_distribution_colors) +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  scale_y_continuous(breaks = waiver()) +
  labs(
    title = "Distribución de TCACs en ventanas móviles",
    x = "TCAC de la ventana",
    y = "Densidad",
    fill = "Ventana",
    color = "Ventana",
    subtitle = "Resume la distribución de TCACs móviles por serie y ventana.",
    caption = append_caption_note(presentation_source_caption, density_note)
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(legend.position = "bottom") +
  facet_wrap(vars(series_id), ncol = 1)

rolling_cagr_distribution <- apply_presentation_plot_style(rolling_cagr_distribution)

save_plot_variants(
  filename = file.path(figure_dir, "rolling_cagr_distribution.png"),
  plot = rolling_cagr_distribution,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(rolling_cagr_distribution)
message("Wrote ", file.path(figure_dir, "rolling_cagr_distribution.png"))

### Graph 43: Distribución TCAC PIB real ----
rolling_cagr_distribution_gdp_data <- rolling_cagr %>%
  filter(series_id == series_labels[["gdp"]]) %>%
  filter(window_years %in% c(3L, 5L, 7L, 10L, 15L, 20L)) %>%
  mutate(window_label = factor(paste0(window_years, " años"), levels = paste0(c(3L, 5L, 7L, 10L, 15L, 20L), " años")))
rolling_cagr_distribution_gdp_colors <- build_priority_color_map(rolling_cagr_distribution_gdp_data$window_label)
rolling_cagr_distribution_gdp <- rolling_cagr_distribution_gdp_data %>% ggplot(aes(x = cagr, fill = window_label)) +
  geom_vline(xintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.35) +
  geom_density(alpha = 0.18, linewidth = 0.65, color = NA) +
  geom_density(aes(color = window_label), fill = NA, linewidth = 0.75) +
  scale_fill_manual(values = rolling_cagr_distribution_gdp_colors) +
  scale_color_manual(values = rolling_cagr_distribution_gdp_colors) +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  scale_y_continuous(breaks = seq(0, 16, by = 4)) +
  labs(
    title = "Distribución de crecimiento sostenido del PIB real",
    x = "TCAC de la ventana",
    y = "Densidad",
    fill = "Ventana",
    color = "Ventana",
    subtitle = "Resume la distribución de TCACs móviles del PIB real.",
    caption = append_caption_note(presentation_source_caption, density_note)
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0, 16), expand = FALSE)

rolling_cagr_distribution_gdp <- apply_presentation_plot_style(rolling_cagr_distribution_gdp)

save_plot_variants(
  filename = file.path(figure_dir, "rolling_cagr_distribution_gdp.png"),
  plot = rolling_cagr_distribution_gdp,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(rolling_cagr_distribution_gdp)
message("Wrote ", file.path(figure_dir, "rolling_cagr_distribution_gdp.png"))

### Graph 44: Distribución TCAC per cápita ----
rolling_cagr_distribution_gdp_per_capita_data <- rolling_cagr %>%
  filter(series_id == series_labels[["gdp_per_capita"]]) %>%
  filter(window_years %in% c(3L, 5L, 7L, 10L, 15L, 20L)) %>%
  mutate(window_label = factor(paste0(window_years, " años"), levels = paste0(c(3L, 5L, 7L, 10L, 15L, 20L), " años")))
rolling_cagr_distribution_gdp_per_capita_colors <- build_priority_color_map(rolling_cagr_distribution_gdp_per_capita_data$window_label)
rolling_cagr_distribution_gdp_per_capita <- rolling_cagr_distribution_gdp_per_capita_data %>% ggplot(aes(x = cagr, fill = window_label)) +
  geom_vline(xintercept = 0, color = presentation_colors[["ink"]], linewidth = 0.35) +
  geom_density(alpha = 0.18, linewidth = 0.65, color = NA) +
  geom_density(aes(color = window_label), fill = NA, linewidth = 0.75) +
  scale_fill_manual(values = rolling_cagr_distribution_gdp_per_capita_colors) +
  scale_color_manual(values = rolling_cagr_distribution_gdp_per_capita_colors) +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  labs(
    title = "Distribución de crecimiento sostenido per cápita",
    x = "TCAC de la ventana",
    y = "Densidad",
    fill = "Ventana",
    color = "Ventana",
    subtitle = "Resume la distribución de TCACs móviles del PIB real per cápita.",
    caption = append_caption_note(presentation_source_caption, density_note)
  ) +
  theme_minimal(base_size = presentation_base_size, base_family = presentation_font_family) +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0, 20), expand = FALSE)

rolling_cagr_distribution_gdp_per_capita <- apply_presentation_plot_style(rolling_cagr_distribution_gdp_per_capita)

save_plot_variants(
  filename = file.path(figure_dir, "rolling_cagr_distribution_gdp_per_capita.png"),
  plot = rolling_cagr_distribution_gdp_per_capita,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(rolling_cagr_distribution_gdp_per_capita)
message("Wrote ", file.path(figure_dir, "rolling_cagr_distribution_gdp_per_capita.png"))

### Graph 45: Frecuencia TCAC PIB real ----
rolling_cagr_heatmap_gdp_selected <- rolling_cagr %>% filter(series_id == series_labels[["gdp"]], window_years %in% c(3L, 5L, 7L, 10L, 15L, 20L))
rolling_cagr_heatmap_gdp_data <- merge(rolling_cagr_heatmap_gdp_selected, data.frame(threshold = c(0.02, 0.05, 0.07, 0.10, 0.15))) %>%
  group_by(window_years, threshold) %>%
  summarise(share = mean(cagr >= threshold, na.rm = TRUE), .groups = "drop") %>%
  mutate(window_label = factor(paste0(window_years, " años"), levels = paste0(rev(c(3L, 5L, 7L, 10L, 15L, 20L)), " años")), threshold_label = factor(percent(threshold, accuracy = 1), levels = percent(c(0.02, 0.05, 0.07, 0.10, 0.15), accuracy = 1)), text_color = if_else(share >= 0.3, "white", presentation_colors[["ink"]]))
rolling_cagr_heatmap_gdp <- rolling_cagr_heatmap_gdp_data %>% ggplot(aes(x = threshold_label, y = window_label, fill = share)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = percent(share, accuracy = 1), color = text_color), size = 5.4, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradient(low = presentation_colors[["light"]], high = presentation_colors[["primary"]], labels = label_percent(accuracy = 1), name = "Frecuencia") +
  labs(
    title = "Frecuencia histórica de crecimiento sostenido del PIB real",
    x = "TCAC mínimo",
    y = "Duración consecutiva",
    subtitle = "Cuenta la frecuencia de TCACs móviles del PIB real por rango y ventana.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size + 2, base_family = presentation_font_family) +
  theme(panel.grid = element_blank(), legend.position = "bottom")

rolling_cagr_heatmap_gdp <- apply_presentation_plot_style(rolling_cagr_heatmap_gdp)

save_plot_variants(
  filename = file.path(figure_dir, "rolling_cagr_heatmap_gdp.png"),
  plot = rolling_cagr_heatmap_gdp,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(rolling_cagr_heatmap_gdp)
message("Wrote ", file.path(figure_dir, "rolling_cagr_heatmap_gdp.png"))

### Graph 46: Frecuencia TCAC per cápita ----
rolling_cagr_heatmap_gdp_per_capita_selected <- rolling_cagr %>% filter(series_id == series_labels[["gdp_per_capita"]], window_years %in% c(3L, 5L, 7L, 10L, 15L, 20L))
rolling_cagr_heatmap_gdp_per_capita_data <- merge(rolling_cagr_heatmap_gdp_per_capita_selected, data.frame(threshold = c(0.02, 0.05, 0.07, 0.10, 0.15))) %>%
  group_by(window_years, threshold) %>%
  summarise(share = mean(cagr >= threshold, na.rm = TRUE), .groups = "drop") %>%
  mutate(window_label = factor(paste0(window_years, " años"), levels = paste0(rev(c(3L, 5L, 7L, 10L, 15L, 20L)), " años")), threshold_label = factor(percent(threshold, accuracy = 1), levels = percent(c(0.02, 0.05, 0.07, 0.10, 0.15), accuracy = 1)), text_color = if_else(share >= 0.3, "white", presentation_colors[["ink"]]))
rolling_cagr_heatmap_gdp_per_capita <- rolling_cagr_heatmap_gdp_per_capita_data %>% ggplot(aes(x = threshold_label, y = window_label, fill = share)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = percent(share, accuracy = 1), color = text_color), size = 5.4, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradient(low = presentation_colors[["light"]], high = presentation_colors[["primary"]], labels = label_percent(accuracy = 1), name = "Frecuencia") +
  labs(
    title = "Frecuencia histórica de crecimiento sostenido per cápita",
    x = "TCAC mínimo",
    y = "Duración consecutiva",
    subtitle = "Cuenta la frecuencia de TCACs móviles per cápita por rango y ventana.",
    caption = presentation_source_caption
  ) +
  theme_minimal(base_size = presentation_base_size + 2, base_family = presentation_font_family) +
  theme(panel.grid = element_blank(), legend.position = "bottom")

rolling_cagr_heatmap_gdp_per_capita <- apply_presentation_plot_style(rolling_cagr_heatmap_gdp_per_capita)

save_plot_variants(
  filename = file.path(figure_dir, "rolling_cagr_heatmap_gdp_per_capita.png"),
  plot = rolling_cagr_heatmap_gdp_per_capita,
  width = presentation_plot_width,
  height = presentation_plot_height,
  dpi = presentation_plot_dpi
)

print(rolling_cagr_heatmap_gdp_per_capita)
message("Wrote ", file.path(figure_dir, "rolling_cagr_heatmap_gdp_per_capita.png"))
