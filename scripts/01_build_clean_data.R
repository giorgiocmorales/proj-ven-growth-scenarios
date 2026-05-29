# Build cleaned historical data from the raw workbook.
if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("The `readxl` package is required to run this script.", call. = FALSE)
}

if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("The `dplyr` package is required to run this script.", call. = FALSE)
}

dir.create("data/interim", recursive = TRUE, showWarnings = FALSE)

raw_data_path <- "data/raw/milagros_economicos.xlsx"
if (!file.exists(raw_data_path)) {
  stop(sprintf("Raw workbook not found at `%s`.", raw_data_path), call. = FALSE)
}

raw_data <- readxl::read_excel(raw_data_path, sheet = "PIB Historico")

message("Raw data preview:")
print(utils::head(raw_data))

clean_data <- raw_data |>
  dplyr::transmute(
    date = as.Date(Fecha),
    year = as.integer(`Año`),
    gdp_growth = as.numeric(Crecimiento),
    gdp_pc_growth = as.numeric(`Crecimiento ppc`),
    gdp_index = as.numeric(`Índice (1830)`),
    gdp_pc_index = as.numeric(`Índice ppc (1830)`),
    population = as.numeric(`Poblacion Estimada`)
  ) |>
  dplyr::arrange(year, date) |>
  dplyr::filter(
    !is.na(year),
    !is.na(gdp_index) | !is.na(gdp_pc_index) |
      !is.na(gdp_growth) | !is.na(gdp_pc_growth)
  )

first_year <- min(clean_data$year, na.rm = TRUE)

clean_data <- clean_data |>
  dplyr::mutate(
    gdp_growth = dplyr::if_else(year == first_year & is.na(gdp_growth), 0, gdp_growth),
    gdp_pc_growth = dplyr::if_else(year == first_year & is.na(gdp_pc_growth), 0, gdp_pc_growth)
  )

clean_summary <- data.frame(
  metric = c(
    "row_count",
    "year_min",
    "year_max",
    "gdp_growth_missing",
    "gdp_pc_growth_missing",
    "gdp_index_missing",
    "gdp_pc_index_missing",
    "population_missing"
  ),
  value = c(
    nrow(clean_data),
    min(clean_data$year, na.rm = TRUE),
    max(clean_data$year, na.rm = TRUE),
    sum(is.na(clean_data$gdp_growth)),
    sum(is.na(clean_data$gdp_pc_growth)),
    sum(is.na(clean_data$gdp_index)),
    sum(is.na(clean_data$gdp_pc_index)),
    sum(is.na(clean_data$population))
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(clean_data, "data/interim/clean_historical_data.csv", row.names = FALSE)
utils::write.csv(clean_summary, "data/interim/clean_historical_summary.csv", row.names = FALSE)

message("Clean data preview:")
print(utils::head(clean_data))

message("Wrote data/interim/clean_historical_data.csv")
message("Wrote data/interim/clean_historical_summary.csv")
