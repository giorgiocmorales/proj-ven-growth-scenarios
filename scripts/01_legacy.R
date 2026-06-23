# Legacy exploratory notebook-style script for early Venezuela growth work.
# The maintained pipeline now uses the numbered download/build/graph scripts.

## Packages ----
# Attach the packages used in the original ad hoc exploration.
library(shiny)
library(readxl)
library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(zoo)

## Raw workbook import ----
# Read the historical GDP sheet from the original workbook and print a quick preview.
pib_data <- read_excel("data/raw/Milagros_economicos.xlsx", sheet = "PIB Historico")
print(pib_data)

## Basic cleaning ----
# Keep the historical span used in the presentation and standardize key types.
pib_data <- pib_data %>%
  filter(Año <= 2025) %>%
  mutate(Fecha = as.Date(Fecha)) %>%
  janitor::clean_names() %>%
  arrange(fecha) %>%
  mutate(crecimiento = as.numeric(crecimiento))

## Exploratory checks ----
# Inspect the cleaned table shape before deriving episodes.
summary(pib_data)
str(pib_data)

## Growth/contraction episodes ----
# Group consecutive years with the same growth sign into boom-bust episodes.
pib_episodes <- pib_data %>%
  mutate(
    signo = case_when(
      is.na(crecimiento) ~ NA_integer_,
      crecimiento >= 0 ~ 1L,
      crecimiento < 0 ~ -1L,
      TRUE ~ 0L
    ),
    # Fill missing signs so blank years remain attached to adjacent episodes.
    signo = zoo::na.locf(signo, na.rm = FALSE),
    signo = zoo::na.locf(signo, fromLast = TRUE, na.rm = FALSE),
    episode_id = 1L + cumsum(if_else(is.na(lag(signo)), 0L, as.integer(signo != lag(signo))))
  ) %>%
  group_by(episode_id) %>%
  # Rebase each episode to 100 to compare cumulative movement within the episode.
  mutate(episode_index = 100 * cumprod(1 + replace_na(crecimiento, 0))) %>%
  ungroup() %>%
  mutate(overlap_flag = 0L)
