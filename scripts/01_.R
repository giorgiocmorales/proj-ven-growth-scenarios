#VEN growth scenarios and simulator

#Load Packages
library(shiny)
library(readxl)
library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(zoo)

#Load GDP data
pib_data <- read_excel("data/raw/Milagros_economicos.xlsx", sheet = "PIB Historico")
view(pib_data)

#Analyse data structure
summary(pib_data)
str(pib_data)

#Rewrite Fecha column, limit initial size to 2024
pib_data <- pib_data %>%
  filter(Año <= 2025) %>%
  mutate(Fecha = as.Date(Fecha))

#Analyse data structure
summary(pib_data)
str(pib_data)

# Clean names for ease of data wrangling
pib_data <- pib_data %>%
  janitor::clean_names() %>%
  arrange(fecha) %>%
  mutate(crecimiento = as.numeric(crecimiento))

# Growth/Contraction episodes


pib_data <- pib_data %>%
  mutate(
    sign = case_when(
      is.na(crecimiento) ~ NA_integer_,
      crecimiento >= 0 ~ 1L,
      crecimiento < 0 ~ -1L,
      TRUE ~ 0L),
    sign = zoo::na.locf(sign, na.rm = FALSE),
    sign = zoo::na.locf(sign, na.rm = FALSE))


  mutate(
    s_raw = case_when(
      is.na(crecimiento) ~ NA_integer_,
      crecimiento >= 0 ~  1L,
      crecimiento < 0 ~ -1L,
      TRUE ~ 0L
    ),
    s = {
      # carry last non-zero sign forward (and backward for leading zeros if any)
      tmp <- s_raw
      tmp[tmp == 0] <- NA
      tmp <- tidyr::fill(tibble(x = tmp), x, .direction = "down")$x
      tmp <- tidyr::fill(tibble(x = tmp), x, .direction = "up")$x
      tmp
    }
  ) %>%
  mutate(
    sign_change = if_else(is.na(lag(s)), 0L, if_else(s != lag(s), 1L, 0L)),
    episode_id  = 1L + cumsum(sign_change)
  )

# 2) Within-episode index (starts at 100, then compounds by crecimiento)
df2 <- df1 %>%
  group_by(episode_id) %>%
  arrange(fecha, .by_group = TRUE) %>%
  mutate(
    episode_index = 100 * cumprod(1 + replace_na(crecimiento, 0))
  ) %>%
  ungroup()

# 3) Add overlap rows: first row of each episode (except episode 1) duplicated with index=100
overlap_rows <- df2 %>%
  group_by(episode_id) %>%
  slice(1) %>%
  ungroup() %>%
  filter(episode_id > 1) %>%
  transmute(
    !!!pib_data,                # keep original columns (from df0)
    s_raw = NA_integer_, s = NA_integer_,
    sign_change = NA_integer_,
    episode_id = episode_id,
    episode_index = 100,
    overlap_flag = 1L
  )

df_episodes_overlap <- df2 %>%
  mutate(overlap_flag = 0L) %>%
  bind_rows(overlap_rows) %>%
  arrange(fecha, episode_id, overlap_flag)  # overlap_flag=0 keeps the “end” before the reset row





