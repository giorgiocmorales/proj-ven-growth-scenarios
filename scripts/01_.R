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
print(pib_data)

#Rewrite Fecha column, limit initial size to 2025
pib_data <- pib_data %>%
  filter(Año <= 2025) %>%
  mutate(Fecha = as.Date(Fecha)) %>%
  janitor::clean_names() %>%
  arrange(fecha) %>%
  mutate(crecimiento = as.numeric(crecimiento))
  
#Analyse data structure
summary(pib_data)
str(pib_data)

# Growth/Contraction episodes
pib_episodes <- pib_data %>%
  mutate(
    signo = case_when(
      is.na(crecimiento) ~ NA_integer_,
      crecimiento >= 0 ~ 1L,
      crecimiento < 0 ~ -1L,
      TRUE ~ 0L),
    signo = zoo::na.locf(signo, na.rm = FALSE),
    signo = zoo::na.locf(signo, fromLast = TRUE, na.rm = FALSE),
    episode_id = 1L + cumsum(if_else(is.na(lag(signo)), 0L, as.integer(signo != lag(signo))))) %>%
  group_by(episode_id) %>%
  mutate(episode_index = 100 * cumprod(1 + replace_na(crecimiento, 0))) %>%
  ungroup() %>%
  mutate(overlap_flag = 0L)

