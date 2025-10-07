#VEN growth scenarios and simulator

#Load Packages
library(shiny)
library(readxl)
library(tidyverse)
library(lubridate)
library(zoo)

#Load GDP data
pib_data <- read_excel("Milagros_economicos.xlsx", sheet = "PIB Historico")
view(pib_data)

#Analyse data structure
summary(pib_data)
str(pib_data)

#Rewrite Fecha column, limit initial size to 2024
pib_data <- pib_data %>%
  filter(Año <= 2024) %>%
  mutate(Fecha = make_date(year = Año, day = 12, month = 31)) %>%
  mutate(Fecha = as.Date(Fecha))

#Analyse data structure
summary(pib_data)
str(pib_data)

