# Source Notes

This folder stores manual source inputs for the project.

Current expected inputs:

- `milagros_economicos.xlsx`
- `venezuela_presidential_periods_fixed.xlsx`: presidential chronology used to assign annual observations by year-end officeholder.
- `venezuelan_civil_wars_conflict_dates.xlsx`: civil-conflict chronology used to place selected historical background intervals.
- `gdp-vs-happiness.csv` and `gdp-vs-happiness.metadata.json`: OWID life-satisfaction observations and definitions cached by `scripts/04_download_owid_development_data.R`. Scores use the Cantril ladder (0–10) and average the three survey years ending in the reported year; GDP uses constant 2021 international dollars.
- Any future source notes or update logs tied to the master workbook.

The analytical pipeline should read raw inputs from this folder and write derived outputs to `data/interim/` and `data/final/`.
