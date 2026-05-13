# Global Earthquake Explorer

An exploratory data analysis of global earthquake data (1965–2023), 
completed as part of a university EDA course.

## Contents
- `earthquake_shiny_app.R` — Interactive R Shiny app to explore earthquake distributions, temporal trends, and spatial patterns
- `earthquake_eda_report.pdf` — Full EDA report covering data wrangling, feature distributions, temporal and spatial analysis
- `earthquake_app_documentation.pdf` — User guide and documentation for the Shiny app

## Data
The app uses two datasets that must be placed in a `data/` subfolder:
- `Earthquakes 1965 - 2016.csv`
- `query.csv`

## Requirements
Install the required R packages with:
install.packages(c("shiny", "tidyverse", "lubridate", "maps", "ggplot2", "DT"))
