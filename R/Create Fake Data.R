# Date: 07/27/2026

# Objective: Create Dataset File

# Project: Brazos Nexus
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Packages ####
install.packages("pacman")
pacman::p_load(DT, tidyverse, tigris, sf, pacman, lubridate, leaflet, ggplot2, plotly, bslib)
library(tidyverse)
library(tigris)
library(sf)
library(lubridate)
library(leaflet)
library(plotly)
library(bslib)

# Brazos County Boundary ####
brazos_county <- tigris::counties(
  state = "TX",
  year = 2025,
  cb = TRUE
) %>%
  filter(NAME == "Brazos") %>%
  st_make_valid() %>%
  st_transform(4326)

# Zip Codes
brazos_zips <- st_read(
  "https://services1.arcgis.com/x5wCko8UnSi4h0CB/ArcGIS/rest/services/Brazos_County_City_Limits_and_Zip_Codes_WFL1/FeatureServer/3/query?where=1%3D1&outFields=*&f=geojson"
)

glimpse(brazos_zips)
names(brazos_zips)

plot(st_geometry(brazos_zips))

# Create Dataset ####
set.seed(0905)
fake_cases <- tibble(
  case_id = 1:1000,
  disease = sample(
    c("HIV", "Syphilis", "Gonorrhea", "Chlamydia"),
    1000,
    replace = TRUE
  ),
  report_date = sample(
    seq.Date(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day"),
    1000,
    replace = TRUE
  ),
  age = sample(0:95, 1000, replace = TRUE),
  sex = sample(c("Female", "Male", "Unknown"), 1000, replace = TRUE),
  race = sample(c("White", "African American", "Asian"), 1000, replace = TRUE),
  GEOID = sample(brazos_zips$Zip_Code, 1000, replace = TRUE)
)

# Write Re-Useable Permanent Dataset
write_csv(fake_cases, "data/fake_cases.csv")
