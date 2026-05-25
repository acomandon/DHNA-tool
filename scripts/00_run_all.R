# Master orchestrator for the DHNA data pipeline.
#
# Run end-to-end with:
#   source(here::here("scripts", "00_run_all.R"))
#
# On a first-time machine, also uncomment the data_downloader source line
# below to fetch the IPUMS extracts (requires an IPUMS API key). After
# data are downloaded the line should be re-commented so subsequent runs
# don't resubmit extracts.

# Packages ------------------------------------------------------------------
library(ipumsr)
library(data.table)
library(tidyverse)
library(tidycensus)
library(tidygeocoder)
library(purrr)
library(here)
library(sf)
library(mapproj)
library(leaflet)
library(survey)
library(spatstat)
library(ggalluvial)
library(ggrepel)
library(forcats)

# Config and shared helpers -------------------------------------------------
source(here("R", "config.R"))
source(here("R", "bin_interpolation.R"))
source(here("R", "risk_classifier.R"))
source(here("R", "validate.R"))

# Optional: first-time data download (uncomment, then re-comment after run)
# source(here("scripts", "data_downloader.R"))

# Pipeline stages -----------------------------------------------------------
source(here("scripts", "01_geo_standardize.R"))
source(here("scripts", "02_local_area.R"))
source(here("scripts", "03_pums.R"))
source(here("scripts", "04_bg_variables.R"))
source(here("scripts", "05_buffer_data.R"))
source(here("scripts", "06_tract_data.R"))
source(here("scripts", "07_risk.R"))
