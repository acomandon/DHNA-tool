# Project configuration for the DHNA tool.
# Sourced by data_downloader.R, data_prep.R, and DHNA/app.R.
# Edit a block to retarget locality, refresh data vintages, or change pipeline params.

# Locality ------------------------------------------------------------------
# Edit to retarget the tool to a different metro area.
locality <- list(
  state_fips         = "21",
  state_name         = "Kentucky",
  state_abbr         = "KY",
  state_nhgis_extent = 210,
  county_fips        = "111",
  county_name        = "Jefferson County",
  pumas              = c(1701, 1702, 1703, 1704, 1705, 1706),
  default_map        = list(lat = 38.252, lng = -85.755, zoom = 12)
)

# Data vintages -------------------------------------------------------------
# Edit during a data refresh (Goal #2).
vintages <- list(
  acs_5yr_recent   = "2020_2024_ACS5a",
  acs_5yr_recent_b = "2020_2024_ACS5b",
  acs_5yr_prior    = "2009_2013_ACS5a",
  decennial_2000   = "2000_SF1b",
  decennial_1990   = "1990_STF1",
  pums_samples     = c("us2022a", "us2023a", "us2024a"),
  bg_shapefile     = "210_blck_grp_2024_tl2024",
  tract_shapefile  = "us_tract_2024_tl2024",
  bg_popcenter     = "us_blck_grp_cenpop_2020_cenpop2020"
)

# Pipeline parameters -------------------------------------------------------
params <- list(
  cpi_prior_to_recent = 1.3086,
  local_area_buffer_m = 800,
  permit_buffer_ft    = 2640,
  permit_buffer_crs   = 2246
)

# Administrative data feeds -------------------------------------------------
# Louisville Metro administrative exports (permits, Housing Trust Fund) that
# roll forward with each refresh. Drop the new export into the matching
# subfolder under data/administrative/ and bump the filename here.
admin <- list(
  permits_csv = "active_construction_permits_5798371747874481478.csv",
  htf_csv     = "Louisville_Metro_KY_-__ARP-0023_Louisville_Affordable_Housing_Trust_Fund.csv"
)

# Not yet absorbed into config (deferred):
#   - HUD_FMI table (data_prep.R) — will move to data/prepackaged/hud_fmi.csv
#   - Risk-classification thresholds (data_prep.R) — Goal #4 will overhaul
#   - UI AMI dollar strings (DHNA/app.R) — derive from HUD_FMI once it is data
