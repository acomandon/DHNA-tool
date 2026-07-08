# Incremental NHGIS fetch — 2009-2013 ACS B25118 (Tenure by Household Income)
# at TRACT level, for the A2 low-income-renter-share-change signal.
#
# B25118 is not available at block group for 2009-2013 (dataset
# 2009_2013_ACS5b, nhgis_code U26, tract-minimum), so the change measure is
# tract-based. The recent vintage (2020-2024, AVF6E cells) is already pulled
# in ct2020. This grabs only the prior vintage.
#
# Run (R 4.5, key from Documents/.Renviron, renv .Rprofile skipped):
#   R_ENVIRON_USER="C:/Users/Andre/Documents/.Renviron" \
#     "C:/Program Files/R/R-4.5.3/bin/Rscript.exe" --no-init-file \
#     scripts/fetch_b25118_prior.R

suppressMessages({ library(here); library(ipumsr) })
source(here("R", "config.R"))

dl_path <- here("data", "nhgis", "tract", "ct2010b")
if (dir.exists(dl_path)) unlink(dl_path, recursive = TRUE, force = TRUE)
dir.create(dl_path, recursive = TRUE)

b25118_spec <- ds_spec("2009_2013_ACS5b",
                       data_tables = "B25118",
                       geog_levels = "tract")

define_extract_nhgis(
  description = "2009-2013 B25118 tenure-by-income (tract) for KY — A2 prior",
  datasets = b25118_spec,
  geographic_extents = locality$state_nhgis_extent) |>
  submit_extract() |>
  wait_for_extract() |>
  download_extract(download_dir = dl_path)

message("Downloaded to ", dl_path)
print(list.files(dl_path))
