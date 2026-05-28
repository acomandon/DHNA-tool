# scripts/fetch_b25038.R
#
# Incremental NHGIS pull: just table B25038 (Tenure by Year Householder
# Moved Into Unit) at block-group level for the configured locality. Use
# this when the main bg2020 extract was pulled before B25038 was added to
# data_downloader.R's bg2020_spec — avoids re-pulling the other six BG2020
# tables.
#
# Output: data/nhgis/blockgroup/bg2020_supp/  (zip read alongside the main
# bg2020 extract in scripts/01_geo_standardize.R).
#
# Run once:  source(here::here("scripts", "fetch_b25038.R"))
# Requires an IPUMS API key (see ipumsr API vignette).

library(ipumsr)
library(tidyverse)
library(here)
source(here("R", "config.R"))

dl_path <- here("data", "nhgis", "blockgroup", "bg2020_supp")
if (dir.exists(dl_path)) unlink(dl_path, recursive = TRUE, force = TRUE)
dir.create(dl_path, recursive = TRUE)

b25038_spec <- ds_spec(vintages$acs_5yr_recent,
                       data_tables = "B25038",
                       geog_levels = c("blck_grp"))
define_extract_nhgis(
  description = paste0("BG2020 B25038 supplementary fetch (",
                       locality$state_name, ")"),
  datasets = b25038_spec,
  geographic_extents = locality$state_nhgis_extent) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = dl_path)

cat("B25038 downloaded to", dl_path, "\n")
cat("Re-run scripts/00_run_all.R to pick it up.\n")
