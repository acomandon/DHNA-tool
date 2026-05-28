# scripts/geocode_foreclosures.R
#
# One-time helper: geocode foreclosure-sale addresses via the Census
# geocoder and cache the result to data/processed/. Stage 05 reads only
# the cached output, so this script does NOT run on every pipeline
# refresh — only when admin$foreclosures_csv changes.
#
# Run:  source(here::here("scripts", "geocode_foreclosures.R"))
# Output: data/processed/foreclosures_geocoded.csv

library(tidyverse)
library(tidygeocoder)
library(here)
source(here("R", "config.R"))

src <- here("data", "administrative", "foreclosures", admin$foreclosures_csv)
out <- here("data", "processed", "foreclosures_geocoded.csv")

cat("Reading", src, "\n")
foreclosures <- read_csv(src, show_col_types = FALSE) %>%
  filter(!is.na(Street), !is.na(ZIP)) %>%
  mutate(singleline = paste0(Street, ", Louisville, ",
                             locality$state_abbr, ", ", ZIP))

cat(sprintf("Geocoding %d addresses via Census geocoder...\n",
            nrow(foreclosures)))
foreclosures_geo <- foreclosures %>%
  geocode(address = singleline, method = "census")

n_hit <- sum(!is.na(foreclosures_geo$lat))
cat(sprintf("Geocoded %d of %d (%.1f%% hit rate).\n",
            n_hit, nrow(foreclosures_geo),
            100 * n_hit / nrow(foreclosures_geo)))

dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
foreclosures_geo %>%
  transmute(sale_date = `Sale Date`,
            address   = singleline,
            lat,
            long) %>%
  write_csv(out)

cat("Wrote", out, "\n")
