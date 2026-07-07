# Build/refresh the Renthub quarterly rent buffer (data/prepackaged/renthub/rent_buffer.csv).
#
# Prep step (run outside 00_run_all.R): reads the raw Renthub parquet tree
# pulled by scripts/data_downloader_dewey.py into data/dewey/renthub/,
# reproduces the original rent_buffer methodology (scripts/rent_impact_model.R
# lines 178-318), and extends coverage through the latest available quarter.
#
# Methodology (must match the original so the series stays comparable):
#   1. KY listings only (arrow filter pushdown on STATE).
#   2. Dedup to one observation per unit per month:
#        uid_t = address|zip|beds|baths|sqft|rent_price|month.
#   3. Drop rent >= RENT_CEILING (6000) and pre-2014-02 partial month.
#   4. Keep listings physically inside Jefferson County, then smooth each
#      listing into every BG whose polygon, BUFFERED BY 1 MILE (5280 ft),
#      contains it. This is why ~all 608 BGs get a value each quarter.
#   5. rent_ma = median rent per market-area x quarter; rent_bg = median rent
#      per BG x market-area x quarter; N = listing count.
#   6. year_qu = dense factor index over quarters present (chronological).
#
# Output schema (unchanged): GEOID_bg, market_area, year_qu, R_quarter,
#   R_year, rent_bg, rent_ma, N, GISJOIN.
#
# Run:  "C:/Program Files/R/R-4.4.1/bin/Rscript.exe" scripts/build_rent_buffer.R
#   (uses renv library; needs arrow, sf, tidyverse, lubridate)

suppressMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(sf)
  library(readr)
  library(stringr)
})

source(here("R", "config.R"))

# --- parameters ------------------------------------------------------------
RENT_CEILING   <- 6000
DATE_FLOOR     <- as.Date("2014-02-01")   # drop partial first month
BUFFER_FT      <- 5280                     # 1-mile BG polygon buffer
WORK_CRS       <- 2246                     # KY State Plane North (US ft)
# KY listings extracted from the raw parquet by scripts/extract_renthub_ky.py
# (pyarrow in the `dewey` conda env — R's arrow is not reliably installed).
listings_csv   <- here("data", "processed", "renthub_ky_listings.csv")
out_path       <- here("data", "prepackaged", "renthub", "rent_buffer.csv")
existing_path  <- out_path                 # validate against the current file

if (!file.exists(listings_csv)) {
  stop("KY listings extract not found at ", listings_csv,
       "\n  Run first (dewey env):  python scripts/extract_renthub_ky.py")
}

# --- 1. read KY listings extract ------------------------------------------
message("Reading KY listings extract ...")
ky <- read_csv(listings_csv, show_col_types = FALSE,
               col_types = cols(
                 DATE_POSTED = col_character(), ADDRESS = col_character(),
                 ZIP = col_character(), BEDS = col_double(),
                 BATHS = col_double(), SQFT = col_double(),
                 RENT_PRICE = col_double(), LATITUDE = col_double(),
                 LONGITUDE = col_double()))
message(sprintf("  %s KY rows", format(nrow(ky), big.mark = ",")))

# --- 2. clean + dedup ------------------------------------------------------
ky <- ky %>%
  mutate(
    date_posted = as.Date(substr(DATE_POSTED, 1, 10)),  # "YYYY-MM-DD ..."
    rent_price  = as.numeric(RENT_PRICE),
    lat         = as.numeric(LATITUDE),
    lon         = as.numeric(LONGITUDE),
    date_f      = floor_date(date_posted, "month")
  ) %>%
  filter(!is.na(lat), !is.na(lon), !is.na(rent_price),
         rent_price < RENT_CEILING,
         date_f >= DATE_FLOOR) %>%
  mutate(uid_t = paste(ADDRESS, ZIP, BEDS, BATHS, SQFT, rent_price, date_f,
                       sep = "|")) %>%
  distinct(uid_t, .keep_all = TRUE)
message(sprintf("  %s rows after dedup + rent/date filters",
                format(nrow(ky), big.mark = ",")))

# --- 3. geographies --------------------------------------------------------
message("Loading BG + market-area geographies ...")
bg <- st_read(here("data", "nhgis", "gis", "blockgroup", "bg2023",
                   "KY_Jefferson_BG_2023.shp"), quiet = TRUE) %>%
  st_transform(WORK_CRS) %>%
  select(GISJOIN, GEOID)

ma <- st_read(here("data", "prepackaged", "Market_areas",
                   "Comp_Plan_Market_Areas.shp"), quiet = TRUE) %>%
  st_transform(WORK_CRS) %>%
  select(OBJECTID, Name) %>%
  rename(market_area = Name)

# Assign each BG to exactly ONE market area, via centroid containment. A
# polygon st_intersects join would tag edge-straddling BGs with 2+ market
# areas, which later duplicates them in stage 05's per-BG pivot (bg_data
# row inflation). The original series is 1:1 BG->market_area; match that.
bg_ma_key <- st_centroid(bg) %>%
  st_join(ma, join = st_within) %>%
  st_drop_geometry() %>%
  distinct(GISJOIN, .keep_all = TRUE) %>%   # safety net: one MA per BG
  select(GISJOIN, market_area)

# BG x market-area, buffered 1 mile (smoothing). Drop BGs with no market area.
bg_ma <- bg %>%
  left_join(bg_ma_key, by = "GISJOIN") %>%
  drop_na(market_area) %>%
  st_buffer(dist = BUFFER_FT)

# County boundary (unbuffered union) for the "physically in Jefferson" filter.
county <- st_union(bg)

# GEOID -> GISJOIN crosswalk (drop geometry).
bg_key <- bg %>% st_drop_geometry() %>% distinct(GEOID, GISJOIN)

# --- 4. spatial assignment -------------------------------------------------
message("Spatial join (dedup points -> buffered BGs) ...")
pts <- ky %>% distinct(lat, lon)
pts_sf <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(WORK_CRS)

# county filter on distinct points
in_cty <- st_intersects(pts_sf, county, sparse = FALSE)[, 1]
pts_cty <- pts_sf[in_cty, ]
pts_cty_key <- pts[in_cty, ]

# buffered-BG join (a point maps to every BG within 1 mile -> many-to-many)
joined <- st_join(pts_cty, bg_ma, join = st_intersects, left = FALSE)
pt_bg <- bind_cols(
  pts_cty_key[rep(seq_len(nrow(pts_cty_key)),
                  times = lengths(st_intersects(pts_cty, bg_ma))), ],
  joined %>% st_drop_geometry() %>% select(GEOID, market_area)
)
message(sprintf("  %s point->BG assignments over %s Jefferson points",
                format(nrow(pt_bg), big.mark = ","),
                format(nrow(pts_cty_key), big.mark = ",")))

listings <- ky %>%
  inner_join(pt_bg, by = c("lat", "lon"), relationship = "many-to-many") %>%
  rename(GEOID_bg = GEOID)

# --- 5. quarterly aggregation ---------------------------------------------
message("Aggregating to BG x market-area x quarter ...")
rent_buffer <- listings %>%
  mutate(qlab = paste(quarters(date_posted), year(date_posted))) %>%
  group_by(market_area, qlab) %>%
  mutate(rent_ma = median(rent_price)) %>%
  ungroup() %>%
  group_by(GEOID_bg, market_area, qlab) %>%
  summarise(rent_bg = median(rent_price),
            rent_ma = mean(rent_ma),
            N = n(), .groups = "drop") %>%
  separate(qlab, c("R_quarter", "R_year"), sep = " ", remove = FALSE) %>%
  arrange(R_year, R_quarter) %>%
  mutate(year_qu = as.numeric(factor(paste0(R_year, R_quarter)))) %>%
  left_join(bg_key, by = c("GEOID_bg" = "GEOID")) %>%
  mutate(R_year = as.integer(R_year)) %>%
  select(GEOID_bg, market_area, year_qu, R_quarter, R_year,
         rent_bg, rent_ma, N, GISJOIN)

rng <- range(rent_buffer$R_year)
message(sprintf("  built %s BG-quarter rows, %d BGs, years %d-%d",
                format(nrow(rent_buffer), big.mark = ","),
                n_distinct(rent_buffer$GEOID_bg), rng[1], rng[2]))

# --- 6. validate against existing on the overlap window -------------------
if (file.exists(existing_path)) {
  old <- read_csv(existing_path, show_col_types = FALSE,
                  col_types = cols(GEOID_bg = col_character(),
                                   GISJOIN = col_character())) %>%
    select(GEOID_bg, R_year, R_quarter, rent_bg_old = rent_bg, N_old = N)
  cmp <- rent_buffer %>%
    inner_join(old, by = c("GEOID_bg", "R_year", "R_quarter"))
  if (nrow(cmp) > 0) {
    rc <- cor(cmp$rent_bg, cmp$rent_bg_old, use = "complete.obs")
    nc <- cor(cmp$N, cmp$N_old, use = "complete.obs")
    med_abs <- median(abs(cmp$rent_bg - cmp$rent_bg_old), na.rm = TRUE)
    message("\n=== VALIDATION vs existing rent_buffer.csv (overlap quarters) ===")
    message(sprintf("  matched BG-quarters : %s", format(nrow(cmp), big.mark = ",")))
    message(sprintf("  rent_bg correlation : %.4f", rc))
    message(sprintf("  N correlation       : %.4f", nc))
    message(sprintf("  median |rent_bg diff|: $%.0f", med_abs))
  } else {
    message("No overlapping BG-quarters found for validation.")
  }
}

# --- 7. write (only after review) -----------------------------------------
# NOTE: writing is gated behind the WRITE env flag so a first run is
# validate-only. Re-run with WRITE=1 to overwrite the prepackaged CSV.
if (identical(Sys.getenv("WRITE"), "1")) {
  write_csv(rent_buffer, out_path)
  message(sprintf("\nWROTE %s", out_path))
} else {
  dbg_dir <- here("data", "processed", "audits")
  dir.create(dbg_dir, recursive = TRUE, showWarnings = FALSE)
  dbg_path <- file.path(dbg_dir, "rent_buffer_rebuilt.csv")
  write_csv(rent_buffer, dbg_path)
  message(sprintf("\nDRY RUN — canonical file not touched. Rebuilt series at:\n  %s",
                  dbg_path))
  message(sprintf("  Set WRITE=1 to overwrite %s", out_path))
}
