# Rental coverage audit V2 — Dewey sources (Renthub, Dwellsy TotalIQ).
#
# Read-only. Replaces V1. Opens whichever of data/dewey/renthub and
# data/dewey/dwellsy exist via arrow::open_dataset(), filters to KY, then
# refines to Jefferson County via spatial intersection with the NHGIS
# BG layer. Produces six tidy CSVs + a handful of PNGs under
# data/processed/audits/.
#
# Methodology informed by scripts/rent_impact_model.R (the existing Renthub
# prep): adopts DATE_POSTED bucketing, rent < 6000 outlier filter, post-
# 2014-01 cutoff. Departs in two ways:
#   1) reports BOTH distinct-property (rounded lat/lon) AND distinct-event
#      (lat/lon + rounded rent) counts. The latter mirrors the existing
#      uid_t hash; the former is the apples-to-apples cross-source metric.
#   2) does NOT buffer BGs — coverage is strict point-in-polygon.
#
# Outputs to data/processed/audits/:
#   - coverage_monthly_KY.csv          month x source x metrics, state level
#   - coverage_monthly_jefferson.csv   month x source x metrics, Jefferson only
#   - coverage_bg_jefferson.csv        BG   x source x reliability metrics
#   - coverage_overlap_jefferson.csv   month x set-comparison (2020-09+)
#   - coverage_fill_KY.csv             one row per source: field fill rates
#   - renthub_2018_gap_check.csv       Renthub-only pre-2020 timeseries with
#                                       rolling-median gap flag
#   - coverage_*.png                    timeseries + overlap plots
#
# Run from R:  source(here::here("scripts","exploratory","rental_coverage_audit.R"))

library(here)
library(arrow)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(readr)
library(scales)
library(sf)

source(here("R", "config.R"))
state_abbr <- locality$state_abbr
county_fips <- locality$county_fips

audit_dir <- here("data", "processed", "audits")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

# Pipeline-aligned constants (see scripts/rent_impact_model.R)
RENT_CEILING       <- 6000      # drop scraped rents at/above this as outliers
RENTHUB_DATE_FLOOR <- as.Date("2014-02-01")  # drop the partial first month
LATLON_PRECISION   <- 5         # decimal places for the property key (~1 m)

# Per-source column mapping. Renthub uses DATE_POSTED (when the listing was
# posted on the source), matching the existing pipeline; Dwellsy uses
# CREATION_TS (listing-creation timestamp; the only time column Dwellsy ships).
spec <- list(
  renthub = list(
    dir   = here("data", "dewey", "renthub"),
    ts    = "DATE_POSTED",
    state = "STATE",
    zip   = "ZIP",
    rent  = "RENT_PRICE",
    beds  = "BEDS",
    year  = "YEAR_BUILT",
    lat   = "LATITUDE",
    lon   = "LONGITUDE",
    date_floor = RENTHUB_DATE_FLOOR
  ),
  dwellsy = list(
    dir   = here("data", "dewey", "dwellsy"),
    ts    = "CREATION_TS",
    state = "ADDRESS_STATE",
    zip   = "ADDRESS_ZIP",
    rent  = "RENT_AMOUNT",
    beds  = "BEDROOMS",          # decimal128 — cast as char after collect
    year  = "YEAR_BUILT",        # string in actual parquet schema
    lat   = "LATITUDE",
    lon   = "LONGITUDE",
    date_floor = as.Date("2020-09-01")
  )
)

has_parquet <- function(dir) {
  dir.exists(dir) && length(list.files(dir, pattern = "\\.parquet$", recursive = TRUE)) > 0
}

# ---- Jefferson BG polygons (loaded once, shared across sources) ------------

bg_path <- here("data", "nhgis", "gis", "blockgroup", "bg2023", "KY_Jefferson_BG_2023.shp")
if (!file.exists(bg_path)) {
  stop("Jefferson BG shapefile not found at ", bg_path,
       " — re-run scripts/data_downloader.R or the GIS section of it.")
}
message("Loading Jefferson BG polygons...")
jefferson_bg <- st_read(bg_path, quiet = TRUE) |>
  st_transform(crs = 4326) |>
  select(GISJOIN)
message(sprintf("  %d BG polygons loaded", nrow(jefferson_bg)))

# ---- Per-source enrichment --------------------------------------------------

# Returns a tibble with one row per listing observation, with:
#   source, month, ts, zip, rent, rent_outlier, beds, year, lat, lon,
#   latlon_ok, prop_key, event_key, GISJOIN_bg (NA if outside Jefferson)
enrich_source <- function(name, s) {
  if (!has_parquet(s$dir)) {
    message(sprintf("  [%s] no parquet under %s — skipping", name, s$dir))
    return(NULL)
  }
  message(sprintf("  [%s] opening dataset and filtering to %s...", name, state_abbr))
  ky <- tryCatch({
    ds <- open_dataset(s$dir, format = "parquet")
    ds |>
      filter(.data[[s$state]] == state_abbr) |>
      select(
        ts   = all_of(s$ts),
        zip  = all_of(s$zip),
        rent = all_of(s$rent),
        beds = all_of(s$beds),
        year = all_of(s$year),
        lat  = all_of(s$lat),
        lon  = all_of(s$lon)
      ) |>
      collect()
  }, error = function(e) {
    message(sprintf("  [%s] SKIPPED — could not read parquet (in-progress download or corrupted file?):\n    %s",
                    name, conditionMessage(e)))
    NULL
  })
  if (is.null(ky)) return(NULL)
  message(sprintf("  [%s] %s %s rows", name, format(nrow(ky), big.mark = ","), state_abbr))

  ky <- ky |>
    mutate(
      ts           = as.POSIXct(ts),
      month        = floor_date(ts, "month"),
      rent         = as.numeric(rent),
      year         = as.character(year),   # Renthub int, Dwellsy string — normalize
      rent_outlier = !is.na(rent) & rent >= RENT_CEILING,
      latlon_ok    = !is.na(lat) & !is.na(lon) &
                       lat >  24  & lat <  50 &       # plausible US lat
                       lon < -65  & lon > -130,       # plausible US lon
      prop_key     = ifelse(latlon_ok,
                            sprintf(paste0("%.", LATLON_PRECISION, "f|%.", LATLON_PRECISION, "f"),
                                    lat, lon),
                            NA_character_),
      event_key    = ifelse(latlon_ok & !is.na(rent) & !rent_outlier,
                            sprintf(paste0("%.", LATLON_PRECISION, "f|%.", LATLON_PRECISION, "f|%d"),
                                    lat, lon, as.integer(round(rent))),
                            NA_character_),
      source       = name
    )

  # Apply source-specific date floor (Renthub: drop pre-Feb-2014; Dwellsy:
  # drop pre-2020-09 just in case any straggler rows exist).
  ky <- ky |> filter(is.na(month) | month >= s$date_floor)

  # BG assignment via spatial join on *distinct* points (huge speedup for
  # Renthub: 5-10M rows but only ~50-200k unique points).
  message(sprintf("  [%s] spatial-joining to Jefferson BGs (deduplicated points)...", name))
  unique_pts <- ky |>
    filter(latlon_ok) |>
    distinct(lat, lon)
  if (nrow(unique_pts) > 0) {
    unique_sf <- st_as_sf(unique_pts, coords = c("lon", "lat"), crs = 4326)
    joined <- st_join(unique_sf, jefferson_bg, join = st_intersects, left = TRUE)
    pt_to_bg <- unique_pts |>
      mutate(GISJOIN_bg = joined$GISJOIN)
    ky <- ky |> left_join(pt_to_bg, by = c("lat", "lon"))
  } else {
    ky$GISJOIN_bg <- NA_character_
  }

  message(sprintf("  [%s] Jefferson rows: %s of %s KY (%.1f%%)",
                  name,
                  format(sum(!is.na(ky$GISJOIN_bg)), big.mark = ","),
                  format(nrow(ky), big.mark = ","),
                  100 * mean(!is.na(ky$GISJOIN_bg))))
  ky
}

# ---- Metric builders --------------------------------------------------------

monthly_metrics <- function(d, geo_label) {
  d |>
    group_by(source, month) |>
    summarise(
      n_observations           = n(),
      n_distinct_properties    = n_distinct(prop_key[!is.na(prop_key)]),
      n_distinct_listing_events = n_distinct(event_key[!is.na(event_key)]),
      n_distinct_BGs           = n_distinct(GISJOIN_bg[!is.na(GISJOIN_bg)]),
      n_distinct_zip           = n_distinct(zip[!is.na(zip) & nchar(zip) > 0]),
      median_rent              = median(rent[!rent_outlier], na.rm = TRUE),
      n_rent_outliers_dropped  = sum(rent_outlier),
      .groups = "drop"
    ) |>
    mutate(geography = geo_label) |>
    relocate(geography, source, .before = month) |>
    arrange(source, month)
}

bg_summary <- function(d_jefferson) {
  total_months_per_source <- d_jefferson |>
    filter(!is.na(month)) |>
    distinct(source, month) |>
    count(source, name = "total_months")

  d_jefferson |>
    filter(!is.na(GISJOIN_bg), !is.na(month)) |>
    group_by(source, GISJOIN_bg) |>
    summarise(
      n_months_observed    = n_distinct(month),
      n_distinct_properties = n_distinct(prop_key[!is.na(prop_key)]),
      n_observations       = n(),
      median_rent          = median(rent[!rent_outlier], na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(total_months_per_source, by = "source") |>
    mutate(p_months_present = round(n_months_observed / total_months, 3)) |>
    arrange(source, desc(p_months_present))
}

overlap_metrics <- function(d_jefferson) {
  # Distinct-property set per (source, month). Cross-source comparison
  # only meaningful where both sources have data — i.e. 2020-09 onward.
  per_sm <- d_jefferson |>
    filter(!is.na(prop_key), !is.na(month), month >= as.Date("2020-09-01")) |>
    distinct(source, month, prop_key)

  wide <- per_sm |>
    mutate(present = 1L) |>
    pivot_wider(names_from = source, values_from = present, values_fill = 0L)

  if (!all(c("renthub", "dwellsy") %in% names(wide))) {
    message("  [overlap] both sources required; skipping (one is absent)")
    return(NULL)
  }

  wide |>
    group_by(month) |>
    summarise(
      n_renthub_only = sum(renthub == 1 & dwellsy == 0),
      n_dwellsy_only = sum(renthub == 0 & dwellsy == 1),
      n_in_both      = sum(renthub == 1 & dwellsy == 1),
      n_renthub_total = sum(renthub == 1),
      n_dwellsy_total = sum(dwellsy == 1),
      jaccard        = round(n_in_both / (n_renthub_total + n_dwellsy_total - n_in_both), 3),
      .groups = "drop"
    ) |>
    arrange(month)
}

# Renthub-only 2018 gap check. Rolling median of the PREVIOUS 6 months;
# a month is flagged if its n_observations is < 50% of that rolling median.
gap_check <- function(d_renthub) {
  if (is.null(d_renthub)) return(NULL)
  ts <- d_renthub |>
    filter(!is.na(month),
           month >= as.Date("2014-01-01"),
           month <  as.Date("2020-09-01")) |>
    group_by(month) |>
    summarise(n_observations = n(),
              n_distinct_properties = n_distinct(prop_key[!is.na(prop_key)]),
              .groups = "drop") |>
    arrange(month) |>
    mutate(
      rolling_med_prev_6 = vapply(
        seq_along(n_observations),
        function(i) {
          prev <- n_observations[max(1, i - 6):(i - 1)]
          prev <- prev[!is.na(prev)]
          if (length(prev) < 3) NA_real_ else median(prev)
        },
        numeric(1)
      ),
      gap_flag = !is.na(rolling_med_prev_6) & n_observations < 0.5 * rolling_med_prev_6
    )
  ts
}

fill_rates <- function(d, name) {
  tibble(
    source              = name,
    n_total_ky_rows     = nrow(d),
    n_jefferson_rows    = sum(!is.na(d$GISJOIN_bg)),
    fill_rent_nonzero   = mean(!is.na(d$rent) & d$rent > 0),
    fill_zip            = mean(!is.na(d$zip) & nchar(d$zip) > 0),
    fill_latlon         = mean(d$latlon_ok),
    fill_beds           = mean(!is.na(suppressWarnings(as.integer(as.character(d$beds))))),
    fill_year_built     = mean(!is.na(d$year) & nchar(as.character(d$year)) > 0),
    p_rent_outliers     = mean(d$rent_outlier)
  )
}

# ---- Run --------------------------------------------------------------------

message("Auditing Dewey rental sources (V2)...")
data_by_source <- list(
  renthub = enrich_source("renthub", spec$renthub),
  dwellsy = enrich_source("dwellsy", spec$dwellsy)
)
data_by_source <- data_by_source[!vapply(data_by_source, is.null, logical(1))]
if (length(data_by_source) == 0) {
  stop("No Dewey parquet found under data/dewey/{renthub,dwellsy}.")
}

all_ky <- bind_rows(data_by_source)
all_jefferson <- all_ky |> filter(!is.na(GISJOIN_bg))

# ---- Compute tables ---------------------------------------------------------

monthly_ky        <- monthly_metrics(all_ky,        "KY")
monthly_jefferson <- monthly_metrics(all_jefferson, "Jefferson")
bg_jefferson      <- bg_summary(all_jefferson)
overlap_jefferson <- overlap_metrics(all_jefferson)
gap_2018          <- gap_check(data_by_source$renthub)

fill_combined <- bind_rows(lapply(names(data_by_source),
                                  function(n) fill_rates(data_by_source[[n]], n)))

# ---- Write CSVs -------------------------------------------------------------

write_csv(monthly_ky,        file.path(audit_dir, "coverage_monthly_KY.csv"))
write_csv(monthly_jefferson, file.path(audit_dir, "coverage_monthly_jefferson.csv"))
write_csv(bg_jefferson,      file.path(audit_dir, "coverage_bg_jefferson.csv"))
write_csv(fill_combined,     file.path(audit_dir, "coverage_fill_KY.csv"))
if (!is.null(overlap_jefferson)) {
  write_csv(overlap_jefferson, file.path(audit_dir, "coverage_overlap_jefferson.csv"))
}
if (!is.null(gap_2018)) {
  write_csv(gap_2018, file.path(audit_dir, "renthub_2018_gap_check.csv"))
}

# ---- Plots ------------------------------------------------------------------

# Jefferson monthly observations (log scale) — primary view.
p1 <- ggplot(monthly_jefferson, aes(month, n_observations, color = source)) +
  geom_line(linewidth = 0.6) +
  scale_y_log10(labels = comma) +
  labs(title    = "Jefferson County — Dewey listing observations per month (log)",
       subtitle = "Renthub: per scrape. Dwellsy: per new listing creation. Outliers (rent >= $6000) excluded.",
       x = NULL, y = "observations (log)") +
  theme_minimal()
ggsave(file.path(audit_dir, "coverage_observations_jefferson.png"),
       p1, width = 9, height = 5, dpi = 110)

# Jefferson distinct-properties per month — apples-to-apples cross-source.
p2 <- ggplot(monthly_jefferson, aes(month, n_distinct_properties, color = source)) +
  geom_line(linewidth = 0.6) +
  scale_y_continuous(labels = comma) +
  labs(title = "Jefferson County — distinct properties per month",
       subtitle = "Property = rounded (lat, lon) to 5 decimals.",
       x = NULL, y = "distinct properties") +
  theme_minimal()
ggsave(file.path(audit_dir, "coverage_properties_jefferson.png"),
       p2, width = 9, height = 5, dpi = 110)

# Jefferson BG breadth — how many BGs are visible per month.
p3 <- ggplot(monthly_jefferson, aes(month, n_distinct_BGs, color = source)) +
  geom_line(linewidth = 0.6) +
  labs(title = "Jefferson County — distinct BGs covered per month",
       x = NULL, y = "distinct BGs") +
  theme_minimal()
ggsave(file.path(audit_dir, "coverage_bgs_jefferson.png"),
       p3, width = 9, height = 5, dpi = 110)

# Median rent.
p4 <- ggplot(monthly_jefferson, aes(month, median_rent, color = source)) +
  geom_line(linewidth = 0.6) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Jefferson County — median advertised rent per month",
       subtitle = "After dropping rent >= $6000.",
       x = NULL, y = "median rent") +
  theme_minimal()
ggsave(file.path(audit_dir, "coverage_median_rent_jefferson.png"),
       p4, width = 9, height = 5, dpi = 110)

# Cross-source overlap stacked area (if both sources present).
if (!is.null(overlap_jefferson)) {
  ov_long <- overlap_jefferson |>
    select(month, n_renthub_only, n_in_both, n_dwellsy_only) |>
    pivot_longer(-month, names_to = "set", values_to = "n") |>
    mutate(set = factor(set, levels = c("n_renthub_only", "n_in_both", "n_dwellsy_only")))
  p5 <- ggplot(ov_long, aes(month, n, fill = set)) +
    geom_area() +
    labs(title    = "Jefferson County — property-set overlap (2020-09 onward)",
         subtitle = "Distinct properties per month, partitioned by which source(s) saw them.",
         x = NULL, y = "distinct properties", fill = NULL) +
    scale_y_continuous(labels = comma) +
    theme_minimal()
  ggsave(file.path(audit_dir, "coverage_overlap_jefferson.png"),
         p5, width = 9, height = 5, dpi = 110)
}

# 2018 gap zoom (Renthub only).
if (!is.null(gap_2018) && nrow(gap_2018) > 0) {
  p6 <- ggplot(gap_2018, aes(month, n_observations)) +
    geom_line(linewidth = 0.6, color = "#2c7fb8") +
    geom_line(aes(y = rolling_med_prev_6), linewidth = 0.5,
              color = "#888", linetype = "dashed") +
    geom_point(data = gap_2018 |> filter(gap_flag),
               color = "#d7191c", size = 2) +
    scale_y_continuous(labels = comma) +
    labs(title = "Renthub Jefferson — 2014-01 to 2020-08 monthly observations",
         subtitle = "Solid: monthly count. Dashed: rolling median of prev 6 months. Red dots: months below 50% of rolling median.",
         x = NULL, y = "observations") +
    theme_minimal()
  ggsave(file.path(audit_dir, "renthub_2018_gap_check.png"),
         p6, width = 9, height = 5, dpi = 110)
}

# ---- Console summary --------------------------------------------------------

message("\n=== fill rates ===")
print(fill_combined)

if (!is.null(gap_2018) && nrow(gap_2018) > 0) {
  flagged <- gap_2018 |> filter(gap_flag)
  message(sprintf("\n=== 2018 gap check: %d months flagged ===", nrow(flagged)))
  if (nrow(flagged) > 0) print(flagged |> select(month, n_observations, rolling_med_prev_6))
}

if (!is.null(overlap_jefferson) && nrow(overlap_jefferson) > 0) {
  message("\n=== overlap summary (Jefferson, 2020-09+) ===")
  print(overlap_jefferson |>
          summarise(
            months         = n(),
            mean_renthub   = round(mean(n_renthub_total)),
            mean_dwellsy   = round(mean(n_dwellsy_total)),
            mean_in_both   = round(mean(n_in_both)),
            mean_jaccard   = round(mean(jaccard, na.rm = TRUE), 3)
          ))
}

message(sprintf("\nAudit V2 done. CSVs + PNGs in %s", audit_dir))
