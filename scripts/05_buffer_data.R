# Stage 05 — Buffer-based block-group inputs
# Renthub asking-rent series, residential permits, and affordable-unit
# counts within an 800m buffer of each BG's population centroid.
# Collates everything (including the change/median variables from stage 04)
# into a single bg_data frame.
#
# Depends on: lvm_bg_popctr_geo from stage 02, pop_change + med_*_s +
#   nhood_vars*_s from stage 04.
# Produces: rent_buffer_wide, rent_buffer, bg_permits, bg_ah, bg_data.
# Writes: DHNA/data/Renthub_quarterly_rent.csv.

# rent from Renthub - change between Q3 2017 and Q3 2024 (wide; feeds rank_rents2)
# limiting to areas where there are at least 30 observations
rent_buffer_wide <- read_csv(here("data", "prepackaged", "renthub", "rent_buffer.csv")) %>%
  mutate(year_qu_text = paste0(R_quarter, "_", R_year)) %>%
  filter(year_qu_text == "Q3_2017" | year_qu_text == "Q3_2024") %>%
  filter(N > 30) %>%
  group_by(GISJOIN) %>%
  mutate(bg_n = n()) %>%
  filter(bg_n == 2) %>% # require both quarters present
  select(-bg_n) %>%
  pivot_wider(id_cols = c(GISJOIN, GEOID_bg, market_area),
              names_from = year_qu_text,
              values_from = c(rent_bg, rent_ma)) %>%
  rename(GISJOIN_proj = GISJOIN) %>%
  select(-market_area)

# rent from Renthub - quarterly change from 01/2019 to 09/2024 (long; written to disk for the Shiny app)
# limiting to areas where there are at least 30 observations
# and 20 quarters of data
rent_buffer <- read_csv(here("data", "prepackaged", "renthub", "rent_buffer.csv")) %>%
  mutate(year_qu_text = paste0(R_quarter, "_", R_year)) %>%
  filter(R_year > 2017) %>%
  filter(N > 30) %>%
  group_by(GISJOIN) %>%
  mutate(bg_n = n()) %>%
  filter(bg_n > 20) %>% # make sure there is enough observations in both years
  select(-bg_n)  %>%
  rename(GISJOIN_proj = GISJOIN) %>%
  select(-market_area) %>%
  write_csv(., here("DHNA", "data", "Renthub_quarterly_rent.csv"))

# number of permits and affordable units within 800 m of block
# Create 800m buffer around BG population center
lvm_bg_popctr_buffer <- lvm_bg_popctr_geo %>%
  st_transform(crs=4326) %>%
  select(GISJOIN_BG_PC) %>%
  rename(GISJOIN_proj = GISJOIN_BG_PC) %>%
  st_transform(crs = params$permit_buffer_crs) %>%
  st_buffer(dist = params$permit_buffer_ft)
# Load permit data from the latest Louisville Metro active-construction permits
# export (admin$permits_csv). Filter to residential subtypes (user choice for
# this refresh: any PERMIT_TYPE starting with "Residential") and project to the
# buffer CRS for the spatial join below.
build_permits <- read_csv(here("data", "administrative", "permits", admin$permits_csv)) %>%
  filter(str_starts(PERMIT_TYPE, "Residential"),
         !is.na(LATITUDE), !is.na(LONGITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = params$permit_buffer_crs)
# Summarize the number of permits
bg_permits <- st_join(lvm_bg_popctr_buffer, build_permits,
                      join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(GISJOIN_proj) %>%
  summarize(permits_N = n()) %>%
  ungroup()

# Goal #4 Phase 4.2 — permit-type split. Re-read the file unfiltered so we
# can pick up Wrecking Permits (excluded from build_permits's Residential*
# filter) for the demolition bucket. HVAC / Electrical / PoolSpa are dropped
# (service activity, not development pressure). The aggregated permits_N
# above is preserved for byte-identical classifier output during 4.2b.1.
build_permits_split <- read_csv(here("data", "administrative", "permits",
                                     admin$permits_csv)) %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>%
  mutate(permit_class = case_when(
    PERMIT_TYPE == "Residential New"                                ~ "new",
    PERMIT_TYPE %in% c("Residential Alteration",
                       "Residential Addition")                      ~ "renov",
    PERMIT_TYPE == "Wrecking Permit"                                ~ "demo",
    TRUE                                                            ~ NA_character_
  )) %>%
  filter(!is.na(permit_class)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = params$permit_buffer_crs)
bg_permits_split <- st_join(lvm_bg_popctr_buffer, build_permits_split,
                            join = st_intersects) %>%
  st_drop_geometry() %>%
  filter(!is.na(permit_class)) %>%
  group_by(GISJOIN_proj, permit_class) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(id_cols = GISJOIN_proj,
              names_from = permit_class,
              values_from = n,
              names_glue = "permits_{permit_class}_N",
              values_fill = 0)

# Goal #4 Phase 4.2 — MLS sales (GLAR) within the 800m buffer. The current
# export is capped at 50,000 rows and is heavily truncated for 2022-2023
# (~312 / 129 vs ~16k each year through 2021), so usable signal is mostly
# 2019-2021. Cities deploying for real should refresh this with full
# year coverage.
mls_sales <- read_csv(here("data", "administrative", "MLS", admin$mls_csv)) %>%
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>%
  rename(sale_price = `Sold Price`, year_sold = YearSold) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = params$permit_buffer_crs)
bg_mls <- st_join(lvm_bg_popctr_buffer, mls_sales, join = st_intersects) %>%
  st_drop_geometry() %>%
  filter(!is.na(year_sold)) %>%
  group_by(GISJOIN_proj, year_sold) %>%
  summarise(mls_n            = n(),
            mls_median_price = median(sale_price, na.rm = TRUE),
            .groups          = "drop") %>%
  pivot_wider(id_cols     = GISJOIN_proj,
              names_from  = year_sold,
              values_from = c(mls_n, mls_median_price),
              names_glue  = "{.value}_{year_sold}")

# Goal #4 Phase 4.2 — Foreclosure sales within the 800m buffer. Source
# addresses live in admin$foreclosures_csv; geocoding is done out of
# band by scripts/geocode_foreclosures.R and cached to data/processed/.
foreclosures_geo_path <- here("data", "processed", "foreclosures_geocoded.csv")
if (!file.exists(foreclosures_geo_path)) {
  stop("Foreclosures cache missing: ", foreclosures_geo_path,
       "\nRun `source(here::here(\"scripts\", \"geocode_foreclosures.R\"))` once to generate it.",
       call. = FALSE)
}
foreclosures <- read_csv(foreclosures_geo_path) %>%
  filter(!is.na(lat), !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = params$permit_buffer_crs)
bg_foreclosures <- st_join(lvm_bg_popctr_buffer, foreclosures,
                           join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(GISJOIN_proj) %>%
  summarise(foreclosure_n = sum(!is.na(sale_date)), .groups = "drop")
# Affordable units: the existing NHPD-based dataset plus the latest Louisville
# ARP-funded Housing Trust Fund projects (admin$htf_csv). ARP rows are treated
# as always active (EndDate_Ye = 9999) so they pass the EndDate cutoff filter
# applied after the spatial join.
affordable_nhpd <- st_read(here("data", "prepackaged", "affordable", "affordable_housing.shp")) %>%
  st_transform(crs = params$permit_buffer_crs) %>%
  transmute(Total_Unit, EndDate_Ye, source = "NHPD")
affordable_arp <- read_csv(here("data", "administrative", "housing trust fund", admin$htf_csv)) %>%
  filter(!is.na(X), !is.na(Y)) %>%
  st_as_sf(coords = c("X", "Y"), crs = 3857) %>%
  st_transform(crs = params$permit_buffer_crs) %>%
  transmute(Total_Unit = Number_of_Units, EndDate_Ye = 9999L, source = "ARP-0023")
affordable <- bind_rows(affordable_nhpd, affordable_arp)
# Summarize the number of affordable units
bg_ah <- st_join(lvm_bg_popctr_buffer, affordable,
                 join = st_intersects) %>%
  st_drop_geometry() %>%
  filter(EndDate_Ye > 2026) %>%
  group_by(GISJOIN_proj) %>%
  summarise(afforbable_units = sum(Total_Unit)) %>%
  ungroup()

# Collate BG data
bg_data <- pop_change %>%
  left_join(., med_rent10_s) %>%
  left_join(., med_rent20_s) %>%
  left_join(., med_hv10_s) %>%
  left_join(., med_hv20_s) %>%
  left_join(., med_hhinc10_s) %>%
  left_join(., med_hhinc20_s) %>%
  left_join(., nhood_vars10_s) %>%
  left_join(., nhood_vars20_s) %>%
  left_join(., rent_buffer_wide) %>%
  left_join(., bg_permits) %>%
  left_join(., bg_permits_split) %>%
  left_join(., bg_mls) %>%
  left_join(., bg_foreclosures) %>%
  left_join(., bg_ah) %>%
  # Phase 4.2b.1 — BGs absent from a count-aggregation table left-join in as
  # NA; convert to 0 (a BG with no MLS / foreclosure / split-permit hit
  # genuinely has zero, not missing data). Median-price stays NA when there
  # are no sales (no defined median).
  mutate(across(any_of(c("permits_new_N", "permits_renov_N", "permits_demo_N",
                         "foreclosure_n",
                         "mls_n_2019", "mls_n_2020", "mls_n_2021",
                         "mls_n_2022", "mls_n_2023")),
                ~ replace_na(., 0L))) %>%
  mutate(vacancy_Ch = (HU_vacant_20/HU_20)-
           (HU_vacant_10/HU_10),
         HH_ch = HH_20 - HH_10,
         HU_ch = HU_20 - HU_10,
         housing_tightness = (HU_ch-HH_ch)/HU_20,
         HH_p_ch = (HH_20 - HH_10)/HH_10,
         HU_p_ch = (HU_20 - HU_10)/HU_10,
         renter_p_10 = renters_10/HH_10,
         renter_p_20 = renters_20/HH_20,
         renter_p_ch = renter_p_20 - renter_p_10,
         #housing_cost_burden =(rent_burden30_20_ct+own_burden30_20_ct)/HH_20_ct,
         college_edu = (adult_college_above_20/adult_over25_20)-
           (adult_college_above_10/adult_over25_10),
         hi_inc_ch = (hi_inc_hh_20/HH_20)-(hi_inc_hh_10/HH_10),
         hi_inc_ch_pop = hi_inc_hh_20-hi_inc_hh_10,
         lo_inc_ch = (lo_inc_hh_20/HH_20)-(lo_inc_hh_10/HH_10),
         lo_inc_ch_pop = lo_inc_hh_20-lo_inc_hh_10,
         hhinc_ch = (median_hhinc_20-median_hhinc_10)/median_hhinc_10) %>%
  # Phase 4.2b.2 — derived rates for tenure-aware risk (Goal #4).
  # owners_20_bg = BG-level owner-occupants (the ct_data owner_20_ct lives
  # at tract level; BG-level renters comes from nhood_vars20_s).
  # mls_price_growth: 2019 -> 2021 (data is sparse for 2022-2023 in the
  # current 50k-row export; cities deploying for real should refresh MLS
  # with full year coverage and extend this to 2019 -> 2023).
  # foreclosure_rate: per 1000 BG-level owner-occupants.
  mutate(owners_20_bg = HH_20 - renters_20,
         mls_price_growth =
           if_else(!is.na(mls_median_price_2019) & !is.na(mls_median_price_2021)
                   & mls_median_price_2019 > 0,
                   (mls_median_price_2021 - mls_median_price_2019) / mls_median_price_2019,
                   NA_real_),
         foreclosure_rate =
           if_else(owners_20_bg > 0,
                   1000 * foreclosure_n / owners_20_bg,
                   NA_real_),
         # Phase 4.2b.3 — B25038-derived owner long-tenure rate. Share of
         # owner-occupied households whose householder moved in >= 10 years
         # ago (mid-window of the 2020-2024 ACS). Uses B25038's own
         # owner-occupied denominator (owners_b25038_20), which tracks
         # owner-occupants of the surveyed universe — close to but not
         # identical to HH_20 - renters_20 (which is derived from B25003).
         owner_longtenure_p =
           if_else(owners_b25038_20 > 0,
                   owner_longtenure_n_20 / owners_b25038_20,
                   NA_real_))

# Validation ---------------------------------------------------------------
validation_banner("Stage 05 — buffer data")
# bg_data is assembled by left-joining onto pop_change; the row count must not
# change (a stray many-to-many join would fan rows out).
check_rows_equal(bg_data, "bg_data", nrow(pop_change), "pop_change")
check_na_share(bg_data, "renter_p_ch", 0.8, "warn")
check_na_share(bg_data, "college_edu", 0.8, "warn")
check_na_share(bg_data, "hhinc_ch", 0.8, "warn")

# Phase 4.2b.1 — new admin data ingestion checkpoints.
# Sources: MLS (GLAR), foreclosure sales (geocoded), permit-type split.
check_min_rows(mls_sales, "mls_sales (point sf)", 30000, "error")
check_min_rows(foreclosures, "foreclosures (geocoded)", 300, "error")
check_not_all_na(bg_data, "mls_n_2019",           "error")
check_not_all_na(bg_data, "mls_n_2021",           "error")
check_not_all_na(bg_data, "mls_median_price_2019","error")
check_not_all_na(bg_data, "foreclosure_n",        "error")
check_not_all_na(bg_data, "permits_new_N",        "error")
check_not_all_na(bg_data, "permits_renov_N",      "error")
check_not_all_na(bg_data, "permits_demo_N",       "warn")  # rarer; not all BGs see demos
# Derived rates (Phase 4.2b.2). MLS growth NA-share is high — many BGs lack
# both 2019 and 2021 median prices; we tolerate up to 50% NA.
check_not_all_na(bg_data, "mls_price_growth", "error")
check_na_share(bg_data, "mls_price_growth", 0.5, "warn")
check_not_all_na(bg_data, "foreclosure_rate", "error")
check_range(bg_data, "foreclosure_rate", 0, 500, "warn")
# B25038-derived owner long-tenure rate (Phase 4.2b.3); requires the bg2020
# extract to include B25038. owner_longtenure_p is a share in [0, 1].
check_not_all_na(bg_data, "owner_longtenure_p", "error")
check_range(bg_data, "owner_longtenure_p", 0, 1, "warn")
