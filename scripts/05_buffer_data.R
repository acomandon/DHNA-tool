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
# Load permit data
build_permits <- st_read(here("data", "prepackaged", "permits", "res_permits2.shp"))
# Summarize the number of permits
bg_permits <- st_join(lvm_bg_popctr_buffer, build_permits,
                      join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(GISJOIN_proj) %>%
  summarize(permits_N = n()) %>%
  ungroup()
# Load affordable unit database and transform to state plane projection
affordable <- st_read(here("data", "prepackaged", "affordable", "affordable_housing.shp")) %>%
  select(NHPD_Prope, Property_N, Total_Unit, EndDate_Ye)
affordable <- st_transform(affordable, params$permit_buffer_crs)
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
  left_join(., bg_ah) %>%
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
         hhinc_ch = (median_hhinc_20-median_hhinc_10)/median_hhinc_10)

# Validation ---------------------------------------------------------------
validation_banner("Stage 05 — buffer data")
# bg_data is assembled by left-joining onto pop_change; the row count must not
# change (a stray many-to-many join would fan rows out).
check_rows_equal(bg_data, "bg_data", nrow(pop_change), "pop_change")
check_na_share(bg_data, "renter_p_ch", 0.8, "warn")
check_na_share(bg_data, "college_edu", 0.8, "warn")
check_na_share(bg_data, "hhinc_ch", 0.8, "warn")
