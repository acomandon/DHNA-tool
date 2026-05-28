# Stage 06 — Tract-level data and ranks
# HUD AFFH tract-level housing burden, NHGIS tract ACS variables, FMI
# renter counts; collates into ct_data, joins BG+CT, and computes the
# percentile-rank variables consumed by classify_risk().
#
# Depends on: bg2020, local_area from earlier stages; bg_data from stage 05.
# Produces: HUD_FMI (overwritten as midpoints for tracts — see note),
#   hud_housing, hud_tract, ct_data_2020, ct2020, rent_income20_ct,
#   rent_fmi20_ct, local_area_ct, ct_data, BGxCT, bg_ct_data.
#
# Note: HUD_FMI was first defined in stage 03 as the multi-year PUMS table.
# This stage redefines it as a single-row midpoint table used by the
# rent_fmi20_ct calculations. Both definitions are preserved verbatim; a
# future cleanup should split the names.

# HUD HOME income limits, midpoint of household sizes 3 and 4, for the
# Louisville/Jefferson County, KY-IN HUD Metro FMR Area. FY2024 limits, to match
# the 2020-2024 ACS tract income (reported in 2024 inflation-adjusted dollars).
# 30/50/60/80 are published HUD values; 70% is computed as 1.4 x the 50% (VLI)
# limit per size, as HUD publishes no 70% limit (matches the prior method).
# Refresh: update to the recent ACS's dollar-year HOME limits when vintages change.
HUD_FMI <- data.frame(MFI_30 = (26050+28900)/2,
                      MFI_50 = (43400+48200)/2,
                      MFI_60 = (52080+57840)/2,
                      MFI_70 = (60760+67480)/2,
                      MFI_80 = (69400+77100)/2)

# HUD AFFH-T data (release identified by hud_affh in R/config.R).
# Unzip the data first.
HUDzip <- here("data", "prepackaged", "hud", paste0(hud_affh$folder, ".zip"))
outDir <- here("data", "prepackaged", "hud")
unzip(HUDzip, exdir = outDir)

# total number of subsidized units
hud_housing <- read_csv(here("data", "prepackaged", "hud", hud_affh$folder,
                             hud_affh$housing_csv)) %>%
  filter(state_name == locality$state_name,
         county_name == locality$county_name,
         program_label == "Summary") %>%
  select(geoid, total_units)
# Number of households with at least one housing problem and
# number of households
hud_tract <- read_csv(here("data", "prepackaged", "hud", hud_affh$folder,
                           hud_affh$tract_csv)) %>%
  filter(state_name == locality$state_name,
         county_name == locality$county_name) %>%
  select(geoid, hh_tot_1_m_hus_pb, hh_tot_husholds)

# Goal #4 Phase 4.2 — HMDA mortgage applications + denials by tract.
# The "2022_calc" sheet is in 2020-vintage tract IDs (matches our pipeline).
# Pooling 2020-2021 from "2020-2021_calc" would require a tract crosswalk
# (deferred — those rows use 2010-vintage tract IDs).
hmda_tract <- readxl::read_excel(
  here("data", "administrative", "HMDA", admin$hmda_xlsx),
  sheet = admin$hmda_sheet) %>%
  transmute(GEOID_CT     = as.character(GEOID),
            hmda_apps    = MortgageApp_2022,
            hmda_denials = Denials_2022)

# Goal #4 Phase 4.2 — Optional windshield-survey slot (Cyclomedia tract-
# aggregated property conditions for Louisville; NULL for other cities).
# We carry numerator + denominator and compute the rate post-summarise so
# the local-area aggregation is properly weighted by housing units.
if (!is.null(optional_data$windshield_survey)) {
  ws_cfg <- optional_data$windshield_survey
  cyclomedia_tract <- read_csv(
    here("data", "administrative", "Property Conditions Survey", ws_cfg$csv)) %>%
    transmute(GEOID_CT          = as.character(.data[[ws_cfg$geoid]]),
              cyclomedia_prob_n = .data[[ws_cfg$num_col]],
              cyclomedia_hu     = .data[[ws_cfg$denom_col]])
} else {
  cyclomedia_tract <- tibble(GEOID_CT          = character(0),
                             cyclomedia_prob_n = numeric(0),
                             cyclomedia_hu     = numeric(0))
}

nhgis_2020_ct <- list.files(here("data", "nhgis", "tract", "ct2020"),
                            full.names = TRUE)

ct_data_2020_A <- read_nhgis(nhgis_2020_ct[1])
ct_data_2020_B <- read_nhgis(nhgis_2020_ct[2])
ct_data_2020 <- left_join(ct_data_2020_A, ct_data_2020_B)

# housing cost burden
ct2020 <- ct_data_2020 %>%
  filter(STATE == locality$state_name,
         COUNTY == locality$county_name) %>%
  mutate(GEOID = str_extract(GEO_ID, "(?<=S).*")) %>%
  rename(renter_20_ct = AUXKE010,
         rent_burden30_20_ct = AUXKE011,
         rent_burden50_20_ct = AUXKE012,
         age_tot = AUOVE001,
         HH_tot = AUQNE001,
         dis_tot = AU92E001) %>%
  mutate(owner_20_ct = AUXKE002+AUXKE006,
         own_burden30_20_ct = AUXKE003 + AUXKE007,
         own_burden50_20_ct = AUXKE004 + AUXKE008,
         over65 = AUOVE020+AUOVE021+AUOVE022+AUOVE023+AUOVE024+AUOVE025+
           +AUOVE044+AUOVE045+AUOVE046+AUOVE047+AUOVE048+AUOVE049,
         SPHH = AUQNE005+AUQNE008,
         lim_eng = AURLE004+AURLE007+AURLE010+AURLE013,
         poverty_wdisability = AU92E004+AU92E011+AU92E018) %>%
  select(GISJOIN, GEOID,
         renter_20_ct,
         rent_burden30_20_ct,
         rent_burden50_20_ct,
         owner_20_ct,
         own_burden30_20_ct,
         own_burden50_20_ct,
         age_tot,
         HH_tot,
         dis_tot,
         over65,
         SPHH,
         lim_eng,
         poverty_wdisability) %>%
  mutate(cost_burden30_20_ct = rent_burden30_20_ct+own_burden30_20_ct,
         cost_burden50_20_ct = rent_burden50_20_ct+own_burden50_20_ct,
         burden_HH_20_ct = renter_20_ct + owner_20_ct) %>%
  rename(GISJOIN_CT = GISJOIN,
         GEOID_CT = GEOID)

# Income by tenure
rent_income20_ct <- ct_data_2020 %>%
  filter(STATE == locality$state_name,
         COUNTY == locality$county_name) %>%
  mutate(GEOID = str_extract(GEO_ID, "(?<=S).*")) %>%
  rename(renters = AVF6E014,
         renters_5000 = AVF6E015,
         renters_9999 = AVF6E016,
         renters_14999 = AVF6E017,
         renters_19999 = AVF6E018,
         renters_24999 = AVF6E019,
         renters_34999 = AVF6E020,
         renters_49999 = AVF6E021,
         renters_74999 = AVF6E022,
         renters_99999 = AVF6E023,
         renters_149999 = AVF6E024,
         renters_200000 = AVF6E025) %>%
  select(starts_with("renters"), GISJOIN) %>%
  filter(renters > 0) %>%
  select(-renters) %>%
  pivot_longer(cols = c(-GISJOIN))

# % renters by FMI levels
rent_fmi20_ct  <- rent_income20_ct %>%
  group_by(GISJOIN) %>%
  summarise(all_renters_ct = sum(value, na.rm = T),
            renters30_ct = renter_adj(name, value, HUD_FMI$MFI_30),
            renters50_ct = renter_adj(name, value, HUD_FMI$MFI_50),
            renters60_ct = renter_adj(name, value, HUD_FMI$MFI_60),
            renters70_ct = renter_adj(name, value, HUD_FMI$MFI_70),
            renters80_ct = renter_adj(name, value, HUD_FMI$MFI_80)) %>%
  rename(GISJOIN_CT = GISJOIN)

# Create local area units for joining between BG and CT
local_area_ct <- local_area %>%
  group_by(GISJOIN_proj, GISJOIN_CT) %>%
  summarise(pop_la = mean(pop_ct))

# collate data
ct_data <- local_area_ct %>%
  left_join(., ct2020) %>%
  left_join(., hud_housing, by = join_by(GEOID_CT == geoid)) %>%
  left_join(., hud_tract, by = join_by(GEOID_CT == geoid)) %>%
  left_join(., rent_fmi20_ct) %>%
  left_join(., hmda_tract) %>%
  left_join(., cyclomedia_tract) %>%
  group_by(GISJOIN_proj) %>%
  summarise(across(where(is.double), ~sum(.x, na.rm = TRUE))) %>%
  mutate(cost_burden30_20_p = cost_burden30_20_ct/burden_HH_20_ct,
         cost_burden50_20_p = cost_burden50_20_ct/burden_HH_20_ct,
         # Phase 4.2b.1 — local-area rates computed post-summarise so the
         # aggregation across tracts is properly weighted (sum of num /
         # sum of denom). NA when the BG's local-area tracts had no apps
         # / no surveyed units.
         hmda_denial_rate           = if_else(hmda_apps > 0,
                                              hmda_denials / hmda_apps,
                                              NA_real_),
         cyclomedia_prob_per_1000hu = if_else(cyclomedia_hu > 0,
                                              1000 * cyclomedia_prob_n / cyclomedia_hu,
                                              NA_real_))

# Join BG data and create rank variables for risk assessment

# Join to BG data
BGxCT <- bg2020 %>%
  mutate(GISJOIN_CT = str_sub(GISJOIN, end = -2)) %>%
  rename(GISJOIN_proj = GISJOIN) %>%
  select(GISJOIN_proj, GISJOIN_CT)

bg_ct_data <- BGxCT %>%
  inner_join(., bg_data) %>%
  left_join(., ct_data) %>%
  mutate(rank_renter_p = rank(renter_p_ch*-1, na.last = "keep"),
         rank_renter_p = ceiling(rank_renter_p/max(rank_renter_p, na.rm = T)*100),
         rank_housing_tight = rank(housing_tightness*-1, na.last = "keep"),
         rank_housing_tight = ceiling(rank_housing_tight/max(rank_housing_tight, na.rm = T)*100),
         rank_hh_growth = rank(HH_p_ch, na.last = "keep"),
         rank_hh_growth = ceiling(rank_hh_growth/max(rank_hh_growth, na.rm = T)*100),
         rank_vacant_ch = rank(vacancy_Ch*-1, na.last = "keep"),
         rank_vacant_ch = ceiling(rank_vacant_ch/max(rank_vacant_ch, na.rm = T)*100),
         rank_college = rank(college_edu, na.last = "keep"),
         rank_college = ceiling(rank_college/max(rank_college, na.rm = T)*100),
         rank_hhinc = rank(hhinc_ch, na.last = "keep"),
         rank_hhinc = ceiling(rank_hhinc/max(rank_hhinc, na.rm = T)*100),
         rank_hi_inc = rank(hi_inc_ch, na.last = "keep"),
         rank_hi_inc = ceiling(rank_hi_inc/max(rank_hi_inc, na.rm = T)*100),
         rank_lo_inc = rank(lo_inc_ch, na.last = "keep"),
         rank_lo_inc = ceiling(rank_lo_inc/max(rank_lo_inc, na.rm = T)*100),
         rank_rents = rank((median_rent_20-median_rent_10)/median_rent_10, na.last = "keep"),
         rank_rents = ceiling(rank_rents/max(rank_rents, na.rm = T)*100),
         rank_rents2 = rank((rent_bg_Q3_2024-rent_bg_Q3_2017)/rent_bg_Q3_2017, na.last = "keep"),
         rank_rents2 = ceiling(rank_rents2/max(rank_rents2, na.rm = T)*100),
         rank_HV = rank((median_hv_20-median_hv_10)/median_hv_10, na.last = "keep"),
         rank_HV = ceiling(rank_HV/max(rank_HV, na.rm = T)*100),
         rank_permits = rank(permits_N, na.last = "keep"),
         rank_permits = ceiling(rank_permits/max(rank_permits, na.rm = T)*100),
         # Phase 4.2b.2 — new ranks for tenure-aware risk (Goal #4).
         # Permit-type splits (BG-level via stage 05).
         rank_permits_new = rank(permits_new_N, na.last = "keep"),
         rank_permits_new = ceiling(rank_permits_new/max(rank_permits_new, na.rm = T)*100),
         rank_permits_renov = rank(permits_renov_N, na.last = "keep"),
         rank_permits_renov = ceiling(rank_permits_renov/max(rank_permits_renov, na.rm = T)*100),
         rank_permits_demo = rank(permits_demo_N, na.last = "keep"),
         rank_permits_demo = ceiling(rank_permits_demo/max(rank_permits_demo, na.rm = T)*100),
         # MLS price growth + foreclosure rate (BG-level via stage 05).
         rank_mls_growth = rank(mls_price_growth, na.last = "keep"),
         rank_mls_growth = ceiling(rank_mls_growth/max(rank_mls_growth, na.rm = T)*100),
         rank_foreclosure = rank(foreclosure_rate, na.last = "keep"),
         rank_foreclosure = ceiling(rank_foreclosure/max(rank_foreclosure, na.rm = T)*100),
         # HMDA mortgage-denial rate + Cyclomedia condition rate (tract-level
         # via ct_data; Cyclomedia has partial BG coverage by design).
         rank_hmda_denial = rank(hmda_denial_rate, na.last = "keep"),
         rank_hmda_denial = ceiling(rank_hmda_denial/max(rank_hmda_denial, na.rm = T)*100),
         rank_cyclomedia = rank(cyclomedia_prob_per_1000hu, na.last = "keep"),
         rank_cyclomedia = ceiling(rank_cyclomedia/max(rank_cyclomedia, na.rm = T)*100))

# Validation ---------------------------------------------------------------
validation_banner("Stage 06 — tract data & ranks")
# CT-level burden joined onto BGs; low coverage means the BG<->CT join broke.
check_na_share(bg_ct_data, "cost_burden30_20_p", 0.8, "warn")
# Phase 4.2b.1 — new tract-level inputs (HMDA tract IDs match 2020 vintage;
# Cyclomedia is partial-coverage Phase I + Phase II in progress).
check_not_all_na(bg_ct_data, "hmda_denial_rate", "error")
check_range(bg_ct_data, "hmda_denial_rate", 0, 1, "warn")
check_na_share(bg_ct_data, "cyclomedia_prob_per_1000hu", 0.3, "warn")  # partial coverage expected
# Every rank feeds classify_risk(); an all-NA rank silently disables a criterion.
rank_cols <- c("rank_renter_p", "rank_housing_tight", "rank_hh_growth",
               "rank_vacant_ch", "rank_college", "rank_hhinc", "rank_hi_inc",
               "rank_lo_inc", "rank_rents", "rank_rents2", "rank_HV", "rank_permits",
               # Phase 4.2b.2 — new ranks for tenure-aware risk (Goal #4).
               "rank_permits_new", "rank_permits_renov", "rank_permits_demo",
               "rank_mls_growth", "rank_foreclosure",
               "rank_hmda_denial", "rank_cyclomedia")
for (rc in rank_cols) check_not_all_na(bg_ct_data, rc, severity = "error")
for (rc in rank_cols) check_range(bg_ct_data, rc, 1, 100, severity = "warn")
