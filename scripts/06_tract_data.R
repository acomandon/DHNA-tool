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

# HUD income limits (midpoint of family of 3 and 4)
HUD_FMI <- data.frame(MFI_30 = (23030+27750)/2,
                      MFI_50 = (38150+42350)/2,
                      MFI_60 = (45780+50820)/2,
                      MFI_70 = (53410+59290)/2,
                      MFI_80 = (61000+67750)/2)

# HUD data
# Unzip the data first
HUDzip <- here("data", "prepackaged", "hud", "HUD_AFFH_2024.zip")
outDir <- here("data", "prepackaged", "hud")
unzip(HUDzip,exdir=outDir)

# total number of subsidized units
hud_housing <- read_csv(here("data", "prepackaged", "hud", "HUD_AFFH_2024",
                             "Housing_tract_AFFHT0007_December2024.csv")) %>%
  filter(state_name == locality$state_name,
         county_name == locality$county_name,
         program_label == "Summary") %>%
  select(geoid, total_units)
# Number of households with at least one housing problem and
# number of households
hud_tract <- read_csv(here("data", "prepackaged", "hud", "HUD_AFFH_2024",
                           "AFFH_tract_AFFHT0007_December2024.csv")) %>%
  filter(state_name == locality$state_name,
         county_name == locality$county_name) %>%
  select(geoid, hh_tot_1_m_hus_pb, hh_tot_husholds)

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
  rename(renter_20_ct = ASWFE010,
         rent_burden30_20_ct = ASWFE011,
         rent_burden50_20_ct = ASWFE012,
         age_tot = ASNQE001,
         HH_tot = ASPIE001,
         dis_tot = AS8RE001) %>%
  mutate(owner_20_ct = ASWFE002+ASWFE006,
         own_burden30_20_ct = ASWFE003 + ASWFE007,
         own_burden50_20_ct = ASWFE004 + ASWFE008,
         over65 = ASNQE020+ASNQE021+ASNQE022+ASNQE023+ASNQE024+ASNQE025+
           +ASNQE044+ASNQE045+ASNQE046+ASNQE047+ASNQE048+ASNQE049,
         SPHH = ASPIE005+ASPIE008,
         lim_eng = ASQGE004+ASQGE007+ASQGE010+ASQGE013,
         poverty_wdisability = AS8RE004+AS8RE011+AS8RE018) %>%
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
  rename(renters = ATEVE014,
         renters_5000 = ATEVE015,
         renters_9999 = ATEVE016,
         renters_14999 = ATEVE017,
         renters_19999 = ATEVE018,
         renters_24999 = ATEVE019,
         renters_34999 = ATEVE020,
         renters_49999 = ATEVE021,
         renters_74999 = ATEVE022,
         renters_99999 = ATEVE023,
         renters_149999 = ATEVE024,
         renters_200000 = ATEVE025) %>%
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
  group_by(GISJOIN_proj) %>%
  summarise(across(where(is.double), ~sum(.x, na.rm = TRUE))) %>%
  mutate(cost_burden30_20_p = cost_burden30_20_ct/burden_HH_20_ct,
         cost_burden50_20_p = cost_burden50_20_ct/burden_HH_20_ct)

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
         rank_permits = ceiling(rank_permits/max(rank_permits, na.rm = T)*100))
