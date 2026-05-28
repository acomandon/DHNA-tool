# Stage 04 — Block-group-level variables
# Population change, ethnoracial change, interpolated medians (rent,
# household income, home value), and neighborhood vars (college, renters,
# housing units, income tails) for the local area of each BG.
#
# Depends on: bg10_20, bg2020, local_area, pop_s from stages 01–02.
# Produces: pop10, pop20, pop_change, med_rent10_s, med_rent20_s,
#   med_hhinc10_s, med_hhinc20_s, med_hv10_s, med_hv20_s,
#   nhood_vars10_s, nhood_vars20_s.

# variables to estimate
# Change in population 2010-2020
# Change in population 2000-2010
# direction of change in population 2000-2010/2010-2020
# Change in Black/White/Latino population 2010-2020
# Change in Black/White/Latino population 2000-2010
# Change in direction of Black/White/Latino population 2000-2010/2010-2020
# Change in share of college graduates 2010-2020
# Change in median rent 2010-2020
# Change in median HH income 2010-2020
# Change in median home values 2010-2020
# HH Growth (10-20)/HU Growth (10-20)

# Population and ethnoracial change
pop10 <- pop_s %>%
  filter(data_yr == 2010) %>%
  select(-data_yr) %>%
  rename_at(vars(starts_with("pop")), ~paste0(., "_10"))
pop20 <- pop_s %>%
  filter(data_yr == 2020) %>%
  select(-data_yr) %>%
  rename_at(vars(starts_with("pop")), ~paste0(., "_20"))
pop_change <- pop_s %>%
  filter(data_yr == 2000) %>%
  select(-data_yr) %>%
  rename_at(vars(starts_with("pop")), ~paste0(., "_00")) %>%
  left_join(., pop10) %>%
  left_join(., pop20) %>%
  filter(pop_20 > 0) %>%
  mutate(pop_change00_10 = pop_10-pop_00,
         pop_change10_20 = pop_20-pop_10,
         white_change00_10 = pop_white_10-pop_white_00,
         white_change10_20 = pop_white_20-pop_white_10,
         black_change00_10 = pop_black_10-pop_black_00,
         black_change10_20 = pop_black_20-pop_black_10,
         latino_change00_10 = pop_latino_10-pop_latino_00,
         latino_change10_20 = pop_latino_20-pop_latino_10) %>%
  select(GISJOIN,
         pop_00,
         pop_10,
         pop_20,
         pop_change00_10,
         pop_change10_20,
         white_change00_10,
         white_change10_20,
         pop_black_00,
         pop_black_20,
         black_change00_10,
         black_change10_20,
         latino_change00_10,
         latino_change10_20) %>%
  mutate(blackpct_ch_00_20 = (pop_black_20/pop_20)-(pop_black_00/pop_00)) %>%
  rename(GISJOIN_proj = GISJOIN)

# Median rent
# 2010
med_rent10_s <- local_area %>%
  left_join(., bg10_20, by = join_by(GISJOIN_comp == GISJOIN)) %>%
  select(GISJOIN_comp, GISJOIN_proj, rent_100:rent_1999, rent_2000) %>%
  pivot_longer(cols = c(-GISJOIN_comp, -GISJOIN_proj)) %>%
  group_by(GISJOIN_proj, name) %>%
  summarise(value = sum(value)) %>%
  mutate(rent_val = as.numeric(str_extract(name,"(?<=_)[0-9]+"))) %>%
  arrange(GISJOIN_proj, rent_val) %>%
  group_by(GISJOIN_proj) %>%
  mutate(check_sum = sum(value)) %>%
  filter(check_sum > 100) %>%
  summarise(median_rent_10 = med_lin_est(name, value))
# 2020
med_rent20_s <- local_area %>%
  left_join(., bg2020, by = join_by(GISJOIN_comp == GISJOIN)) %>%
  select(GISJOIN_comp, GISJOIN_proj, rent_100:rent_3500) %>%
  pivot_longer(cols = c(-GISJOIN_comp, -GISJOIN_proj)) %>%
  group_by(GISJOIN_proj, name) %>%
  summarise(value = sum(value)) %>%
  mutate(rent_val = as.numeric(str_extract(name,"(?<=_)[0-9]+"))) %>%
  arrange(GISJOIN_proj, rent_val) %>%
  group_by(GISJOIN_proj) %>%
  mutate(check_sum = sum(value)) %>%
  filter(check_sum > 100) %>%
  summarise(median_rent_20 = med_lin_est(name, value))

# Median household income
# 2010
med_hhinc10_s <- local_area %>%
  left_join(., bg10_20, by = join_by(GISJOIN_comp == GISJOIN)) %>%
  select(GISJOIN_comp, GISJOIN_proj, HHINC_10000:HHINC_250000) %>%
  pivot_longer(cols = c(-GISJOIN_comp, -GISJOIN_proj,)) %>%
  group_by(GISJOIN_proj, name) %>%
  summarise(value = sum(value)) %>%
  mutate(rent_val = as.numeric(str_extract(name,"(?<=_)[0-9]+"))) %>%
  arrange(GISJOIN_proj, rent_val) %>%
  group_by(GISJOIN_proj) %>%
  summarise(median_hhinc_10 = med_lin_est(name, value))
# 2020
med_hhinc20_s <- local_area %>%
  left_join(., bg2020, by = join_by(GISJOIN_comp == GISJOIN)) %>%
  select(GISJOIN_comp, GISJOIN_proj, HHINC_10000:HHINC_250000) %>%
  pivot_longer(cols = c(-GISJOIN_comp, -GISJOIN_proj)) %>%
  group_by(GISJOIN_proj, name) %>%
  summarise(value = sum(value)) %>%
  mutate(rent_val = as.numeric(str_extract(name,"(?<=_)[0-9]+"))) %>%
  arrange(GISJOIN_proj, rent_val) %>%
  group_by(GISJOIN_proj) %>%
  summarise(median_hhinc_20 = med_lin_est(name, value))

# Median home value
# 2010
med_hv10_s <- local_area %>%
  left_join(., bg10_20, by = join_by(GISJOIN_comp == GISJOIN)) %>%
  select(GISJOIN_comp, GISJOIN_proj, HV_10000:HV_1000000) %>%
  pivot_longer(cols = c(-GISJOIN_comp, -GISJOIN_proj)) %>%
  group_by(GISJOIN_proj, name) %>%
  summarise(value = sum(value)) %>%
  mutate(rent_val = as.numeric(str_extract(name,"(?<=_)[0-9]+"))) %>%
  arrange(GISJOIN_proj, rent_val) %>%
  group_by(GISJOIN_proj) %>%
  summarise(median_hv_10 = med_lin_est(name, value))
# 2020
med_hv20_s <- local_area %>%
  left_join(., bg2020, by = join_by(GISJOIN_comp == GISJOIN)) %>%
  select(GISJOIN_comp, GISJOIN_proj, HV_10000:HV_2000000) %>%
  pivot_longer(cols = c(-GISJOIN_comp, -GISJOIN_proj)) %>%
  group_by(GISJOIN_proj, name) %>%
  summarise(value = sum(value)) %>%
  mutate(rent_val = as.numeric(str_extract(name,"(?<=_)[0-9]+"))) %>%
  arrange(GISJOIN_proj, rent_val) %>%
  group_by(GISJOIN_proj) %>%
  summarise(median_hv_20 = med_lin_est(name, value))

# Additional variables
# 2010
nhood_vars10_s <- local_area %>%
  left_join(., bg10_20, by = join_by(GISJOIN_comp == GISJOIN)) %>%
  mutate(hi_inc_hh = rowSums(across(HHINC_99999:HHINC_250000)),
         lo_inc_hh = rowSums(across(HHINC_10000:HHINC_29999))) %>%
  group_by(GISJOIN_proj) %>%
  summarise(adult_college_above_10 = sum(adult_college_above),
            adult_over25_10 = sum(adult_over25),
            renters_10 = sum(rent_HU),
            HU_10 = sum(HU),
            HU_vacant_10 = sum(HU_vacant),
            HH_10 = sum(HH),
            hi_inc_hh_10 = sum(hi_inc_hh),
            lo_inc_hh_10 = sum(lo_inc_hh))
# 2020
nhood_vars20_s <- local_area %>%
  left_join(., bg2020, by = join_by(GISJOIN_comp == GISJOIN)) %>%
  mutate(hi_inc_hh = rowSums(across(HHINC_124999:HHINC_250000)),
         lo_inc_hh = rowSums(across(HHINC_10000:HHINC_34999))) %>%
  group_by(GISJOIN_proj) %>%
  summarise(adult_college_above_20 = sum(adult_college_above),
            adult_over25_20 = sum(adult_over25),
            renters_20 = sum(rent_HU),
            HU_20 = sum(HU),
            HU_vacant_20 = sum(HU_vacant),
            HH_20 = sum(HH),
            hi_inc_hh_20 = sum(hi_inc_hh),
            lo_inc_hh_20 = sum(lo_inc_hh),
            # Phase 4.2b.3 — B25038-derived owner long-tenure (num/denom
            # summed across the local-area BGs; rate computed in stage 05).
            owners_b25038_20      = sum(owner_occ_total),
            owner_longtenure_n_20 = sum(owner_longtenure_n))

# Validation ---------------------------------------------------------------
validation_banner("Stage 04 — block-group variables")
check_min_rows(pop_change, "pop_change", 100)
check_na_share(med_rent20_s, "median_rent_20", 0.5, "warn")
check_range(med_rent10_s, "median_rent_10", 0, 10000, "warn")
check_range(med_rent20_s, "median_rent_20", 0, 10000, "warn")
check_range(med_hhinc10_s, "median_hhinc_10", 0, 500000, "warn")
check_range(med_hhinc20_s, "median_hhinc_20", 0, 500000, "warn")
check_range(med_hv10_s, "median_hv_10", 0, 5000000, "warn")
check_range(med_hv20_s, "median_hv_20", 0, 5000000, "warn")
