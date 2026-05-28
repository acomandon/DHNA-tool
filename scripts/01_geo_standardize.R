# Stage 01 — Geography standardization
# Crosswalk 1990, 2000, 2010, and 2020 NHGIS data to 2020 block groups.
#
# Depends on: libraries + config sourced by 00_run_all.R; NHGIS extracts
#   already downloaded into data/nhgis/{block,blockgroup,crosswalks}.
# Produces: bg90_20, bg00_20, bg10_20, bg2020, bg_data10_20 + path vars
#   nhgis_xwalk10_20 (consumed by later stages).

# Load data for standardization -------------------------------------------

# 1990 block to 2020 block group for ethnoracial data
# load 1990 block data and formate variables

nhgis_1990_bl <- list.files(here("data", "nhgis", "block", "bl1990"),
                            full.names = TRUE)

bl1990 <- read_nhgis(nhgis_1990_bl) %>%
  mutate(pop = rowSums(across(ET2001:ET2010)),
         pop_latino = rowSums(across(ET2006:ET2010))) %>%
  rename(pop_white = ET2001,
         pop_black = ET2002,
         pop_indigenous = ET2003,
         pop_asian = ET2004,
         pop_other = ET2005) %>%
  select(GISJOIN,
         pop,
         pop_white,
         pop_black,
         pop_indigenous,
         pop_asian,
         pop_other,
         pop_latino)

# crosswalk to 2010 block group
nhgis_xwalk90_10 <- list.files(here("data", "nhgis", "crosswalks"),
                               pattern = "nhgis_blk1990_bg2010",
                            full.names = TRUE)
bl90xbg10 <- read_nhgis(nhgis_xwalk90_10) %>%
  left_join(., bl1990, by = join_by(blk1990gj==GISJOIN)) %>%
  mutate(across(pop:pop_latino, ~ .x * weight)) %>%
  group_by(bg2010gj, bg2010ge) %>%
  summarise(across(pop:pop_latino, ~ sum(.,na.rm = T)))

# crosswalk to 2020 block group
nhgis_xwalk10_20 <- list.files(here("data", "nhgis", "crosswalks"),
                               pattern = "nhgis_bg2010_bg2020",
                               full.names = TRUE)
# Read the 2010->2020 crosswalk once; it is reused for all three vintages below.
xwalk10_20 <- read_nhgis(nhgis_xwalk10_20)
bg90_20 <- xwalk10_20 %>%
  left_join(., bl90xbg10) %>%
  mutate(across(pop:pop_latino, ~ .x * wt_pop)) %>%
  group_by(bg2020gj, bg2020ge) %>%
  summarise(across(pop:pop_latino, ~ sum(.,na.rm = T))) %>%
  mutate(data_yr = 1990) %>%
  rename(GISJOIN = bg2020gj,
         GEOID = bg2020ge) %>%
  mutate(GEOID = as.character(GEOID))

rm(bl1990)
rm(bl90xbg10)

# 2000 block to 2020 block group for ethnoracial data
# load 2000 data
nhgis_2000_bl <- list.files(here("data", "nhgis", "block", "bl2000"),
                            full.names = TRUE)

bl2000 <- read_nhgis(nhgis_2000_bl) %>%
  mutate(pop = rowSums(across(FYF001:FYF014)),
         pop_latino = rowSums(across(FYF008:FYF014)),
         pop_asian = FYF004+FYF005,
         pop_other = FYF006+FYF007) %>%
  rename(pop_white = FYF001,
         pop_black = FYF002,
         pop_indigenous = FYF003) %>%
  select(GISJOIN,
         pop,
         pop_white,
         pop_black,
         pop_indigenous,
         pop_asian,
         pop_other,
         pop_latino)
# Join 2000 data to 2010 crosswalk and apply individual weights
nhgis_xwalk00_10 <- list.files(here("data", "nhgis", "crosswalks"),
                               pattern = "nhgis_blk2000_bg2010",
                               full.names = TRUE)
bl00xbg10 <- read_nhgis(nhgis_xwalk00_10) %>%
  left_join(., bl2000, by = join_by(blk2000gj==GISJOIN)) %>%
  mutate(across(pop:pop_latino, ~ .x * weight)) %>%
  group_by(bg2010gj, bg2010ge) %>%
  summarise(across(pop:pop_latino, ~ sum(.,na.rm = T)))
# Join 2000 data standardized to 2010 geography to 2020 crosswalk
#  and apply individual weights
bg00_20 <- xwalk10_20 %>%
  left_join(., bl00xbg10) %>%
  mutate(across(pop:pop_latino, ~ .x * wt_pop)) %>%
  group_by(bg2020gj, bg2020ge) %>%
  summarise(across(pop:pop_latino, ~ sum(.,na.rm = T))) %>%
  mutate(data_yr = 2000)  %>%
  rename(GISJOIN = bg2020gj,
         GEOID = bg2020ge) %>%
  mutate(GEOID = as.character(GEOID))
# remove extraneous data
rm(bl2000)
rm(bl00xbg10)

# Load 2010 socioeconomic data
nhgis_2010_bg <- list.files(here("data", "nhgis", "blockgroup", "bg2010"),
                            full.names = TRUE)

bg2010 <- read_nhgis(nhgis_2010_bg) %>%
  mutate(pop_asian = UEYE006+UEYE007,
         pop_other = UEYE008+UEYE009,
         adult_less_than_HS = rowSums(across(UGSE002:UGSE016)),
         adult_college_above = rowSums(across(UGSE022:UGSE025))) %>%
  rename(pop = UEYE001,
         adult_over25 = UGSE001,
         pop_white = UEYE003,
         pop_black = UEYE004,
         pop_latino = UEYE012,
         pop_indigenous = UEYE005,
         HU = UKNE001,
         HH = UKNE002,
         HU_vacant = UKNE003,
         rent_HU = UL8E001,
         rent_0  = UL8E024,
         rent_100  = UL8E003,
         rent_149  = UL8E004,
         rent_199  = UL8E005,
         rent_249  = UL8E006,
         rent_299  = UL8E007,
         rent_349  = UL8E008,
         rent_399  = UL8E009,
         rent_449  = UL8E010,
         rent_499  = UL8E011,
         rent_549  = UL8E012,
         rent_599  = UL8E013,
         rent_649  = UL8E014,
         rent_699  = UL8E015,
         rent_749  = UL8E016,
         rent_799  = UL8E017,
         rent_899  = UL8E018,
         rent_999  = UL8E019,
         rent_1249  = UL8E020,
         rent_1499  = UL8E021,
         rent_1999  = UL8E022,
         rent_2000  = UL8E023,
         HV_10000 = UMKE002,
         HV_14999 = UMKE003,
         HV_19999 = UMKE004,
         HV_24999 = UMKE005,
         HV_29999 = UMKE006,
         HV_34999 = UMKE007,
         HV_39999 = UMKE008,
         HV_49999 = UMKE009,
         HV_59999 = UMKE010,
         HV_69999 = UMKE011,
         HV_79999 = UMKE012,
         HV_89999 = UMKE013,
         HV_99999 = UMKE014,
         HV_124999 = UMKE015,
         HV_149999 = UMKE016,
         HV_174999 = UMKE017,
         HV_199999 = UMKE018,
         HV_249999 = UMKE019,
         HV_299999 = UMKE020,
         HV_399999 = UMKE021,
         HV_499999 = UMKE022,
         HV_749999 = UMKE023,
         HV_999999 = UMKE024,
         HV_1000000 = UMKE025,
         HHINC_10000 = UHCE002,
         HHINC_14999 = UHCE003,
         HHINC_19999 = UHCE004,
         HHINC_24999 = UHCE005,
         HHINC_29999 = UHCE006,
         HHINC_34999 = UHCE007,
         HHINC_39999 = UHCE008,
         HHINC_44999 = UHCE009,
         HHINC_49999 = UHCE010,
         HHINC_59999 = UHCE011,
         HHINC_74999 = UHCE012,
         HHINC_99999 = UHCE013,
         HHINC_124999 = UHCE014,
         HHINC_149999 = UHCE015,
         HHINC_199999 = UHCE016,
         HHINC_250000 = UHCE017) %>%
  select(-starts_with("U")) %>%
  select(-YEAR:-NAME_E, -NAME_M)
# Join 2000 data to 2010 crosswalk and apply appropriate weights

bg10_20 <- xwalk10_20 %>%
  left_join(., bg2010, by = join_by(bg2010gj == GISJOIN)) %>%
  mutate(across(starts_with("pop"), ~ .x * wt_pop),
         across(starts_with("adult"), ~ .x * wt_adult),
         across(starts_with("HU"), ~ .x * wt_hu),
         across(starts_with("HH"), ~ .x * wt_hu),
         across(starts_with("rent"), ~ .x * wt_renthu),
         across(starts_with("HV"), ~ .x * wt_ownhu)) %>%
  group_by(bg2020gj, bg2020ge) %>%
  summarise(across(pop:adult_college_above, ~ sum(.,na.rm = T))) %>%
  mutate(data_yr = 2010) %>%
  rename(GISJOIN = bg2020gj,
         GEOID = bg2020ge) %>%
  mutate(GEOID = as.character(GEOID))
# remove extraneous data
rm(bg2010)

# Load 2020 socioeconomic data
nhgis_2020_bg <- list.files(here("data", "nhgis", "blockgroup", "bg2020"),
                            full.names = TRUE)

bg2020_main <- read_nhgis(nhgis_2020_bg)

# Phase 4.2b.3 — B25038 (tenure by year moved in) for owner long-tenure.
# If the main bg2020 extract pre-dates B25038 being added to bg2020_spec,
# the user can run scripts/fetch_b25038.R to pull just B25038 into
# data/nhgis/blockgroup/bg2020_supp/. Join it here so downstream code can
# reference AUVSE* either way.
supp_dir <- here("data", "nhgis", "blockgroup", "bg2020_supp")
if (!"AUVSE002" %in% colnames(bg2020_main) && dir.exists(supp_dir)) {
  supp_files <- list.files(supp_dir, full.names = TRUE)
  if (length(supp_files) > 0) {
    bg2020_supp <- read_nhgis(supp_files) %>%
      select(GISJOIN, starts_with("AUVS"))
    bg2020_main <- left_join(bg2020_main, bg2020_supp, by = "GISJOIN")
  }
}

bg2020 <- bg2020_main %>%
  mutate(pop_asian = AUPFE006+AUPFE007,
         pop_other = AUPFE008+AUPFE009,
         adult_less_than_HS = rowSums(across(AUQ8E002:AUQ8E016)),
         adult_college_above = rowSums(across(AUQ8E022:AUQ8E025)),
         HH = AUUDE002,
         # Phase 4.2b.3 — B25038 owner long-tenure: owners who moved in
         # 2010-2014, 2000-2009, 1990-1999, or 1989-earlier (>= ~10 yrs
         # by mid-window of the 2020-2024 ACS). Requires B25038 in the
         # bg2020_spec extract; re-run data_downloader.R if AUVSE005
         # is missing.
         owner_longtenure_n = AUVSE005+AUVSE006+AUVSE007+AUVSE008,
         GEOID = str_extract(GEO_ID, "(?<=S).*")) %>%
  rename(pop = AUPFE001,
         adult_over25 = AUQ8E001,
         pop_white = AUPFE003,
         pop_black = AUPFE004,
         pop_latino = AUPFE012,
         pop_indigenous = AUPFE005,
         HU = AUUDE001,
         HU_vacant = AUUDE003,
         owner_occ_total = AUVSE002,   # Phase 4.2b.3 — B25038 denominator
         rent_HU = AUWFE001,
         rent_0  = AUWFE027,
         rent_100  =  AUWFE003,
         rent_149  =  AUWFE004,
         rent_199  =  AUWFE005,
         rent_249  =  AUWFE006,
         rent_299  =  AUWFE007,
         rent_349  =  AUWFE008,
         rent_399  =  AUWFE009,
         rent_449  =  AUWFE010,
         rent_499  =  AUWFE011,
         rent_549  =  AUWFE012,
         rent_599  =  AUWFE013,
         rent_649  =  AUWFE014,
         rent_699  =  AUWFE015,
         rent_749  =  AUWFE016,
         rent_799  =  AUWFE017,
         rent_899  =  AUWFE018,
         rent_999  =  AUWFE019,
         rent_1249  = AUWFE020,
         rent_1499  = AUWFE021,
         rent_1999  = AUWFE022,
         rent_2499  = AUWFE023,
         rent_2999  = AUWFE024,
         rent_3499  = AUWFE025,
         rent_3500  = AUWFE026,
         HV_10000 =  AUWQE002,
         HV_14999 =  AUWQE003,
         HV_19999 =  AUWQE004,
         HV_24999 =  AUWQE005,
         HV_29999 =  AUWQE006,
         HV_34999 =  AUWQE007,
         HV_39999 =  AUWQE008,
         HV_49999 =  AUWQE009,
         HV_59999 =  AUWQE010,
         HV_69999 =  AUWQE011,
         HV_79999 =  AUWQE012,
         HV_89999 =  AUWQE013,
         HV_99999 =  AUWQE014,
         HV_124999 = AUWQE015,
         HV_149999 = AUWQE016,
         HV_174999 = AUWQE017,
         HV_199999 = AUWQE018,
         HV_249999 = AUWQE019,
         HV_299999 = AUWQE020,
         HV_399999 = AUWQE021,
         HV_499999 = AUWQE022,
         HV_749999 = AUWQE023,
         HV_999999 = AUWQE024,
         HV_1499999 = AUWQE025,
         HV_1999999 = AUWQE026,
         HV_2000000 = AUWQE027,
         HHINC_10000 = AURTE002,
         HHINC_14999 = AURTE003,
         HHINC_19999 = AURTE004,
         HHINC_24999 = AURTE005,
         HHINC_29999 = AURTE006,
         HHINC_34999 = AURTE007,
         HHINC_39999 = AURTE008,
         HHINC_44999 = AURTE009,
         HHINC_49999 = AURTE010,
         HHINC_59999 = AURTE011,
         HHINC_74999 = AURTE012,
         HHINC_99999 = AURTE013,
         HHINC_124999 = AURTE014,
         HHINC_149999 = AURTE015,
         HHINC_199999 = AURTE016,
         HHINC_250000 = AURTE017) %>%
  select(-starts_with("AU")) %>%
  select(-YEAR:-NAME_E, -NAME_M) %>%
  filter(pop > 0) %>%
  mutate(data_yr = 2020) %>%
  mutate(GEOID = as.character(GEOID))

# consolidated socioeconomic data for 2013 and 2023
bg_data10_20 <- bind_rows(bg10_20,
                          bg2020)

# Validation ---------------------------------------------------------------
validation_banner("Stage 01 — geography standardization")
# The 2010->2020 crosswalk drives every vintage; its weights must partition
# each source block group (sum to 1) for each weighted quantity. NHGIS state
# crosswalks include fragment rows for out-of-state source BGs that overlap
# the state's 2020 boundary -- those rows do not (and should not) sum to 1 in
# this single file, so we restrict the check to in-state source units.
xwalk_instate <- xwalk10_20 %>%
  dplyr::filter(startsWith(bg2010gj, paste0("G", locality$state_fips)))
for (w in c("wt_pop", "wt_adult", "wt_hu", "wt_renthu", "wt_ownhu")) {
  check_weights_sum_to_one(xwalk_instate, "bg2010gj", w)
}
check_min_rows(bg2020, "bg2020", 100)
check_min_rows(bg90_20, "bg90_20", 100)
check_min_rows(bg00_20, "bg00_20", 100)
check_min_rows(bg10_20, "bg10_20", 100)
check_coverage(min(nrow(bg90_20), nrow(bg00_20), nrow(bg10_20)),
               nrow(bg2020), "BG counts consistent across vintages",
               min_frac = 0.7, severity = "warn")
# Population conservation: each crosswalked vintage's total should sit within a
# plausible band of the 2020 total (catches a bad block-level crosswalk).
ref_pop <- sum(bg2020$pop, na.rm = TRUE)
for (nm in c("bg90_20", "bg00_20", "bg10_20")) {
  tot <- sum(get(nm)$pop, na.rm = TRUE)
  dhna_check(tot > 0.4 * ref_pop & tot < 2.5 * ref_pop,
             sprintf("%s total pop within plausible band of 2020", nm),
             sprintf("%.0f vs 2020 %.0f", tot, ref_pop), "warn")
}
rm(xwalk10_20)
