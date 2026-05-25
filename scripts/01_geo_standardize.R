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
bg90_20 <- read_nhgis(nhgis_xwalk10_20) %>%
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
bg00_20 <- read_csv(nhgis_xwalk10_20) %>%
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

bg10_20 <- read_nhgis(nhgis_xwalk10_20) %>%
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

bg2020 <- read_nhgis(nhgis_2020_bg) %>%
  mutate(pop_asian = ASOAE006+ASOAE007,
         pop_other = ASOAE008+ASOAE009,
         adult_less_than_HS = rowSums(across(ASP3E002:ASP3E016)),
         adult_college_above = rowSums(across(ASP3E022:ASP3E025)),
         HH = ASS8E002,
         GEOID = str_extract(GEO_ID, "(?<=S).*")) %>%
  rename(pop = ASOAE001,
         adult_over25 = ASP3E001,
         pop_white = ASOAE003,
         pop_black = ASOAE004,
         pop_latino = ASOAE012,
         pop_indigenous = ASOAE005,
         HU = ASS8E001,
         HU_vacant = ASS8E003,
         rent_HU = ASVAE001,
         rent_0  = ASVAE027,
         rent_100  =  ASVAE003,
         rent_149  =  ASVAE004,
         rent_199  =  ASVAE005,
         rent_249  =  ASVAE006,
         rent_299  =  ASVAE007,
         rent_349  =  ASVAE008,
         rent_399  =  ASVAE009,
         rent_449  =  ASVAE010,
         rent_499  =  ASVAE011,
         rent_549  =  ASVAE012,
         rent_599  =  ASVAE013,
         rent_649  =  ASVAE014,
         rent_699  =  ASVAE015,
         rent_749  =  ASVAE016,
         rent_799  =  ASVAE017,
         rent_899  =  ASVAE018,
         rent_999  =  ASVAE019,
         rent_1249  = ASVAE020,
         rent_1499  = ASVAE021,
         rent_1999  = ASVAE022,
         rent_2499  = ASVAE023,
         rent_2999  = ASVAE024,
         rent_3499  = ASVAE025,
         rent_3500  = ASVAE026,
         HV_10000 =  ASVLE002,
         HV_14999 =  ASVLE003,
         HV_19999 =  ASVLE004,
         HV_24999 =  ASVLE005,
         HV_29999 =  ASVLE006,
         HV_34999 =  ASVLE007,
         HV_39999 =  ASVLE008,
         HV_49999 =  ASVLE009,
         HV_59999 =  ASVLE010,
         HV_69999 =  ASVLE011,
         HV_79999 =  ASVLE012,
         HV_89999 =  ASVLE013,
         HV_99999 =  ASVLE014,
         HV_124999 = ASVLE015,
         HV_149999 = ASVLE016,
         HV_174999 = ASVLE017,
         HV_199999 = ASVLE018,
         HV_249999 = ASVLE019,
         HV_299999 = ASVLE020,
         HV_399999 = ASVLE021,
         HV_499999 = ASVLE022,
         HV_749999 = ASVLE023,
         HV_999999 = ASVLE024,
         HV_1499999 = ASVLE025,
         HV_1999999 = ASVLE026,
         HV_2000000 = ASVLE027,
         HHINC_10000 = ASQOE002,
         HHINC_14999 = ASQOE003,
         HHINC_19999 = ASQOE004,
         HHINC_24999 = ASQOE005,
         HHINC_29999 = ASQOE006,
         HHINC_34999 = ASQOE007,
         HHINC_39999 = ASQOE008,
         HHINC_44999 = ASQOE009,
         HHINC_49999 = ASQOE010,
         HHINC_59999 = ASQOE011,
         HHINC_74999 = ASQOE012,
         HHINC_99999 = ASQOE013,
         HHINC_124999 = ASQOE014,
         HHINC_149999 = ASQOE015,
         HHINC_199999 = ASQOE016,
         HHINC_250000 = ASQOE017) %>%
  select(-starts_with("AS")) %>%
  select(-YEAR:-NAME_E, -NAME_M) %>%
  filter(pop > 0) %>%
  mutate(data_yr = 2020) %>%
  mutate(GEOID = as.character(GEOID))

# consolidated socioeconomic data for 2013 and 2023
bg_data10_20 <- bind_rows(bg10_20,
                          bg2020)
