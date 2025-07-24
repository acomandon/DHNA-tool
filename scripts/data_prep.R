# Tool data builder  
# standardizes all data to 2020 geography
# Creates all required variables
# runs risk assessment

# Packages and functions -----------------------------------------------
library(ipumsr)
library(data.table)
library(tidyverse)
library(tidycensus)
library(tidygeocoder)
library(purrr)
library(here)
library(sf)
library(mapproj)
library(leaflet)
library(survey)
library(spatstat)
library(ggalluvial)
library(ggrepel)
library(forcats)

# functions
# median linear interpolation
med_lin_est <- function(name, value) {
  tot_value = as.numeric(value)
  mp = min(which(cumsum(tot_value)/sum(tot_value) > .5))
  bin_1 = as.numeric(str_extract(name[mp],"(?<=_)[0-9]+")) 
  bin_0 = as.numeric(str_extract(name[min(which(cumsum(tot_value)/sum(tot_value) > .5))-1],"(?<=_)[0-9]+")) 
  inc_width = bin_1 - bin_0
  inc_ratio = (sum(tot_value)/2 - sum(tot_value[1:(mp-1)]))/tot_value[mp]
  inc_ratio_1 <- .5/(sum(tot_value[1])/sum(tot_value))
  median_value = ifelse(mp > 1, bin_0+inc_ratio*inc_width, 
                        as.numeric(str_extract(name[mp],"(?<=_)[0-9]+"))[mp+1]*inc_ratio_1)
  return(median_value)
}
# renter under FMI cutoff adjustment
renter_adj <- function(name, value, fmi) {
  inc_bins = as.numeric(str_extract(name,"(?<=_)[0-9]+"))
  mp = min(which(inc_bins>fmi))
  bin_1 = inc_bins[mp] 
  bin_0 = inc_bins[mp-1]
  inc_width = bin_1 - bin_0
  bin_ratio = (inc_width - (bin_1 - fmi))/inc_width
  adjusted_pop = round(sum(value[1:mp-1])+value[mp]*bin_ratio)
  return(adjusted_pop)
}


# Data Downloader ---------------------------------------------------------

# if running for the first time, run the data downloader by running the 
# source command below
# source(here("scripts", "data_downloader.R"))

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

# Local area unit 
# create population variable for BG and CT to join to layers for filtering
bg_pop20 <- bg2020 %>% 
  select(GISJOIN, pop)
ct_pop20 <- bg2020 %>% 
  select(GISJOIN, pop) %>% 
  mutate(GISJOIN_CT = str_sub(GISJOIN, end = -2)) %>% 
  group_by(GISJOIN_CT) %>% 
  summarize(pop_ct = sum(pop))

# Load layers for BG, BG population center and tract
lvm_bg_geo <- st_read(here("data", "nhgis", "gis", "blockgroup", "bg2023", "KY_Jefferson_BG_2023.shp")) %>% 
  st_transform(lvm_bg_geo, crs=4326) %>% 
  left_join(., bg_pop20) %>% 
  filter(pop > 0) %>% 
  select(GISJOIN, GEOID) %>% 
  rename(GISJOIN_BG = GISJOIN,
         GEOID_BG = GEOID)

lvm_bg_popctr_geo <- st_read(here("data", "nhgis", "gis", "blockgroup", "bgc2020", "KY_Jefferson_BGC_2020.shp")) %>% 
  st_transform(lvm_bg_geo, crs=4326) %>% 
  left_join(., bg_pop20) %>% 
  filter(pop > 0) %>% 
  select(GISJOIN, GEOID, pop) %>% 
  rename(GISJOIN_BG_PC = GISJOIN,
         GEOID_BG_PC = GEOID,
         pop_bg = pop)

# BG in each CT to join to local area
bg_ct_match <- bg_pop20 %>% 
  mutate(GISJOIN_CT = str_sub(GISJOIN, end = -2))
# define local area as the BG in CT where half the population lives within
# 800m of the project BG
local_area <- st_join(lvm_bg_geo, lvm_bg_popctr_geo, 
                      join = st_is_within_distance,
                      dist = 800) %>% 
  st_drop_geometry() %>% 
  mutate(GISJOIN_CT = str_sub(GISJOIN_BG_PC, end = -2)) %>% 
  left_join(., ct_pop20) %>% 
  group_by(GISJOIN_BG, GISJOIN_CT) %>% 
  summarise(pop_in_CT = sum(pop_bg),
            pop_ct = mean(pop_ct)) %>% 
  mutate(pct_in_CT = pop_in_CT/pop_ct) %>% 
  filter(pct_in_CT >= 0.5) %>% 
  left_join(., bg_ct_match,
            relationship = "many-to-many") %>% 
  rename(GISJOIN_proj = GISJOIN_BG, # BG where project is - summarize data
         GISJOIN_comp = GISJOIN,
         pop_BG = pop) # All BG in the CT - join data

# data for ethnoracial change ---------------------------------------------
# population and ethnorace 1990-2020 for change in composition figure 
# comparing change in local area and market area and demographic change 
# calculations


# format ethnoracial data to be import-ready in app
commcols <- intersect(names(bg90_20), names(bg00_20))

pop_ethnorace <- bind_rows(bg90_20[commcols],
                           bg00_20[commcols],
                           bg10_20[commcols],
                           bg2020[commcols]) %>% 
  mutate_at(vars(matches("pop")), round) 


# Local Area
# create long data set with each year data for each local area 
pop_s <- left_join(local_area, pop_ethnorace,
                   by = join_by(GISJOIN_comp == GISJOIN),
                   relationship = "many-to-many") %>% 
  group_by(GISJOIN_proj, data_yr) %>% 
  summarise_at(vars(pop:pop_latino), sum) %>% 
  mutate(Area = "Local Area") %>% 
  rename(GISJOIN = GISJOIN_proj) %>% 
  mutate(group_id = GISJOIN)

# Market Area
# import market area shapefile
Jefferson_ma <- st_read(here("data", "prepackaged", "Market_areas", "Comp_Plan_Market_Areas.shp")) %>% 
  st_transform(., crs=4326) %>% 
  select(OBJECTID, Name) %>% 
  rename(market_area = Name)
# join market areas to population center layer and summarize population
# data by market area
Jefferson_bg_ma <- st_join(lvm_bg_popctr_geo, Jefferson_ma,
                           join = st_intersects) %>% 
  drop_na(market_area) %>% 
  st_drop_geometry() %>% 
  select(-OBJECTID) %>% 
  left_join(., pop_ethnorace, 
            by = join_by(GISJOIN_BG_PC == GISJOIN),
            relationship = "one-to-many") %>% 
  mutate(Area = "Market Area") %>% 
  select(-GEOID, -pop_bg, -GEOID_BG_PC) %>% 
  rename(GISJOIN = GISJOIN_BG_PC,
         group_id = market_area) %>% 
  group_by(group_id, data_yr) %>% 
  mutate(pop = sum(pop),
         pop_white = sum(pop_white),
         pop_black = sum(pop_black),
         pop_indigenous = sum(pop_indigenous),
         pop_asian = sum(pop_asian),
         pop_latino = sum(pop_latino)) %>% 
  ungroup()
# Join the two geographies and
# export processed data to be used direclty in the tool
dir.create(file.path(here("data", "processed")), 
           recursive = TRUE)
pop_ma_s <- bind_rows(pop_s, Jefferson_bg_ma) %>% 
  write_csv(., here("DHNA", "data", "pop_ethnorace.csv"))


# PUMS data for housing assessment plots ----------------------------------
# Data for:
# Housing mismatch figure
# Housing cost burden figure

# Create table with income limits based on HOME program
# ELI: Extremely low income 30% limit
# VLI: Very low income 50% limit
# LI: Low income 80% limit

HUD_FMI <- data.frame(YEAR = c(rep(2023,8), rep(2022,8), rep(2021,8)),
                      NUMPREC = c(rep(seq(1,8,1),3)),
                      MFI_ELI = c(18850,21550,24240,26900,29100,31250,33400,35550,
                                  17800,20350,23030,27750,32470,37190,41910,46630,
                                  16150,18450,21960,26500,31040,35580,40120,44660),
                      MFI_VLI = c(31400,35900,40400,44850,48450,52050,55650,59250,
                                  29650,33900,38150,42350,45750,49150,52550,55950,
                                  26950,30800,34650,38450,41550,44650,47700,50800),
                      MFI_LI = c(50250,57400,64600,71750,77500,83250,89000,98750,
                                 47450,542000,61000,67750,73200,78600,84050,89450,
                                 43050,49200,55350,61500,66450,71350,76300,81200))

pums_acs <- list.files(here("data", "pums_usa", "acs_21_23"),
                       pattern = "\\.xml$",
                            full.names = TRUE) 

ddi <- read_ipums_ddi(pums_acs)


# Affordability
# renters
ky_h <- read_ipums_micro(ddi) %>% 
  filter(PUMA %in% c(1701,1702,1703,1704,1705,1706),
         HHINCOME < 9999999,
         OWNERSHP == 2
  ) %>% 
  mutate(HH_ID = paste(SAMPLE, SERIAL),
         HHINCOME = if_else(HHINCOME < 0, 0, HHINCOME)) %>% 
  left_join(., HUD_FMI) %>% 
  mutate(HHINC_levels = if_else(HHINCOME < MFI_ELI, "Below 30%  ($27,125)", NA),
         HHINC_levels = if_else(HHINCOME >= MFI_ELI & HHINCOME < MFI_VLI, "30% to 50% ($40,400)", HHINC_levels),
         HHINC_levels = if_else(HHINCOME >= MFI_VLI & HHINCOME < MFI_LI, "50% to 80% ($64,625)", HHINC_levels),
         HHINC_levels = if_else(HHINCOME >= MFI_LI, "Above 80%", HHINC_levels),
         RENT_levels = if_else(RENTGRS < (MFI_ELI*.3)/12,"Below 30%  ($678)", NA),
         RENT_levels = if_else(RENTGRS >= (MFI_ELI*.3)/12 & RENTGRS < (MFI_VLI*.3)/12, "30% to 50% ($1010)", RENT_levels),
         RENT_levels = if_else(RENTGRS >= (MFI_VLI*.3)/12 & RENTGRS < (MFI_LI*.3)/12, "50% to 80% ($1615)", RENT_levels),
         RENT_levels = if_else(RENTGRS >= (MFI_LI*.3)/12, "Above 80%  (market)", RENT_levels),
         HHINC_levels = factor(HHINC_levels, levels = c("Below 30%  ($27,125)",
                                                        "30% to 50% ($40,400)",
                                                        "50% to 80% ($64,625)",
                                                        "Above 80%")),
         RENT_levels = factor(RENT_levels, levels = c("Below 30%  ($678)",
                                                      "30% to 50% ($1010)",
                                                      "50% to 80% ($1615)",
                                                      "Above 80%  (market)")),
         rent_burden = if_else(HHINCOME*.3/12 > RENTGRS, "No burden", NA),
         rent_burden = if_else(HHINCOME*.5/12 <= RENTGRS, "Severely burdened", rent_burden),
         rent_burden = if_else(HHINCOME*.3/12 <= RENTGRS & HHINCOME*.5/12 > RENTGRS, "Burdened", rent_burden),
         rent_burden = factor(rent_burden, levels = c("No burden",
                                                      "Burdened",
                                                      "Severely burdened"))) %>% 
  select(HHWT, HHINC_levels, RENT_levels,RENTGRS, rent_burden)
head(ky_h)
write_csv(ky_h, here("DHNA", "data", "hh_micro.csv"))


# BG data formatting ------------------------------------------------------

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
  select(GISJOIN_comp, GISJOIN_proj,, HV_10000:HV_2000000) %>% 
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
            lo_inc_hh_20 = sum(lo_inc_hh))

# BG buffer data ----------------------------------------------------------

# rent from Renthub - change between Q3 2017 and Q3 2024
# limiting to areas where there are at least 50 observations 
rent_buffer <- read_csv(here("data", "prepackaged", "renthub", "rent_buffer.csv")) %>% 
  mutate(year_qu_text = paste0(R_quarter, "_", R_year)) %>% 
  filter(year_qu_text == "Q3_2017"|year_qu_text == "Q3_2024") %>% 
  filter(N > 50) %>% 
  group_by(GISJOIN) %>% 
  mutate(bg_n = n()) %>% 
  filter(bg_n == 2) %>% # make sure there is enough observations in both years
  select(-bg_n) %>% 
  pivot_wider(id_cols = c(GISJOIN, GEOID_bg, market_area), 
              names_from = year_qu_text, 
              values_from = c(rent_bg,rent_ma)) %>% 
  rename(GISJOIN_proj = GISJOIN) %>%
  select(-market_area)

# rent from Renthub - Monthly change from 01/2019 to 09/2024
# limiting to areas where there are at least 50 observations 
rent_buffer <- read_csv(here("data", "prepackaged", "renthub", "rent_buffer.csv")) %>% 
  mutate(year_qu_text = paste0(R_quarter, "_", R_year)) %>% 
  filter(year_qu_text == "Q3_2017"|year_qu_text == "Q3_2024") %>% 
  filter(N > 50) %>% 
  group_by(GISJOIN) %>% 
  mutate(bg_n = n()) %>% 
  filter(bg_n == 2) %>% # make sure there is enough observations in both years
  select(-bg_n) %>% 
  pivot_wider(id_cols = c(GISJOIN, GEOID_bg, market_area), 
              names_from = year_qu_text, 
              values_from = c(rent_bg,rent_ma)) %>% 
  rename(GISJOIN_proj = GISJOIN) %>%
  select(-market_area) %>% 
  write_csv(., here("DHNA", "data", "Renthub_monthly_rent.csv"))

# number of permits and affordable units within 800 m of block
# Create 800m buffer around BG population center
lvm_bg_popctr_buffer <- lvm_bg_popctr_geo %>% 
  st_transform(crs=4326) %>%
  select(GISJOIN_BG_PC) %>% 
  rename(GISJOIN_proj = GISJOIN_BG_PC) %>% 
  st_transform(crs = 2246) %>% 
  st_buffer(dist = 2640)
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
affordable <- st_transform(affordable, 2246)
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
  left_join(., rent_buffer) %>% 
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
           (adult_college_above_10/adult_over25_20),
         hi_inc_ch = (hi_inc_hh_20/HH_20)-(hi_inc_hh_10/HH_10),
         hi_inc_ch_pop = hi_inc_hh_20-hi_inc_hh_10,
         lo_inc_ch = (lo_inc_hh_20/HH_20)-(lo_inc_hh_10/HH_10),
         lo_inc_ch_pop = lo_inc_hh_20-lo_inc_hh_10,
         hhinc_ch = (median_hhinc_20-median_hhinc_10)/median_hhinc_10)

# Tract data --------------------------------------------------------------

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
  filter(state_name == "Kentucky",
         county_name == "Jefferson County",
         program_label == "Summary") %>% 
  select(geoid, total_units)
# Number of households with at least one housing problem and 
# number of households
hud_tract <- read_csv(here("data", "prepackaged", "hud", "HUD_AFFH_2024",
                           "AFFH_tract_AFFHT0007_December2024.csv")) %>% 
  filter(state_name == "Kentucky",
         county_name == "Jefferson County") %>% 
  select(geoid, hh_tot_1_m_hus_pb, hh_tot_husholds)

nhgis_2020_ct <- list.files(here("data", "nhgis", "tract", "ct2020"),
                            full.names = TRUE)

ct_data_2020_A <- read_nhgis(nhgis_2020_ct[1]) 
ct_data_2020_B <- read_nhgis(nhgis_2020_ct[2]) 
ct_data_2020 <- left_join(ct_data_2020_A, ct_data_2020_B)

# housing cost burden
ct2020 <- ct_data_2020 %>% 
  filter(STATE == "Kentucky",
         COUNTY == "Jefferson County") %>%
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
  filter(STATE == "Kentucky",
         COUNTY == "Jefferson County") %>%
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
  select(-starts_with("ATE")) %>%
  select(-YEAR:-NAME_E, -NAME_M) %>%
  filter(renters > 0) %>% 
  select(-renters) %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  pivot_longer(cols = c(-GISJOIN,-GEOID))

# % renters by FMI levels
rent_fmi20_ct  <- rent_income20_ct %>% 
  group_by(GISJOIN) %>%
  summarise(all_renters_ct = sum(value),
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
  summarise(across(where(is.double), ~sum(.x, na.rm = TRUE)))

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

# Risk assessment ---------------------------------------------------------

risk_class <- bg_ct_data %>% 
  mutate(low_N_vulnerable = if_else(renters_20 < 300 &
                                      (median_hhinc_10 < 47000), 1, 0)) %>% 
  filter(low_N_vulnerable == 1|
           renters_20 > 300 &
           (median_hhinc_10 < 47000)
  ) %>%
  mutate(m1a = if_else(rank_rents > 79|rank_rents2 > 79, 1, 0,
                       missing = 0),
         m1b = if_else(rank_HV > 79 & owner_20_ct > 500, 1, 0,
                       missing = 0),
         m1 = if_else(m1a+m1b > 0 & pop_change10_20 > 0, 1, 0),
         m2a = if_else(rank_college > 79,1,0),
         m2b = if_else(rank_hhinc > 79,1,0),
         m2c = if_else(rank_hi_inc > 79,1,0),
         m2d = if_else(rank_lo_inc < 21,1,0),
         m2e = if_else(black_change10_20 < -200 & 
                         blackpct_ch_00_20 < -.2, 1,0),
         m2f = if_else(pop_change10_20 > 0, 1, 0),
         m2 = if_else(m2a+m2b+m2c+m2d+m2e > 1 &  m2f == 1 &
                        (median_hhinc_20 > 66000|median_rent_20 >1100), 
                      1, 0, missing = 0)) %>% 
  mutate(h1a = if_else((rank_rents > 49 & rank_rents < 80)|
                         (rank_rents2 > 49 & rank_rents2 < 80), 1, 0,
                       missing = 0),
         h1b = if_else(rank_HV > 49 & rank_HV < 80 & owner_20_ct > 500, 1, 0,
                       missing = 0),
         h1 = if_else(h1a+h1b > 0, 1, 0),
         h2a = if_else(rank_college > 59,1,0),
         h2b = if_else(rank_hhinc > 59,1,0),
         h2c = if_else(rank_hi_inc > 59,1,0),
         h2d = if_else(rank_lo_inc < 41,1,0),
         h2e = if_else(black_change10_20 < 0 & 
                         blackpct_ch_00_20 < -.2, 1,0),
         h2f = if_else(pop_change10_20 > 0, 1, 0),
         h2 = if_else(h2a+h2b+h2c+h2d+h2e > 1 & #h2f == 1 &
                        median_hhinc_20 < 66000 &
                        median_rent_20 < 1100, 1, 0,
                      missing = 0),
         h3a = if_else(rank_housing_tight > 59|
                         rank_vacant_ch > 59|
                         rank_renter_p > 59|
                         rank_permits > 59, 1, 0,
                       missing = 0),
         h3b = if_else(rank_rents > 49|rank_rents2 > 49, 1, 0,
                       missing = 0),
         h3c = if_else(rank_HV > 49 & owner_20_ct > 500, 1, 0,
                       missing = 0),
         h3 = if_else(h3a > 0 & h1 == 1, 1, 0),) %>% 
  mutate(hi_all = if_else(h1 == 1 |h2 == 1 |h3 == 1, 1, 0),
         med_all = if_else((m1 == 1 | m2 == 1), 1, 0),
         risk_level = if_else(hi_all == 1, "high", "low"),
         risk_level = if_else(med_all == 1, "medium", risk_level),
         risk_level = factor(risk_level, levels = c("low", "medium", "high"))) %>% 
  select(GISJOIN_proj, hi_all, med_all, h1,h2,h3,m1,m2, risk_level)

bg_ct_risk <- left_join(bg_ct_data, risk_class) %>% 
  mutate(risk_level = if_else(is.na(risk_level) == TRUE, "low", risk_level))

rm(list=setdiff(ls(), c("bg2020", "bg_ct_data", "bg_ct_risk")))

write_csv(bg_ct_risk, here("DHNA", "data", "LVM_Risk_Database.csv"))
