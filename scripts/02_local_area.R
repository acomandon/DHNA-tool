# Stage 02 — Local area and ethnoracial change
# Build BG/tract geometry layers, define the local-area unit, compute
# ethnoracial composition over time for both the local area and the
# comprehensive-plan market area, and write the app's ethnoracial CSV
# and the market-areas shapefile.
#
# Depends on: bg2020, bg90_20, bg00_20, bg10_20 from stage 01.
# Produces: bg_pop20, ct_pop20, lvm_bg_geo, lvm_bg_popctr_geo, local_area,
#   pop_ethnorace, pop_s, Jefferson_ma, Jefferson_bg_ma, pop_ma_s.
# Writes: DHNA/data/pop_ethnorace.csv, DHNA/data/gis/Comp_Plan_Market_Areas.shp.

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
  st_transform(crs=4326) %>%
  left_join(., bg_pop20) %>%
  filter(pop > 0) %>%
  select(GISJOIN, GEOID) %>%
  rename(GISJOIN_BG = GISJOIN,
         GEOID_BG = GEOID)

lvm_bg_popctr_geo <- st_read(here("data", "nhgis", "gis", "blockgroup", "bgc2020", "KY_Jefferson_BGC_2020.shp")) %>%
  st_transform(crs=4326) %>%
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
                      dist = params$local_area_buffer_m) %>%
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
# write to tool data folder for use within the tool
# Use a short DBF-safe column name (sf abbreviates anything longer than ~7 chars).
st_write(Jefferson_ma %>% rename(mkt_area = market_area),
         here("DHNA", "data", "gis", "Comp_Plan_Market_Areas.shp"),
         append = FALSE)
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
  rename(GISJOIN_proj = GISJOIN) %>%
  write_csv(., here("DHNA", "data", "pop_ethnorace.csv"))
