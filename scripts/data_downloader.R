# DATA DOWNLOADER 

# Downloads all data required for the tool from NHGIS
# requires a IPUMS API key
# see https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums-api.html
# Run only once to download data, then use cached version

# LIBRARIES
library(here)
library(ipumsr)
library(tidyverse)
library(sf)


# NHGIS data --------------------------------------------------------------

# Block data from decennial census 

# set up folder structure for block data
dir.create(file.path(here("data", "nhgis", "block")), 
           recursive = TRUE)

# set download path to block folder
dl_path <- here("data", "nhgis", "block")

# 1990 block data with demographic data to crosswalk to 2020 block group
dir.create(file.path(here("data", "nhgis", "block", "bl1990")), 
           recursive = TRUE)
bl1990_spec <- ds_spec("1990_STF1",
                       data_tables = "NP10",
                       geog_levels = c("block"),
)
define_extract_nhgis(
  description = "block ethnoracial composition for Kentucky",
  datasets = bl1990_spec,
  geographic_extents = 210) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = paste0(dl_path, "/bl1990")) 

# 2000 block data with demographic data to crosswalk to 2020 block group
dir.create(file.path(here("data", "nhgis", "block", "bl2000")), 
           recursive = TRUE)
bl2000_spec <- ds_spec("2000_SF1b",
                       data_tables = "NP008A",
                       geog_levels = c("block"))
define_extract_nhgis(
  description = "2000 block ethnoracial composition for Kentucky",
  datasets = bl2000_spec,
  geographic_extents = 210) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = paste0(dl_path, "/bl2000")) 

# Block Group Data

# 2009-2013 block group data to crosswalk to 2020 block group
# Tables
# B03002: Hispanic or Latino by Race
# B15003: Educational attainment for people over 25 years old
# B25002: Housing status (occupied/vacant)
# B25063: Gross rent
# B25075: Home value
# B19001: Household income
dir.create(file.path(here("data", "nhgis", "blockgroup", "bg2010")), 
           recursive = TRUE)
dl_path <- here("data", "nhgis", "blockgroup")
bg2010_spec <- ds_spec("2009_2013_ACS5a",
                       data_tables = c("B03002",
                                       "B15003",
                                       "B25002",
                                       "B25063",
                                       "B25075",
                                       "B19001"),
                       geog_levels = c("blck_grp"))
define_extract_nhgis(
  description = "2010 socioeconomic data for Kentucky",
  datasets = bg2010_spec,
  geographic_extents = 210) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = paste0(dl_path, "/bg2010"))

# 2019-2023 block group data 
# Tables
# B03002: Hispanic or Latino by Race
# B15003: Educational attainment for people over 25 years old
# B25002: Housing status (occupied/vacant)
# B25063: Median Gross rent
# B25075: Home value
# B19001: Household income
dir.create(file.path(here("data", "nhgis", "blockgroup", "bg2020")), 
           recursive = TRUE)
bg2020_spec <- ds_spec("2019_2023_ACS5a",
                       data_tables = c("B03002",
                                       "B15003",
                                       "B25002",
                                       "B25063",
                                       "B25075",
                                       "B19001"),
                       geog_levels = c("blck_grp"))
define_extract_nhgis(
  description = "2020 socioeconomic data for Kentucky",
  datasets = bg2020_spec,
  geographic_extents = 210) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = paste0(dl_path, "/bg2020")) 

# Tract data
# 2019-2023 tract data
# Tables
# B25118: Tenure by Household Income in the Past 12 Months
# B25140: Housing Costs as a Percentage of Household Income in the Past 12 Months
# B01001: Sex by Age
# B11005: Households by Presence of People Under 18 Years by Household Type
# C16002: Household Language by Household Limited English Speaking Status
# C18130: Age by Disability Status by Poverty Status

dir.create(file.path(here("data", "nhgis", "tract", "ct2020")), 
           recursive = TRUE)
dl_path <- here("data", "nhgis", "tract")
ct2020_spec_A <- ds_spec("2019_2023_ACS5a",
                       data_tables = c("B25140",
                                       "B01001",
                                       "B11005",
                                       "C16002"),
                       geog_levels = c("tract"))

ct2020_spec_B <- ds_spec("2019_2023_ACS5b",
                         data_tables = c("B25118",
                                         "C18130"),
                         geog_levels = c("tract"))

define_extract_nhgis(
  description = "2020 tract socioeconomic data for Kentucky part A",
  datasets = ct2020_spec_A,
  geographic_extents = 210) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = paste0(dl_path, "/ct2020")) 

define_extract_nhgis(
  description = "2020 tract socioeconomic data for Kentucky",
  datasets = ct2020_spec_B,
  geographic_extents = 210) %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = paste0(dl_path, "/ct2020"))


# PUMS data ---------------------------------------------------------------

dir.create(file.path(here("data", "pums_usa", "acs_21_23")), 
           recursive = TRUE)
dl_path <- here("data", "pums_usa", "acs_21_23")

pums_vars <- list(var_spec("STATEFIP", case_selections = "21"), 
                  "PUMA","COUNTYFIP", "NUMPREC","HHINCOME", 
                  "OWNERSHP", "RENTGRS")

pums_extract <- define_extract_micro(
  collection = "usa",
  description = "2021-2023 KY ACS data",
  samples = c("us2021a", "us2022a", "us2023a"),
  variables = pums_vars,
  data_structure = "household_only") %>% 
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = dl_path) 


# crosswalk ---------------------------------------------------------------
dir.create(file.path(here("data", "nhgis", "crosswalks")), 
           recursive = TRUE)
dl_path <- here("data", "nhgis", "crosswalks")
download_supplemental_data(
  "nhgis",
  "crosswalks/nhgis_blk2000_bg2010_state/nhgis_blk2000_bg2010_21.zip",
  dl_path,
  overwrite = TRUE
)
download_supplemental_data(
  "nhgis",
  "crosswalks/nhgis_blk1990_bg2010_state/nhgis_blk1990_bg2010_21.zip",
  dl_path,
  overwrite = TRUE
)
download_supplemental_data(
  "nhgis",
  "crosswalks/nhgis_bg2010_bg2020_state/nhgis_bg2010_bg2020_21.zip",
  dl_path,
  overwrite = TRUE
)

# GIS layers --------------------------------------------------------------
# 2023 block group geography
dir.create(file.path(here("data", "nhgis", "gis", "blockgroup", "bg2023")), 
           recursive = TRUE)
dl_path <- here("data", "nhgis", "gis", "blockgroup")

define_extract_nhgis(
  description = "2023 Block group shapefiles request",
  shapefiles = "210_blck_grp_2023_tl2023") %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = paste0(dl_path, "/bg2023"))

nhgis_2023_bg_gis <- list.files(here("data", "nhgis", "gis", "blockgroup", "bg2023"),
                            full.names = TRUE)
lvm_bg <- read_ipums_sf(nhgis_2023_bg_gis) %>% 
  filter(STATEFP == "21", COUNTYFP == "111") %>% 
  st_transform(ky_shp, crs=4326)
st_write(lvm_bg, here("data", "nhgis", "gis", "blockgroup", "bg2023", "KY_Jefferson_BG_2023.shp"))

# 2020 block group population center
dir.create(file.path(here("data", "nhgis", "gis", "blockgroup", "bgc2020")), 
           recursive = TRUE)

bgc2020 <- define_extract_nhgis(
  description = "2020 block group population center for Kentucky",
  shapefiles = "us_blck_grp_cenpop_2020_cenpop2020") %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = paste0(dl_path, "/bgc2020")) 

nhgis_2020_bgc_gis <- list.files(here("data", "nhgis", "gis", "blockgroup", "bgc2020"),
                                full.names = TRUE)
lvm_bgc <- read_ipums_sf(nhgis_2020_bgc_gis) %>% 
  filter(STATEFP == "21", COUNTYFP == "111") %>% 
  st_transform(ky_shp, crs=4326)
st_write(lvm_bgc, here("data", "nhgis", "gis", "blockgroup", "bgc2020", "KY_Jefferson_BGC_2020.shp"))

# 2020 Census Tract geography
dl_path <- here("data", "gis", "tract")
define_extract_nhgis(
  description = "2020 Census Tract shapefiles request",
  shapefiles = "us_tract_2023_tl2023") %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = paste0(dl_path, "/ct2023"))
# subset to Louisville only
lvm_ct <- read_ipums_sf(here("data", "gis", "tract", "ct2023", "nhgis0271_shape.zip")) %>% 
  filter(STATEFP == "21", COUNTYFP == "111") %>% 
  st_transform(ky_shp, crs=4326)
st_write(lvm_ct, here("data", "gis", "tract", "ct2023", "KY_Jefferson_tract_2023.shp"))


