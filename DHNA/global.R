# global.R — sourced once at app startup (Shiny auto-loads it for ui.R/server.R apps).
#
# Holds everything shared across the UI and server: libraries, config, the
# app's pure helpers, the data/geography loaded from DHNA/data, theming
# constants, and the prebuilt housing-mismatch plot.
#
# Convention used by the modules in R/:
#   - data frames and reactive selection state are passed in as arguments
#   - theming constants (colors*) and config (locality) are referenced
#     directly as globals.

# Libraries -----------------------------------------------------------------
library(shiny)
library(shinyjs)
library(leaflet)
library(tidyverse)
library(tidygeocoder)
library(sf)
library(ggalluvial)
library(htmltools)
library(bslib)
library(ggrepel)
library(bsicons)

# Config, shared pure helpers, and app modules ------------------------------
source("../R/config.R")
source("../R/project_recommender.R")

source("R/mod_intro.R")
source("R/mod_housing_need.R")
source("R/mod_location.R")
source("R/mod_affordability_rental.R")
source("R/mod_affordability_ownership.R")
source("R/mod_area_overview.R")

# Map click container (initial empty location) ------------------------------
clicks <- data.frame(lat = NA, lng = NA)

# Data ----------------------------------------------------------------------
pop <- read_csv("./data/pop_ethnorace.csv")
pop_map <- pop %>%
  filter(data_yr == 2020) %>%
  select(GISJOIN_proj, pop)
local_area <- read_csv("./data/local_area.csv")
rent_buffer <- read_csv("./data/Renthub_quarterly_rent.csv")
hh_micro <- read_csv("./data/hh_micro.csv")
adat_data <- read_csv("./data/LVM_Risk_Database.csv")

# Geography -----------------------------------------------------------------
lvm_bg_geo <- st_read("./data/gis/KY_Jefferson_BG_2023.shp") %>%
  filter(COUNTYFP == locality$county_fips) %>%
  st_transform(crs = 4326) %>%
  left_join(., pop_map, by = join_by(GISJOIN == GISJOIN_proj)) %>%
  filter(pop > 0)
lvm_ct_geo <- st_read("./data/gis/KY_Jefferson_tract_2023.shp") %>%
  st_transform(crs = 4326)
lvm_ma_geo <- st_read("./data/gis/Comp_Plan_Market_Areas.shp") %>%
  st_transform(crs = 4326)

# Theming -------------------------------------------------------------------
backgroundImageCSS <- "height: 95vh;
                       background-position: center;
                       background-repeat: no-repeat;
                       /* background-size: cover; */
                       background-image: url(background.jpg);
                       "
colors <- hcl.colors(4, "Vik")
colors_3 <- c("#C6AA86", "#91B2D2", "#002E60")
colors_er <- c("#002E60", "#91B2D2", "#C6AA86")
colors_axis <- c(rev(hcl.colors(4, "Vik")), rev(hcl.colors(4, "Vik")))

# Prebuilt housing-mismatch alluvial plot (built once, shared by all sessions)
housing_mismatch_plot <- hh_micro %>%
  mutate(HHINC_levels = factor(HHINC_levels, levels = c("Above 80%",
                                                        "50% to 80% ($73,250)",
                                                        "30% to 50% ($45,800)",
                                                        "Below 30%  ($27,475)")),
         HHINC_levels = fct_relabel(HHINC_levels, function(x)
           str_wrap(x, 10)),
         RENT_levels = factor(RENT_levels, levels = c("Above 80%  (market)",
                                                      "50% to 80% ($1831)",
                                                      "30% to 50% ($1145)",
                                                      "Below 30%  ($687)")),
         RENT_levels = fct_relabel(RENT_levels, function(x)
           str_wrap(x, 10))) %>%
  group_by(HHINC_levels, RENT_levels) %>%
  summarise(renters = round(sum(HHWT, na.rm = T) / 2)) %>%
  drop_na() %>%
  ungroup() %>%
  ggplot(aes(axis1 = HHINC_levels, axis2 = RENT_levels, y = renters)) +
  geom_alluvium(aes(fill = HHINC_levels), knot.pos = 1/4, width = 1/30) +
  geom_stratum(width = 1/30, color = "white", fill = colors_axis) +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 1, as.character(after_stat(stratum)), "")),
    stat = "stratum", size = 5, direction = "y", nudge_x = -.09, segment.color = 'transparent') +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 2, as.character(after_stat(stratum)), "")),
    stat = "stratum", size = 5, direction = "y", nudge_x = .09, segment.color = 'transparent'
  ) +
  scale_x_discrete(limits = c("HHINC_levels", "RENT_levels"),
                   expand = c(0.15, 0.05),
                   labels = c("Income Level", "Rent Level")) +
  scale_fill_manual(values = colors) +
  scale_y_continuous(breaks = NULL) +
  ylab("") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 14))
