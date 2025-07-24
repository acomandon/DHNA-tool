#
# This is a Shiny web application 
# for the Displacement and Housing Need Assessment Tool. 
# This is a public and development version of the tool. 
# Contributors welcomed
#
# You can run the application by clicking the 'Run App' button above.
#

# Frontend loading

# libraries 
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
library(flextable)
library(officer)
library(bsicons)


# Fixed values
# Inflation between July 2013 and July 2023 for ACS data (from BLS)
CPI_13_23 = 1.3086
# Color scheme
colors <- hcl.colors(4, "Roma")
colors_axis <- c(rev(hcl.colors(4, "Roma")), rev(hcl.colors(4, "Roma")))

# Variables
# set clicks variable to receive location
clicks <- data.frame(lat = NA, lng = NA, .nonce = 0.432713)

# Load data  
# Create all required data by running the data_prep script first
pop <- read_csv("./data/pop_ethnorace.csv")
pop_map <- read_csv("./data/pop_ethnorace.csv") %>% 
  filter(data_yr == 2020) %>% 
  select(GISJOIN_BG, pop) %>% 
  rename(GISJOIN = GISJOIN_BG)
rent_buffer <- read_csv("./data/rent_buffer.csv")
hh_micro <- read_csv("./data/hh_micro.csv")
dhna_data <- read_csv("./data/LVM_Risk_Database.csv") 

# load census geography
lvm_bg_geo <- st_read("./data/gis/KY_Jefferson_BG_2023.shp") %>% 
  filter(COUNTYFP == "111") %>% 
  st_transform(lvm_bg_geo, crs=4326) %>% 
  left_join(., pop_map) %>% 
  filter(pop > 0)
lvm_ct_geo <- st_read("./data/gis/KY_Jefferson_tract_2023.shp")
st_transform(lvm_ct_geo, crs=4326)
lvm_ma_geo <-  st_read("./data/gis/Comp_Plan_Market_Areas.shp") %>% 
  st_transform(lvm_ct_geo, crs=4326)

# Parameters for the background image on landing page
backgroundImageCSS <- "height: 95vh;
                       background-position: center;
                       background-repeat: no-repeat;
                       /* background-size: cover; */
                       background-image: url(background.jpg);
                       "


# front load graphic for housing mismatch
p <- hh_micro %>% 
  mutate(HHINC_levels = factor(HHINC_levels, levels = c("Above 80%",
                                                        "50% to 80% ($64,625)",
                                                        "30% to 50% ($40,400)",
                                                        "Below 30%  ($27,125)")),
         HHINC_levels = fct_relabel(HHINC_levels, function(x) 
           str_wrap(x, 10)),
         RENT_levels = factor(RENT_levels, levels = c("Above 80%  (market)",
                                                      "50% to 80% ($1615)",
                                                      "30% to 50% ($1010)",
                                                      "Below 30%  ($678)")),
         RENT_levels = fct_relabel(RENT_levels, function(x)
           str_wrap(x, 10))) %>%
  group_by(HHINC_levels, RENT_levels) %>% 
  summarise(renters = round(sum(HHWT, na.rm = T)/2),0) %>% 
  drop_na() %>% 
  ungroup() %>% 
  ggplot(aes(axis1 = HHINC_levels, axis2 = RENT_levels, y = renters)) +
  geom_alluvium(aes(fill = HHINC_levels),knot.pos = 1/4, width = 1/30) +
  geom_stratum(width = 1/30, color = "white", fill = colors_axis) +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 1, as.character(after_stat(stratum)), "")),
    stat = "stratum", size = 5, direction = "y", nudge_x = -.09,segment.color = 'transparent') +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x)  == 2, as.character(after_stat(stratum)), "")),
    stat = "stratum", size = 5, direction = "y", nudge_x = .09,segment.color = 'transparent'
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


# UI ----------------------------------------------------------------------

ui <- page_navbar(
  # Application title
  title = "ANTI-DISPLACEMENT ASSESSMENT TOOL",
  id = "ADI_tabs",
  useShinyjs(),
  nav_panel("Introduction", value = "intro",
            tags$head(
              tags$script(
                HTML('$(document).ready(function() {
                       $(".navbar .container-fluid")
                         .append("<img id = \'myImage\' src=\'Louisville_logo.png\' align=\'right\' height = \'55px\'>"  );
                      });')),
              tags$style(
                HTML('@media (max-width:992px) { #myImage { position: fixed; right: 1%; top: 0.5%; }
                                           div.navbar-header > button {margin-right: 75px}}')
              )),
            style = backgroundImageCSS,
            fluidRow(
              column(6, align = "left",
                     p("Welcome!", style = "margin-top:50px;
                                           font-size:36px"),
                     p("The Anti-Displacement Assessment Tool (ADA Tool) –
                                      coproduced with the Office of Housing and Community Development –
                                      provides an easy way to assess how a new housing project would fit into existing neighborhoods.",
                       style = "margin-top:50px;
                                        font-size:22px;font-family:arial"),
                     p("The ", tags$a(href="https://codelibrary.amlegal.com/codes/louisvillemetro/latest/loukymetro/0-0-0-71955", "Anti-Displacement Ordinance"), "passed by Louisville Metro City Council
                                        on 9 November 2023 requires that new residential developments in the region
                                        that utilize Louisville Metro subsidies be evaluated for their potential
                                        impact to displace existing residents.",
                       style = "margin-top:50px;
                                        font-size:22px;font-family:arial"),
                     p("The ADA Tool concludes with a Results Matrix, which determines the eligibility of 
                                        projects for Louisville Metro subsidies.",
                       style = "margin-top:50px;
                                           font-size:22px;font-family:arial")),
              column(6, align = "right",
                     h2("The tool works in 4 steps:"),
                     value_box(title = "Housing Need",
                               value = "Learn about why Louisville needs more affordable housing",
                               showcase = bs_icon("1-square"),
                               theme = value_box_theme(bg = "#e6f2fd")
                     ),
                     value_box(title = "Location",
                               value = "Enter information about the size and location of the project",
                               showcase = bs_icon("2-square"),
                               theme = value_box_theme(bg = "#b7d5da")
                     ),
                     value_box(title = "Affordability levels",
                               value = "Enter how many units are at each affordability level and receive recommendations",
                               showcase = bs_icon("3-square"),
                               theme = value_box_theme(bg = "#a1b4b7"),
                               p("You must enter a valid location first")
                     ),
                     value_box(title = "Area overview",
                               value = "Learn about the data behind the recommendations", 
                               showcase = bs_icon("4-square"),
                               theme = value_box_theme(bg = "#879c9f"),
                               p("You must enter a valid location first")
                     ))
            )
  ),
  nav_panel("Housing Need", value = "HNeed",
            tags$head(
              tags$style(HTML(".card, .card-body {
              height: 800 !important;
              max-height: none !important;
              overflow-y: visible !important;
              }
                              "))
            ),
            layout_columns(
              card(height = 400, 
                   card_header("What is AMI?"),
                   HTML('<iframe width="560" height="100%" src="https://www.youtube.com/embed/HeTLFepwn4o?si=bJ1yollvzJcgUvWx" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>')
              ),
              card(height = 400, 
                   card_body(
                     p("This tool relies on the Area Median Income (AMI) to determine affordability levels. The Department of Housing and
                                 Urban Development (HUD) updates the AMI for metropolitan areas every year and based on a household size. The tool uses
                                 a simplified version based on a household between 3 and 4 persons."),
                     p("For more information on AMI and housing need in Louisville, please consult the ", 
                       tags$a(href= "https://louisvilleky.gov/government/housing/housing-needs-assessment", "Housing Need Assessment"))
                   )
              ),
              col_widths = c(6,6)),   
            layout_columns(
              card(height = 400,
                   card_header("Rent Burden"),
                   card_body(class = "align-items-center",
                             plotOutput("rent_burden", width = "100%",
                                        height = "350px"))
              ),
              card(
                p("Many people struggle to pay their rent.
                                    Among people with the lowest income (here defined as below 30% of AMI),
                                    75% pay over half of their income toward rent. This means that paying for
                                    everything else, like healthcare, food, and transportation, is extremely
                                    challenging")
              ),
              col_widths = c(6,6)),
            layout_columns(
              card(height = 400, 
                   card_header("Housing Mismatch"),
                   card_body(class = "align-items-center",
                             plotOutput("housing_mismatch", width = "100%", 
                                        height = "300px"))
              ),
              card(p("The lack of housing affordable to all income levels is due to the shortage of
                                    units affordable to people with income below the metropolitan norm. In this figure,
                                    the left side is the distribution of renters by income level. Hover the pointer over a
                                    rectangle to see how many renter households are in each category."),
                   p("On the right side is the distribution of units affordable to each income level. There is only
                                        one unit affordable to households making 30% of the typical income for every two households
                                        in this category"),
                   p("This imbalance makes it challenging to find affordable housing. The imbalance at higher levels of
                                          affordability worsens this issue. About half of the most afforable units are rented by households
                                      who could afford to pay more. Only one in four household in the lowest income category rents at a level
                                      that is affordable.")
              ),
              col_widths = c(7,5))
            
  ),
  nav_panel("Location", value = "location",
            p("Project can have different impacts based on where they are located.
                      Please, enter information about where the project will be located."),
            numericInput("proj_size", "Number of units in proposed project", 
                         value = 0),
            conditionalPanel(
              condition = "input.proj_size > 0",
              selectInput("build_type", "Housing type",
                          c("Single-Family Home" = "sfh", 
                            "Multi-Family Home" = "mfh"),
                          selected = 'mfh')
            ),
            conditionalPanel(
              condition = "input.build_type == 'sfh' & input.proj_size > 1",
              selectInput("scattered_sites",
                          "Will the homes be scattered across multiple locations",
                          c("Yes, each home is in a distinct location" = "yes",
                            "No, the homes are all within the same block" = "no"),
                          selected = "no")
            ),
            htmlOutput("Scattered_site_response"),
            
            p("Please, provide the location of your project with an address
                      or directly on the map"),
            textInput("address", "Address:"),
            textInput("city", "City:"),
            textInput("zip", "Zip:"),
            actionButton("geocode", "Geocode address"),
            leafletOutput("map"),
            actionButton("use_clik_loc", "Confirm location"),
            textOutput("valid_loc_text"),
            conditionalPanel(condition = "output.valid_loc_text == 'Your selected location and project information have been recorded'&&
            input.proj_size > 0",
                             actionButton("valid_loc_btn", "next page")) 
  )
)




