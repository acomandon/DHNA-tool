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
# Create container for location on map
clicks <- data.frame(lat = NA, lng = NA, .nonce = 0.432713)


# Load required data for the aoo
# data  
pop <- read_csv("./data/pop_ethnorace.csv")
pop_map <- read_csv("./data/pop_ethnorace.csv") %>% 
  filter(data_yr == 2020) %>% 
  select(GISJOIN_proj, pop)
local_area <- read_csv("./data/local_area.csv")
rent_buffer <- read_csv("./data/Renthub_quarterly_rent.csv")
hh_micro <- read_csv("./data/hh_micro.csv")
adat_data <- read_csv("./data/LVM_Risk_Database.csv") 

# geography
lvm_bg_geo <- st_read("./data/gis/KY_blck_grp_2022.shp") %>% 
  filter(COUNTYFP == "111") %>% 
  st_transform(lvm_bg_geo, crs=4326) %>% 
  left_join(., pop_map, by = join_by(GISJOIN == GISJOIN_proj)) %>% 
  filter(pop > 0)
lvm_ct_geo <- st_read("./data/gis/KY_Jefferson_tract_2022.shp")
st_transform(lvm_ct_geo, crs=4326)
lvm_ma_geo <-  st_read("./data/gis/Comp_Plan_Market_Areas.shp") %>% 
  st_transform(lvm_ct_geo, crs=4326)

# Load image for landing page
backgroundImageCSS <- "height: 95vh;
                       background-position: center;
                       background-repeat: no-repeat;
                       /* background-size: cover; */
                       background-image: url(background.jpg);
                       "
# Define colors for plots
colors <- hcl.colors(4, "Vik")
colors_3 <- c("#C6AA86", "#91B2D2","#002E60")
colors_er <- c("#002E60", "#91B2D2","#C6AA86")
colors_axis <- c(rev(hcl.colors(4, "Vik")), rev(hcl.colors(4, "Vik")))

# front load flow chart for housing mismatch
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
                                           font-size:22px;font-family:arial"),
                     tags$p(
                       "For more information about the tool and the methodology, 
                       please see the",
                       tags$a(href = "User_Guide_V1.pdf", target = "_blank", "user guide"),
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

# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  
  observeEvent(input$valid_loc_btn, {
    removeTab(inputId = "ADI_tabs", target = "form")
  })
  observeEvent(input$valid_loc_btn, {
    removeTab(inputId = "ADI_tabs", target = "explainer")
  })
  
  observeEvent(input$valid_loc_btn,{
    nav_insert(
      "ADI_tabs",
      target = "location",
      select = TRUE,
      position = "after",
      nav_panel("Affordability levels", value = "form",
                layout_columns(
                  fill = TRUE,
                  col_widths = 12,
                  card(full_screen = FALSE,
                       card_header("Project details"),
                       
                       # Sidebar ----
                       layout_sidebar(
                         open = TRUE, # Open sidebar
                         fillable = TRUE,
                         
                         # Side bar
                         sidebar = sidebar(
                           width = 400, # sidebar width
                           
                           # Input file 
                           p("The affordability of housing affects local rents and who
                             can move into the neighborhood. Enter the number of units
                             at each level of AMI. If the project is a single-family home,
                             enter '1' in the appropriate box."),
                           
                           #numericInput("median_rent", "Project's median rent in $", value = 0),
                           numericInput("affordable30", "Number of units reserved for 30% AMI or below", value = 0),
                           numericInput("affordable50", "Number of units reserved for 50% AMI or below", value = 0),
                           numericInput("affordable60", "Number of units reserved for 60% AMI or below", value = 0),
                           numericInput("affordable70", "Number of units reserved for 70% AMI or below", value = 0),
                           numericInput("affordable80", "Number of units reserved for 80% AMI or below", value = 0),
                           actionButton("submit", "Submit")
                         ),
                         
                         # Main content ----
                         uiOutput("risk_assessment_res"),
                         uiOutput("recommendation")
                         
                       ))
                )
      )
    )
  })
  
  observeEvent(input$next_sec,{
    nav_insert(
      "ADI_tabs",
      target = "form",
      select = TRUE,
      position = "after",
      nav_panel("Area overview", value = "explainer",
                layout_columns(
                  card(p("The neighborhood contex is crucial for 
                                    understanding displacement risk. This dashboard 
                                    shows the data used to determine displacement risk
                                    for the local area where the project is located.",
                         p("The components of displacement risk are ranked between 
                         0 and 100. This means that, for example, the local area with the 
                         highest rate of college educated adults will rank 100, and
                           the lowest will rank 0. Higher ranks (above 60) are associated
                           with higher risk. When an area was in a position of 
                           vulnerability to displacement in 2010 and 
                           multiple indicators have high ranks, it is classified 
                           as either medium or high risk")))
                ),
                layout_columns(
                  card(card_header("Local area map"),
                       card_body(leafletOutput("neighborhood_map")),
                       card_body(p("The map shows the block group where the
                                   project is located in dark gray and
                                   the local area from which the data is drawn
                                   in light gray."))),
                  card(card_header("Change in racial composition"),
                       fluidRow(
                         column(width = 6, plotOutput("ethrace_plot")),
                         column(width = 6, textOutput("ethrace_change")))
                  )),
                # card(card_header("Summary of population change"),
                #      uiOutput("ethrace_table"))),
                layout_columns(
                  card(card_header("Change in % of adults with college education"),
                       card_body(plotOutput("edu")),
                       card_body(textOutput("edu_change"))),
                  card(card_header("Change in median rent"),
                       card_body(plotOutput("rents")),
                       card_body(textOutput("rent_change"))),
                  card(card_header("Change in household income"),
                       card_body(plotOutput("hhinc")),
                       card_body(textOutput("income_change")))
                ),
                layout_columns(
                  card(card_header("Change in asking rent"),
                       fluidRow(
                         column(width = 6, plotOutput("rent_trend")),
                         column(width = 6, textOutput("market_rent_change")))
                  ))
      )
    )
  })
  
  output$housing_mismatch <- renderPlot(
    p
  ) 
  
  output$rent_burden <- renderPlot({
    hh_micro %>% 
      group_by(HHINC_levels, rent_burden) %>% 
      summarise(renters = sum(HHWT, na.rm = T)) %>% 
      mutate(HHINC_levels = factor(HHINC_levels, levels = c("Below 30%  ($27,125)",
                                                            "30% to 50% ($40,400)",
                                                            "50% to 80% ($64,625)",
                                                            "Above 80%")),
             HHINC_levels = fct_relabel(HHINC_levels, function(x) 
               str_wrap(x, 10)), 
             rent_burden = factor(rent_burden, levels = c("No burden",
                                                          "Burdened",
                                                          "Severely burdened"))) %>% 
      drop_na() %>% 
      ggplot(aes(fill = rent_burden, y = renters, x = HHINC_levels)) +
      geom_bar(position = "fill", stat = "identity", alpha = 0.75) +
      scale_fill_manual(values =  colors_3) +
      scale_y_continuous("% of renters", 
                         breaks = c(0.25,0.50,0.75,1.00), 
                         labels = c("25","50","75","100")) +
      labs(x = "Income level") +
      theme_minimal() + 
      guides(fill=guide_legend(title="Rent burden")) +
      theme(text = element_text(size = 16))
  })  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -85.755, 
              lat = 38.252,
              zoom = 12) %>% 
      addProviderTiles("Esri.WorldGrayCanvas")
  })
  
  pr_location1 <- reactiveVal(clicks)
  
  observeEvent(input$geocode, {
    address_single <- tibble(singlelineaddress = paste(input$address, ", ",
                                                       input$city, ", ",
                                                       "KY, ",
                                                       input$zip))
    
    census_full <- address_single %>% geocode(
      address = singlelineaddress,
      method = "census")
    
    y <- census_full$lat
    print(y)
    x <- census_full$long
    print(x)
    
    leafletProxy("map", data = census_full) %>%
      setView(x, y, zoom = 14) %>% 
      clearShapes() %>% 
      clearControls() %>% 
      addCircles(~x, ~y, stroke = FALSE, fill = TRUE, 
                 fillOpacity = .8, color = "#777777", radius = 20)
    
    pr_location1(mutate(pr_location1(), lat = y, lng = x))
    print(pr_location1)
  })
  
  
  observeEvent(input$map_click, {
    click_loc = input$map_click
    leafletProxy('map') %>%
      clearShapes() %>% 
      addCircles(lng = click_loc$lng, lat = click_loc$lat, color = "#777777",
                 fillOpacity = .8, radius = 20)
    print(click_loc)
    pr_location1(mutate(pr_location1(), lat = click_loc$lat, lng = click_loc$lng))
    print(pr_location1)
  })
  
  
  bg_id <- reactive({
    if (is.na(pr_location1()$lat) == FALSE) {
      bg_coords <- pr_location1()
      lat <- as.numeric(bg_coords$lat)
      long <- as.numeric(bg_coords$lng)
      
      bg <- st_intersects(st_point(c(long, lat)), lvm_bg_geo)[[1]][1]
      print(bg)
      lvm_bg_geo$GISJOIN[bg]
    } else {
      NA
    }
  }) %>% 
    bindCache(pr_location1()) %>% 
    bindEvent(input$use_clik_loc)
  
  valid_loc <- eventReactive(input$use_clik_loc, {
    print(bg_id())
    if (is.na(bg_id()) == FALSE & input$proj_size > 0) {
      "Your selected location and project information have been recorded"
    }
    else if ((is.na(bg_id()) == FALSE & input$proj_size < 1) ) {
      "Please enter a number of units in the proposed project"
    }
    else {
      "Please enter a valid location"
    }
  })
  
  output$valid_loc_text <- renderText({
    valid_loc()
  })
  
  
  output$Scattered_site_response <- renderText({
    if (input$scattered_sites  == "yes" & input$build_type == "sfh") {
      paste("<br> <em>This tool can only process one location at a time. Please, contact the office of housing for scattered site developments.</em> <br>", "<br>", "<br>")
    }
    else if (input$scattered_sites  == "no" & input$build_type == "sfh") {
      paste("<br> <em>Please, enter the address of one of the homes and the tool will treat the project as a cluster of homes.</em> <br>", "<br>", "<br>")
    }
    else {
      ""
    }
  })
  
  
  local_area_res <- reactive({
    adat_data %>% filter(GISJOIN_proj == bg_id())
  })
  
  output$neighborhood_map <- renderLeaflet({
    
    bg_focus <- lvm_bg_geo %>% 
      filter(GISJOIN == bg_id())
    
    local_area_id <- local_area %>% 
      filter(GISJOIN_proj == bg_id())
    
    local_area_ct <- lvm_ct_geo %>% 
      filter(GISJOIN %in% local_area_id$GISJOIN_CT)
    
    bbox <- st_bbox(local_area_ct)
    xview = mean(c(bbox[1]+.02,bbox[3]))
    yview = mean(c(bbox[2],bbox[4]))
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = xview, 
              lat = yview,
              zoom = 13) %>% 
      #addProviderTiles("Esri.WorldGrayCanvas") %>% 
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = local_area_ct, color = 'black', fillOpacity = .2, weight = 2) %>% 
      addPolygons(data = bg_focus, color = "black", fillOpacity = .6, weight = 2) %>% 
      addPolygons(data = lvm_ma_geo, color = "darkred", fillOpacity = .01, weight = 2, label= ~Name) %>% 
      addMiniMap(
        tiles = providers$Esri.WorldGrayCanvas,
        position = 'topright', 
        width = 150, height = 150,
        toggleDisplay = FALSE,
        zoomLevelOffset = -4) 
    
  })
  
  
  output$ethrace_plot <- renderPlot({
    
    pop%>% 
      filter(GISJOIN_proj == bg_id()) %>% 
      mutate(p_black = round(pop_black/pop*100,1),
             p_white = round(pop_white/pop*100,1),
             p_latino = round(pop_latino/pop*100,1),
             Area = factor(Area)) %>% 
      select(GISJOIN_proj, data_yr, pop_black, pop_white, pop_latino, 
             p_black, p_white, p_latino, Area) %>% 
      ggplot() +
      geom_line(aes(x= data_yr, y = p_black, linetype =  Area, color = "Black"), linewidth = 1.1)+
      geom_line(aes(x= data_yr, y = p_latino, linetype =  Area, color = "Latino/Hispanic"), linewidth = 1.1)+
      geom_line(aes(x= data_yr, y = p_white, linetype =  Area, color = "White"), linewidth = 1.1) +
      labs(x = "Year", y = "% of population") +
      scale_color_manual(values = colors_er) +
      theme_bw() +
      theme(text = element_text(size = 16)) +
      guides(color=guide_legend("Group", nrow = 1),
             linetype=guide_legend(nrow=1)) +
      theme(legend.box = "vertical",
            legend.position="bottom") 
  })  
  
  output$ethrace_change <- renderText({
    paste("Rapid changes in demographic composition 
          are often associated with displacement because
          composition otherwise tends to change slowly.The tool looks at how rapidly
          the Black population, which particularly vulnerable to displacement, 
          changed from 2000 to 2020 as one of the factors. A decrease of more than 
          20 percentage point and loss of more than 200 Black people is an indicator
          of high displacement risk. 
          
          The share of Black people in this area changed by", 
          round(local_area_res()$blackpct_ch_00_20*100,1), "percentage point.
          and the Black population changed by", 
          (local_area_res()$black_change00_10 + local_area_res()$black_change10_20),
          "between 2000 and 2020")
  })
  
  # output$ethrace_table <- renderUI({
  #   eth_race <- pop %>% filter(GISJOIN_proj == bg_id())
  #   eth_race_f <- structure(list(Group = c("Population", "Population",
  #                                          "White", "White",
  #                                          "Black", "Black"),
  #                                Area = c("Surrounding", "Market Area",
  #                                         "Surrounding", "Market Area",
  #                                         "Surrounding", "Market Area"),
  #                                "Change 1990-2010" = c(eth_race$pop_change90_10_s,   
  #                                                       eth_race$pop_change90_10_ma,  
  #                                                       eth_race$white_change90_10_s, 
  #                                                       eth_race$white_change90_10_ma,
  #                                                       eth_race$black_change90_10_s, 
  #                                                       eth_race$black_change90_10_ma),
  #                                "Change 2010-2020" = c(eth_race$pop_change10_20_s,   
  #                                                       eth_race$pop_change10_20_ma,  
  #                                                       eth_race$white_change10_20_s, 
  #                                                       eth_race$white_change10_20_ma,
  #                                                       eth_race$black_change10_20_s, 
  #                                                       eth_race$black_change10_20_ma)),
  #                           row.names = c(
  #                             NA,
  #                             -6L
  #                           ), class = c("tbl_df", "tbl", "data.frame"))
  # 
  #   
  #   ft_1 <- flextable(eth_race_f)
  #   ft_2 <- merge_v(ft_1,
  #                   j = "Group")
  #   ft_2 <- fix_border_issues(ft_2)
  #   return(htmltools_value(ft_2))
  #   })
  
  output$edu <- renderPlot({
    adat_data %>% 
      select(GISJOIN_proj, adult_college_above_10, adult_over25_10,
             adult_college_above_20, adult_over25_20) %>% 
      filter(GISJOIN_proj == bg_id()) %>% 
      mutate(collegep10_s = round(adult_college_above_10/adult_over25_10*100,1),
             collegep20_s = round(adult_college_above_20/adult_over25_20*100,1)) %>% 
      select(GISJOIN_proj,
             collegep10_s,
             collegep20_s) %>% 
      pivot_longer(-GISJOIN_proj) %>% 
      mutate(Area = c("Local Area", "Local Area"),
             Year = c(2010,2020)) %>% 
      ggplot() +
      geom_bar(aes(y = value, x = Year, fill = Area), alpha = .7,
               width=6,
               position = position_dodge(6.5), 
               stat = "identity",
      ) +
      labs(y = "% with college education") +
      scale_x_continuous(name="Year", breaks = c(2010,2020)) +
      scale_fill_manual(values = c("#002E60")) +
      theme_bw() +
      theme(text = element_text(size = 14),
            legend.position = "none") 
    
    
  })  
  
  output$edu_change <- renderText({
    paste0("Rapid changes in the share of people with a college degree 
          is associated with displacement because it signals a change in the 
          market that favors rent increases.People with college degrees may not
          have higher income, especially recent graduates, but they tend to have
          high income growth potential, which can be matched by rent increases that
          people with a degree cannot afford. The tool ranks all areas based on 
          the increase in the share of adults with college degrees. 
          
          This local area ranks ", 
           local_area_res()$rank_college, "th within Louisville for how
          quickly the share of adults with collge education increased from
          2013 to 2023")
  })
  
  output$rents <- renderPlot({
    adat_data %>% 
      select(GISJOIN_proj, median_rent_10, median_rent_20) %>% 
      filter(GISJOIN_proj == bg_id()) %>%  
      pivot_longer(-GISJOIN_proj) %>% 
      mutate(Area = c("Local Area", "Local Area"),
             Year = c(2010,2020)) %>% 
      ggplot() +
      geom_bar(aes(y = value, x = Year, fill = Area), alpha = .7,
               width=6,
               position = position_dodge(6.5), 
               stat = "identity",
      ) +
      labs(y = "Median Rent") +
      scale_x_continuous(name="Year", breaks = c(2010,2020)) +
      scale_fill_manual(values = c("#002E60")) +
      theme_bw() +
      theme(text = element_text(size = 14),
            legend.position = "none")
    
  })  
  
  output$rent_change <- renderText({
    paste0("Rapid rent increases are a primary indicator that residents may be
    at risk of displacement. In the absence of income increases, higher rents
    first decreases the ability of renters to afford other necessities, then, 
    when the rent burden becomes too high, people may leave. The tool ranks 
    all areas based on the increase in gross rent and asking rent (see below). 
          
          This local area ranks ", 
           local_area_res()$rank_rents, "th within Louisville for how
          quickly rents increased from 2013 to 2023.")
  })
  
  output$hhinc <- renderPlot({
    adat_data %>% 
      select(GISJOIN_proj, median_hhinc_10, median_hhinc_20) %>% 
      filter(GISJOIN_proj == bg_id()) %>%  
      pivot_longer(-GISJOIN_proj) %>% 
      mutate(Area = c("Local Area", "Local Area"),
             Year = c(2010,2020)) %>% 
      ggplot() +
      geom_bar(aes(y = value, x = Year, fill = Area), alpha = .7,
               width=6,
               position = position_dodge(6.5), 
               stat = "identity",
      ) +
      labs(y = "Median household income") +
      scale_x_continuous(name="Year", breaks = c(2010,2020)) +
      scale_fill_manual(values = c("#002E60")) +
      theme_bw() +
      theme(text = element_text(size = 14),
            legend.position = "none")
    
  }) 
  
  output$income_change <- renderText({
    paste0("As noted in the rent section, if income increase as rapidly as rents,
    there is less of a risk that people will leave their home involuntarily.
    We know from research that rapid income change are more likely to be 
    associated with the entry of new higher income residents than because 
    existing residents' income growth. Therefore, rapid income increases tend to
    suggest a more acute risk of displacement. The tool ranks 
    all areas based on the increase in gross rent and asking rent (see below). 
          
          This local area ranks ", 
           local_area_res()$rank_hhinc, "th within Louisville for how
          quickly household income increased from 2013 to 2023.")
  })
  
  output$rent_trend <- renderPlot({
    rent_buffer %>% 
      filter(GISJOIN_proj == bg_id()) %>% 
      ggplot(aes(y = rent_bg, x = year_qu, group = GEOID_bg)) +
      geom_line(aes(y = rent_bg, x = year_qu, group = GEOID_bg, color = "Block Group", ), size = 1.2)+
      geom_line(aes(y = rent_ma, x = year_qu, group = GEOID_bg, color = "Market Area"), size = 1.2) +
      labs(y= "Rent") +
      scale_color_manual(name = "Quarterly rent level", values = c("Block Group" = "#002E60", "Market Area" = "#91B2D2")) +
      scale_x_continuous("Quarter - Year", breaks = c(17,24,32, 40), 
                         labels = c("Q1 - 2018","Q1 - 2020","Q1 - 2022","Q1 - 2024")) +
      theme_bw() +
      theme(text = element_text(size = 16))
    
  })
  
  output$market_rent_change <- renderText({
    if (!bg_id()  %in% rent_buffer$GISJOIN_proj) {
      paste("There is not enough data in this location to track rent over time.")
    }
    else {
      "Rent increases can be very local, one area where new investments have 
        caused rents to increase rapidly, for example. However, rents can also 
        increase in that area because rents are increasing everywhere.
        This chart shows how asking rents in the local area have changed compared
        to the broader market area where the project is located."
    }
  })  
  
  
  # output$rent_17_24 <- renderUI({
  #   qu_rent <- adat_data %>% filter(GISJOIN_proj == bg_id())
  #   
  #   sfh <- qu_rent$SFH
  #   sfhs <- qu_rent$SFH_sub
  #   mfh <- qu_rent$MFH
  #   mfhs <- qu_rent$MFH_sub
  #   rent17 <- qu_rent$rent_bg_Q3_2017
  #   rent24 <- qu_rent$rent_bg_Q3_2024
  #   Ch_17_24 <- round((rent24-rent17)/rent17*100,0)
  #   
  #   data <- structure(list(
  #     column1 = c("Building type",
  #                 "Single Family",
  #                 sfh,
  #                 "Rent",
  #                 "2017",
  #                 rent17),
  #     column2 = c("Building type",
  #                 "Subsidized Single Family",
  #                 sfhs,
  #                 "Rent",
  #                 "2017",
  #                 rent17),
  #     column3 = c("Building type",
  #                 "Multifamily",
  #                 mfh,
  #                 "Rent",
  #                 "2024",
  #                 rent24),
  #     column4 = c("Building type",
  #                 "Subsidized Multifamily",
  #                 mfhs,
  #                 "Rent",
  #                 "Change 2017-2024",
  #                 paste0(Ch_17_24,"%"))),
  #     row.names = c(NA,
  #                   -6L),
  #     class = c("tbl_df", "tbl", "data.frame"))
  #   
  #   
  #   
  #   
  #   # Create the flextable
  #   ft <- flextable(data)
  #   ft_1 <- ft %>% 
  #     merge_h(i = 1) %>% 
  #     merge_h(i = 4) %>% 
  #     merge_h_range(i = 5, j1 = "column1", j2 = "column2") %>% 
  #     merge_h_range(i = 6, j1 = "column1", j2 = "column2") %>% 
  #     align(align = "center") %>% 
  #     delete_part(part = "header") %>% 
  #     hline_top(part = "body", border= fp_border(color = "Black",
  #                                                width = 2)) %>%  
  #     hline(i = c(1,3,4)) 
  #   
  #   return(htmltools_value(ft_1))
  # })
  # 
  
  # Matrix section ----
  
  output$risk_assessment_res <- renderUI({
    req(input$submit)
    
    validate(
      need(input$affordable30>= 0&
             input$affordable50>= 0&
             input$affordable60>= 0&
             input$affordable70>= 0&
             input$affordable80 >= 0, "Please ensure all fields have a value,
           even if zero")
    )
    
    fmi_rent <- adat_data %>% filter(GISJOIN_proj == bg_id())
    
    crit0 <- fmi_rent %>% 
      select(renter_p_20,
             renters_20) %>% 
      mutate(renter_p_20 = round(renter_p_20*100,0))
    
    crit1 <- fmi_rent %>%
      select(risk_level)
    
    aff_df <- data.frame(aff_level = c("30% AMI",
                                       "50% AMI",
                                       "60% AMI",
                                       "70% AMI",
                                       "80% AMI"),
                         aff_pct = c(round(fmi_rent$renters30_ct/fmi_rent$all_renters_ct*100,0), # % below 30%
                                     round(fmi_rent$renters50_ct/fmi_rent$all_renters_ct*100,0), # % below 50%
                                     round(fmi_rent$renters60_ct/fmi_rent$all_renters_ct*100,0), # % below 60%
                                     round(fmi_rent$renters70_ct/fmi_rent$all_renters_ct*100,0), # % below 70%
                                     round(fmi_rent$renters80_ct/fmi_rent$all_renters_ct*100,0)) # % below 80%
    )
    
    crit2_level <- aff_df$aff_level[min(which(aff_df$aff_pct>50))]
    crit2_pct <- aff_df$aff_pct[min(which(aff_df$aff_pct>50))]
    
    crit3 <- fmi_rent %>%
      select(cost_burden30_20_p)
    
    if(input$affordable30+
       input$affordable50+
       input$affordable60+
       input$affordable70+
       input$affordable80>
       input$proj_size) {
      paste("")
    } 
    else {
      card(
        value_box(title = "Tenure",
                  value = paste0("There are ", 
                                 crit0$renters_20, 
                                 " renters making up ",
                                 crit0$renter_p_20, 
                                 "% of households"),
                  showcase = bs_icon("building"),
                  #theme = "teal",
                  p("Renters are most at risk of displacement. High shares of renters
                  can be a sign of vulnerability")),
        value_box(title = "Risk Level",
                  value = crit1$risk_level,
                  showcase = bs_icon("house-exclamation"),
                  p("The risk level determines the criteria the project must meet")),
        value_box(title = "Affordability",
                  value = paste0(crit2_pct, "% of renters can afford rents up to ", crit2_level),
                  showcase = bs_icon("shield"),
                  p("Projects should be affordable to a majority of renters in 
                medium and high risk areas")),
        value_box(title = "Housing Cost Burden",
                  value = paste0(round(crit3$cost_burden30_20_p*100,0),
                                 "% of households are housing cost-burdened"), 
                  showcase = bs_icon("thermometer-half"),
                  p("The share of cost-burdened household determines how many units
                should be affordable in medium and high risk areas"))
      )
    }
  })
  
  output$recommendation <- renderUI({
    req(input$submit)
    
    validate(
      need(input$affordable30>= 0&
             input$affordable50>= 0&
             input$affordable60>= 0&
             input$affordable70>= 0&
             input$affordable80 >= 0, "")
    )
    
    fmi_rent <- adat_data %>% filter(GISJOIN_proj == bg_id())
    
    crit1 <- fmi_rent %>%
      select(risk_level)
    
    aff_df <- data.frame(aff_level = c("30% AMI",
                                       "50% AMI",
                                       "60% AMI",
                                       "70% AMI",
                                       "80% AMI"),
                         aff_pct = c(round(fmi_rent$renters30_ct/fmi_rent$all_renters_ct*100,0), # % below 30%
                                     round(fmi_rent$renters50_ct/fmi_rent$all_renters_ct*100,0), # % below 50%
                                     round(fmi_rent$renters60_ct/fmi_rent$all_renters_ct*100,0), # % below 60%
                                     round(fmi_rent$renters70_ct/fmi_rent$all_renters_ct*100,0), # % below 70%
                                     round(fmi_rent$renters80_ct/fmi_rent$all_renters_ct*100,0)), # % below 80%
                         aff_proj = c(input$affordable30,
                                      input$affordable30+input$affordable50,
                                      input$affordable30+input$affordable50+
                                        input$affordable60,
                                      input$affordable30+input$affordable50+
                                        input$affordable60+input$affordable70,
                                      input$affordable30+input$affordable50+
                                        input$affordable60+input$affordable70+
                                        input$affordable80)
    )
    
    crit2 <- min(which(aff_df$aff_pct>50))
    
    crit3 <- fmi_rent %>%
      select(cost_burden30_20_p) 
    
    high_criteria1 <- if(crit1$risk_level == "high" & 
                         aff_df$aff_proj[5] == input$proj_size
    ) {
      paste("The project meets the requirement that all units be affordable.")
    } else {
      paste("Projects in high-risk areas are required to only include afforable units.")
    }
    
    high_criteria2 <- if(crit1$risk_level == "high" & 
                         round(aff_df$aff_proj[min(which(aff_df$aff_pct>50))]/
                               input$proj_size*100,0) >= 
                         round(crit3$cost_burden30_20_p*100,0)
    ) {
      paste0(round(aff_df$aff_proj[min(which(aff_df$aff_pct>50))]/input$proj_size*100,0), 
             "% of units are affordable at ",
             aff_df$aff_level[min(which(aff_df$aff_pct>50))], 
             ". The project meets the requirement that the share of units affordable 
            to at least half the renter population be greater than or equal to 
            the percent of housing cost burdened households.")
    } else {
      paste0(round(aff_df$aff_proj[min(which(aff_df$aff_pct>50))]/input$proj_size*100,0), 
             "% of units are affordable at ",
             aff_df$aff_level[min(which(aff_df$aff_pct>50))],
             ". The share of units affordable to at least half the renter 
             population must be greater than or equal to the ", 
             round(crit3$cost_burden30_20_p*100,0),
             "% of housing cost burdened households")
    }
    
    high_criteria3 <- if(crit1$risk_level == "high" & 
                         aff_df$aff_proj[5] == input$proj_size &
                         round(aff_df$aff_proj[min(which(aff_df$aff_pct>50))]/
                               input$proj_size*100,0) >= 
                         round(crit3$cost_burden30_20_p*100,0)) {
      paste("This project is recommended for support.")
    }
    else {
      paste("This project is not recommended for support until it meets these conditions.")
    }
    
    medium_criteria <- if(crit1$risk_level == "medium" & 
                          round(aff_df$aff_proj[min(which(aff_df$aff_pct>50))]/
                                input$proj_size*100,0) >= 
                          round(crit3$cost_burden30_20_p*100,0)
    ) {
      paste0(round(aff_df$aff_proj[min(which(aff_df$aff_pct>50))]/input$proj_size*100,0), 
             "% of units are affordable at ",
             aff_df$aff_level[min(which(aff_df$aff_pct>50))], 
             ". The project meets the requirement that the share of units affordable 
            to at least half the renter population be greater than or equal to 
            the percent of housing cost burdened households. 
             This project is recommended for support.")
    } else {
      paste0(round(aff_df$aff_proj[min(which(aff_df$aff_pct>50))]/input$proj_size*100,0), 
             "% of units are affordable at ",
             aff_df$aff_level[min(which(aff_df$aff_pct>50))],
             ". The share of units affordable to at least half the renter 
             population must be greater than or equal to the ", 
             round(crit3$cost_burden30_20_p*100,0),
             "% of housing cost burdened households. 
             This project is not recommended for support until it meets this condition.")
    }
    
    low_criteria <- if(crit1$risk_level == "low" & 
                       aff_df$aff_proj[2]/input$proj_size >= .1
    ) {
      paste0(round(aff_df$aff_proj[2]/input$proj_size*100,0), 
             "% of units are affordable at ",
             aff_df$aff_level[2],
             ". The project meets the requirement that the share of units affordable 
            at 50% AMI or below be greater than or equal to 10% of all units. 
             This project is recommended for support.")
    } else {
      paste("Projects in low risk areas must include at least 10% of units affordable 
            at 50% AMI or below. This project is not recommended for 
            support until it meets this condition.")
    }
    
    
    
    if(input$affordable30+
       input$affordable50+
       input$affordable60+
       input$affordable70+
       input$affordable80>
       input$proj_size) {
      paste("Please check that the number of affordable units is no larger than
            the total number of units")
    }
    else{
      card(
        if(crit1$risk_level == "high") {
          high_criteria1
          high_criteria2
        }
        else if(crit1$risk_level == "medium") {
          medium_criteria
        }
        else {
          low_criteria
        },
        
        p("For more details about this results click below"), 
        actionButton("next_sec", "See details"),
        # p("For a PDF-form report, click below"),
        # actionButton("next_sec", "Export report")
        )
    }
  })
}



# Run ---------------------------------------------------------------------



# Run the application 
shinyApp(ui = ui, server = server)