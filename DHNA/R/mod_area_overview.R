# Area overview panel — the dashboard of displacement-risk components for the
# local area of the selected block group (map, ethnoracial change, college,
# rent, income, and asking-rent trends).
#
# Args:
#   bg_id       reactive returning the focal block group GISJOIN
#   adat_data   risk database (one row per block group)
#   pop         ethnoracial population by year
#   local_area  block-group -> local-area tract membership
#   rent_buffer Renthub quarterly asking-rent series
#   lvm_bg_geo, lvm_ct_geo, lvm_ma_geo   block-group / tract / market-area geometry

# Shared two-year comparison bar (used by college, rent, and income charts).
two_year_bar <- function(v10, v20, ylab) {
  tibble(Year = c(2010, 2020), value = c(v10, v20), Area = "Local Area") %>%
    ggplot() +
    geom_bar(aes(y = value, x = Year, fill = Area), alpha = .7,
             width = 6,
             position = position_dodge(6.5),
             stat = "identity") +
    labs(y = ylab) +
    scale_x_continuous(name = "Year", breaks = c(2010, 2020)) +
    scale_fill_manual(values = c("#002E60")) +
    theme_bw() +
    theme(text = element_text(size = 14),
          legend.position = "none")
}

mod_area_overview_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Area overview", value = "explainer",
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
           card_body(leafletOutput(ns("neighborhood_map"))),
           card_body(p("The map shows the block group where the
                       project is located in dark gray and
                       the local area from which the data is drawn
                       in light gray."))),
      card(card_header("Change in racial composition"),
           fluidRow(
             column(width = 6, plotOutput(ns("ethrace_plot"))),
             column(width = 6, textOutput(ns("ethrace_change"))))
      )),
    layout_columns(
      card(card_header("Change in % of adults with college education"),
           card_body(plotOutput(ns("edu"))),
           card_body(textOutput(ns("edu_change")))),
      card(card_header("Change in median rent"),
           card_body(plotOutput(ns("rents"))),
           card_body(textOutput(ns("rent_change")))),
      card(card_header("Change in household income"),
           card_body(plotOutput(ns("hhinc"))),
           card_body(textOutput(ns("income_change"))))
    ),
    layout_columns(
      card(card_header("Change in asking rent"),
           fluidRow(
             column(width = 6, plotOutput(ns("rent_trend"))),
             column(width = 6, textOutput(ns("market_rent_change"))))
      ))
  )
}

mod_area_overview_server <- function(id, bg_id, adat_data, pop, local_area,
                                     rent_buffer, lvm_bg_geo, lvm_ct_geo, lvm_ma_geo) {
  moduleServer(id, function(input, output, session) {

    area <- reactive({
      adat_data %>% filter(GISJOIN_proj == bg_id())
    })
    area_pop <- reactive({
      pop %>% filter(GISJOIN_proj == bg_id())
    })

    output$neighborhood_map <- renderLeaflet({

      bg_focus <- lvm_bg_geo %>%
        filter(GISJOIN == bg_id())

      local_area_id <- local_area %>%
        filter(GISJOIN_proj == bg_id())

      local_area_ct <- lvm_ct_geo %>%
        filter(GISJOIN %in% local_area_id$GISJOIN_CT)

      bbox <- st_bbox(local_area_ct)
      xview <- mean(c(bbox[1] + .02, bbox[3]))
      yview <- mean(c(bbox[2], bbox[4]))

      leaflet() %>%
        addTiles() %>%
        setView(lng = xview,
                lat = yview,
                zoom = 13) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = local_area_ct, color = 'black', fillOpacity = .2, weight = 2) %>%
        addPolygons(data = bg_focus, color = "black", fillOpacity = .6, weight = 2) %>%
        addPolygons(data = lvm_ma_geo, color = "darkred", fillOpacity = .01, weight = 2, label = ~mkt_area) %>%
        addMiniMap(
          tiles = providers$Esri.WorldGrayCanvas,
          position = 'topright',
          width = 150, height = 150,
          toggleDisplay = FALSE,
          zoomLevelOffset = -4)

    })

    output$ethrace_plot <- renderPlot({
      area_pop() %>%
        mutate(p_black = round(pop_black / pop * 100, 1),
               p_white = round(pop_white / pop * 100, 1),
               p_latino = round(pop_latino / pop * 100, 1),
               Area = factor(Area)) %>%
        select(GISJOIN_proj, data_yr, pop_black, pop_white, pop_latino,
               p_black, p_white, p_latino, Area) %>%
        ggplot() +
        geom_line(aes(x = data_yr, y = p_black, linetype = Area, color = "Black"), linewidth = 1.1) +
        geom_line(aes(x = data_yr, y = p_latino, linetype = Area, color = "Latino/Hispanic"), linewidth = 1.1) +
        geom_line(aes(x = data_yr, y = p_white, linetype = Area, color = "White"), linewidth = 1.1) +
        labs(x = "Year", y = "% of population") +
        scale_color_manual(values = colors_er) +
        theme_bw() +
        theme(text = element_text(size = 16)) +
        guides(color = guide_legend("Group", nrow = 1),
               linetype = guide_legend(nrow = 1)) +
        theme(legend.box = "vertical",
              legend.position = "bottom")
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
            round(area()$blackpct_ch_00_20 * 100, 1), "percentage point.
            and the Black population changed by",
            (area()$black_change00_10 + area()$black_change10_20),
            "between 2000 and 2020")
    })

    output$edu <- renderPlot({
      a <- area()
      two_year_bar(
        round(a$adult_college_above_10 / a$adult_over25_10 * 100, 1),
        round(a$adult_college_above_20 / a$adult_over25_20 * 100, 1),
        "% with college education"
      )
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
             area()$rank_college, "th within Louisville for how
            quickly the share of adults with collge education increased from
            2013 to 2023")
    })

    output$rents <- renderPlot({
      a <- area()
      two_year_bar(a$median_rent_10, a$median_rent_20, "Median Rent")
    })

    output$rent_change <- renderText({
      paste0("Rapid rent increases are a primary indicator that residents may be
      at risk of displacement. In the absence of income increases, higher rents
      first decreases the ability of renters to afford other necessities, then,
      when the rent burden becomes too high, people may leave. The tool ranks
      all areas based on the increase in gross rent and asking rent (see below).

            This local area ranks ",
             area()$rank_rents, "th within Louisville for how
            quickly rents increased from 2013 to 2023.")
    })

    output$hhinc <- renderPlot({
      a <- area()
      two_year_bar(a$median_hhinc_10, a$median_hhinc_20, "Median household income")
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
             area()$rank_hhinc, "th within Louisville for how
            quickly household income increased from 2013 to 2023.")
    })

    output$rent_trend <- renderPlot({
      rent_buffer %>%
        filter(GISJOIN_proj == bg_id()) %>%
        ggplot(aes(y = rent_bg, x = year_qu, group = GEOID_bg)) +
        geom_line(aes(y = rent_bg, x = year_qu, group = GEOID_bg, color = "Block Group"), size = 1.2) +
        geom_line(aes(y = rent_ma, x = year_qu, group = GEOID_bg, color = "Market Area"), size = 1.2) +
        labs(y = "Rent") +
        scale_color_manual(name = "Quarterly rent level", values = c("Block Group" = "#002E60", "Market Area" = "#91B2D2")) +
        scale_x_continuous("Quarter - Year", breaks = c(17, 24, 32, 40),
                           labels = c("Q1 - 2018", "Q1 - 2020", "Q1 - 2022", "Q1 - 2024")) +
        theme_bw() +
        theme(text = element_text(size = 16))
    })

    output$market_rent_change <- renderText({
      if (!bg_id() %in% rent_buffer$GISJOIN_proj) {
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
  })
}
