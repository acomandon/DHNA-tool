# Location panel — project tenure choice, size/type inputs, the address
# geocoder and click map, and resolution of the focal block group (bg_id)
# from the chosen point.
#
# Returns a list of reactives consumed elsewhere:
#   bg_id       the GISJOIN of the block group containing the chosen point
#   proj_size   input$proj_size
#   tenure      input$tenure ("rental" | "ownership") — drives which
#                 Affordability panel is inserted on "next page"
#   go_to_form  input$valid_loc_btn (drives insertion of the Affordability panel)

mod_location_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Location", value = "location",
    p("Start by telling us what kind of project this is. The tool's assessment
              differs between rental and ownership projects (mixed-tenure
              projects are coming in a future release)."),
    radioButtons(ns("tenure"), "Project type",
                 choices = c("Rental" = "rental",
                             "Homeownership" = "ownership"),
                 selected = "rental",
                 inline = TRUE),
    p("Project can have different impacts based on where they are located.
              Please, enter information about where the project will be located."),
    numericInput(ns("proj_size"), "Number of units in proposed project",
                 value = 0),
    conditionalPanel(
      condition = "input.proj_size > 0", ns = ns,
      selectInput(ns("build_type"), "Housing type",
                  c("Single-Family Home" = "sfh",
                    "Multi-Family Home" = "mfh"),
                  selected = 'mfh')
    ),
    conditionalPanel(
      condition = "input.build_type == 'sfh' & input.proj_size > 1", ns = ns,
      selectInput(ns("scattered_sites"),
                  "Will the homes be scattered across multiple locations",
                  c("Yes, each home is in a distinct location" = "yes",
                    "No, the homes are all within the same block" = "no"),
                  selected = "no")
    ),
    htmlOutput(ns("Scattered_site_response")),

    p("Please, provide the location of your project with an address
              or directly on the map"),
    textInput(ns("address"), "Address:"),
    textInput(ns("city"), "City:"),
    textInput(ns("zip"), "Zip:"),
    actionButton(ns("geocode"), "Geocode address"),
    leafletOutput(ns("map")),
    actionButton(ns("use_clik_loc"), "Confirm location"),
    textOutput(ns("valid_loc_text")),
    conditionalPanel(
      condition = "output.valid_loc_text == 'Your selected location and project information have been recorded' && input.proj_size > 0",
      ns = ns,
      actionButton(ns("valid_loc_btn"), "next page"))
  )
}

mod_location_server <- function(id, lvm_bg_geo) {
  moduleServer(id, function(input, output, session) {

    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = locality$default_map$lng,
                lat = locality$default_map$lat,
                zoom = locality$default_map$zoom) %>%
        addProviderTiles("Esri.WorldGrayCanvas")
    })

    pr_location1 <- reactiveVal(clicks)

    # Shared helper: drop a marker at (x, y) and record the location
    set_location <- function(x, y, zoom = NULL) {
      proxy <- leafletProxy("map") %>%
        clearShapes()
      if (!is.null(zoom)) {
        proxy <- proxy %>% clearControls() %>% setView(x, y, zoom = zoom)
      }
      proxy %>%
        addCircles(lng = x, lat = y, stroke = FALSE, fill = TRUE,
                   fillOpacity = .8, color = "#777777", radius = 20)
      pr_location1(mutate(pr_location1(), lat = y, lng = x))
    }

    observeEvent(input$geocode, {
      address_single <- tibble(singlelineaddress = paste(input$address, ", ",
                                                         input$city, ", ",
                                                         paste0(locality$state_abbr, ", "),
                                                         input$zip))
      census_full <- address_single %>% geocode(
        address = singlelineaddress,
        method = "census")

      set_location(census_full$long, census_full$lat, zoom = 14)
    })

    observeEvent(input$map_click, {
      click_loc <- input$map_click
      set_location(click_loc$lng, click_loc$lat)
    })

    bg_id <- reactive({
      if (is.na(pr_location1()$lat) == FALSE) {
        bg_coords <- pr_location1()
        lat <- as.numeric(bg_coords$lat)
        long <- as.numeric(bg_coords$lng)

        bg <- st_intersects(st_point(c(long, lat)), lvm_bg_geo)[[1]][1]
        lvm_bg_geo$GISJOIN[bg]
      } else {
        NA
      }
    }) %>%
      bindCache(pr_location1()) %>%
      bindEvent(input$use_clik_loc)

    valid_loc <- eventReactive(input$use_clik_loc, {
      if (is.na(bg_id()) == FALSE & input$proj_size > 0) {
        "Your selected location and project information have been recorded"
      }
      else if ((is.na(bg_id()) == FALSE & input$proj_size < 1)) {
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
      if (input$scattered_sites == "yes" & input$build_type == "sfh") {
        paste("<br> <em>This tool can only process one location at a time. Please, contact the office of housing for scattered site developments.</em> <br>", "<br>", "<br>")
      }
      else if (input$scattered_sites == "no" & input$build_type == "sfh") {
        paste("<br> <em>Please, enter the address of one of the homes and the tool will treat the project as a cluster of homes.</em> <br>", "<br>", "<br>")
      }
      else {
        ""
      }
    })

    list(
      bg_id = bg_id,
      proj_size = reactive(input$proj_size),
      tenure = reactive(input$tenure),
      go_to_form = reactive(input$valid_loc_btn)
    )
  })
}
