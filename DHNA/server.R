# server.R — initializes the panel modules and owns cross-panel navigation.
#
# All module servers are created up front; Shiny binds their outputs lazily, so
# the Affordability levels and Area overview panels render correctly even though
# their UI is only inserted into the navbar later, on button events.

server <- function(input, output, session) {

  # Panel modules ----
  mod_housing_need_server("housing_need",
                          hh_micro = hh_micro,
                          housing_mismatch_plot = housing_mismatch_plot)

  loc <- mod_location_server("location", lvm_bg_geo = lvm_bg_geo)

  aff <- mod_affordability_server("affordability",
                                  bg_id = loc$bg_id,
                                  proj_size = loc$proj_size,
                                  adat_data = adat_data)

  mod_area_overview_server("area",
                           bg_id = loc$bg_id,
                           adat_data = adat_data,
                           pop = pop,
                           local_area = local_area,
                           rent_buffer = rent_buffer,
                           lvm_bg_geo = lvm_bg_geo,
                           lvm_ct_geo = lvm_ct_geo,
                           lvm_ma_geo = lvm_ma_geo)

  # Navigation ----
  # "next page" on Location -> (re)insert the Affordability levels panel.
  observeEvent(loc$go_to_form(), {
    removeTab(inputId = "ADI_tabs", target = "form")
    removeTab(inputId = "ADI_tabs", target = "explainer")
    nav_insert(
      "ADI_tabs",
      target = "location",
      select = TRUE,
      position = "after",
      mod_affordability_ui("affordability")
    )
  }, ignoreInit = TRUE)

  # "See details" on Affordability -> insert the Area overview panel.
  observeEvent(aff$see_details(), {
    nav_insert(
      "ADI_tabs",
      target = "form",
      select = TRUE,
      position = "after",
      mod_area_overview_ui("area")
    )
  }, ignoreInit = TRUE)
}
