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

  aff_rental <- mod_affordability_rental_server("affordability_rental",
                                                bg_id = loc$bg_id,
                                                proj_size = loc$proj_size,
                                                adat_data = adat_data,
                                                location_label = loc$location_label)

  aff_ownership <- mod_affordability_ownership_server("affordability_ownership",
                                                     bg_id = loc$bg_id,
                                                     proj_size = loc$proj_size,
                                                     adat_data = adat_data,
                                                     location_label = loc$location_label)

  mod_area_overview_server("area",
                           bg_id = loc$bg_id,
                           tenure = loc$tenure,
                           adat_data = adat_data,
                           pop = pop,
                           local_area = local_area,
                           rent_buffer = rent_buffer,
                           lvm_bg_geo = lvm_bg_geo,
                           lvm_ct_geo = lvm_ct_geo,
                           lvm_ma_geo = lvm_ma_geo)

  # Navigation ----
  # "next page" on Location -> (re)insert the matching Affordability panel
  # based on the chosen project tenure (rental vs ownership).
  observeEvent(loc$go_to_form(), {
    removeTab(inputId = "ADI_tabs", target = "form")
    removeTab(inputId = "ADI_tabs", target = "explainer")
    panel <- if (identical(loc$tenure(), "ownership")) {
      mod_affordability_ownership_ui("affordability_ownership")
    } else {
      mod_affordability_rental_ui("affordability_rental")
    }
    nav_insert(
      "ADI_tabs",
      target = "location",
      select = TRUE,
      position = "after",
      panel
    )
  }, ignoreInit = TRUE)

  # "See details" on either Affordability panel -> insert the Area overview.
  # Only the currently-inserted panel's button can fire, so the two observers
  # don't conflict.
  observeEvent(aff_rental$see_details(), {
    nav_insert(
      "ADI_tabs",
      target = "form",
      select = TRUE,
      position = "after",
      mod_area_overview_ui("area")
    )
  }, ignoreInit = TRUE)

  observeEvent(aff_ownership$see_details(), {
    nav_insert(
      "ADI_tabs",
      target = "form",
      select = TRUE,
      position = "after",
      mod_area_overview_ui("area")
    )
  }, ignoreInit = TRUE)
}
