# ui.R — top-level navbar. The Affordability levels and Area overview panels
# are not declared here; they are inserted at runtime by server.R once the user
# has supplied a valid location (see the nav_insert calls there).

page_navbar(
  title = "ANTI-DISPLACEMENT ASSESSMENT TOOL",
  id = "ADI_tabs",
  useShinyjs(),
  mod_intro_ui(),
  mod_housing_need_ui("housing_need"),
  mod_location_ui("location")
)
