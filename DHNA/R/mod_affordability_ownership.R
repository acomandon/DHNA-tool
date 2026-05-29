# Affordability levels panel — ownership tenure path. Collects ownership
# assistance program info and reserved-units price point, runs the ownership
# recommender, and renders the same four-card summary + recommendation block
# layout as the rental path.
#
# Args:
#   bg_id       reactive returning the focal block group GISJOIN
#   proj_size   reactive returning the proposed number of units
#   adat_data   the risk database (one row per block group)
# Returns:
#   list(see_details = reactive(input$next_sec))  drives the Area overview panel

mod_affordability_ownership_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Affordability levels", value = "form",
    layout_columns(
      fill = TRUE,
      col_widths = 12,
      card(full_screen = FALSE,
           card_header("Project details — homeownership"),

           layout_sidebar(
             open = TRUE,
             fillable = TRUE,

             sidebar = sidebar(
               width = 400,

               p("Ownership projects qualify through assistance programs rather
                 than AMI rent tiers. Enter the program you are using, the
                 number of units it covers, and any units you are reserving
                 for existing residents (first right of refusal) along with
                 their sale price."),

               textInput(ns("program_name"),
                         "Ownership assistance program",
                         placeholder = "e.g. Louisville Affordable Housing Trust Fund"),
               selectInput(ns("home_type"), "Home type",
                           choices = c("Existing home" = "existing",
                                       "New construction" = "new"),
                           selected = "existing"),
               numericInput(ns("assisted_units"),
                            "Units covered by the assistance program",
                            value = 0, min = 0),
               numericInput(ns("reserved_units"),
                            "Units reserved for existing residents (first right of refusal)",
                            value = 0, min = 0),
               conditionalPanel(
                 condition = "input.reserved_units > 0", ns = ns,
                 numericInput(ns("reserved_price"),
                              "Sale price per reserved unit ($)",
                              value = 0, min = 0)
               ),
               checkboxGroupInput(ns("eligibility_tags"),
                                  "Eligibility focus (informational)",
                                  choices = c("Existing residents",
                                              "First-time buyers",
                                              "Age 55 and over",
                                              "Other")),
               actionButton(ns("submit"), "Submit")
             ),

             uiOutput(ns("risk_assessment_res")),
             uiOutput(ns("recommendation"))

           ))
    )
  )
}

mod_affordability_ownership_server <- function(id, bg_id, proj_size, adat_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    area <- reactive({
      adat_data %>% filter(GISJOIN_proj == bg_id())
    })

    # Pick the matching 1-unit HUD purchase-price limit from config based on
    # the selected home type.
    hud_max_price <- reactive({
      if (identical(input$home_type, "new")) {
        hud_for_sale$new_construction[1]
      } else {
        hud_for_sale$existing_home[1]
      }
    })

    recommendation_result <- reactive({
      req(input$submit)
      recommend_project_ownership(
        risk_level      = area()$risk_level_owner,
        dominant_family = area()$dominant_family_owner,
        project_size    = proj_size(),
        ownership_inputs = list(
          program_name     = input$program_name,
          home_type        = input$home_type,
          assisted_units   = input$assisted_units,
          reserved_units   = input$reserved_units,
          reserved_price   = input$reserved_price,
          eligibility_tags = input$eligibility_tags
        ),
        area = area(),
        hud_max_price = hud_max_price()
      )
    })

    fmt_money <- function(x) paste0("$", format(round(x), big.mark = ",", scientific = FALSE))

    output$risk_assessment_res <- renderUI({
      req(input$submit)
      validate(
        need(input$assisted_units >= 0 & input$reserved_units >= 0,
             "Please ensure all unit counts are zero or positive."),
        need(input$reserved_units == 0 || (!is.null(input$reserved_price) && input$reserved_price > 0),
             "Please enter a sale price for the reserved units.")
      )

      result <- recommendation_result()
      if (!result$valid) return(paste(""))

      ctx <- result$context
      price_value <- if (ctx$reserved_units > 0) {
        paste0(fmt_money(ctx$reserved_price),
               if (ctx$price_gate_passes) " — meets HUD limit"
               else paste0(" — exceeds HUD limit ", fmt_money(ctx$hud_max_price)))
      } else {
        paste0("No reserved units (HUD limit ", fmt_money(ctx$hud_max_price), ")")
      }

      card(
        value_box(title = "Tenure",
                  value = paste0("There are ", ctx$owner_count,
                                 " owner-occupied units making up ",
                                 ctx$owner_p_20, "% of households"),
                  showcase = bs_icon("house-door"),
                  p("Ownership projects are evaluated against the local
                    homeowner context and overall cost-burden pressure.")),
        value_box(title = "Risk Level",
                  value = ctx$risk_level,
                  showcase = bs_icon("house-exclamation"),
                  p("The risk level determines the criteria the project must meet")),
        value_box(title = "Reserved-unit price gate",
                  value = price_value,
                  showcase = bs_icon("shield"),
                  p(paste0("Reserved units only count toward the requirement if",
                           " priced at or below the HUD affordable-for-sale limit",
                           " for a ", ctx$home_type_label, "."))),
        value_box(title = "Housing Cost Burden",
                  value = paste0(ctx$cost_burden_pct,
                                 "% of households are housing cost-burdened"),
                  showcase = bs_icon("thermometer-half"),
                  p("The share of cost-burdened households determines how many
                    units must be reserved in medium and high risk areas."))
      )
    })

    output$recommendation <- renderUI({
      req(input$submit)
      result <- recommendation_result()
      if (!result$valid) return(p(result$error))

      card(
        tagList(lapply(result$messages, p)),
        p("For more details about these results click below"),
        actionButton(ns("next_sec"), "See details")
      )
    })

    list(see_details = reactive(input$next_sec))
  })
}
