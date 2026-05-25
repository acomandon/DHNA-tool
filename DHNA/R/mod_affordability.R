# Affordability levels panel — AMI unit inputs plus the two result blocks
# (risk assessment summary and the recommendation). The recommendation is
# computed once in a shared reactive and consumed by both outputs.
#
# Args:
#   bg_id      reactive returning the focal block group GISJOIN
#   proj_size  reactive returning the proposed number of units
#   adat_data  the risk database (one row per block group)
# Returns:
#   list(see_details = reactive(input$next_sec))  drives the Area overview panel

mod_affordability_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Affordability levels", value = "form",
    layout_columns(
      fill = TRUE,
      col_widths = 12,
      card(full_screen = FALSE,
           card_header("Project details"),

           layout_sidebar(
             open = TRUE,
             fillable = TRUE,

             sidebar = sidebar(
               width = 400,

               p("The affordability of housing affects local rents and who
                 can move into the neighborhood. Enter the number of units
                 at each level of AMI. If the project is a single-family home,
                 enter '1' in the appropriate box."),

               numericInput(ns("affordable30"), "Number of units reserved for 30% AMI or below", value = 0),
               numericInput(ns("affordable50"), "Number of units reserved for 50% AMI or below", value = 0),
               numericInput(ns("affordable60"), "Number of units reserved for 60% AMI or below", value = 0),
               numericInput(ns("affordable70"), "Number of units reserved for 70% AMI or below", value = 0),
               numericInput(ns("affordable80"), "Number of units reserved for 80% AMI or below", value = 0),
               actionButton(ns("submit"), "Submit")
             ),

             uiOutput(ns("risk_assessment_res")),
             uiOutput(ns("recommendation"))

           ))
    )
  )
}

mod_affordability_server <- function(id, bg_id, proj_size, adat_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    area <- reactive({
      adat_data %>% filter(GISJOIN_proj == bg_id())
    })

    # Computed once after Submit; both output blocks consume the cached result.
    recommendation_result <- reactive({
      req(input$submit)
      recommend_project(
        risk_level = area()$risk_level,
        project_size = proj_size(),
        affordable_units = list(
          ami30 = input$affordable30, ami50 = input$affordable50,
          ami60 = input$affordable60, ami70 = input$affordable70,
          ami80 = input$affordable80
        ),
        area = area()
      )
    })

    output$risk_assessment_res <- renderUI({
      req(input$submit)
      validate(
        need(input$affordable30 >= 0 & input$affordable50 >= 0 &
               input$affordable60 >= 0 & input$affordable70 >= 0 &
               input$affordable80 >= 0,
             "Please ensure all fields have a value, even if zero")
      )

      result <- recommendation_result()
      if (!result$valid) return(paste(""))

      ctx <- result$context
      card(
        value_box(title = "Tenure",
                  value = paste0("There are ", ctx$renters_20,
                                 " renters making up ", ctx$renter_p_20,
                                 "% of households"),
                  showcase = bs_icon("building"),
                  p("Renters are most at risk of displacement. High shares of renters
                    can be a sign of vulnerability")),
        value_box(title = "Risk Level",
                  value = ctx$risk_level,
                  showcase = bs_icon("house-exclamation"),
                  p("The risk level determines the criteria the project must meet")),
        value_box(title = "Affordability",
                  value = paste0(ctx$majority_pct_renters,
                                 "% of renters can afford rents up to ",
                                 ctx$majority_level),
                  showcase = bs_icon("shield"),
                  p("Projects should be affordable to a majority of renters in
                    medium and high risk areas")),
        value_box(title = "Housing Cost Burden",
                  value = paste0(ctx$cost_burden_pct,
                                 "% of households are housing cost-burdened"),
                  showcase = bs_icon("thermometer-half"),
                  p("The share of cost-burdened household determines how many units
                    should be affordable in medium and high risk areas"))
      )
    })

    output$recommendation <- renderUI({
      req(input$submit)
      validate(
        need(input$affordable30 >= 0 & input$affordable50 >= 0 &
               input$affordable60 >= 0 & input$affordable70 >= 0 &
               input$affordable80 >= 0, "")
      )

      result <- recommendation_result()
      if (!result$valid) return(p(result$error))

      card(
        tagList(lapply(result$messages, p)),
        p("For more details about this results click below"),
        actionButton(ns("next_sec"), "See details")
      )
    })

    list(see_details = reactive(input$next_sec))
  })
}
