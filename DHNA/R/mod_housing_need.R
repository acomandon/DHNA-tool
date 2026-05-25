# Housing Need panel — AMI explainer cards plus the rent-burden and
# housing-mismatch figures. Both figures are driven by hh_micro and do not
# depend on the selected location.

mod_housing_need_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Housing Need", value = "HNeed",
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
               tags$a(href = "https://louisvilleky.gov/government/housing/housing-needs-assessment", "Housing Need Assessment"))
           )
      ),
      col_widths = c(6, 6)),
    layout_columns(
      card(height = 400,
           card_header("Rent Burden"),
           card_body(class = "align-items-center",
                     plotOutput(ns("rent_burden"), width = "100%",
                                height = "350px"))
      ),
      card(
        p("Many people struggle to pay their rent.
                            Among people with the lowest income (here defined as below 30% of AMI),
                            75% pay over half of their income toward rent. This means that paying for
                            everything else, like healthcare, food, and transportation, is extremely
                            challenging")
      ),
      col_widths = c(6, 6)),
    layout_columns(
      card(height = 400,
           card_header("Housing Mismatch"),
           card_body(class = "align-items-center",
                     plotOutput(ns("housing_mismatch"), width = "100%",
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
      col_widths = c(7, 5))
  )
}

mod_housing_need_server <- function(id, hh_micro, housing_mismatch_plot) {
  moduleServer(id, function(input, output, session) {

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
        scale_fill_manual(values = colors_3) +
        scale_y_continuous("% of renters",
                           breaks = c(0.25, 0.50, 0.75, 1.00),
                           labels = c("25", "50", "75", "100")) +
        labs(x = "Income level") +
        theme_minimal() +
        guides(fill = guide_legend(title = "Rent burden")) +
        theme(text = element_text(size = 16))
    })

    output$housing_mismatch <- renderPlot(housing_mismatch_plot)
  })
}
