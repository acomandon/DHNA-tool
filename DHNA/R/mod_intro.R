# Introduction panel — static landing page. UI only (no server logic), so it
# is a plain UI function rather than a full Shiny module. The tags$head block
# injects the Louisville logo into the navbar for the whole app.

mod_intro_ui <- function() {
  nav_panel(
    "Introduction", value = "intro",
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
             p("The ", tags$a(href = "https://codelibrary.amlegal.com/codes/louisvillemetro/latest/loukymetro/0-0-0-71955", "Anti-Displacement Ordinance"), "passed by Louisville Metro City Council
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
  )
}
