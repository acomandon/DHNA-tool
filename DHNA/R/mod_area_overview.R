# Area overview panel — tenure-aware dashboard of displacement-risk signals
# for the focal block group. Layout differs by project tenure:
#   - rental:    rent appreciation, composite gentrification, market tightness
#   - ownership: foreclosure pressure, permit pipeline, HV appreciation,
#                owner vulnerability, composite gentrification
# Both flows start with a risk-classification header card (tier +
# dominant_family + tier mechanism + family emphasis) and the local-area map.
#
# Args:
#   bg_id, tenure  reactives from the Location panel
#   adat_data      risk database (one row per BG; carries tier + dominant_family)
#   pop            ethnoracial population by year
#   local_area     BG -> local-area-tract membership
#   rent_buffer    Renthub quarterly asking-rent series (deferred; unused at present)
#   lvm_bg_geo, lvm_ct_geo, lvm_ma_geo   geometry layers

# --- helpers --------------------------------------------------------------

# Two-year bar (existing — used by rent / HV / college / income)
two_year_bar <- function(v10, v20, ylab) {
  tibble(Year = c(2011, 2022), value = c(v10, v20), Area = "Local Area") %>%
    ggplot() +
    geom_bar(aes(y = value, x = Year, fill = Area), alpha = .7,
             width = 6,
             position = position_dodge(6.5),
             stat = "identity") +
    labs(y = ylab) +
    scale_x_continuous(name = "Year", breaks = c(2011, 2022)) +
    scale_fill_manual(values = c("#002E60")) +
    theme_bw() +
    theme(text = element_text(size = 14),
          legend.position = "none")
}

# Map "ownership" -> "owner" so we can reuse the recommender's text helpers.
.tenure_key <- function(tenure_choice) {
  if (identical(tenure_choice, "ownership")) "owner" else "rental"
}

# Pretty labels.
.tenure_display <- function(t) {
  switch(t, ownership = "homeownership project", rental = "rental project", t)
}
.family_display <- function(f) {
  if (is.null(f) || is.na(f) || f == "") return("Stable / opportunity area")
  display <- c(
    appreciation       = "Price appreciation",
    distress           = "Financial distress",
    composite          = "Composite gentrification",
    `market-tightness` = "Market tightness",
    expansion          = "Pipeline / leading indicator"
  )
  display[[as.character(f)]] %||% as.character(f)
}
.tier_display <- function(t) {
  switch(as.character(t),
    high   = "High risk",
    medium = "Medium risk",
    low    = "Low risk",
    as.character(t))
}

# Rank-line text: describes where a rank sits relative to the classifier's
# reference points. `direction = "above"` (default) treats the band as
# "rank above threshold"; `direction = "below"` inverts the test for
# inverse-direction signals such as low-income share, where a low rank
# means the share has shrunk.
.rank_line <- function(rank_val, label, threshold_name = "q4_cutoff",
                       band_label = "top quintile", direction = "above") {
  if (is.null(rank_val) || is.na(rank_val)) {
    return(paste0(label, ": rank not available."))
  }
  thr <- risk_params[[threshold_name]]
  in_band <- if (identical(direction, "below")) rank_val < thr else rank_val > thr
  side    <- if (identical(direction, "below")) "below" else "above"
  pos <- if (in_band) paste0("in the ", band_label, " (", side, " ", thr, ")")
         else        paste0("outside the ", band_label, " (threshold ", thr, ")")
  paste0(label, ": rank ", rank_val, "/100 — ", pos)
}

# --- UI -------------------------------------------------------------------

mod_area_overview_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    "Area overview", value = "explainer",
    div(
      # Centered max-width container — keeps 2-bar charts and text from
      # stretching too wide on large screens.
      style = "max-width: 1200px; margin: 0 auto;",
    layout_columns(
      card(
        p("The neighborhood context determines displacement risk for the area
           where the project is located. This dashboard shows the data the
           classifier used to reach its decision, organised by the signal
           families that matter for the chosen tenure path."),
        p(strong("How to read the ranks. "),
          "Each variable is percentile-ranked from 0 (lowest in Louisville)
           to 100 (highest). The classifier uses three reference points:
           the ", strong("top quintile"), " (rank above ", risk_params$q4_cutoff,
          ") is the strongest sign of pressure; the ",
          strong("middle band"), " (rank ", risk_params$upper_half, "–",
          risk_params$q4_cutoff, ") signals an early-stage trend; the ",
          strong("above-median band"), " (rank above ", risk_params$above_med,
          ") flags a contributing signal when several appear together.
           Low-income population share works in reverse — a low rank (below ",
          risk_params$q1_cutoff, ") signals displacement because the share
           has shrunk."),
        p("No single signal determines risk by itself; the classifier combines
           signals across families. See the project's recommendation page for
           this area's overall tier and leading driver.")
      )
    ),
    layout_columns(
      card(card_header("Risk classification"),
           uiOutput(ns("header_body")))
    ),
    layout_columns(
      card(card_header("Local area map"),
           card_body(leafletOutput(ns("neighborhood_map"))),
           card_body(p("The map shows the focal block group in dark gray,
                       the local area (BGs in the same tract within 800m
                       of the focal BG's population centre) in light gray,
                       and the comprehensive-plan market areas outlined in red."))),
      card(card_header("Composition change (Black population)"),
           fluidRow(
             column(width = 7, plotOutput(ns("ethrace_plot"))),
             column(width = 5, textOutput(ns("ethrace_change")))))
    ),
    uiOutput(ns("tenure_body"))
    )  # close max-width div
  )
}

# --- Tenure-specific layouts ----------------------------------------------

.ownership_layout <- function(ns) {
  tagList(
    layout_columns(
      card(card_header("Foreclosure pressure"),
           card_body(textOutput(ns("foreclosure_summary"))),
           card_body(textOutput(ns("foreclosure_interp"))))
    ),
    layout_columns(
      card(card_header("Permit pipeline (leading indicator)"),
           card_body(
             fluidRow(
               column(4, textOutput(ns("permits_new_text"))),
               column(4, textOutput(ns("permits_renov_text"))),
               column(4, textOutput(ns("permits_demo_text")))
             )),
           card_body(textOutput(ns("permits_interp"))))
    ),
    layout_columns(
      card(card_header("Home value appreciation"),
           card_body(plotOutput(ns("hv_plot"))),
           card_body(textOutput(ns("hv_text")))),
      card(card_header("Sales-price trend (MLS)"),
           card_body(textOutput(ns("mls_text"))))
    ),
    layout_columns(
      card(card_header("Owner vulnerability"),
           card_body(textOutput(ns("owner_vuln_text"))))
    ),
    layout_columns(
      card(card_header("Income & education"),
           card_body(plotOutput(ns("hhinc"))),
           card_body(textOutput(ns("income_change")))),
      card(card_header("Education"),
           card_body(plotOutput(ns("edu"))),
           card_body(textOutput(ns("edu_change"))))
    )
  )
}

.rental_layout <- function(ns) {
  tagList(
    layout_columns(
      card(card_header("Rent appreciation"),
           card_body(plotOutput(ns("rents"))),
           card_body(textOutput(ns("rent_change"))))
    ),
    layout_columns(
      card(card_header("Income"),
           card_body(plotOutput(ns("hhinc"))),
           card_body(textOutput(ns("income_change")))),
      card(card_header("Education"),
           card_body(plotOutput(ns("edu"))),
           card_body(textOutput(ns("edu_change"))))
    ),
    layout_columns(
      card(card_header("Market tightness"),
           card_body(textOutput(ns("market_tightness_text"))))
    )
  )
}

# --- Server ---------------------------------------------------------------

mod_area_overview_server <- function(id, bg_id, tenure, adat_data, pop,
                                     local_area, rent_buffer,
                                     lvm_bg_geo, lvm_ct_geo, lvm_ma_geo) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    area <- reactive({
      adat_data %>% filter(GISJOIN_proj == bg_id())
    })
    area_pop <- reactive({
      pop %>% filter(GISJOIN_proj == bg_id())
    })

    # --- Header card -----------------------------------------------------
    output$header_body <- renderUI({
      a <- area()
      tkey <- .tenure_key(tenure())
      tier <- if (tkey == "rental") as.character(a$risk_level_renter)
              else as.character(a$risk_level_owner)
      fam  <- if (tkey == "rental") as.character(a$dominant_family_renter)
              else as.character(a$dominant_family_owner)

      tier_stmt <- tryCatch(.tier_statement(tkey, tier), error = function(e) NULL)
      fam_stmt  <- tryCatch(.family_emphasis(tkey, tier, fam), error = function(e) NULL)

      tagList(
        h4(paste0(.tier_display(tier), " — ", .tenure_display(tenure()))),
        h5(paste0("Leading driver: ", .family_display(fam))),
        if (!is.null(tier_stmt)) p(tier_stmt) else NULL,
        if (!is.null(fam_stmt))  p(fam_stmt)  else NULL
      )
    })

    # --- Tenure body -----------------------------------------------------
    output$tenure_body <- renderUI({
      if (identical(tenure(), "ownership")) .ownership_layout(ns)
      else .rental_layout(ns)
    })

    # --- Map (shared) ----------------------------------------------------
    output$neighborhood_map <- renderLeaflet({
      bg_focus <- lvm_bg_geo %>% filter(GISJOIN == bg_id())
      local_area_id <- local_area %>% filter(GISJOIN_proj == bg_id())
      local_area_ct <- lvm_ct_geo %>% filter(GISJOIN %in% local_area_id$GISJOIN_CT)
      bbox <- st_bbox(local_area_ct)
      xview <- mean(c(bbox[1] + .02, bbox[3]))
      yview <- mean(c(bbox[2], bbox[4]))
      leaflet() %>%
        addTiles() %>%
        setView(lng = xview, lat = yview, zoom = 13) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(data = local_area_ct, color = 'black', fillOpacity = .2, weight = 2) %>%
        addPolygons(data = bg_focus,      color = "black", fillOpacity = .6, weight = 2) %>%
        addPolygons(data = lvm_ma_geo,    color = "darkred", fillOpacity = .01, weight = 2,
                    label = ~mkt_area) %>%
        addMiniMap(tiles = providers$Esri.WorldGrayCanvas, position = 'topright',
                   width = 150, height = 150, toggleDisplay = FALSE, zoomLevelOffset = -4)
    })

    # --- Composition change (shared) -------------------------------------
    output$ethrace_plot <- renderPlot({
      area_pop() %>%
        mutate(p_black = round(pop_black / pop * 100, 1),
               p_white = round(pop_white / pop * 100, 1),
               p_latino = round(pop_latino / pop * 100, 1),
               Area = factor(Area)) %>%
        select(GISJOIN_proj, data_yr, pop_black, pop_white, pop_latino,
               p_black, p_white, p_latino, Area) %>%
        ggplot() +
        geom_line(aes(x = data_yr, y = p_black,  linetype = Area, color = "Black"),           linewidth = 1.1) +
        geom_line(aes(x = data_yr, y = p_latino, linetype = Area, color = "Latino/Hispanic"), linewidth = 1.1) +
        geom_line(aes(x = data_yr, y = p_white,  linetype = Area, color = "White"),           linewidth = 1.1) +
        labs(x = "Year", y = "% of population") +
        scale_color_manual(values = colors_er) +
        theme_bw() +
        theme(text = element_text(size = 14),
              legend.box = "vertical",
              legend.position = "bottom") +
        guides(color = guide_legend("Group", nrow = 1),
               linetype = guide_legend(nrow = 1))
    })
    output$ethrace_change <- renderText({
      a <- area()
      paste0("Rapid changes in demographic composition are an indicator of ",
             "gentrification because composition otherwise tends to change ",
             "slowly. The classifier looks at the change in the Black ",
             "population from 2000 to 2020. The share of Black residents in ",
             "this area changed by ",
             round(a$blackpct_ch_00_20 * 100, 1),
             " percentage points, and the Black population changed by ",
             (a$black_change00_10 + a$black_change10_20),
             " over the same period.")
    })

    # --- Rental: rent / income / education / market-tightness ------------
    output$rents <- renderPlot({
      a <- area()
      two_year_bar(a$median_rent_10, a$median_rent_20, "Median rent (2024$)")
    })
    output$rent_change <- renderText({
      a <- area()
      paste0("Rapid rent appreciation is the leading renter-displacement signal. ",
             "The classifier looks at both the level of rents and recent rent change, ",
             "relative to other Louisville block groups. ",
             .rank_line(a$rank_rents,  "Rent level rank"), ". ",
             .rank_line(a$rank_rents2, "Rent change rank"), ".")
    })

    output$hhinc <- renderPlot({
      a <- area()
      two_year_bar(a$median_hhinc_10, a$median_hhinc_20, "Median household income (2024$)")
    })
    output$income_change <- renderText({
      a <- area()
      paste0("Rapid income increases are associated with the entry of higher-income ",
             "residents rather than income growth among existing residents. ",
             "The classifier considers three income-related signals together as ",
             "composite indicators of neighborhood change. ",
             .rank_line(a$rank_hhinc,  "Median income rank"), ". ",
             .rank_line(a$rank_hi_inc, "High-income share rank"), ". ",
             .rank_line(a$rank_lo_inc, "Low-income share rank",
                        threshold_name = "q1_cutoff",
                        band_label = "bottom quintile",
                        direction = "below"), ".")
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
      a <- area()
      paste0("Rapid increases in the college-educated adult share signal a ",
             "market that may favour rent or price increases — people with ",
             "college degrees tend to have higher income-growth potential. ",
             .rank_line(a$rank_college, "College-share growth rank"), ".")
    })

    output$market_tightness_text <- renderText({
      a <- area()
      paste0("Market-tightness signals confirm an early-stage appreciation trend. ",
             "The classifier considers four indicators together — housing tightness, ",
             "vacancy change, renter share, and demolition permits — each measured ",
             "against the above-median band. ",
             .rank_line(a$rank_housing_tight, "Housing-tightness rank",
                        threshold_name = "above_med", band_label = "above-median band"), ". ",
             .rank_line(a$rank_vacant_ch, "Vacancy-change rank",
                        threshold_name = "above_med", band_label = "above-median band"), ". ",
             .rank_line(a$rank_renter_p, "Renter-share rank",
                        threshold_name = "above_med", band_label = "above-median band"), ". ",
             .rank_line(a$rank_permits_demo, "Demolition-permit rank",
                        threshold_name = "above_med", band_label = "above-median band"), ".")
    })

    # --- Ownership: foreclosure (PRIORITY) -------------------------------
    output$foreclosure_summary <- renderText({
      a <- area()
      rate <- if (!is.null(a$foreclosure_rate) && !is.na(a$foreclosure_rate))
        sprintf("%.1f foreclosures per 1,000 owners", a$foreclosure_rate) else
        "rate not available"
      paste0(rate, ". Rank ",
             ifelse(is.na(a$rank_foreclosure), "(NA)", a$rank_foreclosure),
             "/100 in Louisville for foreclosure intensity.")
    })
    output$foreclosure_interp <- renderText({
      a <- area()
      in_band <- !is.na(a$rank_foreclosure) && a$rank_foreclosure > risk_params$q4_cutoff
      paste0("Foreclosure activity is a direct indicator of owner financial distress. ",
             "The classifier treats this as a separate path to high risk — distinct ",
             "from price appreciation — so an area with elevated foreclosure activity ",
             "can be flagged as high risk even when home values aren't rising rapidly. ",
             if (in_band) "This area's foreclosure rank is in the top quintile."
             else "This area's foreclosure rank is outside the top quintile.")
    })

    # --- Ownership: permits (PRIORITY) -----------------------------------
    output$permits_new_text <- renderText({
      a <- area()
      paste0("New construction — ",
             .rank_line(a$rank_permits_new, "rank"), ".")
    })
    output$permits_renov_text <- renderText({
      a <- area()
      paste0("Renovation — ",
             .rank_line(a$rank_permits_renov, "rank"), ".")
    })
    output$permits_demo_text <- renderText({
      a <- area()
      paste0("Demolition — ",
             .rank_line(a$rank_permits_demo, "rank",
                        threshold_name = "above_med",
                        band_label = "above-median band"), ".")
    })
    output$permits_interp <- renderText({
      a <- area()
      in_band <- (!is.na(a$rank_permits_new)   && a$rank_permits_new   > risk_params$q4_cutoff) ||
                 (!is.na(a$rank_permits_renov) && a$rank_permits_renov > risk_params$q4_cutoff)
      paste0("Rapid permit activity is a leading indicator of incoming displacement ",
             "pressure, even before home values rise. The classifier treats top-quintile ",
             "new-construction or renovation activity as a standalone path to high risk. ",
             if (in_band) "This area's permit activity is in the top quintile for new construction or renovation."
             else "This area's permit activity is outside the top quintile for both new construction and renovation.")
    })

    # --- Ownership: HV + MLS ---------------------------------------------
    output$hv_plot <- renderPlot({
      a <- area()
      two_year_bar(a$median_hv_10, a$median_hv_20, "Median home value (2024$)")
    })
    output$hv_text <- renderText({
      a <- area()
      paste0("Home-value appreciation is the primary owner-side appreciation signal. ",
             .rank_line(a$rank_HV, "HV rank"), ".")
    })
    output$mls_text <- renderText({
      a <- area()
      growth_pct <- if (!is.null(a$mls_price_growth) && !is.na(a$mls_price_growth))
        sprintf("%.0f%%", a$mls_price_growth * 100) else "data not available"
      paste0("MLS sales-price growth 2019→2021: ", growth_pct, ". ",
             .rank_line(a$rank_mls_growth, "MLS growth rank"), ".")
    })

    # --- Ownership: owner vulnerability ----------------------------------
    output$owner_vuln_text <- renderText({
      a <- area()
      cb     <- if (!is.na(a$owner_costburden_p))  round(a$owner_costburden_p  * 100, 0) else NA
      lt     <- if (!is.na(a$owner_longtenure_p))  round(a$owner_longtenure_p  * 100, 0) else NA
      hmda   <- if (!is.na(a$hmda_denial_rate))    round(a$hmda_denial_rate    * 100, 0) else NA
      parts <- c()
      if (!is.na(cb))   parts <- c(parts, sprintf("%d%% of owners cost-burdened", cb))
      if (!is.na(lt))   parts <- c(parts, sprintf("%d%% long-tenured (>=10 years)", lt))
      if (!is.na(hmda)) parts <- c(parts, sprintf("%d%% HMDA mortgage denial rate", hmda))
      paste0("Owner-vulnerability context: ",
             paste(parts, collapse = "; "), ". ",
             "The classifier's owner-vulnerability gate requires long-tenure share >= ",
             round(risk_params$longtenure_min * 100, 0),
             "% AND either cost-burdened share >= ",
             round(risk_params$costburden_min * 100, 0),
             "% OR HMDA denial >= ",
             round(risk_params$hmda_denial_min * 100, 0),
             "% (the latter pair is the credit-restricted alternative).")
    })

  })
}
