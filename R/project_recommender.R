# Pure recommender functions for rental and ownership projects.
# Sourced by DHNA/global.R.
#
# Phase 4.4.2: messages are now structured as
#   1. tier-mechanism statement (what the tier means for policy)
#   2. dominant_family emphasis (which driver should shape project features)
#   3. criteria evaluation (one line per pass/fail criterion)
#   4. final verdict (recommended / not recommended)
# Rental-high also re-enables the "all units at or below 80% AMI" criterion
# (restored from the pre-Phase-2.1 logic; A.3).
#
# The criteria themselves (pass/fail bars) are unchanged from Phase 4.4.1
# EXCEPT for rental-high, which now requires BOTH (1) every unit reserved
# at an affordability tier (any of 30/50/60/70/80% AMI) AND (2) cost-burden
# coverage — both must pass to be recommended.

# Tier-mechanism statements ------------------------------------------------
.tier_statement <- function(tenure, tier) {
  rental <- list(
    high   = paste0("High-risk neighborhood: active displacement crisis. ",
                    "Projects should provide deeply affordable units urgently — to enable ",
                    "existing residents to upgrade in place within their community and ",
                    "prevent absorption by higher-income in-movers."),
    medium = paste0("Medium-risk neighborhood: post-transition equilibrium. ",
                    "Projects should provide a mix of deeply affordable and market-rate ",
                    "units — to support those who stayed, enable displaced residents to ",
                    "return, and relieve further upmarketing pressure."),
    low    = paste0("Low-risk neighborhood: opportunity-area expansion. ",
                    "The AFFH goal is to bring lower-income access into low-poverty ",
                    "neighborhoods and add to regional supply.")
  )
  owner <- list(
    high   = paste0("High-risk neighborhood: active displacement pressure on existing owners. ",
                    "The goal is to support long-term residents to invest in their community — ",
                    "through anti-extraction tools, community wealth-building, and ",
                    "inclusionary preservation."),
    medium = paste0("Medium-risk neighborhood: post-transition market. ",
                    "The goal is to stabilize the new equilibrium through affordable ",
                    "for-sale supply, shared-equity programs, and tenure-flexibility tools."),
    low    = paste0("Low-risk neighborhood: opportunity-area expansion. ",
                    "The AFFH goal is to expand ownership access where no defense is ",
                    "needed — first-time buyer subsidies, missing-middle housing, and ",
                    "fair-housing outreach.")
  )
  switch(tenure, rental = rental[[tier]], owner = owner[[tier]])
}

# Per-dominant-family emphasis lines ---------------------------------------
.family_emphasis <- function(tenure, tier, dominant_family) {
  # Low tier always uses AFFH framing regardless of dominant_family.
  if (tier == "low" || is.na(dominant_family) || dominant_family == "") {
    return("Opportunity-area emphasis — prioritize lower-income access and fair-housing outreach.")
  }
  rental <- list(
    appreciation       = "Rent appreciation is the leading driver — prioritize deep affordability (30% AMI units) and long-term affordability covenants.",
    distress           = "Eviction pressure is the leading driver — pair affordability with tenancy protections and eviction-prevention services.",
    composite          = "Composite gentrification signals dominate — prioritize existing-resident preference and cultural preservation.",
    `market-tightness` = "Market constraints are the leading driver — supply addition matters as much as affordability.",
    expansion          = "Rapid renovation and turnover are the leading driver — include anti-flipping covenants."
  )
  owner <- list(
    distress           = "Foreclosure pressure is the leading driver — pair with foreclosure prevention and downpayment assistance for sitting renters who want to buy.",
    appreciation       = "Home-value appreciation is the leading driver — prioritize community land trusts, shared-equity homeownership, and long-term affordability covenants.",
    expansion          = "Rapid permit activity is the leading driver — pursue inclusionary zoning on new construction and counter-cyclical CLT acquisition before flippers arrive.",
    composite          = "Composite gentrification signals dominate — prioritize existing-resident purchase pathways and cultural preservation.",
    `market-tightness` = "Market constraints are the leading driver — focus on housing-supply elasticity and tenure-flexibility tools."
  )
  src <- switch(tenure, rental = rental, owner = owner)
  src[[dominant_family]] %||% "Driver: see area overview for signal details."
}

# --- Rental ---------------------------------------------------------------
# recommend_project_rental(risk_level, dominant_family, project_size,
#                          affordable_units, area) -> list
#
# Inputs:
#   risk_level         "low" | "medium" | "high" (string or factor)
#   dominant_family    one of the 5 substantive families or NA (low/no signal)
#   project_size       total units in the proposed project (numeric, >= 1)
#   affordable_units   named list with elements ami30..ami80
#   area               1-row data frame with renters_20, renter_p_20,
#                      cost_burden30_20_p, all_renters_ct, renters30_ct..80_ct
#
# Rules:
#   - high:   ALL units reserved at <=80% AMI AND
#             share-at-majority-AMI >= cost-burden share
#             (both must pass to be recommended; A.3 restored)
#   - medium: share-at-majority-AMI >= cost-burden share
#   - low:    at least 10% of units at <=50% AMI
recommend_project_rental <- function(risk_level, dominant_family, project_size,
                                     affordable_units, area) {

  affordable_total <- sum(unlist(affordable_units))
  if (affordable_total > project_size) {
    return(list(
      valid = FALSE,
      error = "Please check that the number of affordable units is no larger than the total number of units",
      context = NULL,
      aff_summary = NULL,
      messages = NULL
    ))
  }

  aff_levels <- c("30% AMI", "50% AMI", "60% AMI", "70% AMI", "80% AMI")
  aff_pct <- round(c(area$renters30_ct, area$renters50_ct, area$renters60_ct,
                     area$renters70_ct, area$renters80_ct) / area$all_renters_ct * 100, 0)
  aff_proj <- cumsum(c(affordable_units$ami30, affordable_units$ami50,
                       affordable_units$ami60, affordable_units$ami70,
                       affordable_units$ami80))
  aff_summary <- data.frame(aff_level = aff_levels, aff_pct = aff_pct, aff_proj = aff_proj)

  majority_idx <- min(which(aff_pct > 50))
  majority_level <- aff_levels[majority_idx]
  majority_pct_renters <- aff_pct[majority_idx]
  majority_pct_proj <- round(aff_proj[majority_idx] / project_size * 100, 0)
  cost_burden_pct <- round(area$cost_burden30_20_p * 100, 0)

  rl <- as.character(risk_level)
  df <- if (is.null(dominant_family) || length(dominant_family) == 0) NA_character_
        else as.character(dominant_family)

  context <- list(
    renters_20 = area$renters_20,
    renter_p_20 = round(area$renter_p_20 * 100, 0),
    risk_level = rl,
    dominant_family = df,
    cost_burden_pct = cost_burden_pct,
    majority_level = majority_level,
    majority_pct_renters = majority_pct_renters
  )

  tier_stmt   <- .tier_statement("rental", rl)
  family_stmt <- .family_emphasis("rental", rl, df)

  if (rl == "high") {
    # Criterion 1 (A.3 restored): every unit is reserved at some AMI tier
    # (i.e. <= 80% AMI). aff_proj[5] is the cumulative count across all
    # five tiers; equality with project_size means all units are affordable.
    crit1_pass <- aff_proj[5] >= project_size
    crit1_msg  <- if (crit1_pass) {
      "All units are affordable at 80% AMI or below — meets the all-affordable requirement."
    } else {
      sprintf("Only %d of %d units are affordable at 80%% AMI or below. High-risk areas require every unit to be affordable.",
              aff_proj[5], project_size)
    }
    # Criterion 2: cost-burden coverage
    crit2_pass <- majority_pct_proj >= cost_burden_pct
    crit2_msg  <- if (crit2_pass) {
      sprintf("%d%% of units are affordable at %s — meets the cost-burden coverage requirement (%d%% of households are cost-burdened).",
              majority_pct_proj, majority_level, cost_burden_pct)
    } else {
      sprintf("%d%% of units are affordable at %s. The share affordable to a majority of renters must be at least the cost-burden rate (%d%%).",
              majority_pct_proj, majority_level, cost_burden_pct)
    }
    verdict <- if (crit1_pass && crit2_pass) {
      "This project is recommended for support."
    } else {
      "This project is not recommended for support until both conditions are met."
    }
    messages <- c(tier_stmt, family_stmt, crit1_msg, crit2_msg, verdict)

  } else if (rl == "medium") {
    crit_pass <- majority_pct_proj >= cost_burden_pct
    crit_msg  <- if (crit_pass) {
      sprintf("%d%% of units are affordable at %s — meets the cost-burden coverage requirement (%d%% of households are cost-burdened).",
              majority_pct_proj, majority_level, cost_burden_pct)
    } else {
      sprintf("%d%% of units are affordable at %s. The share affordable to a majority of renters must be at least the cost-burden rate (%d%%).",
              majority_pct_proj, majority_level, cost_burden_pct)
    }
    verdict <- if (crit_pass) {
      "This project is recommended for support."
    } else {
      "This project is not recommended for support until this condition is met."
    }
    messages <- c(tier_stmt, family_stmt, crit_msg, verdict)

  } else { # low
    low_share <- round(aff_proj[2] / project_size * 100, 0)
    crit_pass <- (aff_proj[2] / project_size) >= 0.1
    crit_msg  <- if (crit_pass) {
      sprintf("%d%% of units are affordable at 50%% AMI or below — meets the 10%% opportunity-area floor.",
              low_share)
    } else {
      sprintf("%d%% of units are affordable at 50%% AMI or below. Low-risk projects must include at least 10%% of units at 50%% AMI or below.",
              low_share)
    }
    verdict <- if (crit_pass) {
      "This project is recommended for support."
    } else {
      "This project is not recommended for support until this condition is met."
    }
    messages <- c(tier_stmt, family_stmt, crit_msg, verdict)
  }

  list(
    valid = TRUE,
    error = NULL,
    context = context,
    aff_summary = aff_summary,
    messages = messages
  )
}

# --- Ownership -----------------------------------------------------------
# recommend_project_ownership(risk_level, dominant_family, project_size,
#                             ownership_inputs, area, hud_max_price) -> list
#
# Inputs:
#   risk_level        "low" | "medium" | "high" (string or factor)
#   dominant_family   one of the 5 substantive families or NA (low/no signal)
#   project_size      total units in the proposed project (numeric, >= 1)
#   ownership_inputs  named list (program_name, home_type, assisted_units,
#                     reserved_units, reserved_price, eligibility_tags)
#   area              1-row data frame (renter_p_20, owner_20_ct,
#                     renter_20_ct, cost_burden30_20_p)
#   hud_max_price     HUD HOME/HTF max purchase price for the chosen
#                     home_type (1-unit dwelling), in dollars
#
# Criteria (unchanged from Phase 4.4.1):
#   - reserved_units count only if reserved_price <= hud_max_price (price gate)
#   - high:   assisted_units == project_size AND
#             valid_reserved >= ceiling(cost_burden_rate * project_size)
#   - medium: valid_reserved >= ceiling(cost_burden_rate * project_size)
#   - low:    assisted_units >= ceiling(0.1 * project_size)
recommend_project_ownership <- function(risk_level, dominant_family, project_size,
                                        ownership_inputs, area, hud_max_price) {

  program_name     <- ownership_inputs$program_name
  home_type        <- ownership_inputs$home_type
  assisted_units   <- as.integer(ownership_inputs$assisted_units %||% 0L)
  reserved_units   <- as.integer(ownership_inputs$reserved_units %||% 0L)
  reserved_price   <- as.numeric(ownership_inputs$reserved_price %||% 0)
  eligibility_tags <- ownership_inputs$eligibility_tags %||% character()

  if (assisted_units > project_size || reserved_units > project_size) {
    return(list(
      valid = FALSE,
      error = "Please check that the number of assisted or reserved units is no larger than the total number of units.",
      context = NULL,
      gate_info = NULL,
      messages = NULL
    ))
  }

  price_gate_passes <- if (reserved_units > 0) {
    reserved_price <= hud_max_price
  } else {
    TRUE
  }
  valid_reserved <- if (price_gate_passes) reserved_units else 0L

  owner_p_20      <- round((1 - area$renter_p_20) * 100, 0)
  owner_count     <- area$owner_20_ct
  cost_burden_pct <- round(area$cost_burden30_20_p * 100, 0)
  cost_burden_required <- ceiling(area$cost_burden30_20_p * project_size)
  low_assist_required  <- ceiling(0.1 * project_size)
  home_type_label <- if (identical(home_type, "new")) "new construction" else "existing home"

  rl <- as.character(risk_level)
  df <- if (is.null(dominant_family) || length(dominant_family) == 0) NA_character_
        else as.character(dominant_family)

  context <- list(
    owner_count       = owner_count,
    owner_p_20        = owner_p_20,
    risk_level        = rl,
    dominant_family   = df,
    cost_burden_pct   = cost_burden_pct,
    hud_max_price     = hud_max_price,
    home_type_label   = home_type_label,
    reserved_price    = reserved_price,
    reserved_units    = reserved_units,
    valid_reserved    = valid_reserved,
    assisted_units    = assisted_units,
    price_gate_passes = price_gate_passes,
    program_name      = program_name,
    eligibility_tags  = eligibility_tags
  )

  gate_info <- list(
    reserved_entered = reserved_units,
    reserved_valid   = valid_reserved,
    reserved_price   = reserved_price,
    hud_max          = hud_max_price,
    gate_passes      = price_gate_passes,
    home_type        = home_type
  )

  fmt_money <- function(x) paste0("$", format(round(x), big.mark = ",", scientific = FALSE))

  tier_stmt   <- .tier_statement("owner", rl)
  family_stmt <- .family_emphasis("owner", rl, df)

  reserved_pct <- if (project_size > 0) round(valid_reserved / project_size * 100, 0) else 0
  assisted_pct <- if (project_size > 0) round(assisted_units / project_size * 100, 0) else 0
  prog_clause  <- if (nzchar(program_name)) paste0(" (", program_name, ")") else ""

  price_gate_msg <- if (reserved_units > 0 && !price_gate_passes) {
    paste0("The reserved-units price of ", fmt_money(reserved_price),
           " exceeds the HUD affordable-for-sale limit of ", fmt_money(hud_max_price),
           " for ", home_type_label, "s. These units are not counted as reserved",
           " for the recommendation.")
  } else NULL

  if (rl == "high") {
    full_coverage   <- assisted_units == project_size
    enough_reserved <- valid_reserved >= cost_burden_required
    if (full_coverage && enough_reserved) {
      crit_msgs <- paste0("All ", project_size, " units are covered by the assistance program",
                          prog_clause, " and ", valid_reserved, " of ", project_size,
                          " units (", reserved_pct, "%) are reserved for existing residents at",
                          " an affordable price — meets the high-risk requirement.")
      verdict <- "This project is recommended for support."
    } else {
      crit_msgs <- character()
      if (!full_coverage) {
        crit_msgs <- c(crit_msgs, paste0(
          "Only ", assisted_units, " of ", project_size, " units are covered by",
          " the assistance program. High-risk areas require all units to be covered."))
      }
      if (!enough_reserved) {
        crit_msgs <- c(crit_msgs, paste0(
          "Only ", valid_reserved, " of ", project_size, " units are reserved for existing",
          " residents at an affordable price. The cost-burden rate (", cost_burden_pct,
          "%) requires at least ", cost_burden_required, " reserved units."))
      }
      verdict <- "This project is not recommended for support until these conditions are met."
    }
  } else if (rl == "medium") {
    if (valid_reserved >= cost_burden_required) {
      crit_msgs <- paste0(valid_reserved, " of ", project_size, " units (", reserved_pct,
                          "%) are reserved for existing residents at an affordable price — ",
                          "meets the medium-risk requirement (", cost_burden_pct,
                          "% cost-burden rate).")
      verdict <- "This project is recommended for support."
    } else {
      crit_msgs <- paste0(valid_reserved, " of ", project_size, " units are reserved for existing",
                          " residents at an affordable price. The cost-burden rate (",
                          cost_burden_pct, "%) requires at least ", cost_burden_required,
                          " reserved units.")
      verdict <- "This project is not recommended for support until this condition is met."
    }
  } else { # low
    if (assisted_units >= low_assist_required) {
      crit_msgs <- paste0(assisted_units, " of ", project_size, " units (", assisted_pct,
                          "%) are covered by the assistance program", prog_clause,
                          " — meets the low-risk opportunity-area requirement (at least 10% covered).")
      verdict <- "This project is recommended for support."
    } else {
      crit_msgs <- paste0("Projects in low-risk areas must include at least 10% of units covered",
                          " by an ownership assistance program. This project covers ",
                          assisted_units, " of ", project_size, " (", assisted_pct, "%).")
      verdict <- "This project is not recommended for support until this condition is met."
    }
  }

  messages <- c(tier_stmt, family_stmt, price_gate_msg, crit_msgs, verdict)
  if (length(eligibility_tags) > 0) {
    messages <- c(messages, paste0(
      "Stated eligibility focus: ", paste(eligibility_tags, collapse = "; "), "."))
  }

  list(
    valid = TRUE,
    error = NULL,
    context = context,
    gate_info = gate_info,
    messages = messages
  )
}

# Small helper for default values (avoid loading rlang just for %||%).
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
