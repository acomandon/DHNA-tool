# Pure recommender functions for rental and ownership projects.
# Sourced by DHNA/global.R.
#
# Magic thresholds (50% affordability cutoff, 10% low-risk floor) are inline;
# Goal #4 will revisit.

# --- Rental ---------------------------------------------------------------
# recommend_project_rental(risk_level, project_size, affordable_units, area) -> list
#
# Inputs:
#   risk_level         "low" | "medium" | "high" (string or factor)
#   project_size       total units in the proposed project (numeric, >= 1)
#   affordable_units   named list with elements ami30, ami50, ami60, ami70,
#                      ami80 — units reserved at each affordability tier
#   area               1-row data frame from adat_data filtered to the
#                      project's block group, containing: renters_20,
#                      renter_p_20, cost_burden30_20_p, all_renters_ct,
#                      renters30_ct, renters50_ct, renters60_ct, renters70_ct,
#                      renters80_ct
#
# Returns: list(valid, error, context, aff_summary, messages)
#
# Behavioral notes:
#   - For high-risk areas, only the cost-burden criterion is rendered, matching
#     current app behavior. The original code also computed (but discarded) an
#     "all units must be affordable" criterion (A.3 from the robustness review)
#     and a combined pass/fail message — both omitted here. Restoring them is
#     a separate product decision.
recommend_project_rental <- function(risk_level, project_size, affordable_units, area) {

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

  context <- list(
    renters_20 = area$renters_20,
    renter_p_20 = round(area$renter_p_20 * 100, 0),
    risk_level = as.character(risk_level),
    cost_burden_pct = cost_burden_pct,
    majority_level = majority_level,
    majority_pct_renters = majority_pct_renters
  )

  rl <- as.character(risk_level)
  if (rl == "high") {
    messages <- if (majority_pct_proj >= cost_burden_pct) {
      paste0(majority_pct_proj, "% of units are affordable at ", majority_level,
             ". The project meets the requirement that the share of units affordable",
             " to at least half the renter population be greater than or equal to",
             " the percent of housing cost burdened households.")
    } else {
      paste0(majority_pct_proj, "% of units are affordable at ", majority_level,
             ". The share of units affordable to at least half the renter",
             " population must be greater than or equal to the ", cost_burden_pct,
             "% of housing cost burdened households")
    }
  } else if (rl == "medium") {
    messages <- if (majority_pct_proj >= cost_burden_pct) {
      paste0(majority_pct_proj, "% of units are affordable at ", majority_level,
             ". The project meets the requirement that the share of units affordable",
             " to at least half the renter population be greater than or equal to",
             " the percent of housing cost burdened households.",
             " This project is recommended for support.")
    } else {
      paste0(majority_pct_proj, "% of units are affordable at ", majority_level,
             ". The share of units affordable to at least half the renter",
             " population must be greater than or equal to the ", cost_burden_pct,
             "% of housing cost burdened households.",
             " This project is not recommended for support until it meets this condition.")
    }
  } else { # low
    messages <- if ((aff_proj[2] / project_size) >= 0.1) {
      paste0(round(aff_proj[2] / project_size * 100, 0),
             "% of units are affordable at ", aff_levels[2],
             ". The project meets the requirement that the share of units affordable",
             " at 50% AMI or below be greater than or equal to 10% of all units.",
             " This project is recommended for support.")
    } else {
      paste("Projects in low risk areas must include at least 10% of units affordable",
            "at 50% AMI or below. This project is not recommended for",
            "support until it meets this condition.")
    }
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
# recommend_project_ownership(risk_level, project_size, ownership_inputs, area,
#                             hud_max_price) -> list
#
# Inputs:
#   risk_level        "low" | "medium" | "high" (string or factor)
#   project_size      total units in the proposed project (numeric, >= 1)
#   ownership_inputs  named list with elements:
#                       program_name      (character)
#                       home_type         ("existing" | "new")
#                       assisted_units    (integer, >= 0)
#                       reserved_units    (integer, >= 0)
#                       reserved_price    (numeric, > 0 if reserved_units > 0)
#                       eligibility_tags  (character vector, informational)
#   area              1-row data frame from adat_data filtered to the project's
#                     block group, containing: renter_p_20, owner_20_ct,
#                     renter_20_ct, cost_burden30_20_p
#   hud_max_price     HUD HOME/HTF max purchase price for the chosen home_type
#                     (1-unit dwelling), in dollars
#
# Returns: list(valid, error, context, gate_info, messages)
#
# Rules (mirror the rental risk-tiered structure, using the combined
# household cost-burden rate):
#   - reserved_units only count toward the cost-burden threshold if
#     reserved_price <= hud_max_price ("price gate"). Otherwise they're
#     treated as 0 reserved with an explanatory message.
#   - high:   assisted_units == project_size AND
#             valid_reserved >= ceiling(cost_burden_rate * project_size)
#   - medium: valid_reserved >= ceiling(cost_burden_rate * project_size)
#   - low:    assisted_units >= ceiling(0.1 * project_size)
recommend_project_ownership <- function(risk_level, project_size, ownership_inputs,
                                        area, hud_max_price) {

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

  # Price gate: reserved units only "count" if priced at or below the HUD
  # affordable-for-sale limit for the chosen home type.
  price_gate_passes <- if (reserved_units > 0) {
    reserved_price <= hud_max_price
  } else {
    TRUE
  }
  valid_reserved <- if (price_gate_passes) reserved_units else 0L

  # Context values for the summary boxes
  owner_p_20      <- round((1 - area$renter_p_20) * 100, 0)
  owner_count     <- area$owner_20_ct
  cost_burden_pct <- round(area$cost_burden30_20_p * 100, 0)
  cost_burden_required <- ceiling(area$cost_burden30_20_p * project_size)
  low_assist_required  <- ceiling(0.1 * project_size)
  home_type_label <- if (identical(home_type, "new")) "new construction" else "existing home"

  context <- list(
    owner_count        = owner_count,
    owner_p_20         = owner_p_20,
    risk_level         = as.character(risk_level),
    cost_burden_pct    = cost_burden_pct,
    hud_max_price      = hud_max_price,
    home_type_label    = home_type_label,
    reserved_price     = reserved_price,
    reserved_units     = reserved_units,
    valid_reserved     = valid_reserved,
    assisted_units     = assisted_units,
    price_gate_passes  = price_gate_passes,
    program_name       = program_name,
    eligibility_tags   = eligibility_tags
  )

  gate_info <- list(
    reserved_entered = reserved_units,
    reserved_valid   = valid_reserved,
    reserved_price   = reserved_price,
    hud_max          = hud_max_price,
    gate_passes      = price_gate_passes,
    home_type        = home_type
  )

  # Build messages: price-gate flag first (if applicable), then risk-tier rule.
  fmt_money <- function(x) paste0("$", format(round(x), big.mark = ",", scientific = FALSE))
  messages <- character()

  if (reserved_units > 0 && !price_gate_passes) {
    messages <- c(messages, paste0(
      "The reserved-units price of ", fmt_money(reserved_price),
      " exceeds the HUD affordable-for-sale limit of ", fmt_money(hud_max_price),
      " for ", home_type_label, "s. These units are not counted as reserved",
      " for the recommendation."))
  }

  rl <- as.character(risk_level)
  reserved_pct  <- if (project_size > 0) round(valid_reserved / project_size * 100, 0) else 0
  assisted_pct  <- if (project_size > 0) round(assisted_units / project_size * 100, 0) else 0
  prog_clause   <- if (nzchar(program_name)) paste0(" (", program_name, ")") else ""

  if (rl == "high") {
    full_coverage   <- assisted_units == project_size
    enough_reserved <- valid_reserved >= cost_burden_required
    if (full_coverage && enough_reserved) {
      messages <- c(messages, paste0(
        "All ", project_size, " units are covered by the assistance program",
        prog_clause, " and ", valid_reserved, " of ", project_size,
        " units (", reserved_pct, "%) are reserved for existing residents at",
        " an affordable price. The project meets the high-risk requirement",
        " that all units be assisted and the share of reserved units meet the ",
        cost_burden_pct, "% cost-burden rate."))
    } else if (!full_coverage) {
      messages <- c(messages, paste0(
        "Only ", assisted_units, " of ", project_size, " units are covered by",
        " the assistance program. High-risk areas require ALL units to be",
        " covered. The project does not meet the requirement."))
    } else {
      messages <- c(messages, paste0(
        "Only ", valid_reserved, " of ", project_size, " units are reserved for",
        " existing residents at an affordable price. The cost-burden rate",
        " requires at least ", cost_burden_required, " reserved units (",
        cost_burden_pct, "% of ", project_size, "). The project does not meet",
        " the requirement."))
    }
  } else if (rl == "medium") {
    if (valid_reserved >= cost_burden_required) {
      messages <- c(messages, paste0(
        valid_reserved, " of ", project_size, " units (", reserved_pct,
        "%) are reserved for existing residents at an affordable price.",
        " The project meets the medium-risk requirement that the share of",
        " reserved units be at least the ", cost_burden_pct,
        "% cost-burden rate. This project is recommended for support."))
    } else {
      messages <- c(messages, paste0(
        valid_reserved, " of ", project_size, " units are reserved for existing",
        " residents at an affordable price. The cost-burden rate requires at",
        " least ", cost_burden_required, " reserved units (",
        cost_burden_pct, "% of ", project_size, "). This project is not",
        " recommended for support until it meets this condition."))
    }
  } else { # low
    if (assisted_units >= low_assist_required) {
      messages <- c(messages, paste0(
        assisted_units, " of ", project_size, " units (", assisted_pct,
        "%) are covered by the assistance program", prog_clause,
        ". The project meets the low-risk requirement that at least 10% of",
        " units be covered. This project is recommended for support."))
    } else {
      messages <- c(messages, paste0(
        "Projects in low-risk areas must include at least 10% of units covered",
        " by an ownership assistance program. This project covers ",
        assisted_units, " of ", project_size, " (", assisted_pct,
        "%). It is not recommended for support until it meets this condition."))
    }
  }

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
