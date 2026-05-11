# Pure rental-project recommender. Sourced by DHNA/app.R.
#
# recommend_project(risk_level, project_size, affordable_units, area) -> list
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
# Returns: list(
#   valid       logical, FALSE if affordable mix exceeds project size
#   error       character or NULL — user-facing error message when invalid
#   context     list — display values for the four summary cards
#                 (renters_20, renter_p_20, risk_level, cost_burden_pct,
#                  majority_level, majority_pct_renters)
#   aff_summary data.frame — aff_level, aff_pct (% local renters affording),
#                            aff_proj (cumulative project units)
#   messages    character — user-facing recommendation messages
# )
#
# Behavioral notes:
#   - For high-risk areas, only the cost-burden criterion is rendered, matching
#     current app behavior. The original code also computed (but discarded) an
#     "all units must be affordable" criterion (A.3 from the robustness review)
#     and a combined pass/fail message — both omitted here. Restoring them is
#     a separate product decision.
#   - Magic thresholds (50% affordability cutoff, 10% low-risk floor) are
#     inline; Goal #4 will revisit.
recommend_project <- function(risk_level, project_size, affordable_units, area) {

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
