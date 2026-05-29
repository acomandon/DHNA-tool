# Tenure-aware rule-based displacement risk classifiers.
# Phase 4.4 — matrix-driven interpreter.
#
# Rule structure lives in R/risk_matrix.R (`risk_matrix`); threshold
# constants in R/config.R (`risk_params`); regional forecast modulator in
# R/config.R (`forecast`). The classifiers walk the matrix to produce
# (tier, dominant_family) per BG.
#
# Outputs:
#   classify_risk_renter(bg_ct_data) -> GISJOIN_proj +
#     risk_level_renter, dominant_family_renter
#   classify_risk_owner(bg_ct_data)  -> GISJOIN_proj +
#     risk_level_owner,  dominant_family_owner
#
# Vulnerability filter: BGs that don't pass a tenure's gates fall through
# to NA tier (handled as "low" downstream in scripts/07_risk.R, with
# dominant_family = NA).
#
# Forecast modulator: when the LWA BA+ employment growth forecast exceeds
# baseline, q4_cutoff (and the "high" end of any middle-band) is tightened
# regionally for the appreciation + expansion families. Composite,
# distress, market-tightness, and vulnerability keep static thresholds.
# Forecast does not compete for dominant_family.
#
# Medium-overrides-high preserved (principle 4): m-rules describe
# post-transition state; if both fire, BG is past the early-stage crisis.

# =====================================================================
# Threshold resolution + modulator
# =====================================================================

.forecast_adjustment_ppt <- function(forecast_cfg) {
  if (is.null(forecast_cfg)) return(0)
  raw <- (forecast_cfg$lwa_ba_growth_pct - forecast_cfg$baseline_pct) *
           forecast_cfg$sensitivity * 100
  max(0, min(forecast_cfg$max_adjustment_ppt, raw))
}

.resolve_threshold <- function(threshold_ref, family, params, forecast_cfg) {
  modulated <- isTRUE(family %in% (risk_matrix$forecast_modulator$affects %||% character(0)))
  adj <- if (modulated) .forecast_adjustment_ppt(forecast_cfg) else 0

  if (is.list(threshold_ref)) {
    # Between: only the high end gets tightened (low end is the band floor)
    list(low  = params[[threshold_ref$low]],
         high = params[[threshold_ref$high]] - adj)
  } else {
    params[[threshold_ref]] - adj
  }
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# =====================================================================
# Condition evaluator (recursive)
# =====================================================================

.eval_condition <- function(cond, df, family, params, forecast_cfg) {
  if (!is.null(cond$combiner)) {
    parts <- lapply(cond$conditions, .eval_condition, df = df,
                    family = family, params = params, forecast_cfg = forecast_cfg)
    mat <- do.call(cbind, parts)
    switch(cond$combiner,
           "any" = apply(mat, 1, any, na.rm = FALSE),
           "all" = apply(mat, 1, all, na.rm = FALSE),
           stop("Unknown combiner: ", cond$combiner))
  } else {
    if (is.null(df[[cond$input]])) return(rep(NA, nrow(df)))
    x   <- df[[cond$input]]
    thr <- .resolve_threshold(cond$threshold_ref, family, params, forecast_cfg)
    switch(cond$direction,
           "above"   = x >  thr,
           "below"   = x <  thr,
           "between" = x >  thr$low & x <= thr$high,
           stop("Unknown direction: ", cond$direction))
  }
}

# Trigger result: returns a list with $fired (logical vector) and $family
.eval_trigger <- function(trig, df, params, forecast_cfg) {
  inputs <- .gather_inputs(trig$condition)
  missing_input <- !all(inputs %in% names(df))

  if (missing_input && isTRUE(trig$optional)) {
    return(list(fired = rep(FALSE, nrow(df)), family = trig$family))
  }
  if (missing_input) {
    warning("Required column(s) missing for non-optional trigger: ",
            paste(setdiff(inputs, names(df)), collapse = ", "))
    return(list(fired = rep(FALSE, nrow(df)), family = trig$family))
  }

  fired <- .eval_condition(trig$condition, df, trig$family, params, forecast_cfg)
  fired[is.na(fired)] <- FALSE
  list(fired = fired, family = trig$family)
}

.gather_inputs <- function(cond) {
  if (!is.null(cond$combiner)) {
    unique(unlist(lapply(cond$conditions, .gather_inputs)))
  } else cond$input
}

# Rule result: returns a list with $fired (logical) + $families_fired
# (list of character vectors per row — which families had a fired trigger
# in this rule). Gate is applied last (AND-ed with the combiner result).
.eval_rule <- function(rule, df, params, forecast_cfg, fired_rules) {
  trig_results <- lapply(rule$triggers, .eval_trigger,
                         df = df, params = params, forecast_cfg = forecast_cfg)
  fired_mat <- do.call(cbind, lapply(trig_results, `[[`, "fired"))
  families  <- vapply(trig_results, `[[`, character(1), "family")

  rule_fired <- switch(rule$combiner,
    "any"      = apply(fired_mat, 1, any),
    "all"      = apply(fired_mat, 1, all),
    "count_ge" = rowSums(fired_mat) >= params[[rule$count_threshold_ref]],
    stop("Unknown rule combiner: ", rule$combiner))

  # Use [[ throughout to avoid $ partial-matching (e.g. rule$gate would
  # otherwise match rule$gate_rule when only the latter exists).
  if (!is.null(rule[["gate"]])) {
    gate_ok <- .eval_condition(rule[["gate"]], df, family = NULL,
                               params = params, forecast_cfg = forecast_cfg)
    gate_ok[is.na(gate_ok)] <- FALSE
    rule_fired <- rule_fired & gate_ok
  }

  # gate_rule: rule fires only if another named rule has also fired.
  # Resolved by passing the fired_rules accumulator.
  if (!is.null(rule[["gate_rule"]])) {
    gating <- fired_rules[[rule[["gate_rule"]]]]
    if (is.null(gating)) {
      stop("gate_rule references unknown rule: ", rule[["gate_rule"]])
    }
    rule_fired <- rule_fired & gating
  }

  # families fired per row: character vector of unique family names whose
  # triggers fired in this rule for this BG.
  families_per_row <- apply(fired_mat, 1, function(v) unique(families[v]))

  list(fired = rule_fired, families_per_row = families_per_row)
}

# =====================================================================
# Tier resolution + dominant_family
# =====================================================================

.eval_vulnerability <- function(vuln, df, params, forecast_cfg) {
  gate_results <- lapply(vuln$gates, function(g) {
    .eval_condition(list(combiner = g$combiner, conditions = g$conditions),
                    df = df, family = NULL, params = params,
                    forecast_cfg = forecast_cfg)
  })
  mat <- do.call(cbind, gate_results)
  mat[is.na(mat)] <- FALSE
  apply(mat, 1, switch(vuln$combiner, "any" = any, "all" = all))
}

.classify_tenure <- function(tenure, bg_ct_data, params, forecast_cfg) {
  matrix_t  <- risk_matrix$rules[[tenure]]
  vuln      <- risk_matrix$vulnerability[[tenure]]
  priority  <- risk_matrix$priority_order[[tenure]]

  passes <- .eval_vulnerability(vuln, bg_ct_data, params, forecast_cfg)
  out <- tibble::tibble(
    GISJOIN_proj    = bg_ct_data$GISJOIN_proj,
    tier            = NA_character_,
    dominant_family = NA_character_
  )

  if (!any(passes)) return(out)
  idx <- which(passes)
  df  <- bg_ct_data[idx, , drop = FALSE]

  # Walk rules. Order matters for `gate_rule`: evaluate h1 before h3.
  fired_rules        <- list()
  rule_families      <- list()
  tier_rule_owners   <- list()   # which tier each rule belongs to

  for (tier_name in c("medium", "high")) {
    for (rule_name in names(matrix_t[[tier_name]])) {
      rule <- matrix_t[[tier_name]][[rule_name]]
      res  <- .eval_rule(rule, df, params, forecast_cfg, fired_rules)
      fired_rules[[rule_name]]      <- res$fired
      rule_families[[rule_name]]    <- res$families_per_row
      tier_rule_owners[[rule_name]] <- tier_name
    }
  }

  # Tier per BG: any high rule fired → high; any medium → medium; else low.
  # Medium overrides high when both fire.
  high_rules <- names(tier_rule_owners)[vapply(tier_rule_owners, identical, logical(1), "high")]
  med_rules  <- names(tier_rule_owners)[vapply(tier_rule_owners, identical, logical(1), "medium")]

  any_high <- if (length(high_rules)) Reduce(`|`, fired_rules[high_rules]) else rep(FALSE, nrow(df))
  any_med  <- if (length(med_rules))  Reduce(`|`, fired_rules[med_rules])  else rep(FALSE, nrow(df))

  tier_v <- ifelse(any_med, "medium",
                   ifelse(any_high, "high", "low"))

  # Dominant family per BG: take all rules that fired AT THE WINNING TIER,
  # union the families_per_row, then pick the highest-priority family.
  dominant_v <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    winning_tier <- tier_v[i]
    if (winning_tier == "low") {
      dominant_v[i] <- NA_character_
      next
    }
    winning_rules <- if (winning_tier == "medium") med_rules else high_rules
    fams <- character(0)
    for (rn in winning_rules) {
      if (isTRUE(fired_rules[[rn]][i])) {
        fams <- c(fams, rule_families[[rn]][[i]])
      }
    }
    fams <- unique(fams)
    if (length(fams) == 0) {
      dominant_v[i] <- NA_character_
    } else {
      hit <- intersect(priority, fams)
      dominant_v[i] <- if (length(hit) > 0) hit[1] else fams[1]
    }
  }

  out$tier[idx]            <- tier_v
  out$dominant_family[idx] <- dominant_v
  out
}

# =====================================================================
# Public API
# =====================================================================

classify_risk_renter <- function(bg_ct_data) {
  res <- .classify_tenure("renter", bg_ct_data, risk_params, forecast)
  res$tier <- factor(ifelse(is.na(res$tier), "low", res$tier),
                     levels = c("low", "medium", "high"))
  res$dominant_family <- factor(res$dominant_family,
                                levels = risk_matrix$priority_order$renter)
  names(res)[names(res) == "tier"]            <- "risk_level_renter"
  names(res)[names(res) == "dominant_family"] <- "dominant_family_renter"
  res
}

classify_risk_owner <- function(bg_ct_data) {
  res <- .classify_tenure("owner", bg_ct_data, risk_params, forecast)
  res$tier <- factor(ifelse(is.na(res$tier), "low", res$tier),
                     levels = c("low", "medium", "high"))
  res$dominant_family <- factor(res$dominant_family,
                                levels = risk_matrix$priority_order$owner)
  names(res)[names(res) == "tier"]            <- "risk_level_owner"
  names(res)[names(res) == "dominant_family"] <- "dominant_family_owner"
  res
}
