# Declarative risk-classification matrix.
# Phase 4.4 — the matrix IS the spec.
#
# `classify_risk_renter()` and `classify_risk_owner()` in R/risk_classifier.R
# interpret this list to produce per-BG (tier, dominant_family) outputs.
# Threshold constants live in `risk_params` in R/config.R; this file carries
# only structure (which variables, in which family, contributing to which
# tier rule, with what combiner and gate).
#
# Family taxonomy (7):
#   appreciation, distress, expansion, composite, market-tightness,
#   vulnerability, forecast
# Vulnerability is a gate; forecast is a modulator. Neither competes for
# dominant_family — the 5 substantive families do.
#
# Condition schema (recursive):
#   leaf  : list(input = "<var>", threshold_ref = "<risk_params name>" |
#                                  list(low = "<name>", high = "<name>"),
#                direction = "above" | "below" | "between")
#     - "above"   : input >  threshold
#     - "below"   : input <  threshold
#     - "between" : low_thr < input <= high_thr (the h-tier "middle band")
#   group : list(combiner = "any" | "all", conditions = list(<condition>, ...))
#
# Trigger schema:
#   list(family = "<one of 5>", optional = TRUE|FALSE,
#        condition = <condition>)
#   When `optional = TRUE` and any input column is missing, the trigger
#   evaluates to FALSE (drop-through for locality-specific data slots).
#
# Rule schema:
#   list(description, triggers = list(<trigger>, ...),
#        combiner = "any" | "all" | "count_ge",
#        count_threshold_ref = <name>,   # only for count_ge
#        gate = <condition> | NULL)

risk_matrix <- list(

  # ------------------------------------------------------------------------
  # Tenure-specific priority order for dominant_family resolution.
  # When multiple families have fired triggers within the winning tier,
  # the first family in the listed order wins.
  # ------------------------------------------------------------------------
  priority_order = list(
    renter = c("appreciation", "distress", "composite",
               "market-tightness", "expansion"),
    owner  = c("distress", "appreciation", "expansion",
               "composite", "market-tightness")
  ),

  # ------------------------------------------------------------------------
  # Forecast modulator. Tightens the q4_cutoff (and middle-band thresholds
  # that reference it) for the listed families when the regional BA+
  # employment growth forecast exceeds `baseline_pct`.
  #   adjustment_ppt = max(0, min(max_adjustment_ppt,
  #                               (lwa_growth_pct - baseline_pct) *
  #                                sensitivity * 100))
  #   adjusted_q4_cutoff = q4_cutoff - adjustment_ppt
  # The classifier resolves this at runtime via the `forecast` config block.
  # Forecast does not compete for dominant_family.
  # ------------------------------------------------------------------------
  forecast_modulator = list(
    affects = c("appreciation", "expansion")
  ),

  # ------------------------------------------------------------------------
  # Vulnerability gates. A BG must pass at least one of a tenure's gates to
  # be eligible for classification at all (otherwise it falls through to
  # "low" with NA dominant_family).
  # ------------------------------------------------------------------------
  vulnerability = list(

    renter = list(
      description = "Low-2010-income BG with at least some renter base",
      gates = list(
        low_N = list(
          description = "Small renter base + low 2010 income",
          combiner = "all",
          conditions = list(
            list(input = "renters_20",      threshold_ref = "renters_n_min",   direction = "below"),
            list(input = "median_hhinc_10", threshold_ref = "med_hhinc_10_max", direction = "below")
          )
        ),
        normal = list(
          description = "Sufficient renter base + low 2010 income",
          combiner = "all",
          conditions = list(
            list(input = "renters_20",      threshold_ref = "renters_n_min",   direction = "above"),
            list(input = "median_hhinc_10", threshold_ref = "med_hhinc_10_max", direction = "below")
          )
        )
      ),
      combiner = "any"
    ),

    owner = list(
      description = "Vulnerable owner-occupied BG (low-income, long-tenured, cost-burdened or credit-restricted)",
      gates = list(
        low_N = list(
          description = "Small owner base + low 2010 income + long-tenured majority",
          combiner = "all",
          conditions = list(
            list(input = "owner_20_ct",         threshold_ref = "owners_n_min",    direction = "below"),
            list(input = "median_hhinc_10",     threshold_ref = "med_hhinc_10_max", direction = "below"),
            list(input = "owner_longtenure_p",  threshold_ref = "longtenure_min",   direction = "above")
          )
        ),
        normal = list(
          description = "Sufficient owner base + low 2010 income + long-tenured + (cost-burdened OR HMDA-denied)",
          combiner = "all",
          conditions = list(
            list(input = "owner_20_ct",         threshold_ref = "owners_n_min",    direction = "above"),
            list(input = "median_hhinc_10",     threshold_ref = "med_hhinc_10_max", direction = "below"),
            list(input = "owner_longtenure_p",  threshold_ref = "longtenure_min",   direction = "above"),
            list(combiner = "any", conditions = list(
              list(input = "owner_costburden_p", threshold_ref = "costburden_min",  direction = "above"),
              list(input = "hmda_denial_rate",   threshold_ref = "hmda_denial_min", direction = "above")
            ))
          )
        )
      ),
      combiner = "any"
    )
  ),

  # ------------------------------------------------------------------------
  # Tier rules. Each rule has `triggers` (family-tagged conditions),
  # `combiner` (how triggers combine), and an optional `gate` (extra
  # structural condition the BG must also satisfy).
  #
  # Tier semantics (option γ):
  #   high   = active displacement crisis (early-stage signals fire);
  #            vulnerable still present; urgent intervention
  #   medium = post-transition equilibrium; vulnerable largely already
  #            displaced; rebalance the new market
  #   low    = stable / non-gentrifying; AFFH-aligned opportunity expansion
  # Medium overrides high when both fire (deliberate; principle 4).
  # ------------------------------------------------------------------------
  rules = list(

    # ============ RENTER ============
    renter = list(

      medium = list(

        m1 = list(
          description = "Top-quintile rent appreciation OR top-quintile renovation/eviction activity, gated by pop growth",
          triggers = list(
            appreciation = list(family = "appreciation",
              condition = list(combiner = "any", conditions = list(
                list(input = "rank_rents",  threshold_ref = "q4_cutoff", direction = "above"),
                list(input = "rank_rents2", threshold_ref = "q4_cutoff", direction = "above")
              ))),
            expansion_renov = list(family = "expansion",
              condition = list(input = "rank_permits_renov", threshold_ref = "q4_cutoff", direction = "above")),
            distress_evictions = list(family = "distress", optional = TRUE,
              condition = list(input = "rank_evictions", threshold_ref = "q4_cutoff", direction = "above"))
          ),
          combiner = "any",
          gate = list(input = "pop_change10_20", threshold_ref = "pop_change_min", direction = "above")
        ),

        m2 = list(
          description = "Composite gentrification indicators past the 'expensive enough' transition",
          triggers = list(
            college  = list(family = "composite",
              condition = list(input = "rank_college", threshold_ref = "q4_cutoff", direction = "above")),
            hhinc    = list(family = "composite",
              condition = list(input = "rank_hhinc",   threshold_ref = "q4_cutoff", direction = "above")),
            hi_inc   = list(family = "composite",
              condition = list(input = "rank_hi_inc",  threshold_ref = "q4_cutoff", direction = "above")),
            lo_inc   = list(family = "composite",
              condition = list(input = "rank_lo_inc",  threshold_ref = "q1_cutoff", direction = "below")),
            black_ch = list(family = "composite",
              condition = list(combiner = "all", conditions = list(
                list(input = "black_change10_20",  threshold_ref = "black_change_abs_m",    direction = "below"),
                list(input = "blackpct_ch_00_20",  threshold_ref = "black_change_pct_max",  direction = "below")
              )))
          ),
          combiner = "count_ge",
          count_threshold_ref = "composite_min_count",
          gate = list(combiner = "all", conditions = list(
            list(input = "pop_change10_20", threshold_ref = "pop_change_min", direction = "above"),
            list(combiner = "any", conditions = list(
              list(input = "median_hhinc_20", threshold_ref = "med_hhinc_20_threshold", direction = "above"),
              list(input = "median_rent_20",  threshold_ref = "med_rent_20_threshold",  direction = "above")
            ))
          ))
        )
      ),

      high = list(

        h1 = list(
          description = "Middle-band rent appreciation OR middle-band renovation/eviction (early-stage signal)",
          triggers = list(
            appreciation = list(family = "appreciation",
              condition = list(combiner = "any", conditions = list(
                list(input = "rank_rents",  threshold_ref = list(low = "upper_half", high = "q4_cutoff"), direction = "between"),
                list(input = "rank_rents2", threshold_ref = list(low = "upper_half", high = "q4_cutoff"), direction = "between")
              ))),
            expansion_renov = list(family = "expansion",
              condition = list(input = "rank_permits_renov", threshold_ref = list(low = "upper_half", high = "q4_cutoff"), direction = "between")),
            distress_evictions = list(family = "distress", optional = TRUE,
              condition = list(input = "rank_evictions", threshold_ref = list(low = "upper_half", high = "q4_cutoff"), direction = "between"))
          ),
          combiner = "any"
        ),

        h2 = list(
          description = "Composite gentrification indicators below 'expensive enough' (early-stage)",
          triggers = list(
            college  = list(family = "composite",
              condition = list(input = "rank_college", threshold_ref = "above_med", direction = "above")),
            hhinc    = list(family = "composite",
              condition = list(input = "rank_hhinc",   threshold_ref = "above_med", direction = "above")),
            hi_inc   = list(family = "composite",
              condition = list(input = "rank_hi_inc",  threshold_ref = "above_med", direction = "above")),
            lo_inc   = list(family = "composite",
              condition = list(input = "rank_lo_inc",  threshold_ref = "below_med", direction = "below")),
            black_ch = list(family = "composite",
              condition = list(combiner = "all", conditions = list(
                list(input = "black_change10_20",  threshold_ref = "black_change_abs_h",   direction = "below"),
                list(input = "blackpct_ch_00_20",  threshold_ref = "black_change_pct_max", direction = "below")
              )))
          ),
          combiner = "count_ge",
          count_threshold_ref = "composite_min_count",
          gate = list(combiner = "all", conditions = list(
            list(input = "median_hhinc_20", threshold_ref = "med_hhinc_20_threshold", direction = "below"),
            list(input = "median_rent_20",  threshold_ref = "med_rent_20_threshold",  direction = "below")
          ))
        ),

        h3 = list(
          description = "Market-tightness signals confirming early-stage appreciation",
          triggers = list(
            housing_tight = list(family = "market-tightness",
              condition = list(input = "rank_housing_tight", threshold_ref = "above_med", direction = "above")),
            vacant_ch     = list(family = "market-tightness",
              condition = list(input = "rank_vacant_ch",     threshold_ref = "above_med", direction = "above")),
            renter_p      = list(family = "market-tightness",
              condition = list(input = "rank_renter_p",      threshold_ref = "above_med", direction = "above")),
            permits_demo  = list(family = "expansion",
              condition = list(input = "rank_permits_demo",  threshold_ref = "above_med", direction = "above"))
          ),
          combiner = "any",
          gate_rule = "h1"   # h3 fires only when h1 has also fired (resolved by interpreter)
        )
      )
    ),

    # ============ OWNER ============
    owner = list(

      medium = list(

        m1 = list(
          description = "Top-quintile HV / sales-price appreciation, gated by pop growth",
          triggers = list(
            appreciation = list(family = "appreciation",
              condition = list(combiner = "any", conditions = list(
                list(input = "rank_HV",         threshold_ref = "q4_cutoff", direction = "above"),
                list(input = "rank_mls_growth", threshold_ref = "q4_cutoff", direction = "above")
              ))),
            appreciation_assess = list(family = "appreciation", optional = TRUE,
              condition = list(input = "rank_assess_growth", threshold_ref = "q4_cutoff", direction = "above"))
          ),
          combiner = "any",
          gate = list(input = "pop_change10_20", threshold_ref = "pop_change_min", direction = "above")
        ),

        m2 = list(
          description = "Composite gentrification indicators past the HV transition",
          triggers = list(
            college  = list(family = "composite",
              condition = list(input = "rank_college", threshold_ref = "q4_cutoff", direction = "above")),
            hhinc    = list(family = "composite",
              condition = list(input = "rank_hhinc",   threshold_ref = "q4_cutoff", direction = "above")),
            hi_inc   = list(family = "composite",
              condition = list(input = "rank_hi_inc",  threshold_ref = "q4_cutoff", direction = "above")),
            lo_inc   = list(family = "composite",
              condition = list(input = "rank_lo_inc",  threshold_ref = "q1_cutoff", direction = "below")),
            black_ch = list(family = "composite",
              condition = list(combiner = "all", conditions = list(
                list(input = "black_change10_20",  threshold_ref = "black_change_abs_m",   direction = "below"),
                list(input = "blackpct_ch_00_20",  threshold_ref = "black_change_pct_max", direction = "below")
              )))
          ),
          combiner = "count_ge",
          count_threshold_ref = "composite_min_count",
          gate = list(combiner = "all", conditions = list(
            list(input = "pop_change10_20", threshold_ref = "pop_change_min",     direction = "above"),
            list(input = "median_hv_20",    threshold_ref = "med_hv_20_threshold", direction = "above")
          ))
        )
      ),

      high = list(

        h1 = list(
          description = "Middle-band HV / sales-price appreciation (early-stage)",
          triggers = list(
            appreciation = list(family = "appreciation",
              condition = list(combiner = "any", conditions = list(
                list(input = "rank_HV",         threshold_ref = list(low = "upper_half", high = "q4_cutoff"), direction = "between"),
                list(input = "rank_mls_growth", threshold_ref = list(low = "upper_half", high = "q4_cutoff"), direction = "between")
              )))
          ),
          combiner = "any"
        ),

        h2 = list(
          description = "Composite gentrification indicators below HV transition (early-stage)",
          triggers = list(
            college  = list(family = "composite",
              condition = list(input = "rank_college", threshold_ref = "above_med", direction = "above")),
            hhinc    = list(family = "composite",
              condition = list(input = "rank_hhinc",   threshold_ref = "above_med", direction = "above")),
            hi_inc   = list(family = "composite",
              condition = list(input = "rank_hi_inc",  threshold_ref = "above_med", direction = "above")),
            lo_inc   = list(family = "composite",
              condition = list(input = "rank_lo_inc",  threshold_ref = "below_med", direction = "below")),
            black_ch = list(family = "composite",
              condition = list(combiner = "all", conditions = list(
                list(input = "black_change10_20",  threshold_ref = "black_change_abs_h",   direction = "below"),
                list(input = "blackpct_ch_00_20",  threshold_ref = "black_change_pct_max", direction = "below")
              )))
          ),
          combiner = "count_ge",
          count_threshold_ref = "composite_min_count",
          gate = list(input = "median_hv_20", threshold_ref = "med_hv_20_threshold", direction = "below")
        ),

        h3 = list(
          description = "Owner-side market-tightness signals confirming early-stage appreciation",
          # rank_permits_renov MOVED to o_e1 in γ — h3 is now pure market-tightness.
          triggers = list(
            vacant_ch    = list(family = "market-tightness",
              condition = list(input = "rank_vacant_ch", threshold_ref = "above_med", direction = "above")),
            cyclomedia   = list(family = "market-tightness", optional = TRUE,
              condition = list(input = "rank_cyclomedia", threshold_ref = "above_med", direction = "above"))
          ),
          combiner = "any",
          gate_rule = "h1"
        ),

        d1 = list(
          description = "Active financial distress — top-quintile foreclosure rate (parallel track, no gentrification-signal gate)",
          triggers = list(
            foreclosure = list(family = "distress",
              condition = list(input = "rank_foreclosure", threshold_ref = "q4_cutoff", direction = "above"))
          ),
          combiner = "any"
        ),

        # γ NEW — standalone expansion driver. Rapid new construction OR
        # rapid renovation activity flags owner-high as a leading indicator
        # of coming pressure, independent of current HV appreciation.
        e1 = list(
          description = "Rapid permit activity — leading indicator of incoming displacement pressure",
          triggers = list(
            permits_new   = list(family = "expansion",
              condition = list(input = "rank_permits_new",   threshold_ref = "q4_cutoff", direction = "above")),
            permits_renov = list(family = "expansion",
              condition = list(input = "rank_permits_renov", threshold_ref = "q4_cutoff", direction = "above"))
          ),
          combiner = "any"
        )
      )
    )
  )
)
