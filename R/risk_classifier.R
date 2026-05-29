# Tenure-aware rule-based displacement risk classifiers.
# Sourced by scripts/00_run_all.R; called from scripts/07_risk.R.
#
# Two parallel classifiers mirror the original m/h tier structure with
# tenure-specific appreciation signals, vulnerability filters, and market
# tightness. Thresholds live in `risk_params` in R/config.R.
#
# Outputs:
#   classify_risk_renter(bg_ct_data) -> GISJOIN_proj +
#     r_m1, r_m2, r_h1, r_h2, r_h3, r_hi_all, r_med_all, risk_level_renter
#   classify_risk_owner(bg_ct_data)  -> GISJOIN_proj +
#     o_m1, o_m2, o_h1, o_h2, o_h3, o_d1, o_hi_all, o_med_all, risk_level_owner
#
# The owner side carries a parallel financial-distress track (o_d1, driven
# by foreclosure rate) distinct from the gentrification-driven track
# (o_m1, o_m2, o_h1, o_h2, o_h3). o_hi_all OR's them together.
#
# Optional-slot columns (rank_evictions, rank_cyclomedia, rank_assess_growth)
# are backfilled as NA when absent so rule code can reference them
# unconditionally; their contribution drops out via the OR / `> threshold`
# logic when NA.

# Helper: ensure named columns exist (NA-filled when missing) ---------------
.ensure_cols <- function(df, cols) {
  for (col in cols) if (!col %in% names(df)) df[[col]] <- NA_real_
  df
}

# Renter classifier ----------------------------------------------------------
classify_risk_renter <- function(bg_ct_data) {
  bg_ct_data <- .ensure_cols(bg_ct_data, c("rank_evictions"))
  bg_ct_data %>%
    mutate(low_N_vulnerable = if_else(renters_20 < risk_params$renters_n_min &
                                        (median_hhinc_10 < risk_params$med_hhinc_10_max), 1, 0)) %>%
    filter(low_N_vulnerable == 1 |
             renters_20 > risk_params$renters_n_min &
             (median_hhinc_10 < risk_params$med_hhinc_10_max)) %>%
    # Medium-tier rules: appreciation (m1) + composite gentrification (m2)
    mutate(r_m1a = if_else(rank_rents > risk_params$q4_cutoff |
                             rank_rents2 > risk_params$q4_cutoff, 1, 0, missing = 0),
           r_m1b = if_else(rank_permits_renov > risk_params$q4_cutoff |
                             rank_evictions > risk_params$q4_cutoff, 1, 0, missing = 0),
           r_m1  = if_else(r_m1a + r_m1b > 0 &
                             pop_change10_20 > risk_params$pop_change_min, 1, 0),
           r_m2a = if_else(rank_college  > risk_params$q4_cutoff, 1, 0),
           r_m2b = if_else(rank_hhinc    > risk_params$q4_cutoff, 1, 0),
           r_m2c = if_else(rank_hi_inc   > risk_params$q4_cutoff, 1, 0),
           r_m2d = if_else(rank_lo_inc   < risk_params$q1_cutoff, 1, 0),
           r_m2e = if_else(black_change10_20 < risk_params$black_change_abs_m &
                             blackpct_ch_00_20 < risk_params$black_change_pct_max, 1, 0),
           r_m2f = if_else(pop_change10_20 > risk_params$pop_change_min, 1, 0),
           r_m2  = if_else(r_m2a + r_m2b + r_m2c + r_m2d + r_m2e >= risk_params$composite_min_count &
                             r_m2f == 1 &
                             (median_hhinc_20 > risk_params$med_hhinc_20_threshold |
                                median_rent_20 > risk_params$med_rent_20_threshold),
                           1, 0, missing = 0)) %>%
    # High-tier rules: middle-band appreciation (h1) + composite below
    # "expensive enough" (h2) + market tightness AND h1 (h3).
    mutate(r_h1a = if_else((rank_rents  > risk_params$upper_half & rank_rents  <= risk_params$q4_cutoff) |
                             (rank_rents2 > risk_params$upper_half & rank_rents2 <= risk_params$q4_cutoff),
                           1, 0, missing = 0),
           r_h1b = if_else((rank_permits_renov > risk_params$upper_half & rank_permits_renov <= risk_params$q4_cutoff) |
                             (rank_evictions   > risk_params$upper_half & rank_evictions    <= risk_params$q4_cutoff),
                           1, 0, missing = 0),
           r_h1  = if_else(r_h1a + r_h1b > 0, 1, 0),
           r_h2a = if_else(rank_college  > risk_params$above_med, 1, 0),
           r_h2b = if_else(rank_hhinc    > risk_params$above_med, 1, 0),
           r_h2c = if_else(rank_hi_inc   > risk_params$above_med, 1, 0),
           r_h2d = if_else(rank_lo_inc   < risk_params$below_med, 1, 0),
           r_h2e = if_else(black_change10_20 < risk_params$black_change_abs_h &
                             blackpct_ch_00_20 < risk_params$black_change_pct_max, 1, 0),
           r_h2  = if_else(r_h2a + r_h2b + r_h2c + r_h2d + r_h2e >= risk_params$composite_min_count &
                             median_hhinc_20 < risk_params$med_hhinc_20_threshold &
                             median_rent_20  < risk_params$med_rent_20_threshold,
                           1, 0, missing = 0),
           r_h3a = if_else(rank_housing_tight > risk_params$above_med |
                             rank_vacant_ch    > risk_params$above_med |
                             rank_renter_p     > risk_params$above_med |
                             rank_permits_demo > risk_params$above_med, 1, 0, missing = 0),
           r_h3  = if_else(r_h3a > 0 & r_h1 == 1, 1, 0)) %>%
    mutate(r_hi_all  = if_else(r_h1 == 1 | r_h2 == 1 | r_h3 == 1, 1, 0),
           r_med_all = if_else(r_m1 == 1 | r_m2 == 1, 1, 0),
           # Medium overrides high when both fire — deliberate, locked in
           # at Phase 4.1 (m* rules describe areas past the transition).
           risk_level_renter = if_else(r_hi_all == 1, "high", "low"),
           risk_level_renter = if_else(r_med_all == 1, "medium", risk_level_renter),
           risk_level_renter = factor(risk_level_renter, levels = c("low", "medium", "high"))) %>%
    select(GISJOIN_proj,
           r_m1, r_m2, r_h1, r_h2, r_h3,
           r_hi_all, r_med_all, risk_level_renter)
}

# Owner classifier ----------------------------------------------------------
classify_risk_owner <- function(bg_ct_data) {
  bg_ct_data <- .ensure_cols(bg_ct_data, c("rank_cyclomedia", "rank_assess_growth"))
  bg_ct_data %>%
    # Owner-vulnerability filter: sufficient owner sample, low income,
    # long-tenure majority, AND (cost-burdened owners OR low mortgage
    # access via HMDA denials). The optional HMDA branch lets the filter
    # qualify BGs where owners face mortgage barriers even when not
    # currently cost-burdened.
    mutate(low_N_owner_vulnerable = if_else(
              owner_20_ct < risk_params$owners_n_min &
              median_hhinc_10 < risk_params$med_hhinc_10_max &
              owner_longtenure_p >= risk_params$longtenure_min,
              1, 0, missing = 0)) %>%
    filter(low_N_owner_vulnerable == 1 |
             (owner_20_ct > risk_params$owners_n_min &
                median_hhinc_10 < risk_params$med_hhinc_10_max &
                owner_longtenure_p >= risk_params$longtenure_min &
                (owner_costburden_p >= risk_params$costburden_min |
                   hmda_denial_rate  >= risk_params$hmda_denial_min))) %>%
    # Medium-tier rules: HV/MLS appreciation (m1) + composite gentrification (m2)
    mutate(o_m1a = if_else(rank_HV > risk_params$q4_cutoff |
                             rank_mls_growth > risk_params$q4_cutoff, 1, 0, missing = 0),
           o_m1b = if_else(rank_assess_growth > risk_params$q4_cutoff, 1, 0, missing = 0),
           o_m1  = if_else(o_m1a + o_m1b > 0 &
                             pop_change10_20 > risk_params$pop_change_min, 1, 0),
           o_m2a = if_else(rank_college  > risk_params$q4_cutoff, 1, 0),
           o_m2b = if_else(rank_hhinc    > risk_params$q4_cutoff, 1, 0),
           o_m2c = if_else(rank_hi_inc   > risk_params$q4_cutoff, 1, 0),
           o_m2d = if_else(rank_lo_inc   < risk_params$q1_cutoff, 1, 0),
           o_m2e = if_else(black_change10_20 < risk_params$black_change_abs_m &
                             blackpct_ch_00_20 < risk_params$black_change_pct_max, 1, 0),
           o_m2f = if_else(pop_change10_20 > risk_params$pop_change_min, 1, 0),
           o_m2  = if_else(o_m2a + o_m2b + o_m2c + o_m2d + o_m2e >= risk_params$composite_min_count &
                             o_m2f == 1 &
                             median_hv_20 > risk_params$med_hv_20_threshold,
                           1, 0, missing = 0)) %>%
    # High-tier rules: middle-band appreciation (h1) + composite below
    # "expensive enough" (h2) + market tightness AND h1 (h3).
    mutate(o_h1a = if_else((rank_HV         > risk_params$upper_half & rank_HV         <= risk_params$q4_cutoff) |
                             (rank_mls_growth > risk_params$upper_half & rank_mls_growth <= risk_params$q4_cutoff),
                           1, 0, missing = 0),
           o_h1  = if_else(o_h1a > 0, 1, 0),
           o_h2a = if_else(rank_college  > risk_params$above_med, 1, 0),
           o_h2b = if_else(rank_hhinc    > risk_params$above_med, 1, 0),
           o_h2c = if_else(rank_hi_inc   > risk_params$above_med, 1, 0),
           o_h2d = if_else(rank_lo_inc   < risk_params$below_med, 1, 0),
           o_h2e = if_else(black_change10_20 < risk_params$black_change_abs_h &
                             blackpct_ch_00_20 < risk_params$black_change_pct_max, 1, 0),
           o_h2  = if_else(o_h2a + o_h2b + o_h2c + o_h2d + o_h2e >= risk_params$composite_min_count &
                             median_hv_20 < risk_params$med_hv_20_threshold,
                           1, 0, missing = 0),
           # Owner h3 swaps the renter-side market tightness signals for
           # owner-relevant ones: vacancy change, renovation flipping,
           # plus optional Cyclomedia (poor conditions) when populated.
           o_h3a = if_else(rank_vacant_ch     > risk_params$above_med |
                             rank_permits_renov > risk_params$above_med |
                             rank_cyclomedia    > risk_params$above_med, 1, 0, missing = 0),
           o_h3  = if_else(o_h3a > 0 & o_h1 == 1, 1, 0),
           # Parallel financial-distress track. High foreclosure rate flags
           # owner displacement via the financial-distress mechanism, which
           # is policy-distinct from gentrification displacement.
           o_d1  = if_else(rank_foreclosure > risk_params$q4_cutoff, 1, 0, missing = 0)) %>%
    mutate(o_hi_all  = if_else(o_h1 == 1 | o_h2 == 1 | o_h3 == 1 | o_d1 == 1, 1, 0),
           o_med_all = if_else(o_m1 == 1 | o_m2 == 1, 1, 0),
           risk_level_owner = if_else(o_hi_all == 1, "high", "low"),
           risk_level_owner = if_else(o_med_all == 1, "medium", risk_level_owner),
           risk_level_owner = factor(risk_level_owner, levels = c("low", "medium", "high"))) %>%
    select(GISJOIN_proj,
           o_m1, o_m2, o_h1, o_h2, o_h3, o_d1,
           o_hi_all, o_med_all, risk_level_owner)
}
