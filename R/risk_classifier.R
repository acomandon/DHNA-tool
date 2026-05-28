# Rule-based displacement risk classifier.
# Sourced by data_prep.R.

# classify_risk(bg_ct_data) -> data.frame
#
# Consumes: renters_20, owner_20_ct, median_hhinc_10, median_hhinc_20,
#   median_rent_20, rank_rents, rank_rents2, rank_HV, rank_college, rank_hhinc,
#   rank_hi_inc, rank_lo_inc, rank_housing_tight, rank_vacant_ch,
#   rank_renter_p, rank_permits, pop_change10_20, black_change10_20,
#   blackpct_ch_00_20.
#
# Returns: GISJOIN_proj + rule flags (m1, m2, h1, h2, h3), aggregate flags
#   (hi_all, med_all), and risk_level (factor: low | medium | high).
#
# Thresholds live in `risk_params` in R/config.R. Two integer-equivalent
# rewrites from the original literals: `< 80` -> `<= q4_cutoff` (= 79) and
# `sum > 1` -> `sum >= composite_min_count` (= 2). Both are byte-identical
# on integer-valued inputs (rank_* are 1..100; the sum is in 0..5).
classify_risk <- function(bg_ct_data) {
  bg_ct_data %>%
    mutate(low_N_vulnerable = if_else(renters_20 < risk_params$renters_n_min &
                                        (median_hhinc_10 < risk_params$med_hhinc_10_max), 1, 0)) %>%
    filter(low_N_vulnerable == 1|
             renters_20 > risk_params$renters_n_min &
             (median_hhinc_10 < risk_params$med_hhinc_10_max)
    ) %>%
    mutate(m1a = if_else(rank_rents > risk_params$q4_cutoff|rank_rents2 > risk_params$q4_cutoff, 1, 0,
                         missing = 0),
           m1b = if_else(rank_HV > risk_params$q4_cutoff & owner_20_ct > risk_params$owners_n_min, 1, 0,
                         missing = 0),
           m1 = if_else(m1a+m1b > 0 & pop_change10_20 > risk_params$pop_change_min, 1, 0),
           m2a = if_else(rank_college > risk_params$q4_cutoff,1,0),
           m2b = if_else(rank_hhinc > risk_params$q4_cutoff,1,0),
           m2c = if_else(rank_hi_inc > risk_params$q4_cutoff,1,0),
           m2d = if_else(rank_lo_inc < risk_params$q1_cutoff,1,0),
           m2e = if_else(black_change10_20 < risk_params$black_change_abs_m &
                           blackpct_ch_00_20 < risk_params$black_change_pct_max, 1,0),
           m2f = if_else(pop_change10_20 > risk_params$pop_change_min, 1, 0),
           m2 = if_else(m2a+m2b+m2c+m2d+m2e >= risk_params$composite_min_count &  m2f == 1 &
                          (median_hhinc_20 > risk_params$med_hhinc_20_threshold|median_rent_20 > risk_params$med_rent_20_threshold),
                        1, 0, missing = 0)) %>%
    mutate(h1a = if_else((rank_rents > risk_params$upper_half & rank_rents <= risk_params$q4_cutoff)|
                           (rank_rents2 > risk_params$upper_half & rank_rents2 <= risk_params$q4_cutoff), 1, 0,
                         missing = 0),
           h1b = if_else(rank_HV > risk_params$upper_half & rank_HV <= risk_params$q4_cutoff & owner_20_ct > risk_params$owners_n_min, 1, 0,
                         missing = 0),
           h1 = if_else(h1a+h1b > 0, 1, 0),
           h2a = if_else(rank_college > risk_params$above_med,1,0),
           h2b = if_else(rank_hhinc > risk_params$above_med,1,0),
           h2c = if_else(rank_hi_inc > risk_params$above_med,1,0),
           h2d = if_else(rank_lo_inc < risk_params$below_med,1,0),
           h2e = if_else(black_change10_20 < risk_params$black_change_abs_h &
                           blackpct_ch_00_20 < risk_params$black_change_pct_max, 1,0),
           h2f = if_else(pop_change10_20 > risk_params$pop_change_min, 1, 0),
           h2 = if_else(h2a+h2b+h2c+h2d+h2e >= risk_params$composite_min_count & #h2f == 1 &
                          median_hhinc_20 < risk_params$med_hhinc_20_threshold &
                          median_rent_20 < risk_params$med_rent_20_threshold, 1, 0,
                        missing = 0),
           h3a = if_else(rank_housing_tight > risk_params$above_med|
                           rank_vacant_ch > risk_params$above_med|
                           rank_renter_p > risk_params$above_med|
                           rank_permits > risk_params$above_med, 1, 0,
                         missing = 0),
           h3b = if_else(rank_rents > risk_params$upper_half|rank_rents2 > risk_params$upper_half, 1, 0,
                         missing = 0),
           h3c = if_else(rank_HV > risk_params$upper_half & owner_20_ct > risk_params$owners_n_min, 1, 0,
                         missing = 0),
           h3 = if_else(h3a > 0 & h1 == 1, 1, 0),) %>%
    mutate(hi_all = if_else(h1 == 1 |h2 == 1 |h3 == 1, 1, 0),
           med_all = if_else((m1 == 1 | m2 == 1), 1, 0),
           risk_level = if_else(hi_all == 1, "high", "low"),
           risk_level = if_else(med_all == 1, "medium", risk_level),
           risk_level = factor(risk_level, levels = c("low", "medium", "high"))) %>%
    select(GISJOIN_proj, hi_all, med_all, h1, h2, h3, m1, m2, risk_level)
}
