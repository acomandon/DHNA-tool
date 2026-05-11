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
# Magic-number thresholds (79, 49, 59, 21, 41, 300, 47000, 66000, 1100, etc.)
# are inline by design — Goal #4 will overhaul the methodology and parameterize.
classify_risk <- function(bg_ct_data) {
  bg_ct_data %>%
    mutate(low_N_vulnerable = if_else(renters_20 < 300 &
                                        (median_hhinc_10 < 47000), 1, 0)) %>%
    filter(low_N_vulnerable == 1|
             renters_20 > 300 &
             (median_hhinc_10 < 47000)
    ) %>%
    mutate(m1a = if_else(rank_rents > 79|rank_rents2 > 79, 1, 0,
                         missing = 0),
           m1b = if_else(rank_HV > 79 & owner_20_ct > 500, 1, 0,
                         missing = 0),
           m1 = if_else(m1a+m1b > 0 & pop_change10_20 > 0, 1, 0),
           m2a = if_else(rank_college > 79,1,0),
           m2b = if_else(rank_hhinc > 79,1,0),
           m2c = if_else(rank_hi_inc > 79,1,0),
           m2d = if_else(rank_lo_inc < 21,1,0),
           m2e = if_else(black_change10_20 < -200 &
                           blackpct_ch_00_20 < -.2, 1,0),
           m2f = if_else(pop_change10_20 > 0, 1, 0),
           m2 = if_else(m2a+m2b+m2c+m2d+m2e > 1 &  m2f == 1 &
                          (median_hhinc_20 > 66000|median_rent_20 >1100),
                        1, 0, missing = 0)) %>%
    mutate(h1a = if_else((rank_rents > 49 & rank_rents < 80)|
                           (rank_rents2 > 49 & rank_rents2 < 80), 1, 0,
                         missing = 0),
           h1b = if_else(rank_HV > 49 & rank_HV < 80 & owner_20_ct > 500, 1, 0,
                         missing = 0),
           h1 = if_else(h1a+h1b > 0, 1, 0),
           h2a = if_else(rank_college > 59,1,0),
           h2b = if_else(rank_hhinc > 59,1,0),
           h2c = if_else(rank_hi_inc > 59,1,0),
           h2d = if_else(rank_lo_inc < 41,1,0),
           h2e = if_else(black_change10_20 < 0 &
                           blackpct_ch_00_20 < -.2, 1,0),
           h2f = if_else(pop_change10_20 > 0, 1, 0),
           h2 = if_else(h2a+h2b+h2c+h2d+h2e > 1 & #h2f == 1 &
                          median_hhinc_20 < 66000 &
                          median_rent_20 < 1100, 1, 0,
                        missing = 0),
           h3a = if_else(rank_housing_tight > 59|
                           rank_vacant_ch > 59|
                           rank_renter_p > 59|
                           rank_permits > 59, 1, 0,
                         missing = 0),
           h3b = if_else(rank_rents > 49|rank_rents2 > 49, 1, 0,
                         missing = 0),
           h3c = if_else(rank_HV > 49 & owner_20_ct > 500, 1, 0,
                         missing = 0),
           h3 = if_else(h3a > 0 & h1 == 1, 1, 0),) %>%
    mutate(hi_all = if_else(h1 == 1 |h2 == 1 |h3 == 1, 1, 0),
           med_all = if_else((m1 == 1 | m2 == 1), 1, 0),
           risk_level = if_else(hi_all == 1, "high", "low"),
           risk_level = if_else(med_all == 1, "medium", risk_level),
           risk_level = factor(risk_level, levels = c("low", "medium", "high"))) %>%
    select(GISJOIN_proj, hi_all, med_all, h1, h2, h3, m1, m2, risk_level)
}
