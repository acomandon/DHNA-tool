# Project configuration for the DHNA tool.
# Sourced by data_downloader.R, data_prep.R, and DHNA/app.R.
# Edit a block to retarget locality, refresh data vintages, or change pipeline params.

# Locality ------------------------------------------------------------------
# Edit to retarget the tool to a different metro area.
locality <- list(
  state_fips         = "21",
  state_name         = "Kentucky",
  state_abbr         = "KY",
  state_nhgis_extent = 210,
  county_fips        = "111",
  county_name        = "Jefferson County",
  pumas              = c(1701, 1702, 1703, 1704, 1705, 1706),
  default_map        = list(lat = 38.252, lng = -85.755, zoom = 12)
)

# Data vintages -------------------------------------------------------------
# Edit during a data refresh (Goal #2).
vintages <- list(
  acs_5yr_recent   = "2020_2024_ACS5a",
  acs_5yr_recent_b = "2020_2024_ACS5b",
  acs_5yr_prior    = "2009_2013_ACS5a",
  decennial_2000   = "2000_SF1b",
  decennial_1990   = "1990_STF1",
  pums_samples     = c("us2022a", "us2023a", "us2024a"),
  bg_shapefile     = "210_blck_grp_2024_tl2024",
  tract_shapefile  = "us_tract_2024_tl2024",
  bg_popcenter     = "us_blck_grp_cenpop_2020_cenpop2020"
)

# Pipeline parameters -------------------------------------------------------
params <- list(
  cpi_prior_to_recent = 1.3086,
  local_area_buffer_m = 800,
  permit_buffer_ft    = 2640,
  permit_buffer_crs   = 2246
)

# Administrative data feeds -------------------------------------------------
# Louisville Metro administrative exports (permits, Housing Trust Fund, MLS,
# foreclosures, HMDA) that roll forward with each refresh. Drop the new
# export into the matching subfolder under data/administrative/ and bump
# the filename here.
admin <- list(
  permits_csv      = "active_construction_permits_5798371747874481478.csv",
  htf_csv          = "Louisville_Metro_KY_-__ARP-0023_Louisville_Affordable_Housing_Trust_Fund.csv",
  # Goal #4 Phase 4.2 — tenure-aware risk model adds three administrative feeds:
  mls_csv          = "GIS_MLS_2019to2023_KY_Louisville.csv",            # data/administrative/MLS/
  foreclosures_csv = "GIS_JEFFCOMM_2023 Foreclosure Sales Results.csv", # data/administrative/foreclosures/
  hmda_xlsx        = "HMDA_Denials_KY_Louisville_2020-2022.xlsx",       # data/administrative/HMDA/
  hmda_sheet       = "2022_calc",        # 2020-vintage tract IDs; pooling 2020-2021 deferred (different tract vintage)
  hmda_year_label  = "2022"
)

# Optional locality-specific data slots --------------------------------------
# Each slot is either NULL (the city does not have this data; the classifier
# treats the corresponding indicator as absent and falls through) or a list
# with the file path + relevant column names. Goal #4 Phase 4.2 / Goal #5.
# Louisville status as of 2026-05-28:
#   - evictions: NULL (data not on hand; will be added when sourced)
#   - windshield_survey: populated (Cyclomedia tract-aggregated; Phase I
#       fully covered, Phase II partial)
#   - assessor: NULL (no PVA per-BG assessment-growth feed available)
optional_data <- list(
  evictions         = NULL,
  windshield_survey = list(
    # under data/administrative/Property Conditions Survey/
    csv       = "GIS_Property Condition Survey Data_tract.csv",
    geoid     = "GEOID",
    num_col   = "SurveyCount",      # problematic-unit count (numerator)
    denom_col = "HousingUnits21"    # housing-unit base (denominator for rate)
  ),
  assessor          = NULL
)

# HUD AFFH-T release --------------------------------------------------------
# Update these strings when HUD ships a new AFFHT release. `folder` is the
# basename of the prepackaged zip (also the directory it unzips into) under
# data/prepackaged/hud/.
hud_affh <- list(
  folder      = "HUD_AFFH_2024",
  housing_csv = "Housing_tract_AFFHT0007_December2024.csv",
  tract_csv   = "AFFH_tract_AFFHT0007_December2024.csv"
)

# HUD HOME / Housing Trust Fund Homeownership Sales Price Limits ------------
# Per-area maximum purchase price limits for ownership projects.
# Louisville/Jefferson County, KY-IN HUD Metro FMR Area, FY2024
# (effective 2024-09-01). Vectors index by # of units in the dwelling (1-4);
# typical for-sale projects use [1] (1-unit, i.e. per-unit pricing).
# Refresh by pulling the per-area row from HUD's annual
# HOME-and-Housing-Trust-Fund-Homeownership-Sales-Price-Limits xlsx
# (https://www.huduser.gov/portal/datasets/home-ownership-value-limits.html).
hud_for_sale <- list(
  existing_home    = c(228000L, 292000L, 353000L, 438000L),
  new_construction = c(332000L, 425000L, 515000L, 637000L)
)

# Risk classifier thresholds ------------------------------------------------
# Consumed by `classify_risk()` in R/risk_classifier.R. Two flavors:
#   1. Percentile-rank thresholds (operate on rank_* fields, which are
#      integer percentiles in 1..100 — see scripts/06_tract_data.R).
#   2. Locality-calibrated dollar/count thresholds (Louisville-tuned;
#      retune when retargeting to another metro).
# The classifier's rule structure is fixed for Phase 4.1 — only the
# constants live here. Phase 4.2 will revisit the rules themselves.
risk_params <- list(
  # Vulnerability filter (keeps lower-income 2010 BGs)
  med_hhinc_10_max       = 47000,   # 2010 median household income ceiling
  renters_n_min          = 300,     # renter-count split for low-N flag

  # Sample-size gate for owner-side signals
  owners_n_min           = 500,

  # Percentile-rank thresholds (rank_* values are integer percentiles 1..100)
  q4_cutoff              = 79,      # top-quintile boundary (> q4 = top; <= q4 = below)
  q1_cutoff              = 21,      # bottom-quintile boundary (< q1 = bottom)
  above_med              = 59,      # above-median band with margin
  below_med              = 41,      # below-median band with margin
  upper_half             = 49,      # upper-half cutoff

  # "Expensive enough" gates (medium-tier vs high-tier income/rent split)
  med_hhinc_20_threshold = 66000,
  med_rent_20_threshold  = 1100,

  # Demographic-change cutoffs
  pop_change_min         = 0,       # `pop_change10_20 > 0`
  black_change_abs_m     = -200,    # m2e: `black_change10_20 < -200`
  black_change_abs_h     = 0,       # h2e: `black_change10_20 < 0`
  black_change_pct_max   = -0.20,   # both: `blackpct_ch_00_20 < -0.20`

  # Composite-indicator count threshold (m2/h2 both: `> 1`, i.e. >= 2 indicators)
  composite_min_count    = 2,

  # Phase 4.2b.3b — owner-side parameters for the tenure-aware classifier.
  # Owner-vulnerability filter thresholds (shares in [0, 1]).
  longtenure_min         = 0.50,    # >=50% of owners are long-tenure (moved in >=10y ago)
  costburden_min         = 0.20,    # >=20% of owners cost-burdened (B25140)
  hmda_denial_min        = 0.20,    # >=20% mortgage denial rate (vulnerability supplement)
  # Owner "expensive enough" gate (parallel to med_hhinc_20_threshold +
  # med_rent_20_threshold). Median home value cutoff that separates "past
  # the transition" (medium-tier rules apply) from "still affordable but
  # transitioning" (high-tier rules apply).
  med_hv_20_threshold    = 200000
)

# Forecast modulator (Phase 4.4) --------------------------------------------
# Regional BA+ employment-growth forecast from Phase 4.3 used as a
# threshold modulator on the appreciation + expansion families. When the
# LWA growth rate exceeds the baseline, the q4_cutoff (and the high end of
# the middle band) is tightened regionally by:
#   adjustment_ppt = max(0, min(max_adjustment_ppt,
#                               (lwa_ba_growth_pct - baseline_pct) *
#                                sensitivity * 100))
# At Louisville's +9.35% (Kentuckiana Works LWA, 2022-2032), the
# appreciation/expansion q4_cutoff drops from 79 to ~74.65. Composite,
# distress, market-tightness, and vulnerability families keep static
# thresholds. Forecast does not compete for dominant_family.
forecast <- list(
  lwa_ba_growth_pct  = 0.0935,   # Kentuckiana Works 2022-2032 BA+ entry-ed occupations
  baseline_pct       = 0.05,
  sensitivity        = 1.0,
  max_adjustment_ppt = 10
)

# Not yet absorbed into config (deferred):
#   - HUD_FMI table (data_prep.R) — will move to data/prepackaged/hud_fmi.csv
#   - UI AMI dollar strings (DHNA/app.R) — derive from HUD_FMI once it is data
