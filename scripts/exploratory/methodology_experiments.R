# Methodology experiments — A1 (compositional displacement), B (spatial
# diffusion), C1 (uncertainty/reliability). READ-ONLY, prototyped alongside
# the validated classifier. No production pipeline changes.
#
# Purpose: for each candidate signal, show WHERE it would diverge from the
# current risk_level_renter/owner so the divergent BGs can be judged against
# the HNA 2024 concordance + practitioner knowledge (which live outside this
# repo). We produce the disagreement sets, not a verdict.
#
# Inputs : DHNA/data/LVM_Risk_Database.csv (all classifier inputs + outputs),
#          KY_Jefferson_BG_2023.shp (queen contiguity for B),
#          data/processed/audits/classifier_fragility.csv (for the C1 cross-tab).
# Outputs: data/processed/audits/exp_{A1,B,C1}_*.csv + console summary.
#
# Run: "C:/Program Files/R/R-4.4.1/bin/Rscript.exe" --vanilla \
#        scripts/exploratory/methodology_experiments.R

suppressMessages({
  library(here); library(dplyr); library(readr); library(sf); library(tidyr)
})
source(here("R", "config.R"))

audit <- here("data", "processed", "audits")
dir.create(audit, recursive = TRUE, showWarnings = FALSE)

db <- read_csv(here("DHNA", "data", "LVM_Risk_Database.csv"), show_col_types = FALSE)
med_hhinc_max <- risk_params$med_hhinc_10_max
cat(sprintf("Loaded %d BGs. Vulnerability income ceiling = %d.\n\n",
            nrow(db), med_hhinc_max))

pct_rank <- function(x) {
  r <- rank(x, na.last = "keep")
  ceiling(r / max(r, na.rm = TRUE) * 100)
}
vulnerable <- db$median_hhinc_10 < med_hhinc_max   # classifier's income gate

# ======================================================================
# A1 — Compositional displacement (approximate low-income renter share)
# ======================================================================
# We lack a tenure x income crosstab at BG (that is A2, needs B25118 x2
# vintages). Approximate the low-income renter share via independence:
#   lir_share_t ~= (low-income HH share_t) * (renter share_t)
# comp_disp = lir_share_10 - lir_share_20  (POSITIVE = the vulnerable
# population's share FELL = displacement observed). All inputs are already
# local-area pooled (stage 04), so this inherits the variance reduction.
db <- db %>%
  mutate(
    loinc_share_10 = lo_inc_hh_10 / HH_10,
    loinc_share_20 = lo_inc_hh_20 / HH_20,
    lir_share_10   = loinc_share_10 * renter_p_10,
    lir_share_20   = loinc_share_20 * renter_p_20,
    comp_disp      = lir_share_10 - lir_share_20,          # + = displacement
    rank_comp_disp = pct_rank(comp_disp)
  )

# Interpretive frame: composition change is displacement OBSERVED (lagging/
# concurrent); the current model's HIGH tier is displacement IMMINENT
# (leading, early-stage). So we expect comp_disp to align more with MEDIUM
# (advanced) than HIGH. Report the relationship rather than assume a target.
cat("=== A1: compositional displacement vs current renter tier ===\n")
a1_tier <- db %>%
  group_by(risk_level_renter) %>%
  summarise(n = n(),
            median_comp_disp_pp = round(median(comp_disp, na.rm = TRUE) * 100, 2),
            mean_rank_comp = round(mean(rank_comp_disp, na.rm = TRUE), 1),
            .groups = "drop")
print(as.data.frame(a1_tier))

# Divergence set: vulnerable BGs with strong observed displacement (top
# quintile comp_disp) that the current renter model rates LOW.
a1_missed <- db %>%
  filter(vulnerable, rank_comp_disp > 79, risk_level_renter == "low") %>%
  transmute(GISJOIN_proj, rank_comp_disp,
            loinc_share_10 = round(loinc_share_10, 3),
            loinc_share_20 = round(loinc_share_20, 3),
            renter_p_10 = round(renter_p_10, 3), renter_p_20 = round(renter_p_20, 3),
            comp_disp_pp = round(comp_disp * 100, 2),
            risk_level_renter) %>%
  arrange(desc(rank_comp_disp))
cat(sprintf("\nA1 divergence: %d vulnerable BGs with top-quintile observed\n", nrow(a1_missed)))
cat("displacement that the current renter model rates LOW (candidates the\n")
cat("price-proxy signals miss). Written to exp_A1_missed.csv\n")
write_csv(a1_missed, file.path(audit, "exp_A1_missed.csv"))

# Reverse: current HIGH-renter BGs where the vulnerable share is GROWING
# (comp_disp < 0) — consistent with "early stage, displacement not yet
# realized", i.e. the leading-vs-lagging distinction, not necessarily errors.
a1_leading <- db %>%
  filter(risk_level_renter == "high", comp_disp < 0) %>%
  summarise(n = n())
cat(sprintf("\nA1 note: %d current HIGH-renter BGs still GAINING low-income renter\n", a1_leading$n))
cat("share (comp_disp<0) — consistent with leading-edge (imminent, not yet\n")
cat("realized) displacement. Composition is a lagging measure; expect this.\n")

# ======================================================================
# B — Spatial diffusion (queen contiguity; lag of PRESSURE, not tier)
# ======================================================================
cat("\n=== B: spatial diffusion (in the path of the front) ===\n")
bg_sf <- st_read(here("data", "nhgis", "gis", "blockgroup", "bg2023",
                      "KY_Jefferson_BG_2023.shp"), quiet = TRUE) %>%
  select(GISJOIN) %>% st_transform(2246)

# Appreciation pressure per BG = mean of available appreciation ranks
# (renter + owner). Deliberately NOT the output tier (avoids circular
# neighbor-inflation feedback).
db <- db %>%
  mutate(pressure = rowMeans(cbind(rank_rents, rank_rents2, rank_HV, rank_mls_growth),
                             na.rm = TRUE))

geo <- bg_sf %>% inner_join(db %>% select(GISJOIN_proj, pressure, vulnerable = median_hhinc_10,
                                          risk_level_renter),
                            by = c("GISJOIN" = "GISJOIN_proj"))
# Queen contiguity via shared boundary/point (st_intersects minus self).
nb <- st_intersects(geo, geo)
neigh_pressure <- vapply(seq_along(nb), function(i) {
  j <- setdiff(nb[[i]], i)
  if (length(j) == 0) return(NA_real_)
  mean(geo$pressure[j], na.rm = TRUE)
}, numeric(1))
geo$neigh_pressure <- neigh_pressure

diff_tbl <- geo %>% st_drop_geometry() %>%
  mutate(is_vuln = vulnerable < med_hhinc_max)

# "In the path of the front": vulnerable, own pressure below median, but
# neighbours run hot (top-third neighbour pressure), and the model says LOW.
own_med   <- median(diff_tbl$pressure, na.rm = TRUE)
neigh_hi  <- quantile(diff_tbl$neigh_pressure, 0.66, na.rm = TRUE)
b_candidates <- diff_tbl %>%
  filter(is_vuln, pressure < own_med, neigh_pressure > neigh_hi,
         risk_level_renter == "low") %>%
  transmute(GISJOIN, own_pressure = round(pressure, 1),
            neigh_pressure = round(neigh_pressure, 1), risk_level_renter) %>%
  arrange(desc(neigh_pressure))
cat(sprintf("Own-pressure median = %.1f; neighbour-pressure 66th pct = %.1f\n",
            own_med, neigh_hi))
cat(sprintf("B divergence: %d vulnerable, currently-LOW BGs that are calm\n", nrow(b_candidates)))
cat("themselves but ringed by high-pressure neighbours (in the path of the\n")
cat("front). Written to exp_B_candidates.csv\n")
write_csv(b_candidates, file.path(audit, "exp_B_candidates.csv"))

# ======================================================================
# C1 — Uncertainty / reliability (effective-N proxy; full MOE = future)
# ======================================================================
cat("\n=== C1: reliability via effective pooled sample size ===\n")
# The local-area pooling stabilizes estimates by pooling ~tract-sized
# samples. Effective N ~= pooled households (HH_20). A thin pool => less
# reliable median/share estimates. Proxy now; ACS-MOE propagation is A-later.
db <- db %>%
  mutate(eff_n = HH_20,
         reliability = cut(eff_n,
                           breaks = quantile(eff_n, c(0, .1, .25, 1), na.rm = TRUE),
                           labels = c("low", "modest", "adequate"),
                           include.lowest = TRUE))
cat("Reliability tiers (by pooled-household effective N):\n")
print(as.data.frame(db %>% count(reliability)))

# Cross-tab reliability x risk tier: are low-confidence BGs concentrated in
# any tier? And do they coincide with the fragile BGs from the jitter test?
cat("\nReliability x renter risk tier:\n")
print(table(reliability = db$reliability, renter = db$risk_level_renter))

frag_path <- file.path(audit, "classifier_fragility.csv")
if (file.exists(frag_path)) {
  frag <- read_csv(frag_path, show_col_types = FALSE)
  j <- db %>% select(GISJOIN_proj, reliability, eff_n) %>%
    inner_join(frag %>% select(GISJOIN_proj, flip_rate_r), by = "GISJOIN_proj")
  cat("\nMean renter flip-rate by reliability tier (do fragile BGs cluster in\n")
  cat("low-confidence?): \n")
  print(as.data.frame(j %>% group_by(reliability) %>%
          summarise(n = n(), mean_flip_rate_r = round(mean(flip_rate_r), 3),
                    .groups = "drop")))
}
db %>% select(GISJOIN_proj, eff_n, reliability, risk_level_renter, risk_level_owner) %>%
  write_csv(file.path(audit, "exp_C1_reliability.csv"))
cat("\nWrote exp_C1_reliability.csv\n")

cat("\nDone. Divergence sets + reliability in data/processed/audits/exp_*.csv\n")
