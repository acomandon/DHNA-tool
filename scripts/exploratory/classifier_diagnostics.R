# Classifier robustness diagnostics (read-only; changes no outputs).
#
# 1. Threshold-cliff fragility — Monte-Carlo jitter of every rank_* input by
#    +/- up to JITTER ranks, re-run the classifier, and count how often each
#    BG flips tier. Shows how brittle the classification is to percentile noise.
# 2. Composite collinearity — pairwise correlation of the four composite-family
#    rank signals (college / hhinc / hi_inc / lo_inc), which trip m2/h2 via
#    count_ge >= 2. High collinearity means they can be ~one underlying axis.
#
# Reads DHNA/data/LVM_Risk_Database.csv (= bg_ct_risk: all classifier inputs
# plus the stored tier outputs). Run:
#   "C:/Program Files/R/R-4.4.1/bin/Rscript.exe" --vanilla \
#      scripts/exploratory/classifier_diagnostics.R

suppressMessages({
  library(here); library(dplyr); library(readr); library(tibble); library(tidyr)
})
source(here("R", "config.R"))
source(here("R", "risk_matrix.R"))
source(here("R", "risk_classifier.R"))

set.seed(20260707)
JITTER <- 3      # +/- ranks
N_REP  <- 200    # Monte-Carlo replications

db <- read_csv(here("DHNA", "data", "LVM_Risk_Database.csv"),
               show_col_types = FALSE)

# Baseline: reproduce stored tiers from current inputs (sanity check).
base_r <- classify_risk_renter(db)
base_o <- classify_risk_owner(db)
base <- db %>% select(GISJOIN_proj) %>%
  left_join(base_r %>% select(GISJOIN_proj, r = risk_level_renter), by = "GISJOIN_proj") %>%
  left_join(base_o %>% select(GISJOIN_proj, o = risk_level_owner),  by = "GISJOIN_proj") %>%
  mutate(r = as.character(tidyr::replace_na(as.character(r), "low")),
         o = as.character(tidyr::replace_na(as.character(o), "low")))
stored_r <- as.character(db$risk_level_renter)
stored_o <- as.character(db$risk_level_owner)
cat(sprintf("Sanity: reproduced renter tiers match stored: %s | owner: %s\n",
            all(base$r == stored_r), all(base$o == stored_o)))

rank_cols <- grep("^rank_", names(db), value = TRUE)
rank_cols <- rank_cols[vapply(rank_cols, function(c) any(!is.na(db[[c]])), logical(1))]
cat(sprintf("\nJittering %d rank_* inputs (+/-%d) x %d reps ...\n",
            length(rank_cols), JITTER, N_REP))

flip_r <- integer(nrow(db)); flip_o <- integer(nrow(db))
for (i in seq_len(N_REP)) {
  pert <- db
  for (rc in rank_cols) {
    v <- pert[[rc]]
    j <- sample(-JITTER:JITTER, length(v), replace = TRUE)
    pert[[rc]] <- pmin(100, pmax(1, v + j))
  }
  pr <- classify_risk_renter(pert); po <- classify_risk_owner(pert)
  pr2 <- db %>% select(GISJOIN_proj) %>%
    left_join(pr %>% select(GISJOIN_proj, r = risk_level_renter), by = "GISJOIN_proj") %>%
    mutate(r = as.character(tidyr::replace_na(as.character(r), "low"))) %>% pull(r)
  po2 <- db %>% select(GISJOIN_proj) %>%
    left_join(po %>% select(GISJOIN_proj, o = risk_level_owner), by = "GISJOIN_proj") %>%
    mutate(o = as.character(tidyr::replace_na(as.character(o), "low"))) %>% pull(o)
  flip_r <- flip_r + (pr2 != base$r)
  flip_o <- flip_o + (po2 != base$o)
}

cat("\n=== 1. THRESHOLD-CLIFF FRAGILITY (+/-", JITTER, "rank jitter) ===\n", sep = "")
frag <- tibble(GISJOIN_proj = db$GISJOIN_proj,
               tier_r = base$r, tier_o = base$o,
               flip_rate_r = flip_r / N_REP, flip_rate_o = flip_o / N_REP)
cat(sprintf("Renter: mean per-rep flips = %.1f BGs (%.1f%% of 608); BGs that EVER flip = %d\n",
            sum(flip_r) / N_REP, 100 * sum(flip_r) / N_REP / nrow(db), sum(flip_r > 0)))
cat(sprintf("Owner : mean per-rep flips = %.1f BGs (%.1f%% of 608); BGs that EVER flip = %d\n",
            sum(flip_o) / N_REP, 100 * sum(flip_o) / N_REP / nrow(db), sum(flip_o > 0)))
cat("\nFlip rate by baseline tier (share of BGs in tier that flip >=25% of reps):\n")
frag %>% group_by(tier_r) %>%
  summarise(n = n(), fragile = sum(flip_rate_r >= 0.25), .groups = "drop") %>%
  mutate(pct_fragile = round(100 * fragile / n, 1)) %>% as.data.frame() %>% print()
frag %>% group_by(tier_o) %>%
  summarise(n = n(), fragile = sum(flip_rate_o >= 0.25), .groups = "drop") %>%
  mutate(pct_fragile = round(100 * fragile / n, 1)) %>% as.data.frame() %>% print()

dir.create(here("data", "processed", "audits"), recursive = TRUE, showWarnings = FALSE)
write_csv(frag, here("data", "processed", "audits", "classifier_fragility.csv"))

cat("\n=== 2. COMPOSITE COLLINEARITY (Spearman) ===\n")
comp <- db %>% select(rank_college, rank_hhinc, rank_hi_inc, rank_lo_inc)
cm <- cor(comp, use = "pairwise.complete.obs", method = "spearman")
print(round(cm, 2))
cat("\nNote: rank_lo_inc enters the rules inverted (low rank = signal), so a\n",
    "strong NEGATIVE correlation with the others means they still move together.\n", sep = "")

cat("\nWrote data/processed/audits/classifier_fragility.csv\n")
