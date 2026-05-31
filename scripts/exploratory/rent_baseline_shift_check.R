# Quantify the impact of moving the rank_rents2 baseline quarter from
# Q3_2017 to Q3_2019 (audit-driven change, post-Renthub-regime-shift).
#
# Read-only. Operates on data/prepackaged/renthub/rent_buffer.csv (the same
# input scripts/05_buffer_data.R consumes), recomputes rank_rents2 both
# ways, and reports:
#   - distribution of per-BG rank changes
#   - how many BGs cross the classifier's q4_cutoff threshold (79), the
#     only rent boundary the risk_matrix rent triggers actually depend on
#   - the BGs with the largest rank shifts (top 20)
#
# Output: data/processed/audits/rent_baseline_shift.csv

library(here)
library(dplyr)
library(tidyr)
library(readr)

source(here("R", "config.R"))

audit_dir <- here("data", "processed", "audits")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

rh_cfg <- optional_data$renthub
stopifnot(!is.null(rh_cfg))

raw <- read_csv(here("data", "prepackaged", "renthub", rh_cfg$csv),
                show_col_types = FALSE) |>
  mutate(year_qu_text = paste0(R_quarter, "_", R_year))

build_wide <- function(baseline_q, recent_q, label) {
  raw |>
    filter(year_qu_text %in% c(baseline_q, recent_q),
           N > rh_cfg$min_obs_per_q) |>
    group_by(GISJOIN) |>
    filter(n() == 2) |>
    ungroup() |>
    pivot_wider(id_cols  = GISJOIN,
                names_from  = year_qu_text,
                values_from = rent_bg) |>
    mutate(rent_change_pct = (.data[[recent_q]] - .data[[baseline_q]]) /
                             .data[[baseline_q]],
           rank_raw = rank(rent_change_pct, na.last = "keep"),
           !!paste0("rank_", label) := ceiling(rank_raw / max(rank_raw, na.rm = TRUE) * 100)) |>
    select(GISJOIN, !!paste0("rank_", label))
}

old <- build_wide("Q3_2017", "Q3_2024", "old")  # pre-audit baseline
new <- build_wide("Q3_2019", "Q3_2024", "new")  # post-audit baseline

q4 <- risk_params$q4_cutoff   # 79 — the classifier's rent boundary

cmp <- full_join(old, new, by = "GISJOIN") |>
  mutate(
    delta            = rank_new - rank_old,
    was_top_quintile = !is.na(rank_old) & rank_old > q4,
    is_top_quintile  = !is.na(rank_new) & rank_new > q4,
    flip             = case_when(
      is.na(rank_old) & !is.na(rank_new)               ~ "added (was NA)",
      !is.na(rank_old) & is.na(rank_new)               ~ "dropped (now NA)",
      !was_top_quintile & is_top_quintile              ~ "into top quintile",
      was_top_quintile & !is_top_quintile              ~ "out of top quintile",
      TRUE                                              ~ "no boundary flip"
    )
  )

write_csv(cmp, file.path(audit_dir, "rent_baseline_shift.csv"))

cat("=== BGs scored under each baseline ===\n")
print(cmp |> summarise(
  n_old_only = sum(!is.na(rank_old) & is.na(rank_new)),
  n_new_only = sum(is.na(rank_old) & !is.na(rank_new)),
  n_both     = sum(!is.na(rank_old) & !is.na(rank_new)),
  n_total    = n()
))

cat("\n=== rank shift distribution (BGs scored under both baselines) ===\n")
both <- cmp |> filter(!is.na(delta))
print(both |> summarise(
  n         = n(),
  mean_abs  = round(mean(abs(delta)),  1),
  median_abs= round(median(abs(delta)),1),
  q95_abs   = round(quantile(abs(delta), 0.95), 1),
  max_abs   = max(abs(delta)),
  shifted_gt10 = sum(abs(delta) > 10),
  shifted_gt25 = sum(abs(delta) > 25)
))

cat(sprintf("\n=== q4_cutoff boundary crossings (threshold = %d) ===\n", q4))
print(cmp |> count(flip, sort = TRUE))

cat("\n=== top 20 rank shifts (most-changed BGs) ===\n")
print(both |>
        mutate(abs_delta = abs(delta)) |>
        arrange(desc(abs_delta)) |>
        select(GISJOIN, rank_old, rank_new, delta, flip) |>
        head(20))

cat(sprintf("\nWrote %s\n", file.path(audit_dir, "rent_baseline_shift.csv")))
