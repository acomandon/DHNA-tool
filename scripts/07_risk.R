# Stage 07 — Risk classification and outputs
# Apply the tenure-aware displacement-risk classifiers and write the app's
# final CSV outputs.
#
# Depends on: bg_ct_data from stage 06; local_area from stage 02;
#   classify_risk_renter() and classify_risk_owner() from R/risk_classifier.R.
# Writes: DHNA/data/LVM_Risk_Database.csv, DHNA/data/local_area.csv.

# Risk assessment ---------------------------------------------------------

bg_ct_risk <- bg_ct_data %>%
  left_join(classify_risk_renter(bg_ct_data), by = "GISJOIN_proj") %>%
  left_join(classify_risk_owner(bg_ct_data),  by = "GISJOIN_proj") %>%
  mutate(
    # Vulnerability filters in the classifiers drop BGs that don't qualify
    # for analysis (low income, sufficient sample, etc.). Those BGs join in
    # with NA risk; treat as "low" so the app always has a value.
    risk_level_renter = factor(
      if_else(is.na(risk_level_renter), "low", as.character(risk_level_renter)),
      levels = c("low", "medium", "high")),
    risk_level_owner = factor(
      if_else(is.na(risk_level_owner), "low", as.character(risk_level_owner)),
      levels = c("low", "medium", "high")),
    # Phase 4.2b.3b TRANSITIONAL: keep a combined `risk_level` column as
    # the max of (renter, owner) so the app's existing
    # mod_affordability_rental + mod_affordability_ownership references
    # to risk_level continue to work between this commit and 4.2b.4.
    # 4.2b.4 wires the modules to risk_level_renter / risk_level_owner
    # directly and drops this column.
    risk_level = factor(
      pmax(as.integer(risk_level_renter), as.integer(risk_level_owner)),
      levels = 1:3, labels = c("low", "medium", "high"))
  )

# Keep the data objects needed downstream plus every helper function (config
# lists, bin-interpolation, classifier, and the R/validate.R helpers).
keep_objs <- c("bg2020", "bg_ct_data", "bg_ct_risk", "local_area",
               "locality", "vintages", "params")
keep_fns  <- ls()[vapply(ls(), function(x) is.function(get(x)), logical(1))]
rm(list = setdiff(ls(), c(keep_objs, keep_fns)))

local_area %>%
  select(GISJOIN_proj, GISJOIN_CT) %>%
  write_csv(., here("DHNA", "data", "local_area.csv"))

write_csv(bg_ct_risk, here("DHNA", "data", "LVM_Risk_Database.csv"))

# Validation ---------------------------------------------------------------
validation_banner("Stage 07 — risk classification & outputs")
check_rows_equal(bg_ct_risk, "bg_ct_risk", nrow(bg_ct_data), "bg_ct_data")
check_not_all_na(bg_ct_risk, "risk_level_renter")
check_not_all_na(bg_ct_risk, "risk_level_owner")
check_not_all_na(bg_ct_risk, "risk_level")
for (col in c("risk_level_renter", "risk_level_owner", "risk_level")) {
  shares <- prop.table(table(bg_ct_risk[[col]]))
  max_share <- max(shares)
  dhna_check(max_share < 0.99,
             sprintf("%s distribution not degenerate", col),
             sprintf("largest class %.1f%%", max_share * 100), "warn")
}
dhna_check(file.exists(here("DHNA", "data", "LVM_Risk_Database.csv")),
           "LVM_Risk_Database.csv written", severity = "error")
dhna_check(file.exists(here("DHNA", "data", "local_area.csv")),
           "local_area.csv written", severity = "error")
