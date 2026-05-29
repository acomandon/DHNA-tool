# Stage 07 — Risk classification and outputs
# Apply the tenure-aware displacement-risk classifiers and write the app's
# final CSV outputs.
#
# Depends on: bg_ct_data from stage 06; local_area from stage 02;
#   classify_risk_renter() and classify_risk_owner() from R/risk_classifier.R
#   (matrix-driven interpreters consuming risk_matrix from R/risk_matrix.R).
# Writes: DHNA/data/LVM_Risk_Database.csv, DHNA/data/local_area.csv.
#
# The classifiers now return (risk_level, dominant_family) per tenure.
# BGs that fail the vulnerability gates fall through with risk_level = "low"
# and dominant_family = NA (no rule fired → no dominant signal family).

# Risk assessment ---------------------------------------------------------

bg_ct_risk <- bg_ct_data %>%
  left_join(classify_risk_renter(bg_ct_data), by = "GISJOIN_proj") %>%
  left_join(classify_risk_owner(bg_ct_data),  by = "GISJOIN_proj") %>%
  mutate(
    # Classifier outputs are already factor-coded with "low" for vuln-fail
    # BGs; dominant_family remains NA for those (no rule fired → no signal).
    risk_level_renter = forcats::fct_na_value_to_level(risk_level_renter, level = "low"),
    risk_level_owner  = forcats::fct_na_value_to_level(risk_level_owner,  level = "low")
  )

# Keep the data objects needed downstream plus every helper function (config
# lists, bin-interpolation, classifier, and the R/validate.R helpers).
keep_objs <- c("bg2020", "bg_ct_data", "bg_ct_risk", "local_area",
               "locality", "vintages", "params", "risk_matrix", "forecast")
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
for (col in c("risk_level_renter", "risk_level_owner")) {
  shares <- prop.table(table(bg_ct_risk[[col]]))
  max_share <- max(shares)
  dhna_check(max_share < 0.99,
             sprintf("%s distribution not degenerate", col),
             sprintf("largest class %.1f%%", max_share * 100), "warn")
}
# Dominant family: should be populated wherever risk > low.
for (tenure in c("renter", "owner")) {
  lvl_col <- paste0("risk_level_",      tenure)
  fam_col <- paste0("dominant_family_", tenure)
  flagged <- bg_ct_risk[[lvl_col]] %in% c("medium", "high")
  missing_fam <- flagged & is.na(bg_ct_risk[[fam_col]])
  dhna_check(!any(missing_fam),
             sprintf("%s populated for all medium/high %s BGs", fam_col, tenure),
             sprintf("%d BG(s) missing", sum(missing_fam)),
             severity = "warn")
}
dhna_check(file.exists(here("DHNA", "data", "LVM_Risk_Database.csv")),
           "LVM_Risk_Database.csv written", severity = "error")
dhna_check(file.exists(here("DHNA", "data", "local_area.csv")),
           "local_area.csv written", severity = "error")
