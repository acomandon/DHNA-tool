# Stage 07 — Risk classification and outputs
# Apply the displacement-risk classifier and write the app's final
# CSV outputs.
#
# Depends on: bg_ct_data from stage 06; local_area from stage 02;
#   classify_risk() from R/risk_classifier.R.
# Writes: DHNA/data/LVM_Risk_Database.csv, DHNA/data/local_area.csv.

# Risk assessment ---------------------------------------------------------

bg_ct_risk <- left_join(bg_ct_data, classify_risk(bg_ct_data), by = "GISJOIN_proj") %>%
  mutate(risk_level = if_else(is.na(risk_level) == TRUE, "low", risk_level))

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
check_not_all_na(bg_ct_risk, "risk_level")
max_share <- max(prop.table(table(bg_ct_risk$risk_level)))
dhna_check(max_share < 0.99, "risk_level distribution not degenerate",
           sprintf("largest class %.1f%%", max_share * 100), "warn")
dhna_check(file.exists(here("DHNA", "data", "LVM_Risk_Database.csv")),
           "LVM_Risk_Database.csv written", severity = "error")
dhna_check(file.exists(here("DHNA", "data", "local_area.csv")),
           "local_area.csv written", severity = "error")
