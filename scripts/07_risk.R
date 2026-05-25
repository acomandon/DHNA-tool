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

rm(list=setdiff(ls(), c("bg2020", "bg_ct_data", "bg_ct_risk", "local_area",
                        "locality", "vintages", "params",
                        "med_lin_est", "renter_adj", "classify_risk")))

local_area %>%
  select(GISJOIN_proj, GISJOIN_CT) %>%
  write_csv(., here("DHNA", "data", "local_area.csv"))

write_csv(bg_ct_risk, here("DHNA", "data", "LVM_Risk_Database.csv"))
