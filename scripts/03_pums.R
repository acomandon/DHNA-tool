# Stage 03 — PUMS household microdata
# Build the renter household-level frame used for the app's housing
# mismatch and rent burden figures.
#
# Depends on: PUMS extract already downloaded into data/pums_usa/acs_21_23/.
# Produces: HUD_FMI (used again in stage 06), ky_h.
# Writes: DHNA/data/hh_micro.csv.

# HUD HOME Program income limits (ELI 30% / VLI 50% / LI 80%) by household
# size, one block per PUMS sample year, for the Louisville/Jefferson County,
# KY-IN HUD Metro FMR Area. Sourced verbatim from HUD's FY HOME Income Limits.
# Refresh: when the PUMS window (config vintages$pums_samples) rolls forward,
# add the new year's 8 rows to data/prepackaged/hud_fmi.csv.
HUD_FMI <- read_csv(here("data", "prepackaged", "hud_fmi.csv"))

pums_acs <- list.files(here("data", "pums_usa", "acs_21_23"),
                       pattern = "\\.xml$",
                            full.names = TRUE)

ddi <- read_ipums_ddi(pums_acs)


# Affordability
# renters
ky_h <- read_ipums_micro(ddi) %>%
  filter(PUMA %in% locality$pumas,
         HHINCOME < 9999999,
         OWNERSHP == 2
  ) %>%
  mutate(HH_ID = paste(SAMPLE, SERIAL),
         HHINCOME = if_else(HHINCOME < 0, 0, HHINCOME)) %>%
  left_join(., HUD_FMI) %>%
  mutate(HHINC_levels = if_else(HHINCOME < MFI_ELI, "Below 30%  ($27,475)", NA),
         HHINC_levels = if_else(HHINCOME >= MFI_ELI & HHINCOME < MFI_VLI, "30% to 50% ($45,800)", HHINC_levels),
         HHINC_levels = if_else(HHINCOME >= MFI_VLI & HHINCOME < MFI_LI, "50% to 80% ($73,250)", HHINC_levels),
         HHINC_levels = if_else(HHINCOME >= MFI_LI, "Above 80%", HHINC_levels),
         RENT_levels = if_else(RENTGRS < (MFI_ELI*.3)/12,"Below 30%  ($687)", NA),
         RENT_levels = if_else(RENTGRS >= (MFI_ELI*.3)/12 & RENTGRS < (MFI_VLI*.3)/12, "30% to 50% ($1145)", RENT_levels),
         RENT_levels = if_else(RENTGRS >= (MFI_VLI*.3)/12 & RENTGRS < (MFI_LI*.3)/12, "50% to 80% ($1831)", RENT_levels),
         RENT_levels = if_else(RENTGRS >= (MFI_LI*.3)/12, "Above 80%  (market)", RENT_levels),
         HHINC_levels = factor(HHINC_levels, levels = c("Below 30%  ($27,475)",
                                                        "30% to 50% ($45,800)",
                                                        "50% to 80% ($73,250)",
                                                        "Above 80%")),
         RENT_levels = factor(RENT_levels, levels = c("Below 30%  ($687)",
                                                      "30% to 50% ($1145)",
                                                      "50% to 80% ($1831)",
                                                      "Above 80%  (market)")),
         rent_burden = if_else(HHINCOME*.3/12 > RENTGRS, "No burden", NA),
         rent_burden = if_else(HHINCOME*.5/12 <= RENTGRS, "Severely burdened", rent_burden),
         rent_burden = if_else(HHINCOME*.3/12 <= RENTGRS & HHINCOME*.5/12 > RENTGRS, "Burdened", rent_burden),
         rent_burden = factor(rent_burden, levels = c("No burden",
                                                      "Burdened",
                                                      "Severely burdened"))) %>%
  select(HHWT, HHINC_levels, RENT_levels,RENTGRS, rent_burden)
head(ky_h)
write_csv(ky_h, here("DHNA", "data", "hh_micro.csv"))

# Validation ---------------------------------------------------------------
validation_banner("Stage 03 — PUMS household microdata")
check_min_rows(ky_h, "ky_h (renter PUMS)", 100)
check_na_share(ky_h, "HHINC_levels", 0.95, "warn")
check_na_share(ky_h, "RENT_levels", 0.95, "warn")
dhna_check(sum(ky_h$HHWT, na.rm = TRUE) > 0, "HHWT sums positive",
           sprintf("weighted renters %.0f", sum(ky_h$HHWT, na.rm = TRUE)), "error")
