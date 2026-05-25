# Stage 03 — PUMS household microdata
# Build the renter household-level frame used for the app's housing
# mismatch and rent burden figures.
#
# Depends on: PUMS extract already downloaded into data/pums_usa/acs_21_23/.
# Produces: HUD_FMI (used again in stage 06), ky_h.
# Writes: DHNA/data/hh_micro.csv.

# Create table with income limits based on HOME program
# ELI: Extremely low income 30% limit
# VLI: Very low income 50% limit
# LI: Low income 80% limit

HUD_FMI <- data.frame(YEAR = c(rep(2023,8), rep(2022,8), rep(2021,8)),
                      NUMPREC = c(rep(seq(1,8,1),3)),
                      MFI_ELI = c(18850,21550,24240,26900,29100,31250,33400,35550,
                                  17800,20350,23030,27750,32470,37190,41910,46630,
                                  16150,18450,21960,26500,31040,35580,40120,44660),
                      MFI_VLI = c(31400,35900,40400,44850,48450,52050,55650,59250,
                                  29650,33900,38150,42350,45750,49150,52550,55950,
                                  26950,30800,34650,38450,41550,44650,47700,50800),
                      MFI_LI = c(50250,57400,64600,71750,77500,83250,89000,98750,
                                 47450,54200,61000,67750,73200,78600,84050,89450,
                                 43050,49200,55350,61500,66450,71350,76300,81200))

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
  mutate(HHINC_levels = if_else(HHINCOME < MFI_ELI, "Below 30%  ($27,125)", NA),
         HHINC_levels = if_else(HHINCOME >= MFI_ELI & HHINCOME < MFI_VLI, "30% to 50% ($40,400)", HHINC_levels),
         HHINC_levels = if_else(HHINCOME >= MFI_VLI & HHINCOME < MFI_LI, "50% to 80% ($64,625)", HHINC_levels),
         HHINC_levels = if_else(HHINCOME >= MFI_LI, "Above 80%", HHINC_levels),
         RENT_levels = if_else(RENTGRS < (MFI_ELI*.3)/12,"Below 30%  ($678)", NA),
         RENT_levels = if_else(RENTGRS >= (MFI_ELI*.3)/12 & RENTGRS < (MFI_VLI*.3)/12, "30% to 50% ($1010)", RENT_levels),
         RENT_levels = if_else(RENTGRS >= (MFI_VLI*.3)/12 & RENTGRS < (MFI_LI*.3)/12, "50% to 80% ($1615)", RENT_levels),
         RENT_levels = if_else(RENTGRS >= (MFI_LI*.3)/12, "Above 80%  (market)", RENT_levels),
         HHINC_levels = factor(HHINC_levels, levels = c("Below 30%  ($27,125)",
                                                        "30% to 50% ($40,400)",
                                                        "50% to 80% ($64,625)",
                                                        "Above 80%")),
         RENT_levels = factor(RENT_levels, levels = c("Below 30%  ($678)",
                                                      "30% to 50% ($1010)",
                                                      "50% to 80% ($1615)",
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
