# C-thread broadening — pocket dilution on income + rent axes, to test which
# of the low-income-share confident pockets are robust across axes. READ-ONLY.
#
# Axis 1 (anchor, MOE-confident): low-income HH share (from pocket_confidence.R).
# Axis 2: median household income (level) — focal-own vs pooled (bin-estimated
#         via med_lin_est, same as the pipeline). Direction only, no MOE.
# Axis 3: median gross rent (level) — same.
# A robust vulnerable pocket is one where the focal BG is confidently poorer on
# low-income share AND poorer on median income AND cheaper on median rent.

suppressMessages({library(ipumsr); library(dplyr); library(readr); library(here); library(stringr); library(tidyr)})
source(here("R","config.R")); source(here("R","bin_interpolation.R"))

# Bin cell -> "prefix_upperbound" name maps (matching stage 01 / med_lin_est).
inc_cells <- setNames(paste0("AURTE0", sprintf("%02d", 2:17)),
  paste0("HHINC_", c(10000,14999,19999,24999,29999,34999,39999,44999,49999,
                     59999,74999,99999,124999,149999,199999,250000)))
rent_cells <- setNames(paste0("AUWFE0", sprintf("%02d", 3:26)),
  paste0("rent_", c(100,149,199,249,299,349,399,449,499,549,599,649,699,749,
                    799,899,999,1249,1499,1999,2499,2999,3499,3500)))

bg <- suppressWarnings(read_nhgis(list.files(here("data","nhgis","blockgroup","bg2020"),
        pattern="csv.zip$", full.names=TRUE), verbose=FALSE)) %>%
  filter(STATE == locality$state_name, COUNTY == locality$county_name)

# focal-own medians via med_lin_est on each BG's own bins
own_median <- function(row, cellmap) {
  v <- as.numeric(row[cellmap]); nm <- names(cellmap)
  if (sum(v, na.rm=TRUE) < 30) return(NA_real_)      # too few to estimate
  med_lin_est(nm, v)
}
bgm <- bg %>% rowwise() %>%
  mutate(inc_own  = own_median(cur_data(), inc_cells),
         rent_own = own_median(cur_data(), rent_cells)) %>%
  ungroup() %>%
  transmute(GISJOIN_proj = GISJOIN, inc_own, rent_own)

# pooled medians + the low-income-share pocket flags
db <- read_csv(here("DHNA","data","LVM_Risk_Database.csv"), show_col_types=FALSE) %>%
  transmute(GISJOIN_proj, inc_pool = median_hhinc_20, rent_pool = median_rent_20,
            risk_level_renter)
pk <- read_csv(here("data","processed","audits","exp_C_pocket_confidence.csv"), show_col_types=FALSE) %>%
  mutate(gap_vuln = (share_own - share_pool)*100,
         share_pocket = gap_vuln >= 5 & abs(gap_vuln) > moe_share*100) %>%
  select(GISJOIN_proj, share_own, share_pool, gap_vuln, share_pocket)

m <- db %>% inner_join(bgm, by="GISJOIN_proj") %>% inner_join(pk, by="GISJOIN_proj") %>%
  mutate(inc_gap  = inc_pool - inc_own,      # + = focal poorer than pool
         rent_gap = rent_pool - rent_own,    # + = focal cheaper than pool
         inc_poorer  = inc_gap  > 0.10*inc_pool,    # focal >10% poorer
         rent_cheaper = rent_gap > 0.10*rent_pool)  # focal >10% cheaper

cat("=== Axis agreement across all 608 BGs ===\n")
cat("low-income-share confident pockets:", sum(m$share_pocket), "\n")
cat("  of those ALSO poorer on median income:", sum(m$share_pocket & m$inc_poorer), "\n")
cat("  of those ALSO cheaper on median rent :", sum(m$share_pocket & m$rent_cheaper), "\n")
cat("  robust on all THREE axes            :", sum(m$share_pocket & m$inc_poorer & m$rent_cheaper), "\n")

cat("\n=== The 13 low-income-share pockets (currently low) across axes ===\n")
gj2geoid <- function(gj) paste0(str_sub(gj,2,3), str_sub(gj,5,7), str_sub(gj,9,14), str_sub(gj,15,15))
out <- m %>% filter(share_pocket, risk_level_renter=="low") %>%
  transmute(GEOID = gj2geoid(GISJOIN_proj),
            loinc_gap_pp = round(gap_vuln),
            inc_own = round(inc_own), inc_pool = round(inc_pool),
            inc_poorer,
            rent_own = round(rent_own), rent_pool = round(rent_pool),
            rent_cheaper,
            axes = 1 + inc_poorer + rent_cheaper) %>%
  arrange(desc(axes), desc(loinc_gap_pp))
print(as.data.frame(out), row.names=FALSE)
write_csv(out, here("data","processed","audits","exp_C_pocket_axes.csv"))
