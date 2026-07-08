# C-thread — pocket detection (C-a) + confidence (C-b). READ-ONLY.
#
# For each focal BG, compare its OWN (unpooled) low-income household share to
# its LOCAL-AREA-POOLED share (what the classifier uses). The gap is the
# dilution: focal poorer than its pool = a vulnerable pocket diluted up into a
# more affluent landscape (the 076 case). C-b adds ACS-MOE confidence: is the
# gap larger than the focal BG's own margin of error, or is it noise?
#
# Uses only existing data: the raw bg2020 NHGIS extract (which carries MOE
# cells AURTM* alongside the AURTE* estimates) + the pooled shares already in
# LVM_Risk_Database.csv. No new fetch.
#
# Run (R 4.5 + key): R_ENVIRON_USER=".../Documents/.Renviron" \
#   "C:/Program Files/R/R-4.5.3/bin/Rscript.exe" --no-init-file \
#   scripts/exploratory/pocket_confidence.R

suppressMessages({library(ipumsr); library(dplyr); library(readr); library(here); library(stringr)})
source(here("R","config.R"))

# Low-income HH (<$35k) 2020: AURTE002..E007; total HH AURTE001; MOEs AURTM*.
lo_e <- paste0("AURTE00", 2:7); lo_m <- paste0("AURTM00", 2:7)
bg <- suppressWarnings(read_nhgis(list.files(here("data","nhgis","blockgroup","bg2020"),
        pattern="csv.zip$", full.names=TRUE), verbose=FALSE)) %>%
  filter(STATE == locality$state_name, COUNTY == locality$county_name)

own <- bg %>% transmute(
  GISJOIN_proj = GISJOIN,
  HH_own   = AURTE001,
  lo_own   = rowSums(across(all_of(lo_e))),
  share_own = if_else(HH_own > 0, lo_own/HH_own, NA_real_),
  moe_lo   = sqrt(rowSums(across(all_of(lo_m))^2)),          # MOE of a sum
  moe_HH   = AURTM001) %>%
  mutate(
    # ACS proportion MOE (fall back to the sum form when the radicand < 0)
    rad = moe_lo^2 - (share_own^2) * (moe_HH^2),
    moe_share = if_else(HH_own > 0,
                        (1/HH_own) * sqrt(if_else(rad >= 0, rad, moe_lo^2 + (share_own^2)*(moe_HH^2))),
                        NA_real_))

db <- read_csv(here("DHNA","data","LVM_Risk_Database.csv"), show_col_types=FALSE) %>%
  transmute(GISJOIN_proj,
            share_pool = if_else(HH_20 > 0, lo_inc_hh_20/HH_20, NA_real_),
            risk_level_renter, median_hhinc_10)

m <- inner_join(own, db, by="GISJOIN_proj") %>%
  mutate(gap = share_pool - share_own,          # + = focal poorer than pool = diluted-up pocket
         gap_pp = gap*100,
         # confident the focal BG differs from its pool if |gap| exceeds its
         # own 90% MOE (ACS convention).
         confident = abs(gap) > moe_share,
         kind = case_when(
           abs(gap_pp) < 5            ~ "homogeneous",
           gap_pp >= 5  & confident   ~ "pocket: vulnerable, confident",
           gap_pp >= 5  & !confident  ~ "pocket: vulnerable, uncertain",
           gap_pp <= -5 & confident   ~ "reverse: focal richer, confident",
           TRUE                        ~ "reverse: focal richer, uncertain"))

cat("=== C-a: focal-own vs local-area-pooled low-income share ===\n")
cat("BGs:", nrow(m), "\n")
cat("median |gap|:", round(median(abs(m$gap_pp),na.rm=T),1), "pp",
    " | gap>10pp (strongly diluted):", sum(m$gap_pp>10,na.rm=T),
    " | gap< -10pp:", sum(m$gap_pp< -10,na.rm=T), "\n\n")
cat("Pocket classification (C-a magnitude x C-b confidence):\n")
print(as.data.frame(m %>% count(kind) %>% arrange(desc(n))))

cat("\nAmong currently-LOW renter BGs, how many are confident vulnerable pockets\n")
cat("(diluted up, gap>5pp, exceeds MOE) — candidates the dominant-landscape\n")
cat("default classifies low but the data confidently says a pocket exists:\n")
pk <- m %>% filter(risk_level_renter=="low", kind=="pocket: vulnerable, confident")
cat("  ", nrow(pk), "BGs\n")

cat("\n=== 076 cluster ===\n")
print(as.data.frame(m %>% filter(grepl("00760(21|22|31|33)", GISJOIN_proj)) %>%
  transmute(GISJOIN_proj, share_own=round(share_own,3), share_pool=round(share_pool,3),
            gap_pp=round(gap_pp,1), moe_pp=round(moe_share*100,1), confident, risk_level_renter)))

dir.create(here("data","processed","audits"), recursive=TRUE, showWarnings=FALSE)
write_csv(m, here("data","processed","audits","exp_C_pocket_confidence.csv"))
cat("\nWrote exp_C_pocket_confidence.csv\n")
