# Renter-weighted 2010->2020 tract crosswalk for the A2 low-income-renter
# signal. Aggregates the NHGIS bg2010->bg2020 crosswalk (wt_renthu, the same
# weight stage 01 uses) up to a tract->tract renter-flow map, then interpolates
# the 2010 low-income-renter share onto 2020 tracts as a renter-flow-weighted
# average. Handles unchanged / splits / merges / redraws uniformly and is
# immune to the GEOID-reuse trap (6 Jefferson tracts share a GEOID across
# vintages with as little as 43% overlap).
#
# A2 measure: low-income renters / renters (share of the rental stock that is
# low-income). Both numerator and denominator are renter quantities, so the
# renter-weighted crosswalk is a clean renter-flow-weighted average of the
# 2010 tract shares. CPI-aligned: 2010 <$25k, 2020 <$35k.
#
# READ-ONLY verification. Run (R 4.5 + key):
#   R_ENVIRON_USER="C:/Users/Andre/Documents/.Renviron" \
#     "C:/Program Files/R/R-4.5.3/bin/Rscript.exe" --no-init-file \
#     scripts/exploratory/build_lir_crosswalk.R

suppressMessages({library(ipumsr); library(dplyr); library(here); library(stringr); library(tidyr)})
pr <- function(x){r<-rank(x,na.last="keep"); ceiling(r/max(r,na.rm=T)*100)}
gid <- function(x) str_extract(x, "(?<=US)[0-9]+")

# --- 1. renter-flow tract crosswalk from the BG crosswalk -----------------
xw <- as.data.frame(read_nhgis(here("data","nhgis","crosswalks","nhgis_bg2010_bg2020_21.zip"), verbose=FALSE))
bg10 <- as.data.frame(read_nhgis(list.files(here("data","nhgis","blockgroup","bg2010"),
                                            pattern="csv.zip$", full.names=TRUE), verbose=FALSE))
renters10 <- bg10 |> transmute(bg2010ge = as.character(GEOID) |> (\(x) str_sub(str_remove(x,"^.*US"),1,12))(),
                               r_bg = UL8E001)
flow <- xw |>
  mutate(bg2010ge = as.character(bg2010ge), bg2020ge = as.character(bg2020ge),
         t2010 = str_sub(bg2010ge,1,11), t2020 = str_sub(bg2020ge,1,11)) |>
  left_join(renters10, by="bg2010ge") |>
  mutate(renter_flow = coalesce(r_bg,0) * wt_renthu) |>
  filter(str_sub(t2020,1,5)=="21111") |>              # 2020 tracts in Jefferson
  group_by(t2010, t2020) |>
  summarise(renter_flow = sum(renter_flow, na.rm=TRUE), .groups="drop") |>
  filter(renter_flow > 0)
cat("tract->tract flow pairs:", nrow(flow),
    "| distinct 2020 tracts:", n_distinct(flow$t2020), "\n")

# --- 2. 2010 tract low-income-renter share (CPI <$25k) --------------------
pri <- as.data.frame(read_nhgis(here("data","nhgis","tract","ct2010b","nhgis0030_csv.zip"), verbose=FALSE)) |>
  filter(STATE=="Kentucky", COUNTY=="Jefferson County")
pri2 <- pri |> transmute(t2010 = gid(GEOID),
                         renters_10 = U26E014,
                         share_10 = ifelse(U26E014>0, rowSums(across(paste0("U26E0",15:19)))/U26E014, NA))

# --- 3. crosswalk: renter-flow-weighted avg of 2010 shares onto 2020 tracts
cw <- flow |> left_join(pri2 |> select(t2010, share_10), by="t2010") |>
  filter(!is.na(share_10)) |>
  group_by(t2020) |>
  summarise(share_10_cw = weighted.mean(share_10, renter_flow),
            n_parents = n(), .groups="drop")

# --- 4. recent 2020 tract low-income-renter share (<$35k) -----------------
zips <- list.files(here("data","nhgis","tract","ct2020"), pattern="zip$", full.names=TRUE)
for (z in zips){d<-suppressWarnings(read_nhgis(z,verbose=FALSE)); if(any(grepl("^AVF6E",names(d)))){rec<-as.data.frame(d);break}}
rec <- rec |> filter(STATE=="Kentucky", COUNTY=="Jefferson County")
rec2 <- rec |> transmute(t2020 = gid(GEO_ID),
                         share_20 = ifelse(AVF6E014>0, rowSums(across(paste0("AVF6E0",15:20)))/AVF6E014, NA))

a2 <- rec2 |> left_join(cw, by="t2020") |>
  mutate(change = share_10_cw - share_20, rank_a2 = pr(change))
cat("2020 tracts with crosswalked prior share:", sum(!is.na(a2$share_10_cw)), "of", nrow(a2), "\n")
cat("median change (pp):", round(median(a2$change,na.rm=T)*100,2),
    " | declining:", round(mean(a2$change>0,na.rm=T)*100),"%\n\n")

# --- 5. verify 076 + the redrawn/GEOID-reuse tracts -----------------------
cat("=== 076 cluster (should stay high; HNA=medium) ===\n")
print(as.data.frame(a2 |> filter(t2020 %in% c("21111007602","21111007603")) |>
  transmute(t2020, share_10_cw=round(share_10_cw,3), share_20=round(share_20,3),
            change_pp=round(change*100,1), n_parents, rank_a2)))

cat("\n=== redrawn / GEOID-reuse tracts: crosswalked vs naive same-GEOID join ===\n")
redrawn <- c("21111013100","21111007700","21111008800","21111007501","21111010005","21111009105")
naive <- pri2 |> transmute(t2020=t2010, share_10_naive=share_10)   # if we'd joined on GEOID
chk <- a2 |> filter(t2020 %in% redrawn) |>
  left_join(naive, by="t2020") |>
  transmute(t2020, n_parents,
            share_10_crosswalk=round(share_10_cw,3),
            share_10_naiveGEOID=round(share_10_naive,3),
            share_20=round(share_20,3))
print(as.data.frame(chk))

write.csv(a2, here("data","processed","audits","exp_A2_crosswalked.csv"), row.names=FALSE)
cat("\nWrote exp_A2_crosswalked.csv\n")
