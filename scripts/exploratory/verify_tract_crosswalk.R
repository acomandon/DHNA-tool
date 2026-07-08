# Explicit geometric verification of 2010->2020 Jefferson tract changes,
# to validate the A2 crosswalk assumption (are all changes clean splits?).
# READ-ONLY analysis. Fetches the 2010 tract shapefile, overlays on the 2020
# tract geometry already on disk, and classifies every change.
#
# Run (R 4.5 + key):
#   R_ENVIRON_USER="C:/Users/Andre/Documents/.Renviron" \
#     "C:/Program Files/R/R-4.5.3/bin/Rscript.exe" --no-init-file \
#     scripts/exploratory/verify_tract_crosswalk.R

suppressMessages({library(ipumsr); library(sf); library(dplyr); library(here); library(stringr)})
source(here("R","config.R"))

dl <- here("data","nhgis","gis","tract","ct2010")
if (!file.exists(file.path(dl, "done.flag"))) {
  if (dir.exists(dl)) unlink(dl, recursive=TRUE, force=TRUE); dir.create(dl, recursive=TRUE)
  message("Fetching 2010 tract shapefile ...")
  define_extract_nhgis(description="2010 tracts KY for crosswalk verify",
                       shapefiles="us_tract_2010_tl2010") |>
    submit_extract() |> wait_for_extract() |> download_extract(download_dir=dl)
  file.create(file.path(dl, "done.flag"))
}

t10 <- read_ipums_sf(list.files(dl, pattern="\\.zip$", full.names=TRUE)) |>
  filter(STATEFP10=="21", COUNTYFP10=="111") |>
  transmute(t2010 = paste0(STATEFP10, COUNTYFP10, TRACTCE10)) |>
  st_transform(2246) |> st_make_valid()
t20 <- st_read(here("data","nhgis","gis","tract","ct2023","KY_Jefferson_tract_2023.shp"), quiet=TRUE) |>
  st_transform(2246) |> st_make_valid()
# derive 11-digit 2020 GEOID
g20col <- intersect(c("GEOID","GEOIDFQ","GEO_ID"), names(t20))[1]
t20 <- t20 |> mutate(t2020 = str_sub(str_remove(.data[[g20col]], "^.*US"), 1, 11)) |>
  select(t2020)
message(sprintf("2010 tracts: %d | 2020 tracts: %d", nrow(t10), nrow(t20)))

# Pairwise intersection areas
t10$area10 <- as.numeric(st_area(t10))
t20$area20 <- as.numeric(st_area(t20))
int <- st_intersection(t10, t20)          # carries area10 + area20 through
int$iarea <- as.numeric(st_area(int))
ov <- int |> st_drop_geometry() |>
  filter(iarea > 1000) |>                              # drop sliver artifacts (<1000 sq ft)
  mutate(frac_of_2020 = iarea/area20,                  # share of the 2020 tract from this 2010 tract
         frac_of_2010 = iarea/area10)

# For each 2020 tract: how many 2010 tracts contribute >5% of its area?
per20 <- ov |> filter(frac_of_2020 > 0.05) |>
  group_by(t2020) |>
  summarise(n_parents = n(), top_frac = max(frac_of_2020),
            parents = paste(sort(t2010), collapse=","), .groups="drop")

gid <- function(x) x
matched_geoid <- intersect(t10$t2010, t20$t2020)   # same GEOID both years
per20 <- per20 |>
  mutate(has_geoid_match = t2020 %in% matched_geoid,
         clean_nest = n_parents==1 & top_frac>=0.95)

cat("\n=== 2020 tracts by parentage ===\n")
cat("Cleanly nested in ONE 2010 tract (>=95%):", sum(per20$clean_nest), "of", nrow(per20), "\n")
cat("Span MULTIPLE 2010 tracts (>5% each):", sum(per20$n_parents>1), "\n\n")

cat("=== 2020 tracts spanning multiple 2010 parents (need weighted crosswalk) ===\n")
print(as.data.frame(per20 |> filter(n_parents>1) |>
  transmute(t2020, n_parents, top_frac=round(top_frac,2), parents) |> arrange(desc(n_parents))))

# Verify the GEOID-matched tracts are truly ~unchanged geometrically (IoU)
cat("\n=== GEOID-matched tracts: geometric identity check ===\n")
mm <- ov |> filter(t2010==t2020) |>
  mutate(iou = iarea/(area10+area20-iarea))
cat("matched pairs:", nrow(mm),
    " | min IoU:", round(min(mm$iou),3),
    " | # with IoU<0.95:", sum(mm$iou<0.95), "\n")
bad <- mm |> filter(iou<0.95) |> arrange(iou)
if (nrow(bad)) print(as.data.frame(bad |> transmute(t2020, iou=round(iou,3))))

write.csv(per20, here("data","processed","audits","tract_parentage.csv"), row.names=FALSE)
cat("\nWrote tract_parentage.csv\n")
