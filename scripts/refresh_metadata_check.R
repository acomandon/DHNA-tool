# One-off helper for the Goal #2 data refresh.
#
# Read-only IPUMS metadata queries to confirm the exact dataset / table /
# shapefile / sample codes to put in R/config.R, and to surface the new NHGIS
# code prefixes needed when remapping stages 01 and 06 to the refreshed ACS.
#
# It submits NO extracts and downloads NO data — just metadata lookups.
# Requires an IPUMS API key (already configured if you have run
# data_downloader.R before; otherwise set_ipums_api_key("KEY", save = TRUE)).
#
# Uses the current ipumsr metadata interface:
#   get_metadata_catalog(collection, metadata_type)  -> list available records
#   get_metadata(collection, dataset = ...)          -> detail for one record
#
# Run interactively and paste the printed output back, or read it off directly.

library(ipumsr)
library(dplyr)

# Census tables the pipeline pulls from the recent ACS 5-year.
bg_tables <- c("B03002", "B15003", "B25002", "B25063", "B25075", "B19001")
ct_tables <- c("B25140", "B01001", "B11005", "C16002", "B25118", "C18130")
needed    <- c(bg_tables, ct_tables)

section <- function(title) cat(sprintf("\n========== %s ==========\n", title))

# Detailed table list for one dataset (gives the per-table nhgis_code prefix),
# tolerant of either metadata-interface shape.
get_tables <- function(d) {
  out <- tryCatch(get_metadata("nhgis", dataset = d)$data_tables,
                  error = function(e) NULL)
  if (is.null(out)) {
    out <- tryCatch(get_metadata_catalog("nhgis", "data_tables", dataset = d),
                    error = function(e) NULL)
  }
  out
}

# 1. Which 2020-2024 ACS5 datasets exist (a / b / c ...) ---------------------
section("NHGIS datasets matching 2020_2024_ACS5")
datasets <- tryCatch(get_metadata_catalog("nhgis", "datasets"),
                     error = function(e) { cat("  ERROR:", conditionMessage(e), "\n"); NULL })
cands <- character(0)
if (!is.null(datasets)) {
  hit <- datasets %>% filter(grepl("2020_2024_ACS5", name, ignore.case = TRUE))
  print(as.data.frame(hit[, intersect(c("name", "description"), names(hit))]))
  cands <- hit$name
}

# 2. For each candidate, which needed tables it holds + their NEW code prefix -
section("Needed tables and their new NHGIS code prefixes, per sub-dataset")
for (d in cands) {
  dt <- get_tables(d)
  if (is.null(dt)) { cat("  ", d, ": could not read data tables\n"); next }
  rows <- dt %>% filter(name %in% needed)
  if (nrow(rows) > 0) {
    cat(sprintf("\n  -- %s --\n", d))
    print(as.data.frame(rows))   # show all columns incl. the nhgis_code prefix
  }
}

# 3. 2024 TIGER shapefiles (state block group, US tract) --------------------
section("2024 shapefiles (block group / tract)")
shp <- tryCatch(get_metadata_catalog("nhgis", "shapefiles"),
                error = function(e) { cat("  ERROR:", conditionMessage(e), "\n"); NULL })
if (!is.null(shp)) {
  print(as.data.frame(
    shp %>% filter(grepl("2024", name), grepl("blck_grp|tract", name)) %>%
      select(any_of(c("name", "year", "geographic_level")))
  ))
}

# 4. IPUMS USA samples for the rolled PUMS window ---------------------------
section("IPUMS USA samples (expect us2022a / us2023a / us2024a)")
samp <- tryCatch(get_sample_info("usa"),
                 error = function(e) { cat("  ERROR:", conditionMessage(e), "\n"); NULL })
if (!is.null(samp)) {
  print(as.data.frame(samp %>% filter(grepl("us202[2-4]a", name))))
}

cat("\nDone. Paste the output back so config.R and the stage code maps can be set precisely.\n")
