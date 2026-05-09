# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

This repo builds the **Displacement and Housing Need Assessment (DHNA) Tool**, a Shiny app originally developed for Louisville Metro Government's Office of Housing and Community Development. It assesses residential displacement risk and affordable housing need at the Census block-group level.

The codebase is entirely R. There is no build/lint/test tooling — workflow is "source the script in RStudio."

## Two-stage pipeline

The repo has a strict two-stage flow. Understanding the boundary between them is essential:

1. **`scripts/data_downloader.R`** — pulls raw data from the IPUMS NHGIS and IPUMS USA APIs into `data/` (block, blockgroup, tract, crosswalks, GIS shapefiles, PUMS). Run **once**; expensive (submits IPUMS extracts and waits). Requires an IPUMS API key configured per the [ipumsr API vignette](https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums-api.html).
2. **`scripts/data_prep.R`** — reads from `data/` plus prepackaged inputs in `data/prepackaged/` (HUD AFFH, permits, Renthub rents, market areas, affordable units), standardizes everything to **2020 block-group geography** using NHGIS crosswalks, computes risk variables, and writes the small set of CSV/SHP files the Shiny app actually loads into `DHNA/data/` and `DHNA/data/gis/`.
3. **`DHNA/app.R`** — the Shiny app. Loads only the artifacts produced in step 2; does **not** read from `data/` or call any APIs at runtime. Open `build_tool.Rproj` in RStudio, then `DHNA/app.R`, then click **Run App**.

The line `source(here("scripts", "data_downloader.R"))` near the top of `data_prep.R` is **commented out by default** — uncomment only on first run, then re-comment.

## Key architectural points

- **Geography standardization is the spine of `data_prep.R`.** 1990 and 2000 decennial block data are walked through 1990→2010 and 2000→2010 NHGIS crosswalks, then both 2010 and 2009-2013 ACS data are walked through the 2010→2020 crosswalk so every variable lives on 2020 block groups. Weights differ by quantity: `wt_pop` for people, `wt_adult` for adults, `wt_hu` for housing units, `wt_renthu` for renter units, `wt_ownhu` for owner units. Don't apply the wrong weight.
- **Local area** for each block group = the set of 2020 BGs in the same tract whose 2020 population centers fall within 800m of the focal BG's population center. Used for local context tables and the ethnoracial-change comparisons.
- **Median estimation from ACS bins** uses the custom `med_lin_est()` helper (linear interpolation across the bin containing the median); `renter_adj()` does a similar interpolation to estimate renters under an FMI cutoff. Both live at the top of `data_prep.R`.
- **Risk classification** (high/medium/low) is the `risk_class` block at the bottom of `data_prep.R` (~line 954). It's a rule-based scoring system over percentile-rank variables (rents, home values, college share, income, ethnoracial change, permits, vacancy). Changes here directly drive what users see in the app.
- **Inflation adjustment**: `data_prep.R` and `app.R` both hard-code `CPI_13_23 = 1.3086` (BLS, July 2013 → July 2023) for comparing 2009-2013 ACS to 2019-2023 ACS dollars.
- **Geographic scope is hard-coded to Kentucky/Jefferson County** (`STATEFP == "21"`, `COUNTYFP == "111"`, NHGIS `geographic_extents = 210`). Adapting to another locality means changing these in both scripts plus the prepackaged inputs in `data/prepackaged/`.

## Required data not pulled by `data_downloader.R`

These come pre-committed under `data/prepackaged/` and are Louisville-specific:
- `Market_Areas/` — comprehensive plan market area shapefile
- `affordable/` — affordable housing unit database (geocoded)
- `permits/` — building permit records
- `renthub/` — quarterly rent observations (Renthub)
- `hud/` — HUD AFFH data (zipped, unzipped inside `data_prep.R`)

## Shiny app structure (`DHNA/app.R`)

Single-file ~1080-line app using `bslib::page_navbar`. Top-level nav panels: **Introduction**, **Housing Need**, **Location**. The Location panel contains nested panels (Affordability levels, Area overview) that drive the displacement risk lookup by clicked map location. All inputs are CSVs/SHPs in `DHNA/data/` produced by `data_prep.R`.
