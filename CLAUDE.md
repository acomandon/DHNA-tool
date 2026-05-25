# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

This repo builds the **Displacement and Housing Need Assessment (DHNA) Tool**, a Shiny app originally developed for Louisville Metro Government's Office of Housing and Community Development. It assesses residential displacement risk and affordable housing need at the Census block-group level.

The codebase is entirely R. There is no build/lint/test tooling — workflow is "source the script in RStudio."

## Active project goals

Ongoing directions for the tool — keep these in mind when suggesting changes or scoping work:

1. **Robustness and efficiency review** of the existing pipeline and app.
2. **Refresh all data** to the latest available vintages (NHGIS datasets, ACS releases, PUMS samples, HUD AFFH, crosswalks, shapefiles).
3. **Split the assessment into separate rental and ownership project modules** — currently the risk model and UI treat housing as a single category.
4. **Update the displacement risk methodology** (the `risk_class` rule block in `data_prep.R` ~line 954). Open to methodological changes, not just parameter tweaks.
5. **Add a commercial gentrification measure** — a new dimension alongside the residential displacement risk currently computed.

## Pipeline structure

The repo has a strict two-stage flow:

1. **`scripts/data_downloader.R`** — pulls raw data from the IPUMS NHGIS and IPUMS USA APIs into `data/` (block, blockgroup, tract, crosswalks, GIS shapefiles, PUMS). Run **once**; expensive (submits IPUMS extracts and waits). Requires an IPUMS API key configured per the [ipumsr API vignette](https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums-api.html).
2. **`scripts/00_run_all.R`** — orchestrator that loads libraries, sources `R/` helpers, then sources stage scripts `01_geo_standardize.R` → `07_risk.R` in order. Reads from `data/` plus prepackaged inputs in `data/prepackaged/` (HUD AFFH, permits, Renthub rents, market areas, affordable units), standardizes everything to **2020 block-group geography** using NHGIS crosswalks, computes risk variables, and writes the small set of CSV/SHP files the Shiny app loads into `DHNA/data/` and `DHNA/data/gis/`.
3. **`DHNA/app.R`** — the Shiny app. Loads only the artifacts produced in step 2; does **not** read from `data/` or call any APIs at runtime. Open `build_tool.Rproj` in RStudio, then `DHNA/app.R`, then click **Run App**.

The line `source(here("scripts", "data_downloader.R"))` in `00_run_all.R` is **commented out by default** — uncomment only on first run, then re-comment.

### Stage scripts (sourced in order by `00_run_all.R`)

| Script | Produces |
|---|---|
| `01_geo_standardize.R` | Crosswalks 1990/2000/2010/2020 NHGIS data to 2020 BG geography (`bg90_20`, `bg00_20`, `bg10_20`, `bg2020`). |
| `02_local_area.R` | BG/tract geometry layers; the `local_area` unit (BGs in the same tract within 800m of the focal BG centroid); ethnoracial composition over time; writes `pop_ethnorace.csv` and the market-areas shapefile. |
| `03_pums.R` | PUMS-based renter household microdata for the housing-mismatch and rent-burden figures; writes `hh_micro.csv`. |
| `04_bg_variables.R` | BG-level population change, ethnoracial change, interpolated medians (rent / household income / home value), neighborhood vars. |
| `05_buffer_data.R` | Renthub asking-rent series, residential permits, affordable-unit counts within an 800m buffer of each BG population centroid; collates `bg_data`. |
| `06_tract_data.R` | HUD AFFH tract burden, NHGIS tract ACS, FMI-bracketed renter counts; collates `ct_data`, joins BG+CT, builds percentile ranks. |
| `07_risk.R` | Applies `classify_risk()` and writes `LVM_Risk_Database.csv` + `local_area.csv`. |

Inter-stage data flow is **source-in-one-session**: each stage assumes the workspace state from prior stages. Don't run a stage standalone — start from `00_run_all.R`.

## Key architectural points

- **Geography standardization is the spine of `data_prep.R`.** 1990 and 2000 decennial block data are walked through 1990→2010 and 2000→2010 NHGIS crosswalks, then both 2010 and 2009-2013 ACS data are walked through the 2010→2020 crosswalk so every variable lives on 2020 block groups. Weights differ by quantity: `wt_pop` for people, `wt_adult` for adults, `wt_hu` for housing units, `wt_renthu` for renter units, `wt_ownhu` for owner units. Don't apply the wrong weight.
- **Local area** for each block group = the set of 2020 BGs in the same tract whose 2020 population centers fall within 800m of the focal BG's population center. Used for local context tables and the ethnoracial-change comparisons.
- **Median estimation from ACS bins** uses the custom `med_lin_est()` helper (linear interpolation across the bin containing the median); `renter_adj()` does a similar interpolation to estimate renters under an FMI cutoff. Both live at the top of `data_prep.R`.
- **Risk classification** (high/medium/low) lives in `R/risk_classifier.R` as the pure function `classify_risk(bg_ct_data)`. It's a rule-based scoring system over the percentile-rank variables built in `06_tract_data.R` (rents, home values, college share, income, ethnoracial change, permits, vacancy). Magic-number thresholds inline by design — Goal #4 will overhaul.
- **Rental project recommender** lives in `R/project_recommender.R` as `recommend_project(...)`. Called from `DHNA/app.R` via a `build_recommendation()` closure shared by the `risk_assessment_res` and `recommendation` renderUI blocks.
- **Bin interpolation helpers** `med_lin_est()` and `renter_adj()` live in `R/bin_interpolation.R`. Used in stages 04 and 06 for ACS-bin median estimation and FMI-cutoff renter counts.
- **Configuration is centralized in `R/config.R`**, sourced by all three scripts. Three blocks: `locality` (FIPS codes, county/state names, NHGIS extent, PUMA list, default map center), `vintages` (NHGIS dataset codes, PUMS samples, shapefile codes — the things you'd edit during a data refresh), and `params` (CPI factor, buffer distances, projected CRS). Adapting to another locality is a one-file change here, plus replacing the locality-specific inputs in `data/prepackaged/`. **Not yet absorbed**: the HUD_FMI table (still inline in `data_prep.R`), risk-classification thresholds (Goal #4 will overhaul), and UI AMI dollar strings.
- **Dependencies are pinned with `renv`** (`renv.lock` at the project root). After cloning, run `renv::restore()` in R before sourcing anything.

## Required data not pulled by `data_downloader.R`

These come pre-committed under `data/prepackaged/` and are Louisville-specific:
- `Market_Areas/` — comprehensive plan market area shapefile
- `affordable/` — affordable housing unit database (geocoded)
- `permits/` — building permit records
- `renthub/` — quarterly rent observations (Renthub)
- `hud/` — HUD AFFH data (zipped, unzipped inside `data_prep.R`)

## Shiny app structure (`DHNA/app.R`)

Single-file ~1080-line app using `bslib::page_navbar`. Top-level nav panels: **Introduction**, **Housing Need**, **Location**. The Location panel contains nested panels (Affordability levels, Area overview) that drive the displacement risk lookup by clicked map location. All inputs are CSVs/SHPs in `DHNA/data/` produced by `data_prep.R`.
