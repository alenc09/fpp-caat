# Reproducible analysis pipeline
#
# Run this script to reproduce all figures, spatial models, and supplementary
# tables from the analytical tables in data/.
#
# Requirements:
#   - Open dryfor-HWB.Rproj in RStudio (sets working directory via here())
#   - Run renv::restore() once to install all packages at the correct versions
#   - Internet connection (geobr downloads boundary data on first use)
#
# NOTE: Scripts marked [EXTERNAL DATA] below cannot be run from this repository
# alone — they require raw spatial and socioeconomic datasets not included here.
# See data/README.md for sources. All other scripts run from data/ only.

library(here)

# ---------------------------------------------------------------------------
# [EXTERNAL DATA] Pre-processing — build analytical tables from raw data
# Skip these if you are starting from the tables already in data/
# ---------------------------------------------------------------------------
# source(here("scripts/PP_01_buffer_lc.R"))      # forest cover per buffer (MapBiomas rasters)
# source(here("scripts/PP_02_buffer_pop.R"))      # FPP per buffer (census tract geometries)
# source(here("scripts/DO_01_tab_geral.R"))       # integrates all raw datasets into xlsx tables

# ---------------------------------------------------------------------------
# Step 1 — Derive analysis-ready CSVs from xlsx tables
# ---------------------------------------------------------------------------
source(here("scripts/DO_02_tab_analysis.R"))

# ---------------------------------------------------------------------------
# Step 2 — Descriptive analysis
# ---------------------------------------------------------------------------
source(here("scripts/DA_03_fpp_estimates.R"))
source(here("scripts/DA_04_fpp_nvc_summary.R"))

# ---------------------------------------------------------------------------
# Step 3 — Manuscript figures
# ---------------------------------------------------------------------------
source(here("scripts/FIG_01_fpp_thresholds.R"))
source(here("scripts/FIG_02_map_fpp.R"))
source(here("scripts/FIG_03_fpp_nvc.R"))

# ---------------------------------------------------------------------------
# Step 4 — Spatial models
# ---------------------------------------------------------------------------
source(here("scripts/SA_01_spatial_models.R"))
source(here("scripts/SA_02_spatial_models_thresh70.R"))

# ---------------------------------------------------------------------------
# Step 5 — Supplementary figures and tables
# ---------------------------------------------------------------------------
source(here("scripts/SF_02_fpp_state_thresholds.R"))
source(here("scripts/ST_02_moran_test.R"))
source(here("scripts/ST_03_fpp_threshold_estimates.R"))
source(here("scripts/ST_04_fpp_state_estimates.R"))
source(here("scripts/ST_05_landscape_transitions.R"))
source(here("scripts/ST_06_model_forest2023.R"))
source(here("scripts/ST_07_model_diagnostics.R"))

# ---------------------------------------------------------------------------
# Session info — saved for reproducibility record
# ---------------------------------------------------------------------------
dir.create(here("output"), showWarnings = FALSE)
sink(here("output/sessioninfo.txt"))
print(sessionInfo())
sink()

message("Pipeline complete. Figures in img/, session info in output/sessioninfo.txt")
