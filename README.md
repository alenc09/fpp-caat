# Forest People-Population Dynamics in the Caatinga

Data and scripts for the analysis of forest cover change and rural population dynamics in Brazil's Caatinga biome (2010–2022), examining relationships between deforestation, forest people-population (FPP) settlements, and socioeconomic development.

## Repository structure

```
├── scripts/    # R scripts numbered in execution order
├── data/       # Analysis-ready tabular data (see data/README.md)
└── img/        # Output figures
```

---

## Reproducibility

### What can be reproduced from this repository

All figures, spatial models, and supplementary tables in the manuscript can be reproduced directly from the tabular data in `data/`. The scripts below require only the files already in this repository plus an internet connection (for boundary data downloaded via the `geobr` package):

| Script | Output |
|--------|--------|
| `DO_02_tab_analysis.R` | Derives `tab_mun_analysis.csv` and `tab_buffer_analysis.csv` from the xlsx tables |
| `DA_03_fpp_estimates.R` | FPP descriptive estimates at biome, municipality, and state scales |
| `DA_04_fpp_nvc_summary.R` | Summary statistics by forest–people change category (GG/GP/PG/PP) |
| `FIG_01_fpp_thresholds.R` | Figure 1 — FPP estimates across forest cover thresholds |
| `FIG_02_map_fpp.R` | Figure 2 — Map of FPP change by municipality |
| `FIG_03_fpp_nvc.R` | Figure 3 — Scatter plot and map of FPP vs forest cover change |
| `SA_01_spatial_models.R` | Spatial Durbin error models and GLMs (main threshold: 20%) |
| `SA_02_spatial_models_thresh70.R` | Spatial models — sensitivity analysis at 70% threshold |
| `SF_02_fpp_state_thresholds.R` | Supplementary Figure 2 — FPP per state across thresholds |
| `ST_02_moran_test.R` | Supplementary Table — Moran's I test for spatial autocorrelation |
| `ST_03_fpp_threshold_estimates.R` | Supplementary Table — FPP estimates at varying thresholds |
| `ST_04_fpp_state_estimates.R` | Supplementary Table — State-level FPP estimates |
| `ST_05_landscape_transitions.R` | Supplementary Table — Landscape-level forest and FPP transitions |
| `ST_06_model_forest2023.R` | Supplementary Table — Spatial Durbin model for 2022–2023 forest change |
| `ST_07_model_diagnostics.R` | Supplementary Table — Model diagnostics (spatial Durbin vs OLS) |

### What requires external data

The following scripts build the analytical tables from raw data. They require large spatial files (MapBiomas rasters, census tract geometries) that are not included in this repository due to file size. The raw data are all publicly available — see `data/README.md` for sources.

| Script | What it needs |
|--------|---------------|
| `PP_01_buffer_lc.R` | MapBiomas land cover rasters (2010–2023) |
| `PP_02_buffer_pop.R` | IBGE census tract geometries and population microdata |
| `DO_01_tab_geral.R` | All of the above plus socioeconomic datasets (IFDM, CadÚnico, ANA, DATASUS, etc.) |
| `DA_01_estimates_pop_pov.R` | Census tract population shapefile |
| `DA_02_net_forest.R` | MapBiomas rasters and buffer/biome shapefiles |

> The processed analytical tables (outputs of this pipeline) are provided in `data/` so that the full analysis can be reproduced without re-running these scripts. The underlying raw datasets can be requested from the authors or downloaded directly from the sources listed in `data/README.md`.

---

## Setup

1. Clone this repository and open `dryfor-HWB.Rproj` in RStudio.
2. Restore the package environment:
   ```r
   renv::restore()
   ```
3. Run scripts in the order listed above, starting from `DO_02_tab_analysis.R`.

Package versions are locked in `renv.lock` (R 4.4.2).

---

## Scripts — complete list

### Pre-processing (require external data)

| Script | Description |
|--------|-------------|
| `PP_01_buffer_lc.R` | Extracts forest cover from MapBiomas rasters within 5 km landscape buffers |
| `PP_02_buffer_pop.R` | Intersects census tract population with landscape buffers to compute FPP |
| `DO_01_tab_geral.R` | Integrates all raw datasets into the main analysis tables |

### Data preparation (reproducible from `data/`)

| Script | Description |
|--------|-------------|
| `DO_02_tab_analysis.R` | Derives analysis-ready CSVs with change variables and GG/GP/PG/PP categories |

### Descriptive analysis (reproducible from `data/`)

| Script | Description |
|--------|-------------|
| `DA_01_estimates_pop_pov.R` | Population and poverty estimates for the Caatinga (requires external data) |
| `DA_02_net_forest.R` | Net forest cover change in ha (requires external rasters) |
| `DA_03_fpp_estimates.R` | FPP estimates at biome, municipality, and state scales |
| `DA_04_fpp_nvc_summary.R` | Summary statistics of FPP and forest change by category |

### Figures (reproducible from `data/`)

| Script | Description |
|--------|-------------|
| `FIG_01_fpp_thresholds.R` | Figure 1 — FPP estimates at varying forest cover thresholds |
| `FIG_02_map_fpp.R` | Figure 2 — Map of FPP change by municipality |
| `FIG_03_fpp_nvc.R` | Figure 3 — FPP vs forest cover change scatter plot and map |

### Spatial models (reproducible from `data/`)

| Script | Description |
|--------|-------------|
| `SA_01_spatial_models.R` | Spatial Durbin error models and GLMs — main analysis (20% threshold) |
| `SA_02_spatial_models_thresh70.R` | Spatial models — robustness check at 70% threshold |

### Supplementary tables and figures (reproducible from `data/`)

| Script | Description |
|--------|-------------|
| `SF_02_fpp_state_thresholds.R` | Supplementary Figure 2 — State-level FPP across thresholds |
| `ST_02_moran_test.R` | Moran's I test for spatial autocorrelation |
| `ST_03_fpp_threshold_estimates.R` | FPP estimates at varying forest cover thresholds |
| `ST_04_fpp_state_estimates.R` | State-level FPP estimates |
| `ST_05_landscape_transitions.R` | Forest and FPP transitions by change category |
| `ST_06_model_forest2023.R` | Spatial Durbin model for short-term forest change (2022–2023) |
| `ST_07_model_diagnostics.R` | Model diagnostics table |

---

## Data

See [`data/README.md`](data/README.md) for a full description of each file, the variables it contains, and the original data sources.

---

## License

This repository is licensed under [CC BY 4.0](LICENSE.md).
