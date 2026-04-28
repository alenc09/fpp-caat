# Forest People-Population Dynamics in the Caatinga

Data and scripts for the analysis of forest cover change and rural population dynamics in Brazil's Caatinga biome (2010–2022), examining relationships between deforestation, forest people-population (FPP) settlements, and socioeconomic development.

## Repository structure

```
├── scripts/    # R scripts numbered in execution order
├── data/       # Analysis-ready tabular data
└── img/        # Output figures
```

## Data

| File | Description |
|------|-------------|
| `tabela_geral.csv` | Main analysis table (buffer scale, old pipeline) |
| `tabela_bruta.xlsx` | Raw compiled data |
| `tabela_buffer_nova.xlsx` | Updated buffer-scale analysis table |
| `tabela_mun_nova.xlsx` | Municipality-scale analysis table |
| `tabela_mun_50.xlsx` / `tabela_mun_70.xlsx` | Municipality tables for threshold sensitivity analysis (50% / 70%) |
| `tab_mun_old.xlsx` | Municipality table from older data version (used in validation) |
| `table_analysis.xlsx` / `table_analysis4.csv` | Final formatted tables for statistical analysis |

**External data sources not included in this repository:**
- Land cover: [MapBiomas Collection 5](https://mapbiomas.org) — Annual land use and land cover maps for Brazil
- Population density: [WorldPop](https://www.worldpop.org) — Brazil 2020 constrained population raster
- Census data: [IBGE](https://www.ibge.gov.br) — Brazilian Institute of Geography and Statistics (Census 2010 and 2022)
- Cadunico: Ministry of Social Development — CadÚnico (Single Registry) data
- Forest tree cover reference: [Bastin et al. 2017](https://doi.org/10.1126/science.aam6527) — Supplementary Database S1

## Scripts

Scripts are numbered in recommended execution order. Most scripts require spatial data (`.gpkg`, `.shp`) stored locally and not included in this repository due to file size.

| Script | Description |
|--------|-------------|
| `01_tab_geral.R` | Organises raw data into the main analysis table (old pipeline, 2010–2017) |
| `02_tab_geral_nova.R` | Organises updated data into analysis tables (2010–2022) |
| `03_tab_analysis.R` | Prepares final analysis-ready tables from spatial data |
| `04_estimates_pop_pov_caat.R` | Population and poverty estimates for the Caatinga |
| `05_estimates_fpp_buffer.R` | FPP and forest cover estimates at the landscape (buffer) scale |
| `06_estimates_fpp_mun.R` | FPP and forest cover estimates at the municipality scale |
| `07_estimates_fpp_state.R` | FPP and forest cover estimates at the state scale |
| `08_buffer_lc.R` | Land cover classification within landscape buffers |
| `09_buffer_pop.R` | Population counts within landscape buffers |
| `10_fpp_nvc.R` | Relationship between forest cover change and FPP dynamics |
| `11_context_catChange.R` | Socioeconomic context by forest cover change category |
| `12_context_catChange_thresh.R` | Sensitivity analysis across forest cover thresholds |
| `13_glm.R` | Generalised linear models |
| `14_LISA.R` | Local indicators of spatial autocorrelation (LISA) |
| `15_net_forest.R` | Net forest cover change calculations |
| `16_boxplot_diff_mean.R` | Development indicator comparisons across change categories |
| `17_map_fpp.R` | Maps of FPP distribution and change |
| `18_map_catChange.R` | Maps of forest cover change categories |
| `19_corr_old-new_data.R` | Validation: correlation between old and updated datasets |
| `20_miscelaneous.R` | Miscellaneous calculations |
| `21_press_release_stats.R` | Summary statistics for science communication |
| `22_press_release_development.R` | Development indicator summaries for science communication |

## Requirements

All analyses were conducted in R. Key packages:

```r
install.packages(c(
  "sf", "spdep", "spatialreg", "geobr",
  "dplyr", "tidyr", "readxl", "sidrar",
  "ggplot2", "patchwork", "cowplot", "ragg",
  "nnet", "officer", "flextable", "here"
))
```

Open `dryfor-HWB.Rproj` in RStudio before running any script to set the working directory correctly.

## License

This repository is licensed under [CC BY 4.0](LICENSE.md).
