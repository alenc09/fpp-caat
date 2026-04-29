# Supplementary Table 7: Model diagnostics — Spatial Durbin Error Model vs OLS
# for forest cover change and FPP change models

# Libraries ----
library(here)
library(readr)
library(dplyr)
library(sf)
library(geobr)
library(spdep)
library(spatialreg)
library(tibble)
library(scales)
library(officer)
library(flextable)

# Data ----
# Uses municipalities with mean forest cover >= 50% (tab_mun_analysis_50)
read_csv(here("data/tab_mun_analysis_50.csv"), show_col_types = FALSE) %>%
  mutate(
    change_popUrb          = perc_popUrb_2022          - perc_popUrb_2010,
    change_ifdm_saude      = ifdm_saude_2022           - ifdm_saude_2013,
    change_mean_respRenda  = mean_respRenda_2022        - mean_respRenda_2010,
    change_taxa_u5mort     = taxa_u5mort_2022           - taxa_u5mort_2010,
    change_cisternas       = perc_cisternas_2022        - perc_cisternas_2010,
    change_irrigation      = irrigacao_hectare_2022     - irrigacao_hectare_2010,
    change_public_light    = perc_public_light_2022     - perc_public_light_2010,
    change_bovino_hectare  = bovino_hectare_2022        - bovino_hectare_2010,
    change_caprino_hectare = caprino_hectare_2022       - caprino_hectare_2010,
    change_pib_agro        = perc_pib_agro_2021         - perc_pib_agro_2010
  ) -> tab_mun_models

# Spatial weights from geobr ----
read_municipality(year = 2020, showProgress = FALSE) -> br_mun
read_biomes(showProgress = FALSE) %>%
  filter(name_biome == "Caatinga") %>%
  st_transform(4674) -> caat_biome

br_mun[caat_biome, ] %>%
  filter(code_muni %in% tab_mun_models$code_mun) %>%
  arrange(code_muni) -> mun_caat_sf

tab_mun_models <- tab_mun_models %>%
  filter(code_mun %in% mun_caat_sf$code_muni) %>%
  arrange(code_mun)

poly2nb(mun_caat_sf$geom, queen = TRUE) %>%
  nb2listw(zero.policy = TRUE) -> listw_mun

# Model formulas ----
form_forest <-
  mean_forest_perc_change ~ change_popUrb + change_ifdm_saude +
  change_mean_respRenda + change_taxa_u5mort + change_cisternas +
  change_irrigation + change_public_light +
  change_bovino_hectare + change_caprino_hectare + change_pib_agro

form_fpp <-
  mean_fpp_perc_change ~ change_popUrb + change_ifdm_saude +
  change_mean_respRenda + change_taxa_u5mort + change_cisternas +
  change_irrigation + change_public_light +
  change_bovino_hectare + change_caprino_hectare + change_pib_agro

# Fit models ----
errorsarlm(form_forest, data = tab_mun_models, listw = listw_mun,
           Durbin = TRUE, zero.policy = TRUE) -> mod_spatial_forest

lm(form_forest, data = tab_mun_models) -> mod_ols_forest

errorsarlm(form_fpp, data = tab_mun_models, listw = listw_mun,
           Durbin = TRUE, zero.policy = TRUE) -> mod_spatial_fpp

lm(form_fpp, data = tab_mun_models) -> mod_ols_fpp

# Helper: McFadden-like pseudo-R2 (1 - SSR/SST) ----
pseudo_r2 <- function(model) {
  y <- if (inherits(model, "Sarlm")) model$y else model$model[[1]]
  1 - sum(residuals(model)^2) / sum((y - mean(y))^2)
}

# Build diagnostics table ----
tribble(
  ~Model,                                ~mod_obj,
  "Forest cover change - Spatial model", mod_spatial_forest,
  "Forest cover change - OLS",           mod_ols_forest,
  "FPP change - Spatial model",          mod_spatial_fpp,
  "FPP change - OLS",                    mod_ols_fpp
) %>%
  mutate(
    `Log-likelihood` = sapply(mod_obj, \(m) round(as.numeric(logLik(m)), 3)),
    AIC              = sapply(mod_obj, \(m) round(AIC(m), 3)),
    BIC              = sapply(mod_obj, \(m) round(BIC(m), 3)),
    `Pseudo R2`      = sapply(mod_obj, \(m) round(pseudo_r2(m), 4))
  ) %>%
  select(-mod_obj) -> tab_st7

# Export to Word ----
ft <- flextable(tab_st7) %>%
  set_table_properties(layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all")

doc <- read_docx() %>%
  body_add_par(
    paste0("Supplementary Table 7. Model diagnostics and comparison between the Spatial Durbin ",
           "Error models (spatial models) and Ordinary Least Square (OLS) for two independent ",
           "models (forest cover change and Forest Proximate Population (FPP) change). ",
           "AIC - Akaike Information Criterion, BIC - Bayesian Information Criterion. ",
           "The Pseudo R2 was calculated using McFadden-like Pseudo-R\u00b2."),
    style = "Normal"
  ) %>%
  body_add_flextable(ft)

print(doc, target = here("supp_table7_model_diagnostics.docx"))
