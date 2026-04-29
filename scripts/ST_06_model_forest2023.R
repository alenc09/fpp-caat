# Supplementary Table 6: Spatial Durbin Error Model — short-term forest cover
# change (2022-2023). Direct, indirect and total effects with standard errors.

# Libraries ----
library(here)
library(readr)
library(dplyr)
library(sf)
library(geobr)
library(spdep)
library(spatialreg)
library(tibble)
library(officer)
library(flextable)

# Data ----
read_csv(here("data/tab_mun_analysis.csv"), show_col_types = FALSE) %>%
  mutate(
    change_agrifam_cadunico = perc_agrifam_cadunico_2022 - perc_agrifam_cadunico_2012,
    change_popUrb           = perc_popUrb_2022           - perc_popUrb_2010,
    change_ifdm_saude       = ifdm_saude_2022            - ifdm_saude_2013,
    change_mean_respRenda   = mean_respRenda_2022         - mean_respRenda_2010,
    change_taxa_u5mort      = taxa_u5mort_2022            - taxa_u5mort_2010,
    change_cisternas        = perc_cisternas_2022         - perc_cisternas_2010,
    change_irrigation       = irrigacao_hectare_2022      - irrigacao_hectare_2010,
    change_public_light     = perc_public_light_2022      - perc_public_light_2010,
    change_bovino_hectare   = bovino_hectare_2022         - bovino_hectare_2010,
    change_caprino_hectare  = caprino_hectare_2022        - caprino_hectare_2010,
    change_pib_agro         = perc_pib_agro_2021          - perc_pib_agro_2010,
    change_mean_perc_forest = mean_perc_forest_2023       - mean_perc_forest_2022
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
  nb2listw() -> listw_mun

# Spatial Durbin Error Model — forest 2022-2023 ----
form_forest_23 <-
  change_mean_perc_forest ~ change_agrifam_cadunico + change_popUrb +
  change_ifdm_saude + change_mean_respRenda + change_taxa_u5mort +
  change_cisternas + change_irrigation + change_public_light +
  change_bovino_hectare + change_caprino_hectare + change_pib_agro

errorsarlm(formula = form_forest_23,
           data    = tab_mun_models,
           listw   = listw_mun,
           Durbin  = TRUE,
           zero.policy = TRUE) -> mod_forest_23

impacts(mod_forest_23, listw = listw_mun, R = 1000) -> imp_forest_23
summary(imp_forest_23, short = TRUE)                -> sum_imp_forest_23

# Helper: format estimate + SE + significance stars ----
fmt_cell <- function(est, se, pval) {
  stars <- case_when(
    pval < 0.01 ~ "***",
    pval < 0.05 ~ "**",
    pval < 0.10 ~ "*",
    TRUE        ~ ""
  )
  paste0(formatC(est, format = "e", digits = 2),
         "\n(", formatC(se, format = "e", digits = 2), ")",
         stars)
}

# Variable labels ----
var_labels <- c(
  change_agrifam_cadunico = "Family agriculture poverty",
  change_popUrb           = "Urban population",
  change_ifdm_saude       = "IFDM - health sub-index",
  change_mean_respRenda   = "Head of the household income",
  change_taxa_u5mort      = "Under-five mortality",
  change_cisternas        = "Cisterns donation",
  change_irrigation       = "Irrigation",
  change_public_light     = "Public lighting",
  change_bovino_hectare   = "Cattle herd",
  change_caprino_hectare  = "Goat herd",
  change_pib_agro         = "Agrarian GDP"
)

# Build table ----
tibble(
  var      = names(sum_imp_forest_23$impacts$direct),
  Direct   = fmt_cell(sum_imp_forest_23$impacts$direct,
                      sum_imp_forest_23$se$direct,
                      sum_imp_forest_23$pzmat[, 1]),
  Indirect = fmt_cell(sum_imp_forest_23$impacts$indirect,
                      sum_imp_forest_23$se$indirect,
                      sum_imp_forest_23$pzmat[, 2]),
  Total    = fmt_cell(sum_imp_forest_23$impacts$total,
                      sum_imp_forest_23$se$total,
                      sum_imp_forest_23$pzmat[, 3])
) %>%
  mutate(Variable = var_labels[var],
         `Model outcome` = if_else(row_number() == 1,
                                   "Forest cover change (2022-2023)", "")) %>%
  select(`Model outcome`, Variable, Direct, Indirect, Total) -> tab_st6

# Export to Word ----
ft <- flextable(tab_st6) %>%
  set_table_properties(layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  align(j = 1:2, align = "left", part = "body") %>%
  bold(part = "header") %>%
  fontsize(size = 9, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  merge_v(j = "Model outcome") %>%
  valign(j = "Model outcome", valign = "top", part = "body")

doc <- read_docx() %>%
  body_add_par(
    paste0("Supplementary Table 6. Direct, indirect and global effects of several variables on ",
           "short-term forest cover change measured by Spatial Durbin Error Model. ",
           "Numbers in parentheses are standard errors. ",
           "* - p < 0.1; ** - p < 0.05; *** - p < 0.01."),
    style = "Normal"
  ) %>%
  body_add_flextable(ft)

print(doc, target = here("supp_table6_model_forest2023.docx"))
