# Supplementary Table 2
# Spatial autocorrelation test (Global Moran's I) for GLM residuals
# of forest cover change and FPP change models

# Libraries ----
library(here)
library(readr)
library(dplyr)
library(sf)
library(geobr)
library(spdep)
library(tibble)
library(officer)
library(flextable)

# Data ----
read_csv(here("data/tab_mun_analysis.csv"), show_col_types = FALSE) -> tab_mun_analysis

# Municipality geometries from geobr (for spatial weights matrix) ----
read_municipality(year = 2020, showProgress = FALSE) -> br_mun
read_biomes(showProgress = FALSE) %>%
  filter(name_biome == "Caatinga") %>%
  st_transform(4674) -> caat_biome

br_mun[caat_biome, ] %>%
  filter(code_muni %in% tab_mun_analysis$code_mun) %>%
  arrange(code_muni) -> mun_caat_sf

tab_mun_analysis <- tab_mun_analysis %>%
  filter(code_mun %in% mun_caat_sf$code_muni) %>%
  arrange(code_mun)

# Spatial weights ----
poly2nb(mun_caat_sf$geom, queen = TRUE) -> nb_mun
nb2listw(nb_mun) -> listw_mun

# Compute change variables for models ----
tab_mun_analysis %>%
  mutate(change_agrifam_cadunico  = perc_agrifam_cadunico_2022 - perc_agrifam_cadunico_2012,
         change_popUrb             = perc_popUrb_2022 - perc_popUrb_2010,
         change_ifdm_saude         = ifdm_saude_2022 - ifdm_saude_2013,
         change_taxa_u5mort        = taxa_u5mort_2022 - taxa_u5mort_2010,
         change_cisternas          = perc_cisternas_2022 - perc_cisternas_2010,
         change_irrigation         = irrigacao_hectare_2022 - irrigacao_hectare_2010,
         change_saneamento         = perc_saneamento_2022 - perc_saneamento_2010,
         change_public_light       = perc_public_light_2022 - perc_public_light_2010,
         change_mean_respRenda     = mean_respRenda_2022 - mean_respRenda_2010,
         change_bovino_hectare     = bovino_hectare_2022 - bovino_hectare_2010,
         change_caprino_hectare    = caprino_hectare_2022 - caprino_hectare_2010,
         change_pib_agro           = perc_pib_agro_2021 - perc_pib_agro_2010,
         .keep = "unused") -> tab_mun_models

# GLMs ----
glm(data = tab_mun_models,
    mean_forest_perc_change ~ change_popUrb + change_ifdm_saude +
      change_mean_respRenda + change_taxa_u5mort + change_cisternas +
      change_irrigation + change_saneamento + change_public_light +
      change_bovino_hectare + change_caprino_hectare + change_pib_agro) -> mod_glm_forest

glm(data = tab_mun_models,
    mean_fpp_perc_change ~ change_popUrb + change_ifdm_saude +
      change_mean_respRenda + change_taxa_u5mort + change_cisternas +
      change_irrigation + change_saneamento + change_public_light +
      change_bovino_hectare + change_caprino_hectare + change_pib_agro) -> mod_glm_fpp

# Moran's I test ----
lm.morantest(model = mod_glm_forest, listw = listw_mun) -> moran_forest
lm.morantest(model = mod_glm_fpp,    listw = listw_mun) -> moran_fpp

# Export ----
tibble(
  Model       = c("Forest cover", "Forest-proximate people"),
  `Moran's I` = c(moran_forest$estimate["Moran I statistic"],
                   moran_fpp$estimate["Moran I statistic"]),
  `p-value`   = c(moran_forest$p.value, moran_fpp$p.value)
) %>%
  mutate(across(where(is.numeric), ~ round(., 4))) -> tab_moran

ft <- flextable(tab_moran) %>%
  set_table_properties(layout = "autofit") %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all")

read_docx() %>%
  body_add_par(
    "Supplementary Table 2. Spatial autocorrelation test (Global Moran's I for regression residuals) of GLMs models of forest cover change and forest-proximate people change.",
    style = "Normal"
  ) %>%
  body_add_flextable(ft) %>%
  print(target = here("supp_table2_moran.docx"))
