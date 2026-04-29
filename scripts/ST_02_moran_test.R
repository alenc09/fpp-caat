# Supplementary Table 2
# Spatial autocorrelation test (Global Moran's I) for GLM residuals
# of forest cover change and FPP change models

#Libraries----
library(sf)
library(dplyr)
library(spdep)
library(tibble)
library(officer)
library(flextable)

#Data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_analysis.gpkg", stringsAsFactors = T) -> tab_mun_analysis

#Organisation----
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

poly2nb(tab_mun_analysis$geom, queen = TRUE) -> mat_dist_mun_caat
nb2listw(mat_dist_mun_caat) -> mat_dist_list_mun_caat

#Models----
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

#Moran's I test----
lm.morantest(model = mod_glm_forest, listw = mat_dist_list_mun_caat) -> moran_forest
lm.morantest(model = mod_glm_fpp,    listw = mat_dist_list_mun_caat) -> moran_fpp

#Export----
tibble(
  Model      = c("Forest cover", "Forest-proximate people"),
  `Moran's I` = c(moran_forest$estimate["Moran I statistic"],
                   moran_fpp$estimate["Moran I statistic"]),
  `p-value`  = c(moran_forest$p.value, moran_fpp$p.value)
) %>%
  mutate(across(where(is.numeric), ~ round(., 4))) -> tab_moran

ft <- flextable(tab_moran) %>% autofit()

read_docx() %>%
  body_add_par("Supplementary Table 2. Spatial autocorrelation test (Global Moran's I for regression residuals) of GLMs models of forest cover change and forest-proximate people change.", style = "Normal") %>%
  body_add_flextable(ft) %>%
  print(target = "supp_table2_moran.docx")
