# Mon Nov 10 15:48:21 2025 ------------------------------
#Script para testar diferentes limiares de cobertura florestal e seu efeito nos modelos

#libraries----
library(sf)
library(dplyr)
library(car)
library(spdep)
library(spatialreg)
library(purrr)
library(tibble)
library(officer)
library(flextable)

#data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_analysis_70.gpkg") -> tab_mun_70

##organization
tab_mun_70 %>% 
  mutate(change_agrifam_cadunico = perc_agrifam_cadunico_2022 - perc_agrifam_cadunico_2012,
         change_popUrb = perc_popUrb_2022 - perc_popUrb_2010,
         change_ifdm_saude = ifdm_saude_2022 - ifdm_saude_2013,
         change_pessoas_cadunico = perc_pessoas_cadunico_2022 - perc_pessoas_cadunico_2012,
         change_taxa_u5mort = taxa_u5mort_2022 - taxa_u5mort_2010,
         change_cisternas = perc_cisternas_2022 - perc_cisternas_2010,
         change_irrigation = irrigacao_hectare_2022 - irrigacao_hectare_2010,
         change_saneamento = perc_saneamento_2022 - perc_saneamento_2010,
         change_public_light = perc_public_light_2022 - perc_public_light_2010,
         change_mean_respRenda = mean_respRenda_2022 - mean_respRenda_2010,
         change_bovino_hectare = bovino_hectare_2022 - bovino_hectare_2010,
         change_caprino_hectare = caprino_hectare_2022 - caprino_hectare_2010,
         change_pib_agro = perc_pib_agro_2021 - perc_pib_agro_2010,
         change_mean_perc_forest = mean_perc_forest_2023 - mean_perc_forest_2022,
         .keep = "unused") %>% 
  # filter(cat_change != "stable") %>% 
  glimpse -> tab_mun_models_70

poly2nb(tab_mun_70$geom, 
        queen=TRUE) -> mat_dist_mun_caat_70

nb2listw(mat_dist_mun_caat_70,
         zero.policy = T) -> mat_dist_list_mun_caat_70

##spatial----
##forest
mean_forest_perc_change ~ change_popUrb + change_ifdm_saude +
  change_mean_respRenda + change_taxa_u5mort + change_cisternas +
  # change_irrigation excluded: insufficient data at 70% threshold
  change_public_light +
  change_bovino_hectare + change_caprino_hectare + change_pib_agro -> form_forest

errorsarlm(formula = form_forest,
           data    = tab_mun_models_70,
           listw   = mat_dist_list_mun_caat_70,
           Durbin  = TRUE,       
           zero.policy = TRUE) -> mod_spatial_forest_70

impacts(mod_spatial_forest_70, listw = mat_dist_list_mun_caat_70, R = 1000) -> imp_mod_spatial_forest_70
summary(imp_mod_spatial_forest_70, short = T) -> summary_imp_forest_70


df_impacts_70 <- tibble(
  variable = names(summary_imp_forest_70$impacts$direct),
  direct   = sprintf("%.4f (%.4f), p=%.4f",
                     summary_imp_forest_70$impacts$direct,
                     summary_imp_forest_70$se$direct,
                     summary_imp_forest_70$pzmat[,1]),
  indirect = sprintf("%.4f (%.4f), p=%.4f",
                     summary_imp_forest_70$impacts$indirect,
                     summary_imp_forest_70$se$indirect,
                     summary_imp_forest_70$pzmat[,2]),
  total    = sprintf("%.4f (%.4f), p=%.4f",
                     summary_imp_forest_70$impacts$total,
                     summary_imp_forest_70$se$total,
                     summary_imp_forest_70$pzmat[,3])
)
# Transformar em flextable para exportar pro Word
ft <- flextable(df_impacts_70)

# Exportar para .docx editável
doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "impacts_results_70.docx")


##fpp----
mean_fpp_perc_change ~ change_popUrb + change_ifdm_saude +
  change_mean_respRenda + change_taxa_u5mort + change_cisternas +
  # change_irrigation excluded: insufficient data at 70% threshold
  change_public_light +
  change_bovino_hectare + change_caprino_hectare + change_pib_agro -> form_fpp

errorsarlm(formula = form_fpp,
           data    = tab_mun_models_70,
           listw   = mat_dist_list_mun_caat_70,
           Durbin  = TRUE,
           zero.policy = TRUE) -> mod_spatial_fpp_70

impacts(mod_spatial_fpp_70, listw = mat_dist_list_mun_caat_70, R = 1000) -> imp_mod_spatial_fpp_70
summary(imp_mod_spatial_fpp_70) -> summary_imp_fpp_70

df_impacts <- tibble(
  variable = names(summary_imp_fpp_70$impacts$direct),
  direct   = sprintf("%.4f (%.4f), p=%.4f",
                     summary_imp_fpp_70$impacts$direct,
                     summary_imp_fpp_70$se$direct,
                     summary_imp_fpp_70$pzmat[,1]),
  indirect = sprintf("%.4f (%.4f), p=%.4f",
                     summary_imp_fpp_70$impacts$indirect,
                     summary_imp_fpp_70$se$indirect,
                     summary_imp_fpp_70$pzmat[,2]),
  total    = sprintf("%.4f (%.4f), p=%.4f",
                     summary_imp_fpp_70$impacts$total,
                     summary_imp_fpp_70$se$total,
                     summary_imp_fpp_70$pzmat[,3])
)
# Transformar em flextable para exportar pro Word
ft <- flextable(df_impacts)

# Exportar para .docx editável
doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "impacts_results_fpp_70.docx")
