# Thu Sep  4 14:05:11 2025 ------------------------------
# Script to evaluate changes in socioeconomic conditions per group

#Library----
library(sf)
library(dplyr)
library(nnet)
library(spdep)
library(spatialreg)
library(ggplot2)

#data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_analysis.gpkg", stringsAsFactors = T) -> tab_mun_analysis

##organisation----
tab_mun_analysis %>% 
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
  glimpse -> tab_mun_models

poly2nb(tab_mun_analysis$geom, 
        queen=TRUE) -> mat_dist_mun_caat

nb2listw(mat_dist_mun_caat) -> mat_dist_list_mun_caat

#Analysis----
##multinomial----
# multinom(data = tab_mun_models,
#          cat_change ~ change_agrifam_cadunico + change_popUrb + change_ifdm_saude +
#            change_pessoas_cadunico + change_taxa_u5mort + change_cisternas +
#            change_irrigation + change_saneamento + change_public_light) -> mod_multinom
# 
# summary(mod_multinom)
# exp(coef(mod_multinom))

##linear----
###forest
glm(data = tab_mun_models,
    mean_forest_perc_change ~ change_agrifam_cadunico + change_popUrb + change_ifdm_saude +
      change_mean_respRenda + change_taxa_u5mort + change_cisternas +
      change_irrigation + change_saneamento + change_public_light +
      change_bovino_hectare + change_caprino_hectare + change_pib_agro) -> mod_glm_forest

car:::vif(mod_glm_forest)
# plot(mod_glm_forest)
summary(mod_glm_forest)
lm.morantest(model = mod_glm_forest, listw = mat_dist_list_mun_caat)

ggplot2::ggplot(data = tab_mun_models) +
  geom_point(aes(x = change_cisternas, y = mean_forest_perc_change)) +
  geom_smooth(aes(x = change_cisternas, y = mean_forest_perc_change), stat = "smooth")


###fpp
glm(data = tab_mun_models,
    mean_fpp_perc_change ~ change_agrifam_cadunico + change_popUrb + change_ifdm_saude +
      change_mean_respRenda + change_taxa_u5mort + change_cisternas +
      change_irrigation + change_saneamento + change_public_light +
      change_bovino_hectare + change_caprino_hectare + change_pib_agro) -> mod_glm_fpp

car:::vif(mod_glm_fpp)
# plot(mod_glm_fpp)
summary(mod_glm_fpp)
lm.morantest(model = mod_glm_fpp, listw = mat_dist_list_mun_caat)

##spatial----
##forest
mean_forest_perc_change ~ change_agrifam_cadunico + change_popUrb + change_ifdm_saude +
  change_mean_respRenda + change_taxa_u5mort + change_cisternas +
  change_irrigation + change_public_light +
  change_bovino_hectare + change_caprino_hectare + change_pib_agro -> form_forest

errorsarlm(formula = form_forest,
           data    = tab_mun_models,
           listw   = mat_dist_list_mun_caat,
           Durbin  = TRUE,       
           zero.policy = TRUE) -> mod_spatial_forest

impacts(mod_spatial_forest, listw = mat_dist_list_mun_caat, R = 1000) -> imp_mod_spatial_forest 
summary(imp_mod_spatial_forest, short = T)

##fpp
mean_fpp_perc_change ~ change_agrifam_cadunico + change_popUrb + change_ifdm_saude +
  change_mean_respRenda + change_taxa_u5mort + change_cisternas +
  change_irrigation + change_public_light +
  change_bovino_hectare + change_caprino_hectare + change_pib_agro -> form_fpp

errorsarlm(formula = form_fpp,
           data    = tab_mun_models,
           listw   = mat_dist_list_mun_caat,
           Durbin  = TRUE,       # isso ativa o "Durbin" (variáveis explicativas defasadas)
           zero.policy = TRUE) -> mod_spatial_fpp

impacts(mod_spatial_fpp, listw = mat_dist_list_mun_caat, R = 1000) -> imp_mod_spatial_fpp
summary(imp_mod_spatial_fpp)


##valor bruto----
# glm(data = tab_mun_analysis,
#     mean_perc_forest_2022 ~  perc_agrifam_cadunico_2012 +
#       perc_popUrb_2010 + ifdm_saude_2013 + mean_respRenda_2010 + taxa_u5mort_2010 +
#       perc_cisternas_2010 + irrigacao_hectare_2010 + perc_saneamento_2010 + 
#       perc_public_light_2010 + bovino_hectare_2010 + caprino_hectare_2010 +
#       perc_pib_agro_2010) %>% 
#   summary
# 
# glm(data = tab_mun_analysis,
#     mean_perc_forest_2023 ~ mean_perc_forest_2022 + perc_agrifam_cadunico_2022 +
#       perc_popUrb_2022 + ifdm_saude_2022 + mean_respRenda_2022 + taxa_u5mort_2022 +
#       perc_cisternas_2022 + irrigacao_hectare_2022 + perc_saneamento_2022 +
#       perc_public_light_2022 + bovino_hectare_2022 + caprino_hectare_2022 +
#       perc_pib_agro_2021) -> mod_glm_abs_forest
# 
# car:::vif(mod_glm_abs_forest)
# plot(mod_glm_abs_forest)
# summary(mod_glm_abs_forest)
# lm.morantest(model = mod_glm_abs_forest, listw = mat_dist_list_mun_caat)
# 
# # 
# 
# glm(data = tab_mun_analysis,
#       sum_fpp_2022 ~  sum_fpp_2010 + perc_agrifam_cadunico_2022 +
#       perc_popUrb_2022 + ifdm_saude_2022 + mean_respRenda_2022 + taxa_u5mort_2022 +
#       perc_cisternas_2022 + irrigacao_hectare_2022 + perc_saneamento_2022 +
#       perc_public_light_2022 + bovino_hectare_2022 + caprino_hectare_2022 +
#       perc_pib_agro_2021) -> mod_glm_abs_fpp
# 
# car:::vif(mod_glm_abs_fpp)
# plot(mod_glm_abs_fpp)
# summary(mod_glm_abs_fpp)
# lm.morantest(model = mod_glm_abs_fpp, listw = mat_dist_list_mun_caat)
# 
# ##spatial
# ###forest
# mean_perc_forest_2022 ~ mean_perc_forest_2010 + perc_agrifam_cadunico_2022 +
#   perc_popUrb_2022 + ifdm_saude_2022 + mean_respRenda_2022 + taxa_u5mort_2022 +
#   perc_cisternas_2022 + irrigacao_hectare_2022 + perc_saneamento_2022 +
#   perc_public_light_2022 + bovino_hectare_2022 + caprino_hectare_2022 +
#   perc_pib_agro_2021 -> form_abs_forest
# 
# errorsarlm(formula = form_abs_forest,
#            data    = tab_mun_analysis,
#            listw   = mat_dist_list_mun_caat,
#            Durbin  = TRUE,       # isso ativa o "Durbin" (variáveis explicativas defasadas)
#            zero.policy = TRUE) -> mod_spatial_abs_forest
# 
# summary(mod_spatial_abs_forest)

##forest robustness check
change_mean_perc_forest ~ change_agrifam_cadunico + change_popUrb + change_ifdm_saude +
  change_mean_respRenda + change_taxa_u5mort + change_cisternas +
  change_irrigation + change_public_light +
  change_bovino_hectare + change_caprino_hectare + change_pib_agro -> form_forest_23

errorsarlm(formula = form_forest_23,
           data    = tab_mun_models,
           listw   = mat_dist_list_mun_caat,
           Durbin  = TRUE,       
           zero.policy = TRUE) -> mod_spatial_forest_23

impacts(mod_spatial_forest_23, listw = mat_dist_list_mun_caat, R = 1000) -> imp_mod_spatial_forest_23 
summary(imp_mod_spatial_forest_23, short = T)
