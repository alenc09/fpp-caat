# Thu Feb  9 14:48:33 2023 ------------------------------
#scritpt para modelos lineares

#Libraries----
library(sf)
library(dplyr)

#Data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_analysis.gpkg", stringsAsFactors = T) -> tab_mun_analysis

##organisation----
tab_mun_analysis %>% 
  mutate(mean_forest_perc_change = if_else(condition = mean_forest_perc_change > 100, 
                                           true = 100,
                                           false = mean_forest_perc_change)) %>% 
  glimpse -> tab_mun_analysis

hist(tab_mun_analysis$mean_fpp_perc_change)
hist(tab_mun_analysis$mean_forest_perc_change)

#Analysis----
##forest and fpp---
glm(data = tab_mun_analysis,
    mean_forest_perc_change ~ mean_fpp_perc_change) -> fpp_forest_mod

plot(fpp_forest_mod)
summary(fpp_forest_mod)

###testing spatial autocorrelation
# poly2nb(tab_mun_analysis$geom, 
#         queen=T) -> mat_dist_mun_caat
# nb2listw(mat_dist_mun_caat) -> mat_dist_list_mun_caat
# 
# lm.morantest(fpp_forest_mod, listw = mat_dist_list_mun_caat)
