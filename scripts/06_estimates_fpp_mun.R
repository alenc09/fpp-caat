# Mon Sep  1 17:32:14 2025 ------------------------------
#script to estimate FPP and change at municipality scale

#libraries----
library(sf)

#data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_analysis.gpkg") -> tab_mun_analysis

#analysis----
tab_mun_analysis %>% 
mutate(cat_change = as.factor(cat_change),
       tendencia_fpp = case_when(
         mean_fpp_perc_change > 0 ~ "crescendo",
         mean_fpp_perc_change < 0 ~ "diminuindo",
         TRUE ~ "estável"
       )) %>% 
  count(tendencia_fpp) %>% 
  mutate(perc = 100 * n / sum(n))

tab_mun_analysis %>% 
  mutate(cat_change = as.factor(cat_change),
         tendencia_forest = case_when(
           mean_forest_perc_change > 0 ~ "crescendo",
           mean_forest_perc_change < 0 ~ "diminuindo",
           TRUE ~ "estável"
         )) %>% 
  count(tendencia_forest) %>% 
  mutate(perc = 100 * n / sum(n))
