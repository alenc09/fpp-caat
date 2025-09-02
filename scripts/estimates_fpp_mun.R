# Mon Sep  1 17:32:14 2025 ------------------------------
#script to estimate FPP and change at municipality scale

#libraries----
library(sf)

#data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_nova.gpkg") -> tab_mun

##organization----
tab_mun %>% 
  mutate(mean_fpp_abs_change = mean_fpp_2022 - mean_fpp_2010,
         mean_fpp_perc_change = ((mean_fpp_2022 - mean_fpp_2010)/mean_fpp_2010)*100,
         mean_forest_abs_change = mean_perc_forest_2022 - mean_perc_forest_2010,
         mean_forest_perc_change = ((mean_perc_forest_2022 - mean_perc_forest_2010)/mean_perc_forest_2010)*100) %>% 
  select(code_mun, code_mun_short, starts_with("sum"), starts_with("mean"), starts_with("fpp"),
         starts_with("ifdm"), starts_with("perc"), starts_with("taxa"), contains("hectare")) %>%
  mutate(across(.cols = where(is.character), .fns = as.factor)) %>% 
  mutate(cat_change = if_else(
    condition = mean_forest_perc_change > 0 & mean_fpp_perc_change > 0,
    true = "GG",
    false = if_else(
      condition = mean_forest_perc_change > 0 & mean_fpp_perc_change < 0,
      true = "GP",
      false = if_else(
        condition =  mean_forest_perc_change < 0 & mean_fpp_perc_change > 0,
        true = "PG",
        false = if_else(
          mean_forest_perc_change < 0 & mean_fpp_perc_change < 0,
          true = "PP",
          false = "stable"
        )
      )
    )
  )
  ) %>% 
  glimpse -> tab_mun_analysis

write_sf(obj = tab_mun_analysis, dsn = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_analysis.gpkg")

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
