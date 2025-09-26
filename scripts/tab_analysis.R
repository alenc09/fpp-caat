# Wed Sep  3 17:13:19 2025 ------------------------------
# scritp to organise general table for analysis

#data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tabela_buffer_nova.gpkg") -> tab_buffer
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_nova.gpkg") -> tab_mun
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_50.gpkg") -> tab_mun_50
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_70.gpkg") -> tab_mun_70

##organization----
##tabela escala paisagem----
tab_buffer %>% 
  select(-starts_with("sum_perc_"), -ends_with("2017"), -ends_with("2018"), - Gini, -code_short) %>% 
  mutate(fpp_abs_change = fpp_2022 - fpp_2010,
         fpp_perc_change = ((fpp_2022 - fpp_2010)/fpp_2010)*100,
         forest_abs_change = perc_forest_2022 - perc_forest_2010,
         forest_perc_change = ((perc_forest_2022 - perc_forest_2010)/perc_forest_2010)*100) %>% 
  select(id, code_mun, code_mun_short, starts_with("perc"), starts_with("fpp"),
         starts_with("ifdm"), contains("hectare"), starts_with("taxa"), starts_with("forest")) %>%
  mutate(cat_change = if_else(
    condition = forest_perc_change > 0 & fpp_perc_change > 0,
    true = "GG",
    false = if_else(
      condition = forest_perc_change > 0 & fpp_perc_change < 0,
      true = "GP",
      false = if_else(
        condition =  forest_perc_change < 0 & fpp_perc_change > 0,
        true = "PG",
        false = if_else(
          forest_perc_change < 0 & fpp_perc_change < 0,
          true = "PP",
          false = "stable"))))) %>%
  mutate(across(.cols = where(is.character), .fns = as.factor)) %>%
  glimpse -> tab_buffer_analysis

# write_sf(obj = tab_buffer_analysis, dsn = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_buffer_analysis.gpkg")

##tabela escala municipal----
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

# write_sf(obj = tab_mun_analysis, dsn = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_analysis.gpkg")

##tabela escala municipal 50% and 70% threshold----
tab_mun_70 %>% 
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

# write_sf(obj = tab_mun_analysis, dsn = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_analysis_70.gpkg")
