# Script to organise analysis tables: compute change variables and
# assign forest/FPP change categories (GG/GP/PG/PP/stable)

#Libraries----
library(sf)
library(dplyr)

#Data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tabela_buffer_nova.gpkg") -> tab_buffer
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_nova.gpkg") -> tab_mun
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_50.gpkg") -> tab_mun_50
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_70.gpkg") -> tab_mun_70

#Helper function: assign change categories----
add_cat_change <- function(df, forest_var, fpp_var) {
  df %>%
    mutate(cat_change = case_when(
      .data[[forest_var]] > 0 & .data[[fpp_var]] > 0 ~ "GG",
      .data[[forest_var]] > 0 & .data[[fpp_var]] < 0 ~ "GP",
      .data[[forest_var]] < 0 & .data[[fpp_var]] > 0 ~ "PG",
      .data[[forest_var]] < 0 & .data[[fpp_var]] < 0 ~ "PP",
      TRUE ~ "stable"
    )) %>%
    mutate(across(where(is.character), as.factor))
}

#Landscape (buffer) scale----
tab_buffer %>%
  select(-starts_with("sum_perc_"), -ends_with("2017"), -ends_with("2018"), -Gini, -code_short) %>%
  mutate(fpp_abs_change     = fpp_2022 - fpp_2010,
         fpp_perc_change    = ((fpp_2022 - fpp_2010) / fpp_2010) * 100,
         forest_abs_change  = perc_forest_2022 - perc_forest_2010,
         forest_perc_change = ((perc_forest_2022 - perc_forest_2010) / perc_forest_2010) * 100) %>%
  select(id, code_mun, code_mun_short, starts_with("perc"), starts_with("fpp"),
         starts_with("ifdm"), contains("hectare"), starts_with("taxa"), starts_with("forest")) %>%
  add_cat_change("forest_perc_change", "fpp_perc_change") %>%
  glimpse -> tab_buffer_analysis

write_sf(obj = tab_buffer_analysis, dsn = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_buffer_analysis.gpkg")

#Municipality scale — helper to compute change vars and categories----
prep_mun <- function(tab) {
  tab %>%
    mutate(mean_fpp_abs_change    = mean_fpp_2022 - mean_fpp_2010,
           mean_fpp_perc_change   = ((mean_fpp_2022 - mean_fpp_2010) / mean_fpp_2010) * 100,
           mean_forest_abs_change = mean_perc_forest_2022 - mean_perc_forest_2010,
           mean_forest_perc_change = ((mean_perc_forest_2022 - mean_perc_forest_2010) / mean_perc_forest_2010) * 100) %>%
    select(code_mun, code_mun_short, starts_with("sum"), starts_with("mean"), starts_with("fpp"),
           starts_with("ifdm"), starts_with("perc"), starts_with("taxa"), contains("hectare")) %>%
    add_cat_change("mean_forest_perc_change", "mean_fpp_perc_change")
}

tab_mun_analysis    <- prep_mun(tab_mun)    %>% glimpse
tab_mun_analysis_50 <- prep_mun(tab_mun_50) %>% glimpse
tab_mun_analysis_70 <- prep_mun(tab_mun_70) %>% glimpse

write_sf(obj = tab_mun_analysis,    dsn = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_analysis.gpkg")
write_sf(obj = tab_mun_analysis_50, dsn = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_analysis_50.gpkg")
write_sf(obj = tab_mun_analysis_70, dsn = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_analysis_70.gpkg")
