# Fri Aug 29 12:23:59 2025 ------------------------------
# Script to estimate Forest-proximate population per state at varying thresholds

#Libraries----
library(sf)
library(dplyr)
library(ggplot2)

#data----
read_xlsx("data/tabela_buffer_nova.xlsx") -> tabela_buffer_nova
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/caat_states_5880.gpkg") -> caat_states

#extrapolation per state at 20% forest cover----
tabela_buffer_nova %>% 
  mutate(code_state = str_sub(as.character(code_mun), 1,2),
         code_state = as.factor(code_state)) %>% 
  glimpse -> tabela_buffer_nova

caat_states %>% 
  mutate(area_m2 = as.numeric(st_area(geom)),
         area_state_km2 = area_m2/1e6,
         code_state = as.factor(code_state)) %>% 
  glimpse -> caat_states

tabela_buffer_nova %>% 
  rename(geom_buffer = geom,
         geom_mun = geometry) %>% 
  mutate(area_m2 = as.numeric(st_area(geom_buffer)),
         area_km2 = area_m2/1e6) %>% 
  glimpse -> buffers

buffers %>%
  mutate(is_ge20 = perc_forest_2022 >= 20) %>%
  group_by(code_state) %>%
  summarise(
    pop_sample_ge20      = sum(fpp_2022[is_ge20], na.rm = TRUE),
    area_sample_ge20_km2 = sum(area_km2[is_ge20], na.rm = TRUE),
    n_buffers_ge20       = sum(is_ge20, na.rm = TRUE),
    n_buffers_total      = n(),
    prop_buffers_ge20    = mean(is_ge20, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    dens_hat = pop_sample_ge20 / area_sample_ge20_km2
  ) %>%
  left_join(caat_states, by = "code_state") %>%
  mutate(
    area_ge20_est_km2 = prop_buffers_ge20 * area_state_km2,
    pop_est_ge20      = dens_hat * area_ge20_est_km2) %>% 
  glimpse -> estimates_fpp_states

estimates_fpp_states %>% 
  select(name_state, pop_est_ge20)

#At different thresholds----

