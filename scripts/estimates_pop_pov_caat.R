# Fri Aug 29 12:30:49 2025 ------------------------------
# Script to estimate caatinga population and income levels

#Libraries----
library(sf)
library(censobr)
library(dplyr)

#data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/pop_sc_caat_2022_5880.gpkg") -> pop_sc_2022
read_tracts(year = 2022, dataset = "ResponsavelRenda") -> sc_respRenda_2022

#organization----
sc_respRenda_2022 %>% 
  as_tibble() %>% 
  dplyr::select(code_tract, code_muni, V06001, V06004) %>% 
  rename(total_domicilios_2022 = V06001,
         rendaResp_mean_2022 = V06004) %>%
  mutate(renda_media_mun_2022 = sum(total_domicilios_2022*rendaResp_mean_2022, na.rm = T)/ sum(total_domicilios_2022, na.rm = T)) %>% 
  glimpse -> sc_rendaResp_mean_2022

#analysis----
##população da Caatinga----
pop_sc_2022 %>% 
  reframe(pop_total_caat = sum(V0001),
          pop_urbana_caat = sum(V0001[situacao.x == "Urbana"], na.rm = T),
          pop_rural_caat = sum(V0001[situacao.x == "Rural"], na.rm = T)) %>% 
  glimpse -> pop_caat_2022

##pobreza na Caatinga----
sc_rendaResp_mean_2022 %>% 
  mutate(in_caatinga = if_else(code_tract %in% pop_sc_2022$code_tract, "Caatinga", "Fora")) %>% 
  glimpse -> renda_flag

renda_flag %>% 
  group_by(in_caatinga) %>% 
  summarise(renda_media = mean(rendaResp_mean_2022, na.rm = TRUE)) %>% 
  glimpse

## mudança na população rural da Caatinga----
tab_mun_nova %>%
  mutate(rural_pop_perc_change = perc_popRur_2022 - perc_popRur_2010) %>% 
  mutate(
    tendencia = case_when(
      rural_pop_perc_change > 0  ~ "Crescendo",
      rural_pop_perc_change < 0  ~ "Diminuindo",
      TRUE             ~ "Estável"
    )
  ) %>%
  count(tendencia) %>%
  mutate(perc = 100 * n / sum(n))