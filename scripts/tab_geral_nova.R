# Wed Aug 13 16:35:20 2025 ------------------------------
# Script para organizar tabela geral dos dados novos

#libraries----
library(dplyr)
library(lubridate)
library(readxl)
library(sf)
library(acid)
library(censobr)
library(read.dbc)
library(tidyr)
library(stringr)
library(purrr)
library(areal)

#data----
read.csv("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/cadunico_pessoas_2012-2022.csv", header = F) -> cadunico_2012_2010
read.csv("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/git_projects/fpp-caat/data/tabela_geral.csv") -> tab_geral_old
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/pop_municipio_2010.xlsx") -> pop_mun_2010
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/pop_municipio_censo2022.xlsx") -> pop_mun_2022
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/Serie-Historica-IFDM-2013-a-2023.xlsx", sheet = 3) -> IFDM_saude
read.csv("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/agrifamiliar_cadunico_2012-2023.csv", header = F) -> agrifam_cadunico
read_tracts(year = 2010, dataset = "ResponsavelRenda") -> sc_respRenda_2010
read_tracts(year = 2022, dataset = "ResponsavelRenda") -> sc_respRenda_2022
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/statistical/moratlidade_5anos_evitavel_2010.xlsx") -> u5mort_2010
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/statistical/moratlidade_5anos_evitavel_2017.xlsx") -> u5mort_2017
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/statistical/moratlidade_5anos_evitavel_2022.xlsx") -> u5mort_2022
read.csv("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/statistical/nascidos_vivos_2010.csv") -> nasc_2010
read.csv("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/statistical/nascidos_vivos_2017.csv") -> nasc_2017
read.csv("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/statistical/nascidos_vivos_2022.csv") -> nasc_2022
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/statistical/cisterna_consumo_mds_2010-2022.xlsx") -> cisternas_consumo_mds_2010_2022
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/statistical/cisterna_consumo_outros_2010-2022.xlsx") -> cisternas_consumo_outros_2010_2022
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/statistical/cisterna_producao_mds_2010-2022.xlsx") -> cisternas_producao_mds_2010_2022
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/ANA_DemandaHidrica_Municipio_1931a2040_env.xlsx", sheet = 2) -> irrigation
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/esgoto_censo_2010.xlsx", sheet = 2) -> esgoto_censo_2010
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/esgoto_censo_2022.xlsx", sheet = 2) -> esgoto_censo_2022
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/iluminacao_censo_2010-2022.xlsx", sheet = 2) -> iliminacao_2010_2022
read.csv("~/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/buffer_forest.csv") -> buffer_lc
read_sf("~/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/buffer_fpp_5880.gpkg") -> buffer_fpp
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/caat_mun_2010_5880.shp") -> caat_mun_2010_5880
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/domicilios_br_2010.xlsx") -> domicilios_br_2010
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/caat_mun_2017_5880.gpkg") -> caat_mun_2017_5880
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/caat_mun_2022_5880.shp") -> caat_mun_2022_5880
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/statistical/cattle_production_10_22.xlsx") -> cattle_herd
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/statistical/pib_agricultura.xlsx") -> pib_agro

#tratamento das planilhas----
##cadastro unico----
cadunico_2012_2010 %>% 
  slice(-1) %>% 
  dplyr::select(V1, V4, V5) %>% 
  mutate(code_mun = as.factor(V1),
         date = my(V4),
         pessoas_cadunico = as.double(V5),
         year = year(date),
         .keep = "none") %>% 
  filter(date == "2012-12-01" | date == "2017-12-01" | date == "2022-12-01") %>%
  pivot_wider(id_cols = code_mun,
    names_from = year,
    values_from = pessoas_cadunico,
    names_glue = "pessoas_cadunico_{year}"
  ) %>% 
  glimpse -> pessoas_cadunico_2012_2022

##urban population----
pop_mun_2010 %>% 
  dplyr::select(-name_mun) %>% 
  mutate(code_mun = as.factor(code_mun),
         code_short = as.factor(substr(as.character(code_mun), 1,6)), 
         pop_total_2010 = total,
         pop_urbana_2010 = urbana,
         pop_rural_2010 = rural,
         perc_popUrb_2010 = (urbana/total)*100,
         perc_popRur_2010 = (rural/total)*100,
         .keep = "unused")  %>% 
  glimpse -> pop_mun_2010

pop_mun_2022 %>%
  dplyr::select(-name_mun) %>% 
  mutate(code_mun = as.factor(code_mun),
         code_short = as.factor(substr(as.character(code_mun), 1,6)), 
         perc_popUrb_2022 = (pop_urbana_2022/pop_total_2022)*100,
         perc_popRur_2022 = (pop_rural_2022/pop_total_2022)*100)  %>% 
  glimpse -> pop_mun_2022

##Firjan index----
IFDM_saude %>% 
  dplyr::select(COD_MUNIC, ends_with(c("2013", "2017", "2022"))) %>% 
  dplyr::select(COD_MUNIC, starts_with("IFDM")) %>% 
  mutate(code_mun = as.factor(COD_MUNIC),
         ifdm_saude_2013 = as.double(`IFDM Saúde 2013`),
         ifdm_saude_2017 = `IFDM Saúde 2017`,
         ifdm_saude_2022 = `IFDM Saúde 2022`,
         .keep = "none") %>% 
  glimpse -> IFDM_saude_2013_2022

##family farm in Cadastro unico----
agrifam_cadunico %>% 
  slice(-1) %>% 
  dplyr::select(V1, V4, V5) %>% 
  mutate(code_mun = as.factor(V1),
         date = as.double(V4),
         agrifam_cadunico = as.double(V5),
         .keep = "none") %>% 
  filter(date == 2012 | date == 2017 | date == 2022) %>% 
  pivot_wider(id_cols = code_mun,
              names_from = date,
              values_from = agrifam_cadunico,
              names_glue = "agrifam_cadunico_{date}") %>% 
  glimpse -> agrifam_cadunico_2012_2022

##renda domiciliar----
sc_respRenda_2010 %>% 
  collect() %>% 
  select(code_tract, code_muni, V087, V088) %>% 
  mutate(code_tract = as.factor(code_tract),
         code_mun = as.factor(code_muni),
         respRenda_media_2010 = V088/V087,
         pessoas_resp_2010 = V087,
         .keep = "unused") %>% 
  group_by(code_mun) %>%
  summarise(
    mean_respRenda_2010   = weighted.mean(respRenda_media_2010, pessoas_resp_2010, na.rm = TRUE)) %>% 
  glimpse -> mun_respRenda_2010

sc_respRenda_2022 %>% 
  collect() %>% 
  select(code_tract, code_muni, V06001, V06004) %>% 
  mutate(code_tract = as.factor(code_tract),
         code_mun = as.factor(code_muni),
         respRenda_media_2022 = V06004 * (3195.89/6474.09), #IPCA número-índice. dados coletados da tabela 1737 do IBGE-SIDRA
         pessoas_resp_2022 = V06001,
         .keep = "unused") %>% group_by(code_mun) %>%
  summarise(
    mean_respRenda_2022   = weighted.mean(respRenda_media_2022, pessoas_resp_2022, na.rm = TRUE)) %>% 
  glimpse -> mun_respRenda_2022

##Infant mortality----
u5mort_2010 %>% 
  slice(-4455) %>% 
  separate(col = MunicÌpio,
           into = c("code_mun","name_mun"),
           sep = " ",
           extra = "merge") %>% 
  mutate(code_mun = as.factor(code_mun)) %>% 
  glimpse -> u5mort_2010

nasc_2010 %>% 
  slice(-5582) %>% 
  separate(col = Municipio,
          into = c("code_mun","name_mun"),
          sep = " ",
          extra = "merge") %>% 
  mutate(code_mun = as.factor(code_mun)) %>% 
  glimpse -> nasc_2010

nasc_2010 %>% 
  left_join(y = u5mort_2010, by = "code_mun") %>% 
  dplyr::select(-name_mun.x, -name_mun.y) %>% 
  mutate(across(everything(), ~replace_na(., 0))) %>%
  mutate(taxa_u5mort_2010 = (u5mort_evitavel_2010/nasc_vivo_2010)*1000) %>% 
  filter(code_mun != "") %>% 
  dplyr::select(code_mun, taxa_u5mort_2010) %>% 
  glimpse -> taxa_u5mort_2010

u5mort_2017 %>% 
  slice(-4344) %>%
  separate(col = MunicÌpio,
           into = c("code_mun","name_mun"),
           sep = " ",
           extra = "merge") %>%
  mutate(code_mun = as.factor(code_mun)) %>%
  glimpse -> u5mort_2017

nasc_2017 %>% 
  slice(-5584) %>%
  separate(col = Municipio,
           into = c("code_mun","name_mun"),
           sep = " ",
           extra = "merge") %>%
  mutate(code_mun = as.factor(code_mun)) %>%
  glimpse -> nasc_2017

nasc_2017 %>% 
  left_join(y = u5mort_2017, by = "code_mun") %>% 
  dplyr::select(-name_mun.x, -name_mun.y) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  mutate(taxa_u5mort_2017 = (u5mort_evital_2017/nasc_vivos_2017)*1000) %>%
  filter(code_mun != "") %>%
  dplyr::select(code_mun, taxa_u5mort_2017) %>%
  glimpse -> taxa_u5mort_2017

u5mort_2022 %>% 
  slice(-4294) %>%
  separate(col = MunicÌpio,
           into = c("code_mun","name_mun"),
           sep = " ",
           extra = "merge") %>%
  mutate(code_mun = as.factor(code_mun)) %>%
  glimpse -> u5mort_2022

nasc_2022 %>% 
  slice(-5588) %>%
  separate(col = Municipio,
           into = c("code_mun","name_mun"),
           sep = " ",
           extra = "merge") %>%
  mutate(code_mun = as.factor(code_mun)) %>%
  glimpse -> nasc_2022

nasc_2022 %>% 
  left_join(y = u5mort_2022, by = "code_mun") %>%
  dplyr::select(-name_mun.x, -name_mun.y) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  mutate(taxa_u5mort_2022 = (u5mor_evital_2022/nasc_vivos_2022)*1000) %>%
  filter(code_mun != "") %>%
  dplyr::select(code_mun, taxa_u5mort_2022) %>%
  glimpse -> taxa_u5mort_2022

##cisterns----
cisternas_consumo_mds_2010_2022 %>% 
  dplyr::select(code_mun, year, cisternas_acumulado) %>% 
  filter(year == 2010 | year == 2017 | year == 2022) %>% 
  pivot_wider(id_cols = code_mun,
              names_from = year,
              values_from = cisternas_acumulado,
              names_glue = "cisternas_acumulado_{year}") %>% 
  glimpse -> cisternas_consumo_mds

cisternas_consumo_outros_2010_2022 %>% 
  dplyr::select(code_mun, year, cistern_acumulado) %>% 
  filter(year == 2010 | year == 2017 | year == 2022) %>% 
  pivot_wider(id_cols = code_mun,
              names_from = year,
              values_from = cistern_acumulado,
              names_glue = "cisternas_acumulado_{year}") %>%
  glimpse -> cisternas_consumo_outros

cisternas_producao_mds_2010_2022 %>% 
  dplyr::select(code_mun, year, cisternas_producao_acumulado) %>% 
  filter(year == 2010 | year == 2017 | year == 2022) %>% 
  pivot_wider(id_cols = code_mun,
              names_from = year,
              values_from = cisternas_producao_acumulado,
              names_glue = "cisternas_acumulado_{year}") %>%
  glimpse -> cisternas_producao_mds

cisternas_consumo_mds %>% 
  left_join(y = cisternas_consumo_outros, by = "code_mun") %>% 
  left_join(y = cisternas_producao_mds, by = "code_mun") %>% 
  mutate(across(everything(), ~replace_na(., 0))) %>%
  mutate(code_mun = as.factor(code_mun),
         cisternas_2010 = cisternas_acumulado_2010.x + cisternas_acumulado_2010.y + cisternas_acumulado_2010,
         cisternas_2017 = cisternas_acumulado_2017.x + cisternas_acumulado_2017.y + cisternas_acumulado_2017,
         cisternas_2022 = cisternas_acumulado_2022.x + cisternas_acumulado_2022.y + cisternas_acumulado_2022,
         .keep = "unused") %>% 
  glimpse -> cisternas

##Irrigation----
irrigation %>% 
  dplyr::select(mun_cd_atual, aa_ref, retirada_TOTAL_m3_s) %>% 
  filter(aa_ref == 2010 | aa_ref == 2017| aa_ref == 2022) %>% 
  rename(code_mun = mun_cd_atual,
         year = aa_ref,
         irrigation_m3s = retirada_TOTAL_m3_s) %>% 
  pivot_wider(id_cols = code_mun,
              names_from = year,
              values_from = irrigation_m3s,
              names_glue = "irrigation_m3s_{year}") %>% 
  mutate(code_mun = as.factor(code_mun)) %>% 
  glimpse -> irrigation_2010_2022

##sanitation----
esgoto_censo_2010 %>% 
  select(code_mun, `Rede geral de esgoto ou pluvial`, `Fossa séptica`) %>% 
  mutate(code_mun = as.factor(code_mun),
         perc_esgoto_2010 = as.double(`Rede geral de esgoto ou pluvial`),
         perc_fossa_2010 = as.double(`Fossa séptica`),
         perc_saneamento_2010 = perc_esgoto_2010 + perc_fossa_2010,
         .keep = "unused") %>% 
  glimpse -> esgoto_censo_2010

esgoto_censo_2022 %>% 
  select(code_mun, `Rede geral, rede pluvial ou fossa ligada à rede`, `Fossa séptica ou fossa filtro não ligada à rede`) %>%
  mutate(code_mun = as.factor(code_mun),
         perc_esgoto_2022 = as.double(`Rede geral, rede pluvial ou fossa ligada à rede`),
         perc_fossa_2022 = as.double(`Fossa séptica ou fossa filtro não ligada à rede`),
         perc_saneamento_2022 = perc_esgoto_2022 + perc_fossa_2022,
         .keep = "unused") %>%
  glimpse -> esgoto_censo_2022

##public lighting----
iliminacao_2010_2022 %>% 
  dplyr::select(-name_mun) %>% 
  mutate(code_mun = as.factor(code_mun),
         perc_public_light_2010 = as.double(public_light_2010),
         perc_public_light_2022 = public_light_2022,
         .keep = "unused") %>% 
  glimpse -> iluminacao

##gaaaaaado----
cattle_herd %>% 
  mutate(across(.cols = where(is.character), .fns = as.double)) %>% 
  mutate(code_mun = as.factor(code_mun)) %>%
  select(-name_mun, -starts_with("ovino")) %>% 
  glimpse -> tab_gado

##pib agricola----
pib_agro %>% 
  mutate(code_mun = as.factor(code_mun),
         perc_pib_agro_2010 = as.double(pib_agro_perc_2010),
         perc_pib_agro_2021 = pib_agro_perc_2021) %>% 
  glimpse -> pib_agro

#junção dados----
buffer_lc %>% 
  mutate(id = as.character(id)) %>% 
  left_join(y = buffer_fpp) %>% 
  mutate(code_mun = as.factor(code_mun),
         id = as.factor(id)) %>% 
  select(-X) %>% 
  glimpse -> tabela_buffer

##2010
caat_mun_2010_5880 %>% 
  dplyr::select(code_mn) %>% 
  mutate(code_mun = as.factor(code_mn), .keep = "unused") %>%
  mutate(code_mun_short = code_mun %>%
      as.character() %>%
      str_sub(1, -2) %>%              
      factor()) %>% 
  left_join(y = dplyr::select(agrifam_cadunico_2012_2022, code_mun, agrifam_cadunico_2012), by = c("code_mun_short" = "code_mun")) %>%
  left_join(y = pop_mun_2010) %>% 
  left_join(y = dplyr::select(IFDM_saude_2013_2022, code_mun, ifdm_saude_2013), by = c("code_mun_short" = "code_mun")) %>% 
  left_join(y = dplyr::select(pessoas_cadunico_2012_2022, code_mun, pessoas_cadunico_2012), by = c("code_mun_short" = "code_mun")) %>% 
  left_join(y = taxa_u5mort_2010, by = c("code_mun_short" = "code_mun")) %>% 
  left_join(y = dplyr::select(cisternas, code_mun, cisternas_2010), by = c("code_mun_short" = "code_mun")) %>% 
  mutate(cisternas_2010 = replace_na(cisternas_2010, 0)) %>% 
  left_join(y = dplyr::select(irrigation_2010_2022, code_mun, irrigation_m3s_2010)) %>% 
  left_join(y = dplyr::select(esgoto_censo_2010, code_mun, perc_saneamento_2010)) %>% 
  left_join(y = dplyr::select(iluminacao, code_mun, perc_public_light_2010)) %>% 
  left_join(y = domicilios_br_2010) %>% 
  left_join(y = mun_respRenda_2010, by = "code_mun") %>% 
  left_join(y = tab_gado, by = "code_mun") %>% 
  select(-ends_with("2022")) %>% 
  left_join(y = select(pib_agro, code_mun, perc_pib_agro_2010)) %>% 
  mutate(area_m2 = st_area(geometry),
         area_ha = as.numeric(area_m2)/10000,
         perc_agrifam_cadunico_2012 = (agrifam_cadunico_2012/domicilios_rural)*100,
         perc_pessoas_cadunico_2012 = (pessoas_cadunico_2012/pop_total_2010)*100,
         perc_cisternas_2010 = (cisternas_2010/domicilios_rural)*100,
         irrigacao_hectare_2010 = irrigation_m3s_2010/area_ha,
         bovino_hectare_2010 = bovino_2010/area_ha,
         caprino_hectare_2010 = caprino_2010/area_ha) %>% 
  glimpse -> caat_mun_dados_2010

# write_sf(obj = caat_mun_dados_2010, dsn = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/caat_mun_dados_2010.gpkg")

### Area weighted average
caat_mun_2022_5880 %>% 
  dplyr::select(CD_MUN) %>% 
  mutate(code_mun = as.factor(CD_MUN), .keep = "unused") %>% 
  glimpse -> caat_mun_2022_5880

aw_interpolate(.data = caat_mun_2022_5880,                # malha destino (2022)
               tid = code_mun,                 # ID único da malha destino
               source = caat_mun_dados_2010,               # malha de origem (2010)
               sid = code_mun,                 # ID único da malha origem
               weight = "sum",                  # tipo de agregação
               output = "sf",               # retorna um objeto sf
               intensive = c("perc_agrifam_cadunico_2012", "perc_popUrb_2010", "perc_popRur_2010",
                             "ifdm_saude_2013", "perc_pessoas_cadunico_2012",
                             "taxa_u5mort_2010", "perc_cisternas_2010",
                             "irrigacao_hectare_2010", "perc_saneamento_2010",
                             "perc_public_light_2010", "mean_respRenda_2010",
                             "bovino_hectare_2010", "caprino_hectare_2010",
                             "perc_pib_agro_2010")                 # ou variáveis densidade
               ) -> dados_mun_awa_2010

dados_mun_awa_2010 %>% glimpse

# ##2017

# 
# caat_mun_2017_5880 %>%
#   dplyr::select(CD_GEOCMU) %>% 
#   mutate(code_mun = as.factor(CD_GEOCMU), .keep = "unused") %>% 
#   mutate(code_mun_short = code_mun %>%
#            as.character() %>%
#            str_sub(1, -2) %>%              
#            factor()) %>% 
#   left_join(y = dplyr::select(agrifam_cadunico_2012_2022, code_mun, agrifam_cadunico_2017), by = c("code_mun_short" = "code_mun")) %>% 
#   left_join(y = dplyr::select(IFDM_saude_2013_2022, code_mun, ifdm_saude_2017), by = c("code_mun_short" = "code_mun")) %>% 
#   left_join(y = dplyr::select(pessoas_cadunico_2012_2022, code_mun, pessoas_cadunico_2017), by = c("code_mun_short" = "code_mun")) %>% 
#   left_join(y = taxa_u5mort_2017, by = c("code_mun_short" = "code_mun")) %>% 
#   left_join(y = dplyr::select(cisternas, code_mun, cisternas_2017), by = c("code_mun_short" = "code_mun")) %>% 
#   mutate(cisternas_2017 = replace_na(cisternas_2017, 0)) %>% 
#   left_join(y = dplyr::select(irrigation_2010_2022, code_mun, irrigation_m3s_2017)) %>% 
#   left_join(y = domicilios_rural_brasil_2022) %>% 
#   left_join(y = pop_mun_2022) %>% 
#   mutate(area_m2 = st_area(geom),
#          area_ha = as.numeric(area_m2)/10000,
#          perc_agrifam_cadunico_2017 = (agrifam_cadunico_2017/domicilios_rural_2022)*100,
#          perc_pessoas_cadunico_2017 = (pessoas_cadunico_2017/pop_total_2022)*100,
#          perc_cisternas_2017 = (cisternas_2017/domicilios_rural_2022)*100,
#          irrigacao_hectare_2017 = irrigation_m3s_2017/area_ha) %>%
#   glimpse -> caat_mun_dados_2017

# write_sf(obj = caat_mun_dados_2017, dsn = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/caat_mun_dados_2017.gpkg")

# ##area weighted average
# aw_interpolate(.data = caat_mun_2022_5880,                # malha destino (2022)
#                tid = code_mun,                 # ID único da malha destino
#                source = caat_mun_dados_2017,               # malha de origem (2010)
#                sid = code_mun,                 # ID único da malha origem
#                weight = "sum",                  # tipo de agregação
#                output = "sf",               # retorna um objeto sf
#                intensive = c("perc_agrifam_cadunico_2017",
#                              "ifdm_saude_2017", "perc_pessoas_cadunico_2017",
#                              "taxa_u5mort_2017", "perc_cisternas_2017",
#                              "irrigacao_hectare_2017")                 # ou variáveis densidade
# ) -> dados_mun_awa_2017
# 
# dados_mun_awa_2017 %>% glimpse

###2022
censobr::read_tracts(dataset = "Basico", year = 2022) -> basico_2022

basico_2022 %>%
  collect() %>%
  dplyr::select(code_tract, code_muni, situacao, V0003) %>%
  filter(situacao == "Rural") %>%
  group_by(code_muni) %>%
  reframe(domicilios_rural_2022 = sum(V0003)) %>%
  mutate(code_mun = as.factor(code_muni), .keep = "unused") %>%
  glimpse -> domicilios_rural_brasil_2022

caat_mun_2022_5880 %>% 
  mutate(code_mun_short = code_mun %>%
           as.character() %>%
           str_sub(1, -2) %>%
           factor()) %>%
  left_join(y = dplyr::select(agrifam_cadunico_2012_2022, code_mun, agrifam_cadunico_2022), by = c("code_mun_short" = "code_mun")) %>%
  left_join(y = pop_mun_2022) %>%
  left_join(y = dplyr::select(IFDM_saude_2013_2022, code_mun, ifdm_saude_2022), by = c("code_mun_short" = "code_mun")) %>%
  left_join(y = dplyr::select(pessoas_cadunico_2012_2022, code_mun, pessoas_cadunico_2022), by = c("code_mun_short" = "code_mun")) %>%
  left_join(y = taxa_u5mort_2022, by = c("code_mun_short" = "code_mun")) %>%
  left_join(y = dplyr::select(cisternas, code_mun, cisternas_2022), by = c("code_mun_short" = "code_mun")) %>%
  mutate(cisternas_2022 = replace_na(cisternas_2022, 0)) %>%
  left_join(y = dplyr::select(irrigation_2010_2022, code_mun, irrigation_m3s_2022)) %>%
  left_join(y = dplyr::select(esgoto_censo_2022, code_mun, perc_saneamento_2022)) %>%
  left_join(y = dplyr::select(iluminacao, code_mun, perc_public_light_2022)) %>%
  left_join(y = domicilios_rural_brasil_2022) %>%
  left_join(y = mun_respRenda_2022, by = "code_mun") %>% 
  left_join(y = select(tab_gado, code_mun, bovino_2022, caprino_2022), by = "code_mun") %>% 
  left_join(y = select(pib_agro, code_mun, perc_pib_agro_2021)) %>% 
  mutate(area_m2 = st_area(geometry),
         area_ha = as.numeric(area_m2)/10000,
         perc_agrifam_cadunico_2022 = (agrifam_cadunico_2022/domicilios_rural_2022)*100,
         perc_pessoas_cadunico_2022 = (pessoas_cadunico_2022/pop_total_2022)*100,
         perc_cisternas_2022 = (cisternas_2022/domicilios_rural_2022)*100,
         irrigacao_hectare_2022 = irrigation_m3s_2022/area_ha,
         bovino_hectare_2022 = bovino_2022/area_ha,
         caprino_hectare_2022 = caprino_2022/area_ha
         ) %>%
  glimpse -> caat_mun_dados_2022

##Escala de paisagem----
tabela_buffer%>% 
  left_join(y = dados_mun_awa_2010, by = "code_mun") %>% 
  # left_join(y = dados_mun_awa_2017, by = c("code_mun", "geometry")) %>% 
  left_join(y = caat_mun_dados_2022, by = c("code_mun", "geometry")) %>% 
  mutate(code_state = str_sub(as.character(code_mun), 1,2),
         code_state = as.factor(code_state)) %>% 
  select(-geometry) %>% 
  rename(geometry = geom) %>% 
  glimpse -> tabela_buffer_nova

# writexl::write_xlsx(x = tabela_buffer_nova, path = "data/tabela_buffer_nova.xlsx")
# st_write(obj = tabela_buffer_nova, dsn = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tabela_buffer_nova.gpkg", append=F)

##Escala de município----
df_long <- tabela_buffer %>% 
  filter(perc_forest_2022 >= 20) %>% 
  pivot_longer(
    cols = matches("perc_forest_\\d+|fpp_\\d+"),
    names_to = c("var", "year"),
    names_pattern = "(.*)_(\\d+)"
  )

df_mean <- df_long %>%
  group_by(code_mun, year, var) %>%
  summarise(sum = sum(value),
    mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = T),
            .groups = "drop")

df_mean %>%
  pivot_wider(
    names_from = c(var, year),
    values_from = c(sum, mean, sd),
    names_sep = "_"
  ) -> df_wide

df_wide %>% 
  left_join(y = dados_mun_awa_2010, by = "code_mun") %>% 
  # left_join(y = dados_mun_awa_2017, by = c("code_mun", "geometry")) %>% 
  left_join(y = caat_mun_dados_2022, by = c("code_mun", "geometry")) %>% 
  select(-starts_with("sum_perc_"), -ends_with("2017"), -ends_with("2018"), -code_short) %>%  #remover as colunas que não fazem sentido
  glimpse -> tab_mun_nova

# writexl::write_xlsx(x = tab_mun_nova, path = "data/tabela_mun_nova.xlsx")
# write_sf(obj = tab_mun_nova, dsn = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_nova.gpkg")

##Robustness check - Only landscapes with 50% or 70% forest cover
df_long <- tabela_buffer %>% 
  filter(perc_forest_2022 >= 70) %>% # changed to test the 50% and 70% thresholds
  pivot_longer(
    cols = matches("perc_forest_\\d+|fpp_\\d+"),
    names_to = c("var", "year"),
    names_pattern = "(.*)_(\\d+)"
  )

df_mean <- df_long %>%
  group_by(code_mun, year, var) %>%
  summarise(sum = sum(value),
            mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = T),
            .groups = "drop")

df_mean %>%
  pivot_wider(
    names_from = c(var, year),
    values_from = c(sum, mean, sd),
    names_sep = "_"
  ) -> df_wide

df_wide %>% 
  left_join(y = dados_mun_awa_2010, by = "code_mun") %>% 
  # left_join(y = dados_mun_awa_2017, by = c("code_mun", "geometry")) %>% 
  left_join(y = caat_mun_dados_2022, by = c("code_mun", "geometry")) %>% 
  select(-starts_with("sum_perc_"), -ends_with("2017"), -ends_with("2018"), -code_short) %>%  #remover as colunas que não fazem sentido
  glimpse -> tab_mun_nova

# writexl::write_xlsx(x = tab_mun_nova, path = "data/tabela_mun_70.xlsx")
# write_sf(obj = tab_mun_nova, dsn = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_70.gpkg")
