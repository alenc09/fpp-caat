# Wed Apr 06 11:42:10 2022 ------------------------------
#Script para organizar as tabelas para análises e gráficos

#Library----
library(sf)
library(dplyr)
library(here)
library(readxl)
library(writexl)
library(read.dbc)

#data----
st_read("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/hexGrid_landcover_caat_2006.shp") -> hexGrid_landcover_2006
st_read("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/hexGrid_landcover_caat_2017.shp") -> hexGrid_landcover_2017
st_read("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/popRur_2006.shp") -> popRur_2006
st_read("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/popRur_2017.shp") -> popRur_2017
st_read("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/popUrb_2006.shp") -> popUrb_2006
st_read("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/popUrb_2017.shp") -> popUrb_2017
st_read("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/hexGrid_mun.shp") -> hexGrid_mun
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/pibs_brasil.xlsx", sheet = 1) -> pibTot_br
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/pibs_brasil.xlsx", sheet = 2) -> pibAgro_br
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/pibs_brasil.xlsx", sheet = 3) -> pibInd_br
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/pibs_brasil.xlsx", sheet = 4) -> pibServPriv_br
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/pibs_brasil.xlsx", sheet = 5) -> pibServPub_br
read_xlsx("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap1/forest-develop/dbcap1_clean.xlsx") -> tabela_geral
read.dbc("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/mortalidade_infantil_2022.dbc") -> mortalidade_infantil_22
read.csv("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/agua_esgoto_municipio_2017-2022.csv", sep = ";", dec = ",") -> agua_esgoto

#organization----

##native vegetation cover----
###2006----
hexGrid_landcover_2006 %>% 
  select(id, HISTO_3, HISTO_4) %>%
  st_drop_geometry() %>% 
  mutate(forestPix = HISTO_3 + HISTO_4,
         forest_areaHa_2006 = forestPix*0.09,
         forest_perc_2006 = round((forest_areaHa_2006/1624)*100, digits = 2)) %>% 
  select(id, forest_areaHa_2006, forest_perc_2006) %>% 
  glimpse -> hexGrid_forest_2006
###2017----
hexGrid_landcover_2017 %>% 
  select(id, HISTO_3, HISTO_4) %>%
  st_drop_geometry() %>% 
  mutate(forestPix = HISTO_3 + HISTO_4,
         forest_areaHa_2017 = forestPix*0.09,
         forest_perc_2017 = round((forest_areaHa_2017/1624)*100, digits = 2)) %>% 
  select(id, forest_areaHa_2017, forest_perc_2017) %>% 
  glimpse -> hexGrid_forest_2017

##rural pop----
###2006----
popRur_2006 %>% 
  select(id, popRur_200) %>% 
  st_drop_geometry() %>% 
  mutate(popRur_2006 = round(popRur_200), .keep = "unused") %>% 
  glimpse -> popRur_2006

###2017----
popRur_2017 %>% 
  select(id, popRur_201) %>%
  st_drop_geometry() %>%
  mutate(popRur_2017 = round(popRur_201), .keep = "unused") %>%
  glimpse -> popRur_2017

##Urban pop----
###2006----
popUrb_2006 %>% 
  select(id, popUrb_200) %>%
  st_drop_geometry() %>%
  mutate(popUrb_2006 = round(popUrb_200), .keep = "unused") %>%
  glimpse -> popUrb_2006
###2017----
popUrb_2017 %>% 
  select(id, popUrb_201) %>%
  st_drop_geometry() %>%
  mutate(popUrb_2017 = round(popUrb_201), .keep = "unused") %>%
  glimpse -> popUrb_2017

##PIB per municipality----
pibTot_br %>% 
  left_join(y = pibAgro_br) %>% 
  left_join(y = pibInd_br) %>% 
  left_join(y = pibServPriv_br) %>% 
  left_join(y = pibServPub_br) %>% 
  select(-Município) %>% 
  rename(code_mun = Cód.) %>% 
  mutate(code_mun = as.factor(code_mun)) %>% 
  mutate(across(where(is.character), as.double))%>% 
  glimpse -> pibs_br

hexGrid_mun %>% 
  rename(hex_id = id,
         code_mun = GEOCODIG_M) %>% 
  mutate(code_mun = as.factor(code_mun)) %>% 
  left_join(y = pibs_br) %>% 
  st_drop_geometry() %>% 
  glimpse -> hexGrid_pib

##otras variables----
tabela_geral %>% 
  select(code_muni, expov_2000, expov_2010, gini_2000, gini_2010, u5mort_2000, u5mort_2010.x) %>% 
  mutate(code_mun = as.factor(code_muni), .keep = "unused") %>% 
  rename(u5mort_2010 = u5mort_2010.x) %>% 
  glimpse() -> tab_socioeconomic

##tabela geral----
hexGrid_mun %>% 
  st_drop_geometry() %>% 
  rename(hex_id = id,
         code_mun = GEOCODIG_M) %>% 
  mutate(code_mun = as.factor(code_mun)) %>% 
  left_join(y = hexGrid_forest_2006, by = c("hex_id" = "id")) %>% 
  left_join(y = hexGrid_forest_2017, by = c("hex_id" = "id")) %>% 
  left_join(y = popRur_2006, by = c("hex_id" = "id")) %>% 
  left_join(y = popRur_2017, by = c("hex_id" = "id")) %>% 
  left_join(y = popUrb_2006, by = c("hex_id" = "id")) %>% 
  left_join(y = popUrb_2017, by = c("hex_id" = "id")) %>% 
  left_join(y = hexGrid_pib) %>% 
  left_join(y = tab_socioeconomic) %>% 
  mutate(hex_id = as.factor(hex_id)) %>% 
  glimpse -> tabela_bruta

#table_analysis----
tabela_bruta %>% 
  mutate(forest_areaH_change = forest_areaHa_2017 - forest_areaHa_2006, .keep = "unused") %>% 
  mutate(forest_perc_change = forest_perc_2017 - forest_perc_2006, .keep = "unused") %>% 
  mutate(popRur_perc_change = round(((popRur_2017 - (popRur_2006+1))/(popRur_2006+1))*100, digits = 2), .keep = "unused") %>% 
  mutate(popRur_perc_change = if_else(condition = popRur_perc_change == 100 | popRur_perc_change == -100, true = 0, false = popRur_perc_change)) %>% 
  mutate(popUrb_change = popUrb_2017 - popUrb_2006, .keep = "unused") %>%
  mutate(percpib_agro_2006 = (pibAgro_2006/pibTot_2006)*100,
         percpib_agro_2017 = (pibAgro_2017/pibTot_2017)*100,
         percpib_ind_2006 = (pinInd_2006/pibTot_2006)*100,
         percpib_ind_2017 = (pibInd_2017/pibTot_2017)*100,
         percpib_servPriv_2006 = (pibServPriv_2006/pibTot_2006)*100,
         percpib_servPriv_2017 = (pibServPriv_2017/pibTot_2017)*100,
         percpib_servPub_2006 = (pibServPub_2006/pibTot_2006)*100,
         percpib_servPub_2017 = (pibServPub_2017/pibTot_2017)*100, .keep = "unused") %>%
  mutate(pibAgro_perc_change = round(percpib_agro_2017 - percpib_agro_2006, digits = 2),
         pibInd_perc_change = round(percpib_ind_2017 - percpib_ind_2006, digits = 2),
         pibServPriv_perc_change = round(percpib_servPriv_2017 - percpib_servPriv_2006, digits = 2),
         pibServPub_perc_change = round(percpib_servPub_2017 - percpib_servPub_2006, digits = 2), .keep = "unused") %>%
  mutate(expov_perc_change = expov_2010 - expov_2000,
         gini_change = gini_2010 - gini_2000,
         u5mort_change = u5mort_2010 - u5mort_2000, .keep = "unused") %>%
  mutate(hexGrid_quad = if_else( #based in quantiles (10%), see below
      condition = popRur_perc_change > pop_gain_10 & forest_perc_change > forest_gain_10,
      true = "GG",
      false = if_else(
        condition = popRur_perc_change > pop_gain_10 & forest_perc_change < (forest_loss_10*-1),
        true = "GP",
        false = if_else(
          condition = popRur_perc_change < (pop_loss_10*-1) & forest_perc_change > forest_gain_10,
          true = "PG",
          false = if_else(
            condition = popRur_perc_change < (pop_loss_10*-1) & forest_perc_change < (forest_loss_10*-1),
            true = "PP",
            false = "stable"
          )
        )
      )
    )
  ) %>%
  glimpse -> table_analysis

##mean change per municipality----
table_analysis %>% 
  group_by(code_mun) %>% 
  summarise(meanMun_forest_change = mean(forest_perc_change),
            sdMun_forest_change = sd(forest_perc_change),
            meanMun_popRur_change = mean(popRur_perc_change),
            sdMun_popRur_change = sd(popRur_perc_change)) %>% 
  mutate(mun_quad = if_else( #based on quantile. See below
           condition = meanMun_popRur_change > mun_pop_gain_10 & meanMun_forest_change > mun_fores_gain_10,
         true = "GG",
         false = if_else(
           condition = meanMun_popRur_change > mun_pop_gain_10 & meanMun_forest_change < (mun_fores_loss_10*-1),
           true = "GP",
           false = if_else(
             condition = meanMun_popRur_change < (mun_pop_loss_10*-1) & meanMun_forest_change > mun_fores_gain_10,
             true = "PG",
             false = if_else(
               condition = meanMun_popRur_change < (mun_pop_loss_10*-1) & meanMun_forest_change < (mun_fores_loss_10*-1),
               true = "PP",
               false = "stable"))))) %>%
  glimpse -> table_change_mun

table_analysis %>% 
  left_join(y = table_change_mun) %>% 
  glimpse -> table_analysis

##finding the stable units----
###using SD----
# table_analysis %>% 
#   filter(hex_id != 17549) %>% 
#   select(hex_id, forest_perc_change, popRur_perc_change) %>% 
#   reframe(threshold_up_forest = mean(forest_perc_change) + sd(forest_perc_change),
#          threshold_low_fores = mean(forest_perc_change) - sd(forest_perc_change),
#          threshold_up_pop = mean(popRur_perc_change, na.rm = T) + sd(popRur_perc_change, na.rm = T),
#          threshold_low_pop = mean(popRur_perc_change, na.rm = T) - sd(popRur_perc_change, na.rm = T)) %>% 
#   glimpse()

### using quantiles----
table_analysis %>% 
  select(hex_id, forest_perc_change) %>% 
  filter(forest_perc_change > 0) %>% 
  glimpse -> table_forestGain
quantile(table_forestGain$forest_perc_change, 0.25) -> forest_gain_10

table_analysis %>% 
  select(hex_id, forest_perc_change) %>% 
  filter(forest_perc_change < 0) %>% 
  glimpse -> table_forestLoss
quantile(table_forestLoss$forest_perc_change*-1, 0.25) -> forest_loss_10

table_analysis %>% 
  select(hex_id, popRur_perc_change) %>% 
  filter(popRur_perc_change > 0) %>% 
  glimpse -> table_popGain
quantile(table_popGain$popRur_perc_change, 0.25) -> pop_gain_10

table_analysis %>% 
  select(hex_id, popRur_perc_change) %>% 
  filter(popRur_perc_change < 0) %>% 
  glimpse -> table_popLoss
quantile(table_popLoss$popRur_perc_change*-1, 0.25) -> pop_loss_10

table_change_mun %>% 
  select(code_mun,meanMun_forest_change) %>% 
  filter(meanMun_forest_change > 0) %>% 
  glimpse -> mun_forestGain
quantile(mun_forestGain$meanMun_forest_change, 0.25) -> mun_fores_gain_10

table_change_mun %>% 
  select(code_mun,meanMun_forest_change) %>% 
  filter(meanMun_forest_change < 0) %>% 
  glimpse -> mun_forestLoss
quantile(mun_forestLoss$meanMun_forest_change*-1, 0.25) -> mun_fores_loss_10

table_change_mun %>% 
  select(code_mun,meanMun_popRur_change) %>% 
  filter(meanMun_popRur_change > 0) %>% 
  glimpse -> mun_popGain
quantile(mun_popGain$meanMun_popRur_change, 0.25) -> mun_pop_gain_10

table_change_mun %>% 
  select(code_mun,meanMun_popRur_change) %>% 
  filter(meanMun_popRur_change < 0) %>% 
  glimpse -> mun_popLoss
quantile(mun_popLoss$meanMun_popRur_change*-1, 0.25) -> mun_pop_loss_10



#exports----
#write_xlsx(x = tabela_bruta, path = here("data/tabela_bruta.xlsx"))
#write_xlsx(x = table_analysis, path = here("data/table_analysis.xlsx"))
