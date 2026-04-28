# Wed Apr 06 11:42:10 2022 ------------------------------
#Script para organizar as tabelas para análises e gráficos

#Library----
library(sf)
library(dplyr)
library(here)
library(readxl)
library(tidyr)
library(geobr)


#data----
read.csv(file = here("buff_lsm_2006.csv"), sep = ",", dec=".")-> buff_lsm_2006
read.csv(file = here("buff_lsm_2017.csv"))-> buff_lsm_2017
read_xlsx("/home/alenc/Documents/Doutorado/tese/cap0/data/tabela2006.xlsx") -> table_agrifamiliar_2006
read_xlsx("/home/alenc/Documents/Doutorado/tese/cap0/data/tabela2006.xlsx", sheet = 2) -> table_agrifamiliar_area_2006
read_xlsx("/home/alenc/Documents/Doutorado/tese/cap0/data/tabela2017.xlsx") -> table_agrifamiliar_2017
read_xlsx("/home/alenc/Documents/Doutorado/tese/cap0/data/tabela2017.xlsx", sheet = 2) -> table_agrifamiliar_area_2017
st_read(here("data/buff_5km_pop_rural_WP_2006.shp"))-> pop_rural_2006
st_read(here("data/buff_5km_pop_rural_WP_2017.shp"))-> pop_rural_2017
st_read(here("data/caat_mun.shp"))-> caat_mun
st_read(here("data/sc_rural_caat.shp"))-> sc_rural_caat
st_read(here("data/buffs_distance.shp"))-> dist_sede
st_read(here("data/sc_urbano_caat_pop_2006.shp"))-> pop_urb_2006
st_read(here("data/sc_urbano_caat_pop_2017.shp"))-> pop_urb_2017
st_read(here("data/buff_code_sc_muni_uf.shp"))-> buff_codes
read_municipality(simplified = F) -> br_mun
st_read(here("data/BR_SETORES_2017_CENSOAGRO.shp"))-> sc_br

#manipulation----
##native vegetation cover----
buff_lsm_2006 %>% 
  select(plot_id, metric, class, value) %>% 
  filter(plot_id != 1606) %>% 
  pivot_wider(id_cols = plot_id,
              names_sep = "_",
              names_from = c(metric, class),
              values_from = value
              ) %>% 
  select(plot_id,shdi_NA, pland_3, pland_4, pland_5, pland_9, pland_11, pland_12, 
         pland_13, pland_15, pland_20, pland_21, pland_23, pland_24, pland_25,
         pland_29, pland_30, pland_31, pland_32, pland_33, pland_39, pland_41,
         pland_46, pland_48, ca_3, ca_4, ca_5, ca_9, ca_11, ca_12, ca_13, ca_15,
         ca_20, ca_21, ca_23, ca_24, ca_25, ca_29, ca_30, ca_31, ca_32, ca_33,
         ca_39, ca_41, ca_46, ca_48) %>%
  replace(is.na(.), 0) %>% 
  rename(pland_forest = pland_3,
         pland_savanna = pland_4,
         pland_mangrove = pland_5,
         pland_Fplantation = pland_9,
         pland_wetland = pland_11,
         pland_grass = pland_12,
         pland_otherVeg = pland_13,
         pland_pasture = pland_15,
         pland_sugar = pland_20,
         pland_mosaicAP = pland_21,
         pland_sand = pland_23,
         pland_urban = pland_24,
         pland_otherNveg = pland_25,
         pland_rocky = pland_29,
         pland_mine = pland_30,
         pland_aquacult = pland_31,
         pland_salt = pland_32,
         pland_water = pland_33,
         pland_soy = pland_39,
         pland_otherTcrop = pland_41,
         pland_coffe = pland_46,
         pland_otherPcrop = pland_48,
         ca_forest = ca_3,
         ca_savanna = ca_4,
         ca_mangrove = ca_5,
         ca_Fplantation = ca_9,
         ca_wetland = ca_11,
         ca_grass = ca_12,
         ca_otherVeg = ca_13,
         ca_pasture = ca_15,
         ca_sugar = ca_20,
         ca_mosaicAP = ca_21,
         ca_sand = ca_23,
         ca_urban = ca_24,
         ca_otherNveg = ca_25,
         ca_rocky = ca_29,
         ca_mine = ca_30,
         ca_aquacult = ca_31,
         ca_salt = ca_32,
         ca_water = ca_33,
         ca_soy = ca_39,
         ca_otherTcrop = ca_41,
         ca_coffe = ca_46,
         ca_otherPcrop = ca_48
         ) #-> tab_forest_06
%>% 
  mutate(
    plot_id = plot_id,
    pland_nvc_06 = pland_forest + pland_savanna + pland_grass + pland_rocky +
      pland_mangrove + pland_wetland + pland_otherVeg + pland_salt,
    pland_agri_06 = pland_pasture + pland_mosaicAP + pland_otherPcrop +
      pland_otherTcrop + pland_sugar + pland_aquacult + pland_coffe +
      pland_Fplantation + pland_soy,
    ca_nvc_06 = ca_forest + ca_savanna + ca_grass + ca_rocky +
      ca_mangrove + ca_wetland + ca_otherVeg + ca_salt,
    ca_agri_06 = ca_pasture + ca_mosaicAP + ca_otherPcrop +
      ca_otherTcrop + ca_sugar + ca_aquacult + ca_coffe +
      ca_Fplantation + ca_soy,
    pland_urban_06 = pland_urban,
    ca_urban_06 = ca_urban,
    shdi_06 = shdi_NA,
    .keep = "none"
  ) %>% 
    glimpse #-> tab_1

buff_lsm_2017 %>% 
  select(plot_id, metric, class, value) %>% 
  filter(plot_id != 1606) %>% 
  pivot_wider(id_cols = plot_id,
              names_sep = "_",
              names_from = c(metric, class),
              values_from = value) %>%
  select(plot_id,shdi_NA, pland_3, pland_4, pland_5, pland_9, pland_11, pland_12, 
         pland_13, pland_15, pland_20, pland_21, pland_23, pland_24, pland_25,
         pland_29, pland_30, pland_31, pland_32, pland_33, pland_39, pland_41,
         pland_46, pland_48, ca_3, ca_4, ca_5, ca_9, ca_11, ca_12, ca_13, ca_15,
         ca_20, ca_21, ca_23, ca_24, ca_25, ca_29, ca_30, ca_31, ca_32, ca_33,
         ca_39, ca_41, ca_46, ca_48) %>%
  replace(is.na(.), 0) %>% 
  rename(pland_forest = pland_3,
         pland_savanna = pland_4,
         pland_mangrove = pland_5,
         pland_Fplantation = pland_9,
         pland_wetland = pland_11,
         pland_grass = pland_12,
         pland_otherVeg = pland_13,
         pland_pasture = pland_15,
         pland_sugar = pland_20,
         pland_mosaicAP = pland_21,
         pland_sand = pland_23,
         pland_urban = pland_24,
         pland_otherNveg = pland_25,
         pland_rocky = pland_29,
         pland_mine = pland_30,
         pland_aquacult = pland_31,
         pland_salt = pland_32,
         pland_water = pland_33,
         pland_soy = pland_39,
         pland_otherTcrop = pland_41,
         pland_coffe = pland_46,
         pland_otherPcrop = pland_48,
         ca_forest = ca_3,
         ca_savanna = ca_4,
         ca_mangrove = ca_5,
         ca_Fplantation = ca_9,
         ca_wetland = ca_11,
         ca_grass = ca_12,
         ca_otherVeg = ca_13,
         ca_pasture = ca_15,
         ca_sugar = ca_20,
         ca_mosaicAP = ca_21,
         ca_sand = ca_23,
         ca_urban = ca_24,
         ca_otherNveg = ca_25,
         ca_rocky = ca_29,
         ca_mine = ca_30,
         ca_aquacult = ca_31,
         ca_salt = ca_32,
         ca_water = ca_33,
         ca_soy = ca_39,
         ca_otherTcrop = ca_41,
         ca_coffe = ca_46,
         ca_otherPcrop = ca_48
  ) # -> tab_forest_17
%>%
  mutate(
    plot_id = plot_id,
    pland_nvc_17 = pland_forest + pland_savanna + pland_grass + pland_rocky +
      pland_mangrove + pland_wetland + pland_otherVeg + pland_salt,
    pland_agri_17 = pland_pasture + pland_mosaicAP + pland_otherPcrop +
      pland_otherTcrop + pland_sugar + pland_aquacult + pland_coffe +
      pland_Fplantation + pland_soy,
    ca_nvc_17 = ca_forest + ca_savanna + ca_grass + ca_rocky +
      ca_mangrove + ca_wetland + ca_otherVeg + ca_salt,
    ca_agri_17 = ca_pasture + ca_mosaicAP + ca_otherPcrop +
      ca_otherTcrop + ca_sugar + ca_aquacult + ca_coffe +
      ca_Fplantation + ca_soy,
    pland_urban_17 = pland_urban,
    ca_urban_17 = ca_urban,
    shdi_17 = shdi_NA,
    .keep = "none"
  ) %>%
  right_join(y= tab_1) %>% 
  glimpse -> tab_1

##variable names and type----
tab_1 %>% 
  mutate(plot_id = as.factor(plot_id)) %>% 
  left_join(y=buff_codes, by = c("plot_id" = "id_buff")) %>% 
  rename(buff_id = plot_id,
         geom_centroid_buff = geometry,
         code_muni = GEOCODIG_M,
         code_sc = CD_SETOR_m,
         code_uf = CD_UF_max) %>% 
  mutate(buff_id = as.factor(buff_id),
         code_muni = as.factor(code_muni),
         code_sc = as.factor(code_sc),
         code_uf = as.factor(code_uf)) %>% 
  glimpse -> tab_1

##Family agriculture----
table_agrifamiliar_2006 %>% 
  select(-2) %>%
  mutate (code_muni = as.factor(code_muni),
          perc_agrifam_06 = (familyAgri/total_estab)*100,
          perc_N_agrifam_06 = (Non_familyAgri/total_estab)*100) %>%
  rename(total_estab_06 = total_estab,
         N_agrifam_06 = Non_familyAgri,
         agrifam_06 = familyAgri) %>%
  left_join(y = table_agrifamiliar_area_2006) %>%
  select(-name_muni) %>%
  mutate(perc_area_agrifam_06 = (as.double(familyAgri_area)/area_estab)*100,
         perc_area_N_agrifam_06 = (Non_familyAgri_area/area_estab)*100
         ) %>%
  rename(area_estab_06 = area_estab,
         N_agrifam_area_06 = Non_familyAgri_area,
         agrifam_area_06 = familyAgri_area) %>%
  left_join(y= table_agrifamiliar_2017) %>%
  filter(!is.na(name_muni)) %>% 
  select(-name_muni) %>%
  mutate (perc_agrifam_17 = (total_agri_familiar_17/total_estab_17)*100,
          perc_N_agrifam_17 = (total_agri_Nfamiliar_17/total_estab_17)*100) %>%
  rename(N_agrifam_17 = total_agri_Nfamiliar_17,
         agrifam_17 = total_agri_familiar_17) %>%
  left_join(y= table_agrifamiliar_area_2017) %>%
  select(-name_muni) %>%
  mutate(perc_area_agrifam_17 = (area_agri_familiar_17/area_estab_17)*100,
         perc_area_N_agrifam_17 = (area_agri_Nfamiliar_17/area_estab_17)*100) %>% 
  rename(N_agrifam_area_17 = area_agri_Nfamiliar_17 ,
         agrifam_area_17 = area_agri_familiar_17) %>%
  glimpse -> tab_agrifam

tab_1 %>% 
  left_join(y = tab_agrifam) %>% 
  select(-N_agrifam_06, -perc_N_agrifam_06, -N_agrifam_area_06, -perc_area_N_agrifam_06,
         -N_agrifam_17, -perc_N_agrifam_17, -N_agrifam_area_17, -perc_area_N_agrifam_17) %>% 
  glimpse -> tab_1

pop_rural_2006 %>% 
  select(id_buff, popsum) %>% 
  rename(buff_id = id_buff,
         pop_rural_WP_06 = popsum,
         geom_buff = geometry) %>% 
  left_join(y = tibble(pop_rural_2017), by = c("buff_id" = "id_buff")) %>% 
  select(buff_id, pop_rural_WP_06, pop_sum, geom_buff) %>% 
  rename(pop_rural_WP_17 = pop_sum) %>% 
  glimpse -> table_pop

tab_1 %>% 
  left_join(y = table_pop) %>% 
  mutate(code_muni = as.double(code_muni)) %>% 
  left_join(y = select(br_mun, code_muni, geom)) %>% 
  rename(geom_mun = geom) %>% 
  glimpse() -> tab_1

dist_sede %>% 
  tibble() %>%
  select(-geometry, -TargetID) %>% 
  rename(buff_id = InputID,
         dist_near_sede = Distance) %>% 
  right_join(y = tab_1, by = "buff_id") %>% 
  glimpse ->tab_1

pop_urb_2006 %>% 
  select(CD_SETOR, pop_sum, CD_MUN) %>% 
  tibble() %>% 
  select(-geometry) %>% 
  rename(pop_urb_06 = pop_sum) %>% 
  left_join(pop_urb_2017, by = c("CD_SETOR", "CD_MUN")) %>% 
  select(CD_SETOR, pop_urb_06, pop_sum, CD_MUN) %>% 
  rename(pop_urb_17 = pop_sum) %>%
  group_by(CD_MUN) %>% 
  summarise(pop_urb_mun_06 = sum(pop_urb_06),
            pop_urb_mun_17 = sum(pop_urb_17)
            ) %>% 
  glimpse -> pop_urb

tab_1 %>% 
  mutate(code_muni = as.factor(code_muni)) %>% 
  left_join(y = pop_urb, by = c("code_muni" = "CD_MUN")) %>% 
  glimpse -> tab_1

tab_1 %>%
  mutate(vari_perc_nvc = pland_nvc_17 - pland_nvc_06,
         vari_area_nvc = ca_nvc_17 - ca_nvc_06,
         vari_total_pop_rural = pop_rural_WP_17 - pop_rural_WP_06,
         vari_perc_pop_rural = (pop_rural_WP_17/pop_rural_WP_06)*100-100,
         vari_estab_agrifam = agrifam_17 - agrifam_06,
         vari_perc_estab_agrifam= perc_agrifam_17 - perc_agrifam_06,
         vari_area_agrifam = area_estab_17 - area_estab_06,
         vari_perc_area_agrifam = perc_area_agrifam_17 - perc_area_agrifam_06,
         vari_total_pop_urb = pop_urb_mun_17 - pop_urb_mun_06,
         vari_perc_pop_urb = (pop_urb_mun_17/pop_urb_mun_06)*100-100
           ) %>% 
  glimpse ->tab_1

tab_1 %>%
  mutate(cat_change = if_else(condition = vari_perc_nvc==0 | vari_perc_pop_rural==0,
                              true = "estable",
                              false = if_else(condition = vari_perc_nvc<0 & vari_perc_pop_rural<0,
                                              true = "PP",
                                              false = if_else(condition =  vari_perc_nvc<0 & vari_perc_pop_rural>0,
                                                              true = "PG",
                                                              false = if_else(vari_perc_nvc>0 & vari_perc_pop_rural>0,
                                                                              true = "GG",
                                                                              false = "GP"
                                                                              )
                                                              )
                                              )
                              )
         ) %>% 
  glimpse ->tab_1


mean(tab_1$vari_perc_nvc) + sd(tab_1$vari_perc_nvc)
mean(tab_1$vari_perc_nvc) - sd(tab_1$vari_perc_nvc)

tab_1 %>% 
  mutate(sd_cat = if_else(condition = vari_perc_nvc > 8.849359, true = "outlier",
                          false = if_else(vari_perc_nvc < -7.384349,
                                          true = "outlier",
                                          false = "non-outlier"))) %>% 
  glimpse -> tab_1

tab_1 %>% 
  select(-geom_centroid_buff, -geom_buff, - geom_mun) %>% 
  glimpse -> tab_1

mean(tab_1$vari_perc_pop_rural, na.rm=T) + sd(tab_1$vari_perc_pop_rural, na.rm=T)
mean(tab_1$vari_perc_pop_rural, na.rm=T) - sd(tab_1$vari_perc_pop_rural, na.rm=T)

tab_1 %>% 
  mutate(pop_outlier = if_else(condition = vari_perc_pop_rural > 51.09549,
                               true = "outlier",
                          false = if_else(vari_perc_pop_rural < -29.51439,
                                          true = "outlier",
                                          false = "non-outlier"))) %>% 
  glimpse-> tab_1

rename(tab_1, nvc_outlier = sd_cat) %>% glimpse -> tab_1

tab_1 %>% 
  mutate(vari_shdi = shdi_17 - shdi_06) %>% 
  glimpse -> tab_1

#export----
write.csv(x = tab_1, file = here("tabela_geral.csv"))
