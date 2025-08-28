# Tue May 17 09:44:32 2022 ------------------------------
#Script para calcular estimativas (area, pessoas, floresta) 

#library----
library(here)
library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggbump)
library(scales)
library(ggpubr)

#data----
read.csv(file = here("data/tabela_geral.csv"))-> tab_geral
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/pop_sc_caat_2022_5880.gpkg") -> pop_sc_2022

#cáculos----
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

##n de paisagens
tabela_buffer_nova %>% 
  filter(perc_forest_2022 >= 20) %>%
  summarise(n_paisagens = n_distinct(id)) %>%
  glimpse

##number of FPP----
tabela_buffer_nova %>% 
  filter(perc_forest_2022 >= 20) %>% 
  summarise(fpp_10 = sum(fpp_2010, na.rm = T),
            fpp_22 = sum(fpp_2022, na.rm = T),
            abs_change = fpp_22 - fpp_10,
            mean_fpp_10 = mean(fpp_2010, na.rm = T),
            sd_fpp_10 = sd(fpp_2010, na.rm = T),
            mean_perc_forest_10 = mean(perc_forest_2010),
            sd_perc_fores_10 = sd(perc_forest_2010),
            mean_fpp_22 = mean(fpp_2022),
            sd_fpp_22 = sd(fpp_2022),
            mean_perc_forest_22 = mean(perc_forest_2022),
            sd_perc_forest_22 = sd(perc_forest_2022)
            ) %>%
  glimpse -> fpp_buffs

((fpp_buffs$fpp_17*92.81447)/49.41354) - ((fpp_buffs$fpp_06*92.81447)/49.41354)
#(fpp_buffs$fpp_17 - fpp in 2017
#92.81447 - expected percentage of total area in caatinga with at least 20% of forest cover
#49.41354 - percentage of area in caatinga known to have at least 20% of forest cover
#fpp_buffs$fpp_06 - fpp in 2006

##area----
tab_geral %>% 
  group_by(code_muni) %>% 
  summarise(mean_nvc = mean(pland_nvc_17),
            mun_area_ha = mean(mun_area_ha)) %>%
  filter(mean_nvc >= 20) %>%
  na.omit %>% 
  glimpse ->tab_mun_area

(((pi*5^2)*100)*5748*100)/sum(unique(tab_mun_area$mun_area_ha))
#((pi*5^2)*100) - Area of one buff in hectares
# 5748 - número de buffers com mais de 20% de cobertura 
# 100 - 100% (regra de três)
# sum(unique(tab_geral$mun_area_ha[-3867])) - Sum of area of caatinga municipalities

##forest----
(sum(tab_geral$ca_nvc_17)*100)/(50828383+4159234)
#(sum(tab_geral$ca_nvc_17)*100) - Área em hectares de vegetação nativa dentro dos buffers
#(50828383+4159234) - total de área de vegetação nativa na Caatinga de acordo com o Mapbiomas (2017)

tab_geral %>% 
  filter(pland_nvc_17 >= 20) %>% 
  select(buff_id, ca_nvc_06, ca_nvc_17) %>% 
  mutate(nvc_change = if_else(ca_nvc_06 > ca_nvc_17, true = "def", false = "reg"),
         net_change = ca_nvc_17 - ca_nvc_06) %>% 
  group_by(nvc_change) %>%
  summarise(perc_group = (n()/5752)*100,
            net_change_total = sum(net_change)) %>%
  glimpse

max(tab_geral$ca_nvc_06)

##change in forest and people by category of change----
tab_geral %>% 
  filter(pland_nvc_06 >= 20) %>% 
  group_by(cat_change) %>% 
  summarise(n = n(),
            mean_perc_change_fpp = mean(vari_perc_pop_rural, na.rm = T),
            mean_perc_change_nvc = mean(vari_perc_nvc, na.rm=T)) %>% 
  glimpse

##Percentage of landscapes in each category----
tab_geral %>% 
  filter(pland_nvc_06 >= 20, 
         vari_perc_pop_rural > 0) %>% 
  glimpse

##changes per state----
tab_geral %>% 
  mutate(pland_nvc_17 = pland_nvc_06 + vari_perc_nvc,
         buff_id = as.factor(buff_id)) %>%
  select(buff_id, code_uf, pland_nvc_17, pop_rural_WP_17) %>% 
  glimpse -> tab_fpp_state

##Municipalities ----
###Cumulative from 10 to 100----
list()-> list_fpp_state
for(i in seq(10,100,10)){
  tab_fpp_state %>%
    filter(pland_nvc_17 > i) %>%
    group_by(code_uf) %>%
    summarise(n_buff = n(),
              sum_fpp = sum(pop_rural_WP_17)) %>%
    glimpse -> a
  a -> list_fpp_state[[i]]
}

# for (i in seq(10,100,10)) {
#   list_fpp_state[[i]] -> get(paste0("fpp_state_", i))
# }

list_fpp_state[[10]]-> fpp_state_10
list_fpp_state[[20]]-> fpp_state_20
list_fpp_state[[30]]-> fpp_state_30
list_fpp_state[[40]]-> fpp_state_40
list_fpp_state[[40]]-> fpp_state_40
list_fpp_state[[50]]-> fpp_state_50
list_fpp_state[[60]]-> fpp_state_60
list_fpp_state[[70]]-> fpp_state_70
list_fpp_state[[80]]-> fpp_state_80
list_fpp_state[[90]]-> fpp_state_90
list_fpp_state[[100]]-> fpp_state_100

fpp_state_10 %>%
  select(-n_buff) %>%
  left_join(select(fpp_state_20, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_10 = sum_fpp.x,
         sum_fpp_20 = sum_fpp.y) %>%
  left_join(select(fpp_state_30, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_30 = sum_fpp) %>%
  left_join(select(fpp_state_40, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_40 = sum_fpp) %>%
  left_join(select(fpp_state_50, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_50 = sum_fpp) %>%
  left_join(select(fpp_state_60, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_60 = sum_fpp) %>%
  left_join(select(fpp_state_70, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_70 = sum_fpp) %>%
  left_join(select(fpp_state_80, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_80 = sum_fpp) %>%
  left_join(select(fpp_state_90, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_90 = sum_fpp) %>%
  left_join(select(fpp_state_100, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_100 = sum_fpp) %>%
  glimpse -> fpp_state_all

####Figure----
fpp_state_all[-10,]%>%
  replace(is.na(.), 0) %>%
  pivot_longer(cols = 2:11,
               names_sep = "fpp_",
               names_to = c(".value", "nvc_thresh")) %>%
  mutate(nvc_thresh = as.numeric(nvc_thresh),
         code_uf = as.factor(code_uf))%>%
  glimpse -> fpp_state_all_long

# fpp_state_all_long %>%
# ggplot()+
#   geom_line(aes(x = nvc_thresh, y = mean_fpp, color = code_uf))


# ###from 100 to 10----
# list()-> list_fpp_state
# for(i in seq(0,90,10)){
#   tab_fpp_state %>% 
#     filter(pland_nvc_17 >i+10) %>% 
#     group_by(code_uf) %>% 
#     summarise(n_buff = n(),
#               mean_fpp = mean(pop_rural_WP_17),
#               sd_fpp = sd(pop_rural_WP_17)) %>% 
#     glimpse -> a
#   a -> list_fpp_state[[i+10]]
# }
# 
# # for (i in seq(10,100,10)) {
# #   list_fpp_state[[i]] -> get(paste0("fpp_state_", i))
# # }
# 
# list_fpp_state[[10]]-> fpp_state_10
# list_fpp_state[[20]]-> fpp_state_20
# list_fpp_state[[30]]-> fpp_state_30
# list_fpp_state[[40]]-> fpp_state_40
# list_fpp_state[[40]]-> fpp_state_40
# list_fpp_state[[50]]-> fpp_state_50
# list_fpp_state[[60]]-> fpp_state_60
# list_fpp_state[[70]]-> fpp_state_70
# list_fpp_state[[80]]-> fpp_state_80
# list_fpp_state[[90]]-> fpp_state_90
# list_fpp_state[[100]]-> fpp_state_100
# 
# fpp_state_10 %>% 
#   select(-n_buff) %>% 
#   right_join(select(fpp_state_20, -n_buff), by = "code_uf") %>% 
#   rename(mean_fpp_10 = mean_fpp.x,
#          mean_fpp_20 = mean_fpp.y,
#          sd_fpp_10 = sd_fpp.x,
#          sd_fpp_20 = sd_fpp.y) %>% 
#   right_join(select(fpp_state_30, -n_buff), by = "code_uf") %>%
#   rename(mean_fpp_30 = mean_fpp,
#          sd_fpp_30 = sd_fpp) %>%
#   # right_join(select(fpp_state_40, code_uf, mean_fpp), by = "code_uf") %>% 
#   # rename(mean_fpp_40 = mean_fpp) %>% 
#   # right_join(select(fpp_state_50, code_uf, mean_fpp), by = "code_uf") %>% 
#   # rename(mean_fpp_50 = mean_fpp) %>% 
#   # right_join(select(fpp_state_60, code_uf, mean_fpp), by = "code_uf") %>% 
#   # rename(mean_fpp_60 = mean_fpp) %>% 
#   # right_join(select(fpp_state_70, code_uf, mean_fpp), by = "code_uf") %>% 
#   # rename(mean_fpp_70 = mean_fpp) %>% 
#   # right_join(select(fpp_state_80, code_uf, mean_fpp), by = "code_uf") %>% 
#   # rename(mean_fpp_80 = mean_fpp) %>% 
#   # right_join(select(fpp_state_90, code_uf, mean_fpp), by = "code_uf") %>% 
# # rename(mean_fpp_90 = mean_fpp) %>% 
# # right_join(select(fpp_state_100, code_uf, mean_fpp), by = "code_uf") %>% 
# # rename(mean_fpp_100 = mean_fpp) %>% 
# glimpse #-> fpp_state_all
# 
# ####Figure----
# cols <- c("#FFB400", "#FFC740", "#C20008", "#FF020D", "#13AFEF", "black", "purple", "green", "darkred")
# 
# fpp_state_all [-10,]%>% 
#   replace(is.na(.), 0) %>% 
#   pivot_longer(cols = 2:11, names_prefix = "mean_fpp_", names_to = "nvc_thresh", values_to = "mean_fpp") %>%
#   mutate(nvc_thresh = as.numeric(nvc_thresh),
#          code_uf = as.factor(code_uf))%>%
#   glimpse -> fpp_state_all_long
# 
# fpp_state_all_long %>%
#   ggplot()+
#   geom_line(aes(x = nvc_thresh, y = mean_fpp, color = code_uf))

###cumulative population density----
list()-> list_fpp_dens
for(i in seq(0,90,10)){
  tab_fpp_state %>% 
    dplyr:::filter(pland_nvc_17 >= i+10) %>% 
  group_by(code_uf) %>% 
  summarise(n_buff = n(),
            area_buffs = n_buff * 78.539816,
            sum_fpp = sum(pop_rural_WP_17),
            fpp_dens = sum_fpp/area_buffs) %>% 
  glimpse -> list_fpp_dens [[i+10]]
}


list_fpp_dens[[10]]-> fpp_dens_10
list_fpp_dens[[20]]-> fpp_dens_20
list_fpp_dens[[30]]-> fpp_dens_30
list_fpp_dens[[40]]-> fpp_dens_40
list_fpp_dens[[40]]-> fpp_dens_40
list_fpp_dens[[50]]-> fpp_dens_50
list_fpp_dens[[60]]-> fpp_dens_60
list_fpp_dens[[70]]-> fpp_dens_70
list_fpp_dens[[80]]-> fpp_dens_80
list_fpp_dens[[90]]-> fpp_dens_90
list_fpp_dens[[100]]-> fpp_dens_100

fpp_dens_10 %>% 
  full_join(select(fpp_dens_20, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_10 = fpp_dens.x, fpp_dens_20 = fpp_dens.y) %>% 
  full_join(select(fpp_dens_30, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_30 = fpp_dens) %>% 
  full_join(select(fpp_dens_40, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_40 = fpp_dens) %>% 
  full_join(select(fpp_dens_50, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_50 = fpp_dens) %>% 
  full_join(select(fpp_dens_60, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_60 = fpp_dens) %>% 
  full_join(select(fpp_dens_70, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_70 = fpp_dens) %>%
  full_join(select(fpp_dens_80, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_80 = fpp_dens) %>% 
  full_join(select(fpp_dens_90, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_90 = fpp_dens) %>% 
  full_join(select(fpp_dens_100, code_uf, fpp_dens), by = "code_uf") %>% 
  rename(fpp_dens_100 = fpp_dens) %>% 
  glimpse -> fpp_dens_all

fpp_dens_all[-10,]%>% 
  replace(is.na(.), 0) %>%
  pivot_longer(cols = 5:14, names_prefix = "fpp_dens_", names_to = "nvc_thresh", values_to = "fpp_dens") %>%
  mutate(nvc_thresh = as.numeric(nvc_thresh),
         code_uf = as.factor(code_uf))%>%
  glimpse -> fpp_dens_all_long

fpp_dens_all_long %>%
  ggplot()+
  geom_bump(aes(x = nvc_thresh, y = fpp_dens, color = code_uf), linewidth = 1.5)+
  geom_segment(aes(x = 0, xend = 20, y = 35.393077, yend = 35.393077), linetype = "dashed", color = "lightgrey")+
  geom_segment(aes(x = 20, xend = 20, y = 0, yend = 35.393077), linetype = "dashed", color = "lightgrey")+
  geom_segment(aes(x = 0, xend = 50, y = 38.675893, yend = 38.675893), linetype = "dashed", color = "lightgrey")+
  geom_segment(aes(x = 50, xend = 50, y = 0, yend = 38.675893), linetype = "dashed", color = "lightgrey")+
  geom_segment(aes(x = 0, xend = 70, y = 37.193919, yend = 37.193919), linetype = "dashed", color = "lightgrey")+
  geom_segment(aes(x = 70, xend = 70, y = 0, yend = 37.193919), linetype = "dashed", color = "lightgrey")+
  labs(x = "Forest cover threshold (%)", y = "FPP density (n/km²)")+
  scale_x_continuous(breaks = c(0, 10,20,30,40,50,60,70,80,90,100), expand = c(0,0))+
  scale_y_continuous(breaks = c(0, 10, 20, 30, 35.393077, 37.193919, 38.675893, 40),
                     labels = c(0, 10, 20, 30, 35.3, 37.1, 38.6, 40),
                     expand = c(0,0))+
  scale_color_brewer(type = "qual", palette = "Set1", 
                     labels = c("Piauí", "Ceará", "Rio Grande do Norte",
                                  "Paraíba", "Pernambuco", "Alagoas", "Sergipe",
                                  "Bahia", "Minas Gerais"),
                     name = "State")+
  theme_classic()->fpp_dens_state

###Cumulative population total----
###Cumulative from 10 to 100----
list()-> list_fpp_state
for(i in seq(10,100,10)){
  tab_fpp_state %>%
    filter(pland_nvc_17 > i) %>%
    group_by(code_uf) %>%
    summarise(n_buff = n(),
              sum_fpp = sum(pop_rural_WP_17)) %>%
    glimpse -> a
  a -> list_fpp_state[[i]]
}

# for (i in seq(10,100,10)) {
#   list_fpp_state[[i]] -> get(paste0("fpp_state_", i))
# }

list_fpp_state[[10]]-> fpp_state_10
list_fpp_state[[20]]-> fpp_state_20
list_fpp_state[[30]]-> fpp_state_30
list_fpp_state[[40]]-> fpp_state_40
list_fpp_state[[40]]-> fpp_state_40
list_fpp_state[[50]]-> fpp_state_50
list_fpp_state[[60]]-> fpp_state_60
list_fpp_state[[70]]-> fpp_state_70
list_fpp_state[[80]]-> fpp_state_80
list_fpp_state[[90]]-> fpp_state_90
list_fpp_state[[100]]-> fpp_state_100

fpp_state_10 %>%
  select(-n_buff) %>%
  left_join(select(fpp_state_20, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_10 = sum_fpp.x,
         sum_fpp_20 = sum_fpp.y) %>%
  left_join(select(fpp_state_30, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_30 = sum_fpp) %>%
  left_join(select(fpp_state_40, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_40 = sum_fpp) %>%
  left_join(select(fpp_state_50, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_50 = sum_fpp) %>%
  left_join(select(fpp_state_60, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_60 = sum_fpp) %>%
  left_join(select(fpp_state_70, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_70 = sum_fpp) %>%
  left_join(select(fpp_state_80, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_80 = sum_fpp) %>%
  left_join(select(fpp_state_90, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_90 = sum_fpp) %>%
  left_join(select(fpp_state_100, -n_buff), by = "code_uf") %>%
  rename(sum_fpp_100 = sum_fpp) %>%
  glimpse -> fpp_state_all

fpp_state_all[-10,]%>%
  replace(is.na(.), 0) %>%
  pivot_longer(cols = 2:11,
               names_sep = "fpp_",
               names_to = c(".value", "nvc_thresh")) %>%
  mutate(nvc_thresh = as.numeric(nvc_thresh),
         code_uf = as.factor(code_uf))%>%
  glimpse -> fpp_state_all_long

fpp_state_all_long %>%
  ggplot()+
  geom_bump(aes(x = nvc_thresh, y = sum_, color = code_uf), linewidth = 1.5)+
  # geom_segment(aes(x = 0, xend = 20, y = 258196.567, yend = 258196.567), linetype = "dashed", color = "lightgrey")+
  # geom_segment(aes(x = 20, xend = 20, y = 0, yend = 258196.567), linetype = "dashed", color = "lightgrey")+
  # geom_segment(aes(x = 0, xend = 50, y = 936495.780, yend = 936495.780), linetype = "dashed", color = "lightgrey")+
  # geom_segment(aes(x = 50, xend = 50, y = 0, yend = 936495.780), linetype = "dashed", color = "lightgrey")+
  # geom_segment(aes(x = 0, xend = 70, y = 1225985.470, yend = 1225985.470), linetype = "dashed", color = "lightgrey")+
  # geom_segment(aes(x = 70, xend = 70, y = 0, yend = 1225985.470), linetype = "dashed", color = "lightgrey")+
  labs(x = "Forest cover threshold (%)", y = "Number of FPP")+
  scale_x_continuous(breaks = c(0, 10, 20,30,40,50,60,70,80,90,100))+
  scale_y_continuous(breaks = c(0, 250000, 500000, 750000, 1000000, 1250000, 1500000, 1750000, 2000000),
                       labels = comma)+
  scale_color_brewer(type = "qual", palette = "Set1", 
                     labels = c("Piauí", "Ceará", "Rio Grande do Norte",
                                "Paraíba", "Pernambuco", "Alagoas", "Sergipe",
                                "Bahia", "Minas Gerais"),
                     name = "State")+
  theme_classic() -> fpp_total_state

##Figure fpp per state----
ggarrange(fpp_total_state, fpp_dens_state,
          common.legend = F,
          legend = "right",
          labels = "auto",
          nrow = 2) %>%
  
  ggsave(filename = "img/fpp_state.jpg",
         width = 6,
         height = 8,
         bg = "white")

ggsave(plot = fpp_total_state,
       filename = here("img/fpp_state.jpg"),
       width = 6)
# #descriptive development ind----
# tab_context %>% 
#   filter(year == 2010) %>%
#   na.omit() %>% 
#   summarise(sd_agrifam = sd(agrifam),
#             sd_popurb = sd(pop_urb),
#             sd_idhl = sd(IDHM_L),
#             sd_expov = sd(expov),
#             sd_gini = sd(gini),
#             sd_u5mort = sd(u5mort)) %>% 
#   # summary()
# glimpse
