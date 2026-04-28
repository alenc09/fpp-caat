# Thu May 12 16:02:31 2022 ------------------------------
#Script para montar mapa da mudança de categoria(forest-people) por município

#library----
library(here)
library(geobr)
library(dplyr)
library(sf)
library(ggplot2)
library(ggpubr)

#data----
read.csv(file = here("data/tabela_geral.csv"))-> tab_geral
read_municipality(simplified = F)-> mun_br
read_biomes() %>% 
  filter (name_biome == "Caatinga") -> caat_shp
read_state() -> br_states


sf_use_s2(FALSE) #função para desativar a checagem de vértices duplicados
st_transform(x = mun_br, crs =5880) ->mun_br
st_transform(x = caat_shp, crs = 5880) -> caat_shp
st_transform(x = br_states, crs = 5880) -> br_states
mun_br[caat_shp,] -> mun_caat
br_states[caat_shp,] -> states_caat

#mapa----
tab_geral %>%
  group_by(code_muni) %>%
  summarise(
    mean_pop_change = mean(vari_perc_pop_rural),
    mean_nvc_change = mean(vari_perc_nvc),
    cat_change = cat_change) %>% 
  mutate(
    cat_change = if_else(
      condition = mean_nvc_change > 0 & mean_pop_change > 0,
      true = "GG",
      false = if_else(
        condition = mean_nvc_change > 0 & mean_pop_change < 0,
        true = "GP",
        false = if_else(
          condition =  mean_nvc_change < 0 & mean_pop_change > 0,
          true = "PG",
          false = if_else(
            mean_nvc_change < 0 & mean_pop_change < 0,
            true = "PP",
            false = "stable"
          )
        )
      )
    )
  ) %>%
  distinct() %>% 
  filter(cat_change != "stable") %>%
  right_join(y = mun_caat) %>% 
  glimpse -> tab_cat_change

st_as_sf(tab_cat_change) %>% 
  st_transform(crs=4674) ->tab_cat_change

tab_cat_change %>% 
ggplot() +
  geom_sf(aes(geometry = geom, fill = cat_change), linewidth = 0.2, color = "grey") +
  scale_fill_manual(
    values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"),
    name = "Forest-People",
    label = c("gain-gain", "gain-lose", "lose-gain", "lose-lose"),
    na.value = "grey90")+
  geom_sf(data = states_caat[-1,], fill="transparent", linewidth=0.3)+
  coord_sf(xlim = c(-48, -34), ylim = c(-17.1, -3))+
  geom_text(data = states_caat[-1,], aes(x= c(-42, -39.5,-36.5,-35.5, -34.5, -34.4, -36, -39,-42.4),
                                         y = c(-16.8, -15, -11, -10, -8.5, -7, -4.7, -2.9, -5),
                                         label = c("MG", "BA", "SE", "AL", "PE", "PB", "RN", "CE", "PI")),
            size = 2)+
  theme_map()+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.position = c(0.8, 0.2)) -> map_category_change

#map inset####
# #Inset map####
# ggplot()+
#   geom_sf(data = br, fill = "transparent")+
#   geom_sf(data = caat_shp, fill = "grey")+
#   geom_sf(data = br_states, fill = "transparent", lwd = 0.1)+
#   theme_map() -> inset_map

#Figure map----
source(file = here("scripts/map_fpp.R"))
ggarrange(map_fpp_change_inset, map_category_change, labels = c("a)", "b)")) -> fig_map

ggsave(plot=fig_map, filename = here("img/fig_map.jpg"),
       dpi = 300,
       bg = "white",
       width = 9)
      
source(file = here("scripts/fpp_nvc.R"))
plot_grid(pop_nvc_all, map_category_change, labels = "auto") %>% 
  ggsave(plot = ., filename = here("img/fig_catChange.jpg"),
         width = 8,
         height = 4,
         bg="white")
