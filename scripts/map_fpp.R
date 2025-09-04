# Thu May 12 15:21:40 2022 ------------------------------
#Script para montar mapa da mudança de FPP na caatinga

#library----
library(sf)
library(ggplot2)
library(cowplot)

library(geobr)
library(dplyr)




#data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_analysis.gpkg") -> tab_mun_analysis

read_biomes() %>% 
  filter (name_biome == "Caatinga") -> caat_shp
read_state(year = 2020) -> br_states
read_municipality(year = 2020) -> br_mun
read_country() -> br

st_transform(x = tab_mun_analysis, crs = 5880) -> tab_mun_analysis
st_transform(x = caat_shp, crs = 5880) -> caat_shp
st_transform(x = br_states, crs = 5880) -> br_states
st_transform(x = br_mun, crs = 5880) -> br_mun
br_states[caat_shp,] -> states_caat
br_mun[caat_shp,] -> mun_caat

coords_estados <- data.frame(
  sigla = c("MG", "BA", "SE", "AL", "PE", "PB", "RN", "CE", "PI"),
  x = c(-42, -39.5, -36.5, -35.5, -34.5, -34.4, -36, -39, -42.4),
  y = c(-16.8, -15, -11, -10, -8.5, -7, -4.7, -2.9, -5)
)

# 2. Transformar em sf no CRS geográfico
coords_estados_sf <- st_as_sf(
  coords_estados,
  coords = c("x", "y"),
  crs = 4674 # SIRGAS 2000 em graus
)

# 3. Reprojetar para o CRS do mun_caat (EPSG:5880)
coords_estados_sf <- st_transform(coords_estados_sf, st_crs(mun_caat))

#maps----
## map fpp change----
bbox <- st_bbox(mun_caat)

tab_mun_analysis %>% 
ggplot() +
  geom_sf(data = mun_caat, aes(geometry = geom, fill = code_muni), fill = "grey90", color = "grey", linewidth = 0.2) +
  geom_sf(aes(geometry = geom, fill = mean_fpp_perc_change), linewidth = 0.2, color = "grey") +
  scale_fill_fermenter(
    palette = "BrBG",
    direction = 1,
    breaks = c(-75, -50, -25, -12.5, 0, 12.5, 25, 50, 75),
    name = "FPP change (%)",
    na.value = "grey90")+
  geom_sf(data = states_caat, fill="transparent", linewidth=0.3) +
  coord_sf(
    xlim = c(bbox["xmin"] - 70000, bbox["xmax"] + 100000), 
    ylim = c(bbox["ymin"] - 30000, bbox["ymax"] + 1000), 
    expand = F) +
  geom_sf_text(data = coords_estados_sf, aes(label = sigla),
            size = 3) +
  theme_map()+
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.position = c(0.9, 0.2),
        legend.key.height = unit(0.8, "cm")) -> map_fpp_change

##map lisa----
source(here("scripts/LISA.R"))

###lisa classic
ggplot(tab_mun_analysis) +
  geom_sf(data = mun_caat, aes(geometry = geom, fill = "code_muni"), fill = "grey90", color = "grey", linewidth = 0.2) +
  geom_sf(aes(fill = cluster_type), linewidth = 0.1, color = "grey") +
  scale_fill_manual(values = c("High-High" = "#018571",
                               "High-Low" = "#80cdc1",
                               "Low-High" = "#dfc27d",
                               "Low-Low" = "#a6611a",
                               "Not significant" = "grey70"),
                    name = "Clusters of\n FPP change") +
  geom_sf(data = states_caat, fill="transparent", linewidth=0.3) +
  coord_sf(
    xlim = c(bbox["xmin"] - 70000, bbox["xmax"] + 100000), 
    ylim = c(bbox["ymin"] - 30000, bbox["ymax"] + 1000), 
    expand = F) +
  geom_sf_text(data = coords_estados_sf, aes(label = sigla),
               size = 3) +
  theme_map()+
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.position = c(0.8, 0.2)) -> map_lisa_classic

###lisa mean = 0
# ggplot(tab_mun_analysis) +
#   geom_sf(data = mun_caat, aes(geometry = geom, fill = "code_muni"), fill = "grey90", color = "grey", linewidth = 0.2) +
#   geom_sf(aes(fill = cluster_zero), linewidth = 0.1, color = "grey") +
#   scale_fill_manual(values = c("High-High" = "#018571",
#                                "High-Low" = "#80cdc1",
#                                "Low-High" = "#dfc27d",
#                                "Low-Low" = "#a6611a",
#                                "Not significant" = "grey70"),
#                     name = "Clusters of\n FPP change") +
#   geom_sf(data = states_caat, fill="transparent", linewidth=0.3) +
#   coord_sf(xlim = st_bbox(mun_caat)[c("xmin", "xmax")],
#            ylim = st_bbox(mun_caat)[c("ymin", "ymax")],
#            expand = T) +
#   geom_sf_text(data = coords_estados_sf, aes(label = sigla),
#                size = 3) +
#   theme_map()+
#   theme(legend.text = element_text(size = 10),
#         legend.title = element_text(size = 11),
#         legend.position = c(0.8, 0.2)) -> map_lisa_0

##Inset map----
ggplot()+
  geom_sf(data = br, fill = "transparent")+
  geom_sf(data = caat_shp, aes(geometry = geom), linewidth = 0, fill = "darkgrey")+
  geom_sf(data = br_states, fill = "transparent", lwd = 0.1)+
  theme_map() -> inset_map

#Figure 2----
ggdraw()+
  draw_plot(map_fpp_change)+
  draw_plot(inset_map,
            x = 0.01, y = 0.65, width = 0.40, height = 0.40)-> map_fpp_change_inset

plot_grid(map_fpp_change_inset,
          map_lisa_classic,
          labels = "auto") -> fpp_change_map

# ggsave(plot = fpp_change_map, filename = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/outros_trampos/Manuscritos/FPP_caat/manuscript/PNAS/PNAS_lucas_resubmissao/Fig_2.jpg",
#        dpi = 300,
#        bg = "white",
#        width = 10,
#        height = 5)
