# Thu May 12 15:21:40 2022 ------------------------------
#Script para montar mapa da mudança de FPP na caatinga

#library----
library(sf)
library(ggplot2)
library(cowplot)
library(geobr)
library(dplyr)
library(patchwork)

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
  geom_sf(data = mun_caat, aes(geometry = geom, fill = code_muni), fill = "grey90", color = "grey", linewidth = 0.05) +
  geom_sf(aes(geometry = geom, fill = mean_fpp_perc_change), linewidth = 0.2, color = "grey") +
  scale_fill_fermenter(
    palette = "BrBG",
    direction = 1,
    breaks = c(-75, -50, -25, -12.5, 0, 12.5, 25, 50, 75),
    name = "FPP change (%)",
    na.value = "grey90")+
  geom_sf(data = states_caat, fill="transparent", linewidth=0.2) +
  coord_sf(
    xlim = c(bbox["xmin"] - 70000, bbox["xmax"] + 100000), 
    ylim = c(bbox["ymin"] - 30000, bbox["ymax"] + 1000), 
    expand = F) +
  geom_sf_text(data = coords_estados_sf, aes(label = sigla),
            size = 2) +
  theme_map()+
  theme(legend.text = element_text(size = 5),
        legend.title = element_text(size = 6),
        legend.position.inside = c(0.6, 0.2),
        legend.key.height = unit(0.5, "cm")) -> map_fpp_change

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
            x = 0.01, y = 0.7, width = 0.30, height = 0.30)-> map_fpp_change_inset

ggsave(plot = map_fpp_change_inset, filename = "img/fig2.tiff",
       units = "in", device = "tiff", dpi = 600,
       width = 3.42, height = 3.42, bg = "white")

