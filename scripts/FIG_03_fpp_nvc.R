# Figure 3: Scatter plot and map of FPP vs forest cover change by category

# Libraries ----
library(here)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(geobr)
library(ggthemes)
library(readr)

# Data ----
read_csv(here("data/tab_buffer_analysis.csv"), show_col_types = FALSE) %>%
  mutate(cat_change = as.factor(cat_change)) -> tab_buff_analysis

read_csv(here("data/tab_mun_analysis.csv"), show_col_types = FALSE) %>%
  mutate(cat_change = as.factor(cat_change)) -> tab_mun_data

read_biomes(year = 2019) %>% filter(name_biome == "Caatinga") -> caat_shp
read_state(year = 2020) -> br_states
read_municipality(year = 2020) -> br_mun

br_mun %>%
  inner_join(tab_mun_data, by = c("code_muni" = "code_mun")) -> tab_mun_analysis

# Data organisation ----
tab_buff_analysis %>%
  mutate(forest_perc_change = if_else(condition = forest_perc_change > 100,
                                           true = 100,
                                           false = forest_perc_change)) %>%
  filter(cat_change != "stable") %>%
  filter(fpp_perc_change != Inf) %>%
  filter(!is.na(cat_change)) %>%
  filter(perc_forest_2022 >= 20) -> tab_buff_analysis

tab_mun_analysis %>%
  mutate(mean_forest_perc_change = if_else(condition = mean_forest_perc_change > 100,
                                      true = 100,
                                      false = mean_forest_perc_change)) %>%
  filter(cat_change != "stable") -> tab_mun_analysis

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

coords_estados_sf <- st_as_sf(coords_estados, coords = c("x", "y"), crs = 4326) %>%
  st_transform(5880)

bbox <- st_bbox(tab_mun_analysis)

# Scatter plot ----
tab_buff_analysis %>%
  ggplot() +
  geom_point(aes(x = forest_perc_change, y = fpp_perc_change,
                 colour = cat_change), alpha = 0.3, stroke = 0, size = 2) +
  ylim(-100, 200) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  annotate(geom = "text", label = "n = 628", x = 60, y = 150, color = "#018571", fontface = "bold", size = 2)+
  annotate(geom = "text", label = "n = 1853", x = 60, y = -80, color = "#80cdc1", fontface = "bold", size = 2)+
  annotate(geom = "text", label = "n = 1144", x = -60, y = 150, color = "#dfc27d", fontface = "bold", size = 2)+
  annotate(geom = "text", label = "n = 3605", x = -60, y = -80, color = "#a6611a", fontface = "bold", size = 2)+
  xlab("Change in forest cover (%)") +
  ylab("Change in FPP (%)") +
  scale_color_manual(values = c("#018571", "#80cdc1", "#dfc27d", "#a6611a"))+
  theme_classic(base_size = 10)+
  theme(legend.position = "none",
        panel.background = element_rect(color = "white")) -> fpp_forest_all

# Map ----
ggplot(tab_mun_analysis) +
  geom_sf(data = mun_caat, aes(geometry = geom, fill = "code_muni"), fill = "grey90", color = "grey", linewidth = 0.1) +
  geom_sf(aes(fill = cat_change), linewidth = 0, color = "grey") +
  scale_fill_manual(values = c("GG" = "#018571",
                               "GP" = "#80cdc1",
                               "PG" = "#dfc27d",
                               "PP" = "#a6611a"),
                    labels = c("GG" = "gain-gain",
                               "GP" = "gain-lose",
                               "PG" = "lose-gain",
                               "PP" = "lose-lose"),
                    name = "Forest-People") +
  geom_sf(data = states_caat, fill="transparent", linewidth=0.3) +
  coord_sf(
    xlim = c(bbox["xmin"] - 70000, bbox["xmax"] + 100000),
    ylim = c(bbox["ymin"] - 30000, bbox["ymax"] + 1000),
    expand = F) +
  geom_sf_text(data = coords_estados_sf, aes(label = sigla),
               size = 2) +
  theme_map()+
  theme(legend.text = element_text(size = 5),
        legend.title = element_text(size = 6),
        legend.position = c(0.9, 0.15)) -> map_forest_fpp

# Save ----
fpp_forest_all + map_forest_fpp +
  plot_layout(widths = c(0.8, 1)) +
  plot_annotation() -> fig_3

ggsave(plot = fig_3, filename = "img/fig3.tiff",
       units = "in", device = "tiff", dpi = 600, width = 6, height = 3)
