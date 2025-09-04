# Tue Sep 12 17:00:18 2023 ------------------------------
#Script to creat LISA clusters

#library----
library(sf)
library(spdep)
library(dplyr)
library(ggplot2)
library(cowplot)

#data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_analysis.gpkg") -> tab_mun_analysis

read_biomes() %>% 
  filter (name_biome == "Caatinga") -> caat_shp
read_municipality(year = 2022) -> br_mun

##organisation----
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

##spatial matrix----
poly2nb(tab_mun_analysis$geom, 
        queen=TRUE) -> mat_dist_mun_caat

nb2listw(mat_dist_mun_caat) -> mat_dist_list_mun_caat

#analysis----
##fpp change----
localmoran(x = tab_mun_analysis$mean_fpp_perc_change,
           listw = mat_dist_list_mun_caat) -> Lmoran_change_fpp

###classic lisa
mean_fpp_change <- mean(tab_mun_analysis$mean_fpp_perc_change, na.rm = TRUE)

tab_mun_analysis %>%
  mutate(
    Ii = Lmoran_change_fpp[,1],       # estatística de Moran local
    EIi = Lmoran_change_fpp[,2],      # expectativa
    VarIi = Lmoran_change_fpp[,3],    # variância
    ZIi = Lmoran_change_fpp[,4],      # valor z
    p_value = Lmoran_change_fpp[,5],   # p-valor,
    cluster_type = case_when(
      mean_fpp_perc_change >= mean_fpp_change & Ii > 0 & p_value <= 0.05 ~ "High-High",
      mean_fpp_perc_change <  mean_fpp_change & Ii > 0 & p_value <= 0.05 ~ "Low-Low",
      mean_fpp_perc_change >= mean_fpp_change & Ii < 0 & p_value <= 0.05 ~ "High-Low",
      mean_fpp_perc_change <  mean_fpp_change & Ii < 0 & p_value <= 0.05 ~ "Low-High",
      TRUE ~ "Not significant"
    )
  ) %>% 
  glimpse -> tab_mun_analysis

###lisa mean = 0
lag.listw(x = mat_dist_list_mun_caat,
          var = tab_mun_analysis$mean_fpp_perc_change) -> lag_x

# 5) p-valor (com correção para múltiplos testes – recomendado)
p_raw  <- Lmoran_change_fpp[, 5]
p_fdr  <- p.adjust(p_raw, method = "fdr")  # ou "holm"

tab_mun_analysis %>%
  mutate(
    Ii      = Lmoran_change_fpp[,1],
    z_Ii    = Lmoran_change_fpp[,4],
    p_value = p_raw,
    p_fdr   = p_fdr,
    lag_x   = lag_x,
    # Rotulagem baseada em ZERO (ganho/perda), usando significância do LISA
    cluster_zero = case_when(
      p_fdr <= 0.05 & x >  0 & lag_x >  0 ~ "High-High",
      p_fdr <= 0.05 & x <  0 & lag_x <  0 ~ "Low-Low",
      p_fdr <= 0.05 & x >  0 & lag_x <  0 ~ "High-Low",
      p_fdr <= 0.05 & x <  0 & lag_x >  0 ~ "Low-High",
      TRUE ~ "Not significant"
    )
  ) -> tab_mun_analysis


##fpp change per cluster----
tab_mun_analysis %>%
  group_by(cluster_type) %>%
  summarise(
    media_change = mean(mean_fpp_perc_change, na.rm = TRUE),
    sd_change    = sd(mean_fpp_perc_change, na.rm = TRUE),
    n_municipios = n(),
    .groups = "drop"
  ) -> resumo_cluster

# resumo geral para o bioma
tab_mun_analysis %>%
  as_tibble() %>% 
  summarise(
    cluster_type = "Bioma inteiro",
    media_change = mean(mean_fpp_perc_change, na.rm = TRUE),
    sd_change    = sd(mean_fpp_perc_change, na.rm = TRUE),
    n_municipios = n()
  ) -> resumo_bioma

# juntar os dois
bind_rows(resumo_cluster, resumo_bioma) -> resumo_final

resumo_final
