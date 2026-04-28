buffers <- tabela_buffer_nova %>% 
  rename(geom_buffer = geom) %>% 
  mutate(
    area_m2 = as.numeric(st_area(geom_buffer)),
    area_km2 = area_m2/1e6,
    forest_change = perc_forest_2022 - perc_forest_2010
  ) %>% 
  filter(
    !is.na(forest_change),
    !is.na(perc_forest_2022)
  )

sample_ge20 <- buffers %>% filter(perc_forest_2022 >= 20)

change_sample  <- sum(sample_ge20$forest_change * sample_ge20$area_km2, na.rm=TRUE)
area_sample_km2 <- sum(sample_ge20$area_km2, na.rm=TRUE)

dens_change_hat <- change_sample / area_sample_km2

area_bioma_km2 <- caat_shape_5880 %>% 
  mutate(area_km2 = area_m2/1e6) %>% 
  pull(area_km2)

prop_ge20 <- mean(buffers$perc_forest_2022 >= 20, na.rm = TRUE)

area_ge20_bioma_km2 <- prop_ge20 * area_bioma_km2

forest_change_est_ge20 <- dens_change_hat * area_ge20_bioma_km2

################################
library(raster)
library(sf)
library(exactextractr)
library(future.apply)
library(tidyr)

raster("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/gis/mapbiomas_caatinga_2010_5880.tif") -> caatinga_lc_2010
# raster("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/gis/mapbiomas_caatinga_2011_5880.tif") -> caatinga_lc_2011
# raster("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/gis/mapbiomas_caatinga_2017_5880.tif") -> caatinga_lc_2017
# raster("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/gis/mapbiomas_caatinga_2018_5880.tif") -> caatinga_lc_2018
raster("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/gis/mapbiomas_caatinga_2022_5880.tif") -> caatinga_lc_2022
# raster("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/gis/mapbiomas_caatinga_2023_5880.tif") -> caatinga_lc_2023

read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/buffers_municipio_5880.shp") -> buffers_mun

#organization----
raster_list <- list(
  caatinga_lc_2010,
  caatinga_lc_2022)

names(raster_list) <- c("2010","2022")
fun_area34 <- function(values, coverage_fractions, pixel_area_ha = 0.09) {
  
  # selecionar apenas classes de interesse
  mask <- values %in% c(3, 4)
  if (all(!mask) || all(is.na(mask))) return(0)
  
  # área ponderada = soma(frac * área_pixel)
  area_ha <- sum(coverage_fractions[mask] * pixel_area_ha, na.rm = TRUE)
  
  return(area_ha)
}

future::plan(future::multisession, workers = min(length(raster_list), future::availableCores()-1))

results_area <- future.apply::future_lapply(names(raster_list), function(yr) {
  
  r <- raster_list[[yr]]
  
  v <- exactextractr::exact_extract(
    r,
    buffers,
    fun = fun_area34,
    progress = TRUE
  )
  
  data.frame(id = buffers$id, year = yr, area_ha = unlist(v))
  
}, future.seed = TRUE)

results_area_df <- do.call(rbind, results_area)
results_area_wide <- results_area_df %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = area_ha,
    names_prefix = "forest_area_"
  )

results_area_wide %>%
  left_join(    y = buffers_mun %>% mutate(id = as.character(id)),
                by = "id"
  ) %>%
  st_drop_geometry() %>%
  mutate(code_mun = as.factor(CD_MUN), .keep = "unused") %>%
  dplyr::select(-geometry) %>%
  glimpse() -> results_area_wide

# --- Preparação dos buffers ---
buffers_area <- results_area_wide %>% 
  mutate(
    forest_change_ha = forest_area_2022 - forest_area_2010
  ) %>% 
  left_join(
    tabela_buffer_nova %>% 
      st_drop_geometry() %>% 
      dplyr::select(id, perc_forest_2022),
    by = "id"
  ) %>% 
  filter(!is.na(forest_change_ha), !is.na(perc_forest_2022))

# --- Seleção dos buffers com ≥ 20% floresta em 2022 ---
sample_ge20 <- buffers_area %>%
  filter(perc_forest_2022 >= 20)

# --- Mudança total de floresta dentro do sample (ha) ---
change_sample_ha <- sum(sample_ge20$forest_change_ha, na.rm = TRUE)

# --- Área total do sample (km²) ---
buffers_area <- buffers_area %>% 
  left_join(
    tabela_buffer_nova %>% 
      mutate(area_km2 = as.numeric(st_area(geom)) / 1e6) %>% 
      st_drop_geometry() %>% 
      dplyr::select(id, area_km2),
    by = "id"
  )

area_sample_km2 <- sum(sample_ge20$area_km2, na.rm = TRUE)

# --- Densidade de mudança por km² ---
dens_change_hat_ha_per_km2 <- change_sample_ha / area_sample_km2

# --- Área total do bioma (km²) ---
area_bioma_km2 <- caat_shape_5880 %>%
  mutate(area_km2 = area_m2 / 1e6) %>%
  pull(area_km2)

# --- Proporção de buffers ≥20% floresta ---
prop_ge20 <- mean(buffers$perc_forest_2022 >= 20, na.rm = TRUE)

# --- Área estimada ≥20% floresta no bioma (km²) ---
area_ge20_bioma_km2 <- prop_ge20 * area_bioma_km2

# --- ✅ Mudança total estimada para toda a Caatinga (em hectares) ---
forest_change_est_ge20_ha <- dens_change_hat_ha_per_km2 * area_ge20_bioma_km2
forest_change_est_ge20_ha
