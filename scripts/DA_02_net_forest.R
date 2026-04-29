# Script to estimate net forest area change (ha) in the Caatinga biome
# using MapBiomas rasters and landscape-scale extrapolation

#Libraries----
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(exactextractr)
library(future.apply)

#Data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tabela_buffer_nova.gpkg") -> tabela_buffer_nova
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/caat_shape_5880.shp") -> caat_shape_5880
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/buffers_municipio_5880.shp") -> buffers_mun

raster("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/gis/mapbiomas_caatinga_2010_5880.tif") -> caatinga_lc_2010
raster("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/gis/mapbiomas_caatinga_2022_5880.tif") -> caatinga_lc_2022

#Organisation----
tabela_buffer_nova %>%
  rename(geom_buffer = geom) %>%
  mutate(
    area_m2  = as.numeric(st_area(geom_buffer)),
    area_km2 = area_m2 / 1e6
  ) %>%
  filter(!is.na(perc_forest_2022)) -> buffers

raster_list <- list("2010" = caatinga_lc_2010, "2022" = caatinga_lc_2022)

fun_area34 <- function(values, coverage_fractions, pixel_area_ha = 0.09) {
  mask <- values %in% c(3, 4)
  if (all(!mask) || all(is.na(mask))) return(0)
  sum(coverage_fractions[mask] * pixel_area_ha, na.rm = TRUE)
}

#Analysis----
##Extract forest area (ha) per buffer for each year
future::plan(future::multisession, workers = min(length(raster_list), future::availableCores() - 1))

results_area <- future.apply::future_lapply(names(raster_list), function(yr) {
  r <- raster_list[[yr]]
  v <- exactextractr::exact_extract(r, buffers, fun = fun_area34, progress = TRUE)
  data.frame(id = buffers$id, year = yr, area_ha = unlist(v))
}, future.seed = TRUE)

results_area_df <- do.call(rbind, results_area)

results_area_df %>%
  tidyr::pivot_wider(names_from = year, values_from = area_ha, names_prefix = "forest_area_") %>%
  left_join(buffers_mun %>% mutate(id = as.character(id)), by = "id") %>%
  st_drop_geometry() %>%
  mutate(code_mun = as.factor(CD_MUN), .keep = "unused") %>%
  dplyr::select(-geometry) %>%
  glimpse() -> results_area_wide

##Calculate net forest change per buffer and extrapolate to biome
results_area_wide %>%
  mutate(forest_change_ha = forest_area_2022 - forest_area_2010) %>%
  left_join(
    tabela_buffer_nova %>%
      st_drop_geometry() %>%
      dplyr::select(id, perc_forest_2022),
    by = "id"
  ) %>%
  left_join(
    tabela_buffer_nova %>%
      mutate(area_km2 = as.numeric(st_area(geom)) / 1e6) %>%
      st_drop_geometry() %>%
      dplyr::select(id, area_km2),
    by = "id"
  ) %>%
  filter(!is.na(forest_change_ha), !is.na(perc_forest_2022)) -> buffers_area

sample_ge20 <- buffers_area %>% filter(perc_forest_2022 >= 20)

change_sample_ha      <- sum(sample_ge20$forest_change_ha, na.rm = TRUE)
area_sample_km2       <- sum(sample_ge20$area_km2, na.rm = TRUE)
dens_change_hat       <- change_sample_ha / area_sample_km2

area_bioma_km2        <- caat_shape_5880 %>% mutate(area_km2 = area_m2 / 1e6) %>% pull(area_km2)
prop_ge20             <- mean(buffers$perc_forest_2022 >= 20, na.rm = TRUE)
area_ge20_bioma_km2   <- prop_ge20 * area_bioma_km2

forest_change_est_ge20_ha <- dens_change_hat * area_ge20_bioma_km2
forest_change_est_ge20_ha
