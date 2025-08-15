# Mon Aug 11 16:42:39 2025 ------------------------------
#script para medir cobertura florestal em cada buffer

#libraries----
library(raster)
library(sf)
library(exactextractr)
library(future.apply)
library(tidyr)


#data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/buffer_5km_2ndreview_5880.shp") -> buffers
raster("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/gis/mapbiomas_caatinga_2010_5880.tif") -> caatinga_lc_2010
raster("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/gis/mapbiomas_caatinga_2011_5880.tif") -> caatinga_lc_2011
raster("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/gis/mapbiomas_caatinga_2017_5880.tif") -> caatinga_lc_2017
raster("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/gis/mapbiomas_caatinga_2018_5880.tif") -> caatinga_lc_2018
raster("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/gis/mapbiomas_caatinga_2022_5880.tif") -> caatinga_lc_2022
raster("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/clean/gis/mapbiomas_caatinga_2023_5880.tif") -> caatinga_lc_2023

#organization----
raster_list <- list(
  caatinga_lc_2010,
  caatinga_lc_2011,
  caatinga_lc_2017,
  caatinga_lc_2018,
  caatinga_lc_2022,
  caatinga_lc_2023
)
names(raster_list) <- c("2010","2011","2017","2018","2022","2023")

fun_pct34 <- function(values, coverage_fractions) {
  # values: vector de valores de célula; coverage_fractions: fração da célula no polígono
  mask <- values %in% c(3,4)
  if (all(is.na(mask))) return(NA_real_)
  # soma ponderada das frações onde a classe é 3 ou 4
  pct <- 100 * sum(coverage_fractions[mask], na.rm = TRUE) / sum(coverage_fractions, na.rm = TRUE)
  return(pct)
}

future::plan(future::multisession, workers = min(length(raster_list), future::availableCores()-1))

results <- future.apply::future_lapply(names(raster_list), function(yr) {
  r <- raster_list[[yr]]
  v <- exactextractr::exact_extract(r, buffers, fun = fun_pct34, progress = TRUE)
  data.frame(id = buffers$id, year = yr, pct34 = unlist(v))
}, future.seed = TRUE)

results_df <- do.call(rbind, results)

results_wide <- results_df %>%
  pivot_wider(
    names_from = year,
    values_from = pct34,
    names_prefix = "perc_forest_"
  )

write.csv(x = results_wide, file = "~/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/buffer_forest.csv")
