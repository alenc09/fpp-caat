# Mon Aug 11 18:50:57 2025 ------------------------------
#script para calcular o número de pessoas por buffer

#Libraries----
library(sf)
library(dplyr)
library(future.apply)
library(tidyr)

#data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/buffer_5km_2ndreview_5880.shp") -> buffers
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/popPoint_caat_rural_2010_5880.shp") -> caatinga_pop_2010
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/popPoint_caat_rural_2011_5880.shp") -> caatinga_pop_2011
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/popPoint_caat_rural_2017_5880.shp") -> caatinga_pop_2017
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/popPoint_caat_rural_2018_5880.shp") -> caatinga_pop_2018
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/popPoint_caat_rural_2022_5880.shp") -> caatinga_pop_2022
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/popPoint_caat_rural_2023_5880.shp") -> caatinga_pop_2023

read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/buffers_municipio_5880.shp") -> buffers_mun

##organization----
pop_list <- list(caatinga_pop_2010, caatinga_pop_2011, caatinga_pop_2017,
                 caatinga_pop_2018, caatinga_pop_2022, caatinga_pop_2023)

names(pop_list) <- c("2010","2011","2017","2018","2022","2023")

years <- names(pop_list)  
pop_col <- "pop"          

res_list <- vector("list", length(pop_list))

for(i in seq_along(pop_list)) {
  pp <- pop_list[[i]]
  yr <- years[i]
  
  if(!inherits(pp, "sf")) stop(sprintf("pop_list[[%s]] não é sf", yr))
  if(!pop_col %in% names(pp)) stop(sprintf("coluna '%s' não encontrada em pop_list[[%s]]", pop_col, yr))
  
  # obter índices dos pontos dentro de cada buffer (ordem = ordem de buffers_5km)
  idx <- st_intersects(buffers, pp)
  
  # somar população por buffer
  pop_sum <- sapply(idx, function(ii) {
    if(length(ii) == 0) return(0)
    sum(pp[[pop_col]][ii], na.rm = TRUE)
  })
  
  res_list[[i]] <- data.frame(id = buffers$id, year = yr, pop_sum = pop_sum)
}

pop_results_df <- bind_rows(res_list)
pop_results_wide <- pop_results_df %>%
  pivot_wider(names_from = year, values_from = pop_sum, names_prefix = "pop_")

pop_results_wide %>% 
  left_join(y = buffers_mun) %>% 
  mutate(code_mun = as.factor(CD_MUN), .keep = "unused") %>% 
  dplyr::select(-geometry) %>% 
  glimpse -> pop_results_wide

# write.csv(x = pop_results_wide, file = "~/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/buffer_population.csv")
