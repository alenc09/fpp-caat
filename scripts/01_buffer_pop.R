# Mon Aug 11 18:50:57 2025 ------------------------------
#script para calcular o número de pessoas por buffer

# #Libraries----
# library(sf)
# library(dplyr)
# library(future.apply)
# library(tidyr)

#data----
# read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/buffer_5km_2ndreview_5880.shp") -> buffers
# read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/popPoint_caat_rural_2010_5880.shp") -> caatinga_pop_2010
# read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/popPoint_caat_rural_2011_5880.shp") -> caatinga_pop_2011
# read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/popPoint_caat_rural_2017_5880.shp") -> caatinga_pop_2017
# read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/popPoint_caat_rural_2018_5880.shp") -> caatinga_pop_2018
# read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/popPoint_caat_rural_2022_5880.shp") -> caatinga_pop_2022
# read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/popPoint_caat_rural_2023_5880.shp") -> caatinga_pop_2023
# 
# read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/buffers_municipio_5880.shp") -> buffers_mun

##organization----
# pop_list <- list(caatinga_pop_2010, caatinga_pop_2011, caatinga_pop_2017,
#                  caatinga_pop_2018, caatinga_pop_2022, caatinga_pop_2023)
# 
# names(pop_list) <- c("2010","2011","2017","2018","2022","2023")
# 
# years <- names(pop_list)  
# pop_col <- "pop"          
# 
# res_list <- vector("list", length(pop_list))
# 
# for(i in seq_along(pop_list)) {
#   pp <- pop_list[[i]]
#   yr <- years[i]
#   
#   if(!inherits(pp, "sf")) stop(sprintf("pop_list[[%s]] não é sf", yr))
#   if(!pop_col %in% names(pp)) stop(sprintf("coluna '%s' não encontrada em pop_list[[%s]]", pop_col, yr))
#   
#   # obter índices dos pontos dentro de cada buffer (ordem = ordem de buffers_5km)
#   idx <- st_intersects(buffers, pp)
#   
#   # somar população por buffer
#   pop_sum <- sapply(idx, function(ii) {
#     if(length(ii) == 0) return(0)
#     sum(pp[[pop_col]][ii], na.rm = TRUE)
#   })
#   
#   res_list[[i]] <- data.frame(id = buffers$id, year = yr, pop_sum = pop_sum)
# }
# 
# pop_results_df <- bind_rows(res_list)
# pop_results_wide <- pop_results_df %>%
#   pivot_wider(names_from = year, values_from = pop_sum, names_prefix = "pop_")
# 
# pop_results_wide %>% 
#   left_join(y = buffers_mun) %>% 
#   mutate(code_mun = as.factor(CD_MUN), .keep = "unused") %>% 
#   dplyr::select(-geometry) %>% 
#   glimpse -> pop_results_wide
# 
# write.csv(x = pop_results_wide, file = "~/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/buffer_population.csv")

#population in buffers using census tract 2010 and 2022----
##Libraries----
library(sf)

##data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/buffer_5km_2ndreview_5880.shp") -> buffers
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/sc_caat_rural_2010.shp") -> sc_caat_rural_2010
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/sc_caat_rural_2022_5880_fixed.gpkg") -> sc_caat_rural_2022
read.csv("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/pop_sc_br_2010.csv") -> pop_sc_br_2010
read.csv("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/pop_sc_br_2022.csv") -> pop_sc_br_2022

###organisation----
sc_caat_rural_2010 %>% 
  select(cd_trct, code_mn, cod_stt) %>% 
  mutate(cd_trct = as.double(cd_trct)) %>% 
  left_join(y = pop_sc_br_2010, by = c("cd_trct" = "code_tract")) %>% 
  mutate(code_tract = as.factor(cd_trct),
         code_mun = as.factor(code_mn),
         code_state = as.factor(cod_stt),
         pop_2010 = ifelse(is.na(V002), 0, V002),
         geometry = geometry,
         .keep = "none") %>% 
  glimpse -> pop_sc_rural_caat_2010

sc_caat_rural_2022 %>% 
  select(cd_trct, code_mn, cod_stt) %>%
  left_join(y = pop_sc_br_2022, by = c("cd_trct" = "code_tract")) %>%
  mutate(code_tract = as.factor(cd_trct),
         code_mun = as.factor(code_mn),
         code_state = as.factor(cod_stt),
         pop_2022 = ifelse(is.na(V0001), 0, V0001),
         geom = geom,
         .keep = "none") %>%
  glimpse -> pop_sc_rural_caat_2022

aw_interpolate(.data = buffers,                # malha destino (2022)
               tid = id,                 # ID único da malha destino
               source = pop_sc_rural_caat_2010,               # malha de origem (2010)
               sid = code_tract,                 # ID único da malha origem
               weight = "sum",                  # tipo de agregação
               output = "sf",               # retorna um objeto sf
               extensive = ("pop_2010")                 # ou variáveis densidade
               ) -> pop_buffer_2010

aw_interpolate(.data = buffers,                # malha destino (2022)
               tid = id,                 # ID único da malha destino
               source = pop_sc_rural_caat_2022,               # malha de origem (2010)
               sid = code_tract,                 # ID único da malha origem
               weight = "sum",                  # tipo de agregação
               output = "sf",               # retorna um objeto sf
               extensive = ("pop_2022")                 # ou variáveis densidade
               ) -> pop_buffer_2022

pop_buffer_2010 %>% 
  left_join(y = as_tibble(pop_buffer_2022)) %>% 
  mutate(id = as.factor(id),
         fpp_2010 = round(pop_2010),
         fpp_2022 = round(pop_2022),
         geometry = geometry,
         .keep = "none") %>%
  filter(is.na(fpp_2010)) %>% 
  glimpse #-> buffers_fpp

write_sf(obj = buffers_fpp, dsn = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/buffer_fpp_5880.gpkg")

