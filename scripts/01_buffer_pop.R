# Script to estimate rural population per landscape buffer using
# areal interpolation from census tracts (2010 and 2022)

#Libraries----
library(sf)
library(dplyr)
library(areal)

#Data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/buffer_5km_2ndreview_5880.shp") -> buffers
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/sc_caat_rural_2010.shp") -> sc_caat_rural_2010
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/sc_caat_rural_2022_5880_fixed.gpkg") -> sc_caat_rural_2022
read.csv("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/pop_sc_br_2010.csv") -> pop_sc_br_2010
read.csv("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/pop_sc_br_2022.csv") -> pop_sc_br_2022

#Organisation----
sc_caat_rural_2010 %>%
  select(cd_trct, code_mn, cod_stt) %>%
  mutate(cd_trct = as.double(cd_trct)) %>%
  left_join(y = pop_sc_br_2010, by = c("cd_trct" = "code_tract")) %>%
  mutate(code_tract  = as.factor(cd_trct),
         code_mun    = as.factor(code_mn),
         code_state  = as.factor(cod_stt),
         pop_2010    = ifelse(is.na(V002), 0, V002),
         geometry    = geometry,
         .keep = "none") %>%
  glimpse -> pop_sc_rural_caat_2010

sc_caat_rural_2022 %>%
  select(cd_trct, code_mn, cod_stt) %>%
  left_join(y = pop_sc_br_2022, by = c("cd_trct" = "code_tract")) %>%
  mutate(code_tract  = as.factor(cd_trct),
         code_mun    = as.factor(code_mn),
         code_state  = as.factor(cod_stt),
         pop_2022    = ifelse(is.na(V0001), 0, V0001),
         geom        = geom,
         .keep = "none") %>%
  glimpse -> pop_sc_rural_caat_2022

#Analysis — areal interpolation to buffers----
aw_interpolate(.data = buffers,
               tid = id,
               source = pop_sc_rural_caat_2010,
               sid = code_tract,
               weight = "sum",
               output = "sf",
               extensive = "pop_2010") -> pop_buffer_2010

aw_interpolate(.data = buffers,
               tid = id,
               source = pop_sc_rural_caat_2022,
               sid = code_tract,
               weight = "sum",
               output = "sf",
               extensive = "pop_2022") -> pop_buffer_2022

pop_buffer_2010 %>%
  left_join(y = as_tibble(pop_buffer_2022)) %>%
  mutate(id      = as.factor(id),
         fpp_2010 = round(pop_2010),
         fpp_2022 = round(pop_2022),
         geometry = geometry,
         .keep = "none") %>%
  filter(!is.na(fpp_2010)) %>%
  glimpse -> buffers_fpp

write_sf(obj = buffers_fpp, dsn = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/buffer_fpp_5880.gpkg")
