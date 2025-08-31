censobr::read(simplified = F, year = 2010) 
geobr::read_census_tract(code_tract = "all", year = 2022, simplified = F) -> censo_tract_2022

censobr::read_tracts(dataset = "Domicilio", year = 2022) -> household_2022

household_2022 %>% 
  collect() %>% 
  mutate(situacao = as.factor(situacao)) %>%
  dplyr::select(code_tract, code_muni, situacao) %>% 
  glimpse -> situacao_setor_2022

censobr::read_tracts(dataset = "Basico", year = 2022, as_data_frame = T) -> basico_2022

basico_2022 %>% 
  collect() %>% 
  dplyr::select(code_tract, code_muni, situacao, V0003) %>% 
  filter(situacao == "Rural") %>% 
  group_by(code_muni) %>% 
  reframe(domicilios_rural_2022 = sum(V0003)) %>% 
  glimpse -> domicilios_rural_brasil_2022

data_dictionary(year = 2010, dataset = "tracts")

censobr::read_tracts(dataset = "Entorno", year = 2022) -> entorno_2022


basico_2022 %>% 
  collect %>% 
  mutate(code_tract = as.double(code_tract)) %>% 
  glimpse -> basico_2022_df

censo_tract_2022 %>%
  left_join(y = basico_2022_df, by = "code_tract") %>% 
  glimpse -> pop_sc_br

write_sf(obj = pop_sc_br, dsn = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/gis/pop_sc_br.gpkg")


censobr::read_tracts(dataset = "Basico", year = 2010, as_data_frame = T) -> basico_2010

basico_2010 %>% 
  select(code_tract, V002) %>% 
  glimpse -> pop_sc_br_2010
write.csv(x = pop_sc_br_2010, file = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/pop_sc_br_2010.csv")

basico_2022 %>% 
  select(code_tract, V0001) %>% 
  glimpse -> pop_sc_br_2022
write.csv(x = pop_sc_br_2022, file = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/statistical/pop_sc_br_2022.csv")

write_sf(obj = sc_caat_rural_2022, dsn = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/sc_caat_rural_2022_5880.gpkg")

geobr::read_state(code_state = "all", year = 2020, simplified = F) -> br_states

write_sf(obj = br_states, dsn = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/SFT/Data/Brazil/raw/gis/br_states.gpkg")
