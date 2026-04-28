# Fri Aug 29 12:23:59 2025 ------------------------------
# Script to estimate Forest-proximate population per state at varying thresholds

#Libraries----
library(readxl)
library(sf)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggbump)

#data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tabela_buffer_nova.gpkg") -> tabela_buffer_nova
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/caat_states_5880.gpkg") -> caat_states

#extrapolation per state at 20% forest cover----
tabela_buffer_nova %>% 
  mutate(code_state = str_sub(as.character(code_mun), 1,2),
         code_state = as.factor(code_state)) %>% 
  glimpse -> tabela_buffer_nova

caat_states %>% 
  mutate(area_m2 = as.numeric(st_area(geom)),
         area_state_km2 = area_m2/1e6,
         code_state = as.factor(code_state)) %>% 
  glimpse -> caat_states

tabela_buffer_nova %>% 
  rename(geom_buffer = geom) %>% 
  mutate(area_m2 = as.numeric(st_area(geom_buffer)),
         area_km2 = area_m2/1e6) %>% 
  glimpse -> buffers

buffers %>%
  as.data.frame() %>% 
  mutate(is_ge20 = perc_forest_2022 >= 20) %>%
  group_by(code_state) %>%
  summarise(
    pop_sample_ge20      = sum(fpp_2022[is_ge20], na.rm = TRUE),
    area_sample_ge20_km2 = sum(area_km2[is_ge20], na.rm = TRUE),
    n_buffers_ge20       = sum(is_ge20, na.rm = TRUE),
    n_buffers_total      = n(),
    prop_buffers_ge20    = mean(is_ge20, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    dens_hat = pop_sample_ge20 / area_sample_ge20_km2
  ) %>%
  left_join(caat_states, by = "code_state") %>%
  mutate(
    area_ge20_est_km2 = prop_buffers_ge20 * area_state_km2,
    pop_est_ge20      = dens_hat * area_ge20_est_km2) %>% 
  glimpse -> estimates_fpp_states

estimates_fpp_states %>% 
  select(name_state, pop_est_ge20)

#At different thresholds----
seq(10,100, 10) -> limiares

estimates_fpp_states_all <- map_dfr(limiares, function(lim) {
  buffers %>%
    as.data.frame() %>%
    mutate(is_ge = perc_forest_2022 >= lim) %>%
    group_by(code_state) %>%
    summarise(
      pop_sample      = sum(fpp_2022[is_ge], na.rm = TRUE),
      area_sample_km2 = sum(area_km2[is_ge], na.rm = TRUE),
      n_buffers       = sum(is_ge, na.rm = TRUE),
      n_buffers_total = n(),
      prop_buffers    = mean(is_ge, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      dens_hat = pop_sample / area_sample_km2
    ) %>%
    left_join(caat_states, by = "code_state") %>%
    mutate(
      area_est_km2 = prop_buffers * area_state_km2,
      pop_est      = dens_hat * area_est_km2,
      limiar = lim
    )
})

estimates_fpp_states_all %>% 
  glimpse

estimates_fpp_states_all <- estimates_fpp_states_all %>%
  complete(code_state, limiar = seq(10, 100, 10),
           fill = list(pop_est = 0,
                       dens_hat = 0,
                       area_est_km2 = 0,
                       pop_sample = 0,
                       area_sample_km2 = 0,
                       n_buffers = 0,
                       n_buffers_total = 0,
                       prop_buffers = 0))

#Supplementary figure 2----
ggplot(estimates_fpp_states_all, aes(x = limiar, y = pop_est, color = code_state)) +
  geom_bump(smooth = 10, size = 1) +
  # geom_point(size = 2) +
  scale_x_continuous(breaks = seq(10, 100, 10)) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  scale_color_discrete(labels = c("Piauí", "Ceará", "Rio Grande\n do Norte",
                                  "Paraíba", "Pernambuco", "Alagoas", "Sergipe",
                                  "Bahia", "Minas Gerais")) +
  labs(x = "Forest cover threshold (%)", y = "Number of FPP", color = "State") +
  theme_classic(base_size = 20) -> SF_2

# ggsave(plot = SF_2, filename = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/outros_trampos/Manuscritos/FPP_caat/manuscript/PNAS/PNAS_lucas_resubmissao/SF_2.jpg")

#Rank of states by estimated number of FPP----
limiares_ranking <- c(20, 50, 70)

ranking_estados_resumo <- estimates_fpp_states_all %>%
  filter(limiar %in% limiares_ranking) %>%
  group_by(limiar) %>%
  mutate(
    rank_pop = dense_rank(desc(pop_est))  # maior população recebe rank 1
  ) %>%
  ungroup() %>%
  select(name_state, limiar, pop_est, rank_pop) %>%
  arrange(limiar, rank_pop)

# opcional: transformar em formato wide para relatório
ranking_estados_wide <- ranking_estados_resumo %>%
  pivot_wider(
    names_from = limiar,
    values_from = c(pop_est, rank_pop),
    names_glue = "{.value}_{limiar}"
  )

ranking_estados_resumo %>% glimpse()
ranking_estados_wide %>% glimpse()
