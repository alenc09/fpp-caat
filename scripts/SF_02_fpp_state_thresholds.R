# Supplementary Figure 2: FPP per state at varying forest cover thresholds

# Libraries ----
library(here)
library(sf)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)
library(ggplot2)
library(ggbump)
library(scales)
library(readxl)
library(geobr)

# Data ----
read_excel(here("data/tabela_buffer_nova.xlsx")) -> tabela_buffer_nova

caatinga_state_codes <- c(22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 31L)
read_state(year = 2020) %>%
  filter(code_state %in% caatinga_state_codes) %>%
  mutate(area_m2 = as.numeric(st_area(geom)),
         area_state_km2 = area_m2 / 1e6,
         code_state = as.factor(code_state)) %>%
  st_drop_geometry() -> caat_states

tabela_buffer_nova %>%
  mutate(area_km2 = pi * 5^2) %>%  # 5 km radius circular buffers
  filter(!is.na(fpp_2022), !is.na(perc_forest_2022)) -> buffers

thresholds <- seq(10, 100, 10)

# State FPP estimates at varying thresholds ----
estimates_fpp_states_all <- map_dfr(thresholds, function(lim) {
  buffers %>%
    mutate(code_state = as.factor(str_sub(as.character(code_mun), 1, 2))) %>%
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
    mutate(dens_hat = pop_sample / area_sample_km2) %>%
    left_join(caat_states, by = "code_state") %>%
    mutate(area_est_km2 = prop_buffers * area_state_km2,
           pop_est      = dens_hat * area_est_km2,
           limiar = lim)
})

estimates_fpp_states_all <- estimates_fpp_states_all %>%
  complete(code_state, limiar = seq(10, 100, 10),
           fill = list(pop_est = 0, dens_hat = 0, area_est_km2 = 0,
                       pop_sample = 0, area_sample_km2 = 0,
                       n_buffers = 0, n_buffers_total = 0, prop_buffers = 0))

# Supplementary Figure 2 ----
ggplot(estimates_fpp_states_all, aes(x = limiar, y = pop_est, color = code_state)) +
  geom_bump(smooth = 10, size = 1) +
  scale_x_continuous(breaks = seq(10, 100, 10)) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  scale_color_discrete(labels = c("Piauí", "Ceará", "Rio Grande\n do Norte",
                                  "Paraíba", "Pernambuco", "Alagoas", "Sergipe",
                                  "Bahia", "Minas Gerais")) +
  labs(x = "Forest cover threshold (%)", y = "Number of FPP", color = "State") +
  theme_classic(base_size = 20) -> SF_2

ggsave(plot = SF_2, filename = "img/SF_2.jpg")
