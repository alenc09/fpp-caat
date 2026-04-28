# Script to estimate Forest-proximate population (FPP) at multiple scales:
# biome, municipality, and state

#Libraries----
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
library(patchwork)
library(ragg)

#Data----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tabela_buffer_nova.gpkg") -> tabela_buffer_nova
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/caat_shape_5880.shp") -> caat_shape_5880
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/caat_states_5880.gpkg") -> caat_states
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tab_mun_analysis.gpkg") -> tab_mun_analysis

#Biome scale----
##FPP summary at landscapes >= 20% forest cover
tabela_buffer_nova %>%
  filter(perc_forest_2022 >= 20) %>%
  summarise(n_paisagens = n_distinct(id)) %>%
  glimpse

tabela_buffer_nova %>%
  filter(perc_forest_2022 >= 20) %>%
  summarise(fpp_10 = sum(fpp_2010, na.rm = T),
            fpp_22 = sum(fpp_2022, na.rm = T),
            abs_change = fpp_22 - fpp_10,
            mean_fpp_10 = mean(fpp_2010, na.rm = T),
            sd_fpp_10 = sd(fpp_2010, na.rm = T),
            mean_perc_forest_10 = mean(perc_forest_2010),
            sd_perc_fores_10 = sd(perc_forest_2010),
            mean_fpp_22 = mean(fpp_2022),
            sd_fpp_22 = sd(fpp_2022),
            mean_perc_forest_22 = mean(perc_forest_2022),
            sd_perc_forest_22 = sd(perc_forest_2022)
            ) %>%
  glimpse -> fpp_buffs

##Extrapolation to the entire biome
tabela_buffer_nova %>%
  rename(geom_buffer = geom) %>%
  mutate(area_m2 = as.numeric(st_area(geom_buffer)),
         area_km2 = area_m2/1e6) %>%
  filter(!is.na(fpp_2022), !is.na(perc_forest_2022)) %>%
  glimpse -> buffers

caat_shape_5880 %>%
  mutate(area_km2 = area_m2 / 1e6) %>%
  pull(area_km2) %>%
  glimpse -> area_bioma_km2

##FPP at varying thresholds — 2010
thresholds <- seq(10, 100, 10)

calc_threshold_10 <- function(t) {
  df <- buffers %>% mutate(hit = perc_forest_2010 >= t)
  pop_sample  <- sum(df$fpp_2010[df$hit], na.rm = TRUE)
  area_sample <- sum(df$area_km2[df$hit], na.rm = TRUE)
  dens_hat    <- ifelse(area_sample > 0, pop_sample / area_sample, NA_real_)
  prop_area_ge    <- mean(df$hit, na.rm = TRUE)
  area_ge_est_km2 <- prop_area_ge * area_bioma_km2
  pop_est_ge      <- dens_hat * area_ge_est_km2
  tibble(threshold = t, dens_hat_p_km2 = dens_hat, prop_area_ge = prop_area_ge,
         area_ge_est_km2 = area_ge_est_km2, pop_est_ge = pop_est_ge)
}

results_bioma_10 <- map_dfr(thresholds, calc_threshold_10)
results_bioma_10

##FPP at varying thresholds — 2022
calc_threshold_22 <- function(t) {
  df <- buffers %>% mutate(hit = perc_forest_2022 >= t)
  pop_sample  <- sum(df$fpp_2022[df$hit], na.rm = TRUE)
  area_sample <- sum(df$area_km2[df$hit], na.rm = TRUE)
  dens_hat    <- ifelse(area_sample > 0, pop_sample / area_sample, NA_real_)
  prop_area_ge    <- mean(df$hit, na.rm = TRUE)
  area_ge_est_km2 <- prop_area_ge * area_bioma_km2
  pop_est_ge      <- dens_hat * area_ge_est_km2
  n_buffers <- sum(df$hit, na.rm = TRUE)
  mean_perc <- ifelse(n_buffers > 0, mean(df$perc_forest_2022[df$hit], na.rm = TRUE), NA_real_)
  sd_perc   <- ifelse(n_buffers > 1, sd(df$perc_forest_2022[df$hit], na.rm = TRUE), NA_real_)
  tibble(threshold = t, dens_hat_p_km2 = dens_hat, prop_area_ge = prop_area_ge,
         area_ge_est_km2 = area_ge_est_km2, pop_est_ge = pop_est_ge,
         n_buffers = n_buffers, mean_perc = mean_perc, sd_perc = sd_perc)
}

results_bioma_22 <- map_dfr(thresholds, calc_threshold_22)
results_bioma_22

##Annual rate of change per threshold
results_bioma_10 %>%
  select(threshold, pop_est_ge) %>%
  rename(popest_2010 = pop_est_ge) %>%
  left_join(y = select(results_bioma_22, threshold, pop_est_ge)) %>%
  rename(popest_2022 = pop_est_ge) %>%
  pivot_longer(cols = c(popest_2010, popest_2022),
               names_prefix = "popest_",
               names_to = "year",
               values_to = "pop_est") %>%
  mutate(year = as.numeric(year)) %>%
  glimpse -> results_bioma

results_bioma %>%
  pivot_wider(names_from = year, values_from = pop_est) %>%
  mutate(fpp_change = `2022` - `2010`,
         taxa_anual = (`2022`/`2010`)^(1/12) - 1,
         taxa_anual_perc = taxa_anual * 100) %>%
  glimpse -> results_bioma_diff

#Municipality scale----
##FPP trend
tab_mun_analysis %>%
  mutate(cat_change = as.factor(cat_change),
         tendencia_fpp = case_when(
           mean_fpp_perc_change > 0 ~ "crescendo",
           mean_fpp_perc_change < 0 ~ "diminuindo",
           TRUE ~ "estável"
         )) %>%
  count(tendencia_fpp) %>%
  mutate(perc = 100 * n / sum(n))

##Forest cover trend
tab_mun_analysis %>%
  mutate(cat_change = as.factor(cat_change),
         tendencia_forest = case_when(
           mean_forest_perc_change > 0 ~ "crescendo",
           mean_forest_perc_change < 0 ~ "diminuindo",
           TRUE ~ "estável"
         )) %>%
  count(tendencia_forest) %>%
  mutate(perc = 100 * n / sum(n))

#State scale----
caat_states %>%
  mutate(area_m2 = as.numeric(st_area(geom)),
         area_state_km2 = area_m2/1e6,
         code_state = as.factor(code_state)) %>%
  glimpse -> caat_states

buffers %>%
  mutate(code_state = as.factor(str_sub(as.character(code_mun), 1, 2))) %>%
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
  mutate(dens_hat = pop_sample_ge20 / area_sample_ge20_km2) %>%
  left_join(caat_states, by = "code_state") %>%
  mutate(area_ge20_est_km2 = prop_buffers_ge20 * area_state_km2,
         pop_est_ge20      = dens_hat * area_ge20_est_km2) %>%
  glimpse -> estimates_fpp_states

estimates_fpp_states %>% select(name_state, pop_est_ge20)

##State estimates at varying thresholds
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

##State ranking at selected thresholds
estimates_fpp_states_all %>%
  filter(limiar %in% c(20, 50, 70)) %>%
  group_by(limiar) %>%
  mutate(rank_pop = dense_rank(desc(pop_est))) %>%
  ungroup() %>%
  select(name_state, limiar, pop_est, rank_pop) %>%
  arrange(limiar, rank_pop) -> ranking_estados_resumo

ranking_estados_resumo %>% glimpse()

#Figures----
##Figure 1 — FPP at varying thresholds (biome scale)
ggplot(results_bioma, aes(x = threshold, y = pop_est, color = factor(year))) +
  annotate("segment", x = 0, xend = 20, y = 7034206, yend = 7034206, linetype = "dashed", color = "lightgrey") +
  annotate("text", label = "7.0", x = 3, y = 7300000, color = "grey60", size = 2) +
  annotate("segment", x = 20, xend = 20, y = 0, yend = 7034206, linetype = "dashed", color = "lightgrey") +
  annotate("segment", x = 0, xend = 50, y = 4394562, yend = 4394562, linetype = "dashed", color = "lightgrey") +
  annotate("text", label = "4.4", x = 3, y = 4650000, color = "grey60", size = 2) +
  annotate("segment", x = 50, xend = 50, y = 0, yend = 4394562, linetype = "dashed", color = "lightgrey") +
  annotate("segment", x = 0, xend = 70, y = 2608132, yend = 2608132, linetype = "dashed", color = "lightgrey") +
  annotate("segment", x = 70, xend = 70, y = 0, yend = 2608132, linetype = "dashed", color = "lightgrey") +
  annotate("text", label = "2.6", x = 3, y = 2880000, color = "grey60", size = 2) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(10, 100, 10), expand = c(0, 0), limits = c(0, 105)) +
  scale_y_continuous(labels = label_number(scale = 1e-6), expand = c(0, 100000), limits = c(0, max(results_bioma$pop_est) * 1.05)) +
  labs(x = "Forest cover threshold (%)", y = "Number of FPP (million)", color = "Year") +
  scale_color_manual(values = c("#ffa600", "#5c3811"), name = "Year", labels = c("2010", "2022")) +
  theme_classic(base_size = 10) +
  theme(legend.position = c(0.8, 0.8),
        plot.margin = unit(c(1, 1, 1, 1), "lines")) -> fpp_thresholds

ggplot(results_bioma_diff, aes(x = threshold, y = fpp_change)) +
  geom_col(fill = "#A50026") +
  annotate("segment", x = 0, xend = 20, y = -1893017, yend = -1893017, linetype = "dashed", color = "lightgrey") +
  annotate("text", label = "-1.9", x = 2.5, y = -1843017, color = "grey60", size = 2) +
  annotate("segment", x = 0, xend = 50, y = -1321012, yend = -1321012, linetype = "dashed", color = "lightgrey") +
  annotate("text", label = "-1.3", x = 2.5, y = -1271012, color = "grey60", size = 2) +
  annotate("segment", x = 0, xend = 70, y = -975210, yend = -973210, linetype = "dashed", color = "lightgrey") +
  annotate(geom = "text", label = "-0.9", x = 2.5, y = -925210, color = "grey60", size = 2) +
  scale_x_continuous(position = "top", breaks = seq(10, 100, 10), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = label_number(scale = 1e-6), expand = c(0, 0.05)) +
  labs(x = "Forest cover threshold (%)", y = "Absolute change (million)") +
  theme_classic(base_size = 10) -> fpp_thresholds_change

fpp_thresholds + fpp_thresholds_change + plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "A", theme = theme(plot.tag = element_text(face = "bold", size = 12))) -> fig1

ggsave(plot = fig1, filename = "img/fig1.tiff",
       units = "in", device = "tiff", dpi = 600, width = 7, height = 3.5)

##Supplementary Figure 2 — FPP per state at varying thresholds
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
