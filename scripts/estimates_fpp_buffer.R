# Tue May 17 09:44:32 2022 ------------------------------
#Script para calcular estimativas (area, pessoas, floresta) 

#library----
library(here)
library(sf)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(ggplot2)
library(scales)
library(patchwork)

#data----
# read.csv(file = here("data/tabela_geral.csv"))-> tab_geral
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tabela_buffer_nova.gpkg") -> tabela_buffer_nova
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/caat_shape_5880.shp") -> caat_shape_5880
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/caat_states_5880.gpkg") -> caat_states

#cáculos----
##n de paisagens
tabela_buffer_nova %>% 
  filter(perc_forest_2022 >= 20) %>%
  summarise(n_paisagens = n_distinct(id)) %>%
  glimpse

##number of FPP at landscapes >20%----
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

###extrapolation to the entire Caatinga----
tabela_buffer_nova %>%
  rename(geom_buffer = geom) %>% 
  mutate(area_m2 = as.numeric(st_area(geom_buffer)),
         area_km2 = area_m2/1e6) %>% 
  filter(!is.na(fpp_2022), !is.na(perc_forest_2022)) %>% 
  glimpse -> buffers

buffers %>% filter(perc_forest_2022 >= 20) -> sample_ge20
sum(sample_ge20$fpp_2022, na.rm = TRUE) -> pop_sample
sum(sample_ge20$area_km2, na.rm = TRUE) -> area_sample_km2
pop_sample / area_sample_km2 -> dens_hat

caat_shape_5880 %>% 
  mutate(area_km2 = area_m2 / 1e6) %>% 
  pull(area_km2) %>% 
  glimpse -> area_bioma_km2

mean(buffers$perc_forest_2022 >= 20, na.rm = TRUE) -> prop_ge20
prop_ge20 * area_bioma_km2 -> area_ge20_bioma_km2
dens_hat * area_ge20_bioma_km2 -> fpp_est_ge20

##Number of fpp at varying thresholds of forest cover----
###2010
thresholds <- seq(10, 100, 10)

calc_threshold_10 <- function(t) {
  df <- buffers %>% mutate(hit = perc_forest_2010 >= t)
  
  pop_sample  <- sum(df$fpp_2010[df$hit], na.rm = TRUE)
  area_sample <- sum(df$area_km2[df$hit], na.rm = TRUE)
  dens_hat    <- ifelse(area_sample > 0, pop_sample / area_sample, NA_real_)   # pessoas/km²
  
  prop_area_ge   <- mean(df$hit, na.rm = TRUE)                                 # proporção de buffers ≥ t
  area_ge_est_km2 <- prop_area_ge * area_bioma_km2
  pop_est_ge     <- dens_hat * area_ge_est_km2
  
  tibble(
    threshold = t,
    dens_hat_p_km2 = dens_hat,
    prop_area_ge   = prop_area_ge,
    area_ge_est_km2 = area_ge_est_km2,
    pop_est_ge      = pop_est_ge
  )
}

results_bioma_10 <- map_dfr(thresholds, calc_threshold_10)

results_bioma_10

###2022
thresholds <- seq(10, 100, 10)

calc_threshold_22 <- function(t) {
  df <- buffers %>% mutate(hit = perc_forest_2022 >= t)
  
  pop_sample  <- sum(df$fpp_2022[df$hit], na.rm = TRUE)
  area_sample <- sum(df$area_km2[df$hit], na.rm = TRUE)
  dens_hat    <- ifelse(area_sample > 0, pop_sample / area_sample, NA_real_)   # pessoas/km²
  
  prop_area_ge   <- mean(df$hit, na.rm = TRUE)                                 # proporção de buffers ≥ t
  area_ge_est_km2 <- prop_area_ge * area_bioma_km2
  pop_est_ge     <- dens_hat * area_ge_est_km2
  n_buffers <- sum(df$hit, na.rm = TRUE)
  mean_perc <- ifelse(n_buffers > 0, mean(df$perc_forest_2022[df$hit], na.rm = TRUE), NA_real_)
  sd_perc   <- ifelse(n_buffers > 1, sd(df$perc_forest_2022[df$hit], na.rm = TRUE), NA_real_)
  
  tibble(
    threshold = t,
    dens_hat_p_km2 = dens_hat,
    prop_area_ge   = prop_area_ge,
    area_ge_est_km2 = area_ge_est_km2,
    pop_est_ge      = pop_est_ge,
    n_buffers        = n_buffers,
    mean_perc        = mean_perc,
    sd_perc          = sd_perc
  )
}

results_bioma_22 <- map_dfr(thresholds, calc_threshold_22)
 
results_bioma_22

#Figures----
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

##FPP varying thresholds
ggplot(results_bioma, aes(x = threshold, y = pop_est, color = factor(year))) +
  annotate("segment", x = 0, xend = 20, y = 7034206, yend = 7034206, linetype = "dashed", color = "lightgrey") +
  annotate("text", label = "7.0", x = 2.5, y = 7200000, color = "grey60")+
  annotate("segment", x = 20, xend = 20, y = 0, yend = 7034206, linetype = "dashed", color = "lightgrey") +
  annotate("segment", x = 0, xend = 50, y = 4394562, yend = 4394562, linetype = "dashed", color = "lightgrey") +
  annotate("text", label = "4.4", x = 2.5, y = 4550000, color = "grey60")+
  annotate("segment", x = 50, xend = 50, y = 0, yend = 4394562, linetype = "dashed", color = "lightgrey") +
  annotate("segment", x = 0, xend = 70, y = 2608132, yend = 2608132, linetype = "dashed", color = "lightgrey") +
  annotate("segment", x = 70, xend = 70, y = 0, yend = 2608132, linetype = "dashed", color = "lightgrey") + 
  annotate("text", label = "2.6", x = 2.5, y = 2780000, color = "grey60")+
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(10, 100, 10), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = label_number(scale = 1e-6), expand = c(0, 100000), limits = c(0, NA)) +
  labs(
    x = "Forest cover threshold (%)",
    y = "Number of FPP (million)",
    color = "Year"
  ) +
  scale_color_manual(values = c("#ffa600", "#5c3811"), name = "Year", labels = c("2010", "2022"))+
  theme_classic(base_size = 20) +
  theme(legend.position = c(0.8, 0.8),
        plot.margin = unit(c(1, 1, 1, 1), "lines")) -> fpp_thresholds

##FPP change at varying thresholds
results_bioma %>% 
  pivot_wider(names_from = year, values_from = pop_est) %>% 
  mutate(fpp_change = `2022` - `2010`) %>% 
  glimpse -> results_bioma_diff

ggplot(results_bioma_diff, aes(x = threshold, y = fpp_change)) +
  geom_col(fill = "#A50026") +
  annotate("segment", x = 0, xend = 20, y = -1893017, yend = -1893017, linetype = "dashed", color = "lightgrey") +
  annotate("text", label = "-1.9", x = 2.5, y = -1860000, color = "grey60")+
  annotate("segment", x = 0, xend = 50, y = -1321012, yend = -1321012, linetype = "dashed", color = "lightgrey") +
  annotate("text", label = "-1.3", x = 2.5, y = -1290000, color = "grey60")+
  annotate("segment", x = 0, xend = 70, y = -973210, yend = -973210, linetype = "dashed", color = "lightgrey") +
  annotate("text", label = "-0.9", x = 2.5, y = -945000, color = "grey60")+
  scale_x_continuous(position = "top", breaks = seq(10, 100, 10), expand = c(0, 0), limits = c(0, NA)) +
  scale_y_continuous(labels = label_number(scale = 1e-6), expand = c(0, 0)) +
  labs(x = " Forest cover threshold (%)", y = "Absolute change (million)")+
  theme_classic(base_size = 20) -> fpp_thresholds_change

###Figure 1---
# fpp_thresholds + fpp_thresholds_change + plot_layout(ncol = 2) +
#   plot_annotation(tag_levels = "a", theme = theme(plot.tag = element_text(face = "bold", size = 14)))
#   ggsave(filename = "/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/outros_trampos/Manuscritos/FPP_caat/manuscript/PNAS/PNAS_lucas_resubmissao/Fig_1.jpg",
#          dpi = 300,
#          width = 16,
#          height = 9)

#rate of change per threshold----
results_bioma_diff %>% 
  mutate(taxa_anual = (`2022`/`2010`)^(1/12) - 1,
         taxa_anual_perc = taxa_anual * 100) %>% 
  glimpse -> results_bioma_diff


