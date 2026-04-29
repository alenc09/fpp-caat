# Figure 1: FPP estimates at varying forest cover thresholds (biome scale)

# Libraries ----
library(here)
library(sf)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)
library(ragg)

# Data ----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tabela_buffer_nova.gpkg") -> tabela_buffer_nova
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/caat_shape_5880.shp") -> caat_shape_5880

tabela_buffer_nova %>%
  rename(geom_buffer = geom) %>%
  mutate(area_m2 = as.numeric(st_area(geom_buffer)),
         area_km2 = area_m2/1e6) %>%
  filter(!is.na(fpp_2022), !is.na(perc_forest_2022)) -> buffers

caat_shape_5880 %>%
  mutate(area_km2 = area_m2 / 1e6) %>%
  pull(area_km2) -> area_bioma_km2

thresholds <- seq(10, 100, 10)

# Compute FPP estimates per threshold ----
calc_threshold_10 <- function(t) {
  df <- buffers %>% mutate(hit = perc_forest_2010 >= t)
  pop_sample  <- sum(df$fpp_2010[df$hit], na.rm = TRUE)
  area_sample <- sum(df$area_km2[df$hit], na.rm = TRUE)
  dens_hat    <- ifelse(area_sample > 0, pop_sample / area_sample, NA_real_)
  prop_area_ge    <- mean(df$hit, na.rm = TRUE)
  area_ge_est_km2 <- prop_area_ge * area_bioma_km2
  pop_est_ge      <- dens_hat * area_ge_est_km2
  tibble(threshold = t, pop_est_ge = pop_est_ge)
}

calc_threshold_22 <- function(t) {
  df <- buffers %>% mutate(hit = perc_forest_2022 >= t)
  pop_sample  <- sum(df$fpp_2022[df$hit], na.rm = TRUE)
  area_sample <- sum(df$area_km2[df$hit], na.rm = TRUE)
  dens_hat    <- ifelse(area_sample > 0, pop_sample / area_sample, NA_real_)
  prop_area_ge    <- mean(df$hit, na.rm = TRUE)
  area_ge_est_km2 <- prop_area_ge * area_bioma_km2
  pop_est_ge      <- dens_hat * area_ge_est_km2
  tibble(threshold = t, pop_est_ge = pop_est_ge)
}

results_bioma_10 <- map_dfr(thresholds, calc_threshold_10)
results_bioma_22 <- map_dfr(thresholds, calc_threshold_22)

results_bioma_10 %>%
  rename(popest_2010 = pop_est_ge) %>%
  left_join(rename(results_bioma_22, popest_2022 = pop_est_ge), by = "threshold") %>%
  pivot_longer(cols = c(popest_2010, popest_2022),
               names_prefix = "popest_",
               names_to = "year",
               values_to = "pop_est") %>%
  mutate(year = as.numeric(year)) -> results_bioma

results_bioma %>%
  pivot_wider(names_from = year, values_from = pop_est) %>%
  mutate(fpp_change = `2022` - `2010`) -> results_bioma_diff

# Figure 1A — FPP at varying thresholds ----
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

# Figure 1B — Absolute FPP change per threshold ----
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

# Save ----
fpp_thresholds + fpp_thresholds_change + plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "A", theme = theme(plot.tag = element_text(face = "bold", size = 12))) -> fig1

ggsave(plot = fig1, filename = "img/fig1.tiff",
       units = "in", device = "tiff", dpi = 600, width = 7, height = 3.5)
