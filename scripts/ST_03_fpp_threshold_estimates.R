# Supplementary Table 3: Estimated FPP at varying forest cover thresholds

# Libraries ----
library(here)
library(sf)
library(dplyr)
library(purrr)
library(tibble)
library(scales)
library(officer)
library(flextable)

# Data ----
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/tabela_buffer_nova.gpkg") -> tabela_buffer_nova
read_sf("/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/caat_shape_5880.shp") -> caat_shape_5880

tabela_buffer_nova %>%
  rename(geom_buffer = geom) %>%
  mutate(area_m2 = as.numeric(st_area(geom_buffer)),
         area_km2 = area_m2 / 1e6) %>%
  filter(!is.na(fpp_2022), !is.na(perc_forest_2022)) -> buffers

caat_shape_5880 %>%
  mutate(area_km2 = area_m2 / 1e6) %>%
  pull(area_km2) -> area_bioma_km2

thresholds <- seq(10, 100, 10)

# Compute estimates per threshold ----
calc_threshold <- function(t) {
  df <- buffers %>% mutate(hit = perc_forest_2022 >= t)

  pop_sample_10  <- sum(buffers$fpp_2010[buffers$perc_forest_2010 >= t], na.rm = TRUE)
  area_sample_10 <- sum(buffers$area_km2[buffers$perc_forest_2010 >= t], na.rm = TRUE)
  dens_hat_10    <- ifelse(area_sample_10 > 0, pop_sample_10 / area_sample_10, NA_real_)
  prop_area_10   <- mean(buffers$perc_forest_2010 >= t, na.rm = TRUE)
  pop_est_10     <- dens_hat_10 * prop_area_10 * area_bioma_km2

  pop_sample_22  <- sum(df$fpp_2022[df$hit], na.rm = TRUE)
  area_sample_22 <- sum(df$area_km2[df$hit], na.rm = TRUE)
  dens_hat_22    <- ifelse(area_sample_22 > 0, pop_sample_22 / area_sample_22, NA_real_)
  prop_area_22   <- mean(df$hit, na.rm = TRUE)
  pop_est_22     <- dens_hat_22 * prop_area_22 * area_bioma_km2

  n_buffers <- sum(df$hit, na.rm = TRUE)
  mean_perc <- ifelse(n_buffers > 0, mean(df$perc_forest_2022[df$hit], na.rm = TRUE), NA_real_)
  sd_perc   <- ifelse(n_buffers > 1, sd(df$perc_forest_2022[df$hit], na.rm = TRUE), NA_real_)

  tibble(
    threshold    = t,
    pop_est_2010 = round(pop_est_10),
    pop_est_2022 = round(pop_est_22),
    abs_change   = round(pop_est_22 - pop_est_10),
    n_buffers    = n_buffers,
    mean_sd_forest = paste0(round(mean_perc, 1), " (", round(sd_perc, 2), ")")
  )
}

tab_st3 <- map_dfr(thresholds, calc_threshold)

# Format table ----
tab_st3 %>%
  mutate(
    pop_est_2010 = label_comma()(pop_est_2010),
    pop_est_2022 = label_comma()(pop_est_2022),
    abs_change   = label_comma()(abs_change),
    n_buffers    = label_comma()(n_buffers),
    threshold    = paste0(threshold, "%")
  ) %>%
  rename(
    `Threshold (at least %)` = threshold,
    `Population in 2010`     = pop_est_2010,
    `Population in 2022`     = pop_est_2022,
    `Absolute change`        = abs_change,
    `n landscapes (2022)`    = n_buffers,
    `Mean (SD) forest cover 2022 (%)` = mean_sd_forest
  ) -> tab_st3_fmt

# Export to Word ----
ft <- flextable(tab_st3_fmt) %>%
  set_table_properties(layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all")

doc <- read_docx() %>%
  body_add_par("Supplementary Table 3. Estimated number of forest-proximate rural population (FPP) by extrapolating to the entire Caatinga biome, the FPP in landscapes with different minimum thresholds of forest cover.", style = "Normal") %>%
  body_add_flextable(ft)

print(doc, target = here("supp_table3_fpp_thresholds.docx"))
