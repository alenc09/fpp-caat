# Supplementary Table 4: FPP estimates per state at 20%, 50%, and 70% thresholds

# Libraries ----
library(here)
library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(tidyr)
library(sf)
library(geobr)
library(scales)
library(officer)
library(flextable)

# Data ----
read_xlsx(here("data/tabela_buffer_nova.xlsx")) %>%
  mutate(area_km2   = pi * 5^2,  # 5 km radius circular buffers
         code_state = as.factor(str_sub(as.character(code_mun), 1, 2))) %>%
  filter(!is.na(fpp_2022), !is.na(perc_forest_2022)) -> buffers

# State areas within Caatinga from geobr ----
read_biomes(showProgress = FALSE) %>%
  filter(name_biome == "Caatinga") %>%
  st_transform(5880) -> caat_biome

read_state(year = 2020, showProgress = FALSE) %>%
  st_transform(5880) %>%
  st_intersection(caat_biome) %>%
  mutate(area_state_km2 = as.numeric(st_area(geom)) / 1e6,
         code_state     = as.factor(code_state)) %>%
  select(code_state, name_state, area_state_km2) %>%
  as.data.frame() %>%
  select(-geom) -> caat_states

thresholds <- c(20, 50, 70)

# State FPP estimates at selected thresholds ----
estimates_states <- map_dfr(thresholds, function(lim) {
  buffers %>%
    as.data.frame() %>%
    mutate(is_ge = perc_forest_2022 >= lim) %>%
    group_by(code_state) %>%
    summarise(
      pop_sample      = sum(fpp_2022[is_ge], na.rm = TRUE),
      area_sample_km2 = sum(area_km2[is_ge], na.rm = TRUE),
      prop_buffers    = mean(is_ge, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(dens_hat  = pop_sample / area_sample_km2) %>%
    left_join(caat_states, by = "code_state") %>%
    mutate(area_est_km2 = prop_buffers * area_state_km2,
           pop_est      = round(dens_hat * area_est_km2),
           threshold    = lim)
})

# Rank per threshold and pivot wide ----
estimates_states %>%
  group_by(threshold) %>%
  mutate(rank = dense_rank(desc(pop_est))) %>%
  ungroup() %>%
  select(name_state, threshold, pop_est, rank) %>%
  mutate(
    pop_fmt   = label_comma()(pop_est),
    threshold = paste0("pct", threshold)
  ) %>%
  select(-pop_est) %>%
  pivot_wider(names_from = threshold, values_from = c(pop_fmt, rank),
              names_glue = "{threshold}_{.value}") %>%
  arrange(pct20_rank) %>%
  select(name_state,
         `Population (20%)` = pct20_pop_fmt, `Rank 20%` = pct20_rank,
         `Population (50%)` = pct50_pop_fmt, `Rank 50%` = pct50_rank,
         `Population (70%)` = pct70_pop_fmt, `Rank 70%` = pct70_rank) -> tab_st4

# Export to Word ----
ft <- flextable(tab_st4) %>%
  set_header_labels(name_state = "State") %>%
  set_table_properties(layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all")

doc <- read_docx() %>%
  body_add_par(
    paste0("Supplementary Table 4. Estimate size of forest-proximate population per Brazilian state ",
           "using the sampled landscapes with different levels of forested landscapes, from 20% minimum ",
           "vegetation cover (least conservative), through 50%, and 70% (most conservative). This table ",
           "also shows the rank of each state in terms of forest-proximate population size. Note that the ",
           "rank changes according to the threshold used."),
    style = "Normal"
  ) %>%
  body_add_flextable(ft)

print(doc, target = here("supp_table4_fpp_states.docx"))
