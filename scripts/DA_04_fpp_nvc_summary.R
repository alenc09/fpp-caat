# Descriptive summaries of FPP and forest cover change by GG/GP/PG/PP category
# at landscape and municipality scales

# Libraries ----
library(here)
library(dplyr)
library(readr)

# Data ----
read_csv(here("data/tab_buffer_analysis.csv"), show_col_types = FALSE) %>%
  mutate(cat_change = as.factor(cat_change)) -> tab_buff_analysis
read_csv(here("data/tab_mun_analysis.csv"), show_col_types = FALSE) %>%
  mutate(cat_change = as.factor(cat_change)) -> tab_mun_analysis

# Data organisation ----
tab_buff_analysis %>%
  mutate(forest_perc_change = if_else(condition = forest_perc_change > 100,
                                           true = 100,
                                           false = forest_perc_change)) %>%
  filter(cat_change != "stable") %>%
  filter(fpp_perc_change != Inf) %>%
  filter(!is.na(cat_change)) %>%
  filter(perc_forest_2022 >= 20) -> tab_buff_analysis

tab_mun_analysis %>%
  mutate(mean_forest_perc_change = if_else(condition = mean_forest_perc_change > 100,
                                      true = 100,
                                      false = mean_forest_perc_change)) %>%
  filter(cat_change != "stable") -> tab_mun_analysis

# Landscape scale ----
tab_buff_analysis %>%
  group_by(cat_change) %>%
  summarise(
    n_paisagens = n(),
    total_pop_2010 = sum(fpp_2010, na.rm = TRUE),
    total_pop_2022 = sum(fpp_2022, na.rm = TRUE),
    mean_pop_change = mean(fpp_perc_change, na.rm = TRUE),
    mean_forest_change = mean(forest_perc_change, na.rm = TRUE),
    mean_foresr_cover = mean(perc_forest_2022),
    .groups = "drop"
  ) -> tab_summary_landscapes

tab_summary_landscapes

# Municipality scale ----
tab_mun_analysis %>%
  group_by(cat_change) %>%
  summarise(
    n_mun = n(),
    total_pop_2010 = sum(mean_fpp_2010, na.rm = TRUE),
    total_pop_2022 = sum(mean_fpp_2022, na.rm = TRUE),
    mean_pop_change = mean(mean_fpp_perc_change, na.rm = TRUE),
    mean_forest_change = mean(mean_forest_perc_change, na.rm = TRUE),
    mean_forest_cover = mean(mean_perc_forest_2022),
    .groups = "drop"
  ) -> tab_summary_mun

tab_summary_mun
