# Supplementary Table 5: Forest cover and FPP variation at landscape level
# Considers only landscapes with >= 20% forest cover (n = 7333)

# Libraries ----
library(here)
library(readr)
library(dplyr)
library(scales)
library(officer)
library(flextable)

# Data ----
read_csv(here("data/tab_buffer_analysis.csv"), show_col_types = FALSE) %>%
  mutate(forest_perc_change = if_else(forest_perc_change > 100, 100, forest_perc_change)) %>%
  filter(perc_forest_2022 >= 20,
         cat_change != "stable",
         !is.na(cat_change),
         is.finite(fpp_perc_change)) -> tab_buff

# Municipality counts per category (from municipality-level table) ----
read_csv(here("data/tab_mun_analysis.csv"), show_col_types = FALSE) %>%
  filter(cat_change != "stable", !is.na(cat_change)) %>%
  count(cat_change, name = "n_mun") -> tab_mun_counts

# Summary by category ----
tab_buff %>%
  group_by(cat_change) %>%
  summarise(
    n_landscapes       = n(),
    fpp_2022           = sum(fpp_2022, na.rm = TRUE),
    mean_fpp_change    = round(mean(fpp_perc_change, na.rm = TRUE), 1),
    mean_forest_cover  = round(mean(perc_forest_2022, na.rm = TRUE), 1),
    mean_forest_change = round(mean(forest_perc_change, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  left_join(tab_mun_counts, by = "cat_change") %>%
  mutate(
    type = case_when(
      cat_change == "GG" ~ "Reforestation with population growth",
      cat_change == "GP" ~ "Reforestation with depopulation",
      cat_change == "PG" ~ "Deforestation with population growth",
      cat_change == "PP" ~ "Deforestation with depopulation"
    ),
    fpp_2022 = label_comma()(fpp_2022)
  ) %>%
  arrange(factor(cat_change, levels = c("GG", "GP", "PG", "PP"))) %>%
  select(`Type of transition`         = type,
         `Number of municipalities`   = n_mun,
         `Number of landscapes`       = n_landscapes,
         `FPP in 2022`                = fpp_2022,
         `Mean FPP change (%)`        = mean_fpp_change,
         `Mean forest cover 2022 (%)` = mean_forest_cover,
         `Mean forest cover change (%)` = mean_forest_change) -> tab_st5

# Export to Word ----
ft <- flextable(tab_st5) %>%
  set_table_properties(layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  bold(part = "header") %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all")

doc <- read_docx() %>%
  body_add_par(
    paste0("Supplementary Table 5. Variation of forest cover and FPP at landscape level. ",
           "We consider for this estimation only landscapes with more than 20% of forest cover, ",
           "resulting in 7333 landscapes."),
    style = "Normal"
  ) %>%
  body_add_flextable(ft)

print(doc, target = here("supp_table5_landscape_transitions.docx"))
