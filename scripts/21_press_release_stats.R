# Press release descriptive statistics
# Created: 2026-04-08

library(dplyr)
library(readxl)
library(sidrar)

# ── Data ──────────────────────────────────────────────────────────────────────
buf <- read_xlsx("data/tabela_buffer_nova.xlsx")
mun <- read_xlsx("data/tabela_mun_nova.xlsx")

# Derive change variables and categories at landscape scale (mirrors tab_analysis.R)
buf <- buf %>%
  mutate(
    fpp_abs_change     = fpp_2022 - fpp_2010,
    fpp_perc_change    = ((fpp_2022 - fpp_2010) / fpp_2010) * 100,
    forest_abs_change  = perc_forest_2022 - perc_forest_2010,          # pct-points
    forest_perc_change = ((perc_forest_2022 - perc_forest_2010) / perc_forest_2010) * 100,
    cat_change = case_when(
      forest_perc_change > 0 & fpp_perc_change > 0  ~ "GG",
      forest_perc_change > 0 & fpp_perc_change < 0  ~ "GP",
      forest_perc_change < 0 & fpp_perc_change > 0  ~ "PG",
      forest_perc_change < 0 & fpp_perc_change < 0  ~ "PP",
      TRUE ~ "stable"
    )
  )

# Municipality-level change variables (mirrors tab_analysis.R)
mun <- mun %>%
  mutate(
    perc_agrifam_cadunico_2012 = as.numeric(perc_agrifam_cadunico_2012),
    perc_agrifam_cadunico_2022 = as.numeric(perc_agrifam_cadunico_2022),
    mean_fpp_abs_change    = mean_fpp_2022 - mean_fpp_2010,
    mean_fpp_perc_change   = ((mean_fpp_2022 - mean_fpp_2010) / mean_fpp_2010) * 100,
    mean_forest_perc_change = ((mean_perc_forest_2022 - mean_perc_forest_2010) / mean_perc_forest_2010) * 100,
    cat_change = case_when(
      mean_forest_perc_change > 0 & mean_fpp_perc_change > 0  ~ "GG",
      mean_forest_perc_change > 0 & mean_fpp_perc_change < 0  ~ "GP",
      mean_forest_perc_change < 0 & mean_fpp_perc_change > 0  ~ "PG",
      mean_forest_perc_change < 0 & mean_fpp_perc_change < 0  ~ "PP",
      TRUE ~ "stable"
    )
  )

# Working sample: buffers with ≥20% forest cover (consistent with rest of paper)
buf20 <- buf %>% filter(perc_forest_2022 >= 20, !is.na(fpp_2010), !is.na(fpp_2022))

# PPM bovine data for 2006 and 2017 (to match agricultural census years)
ppm_raw <- get_sidra(api = "/t/3939/n6/all/v/105/p/2006,2017/c79/2670")
ppm <- ppm_raw %>%
  mutate(
    code_mun_short = substr(as.character(`Município (Código)`), 1, 6),
    bovinos        = as.numeric(Valor),
    year           = as.integer(Ano)
  ) %>%
  select(code_mun_short, year, bovinos) %>%
  tidyr::pivot_wider(names_from = year, values_from = bovinos,
                     names_prefix = "bovinos_")

mun_ppm <- mun %>%
  mutate(code_mun_short = substr(as.character(code_mun), 1, 6)) %>%
  left_join(ppm, by = "code_mun_short") %>%
  mutate(
    bovino_ha_2006 = bovinos_2006 / area_ha,
    bovino_ha_2017 = bovinos_2017 / area_ha
  )

# Agricultural census (landscape scale) from tabela_geral.csv
tab_old <- read.csv("data/tabela_geral.csv")

cat("\n\n══════════════════════════════════════════════════\n")
cat("PRESS RELEASE STATISTICS\n")
cat("══════════════════════════════════════════════════\n\n")

# ── POINT 1: Forest landscape dynamics ────────────────────────────────────────
cat("── POINT 1: Forest landscape dynamics ──────────────\n")

total_buf20 <- nrow(buf20)

# % of landscapes with at least 10% relative change in forest cover (gain or loss)
n_gain_10 <- sum(buf20$forest_perc_change >= 10,  na.rm = TRUE)
n_loss_10 <- sum(buf20$forest_perc_change <= -10, na.rm = TRUE)

perc_gain_10 <- round(n_gain_10 / total_buf20 * 100, 1)
perc_loss_10 <- round(n_loss_10 / total_buf20 * 100, 1)

cat(sprintf("Total landscapes (≥20%% forest): %d\n", total_buf20))
cat(sprintf("With ≥10%% relative forest GAIN: %d (%.1f%%)\n", n_gain_10, perc_gain_10))
cat(sprintf("With ≥10%% relative forest LOSS: %d (%.1f%%)\n", n_loss_10, perc_loss_10))

n_mun <- nrow(mun)

# ── POINT 2 (renumbered): Rural exodus / depopulation ─────────────────────────
cat("\n── POINT 2: Rural exodus / depopulation ─────────────\n")

n_depop_buf  <- sum(buf20$fpp_perc_change < 0,  na.rm = TRUE)
perc_depop   <- round(n_depop_buf / total_buf20 * 100, 1)

n_PP_buf     <- sum(buf20$cat_change == "PP", na.rm = TRUE)
perc_PP      <- round(n_PP_buf / total_buf20 * 100, 1)

# area experiencing depopulation (in km²)
area_depop_km2 <- buf20 %>%
  filter(fpp_perc_change < 0) %>%
  summarise(area_km2 = sum(area_ha / 100, na.rm = TRUE)) %>%
  pull(area_km2)

# Extrapolate depopulation area to entire biome (same approach as estimates_fpp_buffer.R):
# proportion of ALL buffers with FPP decline × total Caatinga area (IBGE/MMA)
area_bioma_km2 <- 844453
n_buf_total   <- sum(!is.na(buf$fpp_perc_change))
n_buf_depop   <- sum(buf$fpp_perc_change < 0, na.rm = TRUE)
prop_depop_all <- n_buf_depop / n_buf_total
area_depop_km2 <- prop_depop_all * area_bioma_km2

cat(sprintf("Estimated area experiencing depopulation: ~%.0f km² (%.1f%% of biome)\n",
            area_depop_km2, prop_depop_all * 100))

# ── POINT 3: Smallholders replaced by cattle ranchers ─────────────────────────
cat("\n── POINT 3: Smallholders replaced by cattle ranchers ─\n")

# Family farming: agricultural census 2006 vs 2017 (landscape scale)
agrifam_stats <- tab_old %>%
  summarise(
    median_perc_agrifam_2006 = median(perc_agrifam_06, na.rm = TRUE),
    median_perc_agrifam_2017 = median(perc_agrifam_17, na.rm = TRUE),
    n_declined    = sum(agrifam_17 < agrifam_06, na.rm = TRUE),
    n_total       = sum(!is.na(agrifam_06) & !is.na(agrifam_17)),
    perc_declined = round(n_declined / n_total * 100, 1)
  )

cat(sprintf(
  "Median %% of farm establishments that are family farms (agric. census): %.1f%% (2006) → %.1f%% (2017)\n",
  agrifam_stats$median_perc_agrifam_2006, agrifam_stats$median_perc_agrifam_2017
))
cat(sprintf(
  "Landscapes with decline in family farm establishments: %.1f%%\n",
  agrifam_stats$perc_declined
))

# Cattle density: PPM 2006 vs 2017 vs 2022 (municipal scale)
bovino_stats <- mun_ppm %>%
  summarise(
    median_2006 = median(bovino_ha_2006,       na.rm = TRUE),
    median_2022 = median(bovino_hectare_2022,   na.rm = TRUE),
    n_increase  = sum(bovino_hectare_2022 > bovino_ha_2006, na.rm = TRUE),
    n_total     = sum(!is.na(bovino_ha_2006) & !is.na(bovino_hectare_2022)),
    perc_increase = round(n_increase / n_total * 100, 1)
  )

cat(sprintf(
  "Median cattle density (PPM/IBGE): %.3f bov/ha (2006) → %.3f bov/ha (2022)\n",
  bovino_stats$median_2006, bovino_stats$median_2022
))
cat(sprintf(
  "Municipalities with cattle density increase (2006–2022): %.1f%%\n",
  bovino_stats$perc_increase
))

# ── POINT 4: No evidence of development progress ──────────────────────────────
cat("\n── POINT 4: No evidence of development progress ─────\n")

# Among PP municipalities (depopulation + deforestation): did income/health improve?
mun_PP <- mun %>% filter(cat_change == "PP")

n_mun_PP <- nrow(mun_PP)

# % PP municipalities where income still declined or barely grew
median_income_change_PP <- median(
  mun_PP$mean_respRenda_2022 - mun_PP$mean_respRenda_2010, na.rm = TRUE
)

# % PP municipalities where IFDM health also stayed low (< 0.6 in 2022)
n_PP_low_ifdm <- sum(mun_PP$ifdm_saude_2022 < 0.6, na.rm = TRUE)
perc_PP_low_ifdm <- round(n_PP_low_ifdm / n_mun_PP * 100, 1)

# Correlation between FPP % change and IFDM change across all municipalities
mun_for_cor <- mun %>%
  filter(!is.na(mean_fpp_perc_change), !is.na(ifdm_saude_2022), !is.na(ifdm_saude_2013)) %>%
  mutate(ifdm_change = ifdm_saude_2022 - ifdm_saude_2013)

cor_fpp_ifdm <- cor(mun_for_cor$mean_fpp_perc_change, mun_for_cor$ifdm_change,
                    use = "complete.obs")

cat(sprintf("PP municipalities (depopulation + deforestation): %d\n", n_mun_PP))
cat(sprintf("  with IFDM-Saúde < 0.6 in 2022: %d (%.1f%%)\n",
            n_PP_low_ifdm, perc_PP_low_ifdm))
cat(sprintf(
  "  Median change in household income (resp. renda): R$%.0f (2010→2022)\n",
  median_income_change_PP
))
cat(sprintf(
  "Correlation between FPP change and health index change (all mun.): r = %.2f\n",
  cor_fpp_ifdm
))

cat("\n══════════════════════════════════════════════════\n")
cat("Done.\n")
