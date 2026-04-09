# Press release – Development challenge statistics
# Created: 2026-04-08
# Sources: Atlas Brasil / IPEA (IDHM municipal 2010); UNDP HDR Statistical Annex (HDI trends)

library(dplyr)
library(readxl)
library(ipeadatar)

# ── Caatinga municipality list ─────────────────────────────────────────────────
mun <- read_xlsx("data/tabela_mun_nova.xlsx") %>%
  mutate(code_mun_short = substr(as.character(code_mun), 1, 6))

# ── IDHM 2010 (Atlas Brasil via IPEA) ─────────────────────────────────────────
idhm_raw <- ipeadata("ADH_IDHM", language = "br")

idhm_2010 <- idhm_raw %>%
  filter(date == "2010-01-01", nchar(as.character(tcode)) == 7) %>%
  mutate(code_mun_short = substr(as.character(tcode), 1, 6)) %>%
  select(code_mun_short, idhm_2010 = value)

caat_idhm <- mun %>%
  left_join(idhm_2010, by = "code_mun_short")

# ── Global HDI 2010 (UNDP HDR Statistical Annex – Table 2) ────────────────────
hdr_raw <- read_xlsx("/tmp/hdr_trends.xlsx", sheet = "Table 2", skip = 4)

hdr_2010 <- hdr_raw %>%
  transmute(
    rank     = as.numeric(`HDI rank`),
    country  = Country,
    hdi_2010 = suppressWarnings(as.numeric(substr(as.character(`2010`), 1, 5)))
  ) %>%
  filter(!is.na(country), !is.na(hdi_2010),
         !grepl("HUMAN DEVEL|DEVELOPMENT", country, ignore.case = TRUE)) %>%
  arrange(hdi_2010)

# ── Results ────────────────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════\n")
cat("DEVELOPMENT CHALLENGE – CAATINGA MUNICIPALITIES\n")
cat("══════════════════════════════════════════════════\n\n")

mean_idhm   <- mean(caat_idhm$idhm_2010, na.rm = TRUE)
median_idhm <- median(caat_idhm$idhm_2010, na.rm = TRUE)
n_matched   <- sum(!is.na(caat_idhm$idhm_2010))

cat(sprintf("Municipalities matched: %d of %d\n\n", n_matched, nrow(mun)))
cat(sprintf("Mean   IDHM (Atlas Brasil, 2010): %.3f\n", mean_idhm))
cat(sprintf("Median IDHM (Atlas Brasil, 2010): %.3f\n\n", median_idhm))
cat(sprintf("Brazil national IDHM (2010): 0.727\n\n"))

# Countries closest to Caatinga mean IDHM
cat("── Countries with equivalent HDI (UNDP, 2010) ──────\n")
hdr_2010 %>%
  mutate(diff = abs(hdi_2010 - mean_idhm)) %>%
  arrange(diff) %>%
  slice(1:8) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  print()

cat("\n── Countries in HDI range 0.565–0.620 (2010) ───────\n")
hdr_2010 %>%
  filter(hdi_2010 >= 0.565, hdi_2010 <= 0.620) %>%
  arrange(hdi_2010) %>%
  print()

cat("\n══════════════════════════════════════════════════\n")
cat("Done.\n")
