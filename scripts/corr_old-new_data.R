# Thu Aug 21 16:03:42 2025 ------------------------------
# Scritp to test the correlation of old and new data after reviewer two adn three requests

#libraries----
library(readxl)

#data----
read_xlsx(path = "data/tab_mun_old.xlsx") -> tab_mun_old
read_xlsx(path = "data/tabela_mun_nova.xlsx") -> tab_mun_nova

##organisation----
tab_mun_old %>% 
  dplyr::select(code_muni, mean_nvc_2010, sum_fpp_2010, perc_area_agrifam_17,
                pop_urb_2010, IDHM_L_2010, expov_2010, gini_2010, U5mort_2010) %>% 
  glimpse ->tab_corr

tab_mun_nova %>% 
  dplyr::select(code_mun, mean_perc_forest_2010, mean_pop_2010, perc_agrifam_cadunico_2017,
                perc_popUrb_2010, ifdm_saude_2013, perc_pessoas_cadunico_2012,
                taxa_u5mort_2010) %>% 
  mutate(code_muni = as.double(code_mun)) %>% 
  right_join(y = tab_corr) %>% 
  glimpse -> tab_corr

tibble(
  var_x = c("mean_perc_forest_2010", "mean_pop_2010", "perc_agrifam_cadunico_2017", "perc_popUrb_2010", "ifdm_saude_2013", "perc_pessoas_cadunico_2012", "taxa_u5mort_2010"),
  var_y = c("mean_nvc_2010", "sum_fpp_2010", "perc_area_agrifam_17", "pop_urb_2010", "IDHM_L_2010", "expov_2010", "U5mort_2010")
  ) -> pairs

pairs %>% 
  mutate(cor = map2_dbl(var_x, var_y, ~ cor(tab_corr[[.x]], tab_corr[[.y]], use = "complete.obs"))) -> results

results
