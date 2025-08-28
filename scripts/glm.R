# Thu Feb  9 14:48:33 2023 ------------------------------
#scritpt para modelos lineares

#Libraries----
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(effects)
library(geobr)
library(spdep)
library(tmap)
library(spatialreg)

#Data----
read.csv(file = here("data/tabela_geral.csv")) -> tab_geral
read_xlsx(path = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap1/dbcap1_rma.xlsx") -> dbcap1_rma
read_xlsx(path = "/Users/user/Library/CloudStorage/OneDrive-Personal/Documentos/Doutorado/tese/cap3/data/atlas_dadosbrutos_00_10.xlsx", 
          sheet = 2) -> dados_atlas


##manipulation----
dados_atlas %>% 
  select(i,
         Codmun7,
         pesourb
         ) %>% 
  pivot_wider(names_from = i,
              values_from = pesourb,
              names_prefix = "pop_urb_"
              ) %>% 
  glimpse -> pop_urb

tab_geral %>% 
  select(buff_id,
         code_muni,
         code_uf,
         pland_nvc_06,
         pland_nvc_17, 
         vari_perc_nvc, 
         pop_rural_WP_06,
         pop_rural_WP_17,
         vari_perc_pop_rural, 
         perc_area_agrifam_06, 
         perc_area_agrifam_17
         ) %>% 
  left_join(y = select(dbcap1_rma,
                       code_muni,
                       IDHM_L_2000, 
                       IDHM_L_2010, 
                       expov_2000,
                       expov_2010, 
                       gini_2000, 
                       gini_2010, 
                       u5mort_2000, 
                       U5mort_2010
                       ),
            by = "code_muni"
            ) %>%
  left_join(y = pop_urb, 
            by = c("code_muni" = "Codmun7")
            ) %>%
  group_by(code_muni, code_uf) %>%
  summarise(mean_nvc_2000 = mean(pland_nvc_06),
            mean_nvc_2010 = mean(pland_nvc_17),
            # mean_change_nvc = mean(vari_perc_nvc),
            mean_change_nvc_mun = mean_nvc_2010 - mean_nvc_2000,
            sum_fpp_2000 = sum(pop_rural_WP_06),
            sum_fpp_2010 = sum(pop_rural_WP_17),
            # mean_change_fpp = mean(vari_perc_pop_rural),
            mean_change_fpp_mun = ((sum_fpp_2010/sum_fpp_2000)-1)*100,
            across(.cols = perc_area_agrifam_06:pop_urb_2010, .fns = mean)
            ) %>% 
  #filter(!is.na(.$code_muni)
   #      ) %>%
  na.omit() %>% 
  glimpse -> tab_mun_old

# writexl::write_xlsx(x = tab_mun_old, path = "data/tab_mun_old.xlsx")

### Table for absolute change in development indicators----
tab_mun %>%
  mutate(code_muni = code_muni,
         code_uf = as.factor(code_uf),
         change_agrifam = perc_area_agrifam_17 - perc_area_agrifam_06,
         change_popUrb = pop_urb_2010 - pop_urb_2000,
         change_idhL = IDHM_L_2010 - IDHM_L_2000,
         change_expov = expov_2010 - expov_2000,
         change_gini = gini_2010 - gini_2000,
         change_u5mort = U5mort_2010 - u5mort_2000,
         ) %>% 
  rename(mean_change_nvc = mean_change_nvc_mun,
         mean_change_fpp = mean_change_fpp_mun,
         agrifam_2000 = perc_area_agrifam_06,
         agrifam_2010 = perc_area_agrifam_17
         ) %>% 
  mutate(cat_change = if_else(condition = mean_change_nvc > 0 & mean_change_fpp > 0,
                              true = "GG",
                              false = if_else(condition = mean_change_nvc > 0 & mean_change_fpp < 0,
                                              true = "GP",
                                              false = if_else(condition =  mean_change_nvc < 0 & mean_change_fpp > 0,
                                                              true = "PG",
                                                              false = if_else(mean_change_nvc < 0 & mean_change_fpp < 0,
                                                                              true = "PP",
                                                                              false = "stable"
                                                                              )
                                                              )
                                              )
                              ),
         .before = 2
         ) %>%
  filter(cat_change != "stable") %>%
  glimpse -> tab_abs_change_mun

#Analysis----
##FPP change all ----
###linear models----
hist(tab_abs_change_mun$mean_change_fpp, breaks = 20)

glm(data = tab_abs_change_mun, 
    mean_change_fpp ~ 
      # agrifam_2000 + 
      # pop_urb_2000 + 
      # IDHM_L_2000 + 
      # expov_2000 + 
      # gini_2000 + 
      # u5mort_2000 +
      change_agrifam +
      change_popUrb +
      change_idhL +
      change_expov +
      change_gini +
      change_u5mort 
    ) -> glm.fpp_base


par(mfrow = c(2,2))
plot(glm.fpp_base) #
summary(glm.fpp_base)

###Testing spatial autocorrelation----
read_municipality(simplified = F)->mun_cat

tab_abs_change_mun %>% 
  left_join(y = select(mun_cat,
                       code_muni,
                       geom),
            by = "code_muni"
  ) %>% 
  glimpse -> tab_abs_change_mun_map

poly2nb(tab_abs_change_mun_map$geom, 
        queen=TRUE
) -> mat_dist_abs_change_mun

nb2listw(mat_dist_abs_change_mun,
         zero.policy = T) -> mat_dist_list_abs_change_mun

lm.morantest(glm.fpp_base,
             mat_dist_list_abs_change_mun,
             alternative = "two.sided",
             zero.policy = T) #found significant positive spatial autocorrelation
                              #try GWR models in other script
residuals(glm.fpp_base) -> resdis_glm_fpp_base
cbind(tab_abs_change_mun_map, resdis_glm_fpp_base)-> tab_abs_change_mun_map
qtm(st_as_sf(tab_abs_change_mun_map),
    fill = "...29")

###spatial models----
lm.LMtests(model = glm.fpp_base,
           listw = mat_dist_list_abs_change_mun,
           test = "all",
           zero.policy = T
           ) %>% 
  summary 

errorsarlm(formula = mean_change_fpp ~ 
             # agrifam_2000 + 
             # pop_urb_2000 + 
             # IDHM_L_2000 + 
             # expov_2000 + 
             # gini_2000 + 
             # u5mort_2000 +
             change_agrifam +
             change_popUrb +
             change_idhL +
             change_expov +
             change_gini +
             change_u5mort,
           data = tab_abs_change_mun,
           listw = mat_dist_list_abs_change_mun,
           etype="emixed",
           zero.policy = T
           ) -> SDEM_fpp
summary(SDEM_fpp)

##nvc change ----
glm(data = tab_abs_change_mun, 
    mean_change_nvc ~ 
      # agrifam_2000 + 
      # pop_urb_2000 + 
      # IDHM_L_2000 + 
      # expov_2000 + 
      # gini_2000 + 
      # u5mort_2000 +
      change_agrifam +
      change_popUrb +
      change_idhL +
      change_expov +
      change_gini +
      change_u5mort 
) -> glm.nvc_base

plot(glm.nvc_base)
summary(glm.nvc_base)

###testing spatial autocorrelation----
lm.morantest(glm.nvc_base,
             mat_dist_list_abs_change_mun,
             alternative = "two.sided",
             zero.policy = T)

###spatial models----
lm.LMtests(model = glm.nvc_base,
           listw = mat_dist_list_abs_change_mun,
           test = "all",
           zero.policy = T
) %>% 
  summary 

errorsarlm(formula = mean_change_nvc ~ 
             # agrifam_2000 + 
             # pop_urb_2000 + 
             # IDHM_L_2000 + 
             # expov_2000 + 
             # gini_2000 + 
             # u5mort_2000 +
             change_agrifam +
             change_popUrb +
             change_idhL +
             change_expov +
             change_gini +
             change_u5mort,
           data = tab_abs_change_mun,
           listw = mat_dist_list_abs_change_mun,
           etype="emixed",
           zero.policy = T
           )-> SDEM_nvc

summary(SDEM_nvc)

#forest cover and fpp change correlation----
glm(data = tab_abs_change_mun_map, mean_change_nvc ~ mean_change_fpp) -> glm.fpp_nvc
summary(glm.fpp_nvc)

lm.morantest(glm.fpp_nvc,
             mat_dist_list_abs_change_mun,
             alternative = "two.sided",
             zero.policy = T)

lm.LMtests(model = glm.fpp_nvc,
           listw = mat_dist_list_abs_change_mun,
           test = "all",
           zero.policy = T
) %>% 
  summary 

errorsarlm(formula = mean_change_nvc ~
             mean_change_fpp,
           data = tab_abs_change_mun_map,
           listw = mat_dist_list_abs_change_mun,
           zero.policy = T
)-> SEM_fpp_nvc

summary(SEM_fpp_nvc)
