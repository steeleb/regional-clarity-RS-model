library(tidyverse)
library(xgboost)
library(Metrics)
library(ggthemes)
library(here)
library(arrow)

# basic theme for all ggplots, if Roboto is not installed, just use default, but message
if ({
  require(systemfonts)
  ("Roboto" %in% system_fonts()$family)
}) {
  ROSS_theme <- theme_bw() + #or theme_few()
    theme(plot.title = element_text(hjust = 0.5, face = 'bold', family = "Roboto", size = 24),
          plot.subtitle = element_text(hjust = 0.5, family = "Roboto", size = 20),
          axis.title = element_text(family = "Roboto", size = 20),
          strip.text = element_text(family = "Roboto", size = 16),
          legend.text = element_text(family = "Roboto", size = 16),
          legend.title = element_blank(),
          axis.text = element_text(family = "Roboto", size = 16)) 
} else {
  message("You do not have the Roboto font family installed on your computer, currenly using ggplot default text family.
          See ROSS_themes.R for directions to install the font family on your computer.")
  ROSS_theme <- theme_bw() + #or theme_few()
    theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
          plot.subtitle = element_text(hjust = 0.5)) 
}

CLP_RS <- read_feather(here("../NW-CLP-RS/b_site_RS_data_acquisition/out/NW_Poudre_Historical_collated_DSWE1_point_meta_v2024-10-10.feather")) %>% 
  mutate(across(c(pCount_dswe1, pCount_dswe_gt0, prop_clouds, 
                  med_Red, med_Blue, med_Green, med_Nir, med_Swir1, med_Swir2),
                ~ as.numeric(.))) %>% 
  filter(pCount_dswe1/pCount_dswe_gt0 > .5 & 
           pCount_dswe1 > 8 & 
           prop_clouds == 0 &
           CLOUD_COVER < 50 &
           grepl("CLP", data_group)) 

# apply corrections -------------------------------------------------------

jg_corr <- read_csv("~/OneDrive - Colostate/misc/gardner_data/LC02_Corr_Coef.csv") %>% 
  # pivot for easier application
  pivot_longer(cols = c(intercept, coef1, coef2), 
               names_to = "int_coef", 
               values_to = "value") %>%
  mutate(new_column = paste(band, int_coef, sep = "_")) %>%
  select(-band, -int_coef) %>%
  pivot_wider(names_from = new_column, 
              values_from = value)

# apply coefficients to timeseries data
CLP_RS_corr <- CLP_RS %>% 
  mutate(sat = case_when(mission == "LANDSAT_4" ~ "LT05", # landsat 4 is roughly the same as 5
                         mission == "LANDSAT_5" ~ "LT05",
                         mission == "LANDSAT_7" ~ "LE07",
                         mission == "LANDSAT_8" ~ "LC08",
                         mission == "LANDSAT_9" ~ "LC08", # landsat 9 is roughly the same as 8
                         TRUE ~ NA_character_)) %>% 
  left_join(., jg_corr) %>% 
  mutate(red_corr7 = Red_intercept + Red_coef1*med_Red + Red_coef2*med_Red^2,
         green_corr7 = Green_intercept + Green_coef1*med_Green + Green_coef2*med_Green^2,
         blue_corr7 = Blue_intercept + Blue_coef1*med_Blue + Blue_coef2*med_Blue^2,
         nir_corr7 = Nir_intercept + Nir_coef1*med_Nir + Nir_coef2*med_Nir^2,
         swir1_corr7 = Swir1_intercept + Swir1_coef1*med_Swir1 + Swir1_coef2*med_Swir1^2,
         swir2_corr7 = Swir2_intercept + Swir2_coef1*med_Swir2 + Swir2_coef2*med_Swir2^2) %>% 
  select(-c(all_of(names(jg_corr))))

## add indices ----
CLP_RS_ml <- CLP_RS_corr %>% 
  mutate(NR = nir_corr7/red_corr7, 
         BR = blue_corr7/red_corr7,
         GR = green_corr7/red_corr7,
         SR = swir1_corr7/red_corr7,
         BG = blue_corr7/green_corr7,
         RG = red_corr7/green_corr7,
         NG = nir_corr7/green_corr7,
         SG = swir1_corr7/green_corr7,
         BN = blue_corr7/nir_corr7,
         GN = green_corr7/nir_corr7,
         RN = red_corr7/nir_corr7,
         SN = swir1_corr7/nir_corr7,
         BS = blue_corr7/swir1_corr7,
         GS = green_corr7/swir1_corr7,
         RS = red_corr7/swir1_corr7,
         NS = nir_corr7/swir1_corr7,
         R_GN = red_corr7/(green_corr7 + nir_corr7),
         R_GB = red_corr7/(green_corr7 + blue_corr7),
         R_GS = red_corr7/(green_corr7 + swir1_corr7),
         R_BN = red_corr7/(blue_corr7 + nir_corr7),
         R_BS = red_corr7/(blue_corr7 + swir1_corr7),
         R_NS = red_corr7/(nir_corr7 + swir1_corr7),
         G_BR = green_corr7/(blue_corr7 + swir1_corr7),
         G_BN = green_corr7/(blue_corr7 + nir_corr7),
         G_BS = green_corr7/(blue_corr7 + swir1_corr7),
         G_RN = green_corr7/(red_corr7 + nir_corr7),
         G_RB = green_corr7/(red_corr7 + blue_corr7),
         G_NS = green_corr7/(nir_corr7 + swir1_corr7),
         B_RG = blue_corr7/(red_corr7 + green_corr7),
         B_RS = blue_corr7/(red_corr7 + swir1_corr7),
         B_GN = blue_corr7/(green_corr7 + nir_corr7),
         B_GS = blue_corr7/(green_corr7 + swir1_corr7),
         B_NS = blue_corr7/(nir_corr7 + swir1_corr7),
         N_RG = nir_corr7/(red_corr7 + green_corr7),
         N_RB = nir_corr7/(red_corr7 + blue_corr7),
         N_RS = nir_corr7/(red_corr7 + swir1_corr7),
         N_GB = nir_corr7/(green_corr7 + blue_corr7),
         N_GS = nir_corr7/(green_corr7 + nir_corr7),
         N_BS = nir_corr7/(blue_corr7 + swir1_corr7),
         GR_2 = (red_corr7 + green_corr7)/2,
         GN_2 = (nir_corr7 + green_corr7)/2,
         BR_G = (blue_corr7 - red_corr7)/green_corr7,
         NS_NR = (nir_corr7 - swir1_corr7)/(red_corr7 - swir1_corr7),
         fai = nir_corr7 - (red_corr7 + (swir1_corr7 - red_corr7) * ((830-660)/(1650-660))),
         NmS = nir_corr7 - swir1_corr7,
         NmR = nir_corr7 - red_corr7,
         NDVI = (nir_corr7 - red_corr7)/(nir_corr7 + red_corr7),
         NDWI = (green_corr7 - swir1_corr7)/(green_corr7 + swir1_corr7),
         NDSSI = (blue_corr7 - nir_corr7)/(blue_corr7 + nir_corr7),
         GN_GN = (green_corr7- nir_corr7)/(green_corr7 + nir_corr7)) %>% 
  filter_all(all_vars(!is.infinite(.)))

feats <-  CLP_RS_ml %>% 
  select(red_corr7:GN_GN) %>% 
  names(.)

dCLP <- xgb.DMatrix(data = as.matrix(CLP_RS_ml[,feats]))


# load models -------------------------------------------------------------

xgb.1 <- xgb.load("regional_clarity/xg_models/prelim/xgb1_noNW.model")
xgb.2 <- xgb.load("regional_clarity/xg_models/prelim/xgb2_noNW.model")
xgb.3 <- xgb.load("regional_clarity/xg_models/prelim/xgb3_noNW.model")
xgb.4 <- xgb.load("regional_clarity/xg_models/prelim/xgb4_noNW.model")

CLP_pred <- CLP_RS_ml %>% 
  mutate(pred1 = predict(xgb.1, dCLP),
         pred2 = predict(xgb.2, dCLP),
         pred3 = predict(xgb.3, dCLP),
         pred4 = predict(xgb.4, dCLP)) %>% 
  rowwise() %>% 
  mutate(mean_sdd = sum(pred1, pred2, pred3, pred4)/4) %>% 
  ungroup() %>% 
  select(rowid, r_id, date, mission, permanent_identifier, gnis_name, data_group,
         red_corr7, green_corr7, blue_corr7, nir_corr7, swir1_corr7, swir2_corr7,
         pred1, pred2, pred3, pred4, mean_sdd)

# filter for waterbodies of interest
these_res <- CLP_pred %>% 
  filter(grepl("ROSS_CLP", data_group)) %>% 
  pull(gnis_name) %>% 
  unique() %>% 
  .[!(. %in% c("Horsetooth Reservoir", 
               "Shadow Mountain Lake",
               "Grand Lake"))]

CLP_focus <- CLP_pred %>% 
  filter(gnis_name %in% these_res)

# do a quick look - limit to may through october to reduce ice risk
# Halligan and Seaman have data year-round for some reason.

CLP_focus %>% 
  filter(between(month(date), 5, 10)) %>% 
  ggplot(., aes(x = yday(date), y = mean_sdd)) +
  geom_point() +
  facet_grid(gnis_name ~ .)

write_parquet(CLP_focus, here("data/derived/remote_sensing/landsat_sdd/CLP_SDD_estimate_v2024-10-10.parquet"))

