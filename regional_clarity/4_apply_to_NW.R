library(feather)
library(tidyverse)
library(xgboost)
library(Metrics)
library(ggthemes)

# basic theme for all ggplots, if Roboto is not installed, just use default, but message
if ({
  require(systemfonts)
  ("Roboto" %in% system_fonts()$family)
}) {
  ROSS_theme <- theme_bw() + #or theme_few()
    theme(plot.title = element_text(hjust = 0.5, face = 'bold', family = "Roboto", size = 24),
          plot.subtitle = element_text(hjust = 0.5, family = "Roboto", size = 20),
          axis.title = element_text(family = "Roboto", size = 16),
          strip.text = element_text(family = "Roboto", size = 12),
          legend.text = element_text(family = "Roboto", size = 12),
          legend.title = element_blank(),
          axis.text = element_text(family = "Roboto", size = 12)) 
} else {
  message("You do not have the Roboto font family installed on your computer, currenly using ggplot default text family.
          See ROSS_themes.R for directions to install the font family on your computer.")
  ROSS_theme <- theme_bw() + #or theme_few()
    theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
          plot.subtitle = element_text(hjust = 0.5)) 
}

NW_RS <- read_feather("../NW-CLP-RS/b_site_RS_data_acquisition/out/NW_Poudre_Historical_collated_DSWE1_point_meta_v2024-10-10.feather") %>% 
  mutate(across(c(pCount_dswe1, pCount_dswe_gt0, prop_clouds, 
                  med_Red, med_Blue, med_Green, med_Nir, med_Swir1, med_Swir2),
                ~ as.numeric(.))) %>% 
  filter(pCount_dswe1/pCount_dswe_gt0 > .5 & pCount_dswe1 > 8 & prop_clouds == 0) 

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
NW_RS_corr <- NW_RS %>% 
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
NW_RS_ml <- NW_RS_corr %>% 
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

feats <-  NW_RS_ml %>% 
  select(red_corr7:GN_GN) %>% 
  names(.)

dNW <- xgb.DMatrix(data = as.matrix(NW_RS_ml[,feats]))


# load models -------------------------------------------------------------

xgb.1 <- xgb.load("modeling/regional_clarity/xg_models/prelim/xgb1_noNW.model")
xgb.2 <- xgb.load("modeling/regional_clarity/xg_models/prelim/xgb2_noNW.model")
xgb.3 <- xgb.load("modeling/regional_clarity/xg_models/prelim/xgb3_noNW.model")
xgb.4 <- xgb.load("modeling/regional_clarity/xg_models/prelim/xgb4_noNW.model")

NW_pred <- NW_RS_ml %>% 
  mutate(pred1 = predict(xgb1, dNW),
         pred2 = predict(xgb2, dNW),
         pred3 = predict(xgb3, dNW),
         pred4 = predict(xgb4, dNW)) %>% 
  rowwise() %>% 
  mutate(mean = sum(pred1, pred2, pred3, pred4)/4) %>% 
  ungroup() %>% 
  select(rowid, r_id, date, mission, permanent_identifier,gnis_name, data_group,
         pred1, pred2, pred3, pred4, mean) %>% 
  filter(grepl("NW", data_group))


# read in harmonized secchi -----------------------------------------------

NW_sdd <- read_csv("data/waterQuality/harmonized/Secchi_data_NW_harmonized_2024-01-01.csv") %>% 
  mutate(GNIS_Name = case_when(feature == "Carter Lake" ~ "Carter Lake Reservoir",
                               feature == "Shadow Mountain Reservoir" ~ "Shadow Mountain Lake",
                               feature == "Granby Reservoir" ~ "Lake Granby", 
                               TRUE ~ feature))

res_names <- unique(NW_sdd$GNIS_Name)

NW_pred_fewer <- NW_pred %>% 
  filter(gnis_name %in% res_names)

unique(NW_pred_fewer$gnis_name)

NW_pred_less <- NW_pred_fewer %>% 
  select(c(date, pred1, pred2, pred3, pred4, mean, gnis_name)) %>% 
  left_join(., NW_sdd %>% select(gnis_name = GNIS_Name, feature) %>% unique()) %>% 
  summarise(predicted = mean(mean), .by = c(date, gnis_name, feature))

NW_obs_less <- NW_sdd %>% 
  filter(GNIS_Name %in% unique(NW_pred_less$gnis_name)) %>% 
  summarize(observed = mean(value), .by = c(date, GNIS_Name, feature)) %>% 
  rename(gnis_name = GNIS_Name)

full_join(NW_pred_less, NW_obs_less) %>% 
ggplot(., aes(x = observed, y = predicted, color = feature)) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_y_continuous(limits = c(0, 8)) +
  scale_x_continuous(limits = c(0, 8)) +
  labs(x = "measured SDD (m)",
       y = "estimated SDD (m)") +
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  theme_bw() +
  scale_color_manual(values = c("#002EA3", "#E70870", "#256BF5", 
                                "#745CFB", "#1E4D2B", "#56104E", "black")) +
  ROSS_theme +
  theme(legend.position = "bottom")

evals_NW <- full_join(NW_pred_less, NW_obs_less) %>% 
  filter(!is.na(observed&predicted)) %>%
  summarise(rmse = rmse(observed, predicted),
            mae = mae(observed, predicted),
            mape = mape(observed, predicted),
            bias = bias(observed, predicted),
            p.bias = percent_bias(observed, predicted),
            smape = smape(observed, predicted),
            r2 = cor(observed, predicted)^2)
evals_NW

NW_pred_obs <- full_join(NW_pred_less, NW_obs_less) %>% 
  pivot_longer(c(predicted, observed),
               names_to = "mean",
               values_to = "values")

NW_pred_obs %>% 
  filter(year(date) >= 2015) %>% 
  ggplot(., aes(x = date, y = values, color = mean)) +
  geom_point(size = 2.5, alpha = 0.5) +
  scale_color_manual(values = c("#002EA3", "#E70870", "#256BF5", 
                       "#745CFB", "#1E4D2B", "#56104E")) +
  facet_grid(feature ~ ., labeller = label_wrap_gen(10)) +
  labs(x = NULL, y = "Secchi Depth (m)", color = "data origin") +
  theme_bw() +
  ROSS_theme +
  theme(legend.position = "bottom")

write_csv(NW_pred_obs %>% filter(!is.na(values)),
          "modeling/regional_clarity/preliminary_RS_pred_obs.csv")
