library(feather)
library(tidyverse)
library(xgboost)
library(Metrics)

split_dataframes <- read_feather("modeling/regional_clarity/split_filtered_regional.feather")

# get the permanent_ids of the norther waterbodies and remove those data from the training


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
split_dataframes_corr <- split_dataframes %>% 
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
split_dataframes_ml <- split_dataframes_corr %>% 
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
         GN_GN = (green_corr7- nir_corr7)/(green_corr7 + nir_corr7))


# create train/val split --------------------------------------------------

train_val <- split_dataframes_ml %>% 
  filter(part != 1)

train1 <- train_val %>% 
  filter(part != 2)
val1 <- anti_join(train_val, train1)

train2 <- train_val %>% 
  filter(part != 3)
val2 <- anti_join(train_val, train2)

train3 <- train_val %>% 
  filter(part != 4)
val3 <- anti_join(train_val, train3)

train4<- train_val %>% 
  filter(part != 5)
val4 <- anti_join(train_val, train4)


# train model -------------------------------------------------------------

target <- "harmonized_value"

feats <-  train_val %>% select(red_corr7:GN_GN) %>% names(.)

dtrain1 <- xgb.DMatrix(data = as.matrix(train1[,feats]), 
                       label = train1$harmonized_value)

dval1 <- xgb.DMatrix(data = as.matrix(val1[,feats]), 
                     label = val1$harmonized_value)

dtrain2 <- xgb.DMatrix(data = as.matrix(train2[,feats]), 
                       label = train2$harmonized_value)

dval2 <- xgb.DMatrix(data = as.matrix(val2[,feats]), 
                     label = val2$harmonized_value)

dtrain3 <- xgb.DMatrix(data = as.matrix(train3[,feats]), 
                       label = train3$harmonized_value)

dval3 <- xgb.DMatrix(data = as.matrix(val3[,feats]), 
                     label = val3$harmonized_value)

dtrain4 <- xgb.DMatrix(data = as.matrix(train4[,feats]), 
                       label = train4$harmonized_value)

dval4 <- xgb.DMatrix(data = as.matrix(val4[,feats]), 
                     label = val4$harmonized_value)

# create test split -------------------------------------------------------

test <- split_dataframes_ml %>% 
  filter(part == 1)

dtest <- xgb.DMatrix(data = as.matrix(test[,feats]), 
                     label = test$harmonized_value)

## just get an idea of what we're looking at in terms of error ----
params <- list(booster = "gbtree", 
               objective = 'reg:squarederror', 
               eta=0.01,
               max_depth=7, 
               min_child_weight=1,
               subsample=0.8, 
               colsample_bytree=0.5)

xgb.naive <- xgb.train(params = params, data = dtrain1, nrounds = 2000, 
                       watchlist = list(train = dtrain1, val = dval1), 
                       verbose = 1,
                       early_stopping_rounds = 100)

importance <- xgb.importance(feats, xgb.naive)
xgb.plot.importance(importance_matrix = importance)

params_linear <- list(booster = "gblinear", 
               objective = 'reg:squarederror', 
               eta=0.1,
               max_depth=7, 
               min_child_weight=1,
               subsample=0.8, 
               colsample_bytree=0.5)

xgb.naive.2 <- xgb.train(params = params_linear, 
                         data = dtrain1, 
                         nrounds = 5000, 
                         watchlist = list(train = dtrain1, val = dval1), 
                         verbose = 1,
                         early_stopping_rounds = 100)

importance.2 <- xgb.importance(feats, xgb.naive.2)
xgb.plot.importance(importance_matrix = importance.2)


param_test1 <- expand.grid(
  booster = "gbtree", 
  objective = 'reg:squarederror', 
  eta=0.1,
  max_depth = c(3, 5, 7, 9),
  min_child_weight = c(1, 3, 5, 7),
  subsample=0.8, 
  colsample_bytree=0.5
) %>% 
  rowid_to_column()

hypertune_xgboost = function(train, val, test_id, grid){
  
  params <- list(booster = "gbtree", 
                 objective = 'reg:squarederror', 
                 eta=grid$eta,
                 max_depth=grid$max_depth, 
                 min_child_weight=grid$min_child_weight,
                 subsample=grid$subsample, 
                 colsample_bytree=grid$colsample_bytree)
  
  xgb <- xgb.train(params = params, data = train, nrounds = 5000, 
                   watchlist = list(train = train, val = val), 
                   verbose = 0,
                   early_stopping_rounds = 100)
  
  xgb.save(xgb, paste0("modeling/regional_clarity/xg_models/hypertune/xgb_", test_id, "_", grid$rowid, ".model"))
  
  summary <- grid %>% mutate(val_loss = xgb$best_score, 
                             best_message = xgb$best_msg) 
  
  return(summary) 
  
}

## Hypertune xgboost for max depth and child weight
xgboost_hypertune <- param_test1 %>%
  pmap_dfr(function(...) {
    current <- tibble(...)
    hypertune_xgboost(dtrain1, dval1, "tune1", current)
  })

xgboost_hypertune %>% 
  arrange(-val_loss)

# second round of hypertune
param_test2 <- expand.grid(
  booster = "gbtree", 
  objective = 'reg:squarederror', 
  eta=0.1,
  max_depth = c(2,3,4),
  min_child_weight = c(2,3,4),
  subsample=0.8, 
  colsample_bytree=0.5
) %>% 
  rowid_to_column()

xgboost_hypertune2 <- param_test2 %>%
  pmap_dfr(function(...) {
    current <- tibble(...)
    hypertune_xgboost(dtrain1, dval1, "tune2", current)
  })

xgboost_hypertune2 %>% 
  arrange(-val_loss)


# train and cross validate ------------------------------------------------

params <- list(booster = "gbtree", 
               objective = 'reg:squarederror', 
               eta=0.01,
               max_depth=3, 
               min_child_weight=3,
               subsample=0.8, 
               colsample_bytree=0.5)

set.seed(47)
xgb1 <- xgb.train(params = params, data = dtrain1, nrounds = 2000, 
                 watchlist = list(train = dtrain1, val = dval1), 
                 verbose = 1,
                 early_stopping_rounds = 50)
xgb.save(xgb1, "modeling/regional_clarity/xg_models/prelim/xgb1.model")
xgb2 <- xgb.train(params = params, data = dtrain2, nrounds = 2000, 
                  watchlist = list(train = dtrain2, val = dval2), 
                  verbose = 1,
                  early_stopping_rounds = 50)
xgb.save(xgb2, "modeling/regional_clarity/xg_models/prelim/xgb2.model")
xgb3 <- xgb.train(params = params, data = dtrain3, nrounds = 2000, 
                  watchlist = list(train = dtrain3, val = dval3), 
                  verbose = 1,
                  early_stopping_rounds = 50)
xgb.save(xgb3, "modeling/regional_clarity/xg_models/prelim/xgb3.model")
xgb4 <- xgb.train(params = params, data = dtrain4, nrounds = 2000, 
                  watchlist = list(train = dtrain4, val = dval4), 
                  verbose = 1,
                  early_stopping_rounds = 50)
xgb.save(xgb4, "modeling/regional_clarity/xg_models/prelim/xgb4.model")

pred_1 <- val1 %>% 
  mutate(pred1 = predict(xgb1, dval1),
         pred2 = predict(xgb2, dval1),
         pred3 = predict(xgb3, dval1),
         pred4 = predict(xgb4, dval1)) %>% 
  rowwise() %>% 
  mutate(mean = sum(pred1, pred2, pred3, pred4)/4)

ggplot(pred_1, aes(x = harmonized_value, y = mean)) + 
  geom_point() +
  geom_point(aes(y = pred1), color = "yellow") +
  theme_bw()

pred_2 <- val2 %>% 
  mutate(pred1 = predict(xgb1, dval2),
         pred2 = predict(xgb2, dval2),
         pred3 = predict(xgb3, dval2),
         pred4 = predict(xgb4, dval2)) %>% 
  rowwise() %>% 
  mutate(mean = sum(pred1, pred2, pred3, pred4)/4)

ggplot(pred_2, aes(x = harmonized_value, y = mean)) + 
  geom_point() +
  geom_point(aes(y = pred2), color = "yellow") +
  theme_bw()

pred_3 <- val3 %>% 
  mutate(pred1 = predict(xgb1, dval3),
         pred2 = predict(xgb2, dval3),
         pred3 = predict(xgb3, dval3),
         pred4 = predict(xgb4, dval3)) %>% 
  rowwise() %>% 
  mutate(mean = sum(pred1, pred2, pred3, pred4)/4)

ggplot(pred_3, aes(x = harmonized_value, y = mean)) + 
  geom_point() +
  geom_point(aes(y = pred3), color = "yellow") +
  theme_bw()

pred_4 <- val4 %>% 
  mutate(pred1 = predict(xgb1, dval4),
         pred2 = predict(xgb2, dval4),
         pred3 = predict(xgb3, dval4),
         pred4 = predict(xgb4, dval4)) %>% 
  rowwise() %>% 
  mutate(mean = sum(pred1, pred2, pred3, pred4)/4)

ggplot(pred_4, aes(x = harmonized_value, y = mean)) + 
  geom_point() +
  geom_point(aes(y = pred4), color = "yellow") +
  theme_bw()



# apply to test -----------------------------------------------------------

test_pred <- test %>% 
  mutate(pred1 = predict(xgb1, dtest),
         pred2 = predict(xgb2, dtest),
         pred3 = predict(xgb3, dtest),
         pred4 = predict(xgb4, dtest)) %>% 
  rowwise() %>% 
  mutate(mean = sum(pred1, pred2, pred3, pred4)/4) %>% 
  filter(misc_flag != 1)

test_vert <- test_pred %>% 
  pivot_longer(pred1:pred4, 
               names_to = "pred", 
               values_to = "values")

ggplot(test_vert, aes(x = harmonized_value, y = values)) +
  geom_point(aes(color = pred)) +
  scale_color_viridis_d() +
  labs(x = "observed Secchi",
       y = "predicted Secchi",
       color = "split") +
  theme_bw()

ggplot(test_vert, aes(x = harmonized_value, y = mean)) +
  geom_point() +
  labs(x = "observed Secchi",
       y = "predicted Secchi") +
  geom_abline(slope = 1, intercept = 0, lty = 1, col = "grey") +
  theme_bw()

evals_test <- test_pred %>%
  ungroup() %>% 
  summarise(rmse = rmse(harmonized_value, mean),
            mae = mae(harmonized_value, mean),
            mape = mape(harmonized_value, mean),
            bias = bias(harmonized_value, mean),
            p.bias = percent_bias(harmonized_value, mean),
            smape = smape(harmonized_value, mean),
            r2 = cor(harmonized_value, mean)^2)
evals_test

test_long <- test_pred %>%
  ungroup() %>% 
  summarize(n = n(), .by = rowid) %>% 
  filter(n > 100) %>% 
  left_join(., test_pred)

ggplot(test_long, aes(x = date, y = mean)) +
  geom_point() +
  geom_point(aes(y = harmonized_value), color = "light blue") +
  facet_grid(rowid ~ ., scales = "free_y") +
  theme_bw()

ggplot(test_long, aes(x = date, y = mean - harmonized_value)) +
  geom_point() +
  facet_grid(rowid ~ .) +
  labs(y = "residual (m)", 
       x = NULL) +
  theme_bw()
