library(tidyverse)
library(sf)
library(feather)
library(tigris)
library(tmap)
library(nhdplusTools)

sdd_5d <- read_feather("../AquaMatch_siteSR_WQP/scratch/five_day_sdd_matches.feather")

unique_sites <- sdd_5d %>% 
  select(rowid, WGS84_Longitude, WGS84_Latitude) %>% 
  unique() %>% 
  st_as_sf(., coords = c("WGS84_Longitude", "WGS84_Latitude"), crs = "EPSG:4326")

states <- states() %>% 
  filter(STUSPS %in% c("CO", "WY", "UT", "ID", "MT", "NM"))

HUC4 <- get_huc(AOI = states %>% st_union(),
                type = "huc04")

regional_sites <- unique_sites %>% 
  st_transform(st_crs(HUC4)) %>% 
  .[HUC4,]

rm(unique_sites)

regional_sdd <- regional_sites %>% 
  left_join(., sdd_5d)

reg_sdd_filtered <- regional_sdd %>% 
  filter(pCount_dswe1/pCount_dswe_gt0 > .5 & pCount_dswe1 > 8 & prop_clouds == 0) %>% 
  select(rowid, sat_data_id, subgroup_id, calc_date, date, 
         harmonized_value, depth_flag, tier, misc_flag, field_flag,
         med_Blue:med_SurfaceTemp, med_Aerosol, mission, CLOUD_COVER,
         MonitoringLocationTypeName, HUCEightDigitCode, StateCode, CountyCode,
         WGS84_Longitude, WGS84_Latitude, nhd_permanent_identifier, nhd_ftype,
         dist_to_shore) %>% 
  # do some quick filtering from ASv2 
  filter(tier < 3, harmonized_value  >= 0.1, misc_flag == 0)

reg_sdd_closest <- reg_sdd_filtered %>% 
  mutate(date_diff = date-calc_date) %>% 
  arrange(abs(date_diff)) %>% 
  dplyr::slice(1, .by = c("subgroup_id"))  %>% 
  mutate(HUC4 = str_sub(HUCEightDigitCode, 1, 4))

site_summary <- reg_sdd_closest %>% 
  st_drop_geometry() %>% 
  summarize(n = n(), .by = c(rowid, WGS84_Latitude, WGS84_Longitude)) %>% 
  st_as_sf(., 
           coords = c("WGS84_Longitude", "WGS84_Latitude"),
           crs = "EPSG:4326")

huc4_summary <- reg_sdd_closest %>% 
  st_drop_geometry() %>% 
  mutate(HUC4 = str_sub(HUCEightDigitCode, 1, 4)) %>% 
  summarise(n = n(), .by = HUC4)

huc8_summary <- reg_sdd_closest %>% 
  st_drop_geometry() %>% 
  summarise(n = n(), .by = HUCEightDigitCode)


# filter outliers, where outliers are 2*sd + mean
summary_stats <- reg_sdd_closest %>% 
  st_drop_geometry() %>% 
  select(rowid, harmonized_value) %>% 
  summarise(mean_sdd = mean(harmonized_value),
            sd_sdd = sd(harmonized_value),
            min_sdd = min(harmonized_value),
            max_sdd = max(harmonized_value),
            n = n(),
            .by = rowid)

reg_sdd_lite <- reg_sdd_closest %>% 
  st_drop_geometry() %>% 
  select(rowid, subgroup_id, harmonized_value) 

filter_outliers <- function(r_id, mean, sd) {
  df <- reg_sdd_lite %>% 
    filter(rowid == r_id)
  if (nrow(df) > 8) {
    cutoff <- 2*sd + mean
    filtered <- df %>% 
      filter(harmonized_value < cutoff)
    return(filtered)
  } else {
    return(df)
  }
}

filtered_ml <- pmap(list(r_id = summary_stats$rowid, 
                         mean = summary_stats$mean_sdd, 
                         sd = summary_stats$sd_sdd),
                    filter_outliers) %>% 
  bind_rows() %>% 
  left_join(., reg_sdd_closest)

regional_site_summary <- filtered_ml %>% 
  summarize(n = n(), .by = rowid) %>% 
  inner_join(regional_sites, .) 

regional_long_record <- regional_site_summary %>% 
  st_drop_geometry() %>% 
  filter(n > 100) %>% 
  left_join(., filtered_ml)

ggplot(regional_long_record, aes(x = date, y = harmonized_value)) +
  geom_point() +
  facet_wrap(rowid ~ .) +
  theme_bw()

tm_shape(HUC4) +
  tm_shape(states %>% st_transform(st_crs(HUC4))) +
  tm_polygons() +
  tm_shape(HUC4) +
  tm_polygons(fill_alpha = 0) +
  tm_shape(regional_site_summary %>% 
             st_transform(st_crs(HUC4))) +
  tm_dots("n", 
          fill.scale = tm_scale("viridis"))

write_feather(filtered_ml %>% select(-geometry),
              "modeling/regional_clarity/filtered_regional_sdd.feather")
