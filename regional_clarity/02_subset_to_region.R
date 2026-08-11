# This code reduces the 5-day matchups to those in a 6-state region and
# applies some QA filters to the data to create the ML dataset
# Author: B Steele, Colorado State University, ROSSyndicate
# last updated Aug 11, 2026


## Data prep ----

# load libraries
library(tidyverse)
library(sf)
library(arrow)
library(tigris)
library(tmap)
library(nhdplusTools)

# read in 5-day match
sdd_5d <- read_feather("aquamatch_files/five_day_sdd_matches.feather")

### Reduce AOI to Mountain West HUC4's ----

# get the unique sites
unique_sites <- sdd_5d %>% 
  select(siteSR_id, lon, lat) %>% 
  unique() %>% 
  st_as_sf(., coords = c("lon", "lat"), crs = "EPSG:4326")

# filter to states we want in this analysis
states <- states() %>% 
  filter(STUSPS %in% c("CO", "WY", "UT", "ID", "MT", "NM"))
states_union <- states %>% st_union()

# and grab all the HUC4's from NHD
HUC4 <- get_huc(AOI = states_union,
                type = "huc04")
HUC4v <- st_make_valid(HUC4)

# get the overlapping HUC4s
overlap_areas <- st_make_valid(HUC4v) %>%
  st_intersection(st_make_valid(states_union)) %>%
  mutate(overlap_area = st_area(geometry)) %>%
  st_drop_geometry() %>%
  select(huc4, overlap_area)

# calculate proportion overlap, filter for those greater than/equal to 50%
HUC4_filtered <- HUC4v %>%
  left_join(overlap_areas, by = "huc4") %>%
  mutate(
    overlap_area = coalesce(overlap_area, units::set_units(0, "m^2")),
    overlap_frac = as.numeric(overlap_area / st_area(geometry))
  ) %>%
  filter(overlap_frac >= 0.50) 

# add huc info and filter to this extent
regional_sites <- unique_sites %>% 
  st_transform(st_crs(HUC4_filtered)) %>% 
  st_join(HUC4_filtered %>% select(huc4), left = FALSE)

# add sdd info back
regional_sdd <- regional_sites %>% 
  left_join(., sdd_5d)

## QA the data ----

# apply some QA filters
reg_sdd_filtered <- regional_sdd %>% 
  filter(pCount_dswe1/pCount_dswe_gt0 > .5 & pCount_dswe1 > 8 & prop_clouds == 0) %>% 
  select(siteSR_id, sat_id, subgroup_id, sat_date, date, 
         harmonized_value, depth_flag, tier, misc_flag, field_flag,
         med_Blue:med_SurfaceTemp, med_Aerosol, mission,
         MonitoringLocationTypeName, 
         lon, lat, huc4) %>% 
  # do some quick filtering from ASv2 
  filter(tier < 3, harmonized_value  >= 0.1, misc_flag == 0, depth_flag == 0)

# grab the observation closes in date to a satellite flyover
reg_sdd_closest <- reg_sdd_filtered %>% 
  mutate(date_diff = date-sat_date) %>% 
  arrange(abs(date_diff)) %>% 
  dplyr::slice(1, .by = c("subgroup_id")) 

# run a ransac-like algorithm per site to remove outliers
ransac_site <- function(date, sdd, n_iter = 200, threshold = 5, min_sample = 2) {
  n <- length(sdd)
  if (n < min_sample + 1) return(rep(FALSE, n))  # too few points to do anything
  
  t <- as.numeric(date)
  best_inliers <- rep(FALSE, n)
  best_count <- 0
  
  for (i in 1:n_iter) {
    idx <- sample(n, min_sample)
    if (length(unique(t[idx])) < 2) next  # avoid degenerate fit (same date)
    
    fit <- lm(sdd[idx] ~ t[idx])
    pred <- coef(fit)[1] + coef(fit)[2] * t
    resid <- abs(sdd - pred)
    inliers <- resid < threshold
    
    if (sum(inliers) > best_count) {
      best_count <- sum(inliers)
      best_inliers <- inliers
    }
  }
  best_inliers
}

# apply and filter inliers only
filtered_ml <- reg_sdd_closest %>%
  group_by(siteSR_id) %>%
  arrange(date) %>%
  mutate(is_inlier = ransac_site(date, harmonized_value)) %>%
  ungroup() %>% 
  filter(is_inlier)

## Visualize data ----

# summarize by site for plotting
regional_site_summary <- filtered_ml %>% 
  summarize(n = n(), .by = c(siteSR_id, geometry)) %>% 
  arrange(n) 

# look at some of the records with a lot of data
regional_long_record <- regional_site_summary %>% 
  st_drop_geometry() %>% 
  filter(n > 100) %>% 
  left_join(., filtered_ml)

ggplot(regional_long_record, aes(x = date, y = harmonized_value)) +
  geom_point() +
  facet_wrap(siteSR_id ~ .) +
  theme_bw()

# look at data spatially
tm_shape(HUC4_filtered) +
  tm_shape(states %>% 
             st_transform(st_crs(HUC4_filtered))) +
  tm_polygons() +
  tm_shape(HUC4_filtered) +
  tm_polygons(fill_alpha = 0) +
  tm_shape(regional_site_summary %>% 
             st_transform(st_crs(HUC4_filtered))) +
  tm_dots("n", 
          fill.scale = tm_scale_continuous(values = "viridis"))

# and by HUC
regional_huc_summary <- filtered_ml %>% 
  st_drop_geometry() %>% 
  summarize(n = n(), .by = c(huc4)) %>% 
  inner_join(HUC4_filtered, .) 

tm_shape(HUC4_filtered) +
  tm_shape(states %>% 
             st_transform(st_crs(HUC4_filtered))) +
  tm_polygons() +
  tm_shape(regional_huc_summary) +
  tm_polygons("n", 
              fill.scale = tm_scale_continuous(values = "viridis"),
              fill_alpha = 0.5)


## Export data ----

write_feather(filtered_ml %>% st_drop_geometry(),
              "aquamatch_files/filtered_regional_sdd.feather")
