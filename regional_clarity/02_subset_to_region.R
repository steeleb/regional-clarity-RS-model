library(tidyverse)
library(sf)
library(arrow)
library(tigris)
library(tmap)
library(nhdplusTools)

sdd_5d <- read_feather("aquamatch_files//five_day_sdd_matches.feather")

unique_sites <- sdd_5d %>% 
  select(siteSR_id, lon, lat) %>% 
  unique() %>% 
  st_as_sf(., coords = c("lon", "lat"), crs = "EPSG:4326")

states <- states() %>% 
  filter(STUSPS %in% c("CO", "WY", "UT", "ID", "MT", "NM"))
states_union <- states %>% st_union()
HUC4 <- get_huc(AOI = states_union,
                type = "huc04")
HUC4v <- st_make_valid(HUC4)

overlap_areas <- st_make_valid(HUC4v) %>%
  st_intersection(st_make_valid(states_union)) %>%
  mutate(overlap_area = st_area(geometry)) %>%
  st_drop_geometry() %>%
  select(huc4, overlap_area)  # use whichever column is HUC4's unique ID

HUC4_filtered <- HUC4v %>%
  left_join(overlap_areas, by = "huc4") %>%
  mutate(
    overlap_area = coalesce(overlap_area, units::set_units(0, "m^2")),
    overlap_frac = as.numeric(overlap_area / st_area(geometry))
  ) %>%
  filter(overlap_frac >= 0.50) 

regional_sites <- unique_sites %>% 
  st_transform(st_crs(HUC4_filtered)) %>% 
  st_join(HUC4_filtered %>% select(huc4), left = FALSE)

regional_sdd <- regional_sites %>% 
  left_join(., sdd_5d)

reg_sdd_filtered <- regional_sdd %>% 
  filter(pCount_dswe1/pCount_dswe_gt0 > .5 & pCount_dswe1 > 8 & prop_clouds == 0) %>% 
  select(siteSR_id, sat_id, subgroup_id, sat_date, date, 
         harmonized_value, depth_flag, tier, misc_flag, field_flag,
         med_Blue:med_SurfaceTemp, med_Aerosol, mission,
         MonitoringLocationTypeName, 
         lon, lat, huc4) %>% 
  # do some quick filtering from ASv2 
  filter(tier < 3, harmonized_value  >= 0.1, misc_flag == 0)

reg_sdd_closest <- reg_sdd_filtered %>% 
  mutate(date_diff = date-sat_date) %>% 
  arrange(abs(date_diff)) %>% 
  dplyr::slice(1, .by = c("subgroup_id")) 

site_summary <- reg_sdd_closest %>% 
  st_drop_geometry() %>% 
  summarize(n = n(), .by = c(siteSR_id, lon, lat)) %>% 
  st_as_sf(., 
           coords = c("lon", "lat"),
           crs = "EPSG:4326")

huc4_summary <- reg_sdd_closest %>% 
  st_drop_geometry() %>% 
  summarise(n = n(), .by = huc4)

# filter outliers for each site, where outliers are 2*sd + mean
summary_stats <- reg_sdd_closest %>% 
  st_drop_geometry() %>% 
  select(siteSR_id, harmonized_value) %>% 
  summarise(mean_sdd = mean(harmonized_value),
            sd_sdd = sd(harmonized_value),
            min_sdd = min(harmonized_value),
            max_sdd = max(harmonized_value),
            n = n(),
            .by = siteSR_id)

reg_sdd_lite <- reg_sdd_closest %>% 
  st_drop_geometry() %>% 
  select(siteSR_id, subgroup_id, harmonized_value) 

filter_outliers <- function(id, mean, sd) {
  df <- reg_sdd_lite %>% 
    filter(siteSR_id == id)
  if (nrow(df) > 8) {
    cutoff <- 2*sd + mean
    filtered <- df %>% 
      filter(harmonized_value < cutoff)
    return(filtered)
  } else {
    return(df)
  }
}

filtered_ml <- pmap(list(id = summary_stats$siteSR_id, 
                         mean = summary_stats$mean_sdd, 
                         sd = summary_stats$sd_sdd),
                    filter_outliers) %>% 
  bind_rows() %>% 
  left_join(., reg_sdd_closest)

regional_site_summary <- filtered_ml %>% 
  summarize(n = n(), .by = siteSR_id) %>% 
  inner_join(regional_sites, .) %>% 
  arrange(n)

regional_long_record <- regional_site_summary %>% 
  st_drop_geometry() %>% 
  filter(n > 100) %>% 
  left_join(., filtered_ml)

ggplot(regional_long_record, aes(x = date, y = harmonized_value)) +
  geom_point() +
  facet_wrap(siteSR_id ~ .) +
  theme_bw()

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


write_feather(filtered_ml %>% select(-geometry),
              "aquamatch_files/filtered_regional_sdd.feather")
