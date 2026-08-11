# This code creates matchups between the AquaMatch SDD files and the siteSR files
# we use a generous 5-day window to work from for future steps.
# Author: B Steele, Colorado State University, ROSSyndicate
# last updated Aug 11, 2026


# load libraries
library(AquaMatchr) # if install needed: remotes::install_github(repo = "AquaSat/AquaMatchr")
library(data.table)
library(arrow)
library(sf)
library(tmap)
library(AquaMatchr)
library(tidyverse)

# login to EDI - this is a work around for error popping up in AquaMatchr
# requires EDIutils, which is required by AquaMatchr, so probably no issue here
EDIutils::login(key = Sys.getenv('EDI_API_key'))


## AquaMatch data ----

# note location for file storage when downloading via AquaMatchr
aquamatch_dir <- "aquamatch_files"
dir.create(aquamatch_dir)

# use AquaMatchr for data gathering
sdd <- download_parameters("sdd", "newest")
# get it out of a list
sdd <- sdd[[1]]

# and grab site SR
siteSR <- download_siteSR(save_location = aquamatch_dir)

# sites
sites <- read_csv(file.path(siteSR[grepl("sites_with_NHD_info", siteSR)]))
    
# just grab some info for SDD filtering and plotting                        
sites_less <- sites %>% 
  select(siteSR_id, 
         OrganizationIdentifier = org_id, 
         MonitoringLocationIdentifier = loc_id) 

# just grab those which are visible
sdd_sites <- left_join(sites_less, sdd, 
                       relationship = "many-to-many")

# reduce for matching
sdd_sites_less <- sdd_sites %>% 
  select(siteSR_id, harmonized_utc, subgroup_id)

# do a quick summary 
site_summary <- sdd_sites_less %>% 
  summarise(n = n(), .by = siteSR_id)

# get unique site types here
unique(sdd_sites$ResolvedMonitoringLocationTypeName)

# let's limit to Lake data for our purposes
lakes_only <- sdd_sites %>% 
  filter(grepl("Lake|Reservoir", MonitoringLocationTypeName)) %>%
  # remove the great lakes for this matching process
  filter(!grepl("Great", MonitoringLocationTypeName))

# and then reduce cols to match with sat data
lakes_for_sat <- lakes_only %>% 
  select(siteSR_id, harmonized_utc, subgroup_id)

# look at those real quick
lake_locs <- sites %>% 
  filter(siteSR_id %in% unique(lakes_for_sat$siteSR_id)) %>% 
  st_as_sf(., coords = c("WGS84_Longitude", "WGS84_Latitude"), crs = "EPSG:4326")

tmap_mode("plot")
tm_shape(lake_locs) + 
  tm_dots()

# let's drop the ones that are outside the CONUS for the purposes of this
states <- tigris::states() %>% 
  filter(!grepl("AK|AS|MP|VI|HI|PR|GU", STUSPS))

lake_locs_CONUS <- lake_locs %>% 
  st_transform(., st_crs(states)) %>% 
  .[states,] %>% 
  st_transform(., st_crs(lake_locs))

tm_shape(lake_locs_CONUS) + tm_dots()

# that's better, now let's reduce lakes for sat pairing with that info
lakes_for_sat <- lakes_for_sat %>% 
  filter(siteSR_id %in% lake_locs_CONUS$siteSR_id)

## Make matchups ----


make_matchups <- function(sat_data_fp, wq_data) {
  # data
  wq_data <- wq_data %>% 
    mutate(harmonized_utc = force_tz(harmonized_utc, "UTC"),
           wq_date = ymd(format(harmonized_utc, "%Y-%m-%d")))
  sat_data <- read_feather(sat_data_fp)  %>% 
    filter(siteSR_id %in% unique(wq_data$siteSR_id)) 
  
  # reduce columns in satellite data 
  sat_less <- sat_data %>% 
    select(siteSR_id, sat_id, 
           sat_date = date) %>% 
    mutate(sat_date = ymd(sat_date))
  
  # coerce to data.table
  setDT(wq_data)
  setDT(sat_less)
  setDT(sat_data)
  
  # overjoin using data.table syntax (this is biiiiiiig) 
  matches <- sat_less[
    wq_data, 
    on = .(siteSR_id), 
    allow.cartesian = TRUE
  ][
    # and filter to window of 5 days
    abs(as.integer(difftime(wq_date, 
                            sat_date, 
                            units = "days"))) <= 5
  ]
  
  # add the sat data back in
  left_join(matches, sat_data)
}

# apply function
five_day_matches <- map2(.x = siteSR[grepl("feather", siteSR)],
                         .y = list(lakes_for_sat),
                         make_matchups) %>% 
  bind_rows()

# join with SDD data
five_day_matches <- five_day_matches %>% 
  left_join(., sdd)

# export
write_feather(five_day_matches, "aquamatch_files/five_day_sdd_matches.feather")

## Summary and Figures ----

# summarize by number of matches per site
five_day_summary <- five_day_matches %>% 
  summarize(n = n(),
            .by = siteSR_id)

# add in spatial data
lake_locs_with_matches <- lake_locs_CONUS %>% 
  filter(siteSR_id %in% unique(five_day_matches$siteSR_id)) %>% 
  left_join(., five_day_summary)

# plot all missions
tm_shape(states) +
  tm_polygons() +
  tm_shape(lake_locs_with_matches, bbox = states) +
  tm_dots("n",
          fill.scale = tm_scale(breaks = c(1, 10, 25, 50, 75, 100,
                                           500, 1000, 1500),
                                values = "viridis"),
          size = 0.5,
          fill_alpha = 0.5) +
  tm_layout(legend.position = tm_pos_in("right", "bottom"),
            legend.bg.color = "white",
            legend.frame = TRUE)


# and per site per mission
five_day_summary_sensor <- five_day_matches %>% 
  summarize(n = n(),
            .by = c(siteSR_id, mission))

# add spatial data and label info
lake_locs_by_mission <- lake_locs_with_matches %>% 
  select(siteSR_id, geometry) %>% 
  right_join(five_day_summary_sensor, by = "siteSR_id") %>% 
  mutate(mission = factor(mission, 
                          levels = c("LT04", "LT05", "LE07", "LC08", "LC09"),
                          labels = c("Landsat 4", "Landsat 5", "Landsat 7", "Landsat 8", "Landsat 9")))%>% 
  arrange(n)

# Compute totals per mission before faceting
mission_stats <- lake_locs_by_mission %>% 
  st_drop_geometry() %>% 
  summarize(total_matches = sum(n),
            total_sites = n_distinct(siteSR_id),
            .by = mission)

# and prep for viz
lake_locs_by_mission <- lake_locs_by_mission %>% 
  left_join(mission_stats, by = "mission") %>% 
  mutate(mission_label = paste0(mission, ", ",
                                format(total_matches, big.mark = ","), " matches, ",
                                format(total_sites, big.mark = ","), " sites")) %>% 
  arrange(n)   # keep your draw-order fix from before

# plot!
tm_shape(states) +
  tm_polygons() +
  tm_shape(lake_locs_by_mission, bbox = states) +
  tm_dots("n",
          fill.scale = tm_scale(values = "viridis"),
          fill.legend = tm_legend(position = tm_pos_on_top(pos.h = "right", pos.v = "bottom"),
                                  bg.color = "white",
                                  frame = TRUE, text.size = 1),
          size = 0.5,
          fill_alpha = 0.5) +
  tm_facets_wrap(by = "mission_label", ncol = 2, nrow = 3)
