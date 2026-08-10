library(data.table)
library(tidyverse)
library(feather)
library(sf)
library(tmap)
library(EDIutils)

# using EDIutils, grab the files
package_identifier <- "edi.2254.1"

# get aquasat siteSR entity information
siteSR_entities <- read_data_entity_names(package_identifier)

# reduce to DSWE1 feather files
siteSR_entities_d1 <- siteSR_entities %>% 
  filter(grepl("confident", entityName) &
           !grepl("algal", entityName))

# and this is how to read into the environment...
read_feather(read_data_entity(packageId = package_identifier, entityId = siteSR_entities_d1$entityId[1]))
# and this is where I stopped updating this script.


# jot down file paths, these are pretty big and it's more efficient to load interatively 
# and get matches from each file.
LS4_fp <- "scratch/siteSR_collated_point_meta_LANDSAT_4_DSWE1_v2024-08-27.feather"
LS5_fp <- "scratch/siteSR_collated_point_meta_LANDSAT_5_DSWE1_v2024-08-27.feather"
LS7_fp <- "scratch/siteSR_collated_point_meta_LANDSAT_7_DSWE1_v2024-08-27.feather"
LS8_fp <- "scratch/siteSR_collated_point_meta_LANDSAT_8_DSWE1_v2024-08-27.feather"
LS9_fp <- "scratch/siteSR_collated_point_meta_LANDSAT_9_DSWE1_v2024-08-27.feather"

# read in site file
sites <- read_csv("scratch/visible_sites.csv")

sites_less <- sites %>% 
  select(rowid, OrganizationIdentifier, OrganizationFormalName, MonitoringLocationIdentifier)

# load SDD data
sdd <- read_feather("../AquaMatch_harmonize_WQP/3_harmonize/out/sdd_harmonized_group.feather")

# just grab those which are visible
sdd_sites <- left_join(sites_less, sdd, relationship = "many-to-many")

# reduce for matching
sdd_sites_less <- sdd_sites %>% 
  select(rowid, harmonized_utc, subgroup_id)

# do a quick summary 
site_summary <- sdd_sites_less %>% 
  summarise(n = n(), .by = rowid)

# get unique site types here
unique(sdd_sites$MonitoringLocationTypeName)

# let's limit to Lake data for time purposes
lakes_only <- sdd_sites %>% 
  filter(grepl("Lake|Reservoir", MonitoringLocationTypeName)) %>% 
  filter(!grepl("Great", MonitoringLocationTypeName))

# and then get fewer cols to match with sat data
lakes_for_sat <- lakes_only %>% 
  select(rowid, harmonized_utc, subgroup_id)

# look at those real quick
lake_locs <- sites %>% 
  filter(rowid %in% unique(lakes_for_sat$rowid)) %>% 
  st_as_sf(., coords = c("WGS84_Longitude", "WGS84_Latitude"), crs = "EPSG:4326")

tmap_mode("plot")
tm_shape(lake_locs) + tm_dots()

# let's drop the ones that are outside the CONUS for the purposes of this
states <- tigris::states() %>% 
  filter(!grepl("AK|AS|MP|VI|HI|PR", STUSPS))

lake_locs_CONUS <- lake_locs %>% 
  st_transform(., st_crs(states)) %>% 
  .[states,] %>% 
  st_transform(., st_crs(lake_locs))

tm_shape(lake_locs_CONUS) + tm_dots()

# that's better, now let's reduce lakes for sat pairing with that info
lakes_for_sat <- lakes_for_sat %>% 
  filter(rowid %in% lake_locs_CONUS$rowid)

#### NOW LETS START PAIRING! ########

make_matchups <- function(sat_fp, data) {
  data <- data %>% 
    mutate(harmonized_utc = force_tz(harmonized_utc, "UTC"),
           date = ymd(format(harmonized_utc, "%Y-%m-%d")))
  sat <- read_feather(sat_fp) %>% 
    select(-c(rowid, date)) %>% 
    mutate(rowid = as.numeric(r_id), 
           calc_date = ymd(str_sub(system.index, -8, -1))) %>% 
    rowid_to_column('sat_data_id') %>% 
    filter(rowid %in% unique(data$rowid))
  
  sat_less <- sat %>% 
    select(sat_data_id, rowid, calc_date) 
  
  # coerce to data.table
  setDT(data)
  setDT(sat_less)
  setDT(sat)
  
  # overjoin using data.table syntax (cause this is biiiiiiig) and also filter in one step (also because of size)
  matches <- sat_less[data, on = .(rowid), allow.cartesian = TRUE][abs(as.integer(difftime(date, calc_date, units = "days"))) <= 5]
  
  # add the sat data back in
  left_join(matches, sat)
}

five_day_matches <- map2(.x = list(LS4_fp, LS5_fp, LS7_fp, LS8_fp, LS9_fp),
     .y = list(lakes_for_sat),
     make_matchups) %>% 
  bind_rows()

five_day_matches <- five_day_matches %>% 
  left_join(., sdd)

write_feather(five_day_matches, "scratch/five_day_sdd_matches.feather")


lake_locs_with_matches <- lake_locs_CONUS %>% 
  filter(rowid %in% unique(five_day_matches$rowid)) %>% 
  left_join(., five_day_summary)

states <- states[lake_locs_with_matches %>% st_transform(crs = st_crs(states)), ]
tm_shape(states) +
  tm_polygons() +
  tm_shape(lake_locs_with_matches) + 
  tm_dots("n_matches", 
          fill.scale = tm_scale(breaks = c(1, 10, 25, 50, 75, 100, 
                                           500, 1000, 2000, 3000, 
                                           4000, 5000),
                                values = "viridis"),
          size = 0.5,
          fill_alpha = 0.5)
