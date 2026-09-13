# Reproduce 01_make_matches.Rmd's location_id de-duplication (site-location-
# dedup chunk, lines ~378-405) so the outlier-refilter script can correctly
# map each of this project's modeling siteSR_id values to the representative
# site whose RANSAC band-outlier decision it actually inherited - the
# ransac_site_multiband() filter only runs on ONE representative siteSR_id
# per location_id (sites within 100m of each other AND sharing the same
# wb_nhd_id), and every other siteSR_id at that location borrows its
# decision, keyed by (location_id, sat_id). Exported once, read by
# python/07_rerun_outlier_filter.py.

suppressMessages({
  library(sf); library(igraph); library(dplyr); library(readr); library(arrow)
})

aquamatch_dir <- "aquamatch_files"
out_dir <- "regional_clarity/ls8_harmonization/data"
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

sites <- read_csv(file.path(aquamatch_dir, "siteSR_collated_WQP_NWIS_sites_with_NHD_info_2025-06-04.csv"),
                   show_col_types = FALSE) %>%
  filter(flag_optical_shoreline == 0,
         number_int_wb == 1,
         (flag_thermal_TM_shoreline == 0 |
            flag_thermal_ETM_shoreline == 0 |
            flag_thermal_TIRS_shoreline == 0)) %>%
  select(siteSR_id, wb_nhd_id, WGS84_Latitude, WGS84_Longitude)

log("sites after geometry QA: %s", format(nrow(sites), big.mark = ","))

site_pts <- sites %>%
  st_as_sf(coords = c("WGS84_Longitude", "WGS84_Latitude"), crs = 4326, remove = FALSE)

site_within_100m <- st_is_within_distance(site_pts, dist = 100)

wb_nhd_id <- sites$wb_nhd_id
site_within_100m_same_wb <- Map(
  function(nbrs, wb) nbrs[wb_nhd_id[nbrs] == wb],
  site_within_100m, wb_nhd_id
)

site_components <- components(
  graph_from_adj_list(site_within_100m_same_wb, mode = "all")
)$membership

pad <- nchar(as.character(max(site_components)))
sites <- sites %>%
  mutate(location_id = sprintf(paste0("loc_%0", pad, "d"), site_components))

n_multi <- sites %>% count(location_id) %>% filter(n > 1) %>% nrow()
log("%s locations have more than one site within 100m", n_multi)

# representative siteSR_id per location_id = first row encountered, same as
# 01_make_matches.Rmd's `distinct(location_id, .keep_all = TRUE)`
representative <- sites %>% distinct(location_id, .keep_all = TRUE) %>% pull(siteSR_id)

out <- sites %>%
  select(siteSR_id, location_id) %>%
  mutate(is_representative = siteSR_id %in% representative)

write_feather(out, file.path(out_dir, "site_location_ids.feather"))
log("wrote %s (%s rows, %s distinct locations, %s representative)",
    file.path(out_dir, "site_location_ids.feather"), nrow(out),
    n_distinct(out$location_id), sum(out$is_representative))
