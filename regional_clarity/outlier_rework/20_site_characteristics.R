# Site/catchment characteristics, testing whether they boost model
# performance the way they do in Topp et al. 2021 (Environ. Res. Lett.
# 16 055025), which paired 3 optical variables with 8 static LakeCat
# catchment variables (imperviousness, urban/forest/cropland/wetland land
# cover) for a national Secchi-depth model.
#
# wb_nhd_id verified against a fresh nhdplusTools::get_waterbodies() call
# (exact COMID + area match on Utah Lake) and confirmed via
# AquaMatch_siteSR_WQP's source (add_NHD_waterbody_to_sites.R) that ALL
# 1,525 of our sites have wb_nhd_source == "NHDPlusv2", i.e. wb_nhd_id IS
# the NHDPlusV2 comid for every site in our study region (the
# Permanent_Identifier branch only fires for non-CONUS HUC4s >= 1900,
# which our 7-HUC2 region never reaches) - safe to join to LakeCat
# directly, no crosswalk needed.

suppressMessages({
  library(arrow); library(dplyr); library(readr); library(sf)
  library(elevatr); library(StreamCatTools)
})

aquamatch_dir <- "aquamatch_files"
out_dir <- "regional_clarity/outlier_rework"
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

## ---- unique sites in the final training set ----
final <- read_feather(file.path(out_dir, "filtered_regional_sdd_final.feather"),
                       col_select = c("siteSR_id", "lat", "lon"))
sites_unique <- distinct(final, siteSR_id, lat, lon)
log("n unique sites: %s", nrow(sites_unique))

sites_meta <- read_csv(file.path(aquamatch_dir, "siteSR_collated_WQP_NWIS_sites_with_NHD_info_2025-06-04.csv"),
                        show_col_types = FALSE) %>%
  select(siteSR_id, wb_nhd_id, wb_nhd_source, wb_gnis_name, wb_areasqkm)
sites_unique <- sites_unique %>% left_join(sites_meta, by = "siteSR_id")
stopifnot(all(sites_unique$wb_nhd_source == "NHDPlusv2" | is.na(sites_unique$wb_nhd_source)))
log("sites with a valid wb_nhd_id (comid): %s / %s",
    sum(!is.na(sites_unique$wb_nhd_id)), nrow(sites_unique))

## ---- 1. elevation (point, AWS terrain tiles) ----
log("fetching elevation for all sites")
pts_sf <- st_as_sf(sites_unique, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
t0 <- Sys.time()
elev <- get_elev_point(pts_sf, src = "aws", z = 9)
log("elevation fetch took %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))
elev_df <- st_drop_geometry(elev) %>% select(siteSR_id, elevation_m = elevation)
sites_unique <- sites_unique %>% left_join(elev_df, by = "siteSR_id")
log("elevation summary:"); print(summary(sites_unique$elevation_m))

## ---- 2. LakeCat catchment characteristics ----
## land-cover-focused set, matching Topp et al.'s variable categories:
## imperviousness, urban/forest/cropland/wetland land cover (2006, static,
## same year Topp used), plus catchment area (LakeCat provides this for
## free alongside any metric query)
log("fetching LakeCat catchment metrics")
lakecat_metrics <- paste(c(
  "pctimp2006", "pcturbhi2006", "pcturblo2006", "pcturbmd2006", "pcturbop2006",
  "pctconif2006", "pctdecid2006", "pctmxfst2006",
  "pcthbwet2006", "pctwdwet2006", "pctcrop2006"
), collapse = ",")

comids <- unique(na.omit(sites_unique$wb_nhd_id))
log("n unique comids: %s", length(comids))

# batch in chunks to keep the request URL a reasonable length
chunk_size <- 200
chunks <- split(comids, ceiling(seq_along(comids) / chunk_size))
lakecat_raw <- purrr::map_dfr(chunks, function(ch) {
  lc_get_data(comid = paste(ch, collapse = ","), metric = lakecat_metrics,
              aoi = "catchment", showAreaSqKm = TRUE)
})
log("LakeCat rows returned: %s / %s comids requested", nrow(lakecat_raw), length(comids))

lakecat <- lakecat_raw %>%
  mutate(comid = as.character(comid),
         pct_forest_2006 = pctconif2006cat + pctdecid2006cat + pctmxfst2006cat,
         pct_wetland_2006 = pcthbwet2006cat + pctwdwet2006cat,
         pct_urban_2006 = pcturbhi2006cat + pcturblo2006cat + pcturbmd2006cat + pcturbop2006cat) %>%
  select(comid, catchment_area_sqkm = catareasqkm,
         pct_impervious_2006 = pctimp2006cat, pct_urban_2006, pct_forest_2006,
         pct_cropland_2006 = pctcrop2006cat, pct_wetland_2006)

sites_unique <- sites_unique %>% left_join(lakecat, by = c("wb_nhd_id" = "comid"))
log("sites with LakeCat data: %s / %s", sum(!is.na(sites_unique$pct_impervious_2006)), nrow(sites_unique))

write_feather(sites_unique %>% select(-wb_nhd_source),
              file.path(out_dir, "site_characteristics.feather"))
log("wrote site_characteristics.feather (%s cols, %s rows)", ncol(sites_unique) - 1, nrow(sites_unique))
log("done")
