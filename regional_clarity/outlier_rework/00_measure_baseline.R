# Measure the TRUE "no outlier removal" baseline for matchup/training row
# counts, i.e. what 01_make_matches.Rmd + 02_subset_to_region.Rmd would
# produce if neither the SDD-value RANSAC (is_inlier, currently a no-op
# anyway) nor the reflectance-band RANSAC (is_band_inlier) removed any rows.
#
# This reuses all the already-cached intermediate files from the existing
# pipeline run (sdd_quality_filter.feather, scene_metadata csvs, sites csv)
# so the only expensive step here is the same regional/metadata/cloud QA
# scan of the 15GB nationwide siteSR concatenation that 01 already did once
# - no RANSAC refit, no Gardner correction (row count doesn't depend on it).

suppressMessages({
  library(arrow)
  library(dplyr)
  library(readr)
  library(AquaMatchr)
})

aquamatch_dir <- "aquamatch_files"
out_dir <- "regional_clarity/outlier_rework"
dir.create(out_dir, showWarnings = FALSE)

log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

## ---- 1. SDD side: all QAQC-passed rows, ignoring is_inlier entirely ----
log("reading sdd_quality_filter.feather (QAQC-passed SDD, ignoring is_inlier)")
sdd_baseline <- read_feather(file.path(aquamatch_dir, "sdd_quality_filter.feather"))
log("SDD baseline rows (QAQC only, no RANSAC): %s", format(nrow(sdd_baseline), big.mark = ","))

sdd_baseline_path <- file.path(out_dir, "sdd_baseline_no_ransac.feather")
write_feather(sdd_baseline, sdd_baseline_path)

## ---- 2. siteSR side: metadata/cloud/region/geometry QA, NO band RANSAC ----
scene_metadata_paths <- list.files(aquamatch_dir,
                                    pattern = "sceneMetadata_Landsat(457|89)\\.csv$",
                                    full.names = TRUE)
scene_metadata <- purrr::map(scene_metadata_paths, read_csv, show_col_types = FALSE) %>%
  bind_rows() %>%
  filter(CLOUD_COVER < 20)
log("scene_metadata rows (CLOUD_COVER < 20): %s", format(nrow(scene_metadata), big.mark = ","))

sr_sites_csv <- list.files(aquamatch_dir,
                            pattern = "sites_with_NHD_info.*\\.csv$",
                            full.names = TRUE)[1]
sites <- read_csv(sr_sites_csv, show_col_types = FALSE) %>%
  filter(flag_optical_shoreline == 0,
         number_int_wb == 1,
         (flag_thermal_TM_shoreline == 0 |
            flag_thermal_ETM_shoreline == 0 |
            flag_thermal_TIRS_shoreline == 0))
log("geometry-QA sites: %s", format(nrow(sites), big.mark = ","))

unified_data <- file.path(aquamatch_dir, "siteSR_DSWE1_full_concatenation.feather")
regional_huc2 <- c(10, 11, 13, 14, 15, 16, 17)

log("scanning unified_data for region/metadata/cloud/geometry QA (no RANSAC) - this is the slow step")
t0 <- Sys.time()
siteSR_baseline <- open_dataset(unified_data, format = "feather") %>%
  filter(huc2 %in% regional_huc2,
         pCount_dswe1 / pCount_dswe_gt0 > 0.5,
         pCount_dswe1 > 8,
         prop_clouds == 0,
         sat_id %in% scene_metadata$sat_id,
         siteSR_id %in% sites$siteSR_id) %>%
  collect()
log("scan took %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))
log("siteSR baseline rows (no band RANSAC): %s", format(nrow(siteSR_baseline), big.mark = ","))

siteSR_baseline_path <- file.path(out_dir, "siteSR_baseline_no_ransac.feather")
write_feather(siteSR_baseline, siteSR_baseline_path)
rm(siteSR_baseline); gc()

## ---- 3. Match with no outlier removal on either side ----
dir.create(file.path(tempdir(), "duckdb"), recursive = TRUE, showWarnings = FALSE)
match_out <- file.path(out_dir, "sdd_baseline_matchups_5d.parquet")

log("running match_siteSR_to_WQP on unfiltered inputs")
match_siteSR_to_WQP(
  wqp_path = sdd_baseline_path,
  siteSR_path = siteSR_baseline_path,
  site_list_path = sr_sites_csv,
  save_location = match_out,
  time_window = "5 days"
)

baseline_match <- read_parquet(match_out)
log("baseline 5-day matchup rows (no outlier removal at all): %s",
    format(nrow(baseline_match), big.mark = ","))

## ---- 4. Replicate script 02's region filter + closest-observation dedup ----
## so this baseline is directly comparable to the current final training
## row count (filtered_regional_sdd.feather, 5,199 rows) and to the 10k /
## 80%-retention targets, rather than comparing raw matchup counts that
## still contain multiple mission candidates per field sample.
suppressMessages({ library(sf) })

log("replicating script 02's HUC4 region filter + closest-observation dedup")

sites_sf <- read_csv(sr_sites_csv, show_col_types = FALSE) %>%
  filter(siteSR_id %in% unique(baseline_match$siteSR_id)) %>%
  st_as_sf(coords = c("WGS84_Longitude", "WGS84_Latitude"), crs = "EPSG:4326")

HUC4_filtered <- st_read(file.path(aquamatch_dir, "huc4_filtered.gdb"),
                          layer = "huc4_filtered", quiet = TRUE)
geom_col <- attr(HUC4_filtered, "sf_column")
if (geom_col != "geometry") {
  names(HUC4_filtered)[names(HUC4_filtered) == geom_col] <- "geometry"
  st_geometry(HUC4_filtered) <- "geometry"
}

regional_sites <- sites_sf %>%
  st_transform(st_crs(HUC4_filtered)) %>%
  st_join(HUC4_filtered %>% select(huc4), left = FALSE)

regional_sdd_baseline <- baseline_match %>%
  filter(siteSR_id %in% regional_sites$siteSR_id) %>%
  left_join(
    regional_sites %>%
      st_drop_geometry() %>%
      select(siteSR_id, HUC4 = huc4) %>%
      distinct(siteSR_id, .keep_all = TRUE),
    by = "siteSR_id"
  )

reg_sdd_closest_baseline <- regional_sdd_baseline %>%
  arrange(abs(time_diff), mission) %>%
  slice(1, .by = c("subgroup_id"))

log("FINAL baseline training rows (no outlier removal, after region + closest-obs dedup): %s",
    format(nrow(reg_sdd_closest_baseline), big.mark = ","))

write_feather(reg_sdd_closest_baseline,
              file.path(out_dir, "filtered_regional_sdd_baseline_no_ransac.feather"))

saveRDS(list(
  sdd_rows = nrow(sdd_baseline),
  siteSR_rows = nrow(read_feather(siteSR_baseline_path, col_select = 1)),
  matchup_rows_5d = nrow(baseline_match),
  final_training_rows = nrow(reg_sdd_closest_baseline),
  current_final_training_rows = nrow(read_feather(file.path(aquamatch_dir, "filtered_regional_sdd.feather"), col_select = 1))
), file.path(out_dir, "baseline_counts.rds"))

log("done")
