# Ablation: the "before" pipeline (commit c59ddc9, pre-RANSAC) had none of
# the metadata/cloud/geometry QA filters the current pipeline (44546c9) adds
# on the siteSR candidate side, and no CV/lake-type pre-filter on the SDD
# side. The 00_measure_baseline.R result (5,381 rows with all *current* QA
# minus RANSAC, vs 5,199 with RANSAC) shows RANSAC itself costs only ~3.4%.
# So whatever is costing the other ~65% relative to the "before" pipeline
# must be one or more of those bundled QA filters, not RANSAC.
#
# This script isolates each filter's individual contribution to the FINAL
# (post region-filter, post closest-obs-dedup) training row count, by
# reading the siteSR data once (HUC2-scoped only - a free/lossless
# narrowing per script 01's own comment) and then trying each filter
# combination in-memory.

suppressMessages({
  library(arrow)
  library(dplyr)
  library(readr)
  library(sf)
  library(AquaMatchr)
})

aquamatch_dir <- "aquamatch_files"
out_dir <- "regional_clarity/outlier_rework"
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

regional_huc2 <- c(10, 11, 13, 14, 15, 16, 17)
unified_data <- file.path(aquamatch_dir, "siteSR_DSWE1_full_concatenation.feather")

## ---- read once, HUC2-scoped only (the one narrowing that costs nothing) ----
log("reading siteSR data, HUC2-scoped only (no other QA)")
t0 <- Sys.time()
siteSR_huc2 <- open_dataset(unified_data, format = "feather") %>%
  filter(huc2 %in% regional_huc2) %>%
  collect()
log("read took %.1f min, rows: %s", as.numeric(difftime(Sys.time(), t0, units = "mins")),
    format(nrow(siteSR_huc2), big.mark = ","))
gc()

scene_metadata <- list.files(aquamatch_dir, pattern = "sceneMetadata_Landsat(457|89)\\.csv$",
                              full.names = TRUE) %>%
  purrr::map(read_csv, show_col_types = FALSE) %>% bind_rows() %>%
  filter(CLOUD_COVER < 20)

sr_sites_csv <- list.files(aquamatch_dir, pattern = "sites_with_NHD_info.*\\.csv$", full.names = TRUE)[1]
sites_raw <- read_csv(sr_sites_csv, show_col_types = FALSE)
sites_geomQA <- sites_raw %>%
  filter(flag_optical_shoreline == 0,
         number_int_wb == 1,
         (flag_thermal_TM_shoreline == 0 |
            flag_thermal_ETM_shoreline == 0 |
            flag_thermal_TIRS_shoreline == 0))
log("sites: %s raw -> %s pass geometry QA (%.1f%% retained)",
    format(nrow(sites_raw), big.mark = ","), format(nrow(sites_geomQA), big.mark = ","),
    100 * nrow(sites_geomQA) / nrow(sites_raw))

## ---- SDD side variants ----
sdd_raw <- read_feather(file.path(aquamatch_dir, "sdd_newest.feather"))

sdd_old <- sdd_raw %>%
  filter(tier < 3, harmonized_value >= 0.1, misc_flag == 0, depth_flag == 0)
sdd_new <- sdd_raw %>%
  filter(tier < 3, harmonized_value >= 0.1, misc_flag == 0, depth_flag == 0,
         harmonized_value_cv <= 0.25 | is.na(harmonized_value_cv),
         ResolvedMonitoringLocationTypeName == "Lake, Reservoir, Impoundment")

log("SDD rows: old-style QA = %s, new-style QA = %s",
    format(nrow(sdd_old), big.mark = ","), format(nrow(sdd_new), big.mark = ","))

## ---- helper: given a siteSR data.frame and sdd data.frame, run match + dedup ----
HUC4_filtered <- st_read(file.path(aquamatch_dir, "huc4_filtered.gdb"), layer = "huc4_filtered", quiet = TRUE)
gc_col <- attr(HUC4_filtered, "sf_column")
if (gc_col != "geometry") { names(HUC4_filtered)[names(HUC4_filtered) == gc_col] <- "geometry"; st_geometry(HUC4_filtered) <- "geometry" }

run_config <- function(label, siteSR_df, sdd_df) {
  dir.create(file.path(tempdir(), "duckdb"), recursive = TRUE, showWarnings = FALSE)
  sr_path <- file.path(out_dir, paste0("tmp_sr_", label, ".feather"))
  sdd_path <- file.path(out_dir, paste0("tmp_sdd_", label, ".feather"))
  match_path <- file.path(out_dir, paste0("tmp_match_", label, ".parquet"))
  write_feather(siteSR_df, sr_path, compression = "zstd")
  write_feather(sdd_df, sdd_path, compression = "zstd")

  match_siteSR_to_WQP(wqp_path = sdd_path, siteSR_path = sr_path,
                       site_list_path = sr_sites_csv, save_location = match_path,
                       time_window = "5 days")
  m <- read_parquet(match_path)

  sites_sf <- sites_raw %>%
    filter(siteSR_id %in% unique(m$siteSR_id)) %>%
    st_as_sf(coords = c("WGS84_Longitude", "WGS84_Latitude"), crs = "EPSG:4326")
  regional_sites <- sites_sf %>% st_transform(st_crs(HUC4_filtered)) %>%
    st_join(HUC4_filtered %>% select(huc4), left = FALSE)

  m_regional <- m %>%
    filter(siteSR_id %in% regional_sites$siteSR_id) %>%
    left_join(regional_sites %>% st_drop_geometry() %>% select(siteSR_id, HUC4 = huc4) %>%
                distinct(siteSR_id, .keep_all = TRUE), by = "siteSR_id")

  final <- m_regional %>% arrange(abs(time_diff), mission) %>% slice(1, .by = c("subgroup_id"))

  # if lake-type wasn't pre-filtered on the SDD side, apply it post-match
  # (mirrors old pipeline's post-match "lakes-only" step) so all configs are
  # comparable on a lake-only final basis
  if (!"MonitoringLocationTypeName" %in% names(final)) {
    final_lakes <- final
  } else {
    final_lakes <- final %>%
      filter(grepl("Lake|Reservoir", MonitoringLocationTypeName),
             !grepl("Great", MonitoringLocationTypeName))
  }

  file.remove(sr_path, sdd_path, match_path)
  tibble::tibble(config = label, siteSR_candidate_rows = nrow(siteSR_df),
                 sdd_qaqc_rows = nrow(sdd_df), matchup_5d_rows = nrow(m),
                 final_training_rows = nrow(final_lakes))
}

## ---- build siteSR variants ----
## IMPORTANT: every variant below keeps site geometry QA (single-waterbody +
## shoreline flags). A first attempt without it (both "sr_none" configs)
## blew up to 25GB+ of raw matchup rows within ~90 seconds and was killed -
## geometry QA is what keeps one physical WQP site from fanning out across
## many ambiguously-assigned nearby siteSR_id pixels during the join, so
## it's load-bearing for the match itself being well-posed, not a tunable
## data-quality knob. This grid only varies metadata/cloud QA on top of it.
sr_geom_only  <- siteSR_huc2 %>% filter(siteSR_id %in% sites_geomQA$siteSR_id)
sr_geom_meta  <- sr_geom_only %>% filter(pCount_dswe1 / pCount_dswe_gt0 > 0.5, pCount_dswe1 > 8, prop_clouds == 0)
sr_geom_cloud <- sr_geom_only %>% filter(sat_id %in% scene_metadata$sat_id)
sr_geom_all   <- sr_geom_only %>% filter(pCount_dswe1 / pCount_dswe_gt0 > 0.5, pCount_dswe1 > 8, prop_clouds == 0,
                                          sat_id %in% scene_metadata$sat_id)

log("candidate pool sizes (all geometry-QA'd) -> geom-only: %s, geom+meta: %s, geom+cloud: %s, geom+meta+cloud(current): %s",
    format(nrow(sr_geom_only), big.mark=","), format(nrow(sr_geom_meta), big.mark=","),
    format(nrow(sr_geom_cloud), big.mark=","), format(nrow(sr_geom_all), big.mark=","))

## ---- run ablation grid ----
results <- list(
  run_config("geom-only_sdd-old",             sr_geom_only, sdd_old),
  run_config("geom-only_sdd-new",             sr_geom_only, sdd_new),
  run_config("geom+meta_sdd-new",             sr_geom_meta, sdd_new),
  run_config("geom+cloud_sdd-new",            sr_geom_cloud, sdd_new),
  run_config("geom+meta+cloud(current)_sdd-new", sr_geom_all, sdd_new)
)

ablation_tbl <- dplyr::bind_rows(results)
readr::write_csv(ablation_tbl, file.path(out_dir, "filter_ablation_results.csv"))
log("ABLATION RESULTS:")
print(ablation_tbl)
log("done")
