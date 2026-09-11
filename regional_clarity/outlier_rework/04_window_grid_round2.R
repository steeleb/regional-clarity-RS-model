# Round 2: push window width and cloud threshold further, since round 1
# showed roughly linear (not explosive) growth with window width - safe to
# extend. Best so far: cloud<60%, window=10d -> 8,707 final rows.

suppressMessages({
  library(arrow); library(dplyr); library(readr); library(sf); library(AquaMatchr)
})

aquamatch_dir <- "aquamatch_files"
out_dir <- "regional_clarity/outlier_rework"
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

regional_huc2 <- c(10, 11, 13, 14, 15, 16, 17)
unified_data <- file.path(aquamatch_dir, "siteSR_DSWE1_full_concatenation.feather")

sr_sites_csv <- list.files(aquamatch_dir, pattern = "sites_with_NHD_info.*\\.csv$", full.names = TRUE)[1]
sites_raw <- read_csv(sr_sites_csv, show_col_types = FALSE)
sites_geomQA <- sites_raw %>%
  filter(flag_optical_shoreline == 0, number_int_wb == 1,
         (flag_thermal_TM_shoreline == 0 | flag_thermal_ETM_shoreline == 0 | flag_thermal_TIRS_shoreline == 0))

scene_metadata_all <- list.files(aquamatch_dir, pattern = "sceneMetadata_Landsat(457|89)\\.csv$", full.names = TRUE) %>%
  purrr::map(read_csv, show_col_types = FALSE) %>% bind_rows()

log("reading siteSR data, HUC2 + geometry QA scoped")
siteSR_geom <- open_dataset(unified_data, format = "feather") %>%
  filter(huc2 %in% regional_huc2, siteSR_id %in% sites_geomQA$siteSR_id) %>%
  collect()

sdd_new <- read_feather(file.path(aquamatch_dir, "sdd_quality_filter.feather"), col_select = -is_inlier)

HUC4_filtered <- st_read(file.path(aquamatch_dir, "huc4_filtered.gdb"), layer = "huc4_filtered", quiet = TRUE)
gcol <- attr(HUC4_filtered, "sf_column")
if (gcol != "geometry") { names(HUC4_filtered)[names(HUC4_filtered) == gcol] <- "geometry"; st_geometry(HUC4_filtered) <- "geometry" }

run_config <- function(label, siteSR_df, sdd_df, window_days) {
  dir.create(file.path(tempdir(), "duckdb"), recursive = TRUE, showWarnings = FALSE)
  sr_path <- file.path(out_dir, paste0("tmp_sr_", label, ".feather"))
  sdd_path <- file.path(out_dir, paste0("tmp_sdd_", label, ".feather"))
  match_path <- file.path(out_dir, paste0("tmp_match_", label, ".parquet"))
  write_feather(siteSR_df, sr_path, compression = "zstd")
  write_feather(sdd_df, sdd_path, compression = "zstd")

  match_siteSR_to_WQP(wqp_path = sdd_path, siteSR_path = sr_path,
                       site_list_path = sr_sites_csv, save_location = match_path,
                       time_window = paste(window_days, "days"))
  m <- read_parquet(match_path)

  sites_sf <- sites_raw %>% filter(siteSR_id %in% unique(m$siteSR_id)) %>%
    st_as_sf(coords = c("WGS84_Longitude", "WGS84_Latitude"), crs = "EPSG:4326")
  regional_sites <- sites_sf %>% st_transform(st_crs(HUC4_filtered)) %>%
    st_join(HUC4_filtered %>% select(huc4), left = FALSE)

  m_regional <- m %>%
    filter(siteSR_id %in% regional_sites$siteSR_id) %>%
    left_join(regional_sites %>% st_drop_geometry() %>% select(siteSR_id, HUC4 = huc4) %>%
                distinct(siteSR_id, .keep_all = TRUE), by = "siteSR_id")

  final <- m_regional %>% arrange(abs(time_diff), mission) %>% slice(1, .by = c("subgroup_id")) %>%
    filter(grepl("Lake|Reservoir", MonitoringLocationTypeName), !grepl("Great", MonitoringLocationTypeName))

  file.remove(sr_path, sdd_path, match_path)
  tibble::tibble(config = label, window_days = window_days,
                 siteSR_candidate_rows = nrow(siteSR_df), matchup_rows = nrow(m),
                 final_training_rows = nrow(final))
}

add_meta <- function(df) df %>% filter(pCount_dswe1 / pCount_dswe_gt0 > 0.5, pCount_dswe1 > 8, prop_clouds == 0)
sr_cloud60 <- siteSR_geom %>% filter(sat_id %in% (scene_metadata_all %>% filter(CLOUD_COVER < 60) %>% pull(sat_id)))
sr_nocloud <- siteSR_geom  # rely on pixel-level prop_clouds==0 only, no scene-level threshold at all
sr_meta_cloud60 <- add_meta(sr_cloud60)
sr_meta_nocloud <- add_meta(sr_nocloud)

configs <- list(
  list(label = "meta+cloud60_w14", df = sr_meta_cloud60, w = 14),
  list(label = "meta+nocloud_w10", df = sr_meta_nocloud, w = 10),
  list(label = "meta+nocloud_w14", df = sr_meta_nocloud, w = 14),
  list(label = "meta+nocloud_w7",  df = sr_meta_nocloud, w = 7)
)

results <- list()
for (cfg in configs) {
  log("running %s (candidates=%s, window=%s days)", cfg$label, format(nrow(cfg$df), big.mark=","), cfg$w)
  results[[cfg$label]] <- run_config(cfg$label, cfg$df, sdd_new, cfg$w)
  print(results[[cfg$label]])
}

grid_tbl <- dplyr::bind_rows(results)
readr::write_csv(grid_tbl, file.path(out_dir, "window_grid_round2_results.csv"))
log("ROUND 2 RESULTS:")
print(grid_tbl)
log("done")
