suppressMessages({
  library(arrow); library(dplyr); library(readr); library(sf); library(AquaMatchr)
})
aquamatch_dir <- "aquamatch_files"; out_dir <- "regional_clarity/outlier_rework"
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")
regional_huc2 <- c(10, 11, 13, 14, 15, 16, 17)
unified_data <- file.path(aquamatch_dir, "siteSR_DSWE1_full_concatenation.feather")
sr_sites_csv <- list.files(aquamatch_dir, pattern = "sites_with_NHD_info.*\\.csv$", full.names = TRUE)[1]
sites_raw <- read_csv(sr_sites_csv, show_col_types = FALSE)
sites_geom_relaxed <- sites_raw %>% filter(number_int_wb == 1)
scene_metadata_all <- list.files(aquamatch_dir, pattern = "sceneMetadata_Landsat(457|89)\\.csv$", full.names = TRUE) %>%
  purrr::map(read_csv, show_col_types = FALSE) %>% bind_rows()
log("reading siteSR data, HUC2 + relaxed geometry QA scoped")
siteSR_relaxed <- open_dataset(unified_data, format = "feather") %>%
  filter(huc2 %in% regional_huc2, siteSR_id %in% sites_geom_relaxed$siteSR_id) %>% collect()
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
  m_regional <- m %>% filter(siteSR_id %in% regional_sites$siteSR_id) %>%
    left_join(regional_sites %>% st_drop_geometry() %>% select(siteSR_id, HUC4 = huc4) %>%
                distinct(siteSR_id, .keep_all = TRUE), by = "siteSR_id")
  final <- m_regional %>% arrange(abs(time_diff), mission) %>% slice(1, .by = c("subgroup_id")) %>%
    filter(grepl("Lake|Reservoir", MonitoringLocationTypeName), !grepl("Great", MonitoringLocationTypeName))
  file.remove(sr_path, sdd_path, match_path)
  tibble::tibble(config = label, window_days = window_days,
                 siteSR_candidate_rows = nrow(siteSR_df), matchup_rows = nrow(m), final_training_rows = nrow(final))
}
add_meta <- function(df) df %>% filter(pCount_dswe1 / pCount_dswe_gt0 > 0.5, pCount_dswe1 > 8, prop_clouds == 0)
sr_cloud20 <- siteSR_relaxed %>% filter(sat_id %in% (scene_metadata_all %>% filter(CLOUD_COVER < 20) %>% pull(sat_id)))
sr_meta_cloud20 <- add_meta(sr_cloud20)
log("candidates with relaxed geom + meta + cloud<20: %s", format(nrow(sr_meta_cloud20), big.mark=","))
result <- run_config("relaxedgeom+meta+cloud20_w5", sr_meta_cloud20, sdd_new, 5)
print(result)
readr::write_csv(result, file.path(out_dir, "relaxed_geometry_cloud20_test.csv"))
log("done")
