# Final end-to-end pipeline combining every decision from the overnight
# investigation:
#
#  1. Region prefilter (HUC2, free/lossless) - unchanged.
#  2. Match window: 5 days, UNCHANGED (explicit user direction - not a
#     tunable lever here, even though widening it was the only way to hit
#     10k under the original geometry QA).
#  3. Site geometry QA relaxed to number_int_wb == 1 ONLY - the shoreline
#     flags (flag_optical_shoreline, thermal shoreline flags) are dropped
#     per user direction: shoreline/near-shore contamination is already
#     handled upstream in the siteSR remote-sensing pipeline, so
#     re-filtering on it here is redundant. number_int_wb == 1 is KEPT -
#     ablation showed removing it (not the shoreline flags) is what causes
#     a combinatorial join blowup, since it's what prevents one WQP site
#     from fanning out across many ambiguously-assigned multi-waterbody
#     siteSR candidates.
#  4. Metadata QA (pCount ratios + prop_clouds==0 per-pixel cloud check) -
#     kept as-is, cheap and legitimate.
#  5. Scene-level cloud-cover threshold - kept, but relaxed from <20% to
#     <40% per user direction (still a real filter, just less strict), on
#     top of the per-pixel prop_clouds==0 check in (4).
#  6. SDD-value RANSAC (is_inlier) - now actually wired up as a filter
#     (previously a no-op), but gently: only confirmed outliers (FALSE)
#     are dropped; indeterminate (NA, too-few-observations) sites are kept
#     rather than penalized for lack of evidence.
#  7. Reflectance-band RANSAC - rebuilt as majority-vote scoring (see
#     01_redesign_band_filter.R) instead of winner-take-all, computed fresh
#     on the relaxed-geometry candidate pool, with missing (location_id,
#     sat_id) lookups defaulting to "keep" rather than "drop".

suppressMessages({
  library(arrow); library(dplyr); library(readr); library(sf); library(tidyr)
  library(data.table); library(parallel); library(AquaMatchr); library(igraph)
})

aquamatch_dir <- "aquamatch_files"
out_dir <- "regional_clarity/outlier_rework"
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

WINDOW_DAYS <- 5
VOTE_FRAC_CUTOFF <- 0.5   # band-level: point must be called inlier in >=50% of valid iterations

regional_huc2 <- c(10, 11, 13, 14, 15, 16, 17)
unified_data <- file.path(aquamatch_dir, "siteSR_DSWE1_full_concatenation.feather")

## ============================================================
## 1. SDD side: QAQC filters + gentle RANSAC (keep TRUE/NA, drop FALSE)
## ============================================================
log("building SDD side: QAQC + gentle RANSAC filter")
sdd_all <- read_feather(file.path(aquamatch_dir, "sdd_quality_filter.feather"))
sdd_final <- sdd_all %>% filter(is_inlier != FALSE | is.na(is_inlier))
log("SDD rows: %s QAQC-passed -> %s after gentle RANSAC (dropped %s confirmed outliers, %.2f%%)",
    format(nrow(sdd_all), big.mark=","), format(nrow(sdd_final), big.mark=","),
    format(sum(sdd_all$is_inlier == FALSE, na.rm = TRUE), big.mark=","),
    100 * sum(sdd_all$is_inlier == FALSE, na.rm = TRUE) / nrow(sdd_all))
sdd_final_path <- file.path(out_dir, "sdd_final.feather")
write_feather(sdd_final %>% select(-is_inlier), sdd_final_path, compression = "zstd")

## ============================================================
## 2. siteSR side: relaxed geometry QA + metadata QA + cloud QA (all kept
##    at original strict settings except the shoreline flags)
## ============================================================
sr_sites_csv <- list.files(aquamatch_dir, pattern = "sites_with_NHD_info.*\\.csv$", full.names = TRUE)[1]
sites_raw <- read_csv(sr_sites_csv, show_col_types = FALSE)
sites_geomQA <- sites_raw %>%
  filter(number_int_wb == 1) %>%
  select(siteSR_id, wb_nhd_id, wb_gnis_name, wb_areasqkm,
         flag_thermal_TM_shoreline, flag_thermal_ETM_shoreline, flag_thermal_TIRS_shoreline,
         WGS84_Latitude, WGS84_Longitude)

CLOUD_COVER_THRESHOLD <- 40  # relaxed from the original 20% per user direction

scene_metadata_paths <- list.files(aquamatch_dir, pattern = "sceneMetadata_Landsat(457|89)\\.csv$", full.names = TRUE)
scene_metadata <- purrr::map(scene_metadata_paths, read_csv, show_col_types = FALSE) %>%
  bind_rows() %>% filter(CLOUD_COVER < CLOUD_COVER_THRESHOLD)

log("100m/same-waterbody site de-dup (for RANSAC representative-site selection)")
site_pts <- sites_geomQA %>% st_as_sf(coords = c("WGS84_Longitude", "WGS84_Latitude"), crs = 4326, remove = FALSE)
site_within_100m <- st_is_within_distance(site_pts, dist = 100)
wb_nhd_id <- sites_geomQA$wb_nhd_id
site_within_100m_same_wb <- Map(function(nbrs, wb) nbrs[wb_nhd_id[nbrs] == wb], site_within_100m, wb_nhd_id)
site_components <- components(graph_from_adj_list(site_within_100m_same_wb, mode = "all"))$membership
pad <- nchar(as.character(max(site_components)))
sites_geomQA <- sites_geomQA %>% mutate(location_id = sprintf(paste0("loc_%0", pad, "d"), site_components))

log("scanning unified_data: HUC2 + relaxed geometry QA + metadata QA + cloud<%s%% QA", CLOUD_COVER_THRESHOLD)
siteSR_final_raw <- open_dataset(unified_data, format = "feather") %>%
  filter(huc2 %in% regional_huc2,
         pCount_dswe1 / pCount_dswe_gt0 > 0.5, pCount_dswe1 > 8, prop_clouds == 0,
         siteSR_id %in% sites_geomQA$siteSR_id,
         sat_id %in% scene_metadata$sat_id) %>%
  collect()
log("candidate rows (relaxed geometry + metadata + cloud QA): %s", format(nrow(siteSR_final_raw), big.mark=","))

## ---- Gardner et al. cross-sensor correction (needed for band RANSAC) ----
am_corr_LS7 <- read_csv(file.path(aquamatch_dir, "lakeSR_collated_handoffs_GEEv2025-02-12_QAv2025-06-04.csv"),
                         show_col_types = FALSE) %>%
  filter(correction == "Gardner", dswe == "DSWE1", sat_to == "LS7") %>%
  pivot_longer(cols = c(intercept, B1, B2, min_in_val, max_in_val), names_to = "int_coef", values_to = "value") %>%
  mutate(new_column = paste(band, int_coef, sep = "_")) %>%
  select(-c(band, int_coef, slope, method)) %>%
  pivot_wider(names_from = new_column, values_from = value)

representative_sites <- sites_geomQA %>% distinct(location_id, .keep_all = TRUE) %>% pull(siteSR_id)
siteSR_repr <- siteSR_final_raw %>% filter(siteSR_id %in% representative_sites)
log("representative sites for band RANSAC refit: %s rows across %s sites",
    format(nrow(siteSR_repr), big.mark=","), format(n_distinct(siteSR_repr$siteSR_id), big.mark=","))

siteSR_repr_corr <- siteSR_repr %>%
  mutate(sat_corr = case_when(mission == "LT04" ~ "LS5", mission == "LT05" ~ "LS5", mission == "LE07" ~ "LS7",
                              mission == "LC08" ~ "LS8", mission == "LC09" ~ "LS8", TRUE ~ NA_character_)) %>%
  left_join(am_corr_LS7, by = "sat_corr") %>%
  mutate(red_corr7 = med_Red_intercept + med_Red_B1*med_Red + med_Red_B2*med_Red^2,
         green_corr7 = med_Green_intercept + med_Green_B1*med_Green + med_Green_B2*med_Green^2,
         blue_corr7 = med_Blue_intercept + med_Blue_B1*med_Blue + med_Blue_B2*med_Blue^2,
         nir_corr7 = med_Nir_intercept + med_Nir_B1*med_Nir + med_Nir_B2*med_Nir^2,
         swir1_corr7 = med_Swir1_intercept + med_Swir1_B1*med_Swir1 + med_Swir1_B2*med_Swir1^2,
         swir2_corr7 = med_Swir2_intercept + med_Swir2_B1*med_Swir2 + med_Swir2_B2*med_Swir2^2,
         temp_corr7 = med_SurfaceTemp_intercept + med_SurfaceTemp_B1*med_SurfaceTemp + med_SurfaceTemp_B2*med_SurfaceTemp^2) %>%
  mutate(red_corr7 = if_else(sat_corr == "LS7", med_Red, red_corr7),
         green_corr7 = if_else(sat_corr == "LS7", med_Green, green_corr7),
         blue_corr7 = if_else(sat_corr == "LS7", med_Blue, blue_corr7),
         nir_corr7 = if_else(sat_corr == "LS7", med_Nir, nir_corr7),
         swir1_corr7 = if_else(sat_corr == "LS7", med_Swir1, swir1_corr7),
         swir2_corr7 = if_else(sat_corr == "LS7", med_Swir2, swir2_corr7),
         temp_corr7 = if_else(sat_corr == "LS7", med_SurfaceTemp, temp_corr7)) %>%
  select(-all_of(names(am_corr_LS7)))

## ============================================================
## 3. Majority-vote band RANSAC, computed fresh on the widened pool
## ============================================================
band_cols <- c("red_corr7", "green_corr7", "blue_corr7", "nir_corr7",
               "swir1_corr7", "swir2_corr7", "temp_corr7")

ols_fit <- function(t, y) {
  ok <- is.finite(t) & is.finite(y)
  if (sum(ok) < 2 || length(unique(t[ok])) < 2) return(c(intercept = NA_real_, slope = NA_real_))
  tt <- t[ok]; yy <- y[ok]; tm <- mean(tt); ym <- mean(yy)
  slope <- sum((tt - tm) * (yy - ym)) / sum((tt - tm)^2)
  c(intercept = ym - slope * tm, slope = slope)
}

ransac_site_multiband_v2 <- function(date, band_mat, n_iter = 150, n_mad_detect = 2.5,
                                      min_diff = c(red_corr7 = 0.01, green_corr7 = 0.01, blue_corr7 = 0.01,
                                                   nir_corr7 = 0.01, swir1_corr7 = 0.01, swir2_corr7 = 0.01,
                                                   temp_corr7 = 1),
                                      min_sample = 8) {
  n <- nrow(band_mat); nb <- ncol(band_mat)
  if (n < min_sample + 1) return(rep(NA_real_, n))
  min_diff_b <- min_diff[colnames(band_mat)]
  t <- as.numeric(date)
  band_scale <- vapply(seq_len(nb), function(b) {
    y <- band_mat[, b]; cf <- ols_fit(t, y); pred <- cf["intercept"] + cf["slope"] * t; resid <- y - pred
    s <- mad(resid, na.rm = TRUE); if (is.na(s) || s == 0) s <- sd(resid, na.rm = TRUE); s
  }, numeric(1))
  inlier_votes <- rep(0L, n); n_valid_iter <- 0L
  for (i in 1:n_iter) {
    idx <- sample(n, min_sample)
    if (length(unique(t[idx])) < 2) next
    z <- matrix(NA_real_, nrow = n, ncol = nb)
    for (b in seq_len(nb)) {
      y <- band_mat[, b]
      if (length(unique(y[idx])) < 2 || is.na(band_scale[b]) || band_scale[b] == 0) next
      cf <- ols_fit(t[idx], y[idx]); if (anyNA(cf)) next
      pred <- cf["intercept"] + cf["slope"] * t; resid <- y - pred
      z[, b] <- abs(resid) / band_scale[b]
      z[abs(resid) < min_diff_b[b], b] <- 0
    }
    combined_z <- rowMeans(z, na.rm = TRUE)
    inliers <- !is.na(combined_z) & combined_z < n_mad_detect
    inlier_votes <- inlier_votes + as.integer(inliers)
    n_valid_iter <- n_valid_iter + 1L
  }
  if (n_valid_iter == 0) return(rep(NA_real_, n))
  inlier_votes / n_valid_iter
}

log("running majority-vote band RANSAC across representative sites")
setDT(siteSR_repr_corr)
setorder(siteSR_repr_corr, siteSR_id, date)
site_splits <- split(siteSR_repr_corr, by = "siteSR_id", keep.by = TRUE)
t0 <- Sys.time()
site_splits_scored <- mclapply(site_splits, function(site_df) {
  site_df[, vote_frac := ransac_site_multiband_v2(date, as.matrix(.SD)), .SDcols = band_cols]
  site_df
}, mc.cores = max(1, detectCores() - 2))
log("scoring took %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))
siteSR_scored <- rbindlist(site_splits_scored)
rm(site_splits, site_splits_scored); gc()

# a flagged point can't be implausible if its band values fall within the
# range already spanned by accepted (vote_frac >= cutoff) points at that
# site - mirrors the SDD-side rescue logic
apply_rescue <- function(dt) {
  inlier <- dt$vote_frac >= VOTE_FRAC_CUTOFF & !is.na(dt$vote_frac)
  if (!any(inlier)) return(inlier)
  in_range <- vapply(band_cols, function(b) {
    y <- dt[[b]]; accepted <- y[inlier]
    if (length(accepted) == 0 || all(is.na(accepted))) return(rep(TRUE, nrow(dt)))
    rng <- range(accepted, na.rm = TRUE)
    y >= rng[1] & y <= rng[2]
  }, logical(nrow(dt)))
  inlier | apply(in_range, 1, all)
}
siteSR_scored[, is_band_inlier := apply_rescue(.SD), by = siteSR_id, .SDcols = c("vote_frac", band_cols)]
# a site the RANSAC never resolved (n_valid_iter == 0, vote_frac all NA) has
# no evidence against it - keep by default rather than drop
siteSR_scored[is.na(vote_frac), is_band_inlier := TRUE]

log("representative-site band flagging: %s / %s rows flagged as outliers (%.2f%%)",
    format(sum(!siteSR_scored$is_band_inlier), big.mark=","), format(nrow(siteSR_scored), big.mark=","),
    100 * sum(!siteSR_scored$is_band_inlier) / nrow(siteSR_scored))

## ---- expand representative-site decisions to all geometry-QA sites ----
## missing (location_id, sat_id) lookups default to "keep" (TRUE) rather
## than "drop" - the redesign's other key fix vs. the original filter.
location_lookup <- sites_geomQA %>% select(siteSR_id, location_id)
inlier_by_location_scene <- siteSR_scored %>%
  select(siteSR_id, sat_id, is_band_inlier) %>%
  left_join(location_lookup, by = "siteSR_id") %>%
  select(location_id, sat_id, is_band_inlier) %>%
  distinct()

siteSR_final <- siteSR_final_raw %>%
  left_join(location_lookup, by = "siteSR_id") %>%
  left_join(inlier_by_location_scene, by = c("location_id", "sat_id")) %>%
  mutate(is_band_inlier = coalesce(is_band_inlier, TRUE)) %>%
  filter(is_band_inlier) %>%
  select(-location_id, -is_band_inlier)

log("siteSR final candidate rows after band RANSAC: %s / %s (%.2f%% retained)",
    format(nrow(siteSR_final), big.mark=","), format(nrow(siteSR_final_raw), big.mark=","),
    100 * nrow(siteSR_final) / nrow(siteSR_final_raw))

siteSR_final_path <- file.path(out_dir, "siteSR_final.feather")
write_feather(siteSR_final, siteSR_final_path, compression = "zstd")
rm(siteSR_final_raw, siteSR_repr, siteSR_repr_corr, siteSR_scored); gc()

## ============================================================
## 4. Match with the widened window, then replicate 02's region/dedup logic
## ============================================================
dir.create(file.path(tempdir(), "duckdb"), recursive = TRUE, showWarnings = FALSE)
match_path <- file.path(out_dir, "sdd_final_matchups.parquet")
log("matching with a %s-day window", WINDOW_DAYS)
match_siteSR_to_WQP(wqp_path = sdd_final_path, siteSR_path = siteSR_final_path,
                     site_list_path = sr_sites_csv, save_location = match_path,
                     time_window = paste(WINDOW_DAYS, "days"))
m <- read_parquet(match_path)
log("raw matchup rows: %s", format(nrow(m), big.mark=","))

HUC4_filtered <- st_read(file.path(aquamatch_dir, "huc4_filtered.gdb"), layer = "huc4_filtered", quiet = TRUE)
gcol <- attr(HUC4_filtered, "sf_column")
if (gcol != "geometry") { names(HUC4_filtered)[names(HUC4_filtered) == gcol] <- "geometry"; st_geometry(HUC4_filtered) <- "geometry" }

sites_sf <- sites_raw %>% filter(siteSR_id %in% unique(m$siteSR_id)) %>%
  st_as_sf(coords = c("WGS84_Longitude", "WGS84_Latitude"), crs = "EPSG:4326")
regional_sites <- sites_sf %>% st_transform(st_crs(HUC4_filtered)) %>%
  st_join(HUC4_filtered %>% select(huc4), left = FALSE)

m_regional <- m %>%
  filter(siteSR_id %in% regional_sites$siteSR_id) %>%
  left_join(regional_sites %>% st_drop_geometry() %>% select(siteSR_id, HUC4 = huc4) %>%
              distinct(siteSR_id, .keep_all = TRUE), by = "siteSR_id")

reg_sdd_closest <- m_regional %>% arrange(abs(time_diff), mission) %>% slice(1, .by = c("subgroup_id"))
reg_sdd_additional <- anti_join(m_regional, reg_sdd_closest)

final_lakes <- reg_sdd_closest %>%
  filter(grepl("Lake|Reservoir", MonitoringLocationTypeName), !grepl("Great", MonitoringLocationTypeName))

log("FINAL training rows (post region filter + closest-obs dedup + lake-type filter): %s",
    format(nrow(final_lakes), big.mark=","))
log("median |time_diff| (days) among final rows: %.2f",
    median(abs(final_lakes$time_diff), na.rm = TRUE))
log("time_diff distribution (days, absolute):")
print(summary(abs(final_lakes$time_diff)))

write_feather(final_lakes, file.path(out_dir, "filtered_regional_sdd_final.feather"))
write_feather(reg_sdd_additional, file.path(out_dir, "regional_sdd_not_closest_final.feather"))

log("done")
