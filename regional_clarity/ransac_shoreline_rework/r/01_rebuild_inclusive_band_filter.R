# Rework of 01_make_matches.Rmd's "Filter reflectance outliers before
# matching" section (ransac_site_multiband()), fixing a real scoping bug:
# the `sites` table that feeds representative-site selection there requires
# flag_optical_shoreline == 0 and a thermal-shoreline OR-condition - meaning
# every near-shore/outside-waterbody site (what this project's later
# `shore_flag` feature flags as 1, ~49% of the eventual modeling corpus)
# was NEVER evaluated by the band-outlier RANSAC filter at all, under any
# sensor set. This script re-runs the SAME algorithm, unchanged, on a
# `sites` table that keeps only the genuinely non-shoreline exclusion
# (`number_int_wb == 1` - a site touching 0 or >1 waterbodies can't be
# unambiguously assigned to one lake's per-site trend line, which is a
# geometry-ambiguity issue, not a shoreline-distance one) so every site
# gets its own per-site outlier evaluation regardless of shoreline flags.
#
# Standard canonical scope: all 5 missions, LS7-referenced Gardner
# correction - matching current production. This is deliberately NOT tied
# to the ls8_harmonization project's LS8-reference/LS8-9-only questions;
# it's a general pipeline fix meant to eventually feed every downstream
# workflow (outlier_rework, v2, v3, partition_sensitivity, ls8_harmonization).
#
# Reads aquamatch_files/ read-only; writes only into this project's own
# data/ folder - does NOT overwrite 01_make_matches.Rmd's own cached
# siteSR_DSWE1_regional_band_filtered.feather / _ransac_diagnostics.feather,
# so the original filter's output stays available for direct comparison.

suppressMessages({
  library(sf); library(igraph); library(dplyr); library(readr); library(tidyr)
  library(arrow); library(purrr); library(data.table); library(parallel)
})

aquamatch_dir <- "aquamatch_files"
out_dir <- "regional_clarity/ransac_shoreline_rework/data"
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

## ---- scene metadata (cloud-cover QA), identical to 01_make_matches.Rmd ----
scene_metadata_paths <- list.files(aquamatch_dir, pattern = "sceneMetadata_Landsat(457|89)\\.csv$", full.names = TRUE)
scene_metadata <- map(scene_metadata_paths, read_csv, show_col_types = FALSE) %>%
  bind_rows() %>%
  filter(CLOUD_COVER < 20)
log("scene_metadata: %s qualifying scenes", format(nrow(scene_metadata), big.mark = ","))

## ---- sites table: SAME geometry-ambiguity filter, shoreline exclusions dropped ----
sites <- read_csv(file.path(aquamatch_dir, "siteSR_collated_WQP_NWIS_sites_with_NHD_info_2025-06-04.csv"),
                   show_col_types = FALSE) %>%
  filter(number_int_wb == 1) %>%
  select(siteSR_id, wb_nhd_id, wb_gnis_name, wb_areasqkm,
         flag_thermal_TM_shoreline, flag_thermal_ETM_shoreline,
         flag_thermal_TIRS_shoreline, WGS84_Latitude, WGS84_Longitude)
log("sites after geometry-ambiguity QA only (no shoreline exclusion): %s", format(nrow(sites), big.mark = ","))

## ---- location_id de-dup: identical algorithm to 01_make_matches.Rmd ----
site_pts <- sites %>% st_as_sf(coords = c("WGS84_Longitude", "WGS84_Latitude"), crs = 4326, remove = FALSE)
site_within_100m <- st_is_within_distance(site_pts, dist = 100)
wb_nhd_id <- sites$wb_nhd_id
site_within_100m_same_wb <- Map(function(nbrs, wb) nbrs[wb_nhd_id[nbrs] == wb], site_within_100m, wb_nhd_id)
site_components <- components(graph_from_adj_list(site_within_100m_same_wb, mode = "all"))$membership
pad <- nchar(as.character(max(site_components)))
sites <- sites %>% mutate(location_id = sprintf(paste0("loc_%0", pad, "d"), site_components))
log("%s distinct locations after de-dup", n_distinct(sites$location_id))

representative_sites <- sites %>% distinct(location_id, .keep_all = TRUE) %>% pull(siteSR_id)
log("%s representative sites (was ~3,328 under the old shoreline-restricted sites table)",
    format(length(representative_sites), big.mark = ","))

## ---- regional/QA-scoped raw pull + Gardner LS7 correction (identical to original) ----
unified_data <- file.path(aquamatch_dir, "siteSR_DSWE1_full_concatenation.feather")
regional_huc2 <- c(10, 11, 13, 14, 15, 16, 17)

log("scanning unified_data (15.7GB, lazy Arrow predicate pushdown)...")
siteSR_regional_raw <- open_dataset(unified_data, format = "feather") %>%
  filter(huc2 %in% regional_huc2,
         pCount_dswe1 / pCount_dswe_gt0 > 0.5,
         pCount_dswe1 > 8,
         prop_clouds == 0,
         sat_id %in% scene_metadata$sat_id) %>%
  collect()
log("siteSR_regional_raw: %s rows", format(nrow(siteSR_regional_raw), big.mark = ","))

siteSR_regional_dedup <- siteSR_regional_raw %>% filter(siteSR_id %in% representative_sites)
log("siteSR_regional_dedup (representative sites only): %s rows, %s sites",
    format(nrow(siteSR_regional_dedup), big.mark = ","), n_distinct(siteSR_regional_dedup$siteSR_id))

am_corr_LS7 <- read_csv(file.path(aquamatch_dir, "lakeSR_collated_handoffs_GEEv2025-02-12_QAv2025-06-04.csv"),
                         show_col_types = FALSE) %>%
  filter(correction == "Gardner", dswe == "DSWE1", sat_to == "LS7") %>%
  pivot_longer(cols = c(intercept, B1, B2, min_in_val, max_in_val), names_to = "int_coef", values_to = "value") %>%
  mutate(new_column = paste(band, int_coef, sep = "_")) %>%
  select(-c(band, int_coef, slope, method)) %>%
  pivot_wider(names_from = new_column, values_from = value)

siteSR_regional_corr <- siteSR_regional_dedup %>%
  mutate(sat_corr = case_when(mission == "LT04" ~ "LS5", mission == "LT05" ~ "LS5", mission == "LE07" ~ "LS7",
                              mission == "LC08" ~ "LS8", mission == "LC09" ~ "LS8", TRUE ~ NA_character_)) %>%
  left_join(., am_corr_LS7) %>%
  mutate(red_corr7 = med_Red_intercept + med_Red_B1*med_Red + med_Red_B2*med_Red^2,
         green_corr7 = med_Green_intercept + med_Green_B1*med_Green + med_Green_B2*med_Green^2,
         blue_corr7 = med_Blue_intercept + med_Blue_B1*med_Blue + med_Blue_B2*med_Blue^2,
         nir_corr7 = med_Nir_intercept + med_Nir_B1*med_Nir + med_Nir_B2*med_Nir^2,
         swir1_corr7 = med_Swir1_intercept + med_Swir1_B1*med_Swir1 + med_Swir1_B2*med_Swir1^2,
         swir2_corr7 = med_Swir2_intercept + med_Swir2_B1*med_Swir2 + med_Swir2_B2*med_Swir2^2,
         temp_corr7 = med_SurfaceTemp_intercept +
           med_SurfaceTemp_B1*med_SurfaceTemp + med_SurfaceTemp_B2*med_SurfaceTemp^2) %>%
  mutate(red_corr7 = if_else(sat_corr == "LS7", med_Red, red_corr7),
         green_corr7 = if_else(sat_corr == "LS7", med_Green, green_corr7),
         blue_corr7 = if_else(sat_corr == "LS7", med_Blue, blue_corr7),
         nir_corr7 = if_else(sat_corr == "LS7", med_Nir, nir_corr7),
         swir1_corr7 = if_else(sat_corr == "LS7", med_Swir1, swir1_corr7),
         swir2_corr7 = if_else(sat_corr == "LS7", med_Swir2, swir2_corr7),
         temp_corr7 = if_else(sat_corr == "LS7", med_SurfaceTemp, temp_corr7)) %>%
  select(-c(all_of(names(am_corr_LS7))))

log("applied Gardner LS7 correction: %s rows", format(nrow(siteSR_regional_corr), big.mark = ","))

## ---- ransac_site_multiband(): byte-for-byte identical to 01_make_matches.Rmd ----
band_cols <- c("red_corr7", "green_corr7", "blue_corr7", "nir_corr7", "swir1_corr7", "swir2_corr7", "temp_corr7")

ols_fit <- function(t, y) {
  ok <- is.finite(t) & is.finite(y)
  if (sum(ok) < 2 || length(unique(t[ok])) < 2) {
    return(c(intercept = NA_real_, slope = NA_real_))
  }
  tt <- t[ok]; yy <- y[ok]
  tm <- mean(tt); ym <- mean(yy)
  slope <- sum((tt - tm) * (yy - ym)) / sum((tt - tm)^2)
  c(intercept = ym - slope * tm, slope = slope)
}

ransac_site_multiband <- function(date, band_mat,
                                  n_iter = 100,
                                  n_mad = 2,
                                  min_diff = c(red_corr7 = 0.01, green_corr7 = 0.01,
                                               blue_corr7 = 0.01, nir_corr7 = 0.01,
                                               swir1_corr7 = 0.01, swir2_corr7 = 0.01,
                                               temp_corr7 = 1),
                                  min_sample = 8) {
  n <- nrow(band_mat)
  nb <- ncol(band_mat)
  if (n < min_sample + 1) return(rep(NA, n))

  min_diff_b <- min_diff[colnames(band_mat)]
  t <- as.numeric(date)

  band_scale <- vapply(seq_len(nb), function(b) {
    y <- band_mat[, b]
    cf <- ols_fit(t, y)
    pred <- cf["intercept"] + cf["slope"] * t
    resid <- y - pred
    s <- mad(resid, na.rm = TRUE)
    if (is.na(s) || s == 0) s <- sd(resid, na.rm = TRUE)
    s
  }, numeric(1))

  best_inliers <- rep(FALSE, n)
  best_count <- 0

  for (i in 1:n_iter) {
    idx <- sample(n, min_sample)
    if (length(unique(t[idx])) < 2) next

    z <- matrix(NA_real_, nrow = n, ncol = nb)
    for (b in seq_len(nb)) {
      y <- band_mat[, b]
      if (length(unique(y[idx])) < 2 || is.na(band_scale[b]) || band_scale[b] == 0) next

      cf <- ols_fit(t[idx], y[idx])
      if (anyNA(cf)) next
      pred <- cf["intercept"] + cf["slope"] * t
      resid <- y - pred
      z[, b] <- abs(resid) / band_scale[b]
      z[abs(resid) < min_diff_b[b], b] <- 0
    }

    combined_z <- rowMeans(z, na.rm = TRUE)
    inliers <- !is.na(combined_z) & combined_z < n_mad

    if (sum(inliers) > best_count) {
      best_count <- sum(inliers)
      best_inliers <- inliers
    }
  }

  if (best_count == 0) return(rep(NA, n))

  if (any(best_inliers)) {
    in_range <- vapply(seq_len(nb), function(b) {
      y <- band_mat[, b]
      accepted <- y[best_inliers]
      if (length(accepted) == 0 || all(is.na(accepted))) return(rep(TRUE, n))
      rng <- range(accepted, na.rm = TRUE)
      y >= rng[1] & y <= rng[2]
    }, logical(n))
    best_inliers <- best_inliers | apply(in_range, 1, all)
  }

  best_inliers
}

set.seed(47)
setDT(siteSR_regional_corr)
setorder(siteSR_regional_corr, siteSR_id, date)

site_splits <- split(siteSR_regional_corr, by = "siteSR_id", keep.by = TRUE)
log("running per-site RANSAC on %s sites (parallelized)...", format(length(site_splits), big.mark = ","))

site_splits_flagged <- parallel::mclapply(
  site_splits,
  function(site_df) {
    site_df[, is_band_inlier := ransac_site_multiband(date, as.matrix(.SD)), .SDcols = band_cols]
    site_df
  },
  mc.cores = max(1, parallel::detectCores() - 2)
)

siteSR_regional_corr <- rbindlist(site_splits_flagged)
rm(site_splits, site_splits_flagged)
gc()

log("RANSAC done: inlier rate among representative-site rows = %.3f", mean(siteSR_regional_corr$is_band_inlier, na.rm = TRUE))

write_feather(siteSR_regional_corr, file.path(out_dir, "siteSR_regional_band_ransac_diagnostics_inclusive.feather"))

## ---- expand representative decisions back out to every geometry-QA'd site ----
location_lookup <- sites %>% select(siteSR_id, location_id)

inlier_by_location_scene <- siteSR_regional_corr %>%
  select(siteSR_id, sat_id, is_band_inlier) %>%
  left_join(location_lookup, by = "siteSR_id") %>%
  select(location_id, sat_id, is_band_inlier) %>%
  distinct()

siteSR_regional_filtered_df <- siteSR_regional_raw %>%
  filter(siteSR_id %in% sites$siteSR_id) %>%
  left_join(location_lookup, by = "siteSR_id") %>%
  left_join(inlier_by_location_scene, by = c("location_id", "sat_id")) %>%
  filter(is_band_inlier) %>%
  select(-location_id, -is_band_inlier)

write_feather(siteSR_regional_filtered_df, file.path(out_dir, "siteSR_regional_band_filtered_inclusive.feather"))
write_feather(sites, file.path(out_dir, "sites_inclusive_with_location_id.feather"))

log("wrote inclusive filtered pool: %s rows (vs %s rows in the original shoreline-restricted filter)",
    format(nrow(siteSR_regional_filtered_df), big.mark = ","),
    format(nrow(read_feather(file.path(aquamatch_dir, "siteSR_DSWE1_regional_band_filtered.feather"))), big.mark = ","))
log("done")
