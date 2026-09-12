# v2 of outlier_rework's 23_export_expanded_dataset.R: identical join logic,
# adds shoreline_flag to the exported model_cols. dist_to_shore is kept in
# site_characteristics.feather for reference/diagnostics but deliberately
# NOT exported to the modeling table - only the binary/categorical
# shoreline_flag is a candidate model feature (see 20_add_shoreline_flag.R).

suppressMessages({
  library(arrow); library(dplyr)
})

out_dir <- "regional_clarity/outlier_rework_v2"
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

base <- read_feather(file.path(out_dir, "split_filtered_regional_final.feather"))
log("base dataset: %s rows", nrow(base))

base <- base %>%
  mutate(field_date = as.Date(harmonized_local_time),
         anchor_date = pmax(date, field_date))
log("anchor_date != date for %s / %s rows (image-before-field cases)",
    sum(base$anchor_date != base$date), nrow(base))

site_chars <- read_feather(file.path(out_dir, "site_characteristics.feather")) %>%
  select(-lat, -lon, -wb_nhd_id, -wb_gnis_name, -wb_areasqkm, -dist_to_shore)
weather <- read_feather(file.path(out_dir, "weather_summaries.feather")) %>%
  rename(anchor_date = date)

expanded <- base %>%
  left_join(site_chars, by = "siteSR_id") %>%
  left_join(weather, by = c("siteSR_id", "anchor_date"))

log("expanded dataset: %s rows, %s cols", nrow(expanded), ncol(expanded))
log("rows with elevation: %s, LakeCat: %s, shoreline_flag: %s, weather: %s",
    sum(!is.na(expanded$elevation_m)),
    sum(!is.na(expanded$pct_impervious_2006)),
    sum(!is.na(expanded$shoreline_flag)),
    sum(!is.na(expanded$precip_mm_prev1)))
stopifnot("weather join dropped rows - re-check the fetch range" = sum(!is.na(expanded$precip_mm_prev1)) == nrow(expanded))
stopifnot("shoreline join dropped rows" = sum(!is.na(expanded$shoreline_flag)) == nrow(expanded))

model_cols <- expanded %>%
  select(siteSR_id, date, HUC4, part, harmonized_value, mission, time_diff, misc_flag,
         atm_corr_LaSRC, lat, lon,
         red_corr7, green_corr7, blue_corr7, nir_corr7, swir1_corr7, swir2_corr7, temp_corr7,
         elevation_m, catchment_area_sqkm,
         pct_impervious_2006, pct_urban_2006, pct_forest_2006, pct_cropland_2006, pct_wetland_2006,
         shoreline_flag,
         starts_with("precip_mm_prev"), starts_with("tmax_degC_prev"),
         starts_with("tmean_degC_prev"), starts_with("tmin_degC_prev"), starts_with("srad_Wm2_prev"))

py_dir <- "regional_clarity/outlier_rework_v2/python/data"
write_parquet(model_cols, file.path(py_dir, "modeling_dataset_expanded.parquet"))
log("exported: %s rows, %s cols -> modeling_dataset_expanded.parquet", nrow(model_cols), ncol(model_cols))
log("done")
