# Join site characteristics (elevation + LakeCat catchment) and antecedent
# weather summaries onto the split/corrected modeling dataset, and export
# for the Python feature-group comparison (optical-only vs +site vs
# +weather vs +both).

suppressMessages({
  library(arrow); library(dplyr)
})

out_dir <- "regional_clarity/outlier_rework"
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

base <- read_feather(file.path(out_dir, "split_filtered_regional_final.feather"))
log("base dataset: %s rows", nrow(base))

# anchor antecedent-weather windows on whichever of (image date, field
# sample date) is LATER, not always the image date. time_diff is a near-
# 50/50 split (n=6275 image-before-field, n=6384 image-after-field); when
# the image comes first, "previous N days before the image" never reaches
# forward to the field date, silently missing the image-to-field gap for
# about half the dataset. anchor_date = max(date, field_date) always
# covers that gap regardless of which direction the offset runs.
base <- base %>%
  mutate(field_date = as.Date(harmonized_local_time),
         anchor_date = pmax(date, field_date))
log("anchor_date != date for %s / %s rows (image-before-field cases)",
    sum(base$anchor_date != base$date), nrow(base))

site_chars <- read_feather(file.path(out_dir, "site_characteristics.feather")) %>%
  select(-lat, -lon, -wb_nhd_id, -wb_gnis_name, -wb_areasqkm)
weather <- read_feather(file.path(out_dir, "weather_summaries.feather")) %>%
  rename(anchor_date = date)

expanded <- base %>%
  left_join(site_chars, by = "siteSR_id") %>%
  left_join(weather, by = c("siteSR_id", "anchor_date"))

log("expanded dataset: %s rows, %s cols", nrow(expanded), ncol(expanded))
log("rows with elevation: %s, LakeCat: %s, weather: %s",
    sum(!is.na(expanded$elevation_m)),
    sum(!is.na(expanded$pct_impervious_2006)),
    sum(!is.na(expanded$precip_mm_prev1)))
stopifnot("weather join dropped rows - re-check the fetch range" = sum(!is.na(expanded$precip_mm_prev1)) == nrow(expanded))

model_cols <- expanded %>%
  select(siteSR_id, date, HUC4, part, harmonized_value, mission, time_diff, misc_flag,
         atm_corr_LaSRC, lat, lon,
         red_corr7, green_corr7, blue_corr7, nir_corr7, swir1_corr7, swir2_corr7, temp_corr7,
         elevation_m, catchment_area_sqkm,
         pct_impervious_2006, pct_urban_2006, pct_forest_2006, pct_cropland_2006, pct_wetland_2006,
         starts_with("precip_mm_prev"), starts_with("tmax_degC_prev"),
         starts_with("tmean_degC_prev"), starts_with("tmin_degC_prev"), starts_with("srad_Wm2_prev"))

py_dir <- "regional_clarity/outlier_rework/python/data"
write_parquet(model_cols, file.path(py_dir, "modeling_dataset_expanded.parquet"))
log("exported: %s rows, %s cols -> modeling_dataset_expanded.parquet", nrow(model_cols), ncol(model_cols))
log("done")
