# Build the LS8-harmonization base dataset: same rows/splits/site/weather
# columns as outlier_rework's modeling_dataset_expanded.parquet, restricted
# to mission %in% c("LE07","LC08","LC09") (LT04/LT05 dropped - no direct
# LS5/LS4->LS8 Gardner coefficient exists upstream, see plan), carrying BOTH
# the existing LS7-referenced bands (*_corr7, joined straight across) and
# newly-computed LS8-referenced bands (*_corr8), so the controlled
# LS7-vs-LS8 comparison and the LS8/9-only investigation can both be run
# from one shared file.
#
# filtered_regional_sdd_final.feather and modeling_dataset_expanded.parquet
# are confirmed exactly row-order aligned (same 12,637 rows, same order) -
# verified directly before writing this script - so raw med_* bands are
# attached by column bind, not a key join (duplicate (siteSR_id,date,mission)
# keys exist, so a join would fan out).

suppressMessages({
  library(arrow); library(dplyr); library(readr); library(tidyr)
})

rework_dir <- "regional_clarity/outlier_rework"
aquamatch_dir <- "aquamatch_files"
out_dir <- "regional_clarity/ls8_harmonization"
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

raw <- read_feather(file.path(rework_dir, "filtered_regional_sdd_final.feather"))
expanded <- read_parquet(file.path(rework_dir, "python", "data", "modeling_dataset_expanded.parquet"))
log("loaded raw (%s rows) and expanded (%s rows)", format(nrow(raw), big.mark=","), format(nrow(expanded), big.mark=","))

stopifnot(
  "raw/expanded row-order alignment broken - re-derive the join" =
    all(raw$siteSR_id == expanded$siteSR_id) &&
    all(raw$date == expanded$date) &&
    all(raw$mission == expanded$mission) &&
    all(raw$harmonized_value == expanded$harmonized_value)
)

base <- expanded %>%
  mutate(sat_id = raw$sat_id,
         med_Blue = raw$med_Blue, med_Green = raw$med_Green, med_Red = raw$med_Red,
         med_Nir = raw$med_Nir, med_Swir1 = raw$med_Swir1, med_Swir2 = raw$med_Swir2,
         med_SurfaceTemp = raw$med_SurfaceTemp)

## ---- Gardner cross-sensor correction, referenced to LS8 ----
am_corr_LS8 <- read_csv(file.path(aquamatch_dir, "lakeSR_collated_handoffs_GEEv2025-02-12_QAv2025-06-04.csv"),
                         show_col_types = FALSE) %>%
  filter(correction == "Gardner", dswe == "DSWE1", sat_to == "LS8") %>%
  pivot_longer(cols = c(intercept, B1, B2, min_in_val, max_in_val), names_to = "int_coef", values_to = "value") %>%
  mutate(new_column = paste(band, int_coef, sep = "_")) %>%
  select(-c(band, int_coef, slope, method)) %>%
  pivot_wider(names_from = new_column, values_from = value)
log("LS8-target coefficient rows available for sat_corr: %s", paste(am_corr_LS8$sat_corr, collapse = ", "))

base <- base %>%
  mutate(sat_corr = case_when(mission == "LT04" ~ "LS5", mission == "LT05" ~ "LS5", mission == "LE07" ~ "LS7",
                              mission == "LC08" ~ "LS8", mission == "LC09" ~ "LS8", TRUE ~ NA_character_)) %>%
  left_join(am_corr_LS8, by = "sat_corr") %>%
  mutate(red_corr8 = med_Red_intercept + med_Red_B1*med_Red + med_Red_B2*med_Red^2,
         green_corr8 = med_Green_intercept + med_Green_B1*med_Green + med_Green_B2*med_Green^2,
         blue_corr8 = med_Blue_intercept + med_Blue_B1*med_Blue + med_Blue_B2*med_Blue^2,
         nir_corr8 = med_Nir_intercept + med_Nir_B1*med_Nir + med_Nir_B2*med_Nir^2,
         swir1_corr8 = med_Swir1_intercept + med_Swir1_B1*med_Swir1 + med_Swir1_B2*med_Swir1^2,
         swir2_corr8 = med_Swir2_intercept + med_Swir2_B1*med_Swir2 + med_Swir2_B2*med_Swir2^2,
         temp_corr8 = med_SurfaceTemp_intercept + med_SurfaceTemp_B1*med_SurfaceTemp + med_SurfaceTemp_B2*med_SurfaceTemp^2) %>%
  mutate(red_corr8 = if_else(sat_corr == "LS8", med_Red, red_corr8),
         green_corr8 = if_else(sat_corr == "LS8", med_Green, green_corr8),
         blue_corr8 = if_else(sat_corr == "LS8", med_Blue, blue_corr8),
         nir_corr8 = if_else(sat_corr == "LS8", med_Nir, nir_corr8),
         swir1_corr8 = if_else(sat_corr == "LS8", med_Swir1, swir1_corr8),
         swir2_corr8 = if_else(sat_corr == "LS8", med_Swir2, swir2_corr8),
         temp_corr8 = if_else(sat_corr == "LS8", med_SurfaceTemp, temp_corr8))

log("applied LS8-reference correction to %s rows", format(nrow(base), big.mark=","))

## ---- passthrough correctness check (LC08/LC09 corr8 must equal raw bands exactly,
## NA-for-NA - one LC08/LC09 row has a genuinely missing med_SurfaceTemp) ----
l8_rows <- base %>% filter(sat_corr == "LS8")
exactly_equal_or_both_na <- function(a, b) all(a == b, na.rm = TRUE) && identical(is.na(a), is.na(b))
stopifnot(
  "LC08/LC09 corr8 passthrough failed - not exactly equal to raw med_* bands" =
    exactly_equal_or_both_na(l8_rows$red_corr8, l8_rows$med_Red) &&
    exactly_equal_or_both_na(l8_rows$green_corr8, l8_rows$med_Green) &&
    exactly_equal_or_both_na(l8_rows$blue_corr8, l8_rows$med_Blue) &&
    exactly_equal_or_both_na(l8_rows$nir_corr8, l8_rows$med_Nir) &&
    exactly_equal_or_both_na(l8_rows$swir1_corr8, l8_rows$med_Swir1) &&
    exactly_equal_or_both_na(l8_rows$swir2_corr8, l8_rows$med_Swir2) &&
    exactly_equal_or_both_na(l8_rows$temp_corr8, l8_rows$med_SurfaceTemp)
)
log("passthrough check OK: %s LC08/LC09 rows, corr8 == raw bands exactly", format(nrow(l8_rows), big.mark=","))

## ---- drop LT04/LT05: no direct LS5/LS4->LS8 correction exists upstream ----
before_n <- nrow(base)
before_counts <- table(base$mission)
base_ls8 <- base %>% filter(mission %in% c("LE07", "LC08", "LC09")) %>%
  select(-all_of(setdiff(names(am_corr_LS8), "sat_corr")))
stopifnot("expected all dropped rows to be LT04/LT05 with NA corr8" =
            all(is.na(filter(base, !mission %in% c("LE07","LC08","LC09"))$red_corr8)))
log("dropped LT04/LT05 (no LS5/LS4->LS8 coefficient): %s -> %s rows", format(before_n, big.mark=","), format(nrow(base_ls8), big.mark=","))
log("before (all missions): %s", paste(capture.output(print(before_counts)), collapse = " | "))
log("after (LE07/LC08/LC09 only): %s", paste(capture.output(print(table(base_ls8$mission))), collapse = " | "))

## ---- export: corr7 (existing) + corr8 (new) + site/weather columns, mission preserved ----
final <- base_ls8 %>%
  select(siteSR_id, sat_id, date, HUC4, harmonized_value, mission, time_diff, misc_flag,
         atm_corr_LaSRC, lat, lon,
         red_corr7, green_corr7, blue_corr7, nir_corr7, swir1_corr7, swir2_corr7, temp_corr7,
         red_corr8, green_corr8, blue_corr8, nir_corr8, swir1_corr8, swir2_corr8, temp_corr8,
         elevation_m, catchment_area_sqkm,
         pct_impervious_2006, pct_urban_2006, pct_forest_2006, pct_cropland_2006, pct_wetland_2006,
         starts_with("precip_mm_prev"), starts_with("tmax_degC_prev"),
         starts_with("tmean_degC_prev"), starts_with("tmin_degC_prev"), starts_with("srad_Wm2_prev"))

data_dir <- file.path(out_dir, "data")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
write_parquet(final, file.path(data_dir, "base_expanded.parquet"))
log("exported: %s rows, %s cols -> %s", format(nrow(final), big.mark=","), ncol(final),
    file.path(data_dir, "base_expanded.parquet"))
log("done")
