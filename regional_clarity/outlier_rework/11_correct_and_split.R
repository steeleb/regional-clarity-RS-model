# Apply the Gardner et al. cross-sensor correction (same as
# 04_make_models.Rmd) to the final matched dataset, then split into 5
# HUC4-grouped partitions using the same greedy bin-packing as
# 03_split_data.Rmd, and export a modeling-ready parquet for the Python
# model comparison.

suppressMessages({
  library(arrow); library(dplyr); library(readr); library(tidyr)
})

out_dir <- "regional_clarity/outlier_rework"
aquamatch_dir <- "aquamatch_files"
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

df <- read_feather(file.path(out_dir, "filtered_regional_sdd_final.feather"))
log("loaded %s rows", format(nrow(df), big.mark=","))

## ---- Gardner cross-sensor correction (identical to 04_make_models.Rmd) ----
am_corr_LS7 <- read_csv(file.path(aquamatch_dir, "lakeSR_collated_handoffs_GEEv2025-02-12_QAv2025-06-04.csv"),
                         show_col_types = FALSE) %>%
  filter(correction == "Gardner", dswe == "DSWE1", sat_to == "LS7") %>%
  pivot_longer(cols = c(intercept, B1, B2, min_in_val, max_in_val), names_to = "int_coef", values_to = "value") %>%
  mutate(new_column = paste(band, int_coef, sep = "_")) %>%
  select(-c(band, int_coef, slope, method)) %>%
  pivot_wider(names_from = new_column, values_from = value)

df_corr <- df %>%
  mutate(sat_corr = case_when(mission == "LT04" ~ "LS5", mission == "LT05" ~ "LS5", mission == "LE07" ~ "LS7",
                              mission == "LC08" ~ "LS8", mission == "LC09" ~ "LS8", TRUE ~ NA_character_),
         atm_corr = case_when(mission %in% c("LT04", "LT05", "LE07") ~ "LEDAPS",
                              mission %in% c("LC08", "LC09") ~ "LaSRC", TRUE ~ NA_character_),
         atm_corr_LaSRC = as.numeric(atm_corr == "LaSRC")) %>%
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

log("applied sensor correction; rows: %s", format(nrow(df_corr), big.mark=","))

## ---- 5-partition HUC4 greedy bin-packing (identical to 03_split_data.Rmd) ----
huc4_sizes <- df_corr %>% group_by(HUC4) %>% summarize(count = n()) %>% arrange(desc(count))
parts <- vector("list", 5)
part_sizes <- rep(0, 5)
for (i in 1:nrow(huc4_sizes)) {
  smallest_part <- which.min(part_sizes)
  parts[[smallest_part]] <- c(parts[[smallest_part]], huc4_sizes$HUC4[i])
  part_sizes[smallest_part] <- part_sizes[smallest_part] + huc4_sizes$count[i]
}
log("partition sizes: %s", paste(part_sizes, collapse = ", "))

huc4_to_part <- tibble(HUC4 = unlist(parts), part = rep(seq_along(parts), lengths(parts)))
df_split <- df_corr %>% left_join(huc4_to_part, by = "HUC4")

stopifnot(!anyNA(df_split$part))

write_feather(df_split, file.path(out_dir, "split_filtered_regional_final.feather"))

## ---- export modeling-ready parquet for Python ----
model_cols <- df_split %>%
  select(siteSR_id, date, HUC4, part, harmonized_value, mission, time_diff, misc_flag,
         atm_corr_LaSRC, lat, lon,
         red_corr7, green_corr7, blue_corr7, nir_corr7, swir1_corr7, swir2_corr7, temp_corr7)

py_dir <- "regional_clarity/outlier_rework/python/data"
dir.create(py_dir, showWarnings = FALSE, recursive = TRUE)
write_parquet(model_cols, file.path(py_dir, "modeling_dataset.parquet"))
log("exported modeling dataset: %s rows, %s cols -> %s",
    format(nrow(model_cols), big.mark=","), ncol(model_cols),
    file.path(py_dir, "modeling_dataset.parquet"))

log("done")
