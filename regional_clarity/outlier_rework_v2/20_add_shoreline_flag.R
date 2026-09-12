# Add the shoreline-proximity flag as a candidate categorical feature.
#
# The flag already exists per-site in AquaMatch_siteSR_WQP's site-compile
# step (a_compile_sites.R): dist_to_shore (site point -> nearest matched-
# waterbody-polygon boundary, meters) is thresholded per Landsat sensor
# family into flag_optical_shoreline (<=230m = 1, i.e. within the 200m site
# buffer + 30m optical pixel), with NA when the site sits outside its
# matched waterbody (flag_wb == 1) - see dist_to_wb for that case instead.
# It was computed for QA purposes but never actually consumed as a filter
# or a model input anywhere downstream, in either repo.
#
# This does NOT hit gridMET/LakeCat/elevation - it's a plain join against a
# CSV already sitting locally (the same one 20_site_characteristics.R reads
# for wb_nhd_id/wb_areasqkm), so it's fast and needs no network calls.
# That's why this is a separate small script rather than a full rerun of
# 20_site_characteristics.R: reuse the copied elevation/LakeCat columns
# as-is, only add the 3 new shoreline columns.
#
# Encoding: collapsed to a single 3-level integer category (no NA) rather
# than passing flag_optical_shoreline through with its NAs, since ~4% of
# sites (62/1525) are outside their matched waterbody and would otherwise
# be missing outright rather than meaningfully categorized:
#   0 = open water   (flag_wb==0, flag_optical_shoreline==0, dist_to_shore>230m)
#   1 = near-shore    (flag_wb==0, flag_optical_shoreline==1, dist_to_shore<=230m)
#   2 = outside waterbody (flag_wb==1, dist_to_shore is NA by construction)

suppressMessages({
  library(arrow); library(dplyr); library(readr)
})

aquamatch_dir <- "aquamatch_files"
out_dir <- "regional_clarity/outlier_rework_v2"
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

base <- read_feather(file.path(out_dir, "site_characteristics_base.feather"))
log("base site_characteristics rows: %s", nrow(base))

shoreline <- read_csv(file.path(aquamatch_dir, "siteSR_collated_WQP_NWIS_sites_with_NHD_info_2025-06-04.csv"),
                       show_col_types = FALSE) %>%
  select(siteSR_id, flag_wb, dist_to_shore, flag_optical_shoreline) %>%
  distinct(siteSR_id, .keep_all = TRUE) %>%
  mutate(shoreline_flag = case_when(
    flag_wb == 1 ~ 2L,
    flag_optical_shoreline == 1 ~ 1L,
    flag_optical_shoreline == 0 ~ 0L,
    TRUE ~ NA_integer_
  )) %>%
  select(siteSR_id, dist_to_shore, shoreline_flag)

log("shoreline_flag value counts:")
print(table(shoreline$shoreline_flag, useNA = "always"))

joined <- base %>% left_join(shoreline, by = "siteSR_id")
stopifnot("shoreline join dropped/missed sites" = sum(is.na(joined$shoreline_flag)) == 0)

write_feather(joined, file.path(out_dir, "site_characteristics.feather"))
log("wrote site_characteristics.feather (%s cols, %s rows)", ncol(joined), nrow(joined))
log("done")
