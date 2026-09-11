# Apply the heavy-single-day-rain-in-the-gap filter (see
# 24_gap_period_rain_check.R) to the final training set. This is an
# outlier-removal step - it lives upstream of everything else in the
# pipeline (sensor correction, HUC4 split, site/weather feature export,
# modeling), so re-running from here cascades through all of it.

suppressMessages({
  library(arrow); library(dplyr)
})

out_dir <- "regional_clarity/outlier_rework"
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

HEAVY_RAIN_MM <- 25.4

final <- read_feather(file.path(out_dir, "filtered_regional_sdd_final.feather")) %>%
  mutate(field_date = as.Date(harmonized_local_time))
gap <- read_feather(file.path(out_dir, "gap_period_precip.feather"))

# (siteSR_id, date, field_date) can repeat: two different field-sampling
# events (different subgroup_id, e.g. duplicate WQP submissions of the
# same visit) can both match to the same closest satellite image with the
# same field_date. gap_max_precip_mm depends only on (siteSR_id, gap_start,
# gap_end), which is identical for such rows either way, so de-duplicating
# is safe (verified: duplicated keys always carry the same
# gap_max_precip_mm, never conflicting values).
gap_dup_check <- gap %>% distinct(siteSR_id, date, field_date, gap_max_precip_mm)
stopifnot("duplicate (siteSR_id, date, field_date) keys carry CONFLICTING gap_max_precip_mm values" =
            !any(duplicated(gap_dup_check[, c("siteSR_id", "date", "field_date")])))
gap_unique <- gap %>% distinct(siteSR_id, date, field_date, .keep_all = TRUE)

final_flagged <- final %>%
  left_join(gap_unique %>% select(siteSR_id, date, field_date, gap_max_precip_mm),
            by = c("siteSR_id", "date", "field_date"))
stopifnot("join dropped rows or introduced NAs in gap_max_precip_mm" =
            nrow(final_flagged) == nrow(final) && !anyNA(final_flagged$gap_max_precip_mm))

n_before <- nrow(final_flagged)
final_clean <- final_flagged %>% filter(gap_max_precip_mm < HEAVY_RAIN_MM) %>%
  select(-gap_max_precip_mm, -field_date)
n_after <- nrow(final_clean)

log("gap-period heavy-rain filter: %s -> %s rows (dropped %s, %.2f%%)",
    n_before, n_after, n_before - n_after, 100 * (n_before - n_after) / n_before)
stopifnot("final training rows fell below the 10k floor" = n_after >= 10000)

# keep the pre-filter version for transparency/reproducibility, then
# overwrite the canonical file so every downstream script picks this up
# without further edits
if (!file.exists(file.path(out_dir, "filtered_regional_sdd_final_pre_rain_filter.feather"))) {
  write_feather(final, file.path(out_dir, "filtered_regional_sdd_final_pre_rain_filter.feather"))
}
write_feather(final_clean, file.path(out_dir, "filtered_regional_sdd_final.feather"))
log("wrote filtered_regional_sdd_final.feather (%s rows)", n_after)
log("done")
