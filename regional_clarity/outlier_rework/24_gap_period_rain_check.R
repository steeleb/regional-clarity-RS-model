# Filter (not a feature): for each matched observation, was there a heavy
# single-day rain event between the image date and the field (SDD) date -
# in either order? If so, the reflectance and the target describe
# genuinely different water states, and the matchup shouldn't be trusted
# regardless of which one came first. This is a data-quality/outlier
# question, addressed upstream in the pipeline (excluded before modeling),
# not fed to any model as a predictor.
#
# Peak single-day intensity within the gap, not cumulative gap-total -
# cumulative conflates "a genuine storm" with "a longer gap had more days
# to accumulate ordinary rain." Threshold is the standard NOAA/EPA "heavy
# precipitation day" cut of 1 inch (25.4 mm) in 24 hours - a flat, externally
# grounded threshold, not a percentile of this dataset. No regional
# normalization: this dataset is overwhelmingly semi-arid Mountain West
# sites, where a 25.4mm day is already a genuinely unusual event almost
# everywhere in it.

suppressMessages({
  library(arrow); library(dplyr); library(data.table)
})

out_dir <- "regional_clarity/outlier_rework"
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

HEAVY_RAIN_MM <- 25.4  # NOAA/EPA "heavy precipitation day" threshold (1 inch / 24hr)

final <- read_feather(file.path(out_dir, "filtered_regional_sdd_final.feather"),
                       col_select = c("siteSR_id", "date", "harmonized_local_time", "HUC4"))
final <- final %>%
  mutate(field_date = as.Date(harmonized_local_time),
         gap_start = pmin(date, field_date),
         gap_end = pmax(date, field_date))

weather_raw <- read_feather(file.path(out_dir, "weather_raw.feather"),
                             col_select = c("siteSR_id", "date", "pr")) %>%
  mutate(date = as.Date(date))

# peak single-day precip strictly BETWEEN gap_start and gap_end (exclusive
# of gap_start itself, the "day of" whichever event is earlier). A
# data.table non-equi (interval) join, not a row-by-row filter - the
# latter would be an O(n_obs x n_weather_rows) scan (12,659 x 3.2M).
final_dt <- as.data.table(final)
final_dt[, obs_id := .I]
wx_dt <- as.data.table(weather_raw)
setkey(wx_dt, siteSR_id, date)

joined <- wx_dt[final_dt, on = .(siteSR_id, date > gap_start, date <= gap_end),
                nomatch = NA, allow.cartesian = TRUE]
gap_max <- joined[, .(gap_max_precip_mm = max(pr, na.rm = TRUE)), by = obs_id]
gap_max[!is.finite(gap_max_precip_mm), gap_max_precip_mm := 0]  # gaps with no weather rows -> max() of nothing

gap_precip <- final_dt %>%
  left_join(as.data.frame(gap_max), by = "obs_id") %>%
  mutate(gap_max_precip_mm = coalesce(gap_max_precip_mm, 0),
         gap_days = as.numeric(gap_end - gap_start))

log("gap_max_precip_mm summary (all %s observations):", nrow(gap_precip))
print(summary(gap_precip$gap_max_precip_mm))

nonzero_gap <- gap_precip %>% filter(gap_days > 0)
log("among the %s observations with a nonzero gap:", nrow(nonzero_gap))
print(summary(nonzero_gap$gap_max_precip_mm))

n_flag <- sum(gap_precip$gap_max_precip_mm >= HEAVY_RAIN_MM)
log("flagged at %.1f mm (1 inch/24hr, NOAA/EPA heavy-precip-day threshold): %s observations (%.2f%%)",
    HEAVY_RAIN_MM, n_flag, 100 * n_flag / nrow(gap_precip))

log("flagged-rate by HUC4 (sanity check - should be low everywhere, not concentrated in one basin):")
by_huc <- gap_precip %>%
  summarize(n = n(), n_flagged = sum(gap_max_precip_mm >= HEAVY_RAIN_MM), .by = HUC4) %>%
  mutate(pct_flagged = round(100 * n_flagged / n, 2)) %>%
  arrange(desc(pct_flagged)) %>%
  as.data.frame()
print(by_huc)

write_feather(gap_precip %>% select(siteSR_id, date, field_date, gap_days, gap_max_precip_mm),
              file.path(out_dir, "gap_period_precip.feather"))
log("wrote gap_period_precip.feather")
log("done")
