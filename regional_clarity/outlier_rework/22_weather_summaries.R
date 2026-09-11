# "Previous N days" weather summaries - same pattern as the NASA-NW repo's
# make_prev_days_NLDAS_summary(): total precip, max/mean/min temp, total
# solar radiation, computed per site over rolling windows ending the day
# BEFORE each satellite/field observation (data.table::froll* for
# efficiency, matching that repo's implementation).

suppressMessages({
  library(arrow); library(dplyr); library(data.table)
})

out_dir <- "regional_clarity/outlier_rework"
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

WINDOWS <- c(1, 3, 7, 30)

weather <- read_feather(file.path(out_dir, "weather_raw.feather"))
log("loaded %s weather rows across %s sites", nrow(weather), n_distinct(weather$siteSR_id))

# units: gridMET pr is mm, tmmx/tmmn are Kelvin, srad is W/m^2
weather <- weather %>%
  mutate(date = as.Date(date),
         precip_mm = pr,
         tmax_degC = tmmx - 273.15,
         tmin_degC = tmmn - 273.15,
         tmean_degC = (tmax_degC + tmin_degC) / 2,
         srad_Wm2 = srad) %>%
  select(siteSR_id, date, precip_mm, tmax_degC, tmin_degC, tmean_degC, srad_Wm2)

setDT(weather)
setorder(weather, siteSR_id, date)

# make sure each site has one row per calendar day (frollsum/frollmean
# assume a regular series - gridMET shouldn't have gaps, but guard anyway)
site_splits <- split(weather, by = "siteSR_id", keep.by = TRUE)

make_summary_one_site <- function(df) {
  site_id <- df$siteSR_id[1]
  full_days <- data.table(date = seq(min(df$date), max(df$date), by = "1 day"))
  df <- merge(full_days, df[, -"siteSR_id"], by = "date", all.x = TRUE)
  df[, siteSR_id := site_id]

  for (w in WINDOWS) {
    df[[paste0("precip_mm_prev", w)]] <- frollsum(df$precip_mm, n = w)
    df[[paste0("tmax_degC_prev", w)]] <- frollapply(df$tmax_degC, N = w, FUN = max)
    df[[paste0("tmean_degC_prev", w)]] <- frollmean(df$tmean_degC, n = w)
    df[[paste0("tmin_degC_prev", w)]] <- frollapply(df$tmin_degC, N = w, FUN = min)
    df[[paste0("srad_Wm2_prev", w)]] <- frollmean(df$srad_Wm2, n = w)
  }
  # shift so window w on `date` covers the w days strictly BEFORE date,
  # matching NASA-NW's "date = date + days(1)" convention
  df[, date := date + 1]
  df
}

log("computing rolling summaries across %s sites", length(site_splits))
t0 <- Sys.time()
summaries <- rbindlist(lapply(site_splits, make_summary_one_site), fill = TRUE)
log("took %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))

summaries <- summaries[!is.na(siteSR_id)]
write_feather(as.data.frame(summaries), file.path(out_dir, "weather_summaries.feather"))
log("wrote weather_summaries.feather: %s rows, %s cols", nrow(summaries), ncol(summaries))
log("done")
