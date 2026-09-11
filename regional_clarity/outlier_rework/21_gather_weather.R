# Antecedent weather features, testing whether "previous days" summaries
# (precip, temperature, solar radiation) boost SDD model performance -
# same general idea as the NASA-NW repo's make_prev_days_NLDAS_summary()
# pattern, but using gridMET (public, no auth token needed - the NLDAS
# route needs a fresh Earthdata Login token this machine doesn't have) via
# climateR::getGridMET(), fetched per-site over that site's own observed
# date range (with a lag-window buffer) rather than the full 40-year
# record for every site, since most sites don't need it.
#
# The match window is +-5 days between the satellite image and the field
# SDD sample, and time_diff is close to a 50/50 split on which side leads
# (n=6275 image-before-field, n=6384 image-after-field). "Previous N days
# before the image date" only covers the image-to-field gap when the field
# sample came *first* - when the image comes first (time_diff > 0), that
# window never reaches forward to the field date at all, silently missing
# up to 5 days of real antecedent conditions for about half the dataset.
# Fetch range widened by +5 days on the tail end so summaries.R can anchor
# each observation's window on max(image_date, field_date) instead.

suppressMessages({
  library(arrow); library(dplyr); library(readr); library(parallel); library(climateR)
})
# mclapply (fork-based) crashes immediately on this machine - climateR's
# curl/geospatial dependencies touch Objective-C runtime state that isn't
# fork-safe on macOS ("may have been in progress in another thread when
# fork() was called ... Crashing instead"). Use PSOCK workers (genuinely
# separate processes, not forks) instead.

out_dir <- "regional_clarity/outlier_rework"
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

MAX_WINDOW_DAYS <- 30  # longest antecedent window we'll summarize
VARS <- c("pr", "tmmx", "tmmn", "srad")

## ---- per-site date ranges needed ----
final <- read_feather(file.path(out_dir, "filtered_regional_sdd_final.feather"),
                       col_select = c("siteSR_id", "lat", "lon", "date", "harmonized_local_time"))
final <- final %>% mutate(field_date = as.Date(harmonized_local_time),
                          anchor_date = pmax(date, field_date))
site_ranges <- final %>%
  group_by(siteSR_id, lat, lon) %>%
  summarize(min_date = min(date) - (MAX_WINDOW_DAYS + 5), max_date = max(anchor_date), .groups = "drop")
log("fetching gridMET for %s sites", nrow(site_ranges))

fetch_one <- function(i) {
  row <- site_ranges[i, ]
  out <- tryCatch({
    pt <- sf::st_as_sf(data.frame(lon = row$lon, lat = row$lat),
                        coords = c("lon", "lat"), crs = 4326)
    res <- getGridMET(AOI = pt, varname = VARS,
                       startDate = as.character(row$min_date),
                       endDate = as.character(row$max_date))
    res$siteSR_id <- row$siteSR_id
    res
  }, error = function(e) {
    # one retry after a short pause - transient network/server hiccups
    Sys.sleep(2)
    tryCatch({
      pt <- sf::st_as_sf(data.frame(lon = row$lon, lat = row$lat),
                          coords = c("lon", "lat"), crs = 4326)
      res <- getGridMET(AOI = pt, varname = VARS,
                         startDate = as.character(row$min_date),
                         endDate = as.character(row$max_date))
      res$siteSR_id <- row$siteSR_id
      res
    }, error = function(e2) {
      data.frame(siteSR_id = row$siteSR_id, error = conditionMessage(e2))
    })
  })
  out
}

t0 <- Sys.time()
cl <- makeCluster(8, type = "PSOCK")
clusterEvalQ(cl, { suppressMessages(library(climateR)); suppressMessages(library(sf)) })
clusterExport(cl, c("site_ranges", "VARS"))
results <- parLapply(cl, seq_len(nrow(site_ranges)), fetch_one)
stopCluster(cl)
log("fetch took %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))

ok <- Filter(function(x) !"error" %in% names(x), results)
failed <- Filter(function(x) "error" %in% names(x), results)
log("succeeded: %s sites, failed: %s sites", length(ok), length(failed))

if (length(failed) > 0) {
  failed_df <- bind_rows(failed)
  write_csv(failed_df, file.path(out_dir, "weather_fetch_failures.csv"))
  log("failed site IDs written to weather_fetch_failures.csv")
}

weather_raw <- bind_rows(ok)
log("total weather rows: %s", nrow(weather_raw))
write_feather(weather_raw, file.path(out_dir, "weather_raw.feather"))
log("done")
