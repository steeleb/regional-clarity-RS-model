# Redesign the reflectance-band RANSAC filter (ransac_site_multiband() in
# 01_make_matches.Rmd) to be much less aggressive, while still catching real
# spectral outliers (cloud/shadow contamination, glint, sensor artifacts).
#
# Diagnosis of why the original was so aggressive despite flagging only
# ~4.3% of representative-site rows directly:
#   1. It picks the SINGLE iteration with the most inliers ("winner-take-
#      all") rather than voting across iterations like the SDD-side
#      ransac_site() does - much more sensitive to one lucky/unlucky random
#      draw, and unstable relative to the true site trend.
#   2. A flagged representative-site scene silently drops ALL sibling sites
#      sharing that (location_id, sat_id) via an NA join result treated as
#      "not inlier" - innocent duplicate sites pay for a decision made about
#      a different (if physically co-located) site.
#   3. With a 5-day match window and Landsat's sparse revisit, removing even
#      a modest fraction of candidate scenes can zero out the *only*
#      available candidate for many field samples, not just degrade match
#      quality - so row-level flagging % understates real matchup impact.
#
# Fix: switch to majority-vote scoring (continuous vote_frac per point,
# same design as ransac_site()), default NA lookups to "keep" rather than
# drop, and expose vote_frac_cutoff as a single sweepable knob so we can
# tune retention against real matchup counts without re-running the
# expensive per-site iterative fit each time.

suppressMessages({
  library(data.table)
  library(dplyr)
  library(arrow)
  library(readr)
  library(parallel)
})

aquamatch_dir <- "aquamatch_files"
out_dir <- "regional_clarity/outlier_rework"

log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n")

band_cols <- c("red_corr7", "green_corr7", "blue_corr7",
               "nir_corr7", "swir1_corr7", "swir2_corr7", "temp_corr7")

## ---- closed-form OLS (same as original) ----
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

## ---- v2: majority-vote multiband RANSAC-like filter ----
## Returns a CONTINUOUS per-point vote_frac in [0,1]: the fraction of valid
## iterations in which the point looked like an inlier relative to that
## iteration's candidate line. Thresholding (vote_frac >= cutoff) happens
## downstream so cutoff can be swept cheaply without re-running this.
ransac_site_multiband_v2 <- function(date, band_mat,
                                      n_iter = 150,
                                      n_mad_detect = 2.5,
                                      min_diff = c(red_corr7 = 0.01, green_corr7 = 0.01,
                                                   blue_corr7 = 0.01, nir_corr7 = 0.01,
                                                   swir1_corr7 = 0.01, swir2_corr7 = 0.01,
                                                   temp_corr7 = 1),
                                      min_sample = 8) {
  n <- nrow(band_mat)
  nb <- ncol(band_mat)
  if (n < min_sample + 1) return(rep(NA_real_, n))

  min_diff_b <- min_diff[colnames(band_mat)]
  t <- as.numeric(date)

  # fixed per-band residual scale from a single all-data fit, as in v1
  band_scale <- vapply(seq_len(nb), function(b) {
    y <- band_mat[, b]
    cf <- ols_fit(t, y)
    pred <- cf["intercept"] + cf["slope"] * t
    resid <- y - pred
    s <- mad(resid, na.rm = TRUE)
    if (is.na(s) || s == 0) s <- sd(resid, na.rm = TRUE)
    s
  }, numeric(1))

  inlier_votes <- rep(0L, n)
  n_valid_iter <- 0L

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
    inliers <- !is.na(combined_z) & combined_z < n_mad_detect

    inlier_votes <- inlier_votes + as.integer(inliers)
    n_valid_iter <- n_valid_iter + 1L
  }

  if (n_valid_iter == 0) return(rep(NA_real_, n))
  inlier_votes / n_valid_iter
}

## ---- run per representative site, parallelized ----
log("loading siteSR_regional_corr_diag (already-corrected representative-site pool)")
siteSR_regional_corr <- read_feather(file.path(aquamatch_dir, "siteSR_DSWE1_regional_band_ransac_diagnostics.feather"))
setDT(siteSR_regional_corr)
setorder(siteSR_regional_corr, siteSR_id, date)
log("rows: %s, sites: %s", format(nrow(siteSR_regional_corr), big.mark = ","),
    format(uniqueN(siteSR_regional_corr$siteSR_id), big.mark = ","))

site_splits <- split(siteSR_regional_corr, by = "siteSR_id", keep.by = TRUE)

log("running majority-vote RANSAC v2 across %s site-groups on %s cores",
    format(length(site_splits), big.mark = ","), max(1, detectCores() - 2))
t0 <- Sys.time()
site_splits_scored <- mclapply(
  site_splits,
  function(site_df) {
    site_df[, vote_frac := ransac_site_multiband_v2(date, as.matrix(.SD)), .SDcols = band_cols]
    site_df
  },
  mc.cores = max(1, detectCores() - 2)
)
log("scoring took %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))

siteSR_scored <- rbindlist(site_splits_scored)
rm(site_splits, site_splits_scored); gc()

write_feather(siteSR_scored, file.path(out_dir, "siteSR_regional_vote_scores.feather"))
log("wrote vote scores: %s rows, vote_frac NA (unresolved sites): %s",
    format(nrow(siteSR_scored), big.mark = ","),
    format(sum(is.na(siteSR_scored$vote_frac)), big.mark = ","))

log("vote_frac distribution:")
print(summary(siteSR_scored$vote_frac))

log("done")
