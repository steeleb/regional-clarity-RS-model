"""Python port of 01_make_matches.Rmd's ransac_site_multiband() - same
per-site, random-subsample RANSAC-like band-outlier filter, same
parameters (n_iter=100, n_mad=2, per-band sensor-noise floor, min_sample=8),
faithfully reproduced so it can be re-run on an arm-specific sensor subset
and reference (corr7 or corr8) instead of the original all-5-mission,
LS7-referenced fit.

A site with fewer than min_sample+1=9 observations (in THIS arm's own
sensor subset) can't be resolved at all and returns all-NaN, exactly like
the original - which downstream code must treat as "drop", not "keep",
since an unresolved fit says nothing about whether those rows are outliers.
"""
import numpy as np
import pandas as pd

BAND_COLS_BY_SUFFIX = {
    "corr7": ["red_corr7", "green_corr7", "blue_corr7", "nir_corr7", "swir1_corr7", "swir2_corr7", "temp_corr7"],
    "corr8": ["red_corr8", "green_corr8", "blue_corr8", "nir_corr8", "swir1_corr8", "swir2_corr8", "temp_corr8"],
}

# per-band sensor-noise floor, keyed generically (not per-suffix, since the
# floor is a physical sensor-noise property of the band, not the reference)
MIN_DIFF = {"red": 0.01, "green": 0.01, "blue": 0.01, "nir": 0.01, "swir1": 0.01, "swir2": 0.01, "temp": 1.0}


def _ols_fit(t, y):
    ok = np.isfinite(t) & np.isfinite(y)
    if ok.sum() < 2 or len(np.unique(t[ok])) < 2:
        return np.nan, np.nan
    tt, yy = t[ok], y[ok]
    tm, ym = tt.mean(), yy.mean()
    denom = np.sum((tt - tm) ** 2)
    if denom == 0:
        return np.nan, np.nan
    slope = np.sum((tt - tm) * (yy - ym)) / denom
    return ym - slope * tm, slope


def ransac_site_multiband(t, band_mat, min_diff, n_iter=100, n_mad=2, min_sample=8, rng=None):
    """t: (n,) array of numeric dates. band_mat: (n, nb) array. min_diff: (nb,)
    array of per-band noise floors, same column order as band_mat. Returns
    (n,) bool array (True=inlier), or all-NaN if the site can't be resolved."""
    n, nb = band_mat.shape
    if n < min_sample + 1:
        return np.full(n, np.nan)
    if rng is None:
        rng = np.random.default_rng()

    band_scale = np.empty(nb)
    for b in range(nb):
        y = band_mat[:, b]
        intercept, slope = _ols_fit(t, y)
        if np.isnan(intercept):
            band_scale[b] = np.nan
            continue
        resid = y - (intercept + slope * t)
        s = np.nanmedian(np.abs(resid - np.nanmedian(resid))) * 1.4826  # MAD, matching R's mad()
        if not np.isfinite(s) or s == 0:
            s = np.nanstd(resid)
        band_scale[b] = s

    best_inliers = np.zeros(n, dtype=bool)
    best_count = 0

    for _ in range(n_iter):
        idx = rng.choice(n, size=min_sample, replace=False)
        if len(np.unique(t[idx])) < 2:
            continue

        z = np.full((n, nb), np.nan)
        for b in range(nb):
            y = band_mat[:, b]
            if len(np.unique(y[idx])) < 2 or not np.isfinite(band_scale[b]) or band_scale[b] == 0:
                continue
            intercept, slope = _ols_fit(t[idx], y[idx])
            if np.isnan(intercept):
                continue
            resid = y - (intercept + slope * t)
            zb = np.abs(resid) / band_scale[b]
            zb[np.abs(resid) < min_diff[b]] = 0
            z[:, b] = zb

        with np.errstate(invalid="ignore"):
            combined_z = np.nanmean(z, axis=1)
        valid = ~np.isnan(combined_z)
        inliers = valid & (combined_z < n_mad)

        if inliers.sum() > best_count:
            best_count = int(inliers.sum())
            best_inliers = inliers

    if best_count == 0:
        return np.full(n, np.nan)

    # rescue: a flagged point whose every band value already falls within
    # the accepted inliers' range for that band can't be implausible
    if best_inliers.any():
        in_range = np.ones((n, nb), dtype=bool)
        for b in range(nb):
            y = band_mat[:, b]
            accepted = y[best_inliers]
            accepted = accepted[np.isfinite(accepted)]
            if len(accepted) == 0:
                continue
            lo, hi = accepted.min(), accepted.max()
            in_range[:, b] = (y >= lo) & (y <= hi)
        best_inliers = best_inliers | in_range.all(axis=1)

    return best_inliers.astype(float)


def run_filter(df: pd.DataFrame, suffix: str, seed: int = 47) -> pd.Series:
    """df must have siteSR_id, date, and the suffix's band columns. Returns
    a float Series aligned to df.index: 1.0=inlier, 0.0=outlier, NaN=site
    unresolved (too few observations in this arm's own sensor subset)."""
    band_cols = BAND_COLS_BY_SUFFIX[suffix]
    min_diff = np.array([MIN_DIFF[c.split("_")[0]] for c in band_cols])
    # days since epoch, matches R's as.numeric(Date) - computed via a
    # timestamp difference rather than raw .astype("int64") // ns-per-day,
    # since pandas' datetime64 storage unit (ns vs us) isn't guaranteed and
    # silently changes what a raw int64 cast means (this broke once already:
    # us-resolution timestamps divided by a ns-per-day constant collapsed
    # every date to nearly the same tiny integer, degenerating every fit)
    t_all = (pd.to_datetime(df["date"]) - pd.Timestamp("1970-01-01")).dt.days.to_numpy()

    result = pd.Series(np.nan, index=df.index)
    rng = np.random.default_rng(seed)
    for site_id, sub in df.groupby("siteSR_id", sort=False):
        order = sub.index.to_numpy()
        t = t_all[df.index.get_indexer(order)]
        band_mat = sub[band_cols].to_numpy()
        inliers = ransac_site_multiband(t.astype(float), band_mat, min_diff, rng=rng)
        result.loc[order] = inliers
    return result
