"""Re-derive 01_make_matches.Rmd's per-site band-outlier RANSAC filter
(ransac_site_multiband, ported in ransac_multiband.py) using only each
arm's own retained missions and reference, instead of the original fit
across all 5 missions referenced to LS7.

Why this matters: the original filter's per-site line fit for reflectance
vs. time includes every mission's observations at that site. Dropping
LT04/LT05 (and, for the LS8/9-only arm, LE07 too) changes what points feed
that per-site trend - a point that looked fine against a 5-mission trend
can look like an outlier against a narrower, single-generation trend, or
vice versa. This re-runs the SAME algorithm (n_iter=100, n_mad=2, same
per-band noise floors, min_sample=8) on the dense pre-match per-site time
series (aquamatch_files/siteSR_DSWE1_regional_band_ransac_diagnostics.feather
- every scene at every representative site, not just the ones that ended
up matched to an SDD sample - this project's own matched corpus is far too
sparse per-site, median 2 rows, to refit a per-site trend against; the
diagnostics cache is what 01_make_matches.Rmd itself fits against).

Scope: this reuses the already-matched siteSR<->SDD pairs (built once,
upstream, under the ORIGINAL all-mission filter) and asks which of THOSE
already-matched rows would also survive an arm-specific refit - it does
NOT re-run WQP matching to try to rescue rows the original filter already
dropped (out of scope, would require re-pulling/re-matching from scratch).
"""
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd
import pyarrow.feather as feather

sys.path.insert(0, str(Path(__file__).resolve().parent))
from ransac_multiband import BAND_COLS_BY_SUFFIX, run_filter  # noqa: E402

ROOT = Path(__file__).resolve().parents[1]
AQUAMATCH_DIR = Path(__file__).resolve().parents[3] / "aquamatch_files"
DIAG_PATH = AQUAMATCH_DIR / "siteSR_DSWE1_regional_band_ransac_diagnostics.feather"
COEF_PATH = AQUAMATCH_DIR / "lakeSR_collated_handoffs_GEEv2025-02-12_QAv2025-06-04.csv"
DATA_PATH = ROOT / "data" / "base_with_splits.parquet"
LOCATION_ID_PATH = ROOT / "data" / "site_location_ids.feather"
RESULTS_ROOT = ROOT / "results"

ARM_CONFIG = {
    "ls7ref": dict(missions=["LE07", "LC08", "LC09"], reference="corr7"),
    "ls8ref": dict(missions=["LE07", "LC08", "LC09"], reference="corr8"),
    "l89only": dict(missions=["LC08", "LC09"], reference="corr8"),
}

RAW_BANDS = ["Blue", "Green", "Red", "Nir", "Swir1", "Swir2", "SurfaceTemp"]


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def compute_corr8(df: pd.DataFrame) -> pd.DataFrame:
    coef = pd.read_csv(COEF_PATH)
    coef = coef[(coef["correction"] == "Gardner") & (coef["dswe"] == "DSWE1") & (coef["sat_to"] == "LS8")]
    wide = {}
    for _, row in coef.iterrows():
        b = row["band"].replace("med_", "")  # CSV's band names are "med_Blue" etc, RAW_BANDS are "Blue" etc
        wide.setdefault(row["sat_corr"], {})[b] = (row["intercept"], row["B1"], row["B2"])

    sat_corr = df["mission"].map({"LT04": "LS5", "LT05": "LS5", "LE07": "LS7",
                                   "LC08": "LS8", "LC09": "LS8"})
    df = df.copy()
    df["sat_corr"] = sat_corr
    for b in RAW_BANDS:
        med = df[f"med_{b}"]
        out = pd.Series(np.nan, index=df.index)
        for corr_sensor, coefs in wide.items():
            if b not in coefs:
                continue
            intercept, b1, b2 = coefs[b]
            mask = sat_corr == corr_sensor
            out.loc[mask] = intercept + b1 * med[mask] + b2 * med[mask] ** 2
        out.loc[sat_corr == "LS8"] = med.loc[sat_corr == "LS8"]  # passthrough
        suffix = {"Blue": "blue", "Green": "green", "Red": "red", "Nir": "nir",
                  "Swir1": "swir1", "Swir2": "swir2", "SurfaceTemp": "temp"}[b]
        df[f"{suffix}_corr8"] = out
    return df


def main():
    log(f"loading dense pre-match diagnostics ({DIAG_PATH.name})...")
    cols = (["siteSR_id", "sat_id", "mission", "date", "is_band_inlier"]
            + [f"med_{b}" for b in RAW_BANDS]
            + ["red_corr7", "green_corr7", "blue_corr7", "nir_corr7", "swir1_corr7", "swir2_corr7", "temp_corr7"])
    diag = feather.read_table(DIAG_PATH, columns=cols).to_pandas()
    diag["date"] = pd.to_datetime(diag["date"])
    log(f"loaded {len(diag):,} rows, {diag['siteSR_id'].nunique():,} sites, "
        f"original inlier rate: {diag['is_band_inlier'].mean():.3f}")

    log("computing LS8-referenced bands for the whole dense record...")
    diag = compute_corr8(diag)

    # the diagnostics cache only fit ONE representative siteSR_id per
    # location_id (sites within 100m sharing a waterbody, per
    # 01_make_matches.Rmd's site-location-dedup) - every other siteSR_id at
    # that location borrowed its decision. Reproduce that mapping (built by
    # r/02_compute_location_ids.R) so this project's own modeling rows,
    # most of which are NOT representative sites themselves, can be joined
    # to the right site's refit decision via (location_id, sat_id), exactly
    # as the original does.
    loc_lookup = pd.read_feather(LOCATION_ID_PATH)[["siteSR_id", "location_id"]]
    diag = diag.merge(loc_lookup, on="siteSR_id", how="left")
    log(f"diag rows with a resolved location_id: {diag['location_id'].notna().mean():.1%}")

    modeling = pd.read_parquet(DATA_PATH)
    modeling = modeling.merge(loc_lookup, on="siteSR_id", how="left")
    log(f"modeling rows with a resolved location_id: {modeling['location_id'].notna().mean():.1%}")

    impact_rows = []
    for arm, cfg in ARM_CONFIG.items():
        t0 = time.time()
        sub = diag[diag["mission"].isin(cfg["missions"])].reset_index(drop=True)
        n_sites = sub["siteSR_id"].nunique()
        log(f"=== {arm}: refitting {len(sub):,} rows, {n_sites:,} sites, reference={cfg['reference']} ===")
        new_inlier = run_filter(sub, cfg["reference"], seed=47)
        sub = sub.assign(new_is_band_inlier=new_inlier)
        log(f"{arm}: new inlier rate (of resolvable) = "
            f"{sub['new_is_band_inlier'].mean(skipna=True):.3f}, "
            f"unresolved (NaN, <9 obs at this site under this mission subset) = "
            f"{sub['new_is_band_inlier'].isna().mean():.3f} ({time.time()-t0:.0f}s)")

        # compare to the ORIGINAL all-5-mission, LS7-ref decision on the same rows
        old_vs_new = sub[["siteSR_id", "location_id", "sat_id", "is_band_inlier", "new_is_band_inlier"]].copy()
        old_in_new_in = ((old_vs_new["is_band_inlier"] == True) & (old_vs_new["new_is_band_inlier"] == 1)).sum()
        old_in_new_out = ((old_vs_new["is_band_inlier"] == True) & (old_vs_new["new_is_band_inlier"] == 0)).sum()
        old_in_new_na = ((old_vs_new["is_band_inlier"] == True) & (old_vs_new["new_is_band_inlier"].isna())).sum()
        log(f"{arm}: of rows ORIGINALLY inlier - now inlier={old_in_new_in:,}, "
            f"now flagged outlier={old_in_new_out:,}, now unresolved={old_in_new_na:,}")

        # join the new decision onto this arm's actual modeling rows via
        # (location_id, sat_id) - NOT siteSR_id directly, since most modeling
        # rows are non-representative sites that borrow their location's
        # representative-site decision, exactly like the original
        if cfg["missions"] == ["LE07", "LC08", "LC09"]:
            arm_modeling = modeling  # ls7ref and ls8ref share the same row set
        else:
            arm_modeling = modeling[modeling["mission"].isin(cfg["missions"])]
        decision_by_location_scene = old_vs_new[["location_id", "sat_id", "new_is_band_inlier"]].drop_duplicates(
            subset=["location_id", "sat_id"])
        joined = arm_modeling.merge(decision_by_location_scene, on=["location_id", "sat_id"], how="left")
        n_total = len(joined)
        n_survive = int((joined["new_is_band_inlier"] == 1).sum())
        n_drop_outlier = int((joined["new_is_band_inlier"] == 0).sum())
        n_drop_unresolved = int(joined["new_is_band_inlier"].isna().sum())
        log(f"{arm}: of this arm's {n_total:,} MODELING rows - "
            f"survive={n_survive:,} ({n_survive/n_total:.1%}), "
            f"newly flagged outlier={n_drop_outlier:,}, "
            f"unmatched/unresolved={n_drop_unresolved:,}")

        impact_rows.append(dict(arm=arm, n_modeling_rows=n_total, n_survive=n_survive,
                                 n_drop_outlier=n_drop_outlier, n_drop_unresolved=n_drop_unresolved))

        keep_ids = joined.loc[joined["new_is_band_inlier"] == 1, ["siteSR_id", "sat_id"]]
        keep_ids.to_parquet(RESULTS_ROOT / arm / "outlier_refilter_keep_ids.parquet")

    impact_df = pd.DataFrame(impact_rows)
    impact_df.to_csv(RESULTS_ROOT / "outlier_refilter_impact.csv", index=False)
    log(f"wrote {RESULTS_ROOT / 'outlier_refilter_impact.csv'}")
    log(impact_df.to_string(index=False))


if __name__ == "__main__":
    main()
