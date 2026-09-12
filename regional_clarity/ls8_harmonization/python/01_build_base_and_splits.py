"""Build the ls8_harmonization base dataset: join HUC8 + shore_flag onto
r/01_build_ls8_base.R's base_expanded.parquet (already restricted to
mission %in% c("LE07","LC08","LC09"), carrying both _corr7 and _corr8 band
columns), then build a fixed universal holdout + 5 independent ensemble-
seed 4-fold CV arrangements - identical architecture to
outlier_rework_v3/python/01_build_base_and_splits.py, but generated fresh
on this corpus's own HUC8 distribution (LT04/LT05 rows are gone, so this
project's HUC8 sizes/holdout membership genuinely differ from v3's and
partition_sensitivity's - no attempt is made to force the same holdout
membership across projects with different row sets).

Spectral indices are NOT computed here (unlike v3) - this base file is
shared by two model runs that differ only in which reference (_corr7 vs
_corr8) supplies the raw bands, so index computation is deferred to
02_run_seed_pipeline.py's --reference flag.

HOLDOUT_SEED: v3 reused partition_sensitivity's seed 3 (a HUC8-random split
of the FULL 12,637-row corpus, chosen there because it left both problem
basins reasonably represented in the training pool). Dropping LT04/LT05
changes this corpus's own HUC8 unit sizes enough that seed 3 is a bad
choice here on its own merits: swept seeds 1-30 directly on this corpus and
found seed 3 puts ZERO HUC4-1407 (Lake Powell) rows in the holdout at all
(and only 7.0% of HUC4-1701) - unusable for a holdout that needs to say
anything about either problem basin. Seed 12 was the best-balanced of the
sweep: overall holdout 19.7%, HUC4 1407 19.8%, HUC4 1701 14.6% - all close
to each other, no basin over- or under-represented.
"""
import sys
from pathlib import Path

import numpy as np
import pandas as pd

DATA_PATH = Path(__file__).resolve().parents[1] / "data" / "base_expanded.parquet"
HUC_LOOKUP_PATH = (Path(__file__).resolve().parents[3] / "aquamatch_files"
                    / "siteSR_collated_WQP_NWIS_sites_with_NHD_info_2025-06-04.csv")
OUT_DIR = Path(__file__).resolve().parents[1] / "data"

HOLDOUT_SEED = 12      # best-balanced of a 1-30 sweep on this corpus, see module docstring
ENSEMBLE_SEEDS = [501, 502, 503, 504, 505]


def log(msg):
    print(f"[build] {msg}", flush=True)


def greedy_bin_pack(group_sizes: pd.Series, order: list, n_parts: int) -> dict:
    part_sizes = np.zeros(n_parts)
    assignment = {}
    for gid in order:
        smallest = int(np.argmin(part_sizes))
        assignment[gid] = smallest + 1
        part_sizes[smallest] += group_sizes[gid]
    return assignment


def main():
    df = pd.read_parquet(DATA_PATH)
    log(f"loaded base_expanded.parquet: {len(df)} rows, mission counts:\n{df['mission'].value_counts()}")

    # --- HUC8 join (same source used by v3/partition_sensitivity) ---
    huc_lookup = pd.read_csv(HUC_LOOKUP_PATH,
                              usecols=["siteSR_id", "assigned_HUC", "flag_wb", "flag_optical_shoreline"],
                              dtype={"siteSR_id": str, "assigned_HUC": str})
    huc_lookup = huc_lookup.rename(columns={"assigned_HUC": "HUC8"}).drop_duplicates(subset="siteSR_id")
    df = df.merge(huc_lookup, on="siteSR_id", how="left")
    assert df["HUC8"].notna().all(), "unresolved HUC8 for some sites"
    assert (df["HUC8"].str[:4] == df["HUC4"]).all(), "HUC8 prefix disagrees with HUC4"

    # --- shore_flag: same binary OR of the two v2/v3 QA flags ---
    assert df["flag_wb"].isin([0, 1]).all(), "flag_wb has unexpected values"
    assert df["flag_optical_shoreline"].dropna().isin([0, 1]).all(), "flag_optical_shoreline has unexpected values"
    df["shore_flag"] = ((df["flag_wb"] == 1) | (df["flag_optical_shoreline"] == 1)).astype(int)
    log(f"shore_flag value counts:\n{df['shore_flag'].value_counts()}")
    df = df.drop(columns=["flag_wb", "flag_optical_shoreline"])

    # --- fixed holdout: HUC8-random split of THIS corpus, seed 3, partition 5 ---
    huc8_sizes = df.groupby("HUC8").size()
    rng = np.random.default_rng(HOLDOUT_SEED)
    shuffled = rng.permutation(huc8_sizes.index.to_numpy()).tolist()
    holdout_assignment = greedy_bin_pack(huc8_sizes, shuffled, n_parts=5)
    df["holdout_part"] = df["HUC8"].map(holdout_assignment)

    for huc4 in ["1701", "1407"]:
        sub = df[df["HUC4"] == huc4]
        if len(sub) == 0:
            log(f"HUC4 {huc4}: no rows in this corpus")
            continue
        share = sub.groupby("holdout_part").size().max() / len(sub) * 100
        log(f"HUC4 {huc4} max-partition share under this corpus's own seed-{HOLDOUT_SEED} split: {share:.1f}% (n={len(sub)})")

    is_test = df["holdout_part"] == 5
    log(f"fixed holdout: {is_test.sum()} rows ({is_test.mean():.1%}), CV pool: {(~is_test).sum()} rows")

    cv_pool = df[~is_test].copy()
    cv_pool_huc8_sizes = cv_pool.groupby("HUC8").size()

    ensemble_cols = []
    for seed in ENSEMBLE_SEEDS:
        rng = np.random.default_rng(seed)
        shuffled = rng.permutation(cv_pool_huc8_sizes.index.to_numpy()).tolist()
        assignment = greedy_bin_pack(cv_pool_huc8_sizes, shuffled, n_parts=4)
        col = f"cvfold_seed{seed}"
        ensemble_cols.append(col)
        df[col] = df["HUC8"].map(assignment)  # NaN for holdout rows, by construction
        counts = cv_pool["HUC8"].map(assignment).value_counts().sort_index()
        log(f"seed {seed} CV-fold sizes: {counts.tolist()}")

    df["is_holdout"] = is_test
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    df.to_parquet(OUT_DIR / "base_with_splits.parquet")
    log(f"wrote {OUT_DIR / 'base_with_splits.parquet'} ({len(df)} rows, "
        f"{len(ensemble_cols)} ensemble-seed CV-fold columns + is_holdout)")


if __name__ == "__main__":
    main()
