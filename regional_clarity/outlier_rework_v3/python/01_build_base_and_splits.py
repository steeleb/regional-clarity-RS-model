"""Build the v3 base dataset: fixed universal holdout test set + shore_flag
feature + per-ensemble-seed 4-fold CV arrangements.

Design (see partition_sensitivity/report.html and this project's own
upstream-decision spot check for the motivating evidence):

  - FIXED HOLDOUT: HUC8-random seed 3's partition 5, from the earlier
    partition_sensitivity sweep, is reused as the one permanent test set
    for the entire v3 project - identical across every ensemble member,
    so no ensemble member ever trains on it. Seed 3 was chosen because it
    already left a reasonable share of both problem basins in the
    training pool (HUC4 1701 at 37% holdout concentration, HUC4 1407 at
    29%) rather than either extreme, so the holdout isn't itself
    accidentally starved of - or dominated by - the hard basins. The
    greedy bin-packing algorithm is re-run here from scratch (not read
    from the partition_sensitivity output file) so v3 has no file-path
    dependency on that project - it's deterministic given the same code
    and seed, and the resulting concentration numbers are checked against
    the already-published ones as a sanity check.

  - ENSEMBLE MEMBERS: for the remaining ~80% of rows (everything not in
    the fixed holdout), 5 independent random-but-balanced 4-fold CV
    arrangements are built by re-shuffling HOW those HUC8 units are
    distributed into folds 1-4 (never touching the holdout). Each
    ensemble seed gets its own full feature-selection + tuning pipeline
    (run separately, see 02_run_seed_pipeline.py); the final production
    prediction on the fixed holdout will be the average across all
    ensemble members' models.

  - SHORE_FLAG: new candidate feature, 0/1. 1 if flag_wb==1 (site's
    nearest-matched-waterbody join failed / site sits outside the
    matched waterbody polygon) OR flag_optical_shoreline==1 (site is
    within 230m of the matched waterbody's shoreline - the 200m site
    buffer + 30m optical pixel). 0 only when neither condition holds
    (open water, comfortably inside the waterbody). This collapses
    outlier_rework_v2's 3-level shoreline_flag (0=open water,
    1=near-shore, 2=outside waterbody) into a strict binary at the
    user's request.
"""
import sys
from pathlib import Path

import numpy as np
import pandas as pd

REWORK_DIR = Path(__file__).resolve().parents[2] / "outlier_rework" / "python"
sys.path.insert(0, str(REWORK_DIR))
import features  # noqa: E402

DATA_PATH = REWORK_DIR / "data" / "modeling_dataset_expanded.parquet"
HUC_LOOKUP_PATH = (Path(__file__).resolve().parents[3] / "aquamatch_files"
                    / "siteSR_collated_WQP_NWIS_sites_with_NHD_info_2025-06-04.csv")
OUT_DIR = Path(__file__).resolve().parents[1] / "data"

HOLDOUT_SEED = 3       # reused from partition_sensitivity, defines the fixed test set
ENSEMBLE_SEEDS = [501, 502, 503, 504, 505]  # distinct namespace from partition_sensitivity's 1-10


def log(msg):
    print(f"[build] {msg}", flush=True)


def greedy_bin_pack(group_sizes: pd.Series, order: list, n_parts: int) -> dict:
    """Identical algorithm to partition_sensitivity's greedy_bin_pack,
    generalized to n_parts (5 for the holdout-defining split, 4 for the
    ensemble-member CV-only splits)."""
    part_sizes = np.zeros(n_parts)
    assignment = {}
    for gid in order:
        smallest = int(np.argmin(part_sizes))
        assignment[gid] = smallest + 1
        part_sizes[smallest] += group_sizes[gid]
    return assignment


def main():
    df = pd.read_parquet(DATA_PATH)
    df = features.add_spectral_indices(df)

    # --- HUC8 join (already assigned upstream, no new spatial join needed -
    # same source used in partition_sensitivity) ---
    huc_lookup = pd.read_csv(HUC_LOOKUP_PATH,
                              usecols=["siteSR_id", "assigned_HUC", "flag_wb", "flag_optical_shoreline"],
                              dtype={"siteSR_id": str, "assigned_HUC": str})
    huc_lookup = huc_lookup.rename(columns={"assigned_HUC": "HUC8"}).drop_duplicates(subset="siteSR_id")
    df = df.merge(huc_lookup, on="siteSR_id", how="left")
    assert df["HUC8"].notna().all(), "unresolved HUC8 for some sites"
    assert (df["HUC8"].str[:4] == df["HUC4"]).all(), "HUC8 prefix disagrees with HUC4"

    # --- shore_flag: binary OR of the two v2 QA flags ---
    assert df["flag_wb"].isin([0, 1]).all(), "flag_wb has unexpected values"
    assert df["flag_optical_shoreline"].dropna().isin([0, 1]).all(), "flag_optical_shoreline has unexpected values"
    df["shore_flag"] = ((df["flag_wb"] == 1) | (df["flag_optical_shoreline"] == 1)).astype(int)
    log(f"shore_flag value counts:\n{df['shore_flag'].value_counts()}")
    df = df.drop(columns=["flag_wb", "flag_optical_shoreline"])

    # --- fixed holdout: re-derive HUC8-random seed 3's 5-way split from
    # scratch, take its partition 5 as the permanent test set ---
    huc8_sizes = df.groupby("HUC8").size()
    rng = np.random.default_rng(HOLDOUT_SEED)
    shuffled = rng.permutation(huc8_sizes.index.to_numpy()).tolist()
    holdout_assignment = greedy_bin_pack(huc8_sizes, shuffled, n_parts=5)
    df["holdout_part"] = df["HUC8"].map(holdout_assignment)

    # sanity check against the already-published partition_sensitivity numbers
    for huc4, expected_share in [("1701", 43.9), ("1407", 47.0)]:
        sub = df[df["HUC4"] == huc4]
        share = sub.groupby("holdout_part").size().max() / len(sub) * 100
        log(f"sanity check HUC4 {huc4} max-partition share: {share:.1f}% "
            f"(partition_sensitivity reported {expected_share}%)")

    is_test = df["holdout_part"] == 5
    log(f"fixed holdout: {is_test.sum()} rows ({is_test.mean():.1%}), "
        f"CV pool: {(~is_test).sum()} rows")

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
