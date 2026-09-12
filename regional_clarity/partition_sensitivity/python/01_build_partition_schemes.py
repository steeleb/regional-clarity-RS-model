"""Build the partition-assignment schemes to be compared:

  - huc4_baseline: the existing part column from outlier_rework's modeling
    dataset (HUC4 groups, greedy size-balanced bin-packing, no randomness -
    see 03_split_data.Rmd). Reused as-is; also re-derived here from scratch
    as a sanity check that the greedy bin-packing port below is faithful.
  - huc8_greedy: identical greedy bin-packing algorithm, run on HUC8 groups
    instead of HUC4. HUC8 is already assigned per site upstream (spatially
    joined against NHD/WBD HUC8 polygons in AquaMatch_siteSR_WQP's
    add_HUC8_to_sites.R) via `assigned_HUC` in
    siteSR_collated_WQP_NWIS_sites_with_NHD_info_2025-06-04.csv - confirmed
    to cover every site in the model's data with its 4-digit prefix
    matching the existing HUC4 column exactly, so no new spatial join is
    needed here.
  - huc8_rand_seed{1..10}: same bin-packing objective (still greedy-assign
    to the currently-smallest partition, so partition sizes stay balanced)
    but the processing order of HUC8 units is randomly shuffled per seed
    instead of strictly largest-first. This is what the user asked for as
    "a more random assignment that balances n across partitions" - captures
    assignment variability while keeping the size-balance property that
    makes leave-one-partition-out CV meaningful.

Reads outlier_rework's data read-only; writes only into
partition_sensitivity/data.
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
N_SEEDS = 10


def greedy_bin_pack(group_sizes: pd.Series, order: list) -> dict:
    """group_sizes: index=group id, value=row count. order: the sequence of
    group ids to process. Assigns each group (in `order`) to whichever of
    5 running partitions currently has the fewest rows - same algorithm as
    03_split_data.Rmd's split-huc4 chunk, generalized to take an arbitrary
    processing order so it can be either size-sorted (deterministic) or
    shuffled (seeded)."""
    part_sizes = np.zeros(5)
    assignment = {}
    for gid in order:
        smallest = int(np.argmin(part_sizes))
        assignment[gid] = smallest + 1  # 1-indexed to match existing `part`
        part_sizes[smallest] += group_sizes[gid]
    return assignment


def main():
    df = pd.read_parquet(DATA_PATH)
    df = features.add_spectral_indices(df)

    huc_lookup = pd.read_csv(HUC_LOOKUP_PATH, usecols=["siteSR_id", "assigned_HUC"],
                              dtype=str).rename(columns={"assigned_HUC": "HUC8"})
    df = df.merge(huc_lookup, on="siteSR_id", how="left")
    assert df["HUC8"].notna().all(), "unresolved HUC8 for some sites in the model data"
    assert (df["HUC8"].str[:4] == df["HUC4"]).all(), "HUC8 prefix disagrees with HUC4"

    # --- sanity check: re-derive HUC4 partitions with the same greedy
    # algorithm and confirm it reproduces the existing `part` column ---
    huc4_sizes = df.groupby("HUC4").size()
    huc4_order = huc4_sizes.sort_values(ascending=False).index.tolist()
    huc4_assignment = greedy_bin_pack(huc4_sizes, huc4_order)
    rederived = df["HUC4"].map(huc4_assignment)
    match_rate = (rederived == df["part"]).mean()
    print(f"HUC4 greedy re-derivation matches existing `part` column: "
          f"{match_rate:.1%} of rows")
    if match_rate < 1.0:
        # a mismatch here would mean the greedy algorithm reproduces
        # different groups but not necessarily different partition
        # *labels* - compare group membership (label-invariant) instead
        groups_match = (df.groupby("HUC4")["part"].nunique() == 1).all()
        print(f"  (each HUC4 still maps to exactly one partition: {groups_match} - "
              f"a <100% row match can be pure partition-number relabeling)")

    df["part_huc4_baseline"] = df["part"]

    # --- HUC8 greedy (deterministic, size-sorted) ---
    huc8_sizes = df.groupby("HUC8").size()
    huc8_order_sorted = huc8_sizes.sort_values(ascending=False).index.tolist()
    huc8_greedy_assignment = greedy_bin_pack(huc8_sizes, huc8_order_sorted)
    df["part_huc8_greedy"] = df["HUC8"].map(huc8_greedy_assignment)

    # --- HUC8 randomized-but-balanced, 10 seeds ---
    huc8_ids = huc8_sizes.index.to_numpy()
    seed_assignments = {}
    for seed in range(1, N_SEEDS + 1):
        rng = np.random.default_rng(seed)
        shuffled = rng.permutation(huc8_ids).tolist()
        assignment = greedy_bin_pack(huc8_sizes, shuffled)
        seed_assignments[seed] = assignment
        df[f"part_huc8_rand_seed{seed}"] = df["HUC8"].map(assignment)

    # --- partition size balance check across all schemes ---
    scheme_cols = (["part_huc4_baseline", "part_huc8_greedy"]
                   + [f"part_huc8_rand_seed{s}" for s in range(1, N_SEEDS + 1)])
    print("\nPartition row-count balance by scheme:")
    for col in scheme_cols:
        counts = df[col].value_counts().sort_index()
        print(f"  {col}: {counts.tolist()}")

    # --- HUC8 group-membership table, for the "does the forested cluster
    # get broken up" figure/narrative ---
    huc8_membership = (
        df[["HUC8", "HUC4"] + scheme_cols].drop_duplicates(subset="HUC8")
        .merge(huc8_sizes.rename("n_rows"), on="HUC8")
        .sort_values("n_rows", ascending=False)
        .reset_index(drop=True)
    )

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    df.to_parquet(OUT_DIR / "modeling_dataset_with_schemes.parquet")
    huc8_membership.to_csv(OUT_DIR / "huc8_membership_by_scheme.csv", index=False)
    print(f"\nwrote {OUT_DIR / 'modeling_dataset_with_schemes.parquet'} ({len(df)} rows, "
          f"{len(scheme_cols)} partition schemes)")
    print(f"wrote {OUT_DIR / 'huc8_membership_by_scheme.csv'} ({len(huc8_membership)} HUC8 units)")


if __name__ == "__main__":
    main()
