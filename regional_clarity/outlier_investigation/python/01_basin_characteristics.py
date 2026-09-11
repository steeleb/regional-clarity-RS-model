"""Compare partitions 1-5 (and their constituent HUC4 basins) on basin/lake
characteristics available before any model touches the data: site density,
waterbody type mix, catchment land cover, and the harmonized_value (SDD)
distribution itself. This is the direct-look-at-the-basins half of the
outlier-HUC investigation - independent of any model diagnostic.

Reads outlier_rework's modeling dataset + the raw AquaMatch site metadata
(read-only); writes only into outlier_investigation.
"""
import sys
import time
from pathlib import Path

import pandas as pd

REWORK_DIR = Path(__file__).resolve().parents[2] / "outlier_rework"
AQUAMATCH_DIR = Path(__file__).resolve().parents[3] / "aquamatch_files"
OUT_DIR = Path(__file__).resolve().parents[1] / "data"


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def fcode_to_wbtype(fcode):
    if pd.isna(fcode):
        return "unknown"
    ftype = int(fcode) // 100
    if ftype == 390:
        return "Lake/Pond"
    if ftype == 436:
        return "Reservoir"
    return f"other (fcode {int(fcode)})"


def main():
    df = pd.read_parquet(REWORK_DIR / "python" / "data" / "modeling_dataset_expanded.parquet")
    log(f"modeling dataset: {df.shape}")

    meta = pd.read_csv(
        AQUAMATCH_DIR / "siteSR_collated_WQP_NWIS_sites_with_NHD_info_2025-06-04.csv",
        low_memory=False,
    )
    meta_u = meta.drop_duplicates("siteSR_id")[
        ["siteSR_id", "wb_fcode", "wb_areasqkm", "dist_to_shore",
         "flag_optical_shoreline", "number_int_wb"]
    ]
    df = df.merge(meta_u, on="siteSR_id", how="left")
    df["wb_type"] = df["wb_fcode"].map(fcode_to_wbtype)
    log(f"waterbody type counts (rows):\n{df['wb_type'].value_counts()}")

    df.to_parquet(OUT_DIR / "modeling_dataset_with_wbtype.parquet")

    # ---- row-level table is the source of truth; build partition- and
    # HUC4-level summaries from it ----
    site_level = df.drop_duplicates("siteSR_id")

    part_summary = df.groupby("part").agg(
        n_obs=("siteSR_id", "size"),
        n_sites=("siteSR_id", "nunique"),
        n_huc4=("HUC4", "nunique"),
        sdd_median=("harmonized_value", "median"),
        sdd_mean=("harmonized_value", "mean"),
        sdd_p90=("harmonized_value", lambda s: s.quantile(0.9)),
        sdd_max=("harmonized_value", "max"),
        catchment_area_sqkm_median=("catchment_area_sqkm", "median"),
        pct_impervious_median=("pct_impervious_2006", "median"),
        pct_urban_median=("pct_urban_2006", "median"),
        pct_forest_median=("pct_forest_2006", "median"),
        pct_cropland_median=("pct_cropland_2006", "median"),
        pct_wetland_median=("pct_wetland_2006", "median"),
        elevation_m_median=("elevation_m", "median"),
    ).reset_index()
    part_summary["obs_per_site"] = part_summary["n_obs"] / part_summary["n_sites"]

    wbtype_by_part = (
        df.groupby(["part", "wb_type"]).size().unstack(fill_value=0)
    )
    wbtype_pct = wbtype_by_part.div(wbtype_by_part.sum(axis=1), axis=0) * 100

    huc4_summary = df.groupby(["part", "HUC4"]).agg(
        n_obs=("siteSR_id", "size"),
        n_sites=("siteSR_id", "nunique"),
        sdd_median=("harmonized_value", "median"),
        sdd_p90=("harmonized_value", lambda s: s.quantile(0.9)),
        catchment_area_sqkm_median=("catchment_area_sqkm", "median"),
        pct_impervious_median=("pct_impervious_2006", "median"),
        pct_forest_median=("pct_forest_2006", "median"),
        pct_wetland_median=("pct_wetland_2006", "median"),
    ).reset_index()

    part_summary.to_csv(OUT_DIR / "partition_summary.csv", index=False)
    wbtype_pct.to_csv(OUT_DIR / "partition_wbtype_pct.csv")
    huc4_summary.to_csv(OUT_DIR / "huc4_summary.csv", index=False)
    site_level.to_parquet(OUT_DIR / "site_level_with_wbtype.parquet")

    log("partition summary:\n" + part_summary.to_string())
    log("waterbody type pct by partition:\n" + wbtype_pct.to_string())
    log("done")


if __name__ == "__main__":
    main()
