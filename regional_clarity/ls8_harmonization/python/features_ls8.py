"""Fork of outlier_rework/python/features.py, parameterized on which
cross-sensor reference (`corr7` or `corr8`) supplies the raw bands, so the
identical spectral-index formulas run against either reference from the
same shared base_expanded.parquet (see ls8_harmonization/r/01_build_ls8_base.R).
Every formula/citation is unchanged from the original - only the band-column
suffix is now a parameter instead of hardcoded, and the resulting index
columns keep the same generic names (BR, BG, ...) either way, since a given
pipeline run only ever uses one reference at a time.
"""
import numpy as np
import pandas as pd

BASE_BAND_NAMES = ["red", "green", "blue", "nir", "swir1", "swir2", "temp"]


def base_bands(suffix: str) -> list:
    return [f"{b}_{suffix}" for b in BASE_BAND_NAMES]


def add_spectral_indices(df: pd.DataFrame, suffix: str) -> pd.DataFrame:
    df = df.copy()
    r, g, b = df[f"red_{suffix}"], df[f"green_{suffix}"], df[f"blue_{suffix}"]
    n, s1, s2 = df[f"nir_{suffix}"], df[f"swir1_{suffix}"], df[f"swir2_{suffix}"]

    df["BR"] = b / r
    df["BG"] = b / g
    df["NR"] = n / r
    df["GR"] = g / r

    df["fai"] = n - (r + (s1 - r) * ((830 - 660) / (1650 - 660)))
    df["NDVI"] = (n - r) / (n + r)
    df["NDSSI"] = (b - n) / (b + n)
    df["NDWI"] = (g - n) / (g + n)    # McFeeters 1996
    df["MNDWI"] = (g - s1) / (g + s1)  # Xu 2006

    index_cols = ["BR", "BG", "NR", "GR", "fai", "NDVI", "NDSSI", "NDWI", "MNDWI"]
    df[index_cols] = df[index_cols].replace([np.inf, -np.inf], np.nan)

    return df


def candidate_feature_list(df: pd.DataFrame, suffix: str) -> list:
    other_suffix = "corr8" if suffix == "corr7" else "corr7"
    exclude = {"siteSR_id", "date", "HUC4", "part", "harmonized_value",
               "mission", "misc_flag", "lat", "lon", "time_diff", "sat_corr",
               "HUC8", "holdout_part", "is_holdout"}
    exclude |= {c for c in df.columns if c.endswith(f"_{other_suffix}")}
    exclude |= {c for c in df.columns if c.startswith("cvfold_seed")}
    return [c for c in df.columns if c not in exclude]


def correlation_prune(df: pd.DataFrame, feats: list, target: str = "harmonized_value",
                       corr_threshold: float = 0.95) -> list:
    """Identical logic to outlier_rework/python/features.py's correlation_prune."""
    corr_mat = df[feats].corr(method="pearson").abs()
    n = len(feats)
    parent = list(range(n))

    def find(x):
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x

    def union(x, y):
        rx, ry = find(x), find(y)
        if rx != ry:
            parent[ry] = rx

    idx = {f: i for i, f in enumerate(feats)}
    for i in range(n):
        for j in range(i + 1, n):
            if corr_mat.iloc[i, j] > corr_threshold:
                union(i, j)

    target_corr = df[feats].apply(lambda col: abs(col.corr(df[target])))

    clusters = {}
    for f in feats:
        root = find(idx[f])
        clusters.setdefault(root, []).append(f)

    kept = []
    for members in clusters.values():
        best = max(members, key=lambda f: target_corr[f] if pd.notna(target_corr[f]) else -1)
        kept.append(best)
    return kept
