"""Spectral index feature engineering.

Optical candidates are curated to raw bands plus ratios/indices with
specific literature precedent or clear physical explainability, rather
than an exhaustive sweep of every possible band combination:

  - BR (blue/red): Kloiber et al.'s canonical Landsat Secchi-depth ratio,
    the basis of the statewide Minnesota/Wisconsin clarity mapping
    programs.
  - BG (blue/green): classic ocean-color chlorophyll-a ratio.
  - NR (nir/red): established chlorophyll-a ratio for turbid inland
    waters.
  - GR (green/red): same chlorophyll-absorption-contrast logic as BG/NR,
    adjacent-band pairing.
  - NDVI, FAI, NDSSI: standard, independently well-cited named indices.
  - NDWI, MNDWI: McFeeters (1996) and Xu (2006) respectively - note the
    swap from earlier drafts of this code, which had these two backwards
    (labeled the SWIR-based Xu formula "NDWI" and the original
    green/NIR McFeeters formula "GN_GN").

Reciprocal ratios (e.g. RG alongside GR), the 2-band-sum ratios
(red/(green+nir) and its ~20 siblings), and other ad-hoc combinations
without a specific citation are not included as candidates - they added
volume to the correlation-pruning input without a corresponding
justification for why that particular combination should matter.
"""
import numpy as np
import pandas as pd

BASE_BANDS = ["red_corr7", "green_corr7", "blue_corr7", "nir_corr7",
              "swir1_corr7", "swir2_corr7", "temp_corr7"]


def add_spectral_indices(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    r, g, b = df["red_corr7"], df["green_corr7"], df["blue_corr7"]
    n, s1, s2 = df["nir_corr7"], df["swir1_corr7"], df["swir2_corr7"]

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
    # xgboost/lightgbm handle NaN natively; a ratio landing on inf (band==0
    # denominator) is recoded to NaN so it's treated as missing rather than
    # as an extreme value, matching 04_make_models.Rmd's handling
    df[index_cols] = df[index_cols].replace([np.inf, -np.inf], np.nan)

    return df


def candidate_feature_list(df: pd.DataFrame) -> list:
    # time_diff is match-quality metadata (days between field sample and
    # satellite pass) - it doesn't exist at deployment time when scoring an
    # arbitrary satellite pass with no paired field sample, so it can't be
    # a model input despite being available in this matched training table
    exclude = {"siteSR_id", "date", "HUC4", "part", "harmonized_value",
               "mission", "misc_flag", "lat", "lon", "time_diff"}
    return [c for c in df.columns if c not in exclude]


def correlation_prune(df: pd.DataFrame, feats: list, target: str = "harmonized_value",
                       corr_threshold: float = 0.95) -> list:
    """Cluster mutually-redundant (|r| > threshold) features via connected
    components and keep the one most correlated with the target in each
    cluster - same logic as 04_make_models.Rmd's corr-pruning chunk, just
    computed with networkx-free union-find instead of igraph."""
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
