"""Spectral index feature engineering.

Replicates the band-ratio / normalized-difference features from
04_make_models.Rmd exactly, so the Python model comparison is working from
the same feature space as the existing R xgboost baseline.
"""
import numpy as np
import pandas as pd

BASE_BANDS = ["red_corr7", "green_corr7", "blue_corr7", "nir_corr7",
              "swir1_corr7", "swir2_corr7", "temp_corr7"]


def add_spectral_indices(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()
    r, g, b = df["red_corr7"], df["green_corr7"], df["blue_corr7"]
    n, s1, s2 = df["nir_corr7"], df["swir1_corr7"], df["swir2_corr7"]

    df["NR"] = n / r
    df["BR"] = b / r
    df["GR"] = g / r
    df["SR"] = s1 / r
    df["BG"] = b / g
    df["RG"] = r / g
    df["NG"] = n / g
    df["SG"] = s1 / g
    df["BN"] = b / n
    df["GN"] = g / n
    df["RN"] = r / n
    df["SN"] = s1 / n
    df["BS"] = b / s1
    df["GS"] = g / s1
    df["RS"] = r / s1
    df["NS"] = n / s1
    df["R_GN"] = r / (g + n)
    df["R_GB"] = r / (g + b)
    df["R_GS"] = r / (g + s1)
    df["R_BN"] = r / (b + n)
    df["R_BS"] = r / (b + s1)
    df["R_NS"] = r / (n + s1)
    # G_BR removed: it was byte-for-byte identical to G_BS (both computed
    # g/(b+s1)) - a copy-paste bug inherited from 04_make_models.Rmd. Its
    # name implied green/(blue+red), which G_RB below already covers.
    df["G_BN"] = g / (b + n)
    df["G_BS"] = g / (b + s1)
    df["G_RN"] = g / (r + n)
    df["G_RB"] = g / (r + b)
    df["G_NS"] = g / (n + s1)
    df["B_RG"] = b / (r + g)
    df["B_RS"] = b / (r + s1)
    df["B_GN"] = b / (g + n)
    df["B_GS"] = b / (g + s1)
    df["B_NS"] = b / (n + s1)
    df["N_RG"] = n / (r + g)
    df["N_RB"] = n / (r + b)
    df["N_RS"] = n / (r + s1)
    df["N_GB"] = n / (g + b)
    # N_GS fixed: was computing n/(g+n) - a mislabeled near-duplicate of GN
    # (g/n) - instead of what its name implies, n/(g+swir1). The corrected
    # formula fills a real gap (every other 2-band-sum pair among
    # {R,G,B,S} already has an N_ index; g+swir1 was the only one missing).
    df["N_GS"] = n / (g + s1)
    df["N_BS"] = n / (b + s1)
    df["GR_2"] = (r + g) / 2
    df["GN_2"] = (n + g) / 2
    df["BR_G"] = (b - r) / g
    df["NS_NR"] = (n - s1) / (r - s1)
    df["fai"] = n - (r + (s1 - r) * ((830 - 660) / (1650 - 660)))
    df["NmS"] = n - s1
    df["NmR"] = n - r
    df["NDVI"] = (n - r) / (n + r)
    df["NDWI"] = (g - s1) / (g + s1)
    df["NDSSI"] = (b - n) / (b + n)
    df["GN_GN"] = (g - n) / (g + n)

    index_cols = [c for c in df.columns if c not in BASE_BANDS and c not in
                  ("siteSR_id", "date", "HUC4", "part", "harmonized_value",
                   "mission", "misc_flag", "atm_corr_LaSRC", "lat", "lon")]
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
