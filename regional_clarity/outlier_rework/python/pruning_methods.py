"""Three redundancy-pruning variants, compared side by side before picking
one to carry into backward elimination:

  flat_single_pass  - the method used everywhere in this report so far:
                       |Pearson r| > threshold connected components, keep
                       the feature most correlated with target per cluster.
  flat_two_pass     - flat_single_pass applied twice. Picking one survivor
                       per cluster doesn't guarantee the *survivors*
                       aren't still correlated with each other (two
                       cluster-winners from adjacent-but-unmerged clusters
                       can still clear the threshold pairwise) - a second
                       pass catches that.
  hierarchical      - average-linkage clustering on a
                       1 - max(|Pearson r|, |Spearman rho|) distance
                       matrix, cut at the same threshold. Average linkage
                       requires *every* pair within a cluster to clear the
                       threshold, not just a chain of neighbors (the
                       "chaining" problem connected-components has), and
                       the Spearman term catches monotonic-but-nonlinear
                       redundancy (e.g. a ratio vs. its reciprocal-like
                       transform) that Pearson alone can miss.
"""
import numpy as np
import pandas as pd
from scipy.cluster.hierarchy import fcluster, linkage
from scipy.spatial.distance import squareform


def _target_corr(df: pd.DataFrame, feats: list, target: str) -> pd.Series:
    return df[feats].apply(lambda col: abs(col.corr(df[target])))


def _pick_cluster_reps(feats: list, cluster_ids, target_corr: pd.Series) -> list:
    clusters = {}
    for f, c in zip(feats, cluster_ids):
        clusters.setdefault(c, []).append(f)
    kept = []
    for members in clusters.values():
        best = max(members, key=lambda f: target_corr[f] if pd.notna(target_corr[f]) else -1)
        kept.append(best)
    return kept


def flat_single_pass(df: pd.DataFrame, feats: list, target: str, threshold: float = 0.95) -> list:
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

    for i in range(n):
        for j in range(i + 1, n):
            if corr_mat.iloc[i, j] > threshold:
                union(i, j)

    cluster_ids = [find(i) for i in range(n)]
    target_corr = _target_corr(df, feats, target)
    return _pick_cluster_reps(feats, cluster_ids, target_corr)


def flat_two_pass(df: pd.DataFrame, feats: list, target: str, threshold: float = 0.95) -> list:
    pass1 = flat_single_pass(df, feats, target, threshold)
    if len(pass1) <= 1:
        return pass1
    return flat_single_pass(df, pass1, target, threshold)


def hierarchical(df: pd.DataFrame, feats: list, target: str, threshold: float = 0.95,
                  method: str = "average") -> list:
    if len(feats) <= 1:
        return feats
    pearson = df[feats].corr(method="pearson").abs()
    spearman = df[feats].corr(method="spearman").abs()
    combined = np.maximum(pearson.values, spearman.values)
    np.fill_diagonal(combined, 1.0)
    dist = 1 - combined
    dist = (dist + dist.T) / 2  # enforce exact symmetry against float noise
    np.fill_diagonal(dist, 0.0)

    condensed = squareform(dist, checks=False)
    Z = linkage(condensed, method=method)
    cluster_ids = fcluster(Z, t=1 - threshold, criterion="distance")

    target_corr = _target_corr(df, feats, target)
    return _pick_cluster_reps(feats, cluster_ids, target_corr)


METHODS = {
    "flat_single_pass": flat_single_pass,
    "flat_two_pass": flat_two_pass,
    "hierarchical": hierarchical,
}
