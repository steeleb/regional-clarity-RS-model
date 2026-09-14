"""Example time series showing the 5-seed x 2-model ensemble's min-max
envelope (not just the mean), for a handful of well-sampled holdout sites -
a companion to 16_ensemble_ci_crps.py's aggregate coverage figures, so the
under-coverage finding there is visible site-by-site on real data too.
Same positionally-aligned row-matrix approach as 16_ensemble_ci_crps.py
(verified stable order across all 5 seeds x 2 models, including real
duplicate site+date rows, so no groupby is used)."""
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

ROOT = Path("/Users/steeleb/Documents/GitHub/regional-clarity-RS-model/regional_clarity/outlier_rework_v3")
RESULTS = ROOT / "results"
FIG_DIR = ROOT / "figures"
SEEDS = [501, 502, 503, 504, 505]
MODELS = ["xgboost", "lightgbm"]

plt.rcParams.update({"font.family": "sans-serif", "font.size": 10,
                      "axes.edgecolor": "#D7E3E6", "axes.labelcolor": "#16262E",
                      "text.color": "#16262E", "xtick.color": "#55707A", "ytick.color": "#55707A"})
ACCENT = "#1B6E8C"
FLAG = "#C1442D"
MUTED = "#9AB0B8"


def load_gnis_lookup(path):
    sc = pd.read_feather(path)[["siteSR_id", "wb_gnis_name"]].drop_duplicates("siteSR_id")
    return {row.siteSR_id: (row.wb_gnis_name if pd.notna(row.wb_gnis_name) else "Unnamed")
            for row in sc.itertuples()}


# ---- rebuild the (n_holdout, 10) prediction matrix, keeping identity columns ----
ids = None
y = None
cols = {}
for seed in SEEDS:
    d = pd.read_parquet(RESULTS / f"seed{seed}" / "holdout_predictions.parquet")
    for model in MODELS:
        sub = d[d["model"] == model].reset_index(drop=True)
        if ids is None:
            ids = sub[["siteSR_id", "date", "HUC4"]].copy()
            y = sub["y"].values
        else:
            assert np.allclose(sub["y"].values, y), "row misalignment across seeds/models"
        cols[f"{seed}_{model}"] = sub["pred"].values

pred_matrix = np.column_stack([cols[k] for k in cols])
ids["y"] = y
ids["ens_mean"] = pred_matrix.mean(axis=1)
ids["ens_min"] = pred_matrix.min(axis=1)
ids["ens_max"] = pred_matrix.max(axis=1)
ids["date"] = pd.to_datetime(ids["date"])

gnis = load_gnis_lookup(ROOT.parent / "outlier_rework" / "site_characteristics.feather")

# ---- pick a handful of well-sampled holdout sites, same style as v3_timeseries_examples.png ----
site_counts = ids.groupby("siteSR_id").size().sort_values(ascending=False)
sites = site_counts.head(4).index.tolist()

fig, axes = plt.subplots(len(sites), 1, figsize=(9.5, 2.8 * len(sites)))
for ax, site in zip(axes, sites):
    sub = ids[ids["siteSR_id"] == site].sort_values("date")
    ax.fill_between(sub["date"], sub["ens_min"], sub["ens_max"], color=MUTED, alpha=0.5,
                     label="10-member min-max envelope")
    ax.plot(sub["date"], sub["ens_mean"], "-", color=ACCENT, lw=1.3, label="ensemble mean prediction")
    ax.plot(sub["date"], sub["y"], "o", color=FLAG, markersize=4, label="observed")
    ax.set_title(f"{gnis.get(site, 'Unnamed')} (site {site}, HUC4 {sub['HUC4'].iloc[0]}, n={len(sub)})", fontsize=9)
    ax.set_ylabel("Secchi (m)")
axes[0].legend(fontsize=8, loc="best")
fig.suptitle("Ensemble min-max envelope vs. observed, example holdout sites\n"
             "(envelope is visibly narrow relative to how often observed points fall outside it)",
             fontsize=10.5, y=1.0)
fig.tight_layout()
fig.savefig(FIG_DIR / "v3_ensemble_ci_timeseries.png", dpi=180, facecolor="white", bbox_inches="tight")
plt.close(fig)
print("wrote v3_ensemble_ci_timeseries.png; sites:", sites)
