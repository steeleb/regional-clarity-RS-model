import json
from pathlib import Path
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

ROOT = Path("/Users/steeleb/Documents/GitHub/regional-clarity-RS-model/regional_clarity/outlier_rework_v3")
FIG_DIR = ROOT / "figures"
RESULTS = ROOT / "results"
MUTED, ACCENT2, GOOD = "#9AB0B8", "#57B8D6", "#3E7D53"

def rmse(y, p):
    return float(np.sqrt(np.mean((y - p) ** 2)))

v1 = pd.read_parquet(RESULTS / "v1_frozen_baseline_all_predictions.parquet")
v1_ens = v1.groupby(["siteSR_id", "date"]).agg(y=("y", "first"), pred=("pred", "mean")).reset_index()
wexp = pd.read_parquet(RESULTS / "weighting_experiment_predictions.parquet")
v3_unw = wexp[~wexp["weighted"]].groupby(["siteSR_id", "date"]).agg(y=("y", "first"), pred=("pred", "mean")).reset_index()
v3_w = wexp[wexp["weighted"]].groupby(["siteSR_id", "date"]).agg(y=("y", "first"), pred=("pred", "mean")).reset_index()

cut = 4.5
groups = {"Overall": lambda d: d, "Top-quartile SDD\n(>4.5m)": lambda d: d[d.y >= cut], "Rest\n(<4.5m)": lambda d: d[d.y < cut]}
configs = {"v1 frozen\n(weighted)": v1_ens, "v3 unweighted\n(adopted)": v3_unw, "v3 weighted\n(k=2.0)": v3_w}
colors = [MUTED, GOOD, ACCENT2]

plt.rcParams.update({"font.family": "sans-serif", "font.size": 10})
fig, axes = plt.subplots(1, 3, figsize=(11, 4.6))
for ax, (gname, gfn) in zip(axes, groups.items()):
    vals = [rmse(gfn(df)["y"], gfn(df)["pred"]) for df in configs.values()]
    bars = ax.bar(list(configs.keys()), vals, color=colors, width=0.62)
    for b, v in zip(bars, vals):
        ax.text(b.get_x() + b.get_width()/2, v, f"{v:.3f}", ha="center", va="bottom", fontsize=8.5)
    ax.set_title(gname, fontsize=10)
    ax.spines[["top", "right"]].set_visible(False)
    ax.tick_params(axis="x", labelsize=8)
    if gname == "Overall":
        ax.set_ylabel("RMSE (m)")
fig.suptitle("Same 5 splits, same fixed holdout throughout", fontsize=11)
fig.tight_layout(rect=[0, 0, 1, 0.90])
fig.savefig(FIG_DIR / "v3_three_way_comparison.png", dpi=180, facecolor="white")
print("done")
