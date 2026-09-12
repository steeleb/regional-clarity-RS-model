import json
from pathlib import Path
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

ROOT = Path("/Users/steeleb/Documents/GitHub/regional-clarity-RS-model/regional_clarity/outlier_rework_v3")
FIG_DIR = ROOT / "figures"
RESULTS = ROOT / "results"

plt.rcParams.update({"font.family": "sans-serif", "font.size": 10,
                      "axes.edgecolor": "#D7E3E6", "axes.labelcolor": "#16262E",
                      "text.color": "#16262E", "xtick.color": "#55707A", "ytick.color": "#55707A"})
ACCENT = "#1B6E8C"
ACCENT_STRONG = "#0D4A5E"
FLAG = "#C1442D"
GOOD = "#3E7D53"
MUTED = "#9AB0B8"

v3 = pd.read_parquet(RESULTS / "v3_ensemble_all_predictions.parquet")
v1 = pd.read_parquet(RESULTS / "v1_frozen_baseline_all_predictions.parquet")

v3_ens = v3.groupby(["siteSR_id", "date"]).agg(y=("y", "first"), pred=("pred", "mean"), HUC4=("HUC4", "first")).reset_index()
v1_ens = v1.groupby(["siteSR_id", "date"]).agg(y=("y", "first"), pred=("pred", "mean"), HUC4=("HUC4", "first")).reset_index()

# ============ Figure 1: predicted vs observed scatter (v3 ensemble) ============
fig, ax = plt.subplots(figsize=(6.2, 6.2))
ax.scatter(v3_ens["y"], v3_ens["pred"], s=10, alpha=0.35, color=ACCENT, edgecolors="none")
lims = [0, max(v3_ens["y"].max(), v3_ens["pred"].max()) * 1.02]
ax.plot(lims, lims, color=FLAG, lw=1.5, ls="--", label="1:1 line")
ax.set_xlim(lims); ax.set_ylim(lims)
ax.set_xlabel("observed SDD (m)"); ax.set_ylabel("predicted SDD (m)")
ax.set_aspect("equal")
ax.legend(fontsize=9, loc="upper left")
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout()
fig.savefig(FIG_DIR / "v3_pred_vs_obs.png", dpi=180, facecolor="white")
print("wrote v3_pred_vs_obs.png")

# ============ Figure 2: bias by observed-SDD bin, v3 vs v1-frozen ============
bins = [0, 2, 4, 6, 10, 20]
bin_labels = ["0-2", "2-4", "4-6", "6-10", "10-20"]

def bias_by_bin(df):
    d = df.copy()
    d["bin"] = pd.cut(d["y"], bins=bins, labels=bin_labels)
    return d.groupby("bin", observed=True).apply(lambda g: (g["pred"] - g["y"]).mean())

v3_bias = bias_by_bin(v3_ens)
v1_bias = bias_by_bin(v1_ens)

fig, ax = plt.subplots(figsize=(7.5, 4.2))
x = np.arange(len(bin_labels))
width = 0.35
ax.bar(x - width/2, v1_bias.reindex(bin_labels), width, label="v1 features/params (same splits)", color=MUTED)
ax.bar(x + width/2, v3_bias.reindex(bin_labels), width, label="v3 ensemble (new features/params)", color=GOOD)
ax.axhline(0, color="#16262E", lw=0.8)
ax.set_xticks(x); ax.set_xticklabels([f"{b} m" for b in bin_labels])
ax.set_xlabel("observed SDD bin"); ax.set_ylabel("mean error (pred − obs, m)")
ax.legend(fontsize=8.5)
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout()
fig.savefig(FIG_DIR / "v3_compression_bias.png", dpi=180, facecolor="white")
print("wrote v3_compression_bias.png")
print("v1 bias by bin:\n", v1_bias)
print("v3 bias by bin:\n", v3_bias)

# ============ Figure 3: per-HUC4 RMSE, problem basins highlighted ============
def rmse(y, pred):
    return float(np.sqrt(np.mean((y - pred) ** 2)))

def huc4_rmse(df, min_n=15):
    rows = []
    for huc4, g in df.groupby("HUC4"):
        if len(g) >= min_n:
            rows.append({"HUC4": huc4, "n": len(g), "rmse": rmse(g["y"], g["pred"])})
    return pd.DataFrame(rows).sort_values("n", ascending=False)

v3_huc4 = huc4_rmse(v3_ens).set_index("HUC4")
v1_huc4 = huc4_rmse(v1_ens).set_index("HUC4")
common = v3_huc4.index.intersection(v1_huc4.index)
# order by holdout row count, largest first
order = v3_huc4.loc[common].sort_values("n", ascending=False).index.tolist()

fig, ax = plt.subplots(figsize=(8.5, 4.5))
x = np.arange(len(order))
width = 0.35
bars1 = ax.bar(x - width/2, v1_huc4.loc[order, "rmse"], width, label="v1 features/params (same splits)", color=MUTED)
bars2 = ax.bar(x + width/2, v3_huc4.loc[order, "rmse"], width, label="v3 ensemble (new features/params)", color=GOOD)
labels = [f"{h}\n(n={v3_huc4.loc[h,'n']})" + ("  ★" if h in ("1701", "1407") else "") for h in order]
ax.set_xticks(x); ax.set_xticklabels(labels, fontsize=8)
for h in order:
    if h in ("1701", "1407"):
        i = order.index(h)
        ax.get_xticklabels()[i].set_color(FLAG)
        ax.get_xticklabels()[i].set_fontweight("bold")
ax.set_ylabel("holdout RMSE (m)")
ax.legend(fontsize=8.5)
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout()
fig.savefig(FIG_DIR / "v3_huc4_rmse.png", dpi=180, facecolor="white")
print("wrote v3_huc4_rmse.png")
print(pd.DataFrame({"v1_rmse": v1_huc4.loc[order, "rmse"], "v3_rmse": v3_huc4.loc[order, "rmse"], "n": v3_huc4.loc[order, "n"]}))
