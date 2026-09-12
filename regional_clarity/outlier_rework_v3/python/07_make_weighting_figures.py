import json
from pathlib import Path
import matplotlib.pyplot as plt
import numpy as np

ROOT = Path("/Users/steeleb/Documents/GitHub/regional-clarity-RS-model/regional_clarity/outlier_rework_v3")
FIG_DIR = ROOT / "figures"
RESULTS = ROOT / "results"

plt.rcParams.update({"font.family": "sans-serif", "font.size": 10,
                      "axes.edgecolor": "#D7E3E6", "axes.labelcolor": "#16262E",
                      "text.color": "#16262E", "xtick.color": "#55707A", "ytick.color": "#55707A"})
ACCENT = "#1B6E8C"
FLAG = "#C1442D"
GOOD = "#3E7D53"
MUTED = "#9AB0B8"

d = json.load(open(RESULTS / "weighting_experiment_summary.json"))

# ============ Figure: RMSE by group, unweighted vs weighted ============
groups = ["overall", "top_quartile", "rest"]
group_labels = ["Overall\n(n=2,394)", "Top-quartile SDD\n(n=673, >4.5m)", "Rest\n(n=1,721)"]
unweighted_rmse = [d["unweighted"][g]["rmse"] for g in groups]
weighted_rmse = [d["weighted"][g]["rmse"] for g in groups]

fig, ax = plt.subplots(figsize=(7.5, 4.3))
x = np.arange(len(groups))
width = 0.35
bars1 = ax.bar(x - width/2, unweighted_rmse, width, label="unweighted", color=MUTED)
bars2 = ax.bar(x + width/2, weighted_rmse, width, label="SDD-weighted (k=2.0)", color=ACCENT)
for bars in (bars1, bars2):
    for b in bars:
        ax.text(b.get_x()+b.get_width()/2, b.get_height(), f"{b.get_height():.3f}",
                ha="center", va="bottom", fontsize=8.5)
ax.set_xticks(x); ax.set_xticklabels(group_labels, fontsize=9)
ax.set_ylabel("holdout RMSE (m)")
ax.legend(fontsize=9)
ax.spines[["top","right"]].set_visible(False)
fig.tight_layout()
fig.savefig(FIG_DIR / "v3_weighting_rmse.png", dpi=180, facecolor="white")
print("wrote v3_weighting_rmse.png")

# ============ Figure: bias by group ============
unweighted_bias = [d["unweighted"][g]["bias"] for g in groups]
weighted_bias = [d["weighted"][g]["bias"] for g in groups]

fig, ax = plt.subplots(figsize=(7.5, 4.3))
bars1 = ax.bar(x - width/2, unweighted_bias, width, label="unweighted", color=MUTED)
bars2 = ax.bar(x + width/2, weighted_bias, width, label="SDD-weighted (k=2.0)", color=ACCENT)
ax.axhline(0, color="#16262E", lw=0.8)
for bars in (bars1, bars2):
    for b in bars:
        ax.text(b.get_x()+b.get_width()/2, b.get_height(), f"{b.get_height():+.2f}",
                ha="center", va="bottom" if b.get_height() >= 0 else "top", fontsize=8.5)
ax.set_xticks(x); ax.set_xticklabels(group_labels, fontsize=9)
ax.set_ylabel("mean bias, pred − obs (m)")
ax.legend(fontsize=9)
ax.spines[["top","right"]].set_visible(False)
fig.tight_layout()
fig.savefig(FIG_DIR / "v3_weighting_bias.png", dpi=180, facecolor="white")
print("wrote v3_weighting_bias.png")
