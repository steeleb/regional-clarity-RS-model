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
MUTED, ACCENT = "#9AB0B8", "#1B6E8C"

gap_df = pd.read_csv(RESULTS / "weighting_experiment_fold_gaps.csv")
summ = json.load(open(RESULTS / "weighting_experiment_summary.json"))

train_rmse = gap_df.groupby("weighted")["train_rmse"].mean()
val_rmse = gap_df.groupby("weighted")["val_rmse"].mean()
holdout_rmse = {False: summ["unweighted"]["overall"]["rmse"], True: summ["weighted"]["overall"]["rmse"]}

stages = ["Train\n(within-fold)", "Validation\n(CV, held-out fold)", "Holdout\n(fixed test set)"]
unweighted_vals = [train_rmse[False], val_rmse[False], holdout_rmse[False]]
weighted_vals = [train_rmse[True], val_rmse[True], holdout_rmse[True]]

fig, ax = plt.subplots(figsize=(7.5, 4.6))
x = np.arange(len(stages))
width = 0.35
bars1 = ax.bar(x - width/2, unweighted_vals, width, label="unweighted", color=MUTED)
bars2 = ax.bar(x + width/2, weighted_vals, width, label="SDD-weighted (k=2.0)", color=ACCENT)
for bars in (bars1, bars2):
    for b in bars:
        ax.text(b.get_x()+b.get_width()/2, b.get_height(), f"{b.get_height():.3f}",
                ha="center", va="bottom", fontsize=9)
ax.set_xticks(x); ax.set_xticklabels(stages, fontsize=9.5)
ax.set_ylabel("RMSE (m)")
ax.legend(fontsize=9)
ax.spines[["top","right"]].set_visible(False)
fig.tight_layout()
fig.savefig(FIG_DIR / "v3_weighting_train_val_test.png", dpi=180, facecolor="white")
print("wrote v3_weighting_train_val_test.png")
print("unweighted:", unweighted_vals, "gap(val-train)=", val_rmse[False]-train_rmse[False], "gap(holdout-train)=", holdout_rmse[False]-train_rmse[False])
print("weighted:  ", weighted_vals, "gap(val-train)=", val_rmse[True]-train_rmse[True], "gap(holdout-train)=", holdout_rmse[True]-train_rmse[True])
