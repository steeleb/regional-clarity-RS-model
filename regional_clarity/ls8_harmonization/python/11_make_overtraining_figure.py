"""Does the LS8/9-only arm's much smaller corpus (1,925 vs 7,112 rows)
overtrain relative to the two full-corpus arms, given all three arms share
the same gap-aware hyperparameter search space? Answered directly from
each seed's own final_eval_summary.json (cv_metrics = in-CV, test_metrics
= the shared holdout) - no retraining needed."""
import json
from pathlib import Path
import matplotlib.pyplot as plt
import pandas as pd

ROOT = Path("/Users/steeleb/Documents/GitHub/regional-clarity-RS-model/regional_clarity/ls8_harmonization")
FIG_DIR = ROOT / "figures"
RESULTS = ROOT / "results"
ARMS = ["ls7ref", "ls8ref", "l89only"]
ARM_LABEL = {"ls7ref": "LS7-ref\n(n=7,112)", "ls8ref": "LS8-ref\n(n=7,112)", "l89only": "LS8/9-only\n(n=1,925)"}
SEEDS = [501, 502, 503, 504, 505]

plt.rcParams.update({"font.family": "sans-serif", "font.size": 10,
                      "axes.edgecolor": "#D7E3E6", "axes.labelcolor": "#16262E",
                      "text.color": "#16262E", "xtick.color": "#55707A", "ytick.color": "#55707A"})
ACCENT = "#1B6E8C"
FLAG = "#C1442D"

rows = []
for arm in ARMS:
    for seed in SEEDS:
        f = RESULTS / arm / f"seed{seed}" / "final_eval_summary.json"
        d = json.load(open(f))
        for model in ["xgboost", "lightgbm"]:
            cv, test = d[model]["cv_metrics"], d[model]["test_metrics"]
            rows.append(dict(arm=arm, seed=seed, model=model,
                              cv_rmse=cv["rmse"], test_rmse=test["rmse"],
                              gap_pct=(test["rmse"] - cv["rmse"]) / cv["rmse"] * 100))
df = pd.DataFrame(rows)

fig, ax = plt.subplots(figsize=(7, 4.5))
means = df.groupby("arm")["gap_pct"].mean().reindex(ARMS)
for i, arm in enumerate(ARMS):
    vals = df.loc[df["arm"] == arm, "gap_pct"]
    ax.scatter([i] * len(vals), vals, color=FLAG, alpha=0.6, s=35, zorder=3,
               label="individual seed x model" if i == 0 else None)
ax.bar(range(len(ARMS)), means.values, color=ACCENT, alpha=0.35, width=0.6, zorder=1,
       label="mean across 5 seeds x 2 models")
ax.set_xticks(range(len(ARMS)))
ax.set_xticklabels([ARM_LABEL[a] for a in ARMS])
ax.set_ylabel("CV -> test RMSE gap (%)")
ax.set_title("Generalization gap by arm: LS8/9-only isn't disproportionately\nworse despite 73% less training data")
ax.legend(fontsize=8.5)
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout()
fig.savefig(FIG_DIR / "overtraining_gap_by_arm.png", dpi=180, facecolor="white")
plt.close(fig)

print(df.groupby("arm")["gap_pct"].agg(["mean", "std", "min", "max"]).round(2))
print("wrote overtraining_gap_by_arm.png")
