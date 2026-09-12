"""Core comparison figures for the unified report: v1 frozen config vs.
the final v3 ensemble (unweighted, gap-aware throughout), and per-seed
holdout RMSE. Reads ensemble_comparison_summary.json / per-seed
final_eval_summary.json as they currently stand - rerun this after any
change to the underlying pipeline so the figures never go stale."""
import json
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np

ROOT = Path(__file__).resolve().parents[1]
FIG_DIR = ROOT / "figures"
RESULTS = ROOT / "results"
SEEDS = [501, 502, 503, 504, 505]

plt.rcParams.update({"font.family": "sans-serif", "font.size": 10})
ACCENT_STRONG = "#0D4A5E"
FLAG = "#C1442D"
GOOD = "#3E7D53"
MUTED = "#9AB0B8"

summ = json.load(open(RESULTS / "ensemble_comparison_summary.json"))
v1 = summ["v1_frozen_baseline_ensemble"]
v3 = summ["v3_ensemble"]

# ============ controlled comparison: v1 frozen vs v3 final ============
fig, axes = plt.subplots(1, 3, figsize=(9.5, 4.6))
metrics_to_plot = [("rmse", "RMSE (m)"), ("mae", "MAE (m)"), ("r2", "R²")]
labels = ["v1 features/params\n(same 5 splits)", "v3 final ensemble\n(this report's config)"]
for ax, (key, label) in zip(axes, metrics_to_plot):
    vals = [v1[key], v3[key]]
    bars = ax.bar(labels, vals, color=[MUTED, GOOD], width=0.6)
    for b, v in zip(bars, vals):
        ax.text(b.get_x() + b.get_width() / 2, v, f"{v:.3f}", ha="center", va="bottom", fontsize=9)
    ax.set_title(label, fontsize=10)
    ax.spines[["top", "right"]].set_visible(False)
    ax.tick_params(axis="x", labelsize=8.5)
fig.suptitle("Same 5 splits, same fixed holdout - only the feature set/hyperparameters differ", fontsize=10.5)
fig.tight_layout(rect=[0, 0, 1, 0.90])
fig.savefig(FIG_DIR / "v3_controlled_comparison.png", dpi=180, facecolor="white")
print("wrote v3_controlled_comparison.png")

# ============ per-seed holdout RMSE ============
fig, ax = plt.subplots(figsize=(7, 3.8))
x = np.arange(len(SEEDS))
width = 0.35
xgb_rmse, lgb_rmse = [], []
for seed in SEEDS:
    d = json.load(open(RESULTS / f"seed{seed}" / "final_eval_summary.json"))
    xgb_rmse.append(d["xgboost"]["test_metrics"]["rmse"])
    lgb_rmse.append(d["lightgbm"]["test_metrics"]["rmse"])
ax.bar(x - width / 2, xgb_rmse, width, label="xgboost", color=ACCENT_STRONG)
ax.bar(x + width / 2, lgb_rmse, width, label="lightgbm", color="#57B8D6")
ax.axhline(v3["rmse"], color=FLAG, ls="--", lw=1.5, label=f"5-seed ensemble ({v3['rmse']:.3f})")
ax.set_xticks(x); ax.set_xticklabels([str(s) for s in SEEDS])
ax.set_xlabel("ensemble-member seed"); ax.set_ylabel("test RMSE on fixed holdout (m)")
ax.legend(fontsize=8)
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout()
fig.savefig(FIG_DIR / "v3_per_seed_rmse.png", dpi=180, facecolor="white")
print("wrote v3_per_seed_rmse.png")
