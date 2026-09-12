import json
from pathlib import Path
import matplotlib.pyplot as plt
import numpy as np

ROOT = Path("/Users/steeleb/Documents/GitHub/regional-clarity-RS-model/regional_clarity/outlier_rework_v3")
FIG_DIR = ROOT / "figures"
RESULTS = ROOT / "results"
BASELINE = ROOT / "results_pre_gapaware_baseline"
SEEDS = [501, 502, 503, 504, 505]

plt.rcParams.update({"font.family": "sans-serif", "font.size": 10})
ACCENT = "#1B6E8C"
FLAG = "#C1442D"
GOOD = "#3E7D53"
MUTED = "#9AB0B8"

# ============ Figure 1: feature stability matrix (gap-aware run) ============
pruned = json.load(open(RESULTS / "seed501" / "backward_elim_summary.json"))["correlation_pruned"]
cols, col_labels, matrix = [], [], {}
for seed in SEEDS:
    d = json.load(open(RESULTS / f"seed{seed}" / "backward_elim_summary.json"))
    for m in ["xgboost", "lightgbm"]:
        key = f"{seed}_{m}"
        cols.append(key)
        col_labels.append(f"{seed}\n{m}")
        matrix[key] = set(d["per_model_final"][m])

rows_variable = [f for f in pruned if not all(f in matrix[c] for c in cols)]
rows_unanimous = [f for f in pruned if f not in rows_variable]
rows_variable.sort(key=lambda f: -sum(f in matrix[c] for c in cols))
rows = rows_unanimous + rows_variable
grid = np.array([[1 if f in matrix[c] else 0 for c in cols] for f in rows])

fig, ax = plt.subplots(figsize=(8.5, 7.5))
cmap = plt.matplotlib.colors.ListedColormap(["#F0F4F5", GOOD])
ax.imshow(grid, cmap=cmap, aspect="auto", vmin=0, vmax=1)
ax.set_xticks(range(len(cols))); ax.set_xticklabels(col_labels, fontsize=7.5)
ax.set_yticks(range(len(rows))); ax.set_yticklabels(rows, fontsize=9, family="monospace")
ax.axhline(len(rows_unanimous) - 0.5, color="#55707A", lw=1.2, ls="--")
for i in range(1, 5):
    ax.axvline(i*2 - 0.5, color="white", lw=2.5)
ax.set_xlim(-0.5, len(cols)-0.5); ax.set_ylim(len(rows)-0.5, -0.5)
for spine in ax.spines.values(): spine.set_visible(False)
ax.tick_params(length=0)
fig.text(0.99, 0.005, "dark = feature retained in that seed/model's final set (gap-aware rework)", ha="right", fontsize=8, color="#55707A")
fig.tight_layout()
fig.savefig(FIG_DIR / "v3_feature_stability_gapaware.png", dpi=180, facecolor="white")
print("wrote v3_feature_stability_gapaware.png; unanimous rows:", len(rows_unanimous), "of", len(rows))

# ============ Figure 2: 3-way ensemble comparison ============
gapaware = json.load(open(RESULTS / "ensemble_comparison_summary.json"))
pregap_weighting = json.load(open(BASELINE / "weighting_experiment_summary.json"))

v1_frozen = gapaware["v1_frozen_baseline_ensemble"]  # unchanged (same splits, v1 config, weighted - v1's true config)
v3_pass1_unweighted = pregap_weighting["unweighted"]["overall"]  # task-1 result, gap-aware NOT yet applied to fg-comparison
v3_pass2_gapaware = gapaware["v3_ensemble"]  # this run: gap-aware fg-comparison + unweighted

fig, axes = plt.subplots(1, 3, figsize=(10, 4.6))
labels = ["v1 frozen config\n(weighted, as published)", "v3 pass 1\n(vanilla fg-tuning,\nunweighted)", "v3 pass 2\n(gap-aware fg-tuning,\nunweighted)"]
metrics_to_plot = [("rmse", "RMSE (m)"), ("mae", "MAE (m)"), ("r2", "R²")]
colors = [MUTED, "#57B8D6", GOOD]
for ax, (key, label) in zip(axes, metrics_to_plot):
    vals = [v1_frozen[key], v3_pass1_unweighted[key], v3_pass2_gapaware[key]]
    bars = ax.bar(labels, vals, color=colors, width=0.6)
    for b, v in zip(bars, vals):
        ax.text(b.get_x()+b.get_width()/2, v, f"{v:.3f}", ha="center", va="bottom", fontsize=8.5)
    ax.set_title(label, fontsize=10)
    ax.spines[["top","right"]].set_visible(False)
    ax.tick_params(axis='x', labelsize=7.5)
fig.suptitle("Same 5 splits, same fixed holdout throughout", fontsize=10.5)
fig.tight_layout(rect=[0, 0, 1, 0.93])
fig.savefig(FIG_DIR / "v3_gapaware_comparison.png", dpi=180, facecolor="white")
print("wrote v3_gapaware_comparison.png")
print("v1_frozen:", v1_frozen)
print("v3_pass1_unweighted:", v3_pass1_unweighted)
print("v3_pass2_gapaware:", v3_pass2_gapaware)
