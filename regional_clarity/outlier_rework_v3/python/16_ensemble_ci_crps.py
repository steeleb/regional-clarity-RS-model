"""Exploratory: what does the v3 ensemble's own 5-seed x 2-model spread
look like as a confidence interval, and does treating the 10 members as
an empirical distribution (via CRPS) say anything RMSE/MAE don't already?

This is NOT a calibrated interval - the 10 members share the same base
data, feature engineering, and modeling approach, differing only in CV-
fold assignment and (xgboost vs lightgbm) architecture, so this spread is
best read as "how much does the *already-adopted* ensembling procedure
itself disagree," not a rigorous predictive uncertainty estimate. Framed
in the report as an honest exploratory result, not a settled
recommendation, per direct instruction.

Row alignment: each seed's holdout_predictions.parquet has 3042 rows x 2
models (6084 total) in a stable, positionally-identical row order across
every seed/model (verified directly - not assumed), including real
duplicate site+date rows (multiple same-day observations), so a
(siteSR_id, date) groupby would incorrectly collapse them. Positional
alignment is used instead.
"""
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
GOOD = "#3E7D53"
MUTED = "#9AB0B8"

# ---- build a (n_holdout, 10) prediction matrix, positionally aligned ----
y = None
huc4 = None
cols = {}
for seed in SEEDS:
    d = pd.read_parquet(RESULTS / f"seed{seed}" / "holdout_predictions.parquet")
    for model in MODELS:
        sub = d[d["model"] == model].reset_index(drop=True)
        if y is None:
            y = sub["y"].values
            huc4 = sub["HUC4"].values
        else:
            assert np.allclose(sub["y"].values, y), "row misalignment across seeds/models"
        cols[f"{seed}_{model}"] = sub["pred"].values

pred_matrix = np.column_stack([cols[k] for k in cols])  # (n, 10)
n, m = pred_matrix.shape
ens_mean = pred_matrix.mean(axis=1)
ens_std = pred_matrix.std(axis=1)
ens_min = pred_matrix.min(axis=1)
ens_max = pred_matrix.max(axis=1)

# ---- coverage: does y fall inside various bands? ----
in_minmax = (y >= ens_min) & (y <= ens_max)
in_1sd = (y >= ens_mean - ens_std) & (y <= ens_mean + ens_std)
in_2sd = (y >= ens_mean - 2 * ens_std) & (y <= ens_mean + 2 * ens_std)

print(f"n holdout rows: {n}, ensemble members per row: {m}")
print(f"coverage, min-max envelope (10-member range): {in_minmax.mean()*100:.1f}%")
print(f"coverage, mean +/- 1 SD:  {in_1sd.mean()*100:.1f}%  (~68% expected if Gaussian & calibrated)")
print(f"coverage, mean +/- 2 SD:  {in_2sd.mean()*100:.1f}%  (~95% expected if Gaussian & calibrated)")
print(f"mean interval half-width (1 SD): {ens_std.mean():.3f} m")
print(f"mean min-max envelope width: {(ens_max - ens_min).mean():.3f} m")

# coverage by SDD tercile and by Lake Powell vs rest
tercile = pd.qcut(y, 3, labels=["low SDD", "mid SDD", "high SDD"])
cov_df = pd.DataFrame({"tercile": tercile, "in_1sd": in_1sd, "in_2sd": in_2sd, "huc4": huc4})
print("\ncoverage by SDD tercile:")
print(cov_df.groupby("tercile", observed=True)[["in_1sd", "in_2sd"]].mean().round(3) * 100)
print("\ncoverage, Lake Powell (1407) vs rest:")
cov_df["is_powell"] = cov_df["huc4"] == "1407"
print(cov_df.groupby("is_powell")[["in_1sd", "in_2sd"]].mean().round(3) * 100)

# ---- CRPS (empirical, ensemble-as-distribution) vs RMSE/MAE ----
term1 = np.mean(np.abs(pred_matrix - y[:, None]), axis=1)
diffs = np.abs(pred_matrix[:, :, None] - pred_matrix[:, None, :])
term2 = diffs.mean(axis=(1, 2)) / 2
crps = term1 - term2
rmse = np.sqrt(np.mean((ens_mean - y) ** 2))
mae = np.mean(np.abs(ens_mean - y))
print(f"\nmean CRPS (10-member empirical): {crps.mean():.3f} m")
print(f"ensemble-mean RMSE: {rmse:.3f} m, MAE: {mae:.3f} m")
print(f"CRPS as fraction of MAE: {crps.mean()/mae*100:.1f}%")

pd.DataFrame({"y": y, "huc4": huc4, "ens_mean": ens_mean, "ens_std": ens_std,
              "ens_min": ens_min, "ens_max": ens_max, "crps": crps}).to_parquet(
    RESULTS / "ensemble_ci_crps.parquet")

# ---- figure 1: interval vs observed, sorted subset ----
rng = np.random.default_rng(7)
order = np.argsort(y)
subset_idx = order[np.linspace(0, n - 1, 80).astype(int)]
sub_y = y[subset_idx]
sub_mean = ens_mean[subset_idx]
sub_min = ens_min[subset_idx]
sub_max = ens_max[subset_idx]
x_pos = np.arange(len(subset_idx))

fig, ax = plt.subplots(figsize=(11, 5.5))
ax.vlines(x_pos, sub_min, sub_max, color=MUTED, lw=1.5, alpha=0.8, label="10-member min-max envelope")
ax.scatter(x_pos, sub_mean, color=ACCENT, s=18, zorder=3, label="ensemble mean prediction")
ax.scatter(x_pos, sub_y, color=FLAG, s=18, zorder=3, marker="x", label="observed")
ax.set_xlabel("holdout rows, sorted by observed SDD (80-row evenly-spaced subset)")
ax.set_ylabel("SDD (m)")
ax.set_title("v3 ensemble spread as an interval: envelope widens with SDD magnitude,\nbut observed values still fall outside it often at the high-SDD end")
ax.legend(fontsize=9, loc="upper left")
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout()
fig.savefig(FIG_DIR / "v3_ensemble_ci.png", dpi=180, facecolor="white")
plt.close(fig)
print("\nwrote v3_ensemble_ci.png")

# ---- figure 2: coverage bar chart, min-max / 1sd / 2sd, overall + by tercile ----
fig, ax = plt.subplots(figsize=(7.5, 4.5))
labels = ["Min-max\nenvelope", "Mean +/- 1 SD\n(~68% target)", "Mean +/- 2 SD\n(~95% target)"]
overall = [in_minmax.mean() * 100, in_1sd.mean() * 100, in_2sd.mean() * 100]
targets = [None, 68, 95]
bars = ax.bar(labels, overall, color=ACCENT, width=0.5)
for b, v in zip(bars, overall):
    ax.text(b.get_x() + b.get_width() / 2, v, f"{v:.1f}%", ha="center", va="bottom", fontsize=9)
for i, t in enumerate(targets):
    if t is not None:
        ax.hlines(t, i - 0.25, i + 0.25, color=FLAG, ls="--", lw=1.5)
ax.set_ylabel("% of holdout rows covered")
ax.set_ylim(0, 105)
ax.spines[["top", "right"]].set_visible(False)
fig.tight_layout()
fig.savefig(FIG_DIR / "v3_ensemble_ci_coverage.png", dpi=180, facecolor="white")
plt.close(fig)
print("wrote v3_ensemble_ci_coverage.png")
