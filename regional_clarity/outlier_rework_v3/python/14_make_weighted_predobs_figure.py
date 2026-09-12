from pathlib import Path
import matplotlib.pyplot as plt
import pandas as pd

ROOT = Path("/Users/steeleb/Documents/GitHub/regional-clarity-RS-model/regional_clarity/outlier_rework_v3")
FIG_DIR = ROOT / "figures"
RESULTS = ROOT / "results"
ACCENT, FLAG, GOOD = "#1B6E8C", "#C1442D", "#3E7D53"

plt.rcParams.update({"font.family": "sans-serif", "font.size": 10,
                      "axes.edgecolor": "#D7E3E6", "axes.labelcolor": "#16262E",
                      "text.color": "#16262E", "xtick.color": "#55707A", "ytick.color": "#55707A"})

wexp = pd.read_parquet(RESULTS / "weighting_experiment_predictions.parquet")
unw = wexp[~wexp["weighted"]].groupby(["siteSR_id", "date"]).agg(y=("y", "first"), pred=("pred", "mean")).reset_index()
w = wexp[wexp["weighted"]].groupby(["siteSR_id", "date"]).agg(y=("y", "first"), pred=("pred", "mean")).reset_index()

fig, axes = plt.subplots(1, 2, figsize=(11, 5.4), sharex=True, sharey=True)
lims = [0, max(unw["y"].max(), unw["pred"].max(), w["y"].max(), w["pred"].max()) * 1.02]
for ax, df, title, color in zip(axes, [unw, w], ["Unweighted (adopted)", "SDD-weighted (k=2.0)"], [GOOD, ACCENT]):
    ax.scatter(df["y"], df["pred"], s=10, alpha=0.35, color=color, edgecolors="none")
    ax.plot(lims, lims, color=FLAG, lw=1.5, ls="--", label="1:1 line")
    ax.set_xlim(lims); ax.set_ylim(lims)
    ax.set_xlabel("observed SDD (m)")
    ax.set_title(title, fontsize=11)
    ax.set_aspect("equal")
    ax.spines[["top", "right"]].set_visible(False)
    ax.legend(fontsize=9, loc="upper left")
axes[0].set_ylabel("predicted SDD (m)")
fig.tight_layout()
fig.savefig(FIG_DIR / "v3_pred_vs_obs_weighted.png", dpi=180, facecolor="white")
print("wrote v3_pred_vs_obs_weighted.png")
