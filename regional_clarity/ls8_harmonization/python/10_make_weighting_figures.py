"""SDD-weighting figures for all three ls8_harmonization arms (ls7ref,
ls8ref, l89only) - same figures/design as outlier_rework_v3's
07_make_weighting_figures.py and 14_make_weighted_predobs_figure.py,
just looped over arms since weighting_experiment_summary.json/
weighting_experiment_predictions.parquet share the identical schema."""
import json
from pathlib import Path
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

ROOT = Path("/Users/steeleb/Documents/GitHub/regional-clarity-RS-model/regional_clarity/ls8_harmonization")
FIG_DIR = ROOT / "figures"
RESULTS = ROOT / "results"
ARMS = ["ls7ref", "ls8ref", "l89only"]
ARM_LABEL = {"ls7ref": "LS7-ref (full corpus)", "ls8ref": "LS8-ref (full corpus)", "l89only": "LS8/9-only"}

plt.rcParams.update({"font.family": "sans-serif", "font.size": 10,
                      "axes.edgecolor": "#D7E3E6", "axes.labelcolor": "#16262E",
                      "text.color": "#16262E", "xtick.color": "#55707A", "ytick.color": "#55707A"})
ACCENT = "#1B6E8C"
FLAG = "#C1442D"
GOOD = "#3E7D53"
MUTED = "#9AB0B8"

groups = ["overall", "top_quartile", "rest"]

for arm in ARMS:
    d = json.load(open(RESULTS / arm / "weighting_experiment_summary.json"))
    n_overall = d["unweighted"]["overall"]["n"]
    n_top = d["unweighted"]["top_quartile"]["n"]
    n_rest = d["unweighted"]["rest"]["n"]
    group_labels = [f"Overall\n(n={n_overall})", f"Top-quartile SDD\n(n={n_top})", f"Rest\n(n={n_rest})"]

    # ---- RMSE by group ----
    unweighted_rmse = [d["unweighted"][g]["rmse"] for g in groups]
    weighted_rmse = [d["weighted"][g]["rmse"] for g in groups]
    fig, ax = plt.subplots(figsize=(7.5, 4.3))
    x = np.arange(len(groups))
    width = 0.35
    bars1 = ax.bar(x - width / 2, unweighted_rmse, width, label="unweighted", color=MUTED)
    bars2 = ax.bar(x + width / 2, weighted_rmse, width, label="SDD-weighted (k=2.0)", color=ACCENT)
    for bars in (bars1, bars2):
        for b in bars:
            ax.text(b.get_x() + b.get_width() / 2, b.get_height(), f"{b.get_height():.3f}",
                     ha="center", va="bottom", fontsize=8.5)
    ax.set_xticks(x); ax.set_xticklabels(group_labels, fontsize=9)
    ax.set_ylabel("holdout RMSE (m)")
    ax.set_title(f"{ARM_LABEL[arm]}: unweighted vs. SDD-weighted", fontsize=11)
    ax.legend(fontsize=9)
    ax.spines[["top", "right"]].set_visible(False)
    fig.tight_layout()
    fig.savefig(FIG_DIR / f"{arm}_weighting_rmse.png", dpi=180, facecolor="white")
    plt.close(fig)

    # ---- bias by group ----
    unweighted_bias = [d["unweighted"][g]["bias"] for g in groups]
    weighted_bias = [d["weighted"][g]["bias"] for g in groups]
    fig, ax = plt.subplots(figsize=(7.5, 4.3))
    bars1 = ax.bar(x - width / 2, unweighted_bias, width, label="unweighted", color=MUTED)
    bars2 = ax.bar(x + width / 2, weighted_bias, width, label="SDD-weighted (k=2.0)", color=ACCENT)
    ax.axhline(0, color="#16262E", lw=0.8)
    for bars in (bars1, bars2):
        for b in bars:
            ax.text(b.get_x() + b.get_width() / 2, b.get_height(), f"{b.get_height():+.2f}",
                     ha="center", va="bottom" if b.get_height() >= 0 else "top", fontsize=8.5)
    ax.set_xticks(x); ax.set_xticklabels(group_labels, fontsize=9)
    ax.set_ylabel("mean bias, pred - obs (m)")
    ax.set_title(f"{ARM_LABEL[arm]}: unweighted vs. SDD-weighted", fontsize=11)
    ax.legend(fontsize=9)
    ax.spines[["top", "right"]].set_visible(False)
    fig.tight_layout()
    fig.savefig(FIG_DIR / f"{arm}_weighting_bias.png", dpi=180, facecolor="white")
    plt.close(fig)

    # ---- pred vs obs, unweighted vs weighted ----
    wexp = pd.read_parquet(RESULTS / arm / "weighting_experiment_predictions.parquet")
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
    fig.suptitle(ARM_LABEL[arm], fontsize=12, y=1.02)
    fig.tight_layout()
    fig.savefig(FIG_DIR / f"{arm}_pred_vs_obs_weighted.png", dpi=180, facecolor="white", bbox_inches="tight")
    plt.close(fig)

    print(f"{arm}: wrote weighting_rmse, weighting_bias, pred_vs_obs_weighted figures")
