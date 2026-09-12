"""Figures for the ls8_harmonization report: sensor-count impact, the
controlled Arm A (LS7-ref) vs Arm B (LS8-ref) comparison, per-HUC4
breakdown, feature-selection stability/what-flipped, pred-vs-obs for all
three arms, and a basin-matched fairness check for Arm C (LS8/9-only)."""
import json
import sys
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
FIG_DIR = ROOT / "figures"
FIG_DIR.mkdir(exist_ok=True)
RESULTS = ROOT / "results"
ENSEMBLE_SEEDS = [501, 502, 503, 504, 505]

COLOR_A = "#3d7a8c"   # LS7 reference (control) - teal
COLOR_B = "#c1562e"   # LS8 reference - burnt orange
COLOR_C = "#7a5c9e"   # LS8/9-only - purple
COLOR_DROP = "#b0b0b0"


def load_summary(arm):
    with open(RESULTS / arm / "ensemble_summary.json") as f:
        return json.load(f)


def fig_sensor_counts():
    before = {"LT04": 43, "LT05": 5482, "LE07": 5187, "LC08": 1720, "LC09": 205}
    after = {"LT04": 0, "LT05": 0, "LE07": 5187, "LC08": 1720, "LC09": 205}
    missions = list(before.keys())
    fig, ax = plt.subplots(figsize=(7, 4.2))
    x = np.arange(len(missions))
    kept_colors = [COLOR_DROP if before[m] > 0 and after[m] == 0 else "#4a8f7a" for m in missions]
    bars = ax.bar(x, [before[m] for m in missions], color=kept_colors, edgecolor="white")
    for i, m in enumerate(missions):
        if before[m] > 0 and after[m] == 0:
            ax.text(i, before[m] + 80, "dropped\n(no LS8 coef.)", ha="center", fontsize=8, color="#666")
        else:
            ax.text(i, before[m] + 80, "kept", ha="center", fontsize=8, color="#2a5c47")
    ax.set_xticks(x)
    ax.set_xticklabels(missions)
    ax.set_ylabel("rows in full 12,637-row corpus")
    ax.set_title("Which sensors survive an LS8-referenced harmonization")
    total_before = sum(before.values())
    total_after = sum(after.values())
    ax.text(0.98, 0.95, f"{total_after:,} / {total_before:,} rows retained ({total_after/total_before:.0%})",
            transform=ax.transAxes, ha="right", va="top", fontsize=9,
            bbox=dict(boxstyle="round", fc="#f5f5f0", ec="#ccc"))
    fig.tight_layout()
    fig.savefig(FIG_DIR / "sensor_row_counts.png", dpi=150)
    plt.close(fig)


def fig_controlled_comparison():
    a, b = load_summary("ls7ref"), load_summary("ls8ref")
    metrics = ["rmse", "mae", "bias", "r2"]
    labels = ["RMSE (m)", "MAE (m)", "Bias (m)", "R²"]
    fig, axes = plt.subplots(1, 4, figsize=(11, 3.6))
    for ax, met, lab in zip(axes, metrics, labels):
        va, vb = a["ensemble_metrics"][met], b["ensemble_metrics"][met]
        bars = ax.bar(["LS7-ref\n(control)", "LS8-ref"], [va, vb], color=[COLOR_A, COLOR_B], width=0.6)
        for bar, v in zip(bars, [va, vb]):
            ax.text(bar.get_x() + bar.get_width() / 2, v + (0.02 if v >= 0 else -0.06) * max(abs(va), abs(vb), 1),
                    f"{v:.3f}", ha="center", fontsize=9, va="bottom" if v >= 0 else "top")
        ax.set_title(lab, fontsize=10)
        ax.axhline(0, color="#ccc", lw=0.8)
        ax.set_ylim(min(0, va, vb) * 1.3 - 0.05, max(va, vb) * 1.25 + 0.05)
    fig.suptitle("Controlled comparison: same 1,114-row holdout, same splits, only the reference sensor differs", fontsize=11)
    fig.tight_layout(rect=[0, 0, 1, 0.92])
    fig.savefig(FIG_DIR / "controlled_comparison.png", dpi=150)
    plt.close(fig)


def fig_huc4_comparison():
    a, b = load_summary("ls7ref"), load_summary("ls8ref")
    huc4s = sorted(a["per_huc4"].keys(), key=lambda h: -a["per_huc4"][h]["n"])
    rmse_a = [a["per_huc4"][h]["rmse"] for h in huc4s]
    rmse_b = [b["per_huc4"][h]["rmse"] for h in huc4s]
    ns = [a["per_huc4"][h]["n"] for h in huc4s]

    fig, ax = plt.subplots(figsize=(11, 4.5))
    x = np.arange(len(huc4s))
    w = 0.36
    bars_a = ax.bar(x - w / 2, rmse_a, w, label="LS7-ref (control)", color=COLOR_A)
    bars_b = ax.bar(x + w / 2, rmse_b, w, label="LS8-ref", color=COLOR_B)
    ax.set_xticks(x)
    ax.set_xticklabels([f"{h}\n(n={n})" for h, n in zip(huc4s, ns)], fontsize=8)
    ax.set_ylabel("holdout RMSE (m)")
    ax.set_title("Per-HUC4 RMSE: LS7-ref vs LS8-ref (same rows, same splits)")
    for h in ["1407", "1701"]:
        if h in huc4s:
            i = huc4s.index(h)
            ax.axvspan(i - 0.5, i + 0.5, color="#fff3cd", alpha=0.5, zorder=0)
    ax.legend(fontsize=9)
    fig.tight_layout()
    fig.savefig(FIG_DIR / "huc4_comparison_ab.png", dpi=150)
    plt.close(fig)


def fig_feature_stability():
    fig, axes = plt.subplots(1, 2, figsize=(13, 6), sharey=False)
    for ax, arm, color, title in zip(
            axes, ["ls7ref", "ls8ref"], [COLOR_A, COLOR_B],
            ["LS7-ref: feature selected per seed", "LS8-ref: feature selected per seed"]):
        per_seed_feats = []
        for s in ENSEMBLE_SEEDS:
            with open(RESULTS / arm / f"seed{s}" / "backward_elim_summary.json") as f:
                per_seed_feats.append(set(json.load(f)["intersection_of_per_model"]))
        all_feats = sorted(set.union(*per_seed_feats), key=lambda f: -sum(f in s for s in per_seed_feats))
        mat = np.array([[1 if f in s else 0 for s in per_seed_feats] for f in all_feats])
        ax.imshow(mat, cmap=matplotlib.colors.ListedColormap(["#eee", color]), aspect="auto", vmin=0, vmax=1)
        ax.set_yticks(range(len(all_feats)))
        ax.set_yticklabels(all_feats, fontsize=8)
        ax.set_xticks(range(len(ENSEMBLE_SEEDS)))
        ax.set_xticklabels([f"seed{s}" for s in ENSEMBLE_SEEDS], fontsize=8)
        ax.set_title(title, fontsize=10)
    fig.suptitle("Feature-selection stability across the 5-seed ensemble", fontsize=12)
    fig.tight_layout(rect=[0, 0, 1, 0.95])
    fig.savefig(FIG_DIR / "feature_stability.png", dpi=150)
    plt.close(fig)


def fig_pred_vs_obs():
    fig, axes = plt.subplots(1, 3, figsize=(13, 4.5), sharex=True, sharey=True)
    for ax, arm, color, title in zip(
            axes, ["ls7ref", "ls8ref", "l89only"], [COLOR_A, COLOR_B, COLOR_C],
            ["LS7-ref (control)", "LS8-ref", "LS8/9-only"]):
        df = pd.read_parquet(RESULTS / arm / "ensemble_all_predictions.parquet")
        agg = df.groupby(["siteSR_id", "date"]).agg(y=("y", "first"), pred=("pred", "mean")).reset_index()
        ax.scatter(agg["y"], agg["pred"], s=8, alpha=0.35, color=color, edgecolor="none")
        lims = [0, max(agg["y"].max(), agg["pred"].max()) * 1.05]
        ax.plot(lims, lims, color="#333", lw=1, ls="--")
        ax.set_xlim(lims)
        ax.set_ylim(lims)
        ax.set_title(f"{title} (n={len(agg)})", fontsize=10)
        ax.set_xlabel("observed SDD (m)")
    axes[0].set_ylabel("predicted SDD (m)")
    fig.suptitle("Predicted vs. observed, each arm's own fixed holdout", fontsize=12)
    fig.tight_layout(rect=[0, 0, 1, 0.93])
    fig.savefig(FIG_DIR / "pred_vs_obs_arms.png", dpi=150)
    plt.close(fig)


def fig_l89_basin_matched():
    a = load_summary("ls7ref")
    b = load_summary("ls8ref")
    c = load_summary("l89only")
    common = [h for h in c["per_huc4"] if c["per_huc4"][h]["n"] >= 10]
    common = sorted(common, key=lambda h: -c["per_huc4"][h]["n"])

    fig, ax = plt.subplots(figsize=(9, 4.5))
    x = np.arange(len(common))
    w = 0.27
    rmse_a = [a["per_huc4"][h]["rmse"] for h in common]
    rmse_b = [b["per_huc4"][h]["rmse"] for h in common]
    rmse_c = [c["per_huc4"][h]["rmse"] for h in common]
    ax.bar(x - w, rmse_a, w, label="LS7-ref, full corpus", color=COLOR_A)
    ax.bar(x, rmse_b, w, label="LS8-ref, full corpus", color=COLOR_B)
    ax.bar(x + w, rmse_c, w, label="LS8/9-only", color=COLOR_C)
    ax.set_xticks(x)
    n_c = [c["per_huc4"][h]["n"] for h in common]
    n_ab = [a["per_huc4"][h]["n"] for h in common]
    ax.set_xticklabels([f"{h}\n(n={nc} vs {na})" for h, nc, na in zip(common, n_c, n_ab)], fontsize=8)
    ax.set_ylabel("holdout RMSE (m)")
    ax.set_title("Basin-matched check: does LS8/9-only do worse on the SAME basins?\n(n = LS8/9-only holdout rows vs. full-corpus holdout rows for that basin)")
    ax.legend(fontsize=9)
    fig.tight_layout()
    fig.savefig(FIG_DIR / "l89_basin_matched.png", dpi=150)
    plt.close(fig)


def main():
    fig_sensor_counts()
    fig_controlled_comparison()
    fig_huc4_comparison()
    fig_feature_stability()
    fig_pred_vs_obs()
    fig_l89_basin_matched()
    print("wrote 6 figures to", FIG_DIR)


if __name__ == "__main__":
    main()
