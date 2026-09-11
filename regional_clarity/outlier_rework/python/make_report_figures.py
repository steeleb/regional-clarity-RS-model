"""Generate report figures from train_compare.py's saved outputs:
 - predicted vs. observed scatter, per model
 - model comparison bar chart (RMSE / MAE / R2)
 - residual by HUC4
 - timeseries examples for a handful of longer-record test sites
"""
import json
import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

OUT_DIR = "results"
FIG_DIR = "results/figures"

MODEL_COLORS = {"xgboost": "#1b9e77", "lightgbm": "#d95f02", "nn": "#7570b3"}


def main():
    os.makedirs(FIG_DIR, exist_ok=True)
    preds = pd.read_parquet(f"{OUT_DIR}/test_predictions.parquet")
    with open(f"{OUT_DIR}/model_comparison.json") as fh:
        results = json.load(fh)

    models = list(results["models"].keys())

    # ---- 1. predicted vs observed, one panel per model ----
    fig, axes = plt.subplots(1, len(models), figsize=(5 * len(models), 5), sharex=True, sharey=True)
    if len(models) == 1:
        axes = [axes]
    for ax, name in zip(axes, models):
        sub = preds[preds["model"] == name]
        ax.scatter(sub["harmonized_value"], sub["pred"], alpha=0.4, s=12, color=MODEL_COLORS.get(name, "grey"))
        lims = [0, max(sub["harmonized_value"].max(), sub["pred"].max()) * 1.05]
        ax.plot(lims, lims, "k--", lw=1)
        ax.set_xlim(lims); ax.set_ylim(lims)
        m = results["models"][name]["test_metrics"]
        ax.set_title(f"{name}\nRMSE={m['rmse']:.2f}  R2={m['r2']:.2f}  n={m['n']}")
        ax.set_xlabel("observed Secchi (m)")
    axes[0].set_ylabel("predicted Secchi (m)")
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/pred_vs_obs.png", dpi=150)
    plt.close(fig)

    # ---- 2. model comparison bar chart ----
    metric_names = ["rmse", "mae", "r2", "bias"]
    summary = pd.DataFrame({name: results["models"][name]["test_metrics"] for name in models}).T
    fig, axes = plt.subplots(1, len(metric_names), figsize=(4 * len(metric_names), 4))
    for ax, metric in zip(axes, metric_names):
        colors = [MODEL_COLORS.get(m, "grey") for m in summary.index]
        ax.bar(summary.index, summary[metric], color=colors)
        ax.set_title(metric.upper())
        ax.axhline(0, color="grey", lw=0.5)
        ax.tick_params(axis='x', rotation=30)
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/model_comparison_bars.png", dpi=150)
    plt.close(fig)

    # ---- 3. residuals by HUC4 ----
    preds["residual"] = preds["pred"] - preds["harmonized_value"]
    fig, ax = plt.subplots(figsize=(10, 5))
    huc4_order = preds.groupby("HUC4")["residual"].apply(lambda x: x.abs().mean()).sort_values().index
    for i, name in enumerate(models):
        sub = preds[preds["model"] == name]
        grouped = [sub[sub["HUC4"] == h]["residual"].values for h in huc4_order]
        positions = np.arange(len(huc4_order)) * (len(models) + 1) + i
        bp = ax.boxplot(grouped, positions=positions, widths=0.8, patch_artist=True, showfliers=False)
        for box in bp["boxes"]:
            box.set_facecolor(MODEL_COLORS.get(name, "grey"))
    ax.axhline(0, color="black", lw=0.8)
    tick_positions = np.arange(len(huc4_order)) * (len(models) + 1) + (len(models) - 1) / 2
    ax.set_xticks(tick_positions)
    ax.set_xticklabels(huc4_order, rotation=90)
    ax.set_ylabel("residual (pred - obs, m)")
    ax.set_title("Test-set residuals by HUC4")
    handles = [plt.Rectangle((0, 0), 1, 1, color=MODEL_COLORS[m]) for m in models]
    ax.legend(handles, models, loc="upper right")
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/residuals_by_huc4.png", dpi=150)
    plt.close(fig)

    # ---- 4. timeseries examples for longer-record test sites ----
    site_counts = preds[preds["model"] == models[0]].groupby("siteSR_id").size()
    top_sites = site_counts[site_counts >= 3].sort_values(ascending=False).head(6).index.tolist()
    if top_sites:
        fig, axes = plt.subplots(len(top_sites), 1, figsize=(9, 2.6 * len(top_sites)), sharex=False)
        if len(top_sites) == 1:
            axes = [axes]
        for ax, site in zip(axes, top_sites):
            sub = preds[preds["siteSR_id"] == site].copy()
            sub["date"] = pd.to_datetime(sub["date"])
            obs = sub[sub["model"] == models[0]][["date", "harmonized_value"]].drop_duplicates().sort_values("date")
            ax.plot(obs["date"], obs["harmonized_value"], "o-", color="black", label="observed", markersize=4)
            for name in models:
                s = sub[sub["model"] == name].sort_values("date")
                ax.plot(s["date"], s["pred"], "o--", color=MODEL_COLORS.get(name, "grey"),
                        label=name, markersize=3, alpha=0.8)
            ax.set_title(f"site {site} (n={len(obs)})", fontsize=9)
            ax.set_ylabel("Secchi (m)")
        axes[0].legend(fontsize=8, loc="best")
        fig.tight_layout()
        fig.savefig(f"{FIG_DIR}/timeseries_examples.png", dpi=150)
        plt.close(fig)

    print("figures written to", FIG_DIR)


if __name__ == "__main__":
    main()
