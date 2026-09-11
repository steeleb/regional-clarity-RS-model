"""Figures for the SDD-weighted-training comparison: predicted-vs-observed
before/after weighting, and the top-quartile-vs-rest RMSE tradeoff."""
import json
import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

OUT_DIR = "results"
FIG_DIR = "results/figures"
MODELS = ["xgboost", "lightgbm", "nn"]
MODEL_COLORS = {"xgboost": "#1b9e77", "lightgbm": "#d95f02", "nn": "#7570b3"}


def main():
    os.makedirs(FIG_DIR, exist_ok=True)

    # ---- top-quartile vs rest RMSE, weighted vs unweighted, all models ----
    rows = []
    for name in MODELS:
        with open(f"{OUT_DIR}/weighted_comparison_{name}.json") as fh:
            r = json.load(fh)
        for c in r["comparison"]:
            rows.append({"model": name, **c})
    comp = pd.DataFrame(rows)
    comp.to_csv(f"{OUT_DIR}/weighted_comparison_all.csv", index=False)

    groups = ["top quartile", "rest", "overall"]
    fig, axes = plt.subplots(1, 3, figsize=(13, 4.5), sharey=False)
    x = np.arange(len(MODELS))
    width = 0.35
    for ax, group in zip(axes, groups):
        sub = comp[comp["group"] == group].set_index("model").loc[MODELS]
        ax.bar(x - width/2, sub["rmse_unweighted"], width, label="unweighted", color="#B0B8BD")
        ax.bar(x + width/2, sub["rmse_weighted"], width, label="SDD-weighted", color="#1B6E8C")
        ax.set_xticks(x); ax.set_xticklabels(MODELS)
        ax.set_title(f"{group} (n={int(sub['n'].iloc[0])})")
        ax.set_ylabel("RMSE (m)")
    axes[0].legend(fontsize=9)
    fig.suptitle("SDD-weighted vs. unweighted training: RMSE by SDD range")
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/weighted_vs_unweighted_rmse.png", dpi=150)
    plt.close(fig)

    # ---- pred vs obs, weighted, all models, with unweighted ghosted ----
    fig, axes = plt.subplots(1, len(MODELS), figsize=(5 * len(MODELS), 5), sharex=True, sharey=True)
    for ax, name in zip(axes, MODELS):
        preds = pd.read_parquet(f"{OUT_DIR}/weighted_predictions_{name}.parquet")
        ax.scatter(preds["harmonized_value"], preds["pred_unweighted"], alpha=0.25, s=10,
                   color="#B0B8BD", label="unweighted")
        ax.scatter(preds["harmonized_value"], preds["pred_weighted"], alpha=0.4, s=10,
                   color=MODEL_COLORS[name], label="SDD-weighted")
        lims = [0, max(preds["harmonized_value"].max(), preds["pred_weighted"].max()) * 1.05]
        ax.plot(lims, lims, "k--", lw=1)
        ax.set_xlim(lims); ax.set_ylim(lims)
        ax.set_title(name)
        ax.set_xlabel("observed Secchi (m)")
        ax.legend(fontsize=8)
    axes[0].set_ylabel("predicted Secchi (m)")
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/weighted_pred_vs_obs.png", dpi=150)
    plt.close(fig)

    print(comp.to_string(index=False))
    print("weighted comparison figures written to", FIG_DIR)


if __name__ == "__main__":
    main()
