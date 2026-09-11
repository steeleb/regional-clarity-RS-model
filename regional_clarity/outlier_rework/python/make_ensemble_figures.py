"""Figures for the ensemble analysis: test RMSE/bias across individual
models and ensemble combinations, and the OOF residual correlation matrix
that explains why ensembling doesn't help here."""
import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

OUT_DIR = "results"
FIG_DIR = "results/figures"
MODELS = ["xgboost", "lightgbm", "nn"]


def main():
    os.makedirs(FIG_DIR, exist_ok=True)

    summary = pd.read_csv(f"{OUT_DIR}/ensemble_test_comparison.csv", index_col=0)
    order = ["lightgbm (SDD-weighted, solo)", "xgboost (SDD-weighted, solo)",
             "nn (SDD-weighted, solo)", "simple average (3-model)", "NNLS-stacked (3-model)"]
    summary = summary.loc[order]
    labels = ["LightGBM\n(solo)", "XGBoost\n(solo)", "NN\n(solo)", "Simple avg\n(3-model)", "NNLS-stacked\n(3-model)"]
    colors = ["#d95f02", "#1b9e77", "#7570b3", "#8C8C8C", "#1B6E8C"]

    fig, axes = plt.subplots(1, 2, figsize=(11, 4.5))
    axes[0].bar(labels, summary["rmse"], color=colors)
    axes[0].set_ylabel("test RMSE (m)")
    axes[0].set_title("Held-out test RMSE")
    axes[0].axhline(summary["rmse"].min(), color="black", lw=0.8, linestyle=":")
    axes[0].tick_params(axis='x', rotation=20)

    axes[1].bar(labels, summary["bias"], color=colors)
    axes[1].axhline(0, color="grey", lw=0.6)
    axes[1].set_ylabel("test bias (m)")
    axes[1].set_title("Held-out test bias")
    axes[1].tick_params(axis='x', rotation=20)
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/ensemble_test_comparison.png", dpi=150)
    plt.close(fig)

    # residual correlation heatmap
    oof = {}
    y = None
    for m in MODELS:
        d = pd.read_parquet(f"{OUT_DIR}/oof_predictions_{m}.parquet")
        oof[m] = d.set_index(["siteSR_id", "date", "part"])["pred"]
        if y is None:
            y = d.set_index(["siteSR_id", "date", "part"])["y"]
    resid = pd.concat({m: oof[m] - y for m in MODELS}, axis=1)
    resid.columns = MODELS
    corr = resid.corr()

    fig, ax = plt.subplots(figsize=(4.5, 4))
    im = ax.imshow(corr.values, vmin=0.7, vmax=1.0, cmap="Blues")
    ax.set_xticks(range(len(MODELS))); ax.set_xticklabels(MODELS, rotation=20)
    ax.set_yticks(range(len(MODELS))); ax.set_yticklabels(MODELS)
    for i in range(len(MODELS)):
        for j in range(len(MODELS)):
            ax.text(j, i, f"{corr.values[i,j]:.2f}", ha="center", va="center",
                    color="white" if corr.values[i, j] > 0.88 else "black")
    ax.set_title("OOF residual correlation")
    fig.colorbar(im, ax=ax, shrink=0.8)
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/residual_correlation.png", dpi=150)
    plt.close(fig)

    print("wrote ensemble figures")


if __name__ == "__main__":
    main()
