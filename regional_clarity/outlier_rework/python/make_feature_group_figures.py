"""Figures for the site-characteristics / antecedent-weather feature-group
comparison: test RMSE/R2/bias by model x feature group, and pred-vs-
observed for the best (optical+site+weather) configuration."""
import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

OUT_DIR = "results"
FIG_DIR = "results/figures"
MODELS = ["xgboost", "lightgbm", "nn"]
GROUPS = ["optical", "optical+site", "optical+weather", "optical+site+weather"]
GROUP_LABELS = ["optical\n(baseline)", "+ site\ncharacteristics", "+ antecedent\nweather", "+ both"]
MODEL_COLORS = {"xgboost": "#1b9e77", "lightgbm": "#d95f02", "nn": "#7570b3"}


def main():
    os.makedirs(FIG_DIR, exist_ok=True)
    summary = pd.read_csv(f"{OUT_DIR}/feature_group_comparison.csv")

    fig, axes = plt.subplots(1, 3, figsize=(14, 4.5))
    x = np.arange(len(GROUPS))
    width = 0.25
    for i, m in enumerate(MODELS):
        sub = summary[summary["model"] == m].set_index("feature_group").loc[GROUPS]
        axes[0].bar(x + (i - 1) * width, sub["rmse"], width, label=m, color=MODEL_COLORS[m])
        axes[1].bar(x + (i - 1) * width, sub["r2"], width, label=m, color=MODEL_COLORS[m])
        axes[2].bar(x + (i - 1) * width, sub["bias"], width, label=m, color=MODEL_COLORS[m])
    for ax, title in zip(axes, ["test RMSE (m)", "test R2", "test bias (m)"]):
        ax.set_xticks(x)
        ax.set_xticklabels(GROUP_LABELS, fontsize=8.5)
        ax.set_title(title)
    axes[2].axhline(0, color="grey", lw=0.7)
    axes[0].legend(fontsize=8)
    fig.suptitle("Effect of site characteristics and antecedent weather on test performance")
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/feature_group_comparison.png", dpi=150)
    plt.close(fig)
    print("wrote feature_group_comparison.png")


if __name__ == "__main__":
    main()
