"""Figures for the SHAP analysis: group-contribution stacked bars (optical
vs site vs weather share of total attribution) and per-config top-feature
importance bars."""
import json
import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

OUT_DIR = "results"
FIG_DIR = "results/figures"
MODELS = ["xgboost", "lightgbm", "nn"]
GROUPS = ["optical+site", "optical+weather", "optical+site+weather"]
GROUP_LABELS = ["+ site", "+ weather", "+ both"]
CAT_COLORS = {"optical": "#1B6E8C", "site": "#E0862A", "weather": "#3E7D53"}


def main():
    os.makedirs(FIG_DIR, exist_ok=True)
    summary = pd.read_csv(f"{OUT_DIR}/shap_group_contribution.csv")

    # ---- stacked bar: contribution share by category, per model/config ----
    fig, axes = plt.subplots(1, len(MODELS), figsize=(13, 4.5), sharey=True)
    x = np.arange(len(GROUPS))
    for ax, m in zip(axes, MODELS):
        sub = summary[summary["model"] == m].set_index("feature_group").loc[GROUPS]
        ax.bar(x, sub["optical_pct"], color=CAT_COLORS["optical"], label="optical")
        ax.bar(x, sub["site_pct"], bottom=sub["optical_pct"], color=CAT_COLORS["site"], label="site")
        ax.bar(x, sub["weather_pct"], bottom=sub["optical_pct"] + sub["site_pct"],
               color=CAT_COLORS["weather"], label="weather")
        ax.set_xticks(x)
        ax.set_xticklabels(GROUP_LABELS)
        ax.set_title(m)
        ax.set_ylim(0, 100)
    axes[0].set_ylabel("share of total mean|SHAP| (%)")
    axes[0].legend(fontsize=9, loc="upper right")
    fig.suptitle("SHAP attribution share by feature category")
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/shap_group_contribution.png", dpi=150)
    plt.close(fig)

    # ---- per-config top-feature bars for the optical+site+weather (full) model ----
    fig, axes = plt.subplots(1, len(MODELS), figsize=(14, 5))
    for ax, m in zip(axes, MODELS):
        tag = "optical_site_weather"
        with open(f"{OUT_DIR}/shap_{m}_{tag}.json") as fh:
            r = json.load(fh)
        top = pd.DataFrame(r["per_feature"][:12]).iloc[::-1]
        colors = [CAT_COLORS[g] for g in top["group"]]
        ax.barh(top["feature"], top["mean_abs_shap"], color=colors)
        ax.set_title(m)
        ax.set_xlabel("mean |SHAP|")
    handles = [plt.Rectangle((0, 0), 1, 1, color=c) for c in CAT_COLORS.values()]
    axes[-1].legend(handles, CAT_COLORS.keys(), loc="lower right", fontsize=8)
    fig.suptitle("Top features, optical+site+weather model (test set)")
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/shap_top_features_full.png", dpi=150)
    plt.close(fig)

    print("wrote SHAP figures")


if __name__ == "__main__":
    main()
