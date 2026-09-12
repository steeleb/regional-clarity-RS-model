"""Figures + summary tables for the partition-sensitivity report. Reads
rotation_results.csv (from 02_run_partition_experiments.py) and
huc8_membership_by_scheme.csv (from 01_build_partition_schemes.py)."""
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

DATA_DIR = Path(__file__).resolve().parents[1] / "data"
FIG_DIR = Path(__file__).resolve().parents[1] / "figures"
FIG_DIR.mkdir(parents=True, exist_ok=True)

N_SEEDS = 10
RAND_SEED_COLS = [f"part_huc8_rand_seed{s}" for s in range(1, N_SEEDS + 1)]

plt.rcParams.update({
    "font.family": "sans-serif",
    "font.size": 10,
    "axes.edgecolor": "#D7E3E6",
    "axes.labelcolor": "#16262E",
    "text.color": "#16262E",
    "xtick.color": "#55707A",
    "ytick.color": "#55707A",
    "axes.facecolor": "white",
    "figure.facecolor": "white",
})
ACCENT = "#1B6E8C"
ACCENT_STRONG = "#0D4A5E"
FLAG = "#C1442D"
GOOD = "#3E7D53"
MUTED = "#9AB0B8"


def scheme_label(part_col):
    if part_col == "part_huc4_baseline":
        return "HUC4 (current)"
    if part_col == "part_huc8_greedy":
        return "HUC8 greedy"
    if part_col.startswith("part_huc8_rand_seed"):
        return f"HUC8 random seed {part_col.split('seed')[1]}"
    return part_col


def fig_rotation_by_partition(results):
    """RMSE by held-out test partition, HUC4 baseline vs HUC8 greedy,
    one panel per model - the direct 'circulate the partitions' result."""
    fig, axes = plt.subplots(1, 2, figsize=(9, 3.6), sharey=True)
    for ax, model in zip(axes, ["xgboost", "lightgbm"]):
        sub = results[results["model"] == model]
        width = 0.35
        for i, (col, color, label) in enumerate([
            ("part_huc4_baseline", ACCENT_STRONG, "HUC4 (current)"),
            ("part_huc8_greedy", "#57B8D6", "HUC8 greedy"),
        ]):
            s = sub[sub["part_col"] == col].sort_values("test_part")
            x = s["test_part"].to_numpy() + (i - 0.5) * width
            ax.bar(x, s["rmse"], width=width, color=color, label=label)
        ax.set_xticks([1, 2, 3, 4, 5])
        ax.set_xlabel("held-out test partition")
        ax.set_title(model, fontsize=11)
        ax.spines[["top", "right"]].set_visible(False)
    axes[0].set_ylabel("test RMSE (m)")
    axes[1].legend(fontsize=8, loc="upper right")
    fig.tight_layout()
    fig.savefig(FIG_DIR / "rotation_by_partition.png", dpi=180)
    plt.close(fig)


def fig_scheme_spread(results):
    """Distribution of test RMSE across the 5 rotations, for HUC4 baseline,
    HUC8 greedy, and pooled across all 10 HUC8-random-seed rotations (50
    points) - shows whether HUC8 grouping narrows or widens rotation-to-
    rotation spread relative to today's HUC4 grouping."""
    fig, axes = plt.subplots(1, 2, figsize=(9, 4), sharey=True)
    for ax, model in zip(axes, ["xgboost", "lightgbm"]):
        sub = results[results["model"] == model]
        groups = {
            "HUC4\n(current)": sub[sub["part_col"] == "part_huc4_baseline"]["rmse"].to_numpy(),
            "HUC8\ngreedy": sub[sub["part_col"] == "part_huc8_greedy"]["rmse"].to_numpy(),
            "HUC8 random\n(10 seeds pooled)": sub[sub["part_col"].isin(RAND_SEED_COLS)]["rmse"].to_numpy(),
        }
        positions = range(1, len(groups) + 1)
        bp = ax.boxplot(list(groups.values()), positions=positions, widths=0.5,
                         patch_artist=True, showmeans=True,
                         medianprops=dict(color=ACCENT_STRONG),
                         meanprops=dict(marker="D", markerfacecolor=FLAG, markeredgecolor=FLAG, markersize=5))
        for patch in bp["boxes"]:
            patch.set(facecolor="#DCEEF3", edgecolor=ACCENT)
        for i, (label, vals) in enumerate(groups.items()):
            jitter = np.random.default_rng(0).uniform(-0.12, 0.12, size=len(vals))
            ax.scatter(np.full(len(vals), i + 1) + jitter, vals, s=14, color=MUTED,
                       alpha=0.7, zorder=3)
        ax.set_xticks(positions)
        ax.set_xticklabels(groups.keys(), fontsize=8.5)
        ax.set_title(model, fontsize=11)
        ax.spines[["top", "right"]].set_visible(False)
    axes[0].set_ylabel("test RMSE across rotations (m)")
    fig.tight_layout()
    fig.savefig(FIG_DIR / "scheme_spread.png", dpi=180)
    plt.close(fig)


def fig_cluster_concentration(membership):
    """For HUC4 1701 and HUC4 1407 (the two basins behind the outlier-HUC
    findings), what's the largest share of that HUC4's rows any single
    partition holds, under each scheme? 100% under HUC4 grouping means the
    whole cluster is inseparable from a single fold by construction."""
    def max_share(df, huc4, col):
        sub = df[df["HUC4"] == huc4]
        total = sub["n_rows"].sum()
        by_part = sub.groupby(col)["n_rows"].sum()
        return by_part.max() / total * 100

    huc4s = [("1701", "HUC4 1701\n(forested lake cluster)"), ("1407", "HUC4 1407\n(Lake Powell)")]
    schemes = [("part_huc4_baseline", "HUC4 (current)", ACCENT_STRONG),
               ("part_huc8_greedy", "HUC8 greedy", "#57B8D6")]

    fig, ax = plt.subplots(figsize=(7, 4))
    x = np.arange(len(huc4s))
    width = 0.22
    for i, (col, label, color) in enumerate(schemes):
        vals = [max_share(membership, h, col) for h, _ in huc4s]
        ax.bar(x + (i - 1) * width, vals, width=width, color=color, label=label)

    rand_vals = {h: [max_share(membership, h, c) for c in RAND_SEED_COLS] for h, _ in huc4s}
    rand_mean = [np.mean(rand_vals[h]) for h, _ in huc4s]
    rand_lo = [np.min(rand_vals[h]) for h, _ in huc4s]
    rand_hi = [np.max(rand_vals[h]) for h, _ in huc4s]
    xr = x + 1 * width
    ax.bar(xr, rand_mean, width=width, color=MUTED, label="HUC8 random\n(mean, range over 10 seeds)")
    ax.errorbar(xr, rand_mean,
                yerr=[np.array(rand_mean) - np.array(rand_lo), np.array(rand_hi) - np.array(rand_mean)],
                fmt="none", ecolor="#55707A", capsize=4, lw=1.3)

    ax.set_xticks(x)
    ax.set_xticklabels([lbl for _, lbl in huc4s])
    ax.set_ylabel("% of basin's rows in its largest single partition")
    ax.set_ylim(0, 105)
    ax.legend(fontsize=8, loc="upper right")
    ax.spines[["top", "right"]].set_visible(False)
    fig.tight_layout()
    fig.savefig(FIG_DIR / "cluster_concentration.png", dpi=180)
    plt.close(fig)


def summary_tables(results):
    def summarize(df, group_cols):
        g = df.groupby(group_cols)["rmse"]
        return pd.DataFrame({"mean_rmse": g.mean(), "sd_rmse": g.std(),
                              "min_rmse": g.min(), "max_rmse": g.max(),
                              "range_rmse": g.max() - g.min()}).reset_index()

    by_scheme_model = summarize(results, ["part_col", "model"])
    by_scheme_model.to_csv(DATA_DIR / "summary_by_scheme_model.csv", index=False)

    rand_only = results[results["part_col"].isin(RAND_SEED_COLS)]
    rand_pooled = summarize(rand_only, ["model"])
    rand_pooled["part_col"] = "part_huc8_rand_pooled"
    rand_pooled.to_csv(DATA_DIR / "summary_rand_pooled.csv", index=False)

    print(by_scheme_model.to_string(index=False))
    print()
    print(rand_pooled.to_string(index=False))
    return by_scheme_model, rand_pooled


def main():
    results = pd.read_csv(DATA_DIR / "rotation_results.csv")
    membership = pd.read_csv(DATA_DIR / "huc8_membership_by_scheme.csv", dtype={"HUC4": str, "HUC8": str})

    fig_rotation_by_partition(results)
    fig_scheme_spread(results)
    fig_cluster_concentration(membership)
    summary_tables(results)
    print("\nfigures written to", FIG_DIR)


if __name__ == "__main__":
    main()
