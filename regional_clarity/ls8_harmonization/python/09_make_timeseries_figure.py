"""Example time series for the Claude_models_overview.md README - this
project's report has pred_vs_obs_arms.png but no per-site time series.
Uses the LS8-ref arm (the recommended configuration) ensemble holdout
predictions."""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import pandas as pd

FIG_DIR = "../figures"


def main():
    preds = pd.read_parquet("../results/ls8ref/ensemble_all_predictions.parquet")
    agg = preds.groupby(["siteSR_id", "date", "HUC4"]).agg(y=("y", "first"), pred=("pred", "mean")).reset_index()
    agg["date"] = pd.to_datetime(agg["date"])

    site_counts = agg.groupby("siteSR_id").size().sort_values(ascending=False)
    sites = site_counts.head(6).index.tolist()

    fig, axes = plt.subplots(len(sites), 1, figsize=(9, 2.6 * len(sites)))
    for ax, site in zip(axes, sites):
        sub = agg[agg["siteSR_id"] == site].sort_values("date")
        ax.plot(sub["date"], sub["y"], "o-", color="black", label="observed", markersize=4)
        ax.plot(sub["date"], sub["pred"], "o--", color="#c1562e", label="predicted (LS8-ref, 5-seed ensemble avg.)",
                markersize=3, alpha=0.85)
        ax.set_title(f"site {site} (HUC4 {sub['HUC4'].iloc[0]}, n={len(sub)})", fontsize=9)
        ax.set_ylabel("Secchi (m)")
    axes[0].legend(fontsize=8, loc="best")
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/ls8ref_timeseries_examples.png", dpi=150)
    plt.close(fig)
    print("wrote ls8ref_timeseries_examples.png to", FIG_DIR)


if __name__ == "__main__":
    main()
