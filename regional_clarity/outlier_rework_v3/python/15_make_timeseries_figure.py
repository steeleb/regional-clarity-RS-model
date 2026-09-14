"""Example time series for the Claude_models_overview.md README - v3's
report has a pred-vs-obs (v3_pred_vs_obs.png) but no per-site time series;
this fills that gap using the 5-seed x 2-model ensemble's own holdout
predictions, averaged per site-date, same style as outlier_rework's
original timeseries_examples.png."""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import pandas as pd

FIG_DIR = "../figures"


def load_gnis_lookup(path):
    sc = pd.read_feather(path)[["siteSR_id", "wb_gnis_name"]].drop_duplicates("siteSR_id")
    return {row.siteSR_id: (row.wb_gnis_name if pd.notna(row.wb_gnis_name) else "Unnamed")
            for row in sc.itertuples()}


def main():
    preds = pd.read_parquet("../results/v3_ensemble_all_predictions.parquet")
    agg = preds.groupby(["siteSR_id", "date", "HUC4"]).agg(y=("y", "first"), pred=("pred", "mean")).reset_index()
    agg["date"] = pd.to_datetime(agg["date"])
    gnis = load_gnis_lookup("../../outlier_rework/site_characteristics.feather")

    site_counts = agg.groupby("siteSR_id").size().sort_values(ascending=False)
    sites = site_counts.head(6).index.tolist()

    fig, axes = plt.subplots(len(sites), 1, figsize=(9, 2.6 * len(sites)))
    for ax, site in zip(axes, sites):
        sub = agg[agg["siteSR_id"] == site].sort_values("date")
        ax.plot(sub["date"], sub["y"], "o-", color="black", label="observed", markersize=4)
        ax.plot(sub["date"], sub["pred"], "o--", color="#1b9e77", label="predicted (5-seed ensemble avg.)",
                markersize=3, alpha=0.85)
        ax.set_title(f"{gnis.get(site, 'Unnamed')} (site {site}, HUC4 {sub['HUC4'].iloc[0]}, n={len(sub)})", fontsize=9)
        ax.set_ylabel("Secchi (m)")
    axes[0].legend(fontsize=8, loc="best")
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/v3_timeseries_examples.png", dpi=150)
    plt.close(fig)
    print("wrote v3_timeseries_examples.png to", FIG_DIR)


if __name__ == "__main__":
    main()
