"""Example time series comparing all three arms' predictions against the
same observed values, at sites present in all three arms' holdouts (so
LS7-ref, LS8-ref, and LS8/9-only can be compared directly on identical
observed points, not just each on its own). LS8/9-only is naturally
sparser at any given site since it only covers LC08/LC09 dates - gaps in
its line/markers reflect that, not missing data handling."""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import pandas as pd

FIG_DIR = "../figures"
ARM_COLOR = {"ls7ref": "#6fa8dc", "ls8ref": "#c1562e", "l89only": "#3e7d53"}
ARM_LABEL = {"ls7ref": "LS7-ref", "ls8ref": "LS8-ref", "l89only": "LS8/9-only"}
ARMS = ["ls7ref", "ls8ref", "l89only"]


def load_gnis_lookup(path):
    sc = pd.read_feather(path)[["siteSR_id", "wb_gnis_name"]].drop_duplicates("siteSR_id")
    return {row.siteSR_id: (row.wb_gnis_name if pd.notna(row.wb_gnis_name) else "Unnamed")
            for row in sc.itertuples()}


def main():
    aggs = {}
    for arm in ARMS:
        preds = pd.read_parquet(f"../results/{arm}/ensemble_all_predictions.parquet")
        agg = preds.groupby(["siteSR_id", "date", "HUC4"]).agg(
            y=("y", "first"), pred=("pred", "mean")).reset_index()
        agg["date"] = pd.to_datetime(agg["date"])
        aggs[arm] = agg

    common_sites = set(aggs["ls7ref"]["siteSR_id"]) & set(aggs["ls8ref"]["siteSR_id"]) & set(aggs["l89only"]["siteSR_id"])
    site_counts = aggs["l89only"][aggs["l89only"]["siteSR_id"].isin(common_sites)].groupby("siteSR_id").size().sort_values(ascending=False)
    sites = site_counts.head(4).index.tolist()

    gnis = load_gnis_lookup("../../outlier_rework/site_characteristics.feather")

    fig, axes = plt.subplots(len(sites), 1, figsize=(10, 2.8 * len(sites)))
    for ax, site in zip(axes, sites):
        obs = aggs["ls7ref"][aggs["ls7ref"]["siteSR_id"] == site][["date", "y", "HUC4"]].drop_duplicates("date").sort_values("date")
        ax.plot(obs["date"], obs["y"], "o-", color="black", label="observed", markersize=4, zorder=5)
        for arm in ARMS:
            sub = aggs[arm][aggs[arm]["siteSR_id"] == site].sort_values("date")
            ax.plot(sub["date"], sub["pred"], "o--", color=ARM_COLOR[arm], label=f"predicted ({ARM_LABEL[arm]})",
                     markersize=3, alpha=0.85)
        n_by_arm = {arm: (aggs[arm]["siteSR_id"] == site).sum() for arm in ARMS}
        ax.set_title(f"{gnis.get(site, 'Unnamed')} (site {site}, HUC4 {obs['HUC4'].iloc[0]}, "
                     f"n: LS7/LS8={n_by_arm['ls7ref']}, LS8/9-only={n_by_arm['l89only']})", fontsize=9)
        ax.set_ylabel("Secchi (m)")
    axes[0].legend(fontsize=7.5, loc="best", ncol=2)
    fig.suptitle("All three arms' predictions vs. the same observed values, example holdout sites", fontsize=11, y=1.0)
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/multiarm_timeseries_examples.png", dpi=180, bbox_inches="tight")
    plt.close(fig)
    print("wrote multiarm_timeseries_examples.png to", FIG_DIR, "; sites:", sites)


if __name__ == "__main__":
    main()
