"""Overview figures for the Claude_models_overview.md README: a general
pred-vs-obs (CV + test, all basins, 3-model average) and example time
series, the latter deliberately including HUC4 1701 / Lake Powell (1407)
sites since that's this report's own focus - same combined CV/test framing
already used for the report's own lake_powell_pred_vs_obs.png."""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import pandas as pd

FIG_DIR = "../figures"
REWORK_RESULTS = "../../outlier_rework/python/results"
MODELS = ["xgboost", "lightgbm", "nn"]


def main():
    cv = pd.concat([pd.read_parquet(f"../data/cv_oof_predictions_{m}.parquet") for m in MODELS], ignore_index=True)
    cv_avg = cv.groupby(["siteSR_id", "date", "HUC4"]).agg(y=("y", "first"), pred=("pred", "mean")).reset_index()
    cv_avg["split"] = "train/CV (parts 1-4)"

    test = pd.concat([pd.read_parquet(f"{REWORK_RESULTS}/final_test_predictions_{m}.parquet") for m in MODELS],
                      ignore_index=True)
    test_avg = test.groupby(["siteSR_id", "date", "HUC4"]).agg(
        y=("harmonized_value", "first"), pred=("pred", "mean")).reset_index()
    test_avg["split"] = "test (part 5)"

    all_avg = pd.concat([cv_avg, test_avg], ignore_index=True)
    all_avg["date"] = pd.to_datetime(all_avg["date"])

    # ---- general pred-vs-obs, all basins, CV vs test ----
    fig, ax = plt.subplots(figsize=(6, 6))
    colors = {"train/CV (parts 1-4)": "#6fa8dc", "test (part 5)": "#e06666"}
    for split, sub in all_avg.groupby("split"):
        ax.scatter(sub["y"], sub["pred"], s=10, alpha=0.35, color=colors[split], label=f"{split} (n={len(sub)})")
    lims = [0, all_avg[["y", "pred"]].max().max() * 1.05]
    ax.plot(lims, lims, "k--", lw=1, label="1:1")
    ax.set_xlim(lims); ax.set_ylim(lims)
    ax.set_xlabel("observed Secchi depth (m)")
    ax.set_ylabel("predicted Secchi depth (m), avg. of 3 models")
    ax.set_title("All basins: predicted vs. observed")
    ax.legend(fontsize=9)
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/overview_pred_vs_obs.png", dpi=150)
    plt.close(fig)

    # ---- time series examples: prioritize the two basins this report is about ----
    focus_sites = (all_avg[all_avg["HUC4"].isin(["1701", "1407"])]
                    .groupby("siteSR_id").size().sort_values(ascending=False).head(3).index.tolist())
    other_sites = (all_avg[~all_avg["siteSR_id"].isin(focus_sites)]
                   .groupby("siteSR_id").size().sort_values(ascending=False).head(3).index.tolist())
    sites = focus_sites + other_sites

    fig, axes = plt.subplots(len(sites), 1, figsize=(9, 2.6 * len(sites)))
    for ax, site in zip(axes, sites):
        sub = all_avg[all_avg["siteSR_id"] == site].sort_values("date")
        huc4 = sub["HUC4"].iloc[0]
        tag = "  [HUC4 1701 - forested cluster]" if huc4 == "1701" else "  [HUC4 1407 - Lake Powell]" if huc4 == "1407" else ""
        ax.plot(sub["date"], sub["y"], "o-", color="black", label="observed", markersize=4)
        ax.plot(sub["date"], sub["pred"], "o--", color="#e06666", label="predicted (avg. of 3 models)",
                markersize=3, alpha=0.85)
        ax.set_title(f"site {site} (HUC4 {huc4}, n={len(sub)}){tag}", fontsize=9)
        ax.set_ylabel("Secchi (m)")
    axes[0].legend(fontsize=8, loc="best")
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/overview_timeseries_examples.png", dpi=150)
    plt.close(fig)

    print("wrote overview_pred_vs_obs.png and overview_timeseries_examples.png to", FIG_DIR)


if __name__ == "__main__":
    main()
