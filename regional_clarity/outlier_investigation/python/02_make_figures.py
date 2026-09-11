"""Figures for the outlier-HUC investigation report. Reads the row-level CV
OOF predictions (regenerated with the final production config) and the
final test predictions from outlier_rework (read-only); writes only into
outlier_investigation."""
import os

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

DATA_DIR = "../data"
REWORK_RESULTS = "../../outlier_rework/python/results"
FIG_DIR = "../figures"

MODEL_COLORS = {"xgboost": "#1b9e77", "lightgbm": "#d95f02", "nn": "#7570b3"}
SDD_BINS = [0, 2, 4, 6, 10, 20]
SDD_LABELS = ["0-2", "2-4", "4-6", "6-10", "10-20"]


def load_oof():
    return pd.concat(
        [pd.read_parquet(f"{DATA_DIR}/cv_oof_predictions_{m}.parquet") for m in MODEL_COLORS],
        ignore_index=True,
    )


def load_test():
    frames = []
    for m in MODEL_COLORS:
        t = pd.read_parquet(f"{REWORK_RESULTS}/final_test_predictions_{m}.parquet")
        t["model"] = m
        t = t.rename(columns={"harmonized_value": "y"})
        frames.append(t)
    return pd.concat(frames, ignore_index=True)


def bin_err(df):
    df = df.copy()
    df["err"] = df["pred"] - df["y"]
    df["sdd_bin"] = pd.cut(df["y"], bins=SDD_BINS, labels=SDD_LABELS)
    return df


def fig_compression(oof, test):
    """Universal compression-toward-the-mean bias: mean (pred - obs) by
    observed-SDD bin, comparing the outlier partition/basin against the
    rest of its own split."""
    oof = bin_err(oof)
    test = bin_err(test)

    p3 = oof[oof["part"] == 3]
    rest_cv = oof[oof["part"] != 3]
    huc1407 = test[test["HUC4"].astype(str) == "1407"]
    rest_test = test[test["HUC4"].astype(str) != "1407"]

    fig, axes = plt.subplots(1, 2, figsize=(11, 4.3), sharey=True)

    def plot_panel(ax, outlier, rest, outlier_label, rest_label, title):
        x = np.arange(len(SDD_LABELS))
        w = 0.36
        o_means = outlier.groupby("sdd_bin", observed=True)["err"].mean().reindex(SDD_LABELS)
        r_means = rest.groupby("sdd_bin", observed=True)["err"].mean().reindex(SDD_LABELS)
        ax.bar(x - w / 2, r_means, width=w, color="#9AB0B8", label=rest_label)
        ax.bar(x + w / 2, o_means, width=w, color="#C1442D", label=outlier_label)
        ax.axhline(0, color="black", lw=0.8)
        ax.set_xticks(x)
        ax.set_xticklabels([f"{l} m" for l in SDD_LABELS])
        ax.set_xlabel("observed Secchi depth bin")
        ax.set_title(title)
        ax.legend(fontsize=8)

    plot_panel(axes[0], p3, rest_cv, "partition 3 (held out)", "partitions 1,2,4",
               "CV: partition 3 vs. the rest")
    plot_panel(axes[1], huc1407, rest_test, "HUC4 1407 (Lake Powell)", "rest of partition 5",
               "Test: HUC4 1407 vs. the rest")
    axes[0].set_ylabel("mean error, pred − obs (m)\npooled across xgboost/lightgbm/nn")
    fig.suptitle("The same high-clarity compression bias, concentrated in the outlier basins", y=1.02)
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/compression_bias.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def fig_partition_composition():
    df = pd.read_parquet(f"{DATA_DIR}/modeling_dataset_with_wbtype.parquet")
    q90 = df["harmonized_value"].quantile(0.9)
    fq75 = df["pct_forest_2006"].quantile(0.75)

    row_share = df["part"].value_counts(normalize=True).sort_index() * 100
    clarity_share = df[df["harmonized_value"] >= q90]["part"].value_counts(normalize=True).sort_index() * 100
    forest_share = df[df["pct_forest_2006"] >= fq75]["part"].value_counts(normalize=True).sort_index() * 100

    parts = sorted(df["part"].unique())
    x = np.arange(len(parts))
    w = 0.26
    fig, ax = plt.subplots(figsize=(7, 4.3))
    ax.bar(x - w, row_share.reindex(parts), width=w, label="share of all rows", color="#9AB0B8")
    ax.bar(x, clarity_share.reindex(parts).fillna(0), width=w,
           label=f"share of top-decile clarity rows (>{q90:.1f} m)", color="#1B6E8C")
    ax.bar(x + w, forest_share.reindex(parts).fillna(0), width=w,
           label=f"share of high-forest-catchment rows (>{fq75:.0f}% forest)", color="#3E7D53")
    ax.set_xticks(x)
    ax.set_xticklabels([f"partition {p}" for p in parts])
    ax.set_ylabel("% of rows in category")
    ax.set_title("Partition 3 concentrates the region's clearest, most-forested lakes")
    ax.legend(fontsize=8)
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/partition_composition.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def fig_huc4_test_rmse(test):
    test = test.copy()
    test["err"] = test["pred"] - test["y"]
    n_per_huc4 = test[test["model"] == "xgboost"].groupby("HUC4").size()
    g = test.groupby("HUC4").agg(rmse=("err", lambda s: np.sqrt((s ** 2).mean())))
    g["n"] = n_per_huc4
    names = pd.read_csv(f"{DATA_DIR}/huc4_names.csv").rename(columns={"huc4": "HUC4"})
    names["HUC4"] = names["HUC4"].astype(str)
    g = g.reset_index()
    g["HUC4"] = g["HUC4"].astype(str)
    g = g.merge(names, on="HUC4", how="left")
    g["label"] = g["HUC4"] + " " + g["name"].fillna("")
    g = g.sort_values("rmse", ascending=True)

    fig, ax = plt.subplots(figsize=(7.5, 4.5))
    colors = ["#C1442D" if h == "1407" else "#57B8D6" for h in g["HUC4"]]
    bars = ax.barh(g["label"], g["rmse"], color=colors)
    for bar, n in zip(bars, g["n"]):
        ax.text(bar.get_width() + 0.03, bar.get_y() + bar.get_height() / 2, f"n={n}",
                va="center", fontsize=7.5, color="#55707A")
    ax.set_xlabel("test RMSE (m), pooled across xgboost/lightgbm/nn")
    ax.set_title("Where partition 5's test error concentrates")
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/huc4_test_rmse.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def fig_literature_context(oof, test):
    """Error inside vs. outside the ~6 m range where established national
    Landsat-SDD products (e.g. Topp et al.) report skill, alongside their
    published accuracy for scale."""
    def stats(df, col):
        e_all = df["pred"] - df[col]
        le6 = df[df[col] <= 6]
        gt6 = df[df[col] > 6]
        e_le6 = le6["pred"] - le6[col]
        e_gt6 = gt6["pred"] - gt6[col]
        rmse = lambda e: np.sqrt((e ** 2).mean())
        mae = lambda e: e.abs().mean()
        return {
            "overall": (rmse(e_all), mae(e_all), len(df)),
            "<=6 m": (rmse(e_le6), mae(e_le6), len(le6)),
            ">6 m": (rmse(e_gt6), mae(e_gt6), len(gt6)),
        }

    cv_stats = stats(oof, "y")
    test_stats = stats(test, "y")

    groups = ["overall", "<=6 m", ">6 m"]
    fig, axes = plt.subplots(1, 2, figsize=(10, 4.3), sharey=True)
    for ax, (label, s) in zip(axes, [("CV (partitions 1-4)", cv_stats), ("Test (partition 5)", test_stats)]):
        x = np.arange(len(groups))
        w = 0.32
        rmse_vals = [s[g][0] for g in groups]
        mae_vals = [s[g][1] for g in groups]
        ax.bar(x - w / 2, rmse_vals, width=w, color="#1B6E8C", label="RMSE")
        ax.bar(x + w / 2, mae_vals, width=w, color="#57B8D6", label="MAE")
        ax.axhline(1.0, color="#3E7D53", lw=1.2, ls="--")
        ax.axhline(0.6, color="#3E7D53", lw=1.2, ls=":")
        for i, g in enumerate(groups):
            ax.text(i, max(rmse_vals[i], mae_vals[i]) + 0.08, f"n={s[g][2]}", ha="center", fontsize=7.5,
                     color="#55707A")
        ax.set_xticks(x)
        ax.set_xticklabels(groups)
        ax.set_title(label)
        ax.legend(fontsize=8, loc="upper left")
    axes[0].set_ylabel("error (m)")
    fig.suptitle("This pipeline vs. Topp et al.'s national RMSE≈1.0 m / MAE≈0.6 m (dashed/dotted)", y=1.03)
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/literature_context.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def fig_powell_interannual():
    sc = pd.read_feather("../../outlier_rework/site_characteristics.feather")
    powell_sites = set(sc[sc["wb_nhd_id"] == "3528295"]["siteSR_id"])
    df = pd.read_parquet(f"{DATA_DIR}/modeling_dataset_with_wbtype.parquet")
    powell = df[df["siteSR_id"].isin(powell_sites)].copy()
    powell["year"] = pd.to_datetime(powell["date"]).dt.year

    yearly = powell.groupby("year")["harmonized_value"].agg(["mean", "min", "max", "size"])
    yearly = yearly[yearly["size"] >= 3]  # drop single/double-observation years, too noisy to read

    fig, ax = plt.subplots(figsize=(9, 4))
    ax.vlines(yearly.index, yearly["min"], yearly["max"], color="#9AB0B8", lw=1.5)
    ax.scatter(yearly.index, yearly["mean"], color="#C1442D", s=[n * 3 for n in yearly["size"]], zorder=3,
               label="annual mean (sized by n obs.)")
    ax.set_ylabel("Secchi depth (m)")
    ax.set_xlabel("year")
    ax.set_title("Lake Powell: year-to-year Secchi depth swings are real,\nnot measurement noise")
    ax.legend(fontsize=8)
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/powell_interannual.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def fig_lake_powell(test):
    oof = load_oof()
    sc = pd.read_feather("../../outlier_rework/site_characteristics.feather")
    powell_sites = set(sc[sc["wb_nhd_id"] == "3528295"]["siteSR_id"])

    train_rows = oof[oof["siteSR_id"].isin(powell_sites)].copy()
    train_rows["split"] = "train/CV (part 2, San Juan arm)"
    train_rows = train_rows.rename(columns={"y": "obs"})[["obs", "pred", "split", "model"]]

    test_rows = test[test["siteSR_id"].isin(powell_sites)].copy()
    test_rows["split"] = "test (part 5, main body)"
    test_rows = test_rows.rename(columns={"y": "obs"})[["obs", "pred", "split", "model"]]

    both = pd.concat([train_rows, test_rows], ignore_index=True)
    # average predictions across the 3 models per observation for a clean scatter
    both_avg = both.groupby(["obs", "split"], as_index=False).agg(pred=("pred", "mean"))

    fig, ax = plt.subplots(figsize=(5.5, 5.5))
    for split, color, marker in [("train/CV (part 2, San Juan arm)", "#57B8D6", "o"),
                                   ("test (part 5, main body)", "#C1442D", "o")]:
        sub = both_avg[both_avg["split"] == split]
        ax.scatter(sub["obs"], sub["pred"], alpha=0.55, s=22, color=color, label=f"{split} (n={len(sub)})",
                   marker=marker, edgecolors="none")
    lims = [0, max(both_avg["obs"].max(), both_avg["pred"].max()) * 1.05]
    ax.plot(lims, lims, "k--", lw=1, label="1:1")
    ax.set_xlim(lims); ax.set_ylim(lims)
    ax.set_xlabel("observed Secchi depth (m)")
    ax.set_ylabel("predicted Secchi depth (m), avg. of 3 models")
    ax.set_title("Lake Powell: predictions compress toward ~4-7 m\nregardless of the true value")
    ax.legend(fontsize=8, loc="upper left")
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/lake_powell_pred_vs_obs.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def fig_wbtype_test_error(test):
    sc = pd.read_feather("../../outlier_rework/site_characteristics.feather")
    site_wb = pd.read_parquet(f"{DATA_DIR}/site_level_with_wbtype.parquet")[["siteSR_id", "wb_type"]]
    t = test.merge(site_wb, on="siteSR_id", how="left")
    t["err"] = t["pred"] - t["y"]
    g = t.groupby(["wb_type", "model"]).agg(n=("err", "size"),
                                              rmse=("err", lambda s: np.sqrt((s ** 2).mean()))).reset_index()

    fig, ax = plt.subplots(figsize=(6, 4))
    wbtypes = sorted(g["wb_type"].unique())
    x = np.arange(len(wbtypes))
    w = 0.25
    for i, m in enumerate(MODEL_COLORS):
        vals = g[g["model"] == m].set_index("wb_type")["rmse"].reindex(wbtypes)
        ax.bar(x + (i - 1) * w, vals, width=w, color=MODEL_COLORS[m], label=m)
    ax.set_xticks(x)
    n_by_type = t.drop_duplicates("siteSR_id").groupby("wb_type").size()
    ax.set_xticklabels([f"{wt}\n(n rows={g[g.wb_type==wt]['n'].iloc[0]})" for wt in wbtypes])
    ax.set_ylabel("test RMSE (m)")
    ax.set_title("Test error by waterbody type\n(reservoirs are not the problem)")
    ax.legend(fontsize=8)
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/wbtype_test_error.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def _envelope_panel(ax, train_vals, outlier_val, outlier_label, xlabel, log=False, bins=40):
    """Histogram of a feature's training-fold distribution with the outlier
    basin's value marked, to show whether it sits inside or outside the
    range the model actually had density to learn from."""
    plot_vals = train_vals.clip(lower=0.01) if log else train_vals
    if log:
        bin_edges = np.logspace(np.log10(plot_vals.min()), np.log10(plot_vals.max()), bins)
    else:
        bin_edges = bins
    ax.hist(plot_vals, bins=bin_edges, color="#9AB0B8", edgecolor="white", linewidth=0.3)
    if log:
        ax.set_xscale("log")
    pct = (train_vals < outlier_val).mean() * 100
    pct_int = int(round(pct))
    suffix = "th" if 10 <= pct_int % 100 <= 20 else {1: "st", 2: "nd", 3: "rd"}.get(pct_int % 10, "th")
    ax.axvline(outlier_val, color="#C1442D", lw=2.2,
               label=f"{outlier_label}\n({pct_int}{suffix} pct. of training)")
    ax.set_xlabel(xlabel)
    ax.set_ylabel("training rows")
    ax.legend(fontsize=8, loc="upper right")


def fig_catchment_envelope_powell():
    """Lake Powell's catchment area against the distribution the model was
    actually trained on for the test evaluation (partitions 1-4)."""
    df = pd.read_parquet(f"{DATA_DIR}/modeling_dataset_with_wbtype.parquet")
    train = df[df["part"] != 5]
    powell_area = df.loc[df["HUC4"].astype(str) == "1407", "catchment_area_sqkm"].median()

    fig, ax = plt.subplots(figsize=(6, 4.3))
    _envelope_panel(ax, train["catchment_area_sqkm"], powell_area, "Lake Powell (2,667 km²)",
                     "catchment area (km², log scale)", log=True)
    ax.set_title("Lake Powell's catchment sits in the sparse\nupper tail of the training distribution")
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/catchment_envelope_powell.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def fig_forest_envelope_1701():
    """HUC4 1701's catchment forest cover against the distribution the
    model was actually trained on for the partition-3 CV fold (partitions
    1, 2, 4 - i.e. with partition 3 itself held out)."""
    df = pd.read_parquet(f"{DATA_DIR}/modeling_dataset_with_wbtype.parquet")
    train = df[df["part"].isin([1, 2, 4])]
    huc1701_forest = df.loc[df["HUC4"].astype(str) == "1701", "pct_forest_2006"].median()

    fig, ax = plt.subplots(figsize=(6, 4.3))
    _envelope_panel(ax, train["pct_forest_2006"], huc1701_forest, "HUC4 1701 (54.5% forest)",
                     "catchment forest cover (%)")
    ax.set_title("HUC4 1701's catchment forest cover sits in the sparse\nupper tail of partition 3's training folds")
    fig.tight_layout()
    fig.savefig(f"{FIG_DIR}/forest_envelope_1701.png", dpi=150, bbox_inches="tight")
    plt.close(fig)


def main():
    os.makedirs(FIG_DIR, exist_ok=True)
    oof = load_oof()
    test = load_test()

    fig_compression(oof, test)
    fig_partition_composition()
    fig_huc4_test_rmse(test)
    fig_lake_powell(test)
    fig_wbtype_test_error(test)
    fig_literature_context(oof, test)
    fig_powell_interannual()
    fig_catchment_envelope_powell()
    fig_forest_envelope_1701()
    print("figures written to", FIG_DIR)


if __name__ == "__main__":
    main()
