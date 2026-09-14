"""SHAP figures for all three ls8_harmonization arms - ported from
outlier_rework_v3/python/13_make_shap_figures.py. Produces a per-arm
aggregate mean(|SHAP|) bar chart for all three (comparable at a glance),
plus the fuller HUC4-heatmap and beeswarm treatment for ls8ref only (the
recommended configuration), matching how the rest of this report already
treats ls8ref as primary."""
from pathlib import Path
import matplotlib.pyplot as plt
from matplotlib.patches import Patch
import numpy as np
import pandas as pd
import shap

ROOT = Path("/Users/steeleb/Documents/GitHub/regional-clarity-RS-model/regional_clarity/ls8_harmonization")
FIG_DIR = ROOT / "figures"
RESULTS = ROOT / "results"
ARMS = ["ls7ref", "ls8ref", "l89only"]

plt.rcParams.update({"font.family": "sans-serif", "font.size": 10,
                      "axes.edgecolor": "#D7E3E6", "axes.labelcolor": "#16262E",
                      "text.color": "#16262E", "xtick.color": "#55707A", "ytick.color": "#55707A"})
ACCENT = "#1B6E8C"
GOOD = "#3E7D53"

SITE_COLS = {"elevation_m", "catchment_area_sqkm", "pct_impervious_2006",
             "pct_urban_2006", "pct_forest_2006", "pct_cropland_2006",
             "pct_wetland_2006", "shore_flag"}
WEATHER_PREFIXES = ("precip_mm_prev", "tmax_degC_prev", "tmean_degC_prev",
                    "tmin_degC_prev", "srad_Wm2_prev")


def categorize(feat):
    if feat in SITE_COLS:
        return "site"
    if feat.startswith(WEATHER_PREFIXES):
        return "weather"
    return "optical"


CAT_COLOR = {"optical": ACCENT, "site": GOOD, "weather": "#B08D2A"}

# ============ per-arm aggregate mean(|SHAP|) bar charts ============
cat_shares = {}
for arm in ARMS:
    long_df = pd.read_parquet(RESULTS / arm / "shap_values_long.parquet")
    ens_mean = pd.read_parquet(RESULTS / arm / "shap_ensemble_mean.parquet")
    feats = list(ens_mean.columns)
    mean_abs = long_df[feats].abs().mean().sort_values(ascending=False)
    cats = [categorize(f) for f in mean_abs.index]
    colors = [CAT_COLOR[c] for c in cats]

    fig, ax = plt.subplots(figsize=(7, 5.5))
    y = np.arange(len(mean_abs))
    ax.barh(y, mean_abs.values[::-1], color=colors[::-1])
    ax.set_yticks(y); ax.set_yticklabels(mean_abs.index[::-1], family="monospace", fontsize=8.5)
    ax.set_xlabel("mean |SHAP value| (m)")
    handles = [Patch(color=CAT_COLOR[c], label=c) for c in ["optical", "site", "weather"]]
    ax.legend(handles=handles, fontsize=9, loc="lower right")
    ax.set_title(f"{arm}: SHAP attribution ({len(feats)}-feature unanimous core)", fontsize=10)
    ax.spines[["top", "right"]].set_visible(False)
    fig.tight_layout()
    fig.savefig(FIG_DIR / f"{arm}_shap_aggregate.png", dpi=180, facecolor="white")
    plt.close(fig)

    cat_total = {c: mean_abs[[f for f in feats if categorize(f) == c]].sum() for c in ["optical", "site", "weather"]}
    total = sum(cat_total.values())
    cat_shares[arm] = {k: round(v / total * 100, 1) for k, v in cat_total.items()}
    print(f"{arm}: wrote {arm}_shap_aggregate.png; category share: {cat_shares[arm]}")

# ============ ls8ref only: HUC4 heatmap + beeswarm (recommended arm, fuller treatment) ============
arm = "ls8ref"
long_df = pd.read_parquet(RESULTS / arm / "shap_values_long.parquet")
ens_mean = pd.read_parquet(RESULTS / arm / "shap_ensemble_mean.parquet")
holdout_feats = pd.read_parquet(RESULTS / arm / "shap_holdout_features.parquet")
feats = list(ens_mean.columns)
mean_abs = long_df[feats].abs().mean().sort_values(ascending=False)

huc4_counts = long_df.drop_duplicates("_row")["HUC4"].value_counts()
top_huc4 = huc4_counts[huc4_counts >= 30].index.tolist()
for h in ["1701", "1407"]:
    if h not in top_huc4 and h in huc4_counts.index:
        top_huc4.append(h)
top_huc4 = sorted(set(top_huc4), key=lambda h: -huc4_counts.get(h, 0))

feat_order = mean_abs.index.tolist()
grid = np.zeros((len(feat_order), len(top_huc4)))
for j, h in enumerate(top_huc4):
    sub = long_df[long_df["HUC4"] == h]
    grid[:, j] = sub[feat_order].abs().mean().values

fig, ax = plt.subplots(figsize=(9, 6))
im = ax.imshow(grid, aspect="auto", cmap="YlGnBu")
ax.set_yticks(range(len(feat_order))); ax.set_yticklabels(feat_order, family="monospace", fontsize=8.5)
ax.set_xticks(range(len(top_huc4)))
labels = [f"{h}{' *' if h in ('1701', '1407') else ''}\n(n={huc4_counts.get(h, 0)})" for h in top_huc4]
ax.set_xticklabels(labels, fontsize=7.5)
cbar = fig.colorbar(im, ax=ax, fraction=0.03, pad=0.02)
cbar.set_label("mean |SHAP value| (m)", fontsize=9)
ax.set_title("ls8ref: SHAP attribution by HUC4", fontsize=11)
fig.tight_layout()
fig.savefig(FIG_DIR / "ls8ref_shap_by_huc4.png", dpi=180, facecolor="white")
plt.close(fig)
print("wrote ls8ref_shap_by_huc4.png; HUC4s shown:", top_huc4)

X = holdout_feats[feats].reset_index(drop=True)
sv = ens_mean[feats].reset_index(drop=True)
plt.figure(figsize=(8, 6))
shap.summary_plot(sv.values, X, feature_names=feats, show=False, plot_size=None)
fig = plt.gcf()
fig.set_size_inches(8, 6)
fig.tight_layout()
fig.savefig(FIG_DIR / "ls8ref_shap_beeswarm.png", dpi=180, facecolor="white", bbox_inches="tight")
plt.close(fig)
print("wrote ls8ref_shap_beeswarm.png")

print("category shares by arm:", cat_shares)
