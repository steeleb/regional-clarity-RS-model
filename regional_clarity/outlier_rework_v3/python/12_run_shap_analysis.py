"""SHAP attribution for the adopted v3 production configuration - does
feature importance line up with physical expectation, and how much of
the prediction comes from non-remote-sensing (site/weather) variables
vs. optical/spectral ones?

Each seed's own final feature set and gap-aware-tuned hyperparameters are
reused unchanged (no re-elimination, no re-tuning), trained unweighted
(the adopted default) on that seed's own 4 CV folds, and explained with
shap.TreeExplainer on the one fixed holdout every seed shares - so a
feature's SHAP value is always computed by a model that never trained on
that row, and every seed/model explains the exact same rows, making
cross-seed and cross-HUC4 comparison direct.

Because seeds select slightly different feature sets (18-22 features,
see the feature-selection section of the report), the cross-seed
aggregate and per-HUC4 stability figures are restricted to the 18
features unanimous across all 5 seeds x 2 models - the actual
recommended production core - so every one of the 10 (seed, model)
SHAP explanations contributes to every bar/cell shown. Boundary features
outside that core are noted in text only.
"""
import json
import sys
import time
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd
import shap
import xgboost as xgb

REWORK_DIR = Path(__file__).resolve().parents[2] / "outlier_rework" / "python"
sys.path.insert(0, str(REWORK_DIR))
import model_lgb  # noqa: E402
import model_xgb  # noqa: E402

DATA_PATH = Path(__file__).resolve().parents[1] / "data" / "base_with_splits.parquet"
RESULTS_ROOT = Path(__file__).resolve().parents[1] / "results"
TARGET = "harmonized_value"
MODELS = {"xgboost": model_xgb, "lightgbm": model_lgb}
ENSEMBLE_SEEDS = [501, 502, 503, 504, 505]

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


@dataclass
class Fold:
    part: int
    train: pd.DataFrame
    val: pd.DataFrame


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def build_folds(df, seed):
    col = f"cvfold_seed{seed}"
    cv_pool = df[~df["is_holdout"]]
    parts = sorted(cv_pool[col].dropna().unique())
    return [Fold(part=int(p),
                  train=cv_pool[cv_pool[col] != p].reset_index(drop=True),
                  val=cv_pool[cv_pool[col] == p].reset_index(drop=True))
            for p in parts]


def load_seed_config(seed, model_name):
    with open(RESULTS_ROOT / f"seed{seed}" / "backward_elim_summary.json") as fh:
        feats = json.load(fh)["intersection_of_per_model"]
    with open(RESULTS_ROOT / f"seed{seed}" / f"final_tune_{model_name}.json") as fh:
        params = json.load(fh)["best_params"]
    return feats, params


def shap_for_model(model_name, model, holdout, feats):
    explainer = shap.TreeExplainer(model)
    if model_name == "xgboost":
        sv = explainer.shap_values(xgb.DMatrix(holdout[feats]))
    else:
        sv = explainer.shap_values(holdout[feats])
    return np.asarray(sv)


def main():
    df = pd.read_parquet(DATA_PATH)
    holdout = df[df["is_holdout"]].reset_index(drop=True)
    n = len(holdout)

    # per (seed, model): mean-over-4-folds SHAP array, aligned to holdout rows,
    # in that seed/model's own feature order
    per_seed_model = {}
    for seed in ENSEMBLE_SEEDS:
        folds = build_folds(df, seed)
        for model_name, mod in MODELS.items():
            feats, params = load_seed_config(seed, model_name)
            log(f"seed{seed}/{model_name}: training 4 folds + SHAP on {n}-row holdout ({len(feats)} feats)")
            fold_shaps = []
            for f in folds:
                model = mod.train_fold_models([f], feats, TARGET, params)[0]
                fold_shaps.append(shap_for_model(model_name, model, holdout, feats))
            mean_shap = np.mean(fold_shaps, axis=0)  # (n_holdout, n_feats)
            per_seed_model[(seed, model_name)] = pd.DataFrame(mean_shap, columns=feats)

    # unanimous core: present in every seed/model's own feature list
    unanimous = None
    for feats_df in per_seed_model.values():
        cols = set(feats_df.columns)
        unanimous = cols if unanimous is None else (unanimous & cols)
    unanimous = sorted(unanimous)
    log(f"unanimous core ({len(unanimous)} features): {unanimous}")

    # long-format: one row per (seed, model, holdout_row), unanimous features only
    long_rows = []
    for (seed, model_name), feats_df in per_seed_model.items():
        sub = feats_df[unanimous].copy()
        sub["seed"] = seed
        sub["model"] = model_name
        sub["_row"] = np.arange(n)
        long_rows.append(sub)
    long_df = pd.concat(long_rows, ignore_index=True)
    long_df["HUC4"] = np.tile(holdout["HUC4"].values, len(per_seed_model))

    OUT_DIR = RESULTS_ROOT
    long_df.to_parquet(OUT_DIR / "shap_values_long.parquet")

    # ensemble-mean SHAP per row (average across all 10 seed/model combos) -
    # for the beeswarm-style aggregate plot, paired with the holdout's own
    # feature values (identical regardless of which model explains them)
    ensemble_mean = long_df.groupby("_row")[unanimous].mean().sort_index()
    ensemble_mean.to_parquet(OUT_DIR / "shap_ensemble_mean.parquet")
    holdout[["siteSR_id", "date", "HUC4"] + unanimous].to_parquet(OUT_DIR / "shap_holdout_features.parquet")

    log(f"wrote shap_values_long.parquet ({len(long_df)} rows), "
        f"shap_ensemble_mean.parquet ({len(ensemble_mean)} rows)")
    log("DONE")


if __name__ == "__main__":
    main()
