"""SHAP attribution for all three ls8_harmonization arms (ls7ref, ls8ref,
l89only) - ported from outlier_rework_v3/python/12_run_shap_analysis.py,
same design (each seed's own already-selected features/gap-aware-tuned
hyperparameters reused unchanged, trained unweighted on that seed's own 4
CV folds, explained with shap.TreeExplainer on the shared fixed holdout),
looped over arms since each arm has its own reference/mission-filter and
its own per-seed feature sets."""
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

sys.path.insert(0, str(Path(__file__).resolve().parent))
import features_ls8 as features  # noqa: E402

DATA_PATH = Path(__file__).resolve().parents[1] / "data" / "base_with_splits.parquet"
RESULTS_ROOT = Path(__file__).resolve().parents[1] / "results"
TARGET = "harmonized_value"
MODELS = {"xgboost": model_xgb, "lightgbm": model_lgb}
ENSEMBLE_SEEDS = [501, 502, 503, 504, 505]

ARMS = {
    "ls7ref": dict(reference="corr7", mission_filter="all"),
    "ls8ref": dict(reference="corr8", mission_filter="all"),
    "l89only": dict(reference="corr8", mission_filter="l8_only"),
}

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


def load_seed_config(results_dir, seed, model_name):
    with open(results_dir / f"seed{seed}" / "backward_elim_summary.json") as fh:
        feats = json.load(fh)["intersection_of_per_model"]
    with open(results_dir / f"seed{seed}" / f"final_tune_{model_name}.json") as fh:
        params = json.load(fh)["best_params"]
    return feats, params


def shap_for_model(model_name, model, holdout, feats):
    explainer = shap.TreeExplainer(model)
    if model_name == "xgboost":
        sv = explainer.shap_values(xgb.DMatrix(holdout[feats]))
    else:
        sv = explainer.shap_values(holdout[feats])
    return np.asarray(sv)


def run_arm(arm_name, reference, mission_filter):
    results_dir = RESULTS_ROOT / arm_name
    df = pd.read_parquet(DATA_PATH)
    if mission_filter == "l8_only":
        df = df[df["mission"].isin(["LC08", "LC09"])].reset_index(drop=True)
    df = features.add_spectral_indices(df, reference)
    holdout = df[df["is_holdout"]].reset_index(drop=True)
    n = len(holdout)

    per_seed_model = {}
    for seed in ENSEMBLE_SEEDS:
        folds = build_folds(df, seed)
        for model_name, mod in MODELS.items():
            feats, params = load_seed_config(results_dir, seed, model_name)
            log(f"{arm_name}/seed{seed}/{model_name}: training {len(folds)} folds + SHAP on {n}-row holdout ({len(feats)} feats)")
            fold_shaps = []
            for f in folds:
                model = mod.train_fold_models([f], feats, TARGET, params)[0]
                fold_shaps.append(shap_for_model(model_name, model, holdout, feats))
            mean_shap = np.mean(fold_shaps, axis=0)
            per_seed_model[(seed, model_name)] = pd.DataFrame(mean_shap, columns=feats)

    unanimous = None
    for feats_df in per_seed_model.values():
        cols = set(feats_df.columns)
        unanimous = cols if unanimous is None else (unanimous & cols)
    unanimous = sorted(unanimous)
    log(f"{arm_name}: unanimous core ({len(unanimous)} features): {unanimous}")

    long_rows = []
    for (seed, model_name), feats_df in per_seed_model.items():
        sub = feats_df[unanimous].copy()
        sub["seed"] = seed
        sub["model"] = model_name
        sub["_row"] = np.arange(n)
        long_rows.append(sub)
    long_df = pd.concat(long_rows, ignore_index=True)
    long_df["HUC4"] = np.tile(holdout["HUC4"].values, len(per_seed_model))

    long_df.to_parquet(results_dir / "shap_values_long.parquet")
    ensemble_mean = long_df.groupby("_row")[unanimous].mean().sort_index()
    ensemble_mean.to_parquet(results_dir / "shap_ensemble_mean.parquet")
    holdout[["siteSR_id", "date", "HUC4"] + unanimous].to_parquet(results_dir / "shap_holdout_features.parquet")

    log(f"{arm_name}: wrote shap_values_long.parquet ({len(long_df)} rows), "
        f"shap_ensemble_mean.parquet ({len(ensemble_mean)} rows)")
    return unanimous


def main():
    for arm_name, cfg in ARMS.items():
        run_arm(arm_name, **cfg)
    log("DONE")


if __name__ == "__main__":
    main()
