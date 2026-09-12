"""Two things, both evaluated on the one fixed holdout every ensemble
member shares:

  1. The v3 ensemble itself: average predictions across all 5 seeds x 2
     models x 4 folds (40 models total) - each seed's own independently
     selected features/hyperparameters, as actually produced by
     02_run_seed_pipeline.py.

  2. A controlled baseline: v1's FROZEN intersection-16 feature set and
     FROZEN final hyperparameters (no shore_flag, no re-selection, no
     re-tuning) retrained on the same 5 seeds' CV-fold arrangements and
     ensembled the same way. This isolates the effect of the new feature
     selection/hyperparameters from the effect of just moving to the new
     split design - v1's original published test RMSE was measured on a
     different, harder test set (dominated by Lake Powell/HUC4 1701), so
     it can't be compared to the v3 holdout number directly; this
     baseline can.
"""
import json
import sys
import time
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd

REWORK_DIR = Path(__file__).resolve().parents[2] / "outlier_rework" / "python"
sys.path.insert(0, str(REWORK_DIR))
import metrics as M  # noqa: E402
import model_lgb  # noqa: E402
import model_xgb  # noqa: E402
from weighting import make_sdd_weight_fn  # noqa: E402

DATA_PATH = Path(__file__).resolve().parents[1] / "data" / "base_with_splits.parquet"
RESULTS_ROOT = Path(__file__).resolve().parents[1] / "results"
TARGET = "harmonized_value"
MODELS = {"xgboost": model_xgb, "lightgbm": model_lgb}
ENSEMBLE_SEEDS = [501, 502, 503, 504, 505]

V1_INTERSECTION16 = ["BG", "MNDWI", "NDSSI", "NDVI", "NDWI", "NR", "atm_corr_LaSRC",
                      "catchment_area_sqkm", "fai", "nir_corr7", "pct_cropland_2006",
                      "pct_forest_2006", "pct_urban_2006", "pct_wetland_2006",
                      "red_corr7", "temp_corr7"]


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


def load_v1_params(model_name):
    with open(REWORK_DIR / "results" / f"final_tune_{model_name}.json") as fh:
        return json.load(fh)["best_params"]


def main():
    df = pd.read_parquet(DATA_PATH)
    test = df[df["is_holdout"]].reset_index(drop=True)

    # --- 1. v3 ensemble: read back each seed's own holdout predictions ---
    v3_rows = []
    for seed in ENSEMBLE_SEEDS:
        v3_rows.append(pd.read_parquet(RESULTS_ROOT / f"seed{seed}" / "holdout_predictions.parquet"))
    v3_all = pd.concat(v3_rows, ignore_index=True)
    v3_all.to_parquet(RESULTS_ROOT / "v3_ensemble_all_predictions.parquet")

    v3_ensemble_pred = v3_all.groupby(["siteSR_id", "date"]).agg(y=("y", "first"), pred=("pred", "mean")).reset_index()
    v3_ensemble_metrics = M.all_metrics(v3_ensemble_pred["y"], v3_ensemble_pred["pred"])
    log(f"v3 ENSEMBLE (5 seeds x 2 models, {v3_all['seed'].nunique()}x{v3_all['model'].nunique()}=40 models): "
        f"{v3_ensemble_metrics}")

    per_model_v3 = {}
    for m in MODELS:
        sub = v3_all[v3_all["model"] == m].groupby(["siteSR_id", "date"]).agg(
            y=("y", "first"), pred=("pred", "mean")).reset_index()
        per_model_v3[m] = M.all_metrics(sub["y"], sub["pred"])
        log(f"v3 {m}-only ensemble (5 seeds): {per_model_v3[m]}")

    # --- 2. frozen v1 baseline on the same 5 CV-fold arrangements ---
    weight_fn = make_sdd_weight_fn(k=2.0)
    v1_params = {m: load_v1_params(m) for m in MODELS}
    v1_rows = []
    for seed in ENSEMBLE_SEEDS:
        folds = build_folds(df, seed)
        for model_name, mod in MODELS.items():
            fold_models = mod.train_fold_models(folds, V1_INTERSECTION16, TARGET, v1_params[model_name],
                                                weight_fn=weight_fn)
            pred = mod.predict_ensemble(fold_models, test[V1_INTERSECTION16])
            r = test[["siteSR_id", "date", "HUC4"]].copy()
            r["seed"] = seed
            r["model"] = model_name
            r["y"] = test[TARGET].values
            r["pred"] = pred
            v1_rows.append(r)
            m_ = M.all_metrics(r["y"], r["pred"])
            log(f"v1-frozen-config seed{seed}/{model_name}: test_rmse={m_['rmse']:.4f}")

    v1_all = pd.concat(v1_rows, ignore_index=True)
    v1_all.to_parquet(RESULTS_ROOT / "v1_frozen_baseline_all_predictions.parquet")
    v1_ensemble_pred = v1_all.groupby(["siteSR_id", "date"]).agg(y=("y", "first"), pred=("pred", "mean")).reset_index()
    v1_ensemble_metrics = M.all_metrics(v1_ensemble_pred["y"], v1_ensemble_pred["pred"])
    log(f"v1-FROZEN-CONFIG ENSEMBLE (same 5 splits, old features/hyperparams): {v1_ensemble_metrics}")

    summary = dict(v3_ensemble=v3_ensemble_metrics, v3_per_model=per_model_v3,
                    v1_frozen_baseline_ensemble=v1_ensemble_metrics)
    with open(RESULTS_ROOT / "ensemble_comparison_summary.json", "w") as fh:
        json.dump(summary, fh, indent=2, default=str)
    log(f"wrote {RESULTS_ROOT / 'ensemble_comparison_summary.json'}")


if __name__ == "__main__":
    main()
