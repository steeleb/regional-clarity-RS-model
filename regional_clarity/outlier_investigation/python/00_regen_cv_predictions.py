"""Regenerate row-level out-of-fold CV predictions using the exact final
production config (intersection-16 features, gap-aware-tuned hyperparameters,
SDD-weighting per model) from run_final_evaluation.py in outlier_rework -
but keeping siteSR_id/HUC4/date per row instead of only the aggregate RMSE
that final_fold_diagnostics_*.csv reports. This is what the outlier-HUC
investigation needs: which individual sites/observations in partition 3
(worst CV gap) and partition 5 (test) actually drive the error.

Reads models/config from outlier_rework (read-only); writes only into
outlier_investigation. No files in outlier_rework are modified.

Run once per model (own process each - xgboost/lightgbm's OpenMP runtime
and PyTorch's threading deadlock if loaded together in one process, same
reason the original pipeline scripts split model types across processes):
    python 00_regen_cv_predictions.py xgboost
    python 00_regen_cv_predictions.py lightgbm
    python 00_regen_cv_predictions.py nn
"""
import argparse
import json
import sys
import time
from pathlib import Path

import pandas as pd

REWORK_DIR = Path(__file__).resolve().parents[2] / "outlier_rework" / "python"
sys.path.insert(0, str(REWORK_DIR))

import features  # noqa: E402
import metrics as M  # noqa: E402
import spatial_cv as cv  # noqa: E402
from weighting import make_sdd_weight_fn  # noqa: E402

DATA_PATH = REWORK_DIR / "data" / "modeling_dataset_expanded.parquet"
OUT_DIR = Path(__file__).resolve().parents[1] / "data"

INTERSECTION16 = ["BG", "MNDWI", "NDSSI", "NDVI", "NDWI", "NR", "atm_corr_LaSRC",
                   "catchment_area_sqkm", "fai", "nir_corr7", "pct_cropland_2006",
                   "pct_forest_2006", "pct_urban_2006", "pct_wetland_2006",
                   "red_corr7", "temp_corr7"]

USE_WEIGHTING = {"xgboost": True, "lightgbm": True, "nn": False}


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def load_final_params(model_name):
    with open(REWORK_DIR / "results" / f"final_tune_{model_name}.json") as fh:
        params = json.load(fh)["best_params"]
    if model_name == "nn":
        params["hidden_sizes"] = tuple(params["hidden_sizes"])
    return params


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("model", choices=["xgboost", "lightgbm", "nn"])
    name = ap.parse_args().model

    if name == "xgboost":
        import model_xgb as mod
    elif name == "lightgbm":
        import model_lgb as mod
    else:
        import model_nn as mod

    df = pd.read_parquet(DATA_PATH)
    df = features.add_spectral_indices(df)
    train_val, test = cv.train_val_test_split(df)
    folds = cv.build_folds(train_val)
    weight_fn = make_sdd_weight_fn(k=2.0)

    params = load_final_params(name)
    wfn = weight_fn if USE_WEIGHTING[name] else None
    log(f"=== {name}: regenerating row-level OOF CV predictions ===")

    fold_models = mod.train_fold_models(folds, INTERSECTION16, cv.TARGET, params, weight_fn=wfn)

    rows = []
    for f, model in zip(folds, fold_models):
        pred = mod.predict_ensemble([model], f.val[INTERSECTION16])
        r = f.val[["siteSR_id", "date", "HUC4", "part"]].copy()
        r["model"] = name
        r["y"] = f.val[cv.TARGET].values
        r["pred"] = pred
        rows.append(r)

    oof = pd.concat(rows, ignore_index=True)
    check_rmse = M.rmse(oof["y"], oof["pred"])
    log(f"{name} overall OOF RMSE (sanity check): {check_rmse:.4f}")

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    oof.to_parquet(OUT_DIR / f"cv_oof_predictions_{name}.parquet")
    log(f"wrote {len(oof)} rows to cv_oof_predictions_{name}.parquet")
    log("done")


if __name__ == "__main__":
    main()
