"""Corrected weighted-vs-unweighted ablation on v1's ACTUAL final,
adopted production config (16-feature intersection set, gap-aware-tuned
hyperparameters from final_tune_*.json) - the existing
weighted_comparison_{model}.json reused model_result_{model}.json, a
13-feature optical-only candidate from an earlier pipeline stage,
predating backward elimination's final feature set (site/weather
features added, catchment/pct_* columns included). This script fixes
that by loading the same feats/params run_final_evaluation.py uses, and
overwrites the same output files so make_weighted_figures.py's
downstream figures pick up the corrected numbers with no other changes
needed.

Run one model per process (own CLI arg), matching this project's own
established convention (see run_single_model.py / the original
run_weighted_variant.py's docstring) - xgboost+lightgbm+nn (torch) loaded
in the same process causes an OpenMP thread-pool deadlock on this
machine, confirmed by the outlier_rework_v2 run of this same fix hanging
indefinitely on the nn model until killed, immediately after xgboost and
lightgbm completed in under a minute total.
"""
import argparse
import json
import time

import numpy as np
import pandas as pd

import features
import metrics as M
import spatial_cv as cv
from weighting import make_sdd_weight_fn

OUT_DIR = "results"
DATA_PATH = "data/modeling_dataset_expanded.parquet"

INTERSECTION16 = ["BG", "MNDWI", "NDSSI", "NDVI", "NDWI", "NR", "atm_corr_LaSRC",
                   "catchment_area_sqkm", "fai", "nir_corr7", "pct_cropland_2006",
                   "pct_forest_2006", "pct_urban_2006", "pct_wetland_2006",
                   "red_corr7", "temp_corr7"]


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def load_final_params(model_name):
    with open(f"{OUT_DIR}/final_tune_{model_name}.json") as fh:
        params = json.load(fh)["best_params"]
    if model_name == "nn":
        params["hidden_sizes"] = tuple(params["hidden_sizes"])
    return params


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("model", choices=["xgboost", "lightgbm", "nn"])
    args = parser.parse_args()
    name = args.model

    if name == "xgboost":
        import model_xgb as mod
    elif name == "lightgbm":
        import model_lgb as mod
    else:
        import model_nn as mod

    feats = INTERSECTION16
    df = pd.read_parquet(DATA_PATH)
    df = features.add_spectral_indices(df)
    train_val, test = cv.train_val_test_split(df)
    folds = cv.build_folds(train_val)
    top_quartile_cut = float(np.quantile(train_val[cv.TARGET], 0.75))
    log(f"final feature set (n={len(feats)}): {feats}")
    log(f"top-quartile SDD cutoff (from train_val): {top_quartile_cut:.3f} m")

    weight_fn = make_sdd_weight_fn(k=2.0)
    params = load_final_params(name)
    log(f"=== {name}: final production feats/params, weighted vs unweighted ===")

    unweighted_models = mod.train_fold_models(folds, feats, cv.TARGET, params)
    unweighted_pred = mod.predict_ensemble(unweighted_models, test[feats])

    weighted_models = mod.train_fold_models(folds, feats, cv.TARGET, params, weight_fn=weight_fn)
    weighted_pred = mod.predict_ensemble(weighted_models, test[feats])

    test_y = test[cv.TARGET].values
    sdd_group = np.where(test_y >= top_quartile_cut, "top quartile", "rest")

    rows = []
    for group in ["top quartile", "rest", "overall"]:
        mask = (sdd_group == group) if group != "overall" else np.ones_like(test_y, dtype=bool)
        rows.append({
            "group": group, "n": int(mask.sum()),
            "rmse_unweighted": M.rmse(test_y[mask], unweighted_pred[mask]),
            "rmse_weighted": M.rmse(test_y[mask], weighted_pred[mask]),
            "bias_unweighted": M.bias(test_y[mask], unweighted_pred[mask]),
            "bias_weighted": M.bias(test_y[mask], weighted_pred[mask]),
        })
    compare_df = pd.DataFrame(rows)
    log(f"{name} weighted-vs-unweighted comparison (final config):\n{compare_df.to_string(index=False)}")

    out = {
        "model": name, "weight_k": 2.0,
        "top_quartile_cut": top_quartile_cut,
        "feature_set": "final_16_intersection",
        "comparison": rows,
        "unweighted_test_metrics": M.all_metrics(test_y, unweighted_pred),
        "weighted_test_metrics": M.all_metrics(test_y, weighted_pred),
    }
    with open(f"{OUT_DIR}/weighted_comparison_{name}.json", "w") as fh:
        json.dump(out, fh, indent=2, default=str)

    pred_df = test[["siteSR_id", "date", "HUC4", "mission", cv.TARGET]].copy()
    pred_df["model"] = name
    pred_df["pred_unweighted"] = unweighted_pred
    pred_df["pred_weighted"] = weighted_pred
    pred_df.to_parquet(f"{OUT_DIR}/weighted_predictions_{name}.parquet")

    log(f"{name}: wrote corrected weighted_comparison_{name}.json")


if __name__ == "__main__":
    main()
