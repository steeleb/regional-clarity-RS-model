"""Corrected weighted-vs-unweighted ablation on v2's ACTUAL final,
adopted production config (16-feature intersection set incl.
shoreline_flag, gap-aware-tuned hyperparameters from final_tune_*.json) -
same comparison run_weighted_variant.py performs, but that script reused
model_result_{model}.json, which is a 13-feature optical-only candidate
from an earlier pipeline stage, predating both shoreline_flag and the
final backward-elimination feature set. That made weighted_comparison_
{model}.json stale/not representative of the actual production model.
This script fixes that by loading the same feats/params
run_final_evaluation.py uses, and overwrites the same output files so
make_weighted_figures.py's downstream figures pick up the corrected
numbers with no other changes needed.

Production for reference (matching run_final_evaluation.py's own
docstring): XGBoost/LightGBM are already SDD-weighted (k=2.0) in
production; NN is not. This script reports the counterfactual for each -
i.e. what NOT weighting would have looked like for xgboost/lightgbm, and
what weighting WOULD look like for the NN - not a proposal to change
anything.
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


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def load_selected_features(key="intersection"):
    with open(f"{OUT_DIR}/selected_features.json") as fh:
        return json.load(fh)[key]


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

    feats = load_selected_features("intersection")
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
        "feature_set": "final_16_intersection_with_shoreline_flag",
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
