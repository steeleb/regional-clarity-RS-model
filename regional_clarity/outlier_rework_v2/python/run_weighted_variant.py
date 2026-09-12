"""Retrain one model with SDD-weighted training (reusing that model's
already-tuned hyperparameters - no re-tuning, matching 04_make_models.Rmd's
approach of reusing the unweighted model's tuned params for the weighted
retrain) and compare top-quartile-of-SDD vs. rest performance against the
unweighted baseline. Run as its own process for the same OpenMP-isolation
reason as run_single_model.py.
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


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("model", choices=["xgboost", "lightgbm", "nn"])
    parser.add_argument("--weight-k", type=float, default=2.0)
    args = parser.parse_args()

    if args.model == "xgboost":
        import model_xgb as mod
    elif args.model == "lightgbm":
        import model_lgb as mod
    else:
        import model_nn as mod

    with open(f"{OUT_DIR}/model_result_{args.model}.json") as fh:
        prior = json.load(fh)
    best_params = prior["best_params"]
    if args.model == "nn":
        best_params["hidden_sizes"] = tuple(best_params["hidden_sizes"])
    feats = prior["features"]
    log(f"reusing tuned params for {args.model}: {best_params}")

    df = cv.load("data/modeling_dataset.parquet")
    df = features.add_spectral_indices(df)
    train_val, test = cv.train_val_test_split(df)
    folds = cv.build_folds(train_val)

    # top-quartile cutoff from train_val's own distribution (never touches test)
    top_quartile_cut = float(np.quantile(train_val[cv.TARGET], 0.75))
    log(f"top-quartile SDD cutoff (from train_val): {top_quartile_cut:.3f} m")

    weight_fn = make_sdd_weight_fn(k=args.weight_k)

    log(f"training unweighted {args.model} fold ensemble (for this comparison's baseline)")
    unweighted_models = mod.train_fold_models(folds, feats, cv.TARGET, best_params)
    unweighted_pred = mod.predict_ensemble(unweighted_models, test[feats])

    log(f"training SDD-weighted {args.model} fold ensemble (k={args.weight_k})")
    weighted_models = mod.train_fold_models(folds, feats, cv.TARGET, best_params, weight_fn=weight_fn)
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
    log(f"{args.model} weighted-vs-unweighted comparison:\n{compare_df.to_string(index=False)}")

    out = {
        "model": args.model, "weight_k": args.weight_k,
        "top_quartile_cut": top_quartile_cut,
        "comparison": rows,
        "unweighted_test_metrics": M.all_metrics(test_y, unweighted_pred),
        "weighted_test_metrics": M.all_metrics(test_y, weighted_pred),
    }
    with open(f"{OUT_DIR}/weighted_comparison_{args.model}.json", "w") as fh:
        json.dump(out, fh, indent=2, default=str)

    pred_df = test[["siteSR_id", "date", "HUC4", "mission", cv.TARGET]].copy()
    pred_df["model"] = args.model
    pred_df["pred_unweighted"] = unweighted_pred
    pred_df["pred_weighted"] = weighted_pred
    pred_df.to_parquet(f"{OUT_DIR}/weighted_predictions_{args.model}.parquet")

    log(f"{args.model} weighted comparison done")


if __name__ == "__main__":
    main()
