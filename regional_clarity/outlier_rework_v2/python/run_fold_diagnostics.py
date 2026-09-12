"""Per-fold overfitting diagnostics: for each model, reuse its already-
tuned hyperparameters, retrain each of the 4 CV folds individually, and
report train RMSE, val RMSE, and the gap between them per fold - not just
the pooled out-of-fold metric reported in run_single_model.py. A model
that's overfitting shows a large, consistent train-val gap; a model that's
just hitting a spatially harder partition shows fold-to-fold RMSE variance
without a large gap on any individual fold.

Run as its own process per model, same OpenMP-isolation reason as
run_single_model.py / run_weighted_variant.py.
"""
import argparse
import json
import time

import numpy as np
import pandas as pd

import features
import metrics as M
import spatial_cv as cv

OUT_DIR = "results"


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("model", choices=["xgboost", "lightgbm", "nn"])
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

    df = cv.load("data/modeling_dataset.parquet")
    df = features.add_spectral_indices(df)
    train_val, test = cv.train_val_test_split(df)
    folds = cv.build_folds(train_val)

    rows = []
    for fold in folds:
        # train a single-fold model (not the 4-fold ensemble) so train/val
        # RMSE reflect exactly the model that produced this fold's
        # out-of-fold validation predictions
        one_fold_model = mod.train_fold_models([fold], feats, cv.TARGET, best_params)
        train_pred = mod.predict_ensemble(one_fold_model, fold.train[feats])
        val_pred = mod.predict_ensemble(one_fold_model, fold.val[feats])
        train_rmse = M.rmse(fold.train[cv.TARGET].values, train_pred)
        val_rmse = M.rmse(fold.val[cv.TARGET].values, val_pred)
        val_r2 = M.r2(fold.val[cv.TARGET].values, val_pred)
        rows.append({
            "model": args.model, "held_out_part": fold.part,
            "n_train": len(fold.train), "n_val": len(fold.val),
            "train_rmse": train_rmse, "val_rmse": val_rmse,
            "gap": val_rmse - train_rmse, "val_r2": val_r2,
        })
        log(f"part {fold.part}: n_train={len(fold.train)} n_val={len(fold.val)} "
            f"train_rmse={train_rmse:.3f} val_rmse={val_rmse:.3f} gap={val_rmse-train_rmse:+.3f} "
            f"val_r2={val_r2:.3f}")

    fold_df = pd.DataFrame(rows)
    log(f"{args.model} fold summary: mean val_rmse={fold_df['val_rmse'].mean():.3f} "
        f"(sd={fold_df['val_rmse'].std():.3f}), mean gap={fold_df['gap'].mean():.3f} "
        f"(sd={fold_df['gap'].std():.3f})")

    fold_df.to_csv(f"{OUT_DIR}/fold_diagnostics_{args.model}.csv", index=False)
    log(f"{args.model} fold diagnostics done")


if __name__ == "__main__":
    main()
