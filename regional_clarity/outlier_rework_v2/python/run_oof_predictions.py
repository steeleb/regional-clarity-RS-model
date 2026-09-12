"""Save per-observation out-of-fold (OOF) CV predictions for one model,
using its already-tuned hyperparameters and SDD-weighted training (each
model's best variant per the weighted-training comparison). These OOF
predictions - each row predicted by the single-fold model that held it out
- are what ensemble weights get fit on, so the test set is never touched
while choosing how to combine models. Run as its own process (OpenMP
isolation, as elsewhere).
"""
import argparse
import json

import pandas as pd

import features
import spatial_cv as cv
from weighting import make_sdd_weight_fn

OUT_DIR = "results"


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
    weight_fn = make_sdd_weight_fn(k=2.0)

    oof_rows = []
    for fold in folds:
        one_fold_model = mod.train_fold_models([fold], feats, cv.TARGET, best_params, weight_fn=weight_fn)
        pred = mod.predict_ensemble(one_fold_model, fold.val[feats])
        oof_rows.append(pd.DataFrame({
            "siteSR_id": fold.val["siteSR_id"].values, "date": fold.val["date"].values,
            "part": fold.part, "y": fold.val[cv.TARGET].values, "pred": pred,
        }))
    oof_df = pd.concat(oof_rows, ignore_index=True)
    oof_df.to_parquet(f"{OUT_DIR}/oof_predictions_{args.model}.parquet")
    print(f"{args.model}: wrote {len(oof_df)} OOF predictions")


if __name__ == "__main__":
    main()
