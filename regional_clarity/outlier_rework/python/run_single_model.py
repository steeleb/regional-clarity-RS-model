"""Train+evaluate exactly ONE model type and exit. Run as a separate
process per model (see run_all.sh) rather than importing xgboost, lightgbm,
and torch in the same process - on this machine, torch's bundled OpenMP
runtime and xgboost/lightgbm's bundled OpenMP runtime crash on
co-initialization (OMP Error #179, pthread_mutex_init failure), a known
macOS conda/pip packaging issue, not a computation problem. Process-level
isolation sidesteps it cleanly instead of relying on the unsafe
KMP_DUPLICATE_LIB_OK workaround, which didn't fully resolve it here anyway.

No-leakage discipline is identical to train_compare.py's original design:
part 5 untouched until final eval, correlation pruning fit on train_val
only, tuning only ever sees the 4-fold CV over parts 1-4.
"""
import argparse
import json
import time

import pandas as pd

import features
import metrics as M
import spatial_cv as cv

DATA_PATH = "data/modeling_dataset.parquet"
OUT_DIR = "results"


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("model", choices=["xgboost", "lightgbm", "nn"])
    parser.add_argument("--n-trials", type=int, default=None)
    args = parser.parse_args()

    if args.model == "xgboost":
        import model_xgb as mod
        default_trials = 25
    elif args.model == "lightgbm":
        import model_lgb as mod
        default_trials = 25
    else:
        import model_nn as mod
        default_trials = 15
    n_trials = args.n_trials or default_trials

    import os
    os.makedirs(OUT_DIR, exist_ok=True)

    log(f"loading data for {args.model}")
    df = cv.load(DATA_PATH)
    df = features.add_spectral_indices(df)
    candidate_feats = features.candidate_feature_list(df)

    train_val, test = cv.train_val_test_split(df)
    feats = features.correlation_prune(train_val, candidate_feats, target=cv.TARGET)
    log(f"{len(feats)} features after correlation pruning: {feats}")

    folds = cv.build_folds(train_val)

    log(f"=== tuning {args.model} ({n_trials} trials) ===")
    t0 = time.time()
    tune_result = mod.tune(folds, feats, cv.TARGET, n_trials=n_trials)
    log(f"tuning done in {time.time()-t0:.1f}s, best mean CV RMSE={tune_result['best_score']:.4f}")
    log(f"best params: {tune_result['best_params']}")

    fold_models = mod.train_fold_models(folds, feats, cv.TARGET, tune_result["best_params"])

    cv_preds = []
    for f, model in zip(folds, fold_models):
        pred = mod.predict_ensemble([model], f.val[feats])
        cv_preds.append(pd.DataFrame({
            "part": f.part, "pred": pred, "y": f.val[cv.TARGET].values,
            "siteSR_id": f.val["siteSR_id"].values, "date": f.val["date"].values,
            "HUC4": f.val["HUC4"].values,
        }))
    cv_preds = pd.concat(cv_preds, ignore_index=True)
    cv_metrics = M.all_metrics(cv_preds["y"], cv_preds["pred"])
    log(f"CV (parts 1-4, out-of-fold) metrics: {cv_metrics}")

    test_pred = mod.predict_ensemble(fold_models, test[feats])
    test_metrics = M.all_metrics(test[cv.TARGET].values, test_pred)
    log(f"TEST (part 5, held out) metrics: {test_metrics}")

    test_df = test[["siteSR_id", "date", "HUC4", "mission", "time_diff", cv.TARGET]].copy()
    test_df["model"] = args.model
    test_df["pred"] = test_pred
    test_df.to_parquet(f"{OUT_DIR}/test_predictions_{args.model}.parquet")

    result = {
        "model": args.model,
        "features": feats,
        "best_params": {k: (list(v) if isinstance(v, tuple) else v)
                         for k, v in tune_result["best_params"].items()},
        "cv_metrics": cv_metrics,
        "test_metrics": test_metrics,
        "tuning_trials": tune_result["trials"],
    }
    with open(f"{OUT_DIR}/model_result_{args.model}.json", "w") as fh:
        json.dump(result, fh, indent=2, default=str)

    log(f"{args.model} done")


if __name__ == "__main__":
    main()
