"""The one final look at the test partition for this workflow. Every
decision that got here - feature set (backward elimination's intersection
set, read from results/selected_features.json), hyperparameters (gap-aware
tuning), and SDD-weighting (established earlier: helps XGBoost/LightGBM,
hurts the capped NN) - was already made from CV (partitions 1-4) alone.
This script only reports the outcome of those decisions on the untouched
test partition; it makes no new decisions from test performance.

Final production config per model:
  XGBoost / LightGBM: gap-aware-tuned hyperparameters + SDD-weighted
    training (k=2.0), matching the earlier established finding that
    weighting improves top-quartile accuracy and bias for the tree models.
  Neural net: gap-aware-tuned hyperparameters, unweighted - weighting was
    shown earlier to slightly hurt the capped 3x8 architecture.

Reports both out-of-fold CV metrics (for continuity with everything
reported so far) and the test metrics (the final number), from the same
4-fold ensemble used throughout this project.
"""
import json
import time

import pandas as pd

import features
import metrics as M
import model_lgb
import model_nn
import model_xgb
import spatial_cv as cv
from weighting import make_sdd_weight_fn

DATA_PATH = "data/modeling_dataset_expanded.parquet"
OUT_DIR = "results"

MODELS = {"xgboost": model_xgb, "lightgbm": model_lgb, "nn": model_nn}
USE_WEIGHTING = {"xgboost": True, "lightgbm": True, "nn": False}


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
    feats = load_selected_features("intersection")
    df = pd.read_parquet(DATA_PATH)
    df = features.add_spectral_indices(df)
    train_val, test = cv.train_val_test_split(df)
    folds = cv.build_folds(train_val)
    weight_fn = make_sdd_weight_fn(k=2.0)

    summary = {}
    for name, mod in MODELS.items():
        params = load_final_params(name)
        wfn = weight_fn if USE_WEIGHTING[name] else None
        log(f"=== {name}: final production config (weighted={USE_WEIGHTING[name]}) ===")
        log(f"{name} params: {params}")
        log(f"{name} features (n={len(feats)}): {feats}")

        fold_models = mod.train_fold_models(folds, feats, cv.TARGET, params, weight_fn=wfn)

        cv_preds = []
        for f, model in zip(folds, fold_models):
            pred = mod.predict_ensemble([model], f.val[feats])
            cv_preds.append(pd.DataFrame({"part": f.part, "pred": pred,
                                          "y": f.val[cv.TARGET].values}))
        cv_preds = pd.concat(cv_preds, ignore_index=True)
        cv_metrics = M.all_metrics(cv_preds["y"], cv_preds["pred"])
        log(f"{name} CV (out-of-fold) metrics: {cv_metrics}")

        test_pred = mod.predict_ensemble(fold_models, test[feats])
        test_metrics = M.all_metrics(test[cv.TARGET].values, test_pred)
        log(f"{name} TEST metrics (final): {test_metrics}")

        test_df = test[["siteSR_id", "date", "HUC4", "mission", cv.TARGET]].copy()
        test_df["pred"] = test_pred
        test_df.to_parquet(f"{OUT_DIR}/final_test_predictions_{name}.parquet")

        summary[name] = dict(weighted=USE_WEIGHTING[name], params={k: str(v) for k, v in params.items()},
                              features=feats, cv_metrics=cv_metrics, test_metrics=test_metrics)
        with open(f"{OUT_DIR}/final_eval_{name}.json", "w") as fh:
            json.dump(summary[name], fh, indent=2, default=str)

    with open(f"{OUT_DIR}/final_eval_summary.json", "w") as fh:
        json.dump(summary, fh, indent=2, default=str)

    log("FINAL EVALUATION DONE")


if __name__ == "__main__":
    main()
