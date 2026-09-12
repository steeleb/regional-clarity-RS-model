"""Final hyperparameter tuning on the chosen feature-intersection set from
backward elimination (results/selected_features.json's "intersection" key
- read from file rather than a hardcoded literal, see
run_backward_elimination.py's docstring for why).

gap_aware_tune (now shared in tuning.py, used at every tuning step in this
pipeline, not just this one - see tuning.py's docstring for the original
motivation: a first pass selecting purely by lowest mean CV RMSE did NOT
close the train-val gap, because nothing in that objective penalized it).

Per-fold train-vs-val RMSE gap is then reported for each model's chosen
config (one fold at a time, not the 4-fold ensemble, so the gap reflects
exactly the model that produced that fold's held-out predictions). All of
this uses CV (partitions 1-4) only; the test partition is not touched by
this script.
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
from tuning import gap_aware_tune

DATA_PATH = "data/modeling_dataset_expanded.parquet"
OUT_DIR = "results"

MODELS = {"xgboost": model_xgb, "lightgbm": model_lgb, "nn": model_nn}
N_TRIALS = {"xgboost": 40, "lightgbm": 40, "nn": 15}


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def load_selected_features(key="intersection"):
    with open(f"{OUT_DIR}/selected_features.json") as fh:
        return json.load(fh)[key]


def main():
    feats = load_selected_features("intersection")
    df = pd.read_parquet(DATA_PATH)
    df = features.add_spectral_indices(df)
    train_val, test = cv.train_val_test_split(df)
    folds = cv.build_folds(train_val)
    log(f"selected feature set (n={len(feats)}): {feats}")

    for name, mod in MODELS.items():
        log(f"=== gap-aware tuning {name} on the selected feature set ({N_TRIALS[name]} trials) ===")
        result = gap_aware_tune(mod, folds, feats, cv.TARGET, n_trials=N_TRIALS[name], seed=47)
        best_params = result["best_params"]
        log(f"{name} chosen params: {best_params}")
        log(f"{name} chosen: val_rmse={result['best_score']:.4f} train_rmse={result['best_train_rmse']:.4f} "
            f"gap={result['best_gap']:.4f} | pure-best val_rmse would have been {result['pure_best_val_rmse']:.4f} "
            f"({result['band_size']}/{result['n_trials']} trials within 2% tolerance band)")

        with open(f"{OUT_DIR}/final_tune_{name}.json", "w") as fh:
            json.dump({"model": name, "features": feats,
                       "best_params": {k: (list(v) if isinstance(v, tuple) else v)
                                       for k, v in best_params.items()},
                       "best_score": result["best_score"], "best_gap": result["best_gap"],
                       "pure_best_val_rmse": result["pure_best_val_rmse"],
                       "band_size": result["band_size"], "trials": result["trials"]},
                      fh, indent=2, default=str)

        log(f"=== {name} per-fold train/val diagnostics ===")
        rows = []
        for fold in folds:
            one_fold_model = mod.train_fold_models([fold], feats, cv.TARGET, best_params)
            train_pred = mod.predict_ensemble(one_fold_model, fold.train[feats])
            val_pred = mod.predict_ensemble(one_fold_model, fold.val[feats])
            train_rmse = M.rmse(fold.train[cv.TARGET].values, train_pred)
            val_rmse = M.rmse(fold.val[cv.TARGET].values, val_pred)
            val_r2 = M.r2(fold.val[cv.TARGET].values, val_pred)
            rows.append({"model": name, "held_out_part": fold.part,
                        "n_train": len(fold.train), "n_val": len(fold.val),
                        "train_rmse": train_rmse, "val_rmse": val_rmse,
                        "gap": val_rmse - train_rmse, "val_r2": val_r2})
            log(f"  part {fold.part}: train_rmse={train_rmse:.3f} val_rmse={val_rmse:.3f} "
                f"gap={val_rmse-train_rmse:+.3f} val_r2={val_r2:.3f}")

        fold_df = pd.DataFrame(rows)
        log(f"{name} fold summary: mean val_rmse={fold_df['val_rmse'].mean():.3f} "
            f"(sd={fold_df['val_rmse'].std():.3f}), mean gap={fold_df['gap'].mean():.3f} "
            f"(sd={fold_df['gap'].std():.3f}), mean train_rmse={fold_df['train_rmse'].mean():.3f}")
        fold_df.to_csv(f"{OUT_DIR}/final_fold_diagnostics_{name}.csv", index=False)

    log("FINAL TUNING DONE")


if __name__ == "__main__":
    main()
