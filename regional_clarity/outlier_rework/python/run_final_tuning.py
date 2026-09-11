"""Final hyperparameter tuning on the chosen 16-feature intersection set.

A first pass (narrowed, more-regularized search spaces, selecting purely
by lowest mean CV RMSE) did NOT close the train-val gap - for XGBoost the
gap actually grew relative to the pre-elimination baseline (0.484 vs.
0.335), and the neural net's tuner picked dropout=0, weight_decay=0, the
least-regularized options in its own search space, because they minimized
raw CV RMSE with no penalty for the gap. A narrower search space alone
doesn't select for generalization if the objective being optimized never
looks at it.

This version fixes that at the objective, not just the search space:
`gap_aware_tune` finds every trial within `val_tolerance` (2% relative) of
the best mean CV RMSE, then picks the smallest train-val gap among that
band - trading a small, bounded amount of raw CV performance for a
meaningfully smaller generalization gap, rather than hoping regularized
hyperparameter ranges produce that outcome as a side effect.

Per-fold train-vs-val RMSE gap is then reported for each model's chosen
config (one fold at a time, not the 4-fold ensemble, so the gap reflects
exactly the model that produced that fold's held-out predictions). All of
this uses CV (partitions 1-4) only; the test partition is not touched by
this script.
"""
import json
import random
import time

import numpy as np
import pandas as pd

import features
import metrics as M
import model_lgb
import model_nn
import model_xgb
import spatial_cv as cv

DATA_PATH = "data/modeling_dataset_expanded.parquet"
OUT_DIR = "results"

INTERSECTION16 = ["BG", "MNDWI", "NDSSI", "NDVI", "NDWI", "NR", "atm_corr_LaSRC",
                   "catchment_area_sqkm", "fai", "nir_corr7", "pct_cropland_2006",
                   "pct_forest_2006", "pct_urban_2006", "pct_wetland_2006",
                   "red_corr7", "temp_corr7"]

MODELS = {"xgboost": model_xgb, "lightgbm": model_lgb, "nn": model_nn}
N_TRIALS = {"xgboost": 40, "lightgbm": 40, "nn": 15}


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def gap_aware_tune(mod, folds, feats, target, n_trials, seed=47, val_tolerance=0.02):
    """Same random search as mod.tune(), but selects among trials within
    val_tolerance of the best mean CV RMSE by smallest train-val gap,
    instead of by lowest RMSE alone."""
    rng = random.Random(seed)
    trials = []
    for _ in range(n_trials):
        params = mod._sample_params(rng)
        fold_train_rmse, fold_val_rmse = [], []
        for fold in folds:
            one_fold_model = mod.train_fold_models([fold], feats, target, params)
            train_pred = mod.predict_ensemble(one_fold_model, fold.train[feats])
            val_pred = mod.predict_ensemble(one_fold_model, fold.val[feats])
            fold_train_rmse.append(M.rmse(fold.train[target].values, train_pred))
            fold_val_rmse.append(M.rmse(fold.val[target].values, val_pred))
        mean_val = float(np.mean(fold_val_rmse))
        mean_train = float(np.mean(fold_train_rmse))
        trials.append(dict(params=params, mean_val_rmse=mean_val, mean_train_rmse=mean_train,
                            mean_gap=mean_val - mean_train))

    best_val = min(t["mean_val_rmse"] for t in trials)
    band = [t for t in trials if t["mean_val_rmse"] <= best_val * (1 + val_tolerance)]
    chosen = min(band, key=lambda t: t["mean_gap"])
    return dict(best_params=chosen["params"], best_score=chosen["mean_val_rmse"],
                best_gap=chosen["mean_gap"], best_train_rmse=chosen["mean_train_rmse"],
                pure_best_val_rmse=best_val, band_size=len(band), n_trials=n_trials,
                trials=[{**{k: str(v) for k, v in t["params"].items()},
                         "mean_val_rmse": t["mean_val_rmse"], "mean_train_rmse": t["mean_train_rmse"],
                         "mean_gap": t["mean_gap"]} for t in trials])


def main():
    df = pd.read_parquet(DATA_PATH)
    df = features.add_spectral_indices(df)
    train_val, test = cv.train_val_test_split(df)
    folds = cv.build_folds(train_val)
    log(f"intersection-16 features: {INTERSECTION16}")

    for name, mod in MODELS.items():
        log(f"=== gap-aware tuning {name} on intersection-16 ({N_TRIALS[name]} trials) ===")
        result = gap_aware_tune(mod, folds, INTERSECTION16, cv.TARGET, n_trials=N_TRIALS[name], seed=47)
        best_params = result["best_params"]
        log(f"{name} chosen params: {best_params}")
        log(f"{name} chosen: val_rmse={result['best_score']:.4f} train_rmse={result['best_train_rmse']:.4f} "
            f"gap={result['best_gap']:.4f} | pure-best val_rmse would have been {result['pure_best_val_rmse']:.4f} "
            f"({result['band_size']}/{result['n_trials']} trials within 2% tolerance band)")

        with open(f"{OUT_DIR}/final_tune_{name}.json", "w") as fh:
            json.dump({"model": name, "features": INTERSECTION16,
                       "best_params": {k: (list(v) if isinstance(v, tuple) else v)
                                       for k, v in best_params.items()},
                       "best_score": result["best_score"], "best_gap": result["best_gap"],
                       "pure_best_val_rmse": result["pure_best_val_rmse"],
                       "band_size": result["band_size"], "trials": result["trials"]},
                      fh, indent=2, default=str)

        log(f"=== {name} per-fold train/val diagnostics ===")
        rows = []
        for fold in folds:
            one_fold_model = mod.train_fold_models([fold], INTERSECTION16, cv.TARGET, best_params)
            train_pred = mod.predict_ensemble(one_fold_model, fold.train[INTERSECTION16])
            val_pred = mod.predict_ensemble(one_fold_model, fold.val[INTERSECTION16])
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
