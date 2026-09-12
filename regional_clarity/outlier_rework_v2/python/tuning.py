"""Gap-aware hyperparameter selection, shared by every tuning step in the
v2 pipeline (v1 only used this at the final-tuning stage; see the module
docstring history in outlier_rework/python/run_final_tuning.py for why it
was introduced).

A first pass at final tuning (narrowed, more-regularized search spaces,
selecting purely by lowest mean CV RMSE) did NOT close the train-val gap -
for XGBoost the gap actually grew relative to the pre-elimination baseline,
and the neural net's tuner picked dropout=0, weight_decay=0, the least-
regularized options in its own search space, because they minimized raw CV
RMSE with no penalty for the gap. A narrower search space alone doesn't
select for generalization if the objective being optimized never looks at
it.

`gap_aware_tune` fixes that at the objective, not just the search space:
finds every trial within `val_tolerance` (2% relative, by default) of the
best mean CV RMSE, then picks the smallest train-val gap among that band -
trading a small, bounded amount of raw CV performance for a meaningfully
smaller generalization gap, rather than hoping regularized hyperparameter
ranges produce that outcome as a side effect.

It only depends on the generic model-module interface already implemented
identically by model_lgb, model_xgb, and model_nn (`_sample_params`,
`train_fold_models`, `predict_ensemble`), so it's model-agnostic and safe
to call from any tuning step in the pipeline (baseline, feature-group
ablation, final tuning).
"""
import random

import numpy as np

import metrics as M


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
