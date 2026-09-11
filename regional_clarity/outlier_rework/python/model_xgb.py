"""XGBoost training with the same leave-one-partition-out spatial CV used
throughout (folds over parts 1-4, part 5 held out entirely). Hyperparameter
search is a random search over a reasonable grid, scored by mean CV RMSE -
an automated stand-in for 04_make_models.Rmd's two-round manual grid
search, using the same "never touch the test partition" discipline."""
import random

import numpy as np
import xgboost as xgb

from metrics import rmse
from spatial_cv import Fold

PARAM_SPACE = dict(
    eta=[0.01, 0.03, 0.05, 0.1],
    max_depth=[2, 3, 4, 5, 6],
    min_child_weight=[1, 3, 5, 7],
    subsample=[0.7, 0.8, 0.9, 1.0],
    colsample_bytree=[0.5, 0.7, 0.9, 1.0],
)


def _sample_params(rng: random.Random) -> dict:
    return {k: rng.choice(v) for k, v in PARAM_SPACE.items()}


def _fit_fold(train_X, train_y, val_X, val_y, params, nrounds=3000, early_stop=100, train_weight=None):
    dtrain = xgb.DMatrix(train_X, label=train_y, weight=train_weight)
    dval = xgb.DMatrix(val_X, label=val_y)
    full_params = dict(booster="gbtree", objective="reg:squarederror", **params)
    booster = xgb.train(full_params, dtrain, num_boost_round=nrounds,
                         evals=[(dtrain, "train"), (dval, "val")],
                         early_stopping_rounds=early_stop, verbose_eval=False)
    best_val_rmse = booster.best_score
    return booster, best_val_rmse


def tune(folds: list, feats: list, target: str, n_trials: int = 25, seed: int = 47) -> dict:
    rng = random.Random(seed)
    best_params, best_score = None, np.inf
    trials = []
    for _ in range(n_trials):
        params = _sample_params(rng)
        fold_scores = []
        for fold in folds:
            _, val_rmse = _fit_fold(fold.train[feats], fold.train[target],
                                     fold.val[feats], fold.val[target], params)
            fold_scores.append(val_rmse)
        mean_score = float(np.mean(fold_scores))
        trials.append({**params, "mean_val_rmse": mean_score})
        if mean_score < best_score:
            best_score, best_params = mean_score, params
    return {"best_params": best_params, "best_score": best_score, "trials": trials}


def train_fold_models(folds: list, feats: list, target: str, params: dict, weight_fn=None) -> list:
    models = []
    for fold in folds:
        w = weight_fn(fold.train[target].values) if weight_fn is not None else None
        booster, _ = _fit_fold(fold.train[feats], fold.train[target],
                                fold.val[feats], fold.val[target], params,
                                nrounds=5000, early_stop=250, train_weight=w)
        models.append(booster)
    return models


def predict_ensemble(models: list, X) -> np.ndarray:
    d = xgb.DMatrix(X)
    preds = np.column_stack([m.predict(d) for m in models])
    return preds.mean(axis=1)
