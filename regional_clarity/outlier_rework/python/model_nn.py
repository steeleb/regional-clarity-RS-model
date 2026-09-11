"""A basic feedforward NN (PyTorch), tuned via the same spatial CV / random
search discipline as the two boosted-tree models. Unlike xgboost/lightgbm,
an NN needs explicit imputation and feature scaling - both are fit on the
fold's TRAIN split only and applied unchanged to val/test, so no
information about held-out rows leaks into preprocessing."""
import random

import numpy as np
import torch
import torch.nn as nn

DEVICE = torch.device("cpu")

PARAM_SPACE = dict(
    hidden_sizes=[(64, 32), (128, 64), (64, 64, 32), (32, 32)],
    dropout=[0.0, 0.1, 0.2, 0.3],
    lr=[1e-2, 5e-3, 1e-3],
    weight_decay=[0.0, 1e-4, 1e-3],
)


class MLP(nn.Module):
    def __init__(self, n_features, hidden_sizes, dropout):
        super().__init__()
        layers = []
        prev = n_features
        for h in hidden_sizes:
            layers += [nn.Linear(prev, h), nn.ReLU(), nn.Dropout(dropout)]
            prev = h
        layers.append(nn.Linear(prev, 1))
        self.net = nn.Sequential(*layers)

    def forward(self, x):
        return self.net(x).squeeze(-1)


class Preprocessor:
    """Median-impute then standardize, fit on train only."""
    def fit(self, X: np.ndarray):
        self.median_ = np.nanmedian(X, axis=0)
        X_imputed = np.where(np.isnan(X), self.median_, X)
        self.mean_ = X_imputed.mean(axis=0)
        self.std_ = X_imputed.std(axis=0)
        self.std_[self.std_ == 0] = 1.0
        return self

    def transform(self, X: np.ndarray) -> np.ndarray:
        X_imputed = np.where(np.isnan(X), self.median_, X)
        return (X_imputed - self.mean_) / self.std_


def _sample_params(rng: random.Random) -> dict:
    return {k: rng.choice(v) for k, v in PARAM_SPACE.items()}


def _fit_fold(train_X, train_y, val_X, val_y, params, max_epochs=500, patience=30, seed=47):
    torch.manual_seed(seed)
    prep = Preprocessor().fit(train_X)
    Xtr = torch.tensor(prep.transform(train_X), dtype=torch.float32)
    ytr = torch.tensor(train_y, dtype=torch.float32)
    Xval = torch.tensor(prep.transform(val_X), dtype=torch.float32)
    yval = torch.tensor(val_y, dtype=torch.float32)

    model = MLP(Xtr.shape[1], params["hidden_sizes"], params["dropout"]).to(DEVICE)
    opt = torch.optim.Adam(model.parameters(), lr=params["lr"], weight_decay=params["weight_decay"])
    loss_fn = nn.MSELoss()

    best_val_rmse = np.inf
    best_state = None
    epochs_no_improve = 0

    for epoch in range(max_epochs):
        model.train()
        opt.zero_grad()
        pred = model(Xtr)
        loss = loss_fn(pred, ytr)
        loss.backward()
        opt.step()

        model.eval()
        with torch.no_grad():
            val_pred = model(Xval)
            val_rmse = torch.sqrt(loss_fn(val_pred, yval)).item()

        if val_rmse < best_val_rmse - 1e-5:
            best_val_rmse = val_rmse
            best_state = {k: v.clone() for k, v in model.state_dict().items()}
            epochs_no_improve = 0
        else:
            epochs_no_improve += 1
            if epochs_no_improve >= patience:
                break

    model.load_state_dict(best_state)
    return model, prep, best_val_rmse


def tune(folds: list, feats: list, target: str, n_trials: int = 15, seed: int = 47) -> dict:
    rng = random.Random(seed)
    best_params, best_score = None, np.inf
    trials = []
    for _ in range(n_trials):
        params = _sample_params(rng)
        fold_scores = []
        for fold in folds:
            _, _, val_rmse = _fit_fold(fold.train[feats].values, fold.train[target].values,
                                        fold.val[feats].values, fold.val[target].values, params)
            fold_scores.append(val_rmse)
        mean_score = float(np.mean(fold_scores))
        trials.append({**{k: str(v) for k, v in params.items()}, "mean_val_rmse": mean_score})
        if mean_score < best_score:
            best_score, best_params = mean_score, params
    return {"best_params": best_params, "best_score": best_score, "trials": trials}


def train_fold_models(folds: list, feats: list, target: str, params: dict) -> list:
    models = []
    for fold in folds:
        model, prep, _ = _fit_fold(fold.train[feats].values, fold.train[target].values,
                                    fold.val[feats].values, fold.val[target].values, params,
                                    max_epochs=1000, patience=50)
        models.append((model, prep))
    return models


def predict_ensemble(models: list, X) -> np.ndarray:
    Xv = X.values if hasattr(X, "values") else X
    preds = []
    for model, prep in models:
        model.eval()
        with torch.no_grad():
            Xt = torch.tensor(prep.transform(Xv), dtype=torch.float32)
            preds.append(model(Xt).numpy())
    return np.column_stack(preds).mean(axis=1)
