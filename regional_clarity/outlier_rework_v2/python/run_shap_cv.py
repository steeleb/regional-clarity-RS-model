"""SHAP attribution for the backward-elimination candidate feature sets,
computed strictly out-of-fold on CV (partitions 1-4) - NOT the test
partition, since we haven't chosen a final feature set or done final
tuning yet. For each of the 4 folds: train one model on that fold's own
train split, explain that fold's own held-out val split, then concatenate
the 4 folds' out-of-fold SHAP values into one CV-wide picture (same
out-of-fold-concatenation logic already used for CV metrics elsewhere in
this project) - so every row is explained by a model that never saw it,
same discipline as the rest of this phase.

Answers: with weather fully eliminated from both candidate shared sets,
how much of the prediction now comes from site characteristics (including
shoreline proximity) vs. optical, and does that change per model?

v2 change: feature sets are read from results/selected_features.json
(written by run_backward_elimination.py) instead of a hardcoded literal;
SITE_COLS comes from features.py, the single source of truth.
"""
import json

import numpy as np
import pandas as pd
import shap

import features
import model_lgb
import model_nn
import model_xgb
import spatial_cv as cv
from features import SITE_COLS

OUT_DIR = "results"

SITE_COLS_SET = set(SITE_COLS)


def load_feature_sets():
    with open(f"{OUT_DIR}/selected_features.json") as fh:
        return json.load(fh)


def categorize(feat):
    if feat in SITE_COLS_SET:
        return "site"
    if feat.startswith(("precip_mm_prev", "tmax_degC_prev", "tmean_degC_prev",
                        "tmin_degC_prev", "srad_Wm2_prev")):
        return "weather"
    return "optical"


def load_fixed_params(model_name):
    with open(f"{OUT_DIR}/fg_result_{model_name}_optical_site_weather.json") as fh:
        params = json.load(fh)["best_params"]
    if model_name == "nn":
        params["hidden_sizes"] = tuple(params["hidden_sizes"])
    return params


def shap_for_fold_tree(mod_module, is_xgb, fold, feats, target, params):
    models = mod_module.train_fold_models([fold], feats, target, params)
    model = models[0]
    explainer = shap.TreeExplainer(model)
    if is_xgb:
        import xgboost as xgb
        sv = explainer.shap_values(xgb.DMatrix(fold.val[feats]))
    else:
        sv = explainer.shap_values(fold.val[feats])
    return sv, fold.val[feats].reset_index(drop=True)


def shap_for_fold_nn(fold, feats, target, params, seed=47, val_subsample=400):
    models = model_nn.train_fold_models([fold], feats, target, params)
    model, prep = models[0]
    rng = np.random.RandomState(seed)
    bg_idx = rng.choice(len(fold.train), size=min(100, len(fold.train)), replace=False)
    val_idx = rng.choice(len(fold.val), size=min(val_subsample, len(fold.val)), replace=False)
    val_sub = fold.val.iloc[val_idx][feats].reset_index(drop=True)

    def predict_fn(X, _model=model, _prep=prep):
        import torch
        Xt = torch.tensor(_prep.transform(np.asarray(X, dtype=float)), dtype=torch.float32)
        _model.eval()
        with torch.no_grad():
            return _model(Xt).numpy()

    background = fold.train.iloc[bg_idx][feats].values
    explainer = shap.KernelExplainer(predict_fn, background)
    sv = explainer.shap_values(val_sub.values, nsamples=100, silent=True)
    return sv, val_sub


def run_one(model_name, set_name, feats, folds, target, params):
    groups = {f: categorize(f) for f in feats}
    sv_parts, data_parts = [], []
    for fold in folds:
        if model_name == "xgboost":
            sv, data = shap_for_fold_tree(model_xgb, True, fold, feats, target, params)
        elif model_name == "lightgbm":
            sv, data = shap_for_fold_tree(model_lgb, False, fold, feats, target, params)
        else:
            sv, data = shap_for_fold_nn(fold, feats, target, params)
        sv_parts.append(sv)
        data_parts.append(data)

    shap_values = np.concatenate(sv_parts, axis=0)
    mean_abs_shap = np.abs(shap_values).mean(axis=0)
    per_feature = pd.DataFrame({"feature": feats, "mean_abs_shap": mean_abs_shap,
                                "group": [groups[f] for f in feats]}).sort_values(
        "mean_abs_shap", ascending=False)
    by_group = per_feature.groupby("group")["mean_abs_shap"].sum()
    group_pct = (by_group / by_group.sum() * 100).to_dict()

    print(f"{model_name} / {set_name} (n_oof_rows={shap_values.shape[0]})")
    print(f"  group contribution (% of total mean|SHAP|): {group_pct}")
    print(f"  top 8: {per_feature.head(8)[['feature','mean_abs_shap','group']].to_dict('records')}")

    result = dict(model=model_name, feature_set=set_name, n_oof_rows=int(shap_values.shape[0]),
                  per_feature=per_feature.to_dict("records"), group_contribution_pct=group_pct)
    with open(f"{OUT_DIR}/shap_cv_{model_name}_{set_name}.json", "w") as fh:
        json.dump(result, fh, indent=2, default=str)


def main():
    df = pd.read_parquet("data/modeling_dataset_expanded.parquet")
    df = features.add_spectral_indices(df)
    train_val, test = cv.train_val_test_split(df)
    folds = cv.build_folds(train_val)

    feature_sets = load_feature_sets()

    for model_name in ["xgboost", "lightgbm", "nn"]:
        params = load_fixed_params(model_name)
        for set_name, feats in feature_sets.items():
            run_one(model_name, set_name, feats, folds, cv.TARGET, dict(params))

    print("SHAP-CV DONE")


if __name__ == "__main__":
    main()
