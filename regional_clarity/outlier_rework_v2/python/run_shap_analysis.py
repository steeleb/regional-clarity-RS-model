"""SHAP feature attribution on the held-out test set, for every (model,
feature_group) combination. Answers two questions directly:

 1. Do weather (and site) features behave physically sensibly - e.g. does
    more antecedent precip/warmth push toward LOWER predicted clarity, not
    some arbitrary or reversed direction?
 2. Are they a secondary "tuning" contribution on top of an optical-driven
    prediction, or do they actually dominate the model? This determines
    how the model should honestly be framed - primarily RS-driven with
    met/catchment refinement, or something else - rather than assuming
    the former.

SHAP is computed per fold model (matching how each fold model was
actually tuned/trained) and averaged across the 4-fold ensemble, since
that average is what the model's actual prediction is.
"""
import argparse
import json

import numpy as np
import pandas as pd
import shap

import features
import spatial_cv as cv
from features import SITE_COLS, weather_cols
from weighting import make_sdd_weight_fn

OUT_DIR = "results"

SITE_COLS_SET = set(SITE_COLS)


def categorize(feat: str, weather_col_set: set) -> str:
    if feat in SITE_COLS_SET:
        return "site"
    if feat in weather_col_set:
        return "weather"
    return "optical"


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("model", choices=["xgboost", "lightgbm", "nn"])
    parser.add_argument("feature_group", choices=["optical", "optical+site", "optical+weather", "optical+site+weather"])
    args = parser.parse_args()

    tag = args.feature_group.replace("+", "_")
    with open(f"{OUT_DIR}/fg_result_{args.model}_{tag}.json") as fh:
        prior = json.load(fh)
    feats = prior["features"]
    best_params = prior["best_params"]
    if args.model == "nn":
        best_params["hidden_sizes"] = tuple(best_params["hidden_sizes"])

    df = cv.load("data/modeling_dataset_expanded.parquet")
    df = features.add_spectral_indices(df)
    train_val, test = cv.train_val_test_split(df)
    folds = cv.build_folds(train_val)
    weight_fn = make_sdd_weight_fn(k=2.0)

    weather_col_set = set(weather_cols(df))
    groups = {f: categorize(f, weather_col_set) for f in feats}

    if args.model == "xgboost":
        import model_xgb as mod
        import xgboost as xgb
        fold_models = mod.train_fold_models(folds, feats, cv.TARGET, best_params, weight_fn=weight_fn)
        shap_matrices = []
        for booster in fold_models:
            explainer = shap.TreeExplainer(booster)
            sv = explainer.shap_values(xgb.DMatrix(test[feats]))
            shap_matrices.append(sv)
        shap_values = np.mean(shap_matrices, axis=0)

    elif args.model == "lightgbm":
        import model_lgb as mod
        fold_models = mod.train_fold_models(folds, feats, cv.TARGET, best_params, weight_fn=weight_fn)
        shap_matrices = []
        for booster in fold_models:
            explainer = shap.TreeExplainer(booster)
            sv = explainer.shap_values(test[feats])
            shap_matrices.append(sv)
        shap_values = np.mean(shap_matrices, axis=0)

    else:
        import model_nn as mod
        fold_models = mod.train_fold_models(folds, feats, cv.TARGET, best_params, weight_fn=weight_fn)
        # background: a small random sample of train_val in the SAME
        # (unscaled) feature space each fold's own preprocessor expects
        rng = np.random.RandomState(47)
        bg_idx = rng.choice(len(train_val), size=min(100, len(train_val)), replace=False)
        test_sub_idx = rng.choice(len(test), size=min(400, len(test)), replace=False)
        test_sub = test.iloc[test_sub_idx]

        shap_matrices = []
        for model, prep in fold_models:
            def predict_fn(X, _model=model, _prep=prep):
                import torch
                Xt = torch.tensor(_prep.transform(np.asarray(X, dtype=float)), dtype=torch.float32)
                _model.eval()
                with torch.no_grad():
                    return _model(Xt).numpy()

            background = train_val.iloc[bg_idx][feats].values
            explainer = shap.KernelExplainer(predict_fn, background)
            sv = explainer.shap_values(test_sub[feats].values, nsamples=100, silent=True)
            shap_matrices.append(sv)
        shap_values = np.mean(shap_matrices, axis=0)
        test = test_sub  # align rows with the subsampled SHAP matrix

    # ---- per-feature and per-category summaries ----
    mean_abs_shap = np.abs(shap_values).mean(axis=0)
    per_feature = pd.DataFrame({"feature": feats, "mean_abs_shap": mean_abs_shap,
                                "group": [groups[f] for f in feats]}).sort_values(
        "mean_abs_shap", ascending=False)

    by_group = per_feature.groupby("group")["mean_abs_shap"].sum()
    total = by_group.sum()
    group_pct = (by_group / total * 100).to_dict()

    # direction sanity for weather features: correlation between the raw
    # feature value and its SHAP value - negative precip/temp SHAP
    # correlation would mean "more rain/warmth -> lower predicted clarity"
    weather_direction = {}
    for i, f in enumerate(feats):
        if groups[f] == "weather":
            corr = np.corrcoef(test[f].values, shap_values[:, i])[0, 1]
            weather_direction[f] = None if np.isnan(corr) else float(corr)

    result = {
        "model": args.model, "feature_group": args.feature_group,
        "n_test": len(test),
        "per_feature": per_feature.to_dict("records"),
        "group_contribution_pct": group_pct,
        "weather_value_shap_correlation": weather_direction,
    }
    with open(f"{OUT_DIR}/shap_{args.model}_{tag}.json", "w") as fh:
        json.dump(result, fh, indent=2, default=str)

    np.save(f"{OUT_DIR}/shap_values_{args.model}_{tag}.npy", shap_values)
    test[feats].to_csv(f"{OUT_DIR}/shap_testdata_{args.model}_{tag}.csv", index=False)

    print(f"{args.model} / {args.feature_group}")
    print(f"  group contribution (% of total mean|SHAP|): {group_pct}")
    print(f"  top 5 features: {per_feature.head(5)[['feature','mean_abs_shap','group']].to_dict('records')}")
    if weather_direction:
        print(f"  weather value-SHAP correlation (negative = higher value -> lower clarity): {weather_direction}")


if __name__ == "__main__":
    main()
