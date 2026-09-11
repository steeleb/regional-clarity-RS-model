"""Test whether site characteristics (elevation, LakeCat catchment land
cover) and/or antecedent weather (gridMET previous-days precip/temp/solar
radiation) improve on the optical-only baseline - the same question Topp
et al. 2021 answered for their national SDD model (they paired 3 optical
variables with 8 static LakeCat catchment variables; this repeats that
comparison with our own optical feature set, spatial CV, and adds an
antecedent-weather arm they didn't test as a model input).

Four feature groups, each run through the identical pipeline (correlation
pruning on train_val only, SDD-weighted training with each model's
already-tuned hyperparameters re-tuned fresh per group since a different
feature set can favor different hyperparameters, 4-fold spatial CV
ensemble, one evaluation on the untouched test partition):
  optical          - the baseline from the original report (spectral bands/indices only)
  optical+site      - + elevation, catchment land cover (LakeCat)
  optical+weather    - + antecedent precip/temp/solar radiation (1/3/7/30-day)
  optical+site+weather - both

Run as its own process per (model, feature_group) - OpenMP isolation, as
elsewhere.
"""
import argparse
import json
import time

import numpy as np
import pandas as pd

import features
import metrics as M
import spatial_cv as cv
from weighting import make_sdd_weight_fn

DATA_PATH = "data/modeling_dataset_expanded.parquet"
OUT_DIR = "results"

SITE_COLS = ["elevation_m", "catchment_area_sqkm", "pct_impervious_2006",
             "pct_urban_2006", "pct_forest_2006", "pct_cropland_2006", "pct_wetland_2006"]
WEATHER_PREFIXES = ("precip_mm_prev", "tmax_degC_prev", "tmean_degC_prev",
                    "tmin_degC_prev", "srad_Wm2_prev")


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def weather_cols(df):
    return [c for c in df.columns if c.startswith(WEATHER_PREFIXES)]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("model", choices=["xgboost", "lightgbm", "nn"])
    parser.add_argument("feature_group", choices=["optical", "optical+site", "optical+weather", "optical+site+weather"])
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

    log(f"loading expanded data for {args.model} / {args.feature_group}")
    df = cv.load(DATA_PATH)
    df = features.add_spectral_indices(df)

    # candidate_feature_list() excludes only metadata columns, so on this
    # expanded dataframe it would otherwise include the site/weather
    # columns too - explicitly strip those back out so "optical" really
    # means optical-only, regardless of what other columns this dataset
    # happens to carry
    all_extra_cols = set(SITE_COLS) | set(weather_cols(df))
    optical_feats = [f for f in features.candidate_feature_list(df) if f not in all_extra_cols]

    extra_feats = []
    if "site" in args.feature_group:
        extra_feats += SITE_COLS
    if "weather" in args.feature_group:
        extra_feats += weather_cols(df)

    candidate_feats = optical_feats + extra_feats
    log(f"candidate features: {len(optical_feats)} optical + {len(extra_feats)} extra = {len(candidate_feats)}")

    train_val, test = cv.train_val_test_split(df)
    feats = features.correlation_prune(train_val, candidate_feats, target=cv.TARGET)
    log(f"{len(feats)} features after correlation pruning: {feats}")

    folds = cv.build_folds(train_val)
    weight_fn = make_sdd_weight_fn(k=2.0)

    log(f"=== tuning {args.model} / {args.feature_group} ({n_trials} trials) ===")
    t0 = time.time()
    tune_result = mod.tune(folds, feats, cv.TARGET, n_trials=n_trials)
    log(f"tuning done in {time.time()-t0:.1f}s, best mean CV RMSE={tune_result['best_score']:.4f}")

    fold_models = mod.train_fold_models(folds, feats, cv.TARGET, tune_result["best_params"], weight_fn=weight_fn)

    cv_preds = []
    for f, model in zip(folds, fold_models):
        pred = mod.predict_ensemble([model], f.val[feats])
        cv_preds.append(pd.DataFrame({"part": f.part, "pred": pred, "y": f.val[cv.TARGET].values}))
    cv_preds = pd.concat(cv_preds, ignore_index=True)
    cv_metrics = M.all_metrics(cv_preds["y"], cv_preds["pred"])
    log(f"CV metrics: {cv_metrics}")

    test_pred = mod.predict_ensemble(fold_models, test[feats])
    test_metrics = M.all_metrics(test[cv.TARGET].values, test_pred)
    log(f"TEST metrics: {test_metrics}")

    tag = args.feature_group.replace("+", "_")
    test_df = test[["siteSR_id", "date", "HUC4", "mission", cv.TARGET]].copy()
    test_df["model"] = args.model
    test_df["feature_group"] = args.feature_group
    test_df["pred"] = test_pred
    test_df.to_parquet(f"{OUT_DIR}/fg_predictions_{args.model}_{tag}.parquet")

    result = {
        "model": args.model, "feature_group": args.feature_group,
        "n_optical_feats": len(optical_feats), "n_extra_feats": len(extra_feats),
        "features": feats, "best_params": {k: (list(v) if isinstance(v, tuple) else v)
                                            for k, v in tune_result["best_params"].items()},
        "cv_metrics": cv_metrics, "test_metrics": test_metrics,
    }
    with open(f"{OUT_DIR}/fg_result_{args.model}_{tag}.json", "w") as fh:
        json.dump(result, fh, indent=2, default=str)

    log(f"{args.model} / {args.feature_group} done")


if __name__ == "__main__":
    main()
