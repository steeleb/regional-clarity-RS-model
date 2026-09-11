"""Backward feature elimination, gated entirely on CV (partitions 1-4).

Starting candidate pool: 17 optical + 7 site + 2 weather (precip_mm_prev30,
tmax_degC_prev30 - the only two antecedent-weather variables that showed a
consistently, physically sensible SHAP direction in the prior phase),
reduced by flat single-pass correlation pruning (the method already used
throughout this report, carried forward per the user's explicit choice).

Hyperparameters are held fixed throughout elimination - each model reuses
its already-tuned optical+site+weather config from the feature-group
comparison phase - and retuned only on the final chosen feature sets, in a
later step. No SDD-weighting during elimination either: that's a separate,
already-answered axis (helps the tree models, not the capped NN), and
mixing it in here would conflate two different decisions.

Runs three independent per-model eliminations (xgboost, lightgbm, nn) plus
one joint elimination scored across all three at once, then reports the
joint set alongside the intersection of the three independent per-model
sets - both requested as points of comparison before picking a shared
benchmark feature set.
"""
import json
import time

import pandas as pd

import backward_elim as be
import features
import model_lgb
import model_nn
import model_xgb
import spatial_cv as cv

DATA_PATH = "data/modeling_dataset_expanded.parquet"
OUT_DIR = "results"

SITE_COLS = ["elevation_m", "catchment_area_sqkm", "pct_impervious_2006",
             "pct_urban_2006", "pct_forest_2006", "pct_cropland_2006", "pct_wetland_2006"]
WEATHER_COLS_CURATED = ["precip_mm_prev30", "tmax_degC_prev30"]

MODEL_NAMES = ["xgboost", "lightgbm", "nn"]
TOL = 0.005


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def load_fixed_params(model_name):
    with open(f"{OUT_DIR}/fg_result_{model_name}_optical_site_weather.json") as fh:
        result = json.load(fh)
    params = result["best_params"]
    if model_name == "nn":
        params["hidden_sizes"] = tuple(params["hidden_sizes"])
    return params


def main():
    be.register_model("xgboost", model_xgb)
    be.register_model("lightgbm", model_lgb)
    be.register_model("nn", model_nn)

    log("loading data")
    df = pd.read_parquet(DATA_PATH)
    df = features.add_spectral_indices(df)
    train_val, test = cv.train_val_test_split(df)
    folds = cv.build_folds(train_val)

    optical_feats = [f for f in features.candidate_feature_list(df)
                      if f not in SITE_COLS and not f.startswith(
                          ("precip_mm_prev", "tmax_degC_prev", "tmean_degC_prev",
                           "tmin_degC_prev", "srad_Wm2_prev"))]
    candidates = optical_feats + SITE_COLS + WEATHER_COLS_CURATED
    log(f"candidate pool: {len(optical_feats)} optical + {len(SITE_COLS)} site + "
        f"{len(WEATHER_COLS_CURATED)} weather = {len(candidates)}")

    pruned = features.correlation_prune(train_val, candidates)
    log(f"flat single-pass correlation pruning: {len(candidates)} -> {len(pruned)} features")
    log(f"pruned starting set: {pruned}")

    params_by_model = {m: load_fixed_params(m) for m in MODEL_NAMES}
    for m in MODEL_NAMES:
        log(f"{m} fixed hyperparameters (reused from optical+site+weather tuning): {params_by_model[m]}")

    per_model_results = {}
    for m in MODEL_NAMES:
        log(f"=== backward elimination: {m} ===")
        result = be.backward_eliminate(m, folds, cv.TARGET, params_by_model[m], pruned,
                                        weight_fn=None, tol=TOL, seed=47, log=log)
        per_model_results[m] = result
        log(f"{m} final set ({len(result['final_features'])}): {result['final_features']}")
        with open(f"{OUT_DIR}/backward_elim_{m}.json", "w") as fh:
            json.dump(result, fh, indent=2, default=str)

    log("=== joint backward elimination (all 3 models at once) ===")
    folds_by_model = {m: folds for m in MODEL_NAMES}
    joint_result = be.joint_backward_eliminate(MODEL_NAMES, folds_by_model, cv.TARGET,
                                                params_by_model, pruned,
                                                weight_fn=None, tol=TOL, seed=47, log=log)
    log(f"joint final set ({len(joint_result['final_features'])}): {joint_result['final_features']}")
    with open(f"{OUT_DIR}/backward_elim_joint.json", "w") as fh:
        json.dump(joint_result, fh, indent=2, default=str)

    intersection = sorted(set(per_model_results["xgboost"]["final_features"])
                           & set(per_model_results["lightgbm"]["final_features"])
                           & set(per_model_results["nn"]["final_features"]))
    union = sorted(set(per_model_results["xgboost"]["final_features"])
                    | set(per_model_results["lightgbm"]["final_features"])
                    | set(per_model_results["nn"]["final_features"]))
    log(f"intersection of 3 per-model sets ({len(intersection)}): {intersection}")
    log(f"union of 3 per-model sets ({len(union)}): {union}")

    summary = dict(
        starting_candidates=candidates,
        correlation_pruned=pruned,
        per_model_final={m: per_model_results[m]["final_features"] for m in MODEL_NAMES},
        joint_final=joint_result["final_features"],
        intersection_of_per_model=intersection,
        union_of_per_model=union,
    )
    with open(f"{OUT_DIR}/backward_elim_summary.json", "w") as fh:
        json.dump(summary, fh, indent=2, default=str)

    log("BACKWARD ELIMINATION DONE")


if __name__ == "__main__":
    main()
