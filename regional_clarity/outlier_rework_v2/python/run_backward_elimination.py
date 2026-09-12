"""Backward feature elimination, gated entirely on CV (partitions 1-4).

Starting candidate pool: optical + site (now including shoreline_flag) +
the 2 antecedent-weather variables that showed a consistently, physically
sensible SHAP direction in the prior phase, reduced by flat single-pass
correlation pruning (the method already used throughout this report,
carried forward per the user's explicit choice).

Hyperparameters are held fixed throughout elimination - each model reuses
its already gap-aware-tuned optical+site+weather config from the
feature-group comparison phase - and retuned (gap-aware) only on the final
chosen feature sets, in a later step. Elimination itself never chooses
among hyperparameters, only features, so it doesn't need its own tuning
call to stay within the "gap-aware wherever hyperparameters are chosen"
rule - it just needs to start from an already gap-aware-tuned fixed config,
which it now does. No SDD-weighting during elimination either: that's a
separate, already-answered axis (helps the tree models, not the capped
NN), and mixing it in here would conflate two different decisions.

Runs three independent per-model eliminations (xgboost, lightgbm, nn) plus
one joint elimination scored across all three at once, then reports the
joint set alongside the intersection of the three independent per-model
sets - both requested as points of comparison before picking a shared
benchmark feature set.

v2 change: also writes results/selected_features.json, a single small file
that downstream final-tuning/final-evaluation/SHAP-CV scripts read instead
of each hardcoding its own copy-pasted feature-list literal (v1's
INTERSECTION16 was duplicated verbatim across 3 files with no code path
connecting it back to this script's actual output - a real drift risk,
worse now that a new feature can change which set gets selected).
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
from features import SITE_COLS

DATA_PATH = "data/modeling_dataset_expanded.parquet"
OUT_DIR = "results"

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
        log(f"{m} fixed hyperparameters (reused from gap-aware optical+site+weather tuning): {params_by_model[m]}")

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

    # single canonical feature-set file for every downstream script to read
    # (final tuning, final evaluation, SHAP-CV) instead of each hardcoding
    # its own copy of these lists. Keys are named by role, not by count -
    # v1 baked the count into the name (INTERSECTION16/joint18), which
    # silently goes stale the moment a different feature set (e.g. this
    # one, with shoreline_flag added) changes how many features survive.
    selected = dict(
        intersection=intersection,
        joint=joint_result["final_features"],
    )
    with open(f"{OUT_DIR}/selected_features.json", "w") as fh:
        json.dump(selected, fh, indent=2)
    log(f"wrote selected_features.json: intersection (n={len(intersection)}), "
        f"joint (n={len(joint_result['final_features'])})")

    log("BACKWARD ELIMINATION DONE")


if __name__ == "__main__":
    main()
