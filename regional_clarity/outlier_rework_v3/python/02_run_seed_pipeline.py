"""Full per-seed v3 pipeline: feature-group comparison -> correlation
pruning -> backward elimination -> gap-aware final tuning -> final fold
training + holdout predictions. One ensemble member = one --seed.

Structurally the same stages as outlier_rework's
run_feature_group_model.py / run_backward_elimination.py /
run_final_tuning.py / run_final_evaluation.py, but:
  - reads folds from this seed's own cvfold_seed{N} column (built by
    01_build_base_and_splits.py) instead of spatial_cv.py's fixed `part`
    column - the CV pool and fixed holdout are shared across all seeds,
    only the 4-fold arrangement of the CV pool differs by seed
  - xgboost + lightgbm only (NN excluded throughout this project)
  - SITE_COLS includes the new shore_flag candidate
  - gap_aware_tune is ported inline (not imported from run_final_tuning.py)
    to avoid that module's top-level `import model_nn` (torch)

Run as its own process per seed - isolates state between seeds and makes
individual seed runs resumable/inspectable independently.
"""
import argparse
import json
import random
import sys
import time
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd

REWORK_DIR = Path(__file__).resolve().parents[2] / "outlier_rework" / "python"
sys.path.insert(0, str(REWORK_DIR))
import backward_elim as be  # noqa: E402
import features  # noqa: E402
import metrics as M  # noqa: E402
import model_lgb  # noqa: E402
import model_xgb  # noqa: E402
from weighting import make_sdd_weight_fn  # noqa: E402

DATA_PATH = Path(__file__).resolve().parents[1] / "data" / "base_with_splits.parquet"
RESULTS_ROOT = Path(__file__).resolve().parents[1] / "results"
TARGET = "harmonized_value"

MODELS = {"xgboost": model_xgb, "lightgbm": model_lgb}
N_TRIALS_GROUP = {"xgboost": 25, "lightgbm": 25}
N_TRIALS_FINAL = {"xgboost": 40, "lightgbm": 40}
ELIM_TOL = 0.005
ELIM_SEED = 47
TUNE_SEED = 47

SITE_COLS = ["elevation_m", "catchment_area_sqkm", "pct_impervious_2006",
             "pct_urban_2006", "pct_forest_2006", "pct_cropland_2006",
             "pct_wetland_2006", "shore_flag"]
WEATHER_PREFIXES = ("precip_mm_prev", "tmax_degC_prev", "tmean_degC_prev",
                    "tmin_degC_prev", "srad_Wm2_prev")
WEATHER_COLS_CURATED = ["precip_mm_prev30", "tmax_degC_prev30"]

ENSEMBLE_SEEDS = [501, 502, 503, 504, 505]
EXTRA_METADATA_COLS = {"HUC8", "holdout_part", "is_holdout"} | {f"cvfold_seed{s}" for s in ENSEMBLE_SEEDS}

be.register_model("xgboost", model_xgb)
be.register_model("lightgbm", model_lgb)


@dataclass
class Fold:
    part: int
    train: pd.DataFrame
    val: pd.DataFrame


def log(seed, msg):
    print(f"[{time.strftime('%H:%M:%S')}][seed{seed}] {msg}", flush=True)


def weather_cols(df):
    return [c for c in df.columns if c.startswith(WEATHER_PREFIXES)]


def build_folds(df, seed):
    col = f"cvfold_seed{seed}"
    cv_pool = df[~df["is_holdout"]]
    parts = sorted(cv_pool[col].dropna().unique())
    return [Fold(part=int(p),
                  train=cv_pool[cv_pool[col] != p].reset_index(drop=True),
                  val=cv_pool[cv_pool[col] == p].reset_index(drop=True))
            for p in parts]


def gap_aware_tune(mod, folds, feats, target, n_trials, seed, val_tolerance=0.02):
    """Ported from outlier_rework/python/run_final_tuning.py - see
    partition_sensitivity/python/05_upstream_decision_spotcheck.py for the
    same port, same reasoning (avoids run_final_tuning.py's top-level
    `import model_nn`)."""
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
                pure_best_val_rmse=best_val, band_size=len(band), n_trials=n_trials)


def run_feature_group_comparison(seed, df, folds, test, out_dir):
    results = {}
    all_extra_cols = set(SITE_COLS) | set(weather_cols(df)) | EXTRA_METADATA_COLS
    optical_feats = [f for f in features.candidate_feature_list(df) if f not in all_extra_cols]

    groups = {
        "optical": [],
        "optical_site": SITE_COLS,
        "optical_weather": weather_cols(df),
        "optical_site_weather": SITE_COLS + weather_cols(df),
    }
    for group_name, extra_feats in groups.items():
        candidate_feats = optical_feats + extra_feats
        train_val = pd.concat([f.train for f in folds] + [folds[0].val], ignore_index=True).drop_duplicates(
            subset=["siteSR_id", "date"])
        pruned = features.correlation_prune(train_val, candidate_feats, target=TARGET)
        for model_name, mod in MODELS.items():
            log(seed, f"=== feature-group {group_name} / {model_name} "
                      f"({len(pruned)} candidates after pruning) ===")
            t0 = time.time()
            tune_result = mod.tune(folds, pruned, TARGET, n_trials=N_TRIALS_GROUP[model_name], seed=47)
            fold_models = mod.train_fold_models(folds, pruned, TARGET, tune_result["best_params"],
                                                weight_fn=make_sdd_weight_fn(k=2.0))
            cv_preds, cv_ys = [], []
            for f, model in zip(folds, fold_models):
                cv_preds.append(mod.predict_ensemble([model], f.val[pruned]))
                cv_ys.append(f.val[TARGET].values)
            cv_metrics = M.all_metrics(np.concatenate(cv_ys), np.concatenate(cv_preds))
            test_pred = mod.predict_ensemble(fold_models, test[pruned])
            test_metrics = M.all_metrics(test[TARGET].values, test_pred)
            log(seed, f"{group_name}/{model_name}: cv_rmse={cv_metrics['rmse']:.4f} "
                      f"test_rmse={test_metrics['rmse']:.4f} ({time.time()-t0:.0f}s)")
            key = f"{model_name}_{group_name}"
            results[key] = dict(model=model_name, feature_group=group_name, features=pruned,
                                 best_params=tune_result["best_params"],
                                 cv_metrics=cv_metrics, test_metrics=test_metrics)
    with open(out_dir / "feature_group_results.json", "w") as fh:
        json.dump(results, fh, indent=2, default=str)
    return results


def run_elimination(seed, folds, fg_results, out_dir):
    df_all = pd.concat([f.train for f in folds] + [folds[0].val], ignore_index=True).drop_duplicates(
        subset=["siteSR_id", "date"])
    all_extra_cols = set(SITE_COLS) | set(weather_cols(df_all)) | EXTRA_METADATA_COLS
    optical_feats = [f for f in features.candidate_feature_list(df_all) if f not in all_extra_cols]
    candidates = optical_feats + SITE_COLS + WEATHER_COLS_CURATED
    pruned = features.correlation_prune(df_all, candidates, target=TARGET)
    log(seed, f"elimination candidate pool: {len(candidates)} -> {len(pruned)} after correlation pruning")
    log(seed, f"pruned set: {pruned}")

    fixed_params = {m: fg_results[f"{m}_optical_site_weather"]["best_params"] for m in MODELS}

    per_model = {}
    for model_name in MODELS:
        log(seed, f"=== backward elimination: {model_name} ===")
        result = be.backward_eliminate(model_name, folds, TARGET, fixed_params[model_name], pruned,
                                        weight_fn=None, tol=ELIM_TOL, seed=ELIM_SEED,
                                        log=lambda m: log(seed, m))
        per_model[model_name] = result
        log(seed, f"{model_name} final ({len(result['final_features'])}): {result['final_features']}")

    log(seed, "=== joint elimination (xgboost + lightgbm) ===")
    folds_by_model = {m: folds for m in MODELS}
    joint_result = be.joint_backward_eliminate(list(MODELS), folds_by_model, TARGET, fixed_params,
                                                pruned, weight_fn=None, tol=ELIM_TOL, seed=ELIM_SEED,
                                                log=lambda m: log(seed, m))
    log(seed, f"joint final ({len(joint_result['final_features'])}): {joint_result['final_features']}")

    intersection = sorted(set(per_model["xgboost"]["final_features"]) & set(per_model["lightgbm"]["final_features"]))
    union = sorted(set(per_model["xgboost"]["final_features"]) | set(per_model["lightgbm"]["final_features"]))
    log(seed, f"intersection ({len(intersection)}): {intersection}")

    summary = dict(
        starting_candidates=candidates, correlation_pruned=pruned,
        per_model_final={m: per_model[m]["final_features"] for m in MODELS},
        per_model_path={m: per_model[m]["path"] for m in MODELS},
        joint_final=joint_result["final_features"], joint_path=joint_result["path"],
        intersection_of_per_model=intersection, union_of_per_model=union,
    )
    with open(out_dir / "backward_elim_summary.json", "w") as fh:
        json.dump(summary, fh, indent=2, default=str)
    return intersection


def run_final_tuning(seed, folds, feats, out_dir):
    tuned = {}
    for model_name, mod in MODELS.items():
        log(seed, f"=== gap-aware tuning: {model_name} on final feature set ===")
        result = gap_aware_tune(mod, folds, feats, TARGET, n_trials=N_TRIALS_FINAL[model_name], seed=TUNE_SEED)
        log(seed, f"{model_name} chosen: {result['best_params']} "
                  f"val_rmse={result['best_score']:.4f} gap={result['best_gap']:.4f}")
        tuned[model_name] = result
        with open(out_dir / f"final_tune_{model_name}.json", "w") as fh:
            json.dump({"model": model_name, "features": feats, **result}, fh, indent=2, default=str)
    return tuned


def run_final_evaluation(seed, folds, test, feats, tuned, out_dir):
    weight_fn = make_sdd_weight_fn(k=2.0)
    summary = {}
    holdout_rows = []
    for model_name, mod in MODELS.items():
        params = tuned[model_name]["best_params"]
        fold_models = mod.train_fold_models(folds, feats, TARGET, params, weight_fn=weight_fn)

        cv_preds, cv_ys = [], []
        for f, model in zip(folds, fold_models):
            cv_preds.append(mod.predict_ensemble([model], f.val[feats]))
            cv_ys.append(f.val[TARGET].values)
        cv_metrics = M.all_metrics(np.concatenate(cv_ys), np.concatenate(cv_preds))

        test_pred = mod.predict_ensemble(fold_models, test[feats])
        test_metrics = M.all_metrics(test[TARGET].values, test_pred)
        log(seed, f"{model_name} FINAL: cv_rmse={cv_metrics['rmse']:.4f} test_rmse={test_metrics['rmse']:.4f}")

        summary[model_name] = dict(features=feats, params=params, cv_metrics=cv_metrics, test_metrics=test_metrics)

        r = test[["siteSR_id", "date", "HUC4"]].copy()
        r["seed"] = seed
        r["model"] = model_name
        r["y"] = test[TARGET].values
        r["pred"] = test_pred
        holdout_rows.append(r)

    with open(out_dir / "final_eval_summary.json", "w") as fh:
        json.dump(summary, fh, indent=2, default=str)
    holdout_df = pd.concat(holdout_rows, ignore_index=True)
    holdout_df.to_parquet(out_dir / "holdout_predictions.parquet")
    return summary


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--seed", type=int, required=True)
    seed = ap.parse_args().seed

    out_dir = RESULTS_ROOT / f"seed{seed}"
    out_dir.mkdir(parents=True, exist_ok=True)

    t_start = time.time()
    df = pd.read_parquet(DATA_PATH)
    folds = build_folds(df, seed)
    test = df[df["is_holdout"]].reset_index(drop=True)
    log(seed, f"folds: {[(f.part, len(f.train), len(f.val)) for f in folds]}, holdout n={len(test)}")

    log(seed, "### STAGE 1: feature-group comparison ###")
    fg_results = run_feature_group_comparison(seed, df, folds, test, out_dir)

    log(seed, "### STAGE 2: backward elimination ###")
    final_feats = run_elimination(seed, folds, fg_results, out_dir)

    log(seed, "### STAGE 3: gap-aware final tuning ###")
    tuned = run_final_tuning(seed, folds, final_feats, out_dir)

    log(seed, "### STAGE 4: final evaluation on fixed holdout ###")
    run_final_evaluation(seed, folds, test, final_feats, tuned, out_dir)

    log(seed, f"SEED {seed} DONE in {time.time()-t_start:.0f}s")


if __name__ == "__main__":
    main()
