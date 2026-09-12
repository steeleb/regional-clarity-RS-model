"""Spot-check: would backward feature elimination and final hyperparameter
tuning have landed on a different result if the problem HUCs (1701, 1407)
had been distributed across partitions differently - i.e., if the
production pipeline had used a different partition scheme instead of
today's HUC4 grouping?

This is deliberately NOT a full re-run of the pipeline. Both the
correlation-pruned 22-candidate starting pool and each model's fixed
"optical+site+weather" hyperparameters (used to hold hyperparameters
constant during elimination, exactly as production did) are frozen at
their actual production values - the only thing that changes between
"production" and a spot-check scheme is which rows land in which of the
4 CV folds vs. the held-out test partition. That isolates the question
actually being asked: does partition *composition* change these upstream
decisions, holding everything else fixed?

Two alternate schemes are tested against the production baseline:
  - part_huc8_greedy: the deterministic HUC8 regrouping from the earlier
    partition-sensitivity work. Under this scheme, keeping partition 5 as
    the designated test partition, only 24% of Lake Powell's rows (vs.
    89% today) and 21% of HUC4 1701's rows (vs. 100% today, though 1701
    was always entirely within the CV pool - it's *how* it's spread across
    the 4 CV folds that changes) end up outside the CV pool.
  - part_huc8_rand_seed3: the random-HUC8 seed with the lowest HUC4 1701
    concentration found in the earlier sweep (43.9%), as a contrasting,
    more different draw.

xgboost + lightgbm only, matching this project's established scope (NN
excluded - avoids torch/OpenMP process-isolation overhead for what's
meant to be a fast triage check, not a NN architecture reconsideration).
"""
import json
import random
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

REWORK_DIR = Path(__file__).resolve().parents[2] / "outlier_rework" / "python"
sys.path.insert(0, str(REWORK_DIR))
import backward_elim as be  # noqa: E402
import metrics as M  # noqa: E402
import model_lgb  # noqa: E402
import model_xgb  # noqa: E402
from spatial_cv import Fold, TARGET  # noqa: E402

DATA_PATH = Path(__file__).resolve().parents[1] / "data" / "modeling_dataset_with_schemes.parquet"
OUT_DIR = Path(__file__).resolve().parents[1] / "data"

MODELS = {"xgboost": model_xgb, "lightgbm": model_lgb}
TOL = 0.005
ELIM_SEED = 47
TUNE_SEED = 47
N_TUNE_TRIALS = 40

SCHEMES_TO_TEST = ["part_huc8_greedy", "part_huc8_rand_seed3"]

be.register_model("xgboost", model_xgb)
be.register_model("lightgbm", model_lgb)


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def load_json(name):
    with open(REWORK_DIR / "results" / name) as fh:
        return json.load(fh)


def build_folds(df, part_col, test_part=5):
    train_val = df[df[part_col] != test_part]
    cv_parts = sorted(train_val[part_col].unique())
    return [Fold(part=p, train=train_val[train_val[part_col] != p].reset_index(drop=True),
                 val=train_val[train_val[part_col] == p].reset_index(drop=True))
            for p in cv_parts]


def gap_aware_tune(mod, folds, feats, target, n_trials, seed, val_tolerance=0.02):
    """Ported verbatim from outlier_rework/python/run_final_tuning.py -
    not imported directly to avoid that module's top-level `import
    model_nn` (torch), which this spot-check's xgboost/lightgbm-only scope
    deliberately avoids loading."""
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


def main():
    df = pd.read_parquet(DATA_PATH)

    elim_summary = load_json("backward_elim_summary.json")
    pruned_candidates = elim_summary["correlation_pruned"]
    prod_final = elim_summary["per_model_final"]

    fg_params = {m: load_json(f"fg_result_{m}_optical_site_weather.json")["best_params"] for m in MODELS}

    intersection16 = elim_summary["intersection_of_per_model"]
    prod_tune = {m: load_json(f"final_tune_{m}.json") for m in MODELS}

    all_results = {"elimination": {}, "tuning": {}}

    for scheme in SCHEMES_TO_TEST:
        log(f"##### scheme: {scheme} #####")
        folds = build_folds(df, scheme, test_part=5)
        log(f"fold sizes: {[(f.part, len(f.train), len(f.val)) for f in folds]}")

        all_results["elimination"][scheme] = {}
        all_results["tuning"][scheme] = {}

        for name, mod in MODELS.items():
            log(f"=== [{scheme}] backward elimination: {name} ===")
            result = be.backward_eliminate(name, folds, TARGET, fg_params[name], pruned_candidates,
                                            weight_fn=None, tol=TOL, seed=ELIM_SEED, log=log)
            final_feats = result["final_features"]
            prod_feats = prod_final[name]
            same = set(final_feats) == set(prod_feats)
            log(f"[{scheme}] {name} final set ({len(final_feats)}): {final_feats}")
            log(f"[{scheme}] {name} vs production ({len(prod_feats)} feats): "
                f"{'IDENTICAL' if same else 'DIFFERENT'} - "
                f"added={sorted(set(final_feats)-set(prod_feats))}, "
                f"dropped={sorted(set(prod_feats)-set(final_feats))}")
            all_results["elimination"][scheme][name] = {
                "final_features": final_feats, "path": result["path"], "same_as_production": same,
            }

            log(f"=== [{scheme}] gap-aware tuning: {name} (intersection-16, frozen) ===")
            tune_result = gap_aware_tune(mod, folds, intersection16, TARGET,
                                         n_trials=N_TUNE_TRIALS, seed=TUNE_SEED)
            log(f"[{scheme}] {name} chosen params: {tune_result['best_params']}")
            log(f"[{scheme}] {name} val_rmse={tune_result['best_score']:.4f} "
                f"gap={tune_result['best_gap']:.4f} (production: "
                f"val_rmse={prod_tune[name]['best_score']:.4f} gap={prod_tune[name]['best_gap']:.4f})")
            all_results["tuning"][scheme][name] = tune_result

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    with open(OUT_DIR / "upstream_decision_spotcheck.json", "w") as fh:
        json.dump(all_results, fh, indent=2, default=str)
    log(f"wrote {OUT_DIR / 'upstream_decision_spotcheck.json'}")
    log("DONE")


if __name__ == "__main__":
    main()
