"""Does SDD-weighted training (k=2.0, the scheme v1 used and v3 re-tested)
help any of this project's three arms? Reuses each seed's ALREADY-SELECTED
feature set and ALREADY gap-aware-tuned hyperparameters (no re-elimination,
no re-tuning - same design as outlier_rework_v3/python/06_weighting_experiment.py)
and retrains final fold models twice per seed/model: unweighted and
SDD-weighted, evaluated on that arm's own fixed holdout.

Run once per --arm (ls7ref, ls8ref, l89only) - each arm's holdout composition
and bias pattern differ enough (see report.html) that a single shared answer
isn't assumed going in.
"""
import argparse
import json
import sys
import time
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd

REWORK_DIR = Path(__file__).resolve().parents[2] / "outlier_rework" / "python"
sys.path.insert(0, str(REWORK_DIR))
import metrics as M  # noqa: E402
import model_lgb  # noqa: E402
import model_xgb  # noqa: E402
from weighting import make_sdd_weight_fn  # noqa: E402

sys.path.insert(0, str(Path(__file__).resolve().parent))
import features_ls8 as features  # noqa: E402

DATA_PATH = Path(__file__).resolve().parents[1] / "data" / "base_with_splits.parquet"
RESULTS_ROOT = Path(__file__).resolve().parents[1] / "results"
TARGET = "harmonized_value"
MODELS = {"xgboost": model_xgb, "lightgbm": model_lgb}
ENSEMBLE_SEEDS = [501, 502, 503, 504, 505]
WEIGHT_K = 2.0

ARM_CONFIG = {
    "ls7ref": dict(reference="corr7", mission_filter="all"),
    "ls8ref": dict(reference="corr8", mission_filter="all"),
    "l89only": dict(reference="corr8", mission_filter="l8_only"),
}


@dataclass
class Fold:
    part: int
    train: pd.DataFrame
    val: pd.DataFrame


def log(arm, msg):
    print(f"[{time.strftime('%H:%M:%S')}][{arm}] {msg}", flush=True)


def build_folds(df, seed):
    col = f"cvfold_seed{seed}"
    cv_pool = df[~df["is_holdout"]]
    parts = sorted(cv_pool[col].dropna().unique())
    return [Fold(part=int(p),
                  train=cv_pool[cv_pool[col] != p].reset_index(drop=True),
                  val=cv_pool[cv_pool[col] == p].reset_index(drop=True))
            for p in parts]


def load_seed_config(arm, seed, model_name):
    with open(RESULTS_ROOT / arm / f"seed{seed}" / "backward_elim_summary.json") as fh:
        feats = json.load(fh)["intersection_of_per_model"]
    with open(RESULTS_ROOT / arm / f"seed{seed}" / f"final_tune_{model_name}.json") as fh:
        params = json.load(fh)["best_params"]
    return feats, params


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--arm", choices=list(ARM_CONFIG), required=True)
    arm = ap.parse_args().arm
    cfg = ARM_CONFIG[arm]

    df = pd.read_parquet(DATA_PATH)
    if cfg["mission_filter"] == "l8_only":
        df = df[df["mission"].isin(["LC08", "LC09"])].reset_index(drop=True)
    df = features.add_spectral_indices(df, cfg["reference"])

    test = df[df["is_holdout"]].reset_index(drop=True)
    weight_fn = make_sdd_weight_fn(k=WEIGHT_K)
    top_quartile_cut = float(np.quantile(df[~df["is_holdout"]][TARGET], 0.75))
    log(arm, f"top-quartile SDD cutoff (from this arm's own CV pool): {top_quartile_cut:.3f} m")

    all_rows = []
    fold_gap_rows = []

    for seed in ENSEMBLE_SEEDS:
        folds = build_folds(df, seed)
        for model_name, mod in MODELS.items():
            feats, params = load_seed_config(arm, seed, model_name)
            for weighted in [False, True]:
                wfn = weight_fn if weighted else None
                tag = "weighted" if weighted else "unweighted"

                fold_models = []
                for f in folds:
                    m = mod.train_fold_models([f], feats, TARGET, params, weight_fn=wfn)[0]
                    fold_models.append(m)
                    train_pred = mod.predict_ensemble([m], f.train[feats])
                    val_pred = mod.predict_ensemble([m], f.val[feats])
                    fold_gap_rows.append(dict(
                        seed=seed, model=model_name, weighted=weighted, fold=f.part,
                        train_rmse=M.rmse(f.train[TARGET].values, train_pred),
                        val_rmse=M.rmse(f.val[TARGET].values, val_pred),
                    ))

                test_pred = mod.predict_ensemble(fold_models, test[feats])
                r = test[["siteSR_id", "date", "HUC4"]].copy()
                r["seed"] = seed
                r["model"] = model_name
                r["weighted"] = weighted
                r["y"] = test[TARGET].values
                r["pred"] = test_pred
                all_rows.append(r)
                log(arm, f"seed{seed}/{model_name}/{tag}: test_rmse={M.rmse(r['y'], r['pred']):.4f}")

    out_dir = RESULTS_ROOT / arm
    all_df = pd.concat(all_rows, ignore_index=True)
    all_df.to_parquet(out_dir / "weighting_experiment_predictions.parquet")
    gap_df = pd.DataFrame(fold_gap_rows)
    gap_df.to_csv(out_dir / "weighting_experiment_fold_gaps.csv", index=False)

    summary = {}
    for weighted in [False, True]:
        tag = "weighted" if weighted else "unweighted"
        sub = all_df[all_df["weighted"] == weighted]
        ens = sub.groupby(["siteSR_id", "date"]).agg(y=("y", "first"), pred=("pred", "mean")).reset_index()
        overall = M.all_metrics(ens["y"], ens["pred"])
        top_mask = ens["y"] >= top_quartile_cut
        top_metrics = M.all_metrics(ens.loc[top_mask, "y"], ens.loc[top_mask, "pred"])
        rest_metrics = M.all_metrics(ens.loc[~top_mask, "y"], ens.loc[~top_mask, "pred"])
        summary[tag] = dict(overall=overall, top_quartile=top_metrics, rest=rest_metrics)
        log(arm, f"{tag} ENSEMBLE: overall_rmse={overall['rmse']:.4f} "
                 f"top_quartile_rmse={top_metrics['rmse']:.4f} (n={top_mask.sum()}) "
                 f"rest_rmse={rest_metrics['rmse']:.4f} (n={(~top_mask).sum()})")

    gap_summary = gap_df.groupby("weighted").apply(
        lambda g: pd.Series({"mean_gap": (g["val_rmse"] - g["train_rmse"]).mean(),
                              "sd_gap": (g["val_rmse"] - g["train_rmse"]).std()})
    ).to_dict()

    seed_rmse = all_df.groupby(["seed", "model", "weighted"]).apply(
        lambda g: M.rmse(g["y"], g["pred"])
    ).reset_index(name="rmse")
    seed_variance = seed_rmse.groupby("weighted")["rmse"].agg(["mean", "std"]).to_dict()

    log(arm, f"fold train-val gap by weighting: {gap_summary}")
    log(arm, f"cross-seed test-RMSE variance by weighting: {seed_variance}")

    summary["fold_gap"] = gap_summary
    summary["cross_seed_variance"] = seed_variance
    summary["top_quartile_cut"] = top_quartile_cut
    with open(out_dir / "weighting_experiment_summary.json", "w") as fh:
        json.dump(summary, fh, indent=2, default=str)
    log(arm, f"wrote {out_dir / 'weighting_experiment_summary.json'}")


if __name__ == "__main__":
    main()
