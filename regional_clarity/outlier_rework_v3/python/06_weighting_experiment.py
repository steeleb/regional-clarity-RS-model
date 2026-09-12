"""Does SDD-weighted training (k=2.0, the scheme already used at v1's/v3's
final-training step) actually help within the v3 5-seed ensemble, or was it
just inherited by convention? Reuses each seed's ALREADY-SELECTED feature
set and ALREADY gap-aware-tuned hyperparameters (no re-elimination, no
re-tuning - same "reuse the tuned config, only change weighting" design as
outlier_rework's run_weighted_variant.py) and retrains final fold models
twice per seed/model: unweighted and SDD-weighted.

Evaluated two ways, per the user's framing ("if it improves the folds and
test reliability"):
  - accuracy: overall holdout RMSE/MAE, and top-quartile-SDD vs. rest
    (the same split run_weighted_variant.py used, since weighting is
    specifically meant to help the sparse high-SDD tail)
  - reliability: per-fold train-val gap (does weighting tighten or widen
    the generalization gap within a seed's own CV folds?) and cross-seed
    variance (does it make the 5 seeds agree with each other more, or
    less, on holdout RMSE?)
"""
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

DATA_PATH = Path(__file__).resolve().parents[1] / "data" / "base_with_splits.parquet"
RESULTS_ROOT = Path(__file__).resolve().parents[1] / "results"
TARGET = "harmonized_value"
MODELS = {"xgboost": model_xgb, "lightgbm": model_lgb}
ENSEMBLE_SEEDS = [501, 502, 503, 504, 505]
WEIGHT_K = 2.0


@dataclass
class Fold:
    part: int
    train: pd.DataFrame
    val: pd.DataFrame


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def build_folds(df, seed):
    col = f"cvfold_seed{seed}"
    cv_pool = df[~df["is_holdout"]]
    parts = sorted(cv_pool[col].dropna().unique())
    return [Fold(part=int(p),
                  train=cv_pool[cv_pool[col] != p].reset_index(drop=True),
                  val=cv_pool[cv_pool[col] == p].reset_index(drop=True))
            for p in parts]


def load_seed_config(seed, model_name):
    with open(RESULTS_ROOT / f"seed{seed}" / "backward_elim_summary.json") as fh:
        feats = json.load(fh)["intersection_of_per_model"]
    with open(RESULTS_ROOT / f"seed{seed}" / f"final_tune_{model_name}.json") as fh:
        params = json.load(fh)["best_params"]
    return feats, params


def main():
    df = pd.read_parquet(DATA_PATH)
    test = df[df["is_holdout"]].reset_index(drop=True)
    weight_fn = make_sdd_weight_fn(k=WEIGHT_K)
    top_quartile_cut = float(np.quantile(df[~df["is_holdout"]][TARGET], 0.75))
    log(f"top-quartile SDD cutoff (from full CV pool): {top_quartile_cut:.3f} m")

    all_rows = []          # holdout predictions, both weighting configs
    fold_gap_rows = []      # per-fold train/val rmse, both configs

    for seed in ENSEMBLE_SEEDS:
        folds = build_folds(df, seed)
        for model_name, mod in MODELS.items():
            feats, params = load_seed_config(seed, model_name)
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
                log(f"seed{seed}/{model_name}/{tag}: test_rmse={M.rmse(r['y'], r['pred']):.4f}")

    all_df = pd.concat(all_rows, ignore_index=True)
    all_df.to_parquet(RESULTS_ROOT / "weighting_experiment_predictions.parquet")
    gap_df = pd.DataFrame(fold_gap_rows)
    gap_df.to_csv(RESULTS_ROOT / "weighting_experiment_fold_gaps.csv", index=False)

    # ---- accuracy: ensemble (5 seeds x 2 models) overall + by SDD group ----
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
        log(f"{tag} ENSEMBLE: overall_rmse={overall['rmse']:.4f} "
            f"top_quartile_rmse={top_metrics['rmse']:.4f} (n={top_mask.sum()}) "
            f"rest_rmse={rest_metrics['rmse']:.4f} (n={(~top_mask).sum()})")

    # ---- reliability: fold-level gap, and cross-seed test-RMSE variance ----
    gap_summary = gap_df.groupby("weighted").apply(
        lambda g: pd.Series({"mean_gap": (g["val_rmse"] - g["train_rmse"]).mean(),
                              "sd_gap": (g["val_rmse"] - g["train_rmse"]).std()})
    ).to_dict()

    seed_rmse = all_df.groupby(["seed", "model", "weighted"]).apply(
        lambda g: M.rmse(g["y"], g["pred"])
    ).reset_index(name="rmse")
    seed_variance = seed_rmse.groupby("weighted")["rmse"].agg(["mean", "std"]).to_dict()

    log(f"fold train-val gap by weighting: {gap_summary}")
    log(f"cross-seed test-RMSE variance by weighting: {seed_variance}")

    summary["fold_gap"] = gap_summary
    summary["cross_seed_variance"] = seed_variance
    summary["top_quartile_cut"] = top_quartile_cut
    with open(RESULTS_ROOT / "weighting_experiment_summary.json", "w") as fh:
        json.dump(summary, fh, indent=2, default=str)
    log(f"wrote {RESULTS_ROOT / 'weighting_experiment_summary.json'}")


if __name__ == "__main__":
    main()
