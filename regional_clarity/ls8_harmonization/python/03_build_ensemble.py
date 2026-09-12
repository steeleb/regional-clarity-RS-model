"""Build one arm's 5-seed x 2-model ensemble (40 models) from its seeds'
independently-produced holdout_predictions.parquet files (written by
02_run_seed_pipeline.py) - same aggregation pattern as
outlier_rework_v3/python/03_build_ensemble_and_baseline.py's first half.
Run once per arm (ls7ref, ls8ref, l89only).
"""
import argparse
import json
import sys
import time
from pathlib import Path

import pandas as pd

REWORK_DIR = Path(__file__).resolve().parents[2] / "outlier_rework" / "python"
sys.path.insert(0, str(REWORK_DIR))
import metrics as M  # noqa: E402

RESULTS_ROOT = Path(__file__).resolve().parents[1] / "results"
ENSEMBLE_SEEDS = [501, 502, 503, 504, 505]
MODEL_NAMES = ["xgboost", "lightgbm"]


def log(msg):
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", flush=True)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--arm", choices=["ls7ref", "ls8ref", "l89only"], required=True)
    arm = ap.parse_args().arm
    arm_dir = RESULTS_ROOT / arm

    rows = []
    for seed in ENSEMBLE_SEEDS:
        p = arm_dir / f"seed{seed}" / "holdout_predictions.parquet"
        if not p.exists():
            log(f"WARNING: missing {p}, skipping seed {seed}")
            continue
        rows.append(pd.read_parquet(p))
    all_preds = pd.concat(rows, ignore_index=True)
    all_preds.to_parquet(arm_dir / "ensemble_all_predictions.parquet")

    ensemble_pred = all_preds.groupby(["siteSR_id", "date"]).agg(y=("y", "first"), pred=("pred", "mean")).reset_index()
    ensemble_metrics = M.all_metrics(ensemble_pred["y"], ensemble_pred["pred"])
    n_seeds = all_preds["seed"].nunique()
    log(f"{arm} ENSEMBLE ({n_seeds} seeds x {all_preds['model'].nunique()} models "
        f"= {n_seeds * all_preds['model'].nunique()} models): {ensemble_metrics}")

    per_model = {}
    for m in MODEL_NAMES:
        sub = all_preds[all_preds["model"] == m].groupby(["siteSR_id", "date"]).agg(
            y=("y", "first"), pred=("pred", "mean")).reset_index()
        per_model[m] = M.all_metrics(sub["y"], sub["pred"])
        log(f"{arm} {m}-only ensemble ({n_seeds} seeds): {per_model[m]}")

    # per-HUC4 breakdown
    huc4_pred = all_preds.groupby(["siteSR_id", "date"]).agg(
        y=("y", "first"), pred=("pred", "mean"), HUC4=("HUC4", "first")).reset_index()
    per_huc4 = {}
    for huc4, sub in huc4_pred.groupby("HUC4"):
        if len(sub) < 10:
            continue
        per_huc4[huc4] = M.all_metrics(sub["y"], sub["pred"])
    log(f"{arm} per-HUC4 n>=10: {list(per_huc4.keys())}")

    summary = dict(arm=arm, n_seeds=n_seeds, n_holdout_rows=len(ensemble_pred),
                    ensemble_metrics=ensemble_metrics, per_model=per_model, per_huc4=per_huc4)
    with open(arm_dir / "ensemble_summary.json", "w") as fh:
        json.dump(summary, fh, indent=2, default=str)
    log(f"wrote {arm_dir / 'ensemble_summary.json'}")


if __name__ == "__main__":
    main()
