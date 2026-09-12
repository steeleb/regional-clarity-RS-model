"""Run the leave-one-partition-out rotation experiment across every
partition scheme built by 01_build_partition_schemes.py.

For a given scheme and a given held-out test partition, the other 4
partitions become leave-one-out CV folds (identical structure to
spatial_cv.py / run_final_evaluation.py in outlier_rework); one model is
trained per fold with the fixed, already-tuned final hyperparameters (no
retuning - isolates the effect of partitioning from model selection), the
4 fold models' predictions on the held-out test partition are averaged,
and test-set metrics are recorded. Every partition takes a turn as the
test set (the "circulate the partitions" ask), for:

  - part_huc4_baseline (5 rotations - today's actual grouping)
  - part_huc8_greedy   (5 rotations - same greedy algorithm, HUC8 units)
  - part_huc8_rand_seed{1..10} (5 rotations each - randomized-but-balanced
    HUC8 grouping, to see how much a given scheme's rotation results move
    around with a different, equally-plausible random assignment)

xgboost + lightgbm only (matching the user's explicit ask), same SDD
weighting (k=2.0) and intersection-16 feature set as the production
pipeline. ~15s/rotation for both models combined (benchmarked before this
run), ~900s total for all 60 rotations - run inline, no backgrounding
needed.
"""
import json
import sys
import time
from dataclasses import dataclass
from pathlib import Path

import pandas as pd

REWORK_DIR = Path(__file__).resolve().parents[2] / "outlier_rework" / "python"
sys.path.insert(0, str(REWORK_DIR))
import metrics as M  # noqa: E402
import model_lgb  # noqa: E402
import model_xgb  # noqa: E402
from weighting import make_sdd_weight_fn  # noqa: E402

DATA_PATH = Path(__file__).resolve().parents[1] / "data" / "modeling_dataset_with_schemes.parquet"
OUT_DIR = Path(__file__).resolve().parents[1] / "data"
TARGET = "harmonized_value"

INTERSECTION16 = ["BG", "MNDWI", "NDSSI", "NDVI", "NDWI", "NR", "atm_corr_LaSRC",
                   "catchment_area_sqkm", "fai", "nir_corr7", "pct_cropland_2006",
                   "pct_forest_2006", "pct_urban_2006", "pct_wetland_2006",
                   "red_corr7", "temp_corr7"]

MODELS = {"xgboost": model_xgb, "lightgbm": model_lgb}
N_SEEDS = 10


@dataclass
class Fold:
    train: pd.DataFrame
    val: pd.DataFrame


def load_final_params(model_name):
    with open(REWORK_DIR / "results" / f"final_tune_{model_name}.json") as fh:
        return json.load(fh)["best_params"]


def run_rotation(df, part_col, test_part, model_name, params, weight_fn):
    mod = MODELS[model_name]
    test = df[df[part_col] == test_part]
    train_val = df[df[part_col] != test_part]
    cv_parts = sorted(train_val[part_col].unique())

    folds = [Fold(train=train_val[train_val[part_col] != p],
                   val=train_val[train_val[part_col] == p])
             for p in cv_parts]
    fold_models = mod.train_fold_models(folds, INTERSECTION16, TARGET, params, weight_fn=weight_fn)
    pred = mod.predict_ensemble(fold_models, test[INTERSECTION16])
    m = M.all_metrics(test[TARGET].values, pred)
    m.update(model=model_name, part_col=part_col, test_part=int(test_part),
             n_cv_folds=len(cv_parts))
    return m


def scheme_list():
    schemes = ["part_huc4_baseline", "part_huc8_greedy"]
    schemes += [f"part_huc8_rand_seed{s}" for s in range(1, N_SEEDS + 1)]
    return schemes


def main():
    df = pd.read_parquet(DATA_PATH)
    weight_fn = make_sdd_weight_fn(k=2.0)
    params = {name: load_final_params(name) for name in MODELS}

    results = []
    t_start = time.time()
    schemes = scheme_list()
    total_rotations = sum(df[s].nunique() for s in schemes) * len(MODELS)
    done = 0

    for part_col in schemes:
        test_parts = sorted(df[part_col].unique())
        for test_part in test_parts:
            for model_name in MODELS:
                t0 = time.time()
                m = run_rotation(df, part_col, test_part, model_name,
                                  params[model_name], weight_fn)
                dt = time.time() - t0
                done += 1
                print(f"[{done}/{total_rotations}] {part_col} test={test_part} "
                      f"{model_name}: rmse={m['rmse']:.4f} mae={m['mae']:.4f} "
                      f"n={m['n']} ({dt:.1f}s)", flush=True)
                results.append(m)

    print(f"\ntotal runtime: {time.time() - t_start:.1f}s")
    out = pd.DataFrame(results)
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    out.to_csv(OUT_DIR / "rotation_results.csv", index=False)
    print(f"wrote {OUT_DIR / 'rotation_results.csv'} ({len(out)} rows)")


if __name__ == "__main__":
    main()
