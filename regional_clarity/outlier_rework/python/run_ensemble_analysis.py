"""Does stacking XGBoost + LightGBM + NN improve on the best single model?

Two combination rules, both fit (or fixed) without ever touching the test
set:
 - simple average: fixed 1/3-1/3-1/3 weights, no fitting at all.
 - stacked (NNLS): non-negative least-squares weights fit on the pooled
   out-of-fold CV predictions (run_oof_predictions.py), i.e. exactly the
   kind of validation data model selection is supposed to use - not the
   held-out test partition.

Both rules are then applied ONCE to the test-set predictions already saved
by run_weighted_variant.py (each model's SDD-weighted variant, its best
single-model version) to get the final, one-shot test comparison.
"""
import json

import numpy as np
import pandas as pd
from scipy.optimize import nnls

import metrics as M

OUT_DIR = "results"
MODELS = ["xgboost", "lightgbm", "nn"]


def main():
    # ---- fit combination weights on OOF (CV) predictions only ----
    oof = {}
    for m in MODELS:
        d = pd.read_parquet(f"{OUT_DIR}/oof_predictions_{m}.parquet")
        oof[m] = d.set_index(["siteSR_id", "date", "part"])["pred"]
    y_oof = pd.read_parquet(f"{OUT_DIR}/oof_predictions_{MODELS[0]}.parquet") \
        .set_index(["siteSR_id", "date", "part"])["y"]

    oof_matrix = pd.concat(oof, axis=1)
    oof_matrix.columns = MODELS
    assert not oof_matrix.isna().any().any(), "OOF predictions misaligned across models"

    print(f"OOF matrix: {oof_matrix.shape}")
    print("individual OOF RMSE:", {m: M.rmse(y_oof, oof_matrix[m]) for m in MODELS})

    # simple average, sanity-checked on OOF
    simple_avg_oof = oof_matrix.mean(axis=1)
    print("simple-average OOF RMSE:", M.rmse(y_oof, simple_avg_oof))

    # NNLS stacking weights, fit on OOF only
    w, _ = nnls(oof_matrix.values, y_oof.values)
    w_normalized = w / w.sum() if w.sum() > 0 else w
    print("NNLS raw weights:", dict(zip(MODELS, w)))
    print("NNLS normalized weights (sum=1):", dict(zip(MODELS, w_normalized)))
    stacked_oof = oof_matrix.values @ w
    print("NNLS-stacked OOF RMSE:", M.rmse(y_oof.values, stacked_oof))

    # ---- apply both fixed rules to the test set, exactly once ----
    test = {}
    for m in MODELS:
        d = pd.read_parquet(f"{OUT_DIR}/weighted_predictions_{m}.parquet")
        test[m] = d["pred_weighted"]
    test_matrix = pd.concat(test, axis=1)
    test_matrix.columns = MODELS
    y_test = pd.read_parquet(f"{OUT_DIR}/weighted_predictions_{MODELS[0]}.parquet")["harmonized_value"]

    results = {}
    for m in MODELS:
        results[f"{m} (SDD-weighted, solo)"] = M.all_metrics(y_test, test_matrix[m])
    results["simple average (3-model)"] = M.all_metrics(y_test, test_matrix.mean(axis=1))
    results["NNLS-stacked (3-model)"] = M.all_metrics(y_test, test_matrix.values @ w)

    summary = pd.DataFrame(results).T
    print("\n=== TEST SET: individual models vs. ensembles ===")
    print(summary.to_string())
    summary.to_csv(f"{OUT_DIR}/ensemble_test_comparison.csv")

    with open(f"{OUT_DIR}/ensemble_weights.json", "w") as fh:
        json.dump({"nnls_raw": dict(zip(MODELS, w.tolist())),
                    "nnls_normalized": dict(zip(MODELS, w_normalized.tolist())),
                    "oof_rmse": {m: M.rmse(y_oof, oof_matrix[m]) for m in MODELS},
                    "oof_rmse_simple_avg": M.rmse(y_oof, simple_avg_oof),
                    "oof_rmse_nnls": M.rmse(y_oof.values, stacked_oof)}, fh, indent=2)


if __name__ == "__main__":
    main()
