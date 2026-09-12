"""Combine the three per-model result files (written by run_single_model.py,
run as separate processes to avoid the xgboost/lightgbm/torch OpenMP
conflict) into one comparison table + one predictions file, matching what
a single unified run would have produced."""
import json

import pandas as pd

OUT_DIR = "results"
MODELS = ["xgboost", "lightgbm", "nn"]


def main():
    results = {"models": {}}
    all_preds = []
    for name in MODELS:
        with open(f"{OUT_DIR}/model_result_{name}.json") as fh:
            r = json.load(fh)
        results["models"][name] = {
            "best_params": r["best_params"],
            "cv_metrics": r["cv_metrics"],
            "test_metrics": r["test_metrics"],
        }
        all_preds.append(pd.read_parquet(f"{OUT_DIR}/test_predictions_{name}.parquet"))

    all_preds_df = pd.concat(all_preds, ignore_index=True)
    all_preds_df.to_parquet(f"{OUT_DIR}/test_predictions.parquet")

    with open(f"{OUT_DIR}/model_comparison.json", "w") as fh:
        json.dump(results, fh, indent=2, default=str)

    summary = pd.DataFrame({name: r["test_metrics"] for name, r in results["models"].items()}).T
    summary.to_csv(f"{OUT_DIR}/test_metrics_summary.csv")
    print(summary)

    cv_summary = pd.DataFrame({name: r["cv_metrics"] for name, r in results["models"].items()}).T
    cv_summary.to_csv(f"{OUT_DIR}/cv_metrics_summary.csv")
    print(cv_summary)


if __name__ == "__main__":
    main()
