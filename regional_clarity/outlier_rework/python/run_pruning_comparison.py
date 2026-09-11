"""Compare the three redundancy-pruning methods across all 4 feature
groups, on train_val only (never touches the test partition - this is
still feature *selection*, which is model-development, not evaluation).
No model training here - this is pure correlation-structure comparison,
fast enough to run for every (method, feature_group) combination.
"""
import json

import pandas as pd

import features
import pruning_methods
import spatial_cv as cv
from run_feature_group_model import SITE_COLS, weather_cols

GROUPS = ["optical", "optical+site", "optical+weather", "optical+site+weather"]


def main():
    df = cv.load("data/modeling_dataset_expanded.parquet")
    df = features.add_spectral_indices(df)
    all_extra = set(SITE_COLS) | set(weather_cols(df))
    optical_feats = [f for f in features.candidate_feature_list(df) if f not in all_extra]
    train_val, _ = cv.train_val_test_split(df)

    results = {}
    rows = []
    for group in GROUPS:
        extra = []
        if "site" in group:
            extra += SITE_COLS
        if "weather" in group:
            extra += weather_cols(df)
        candidates = optical_feats + extra
        results[group] = {"n_candidates": len(candidates)}
        for method_name, method_fn in pruning_methods.METHODS.items():
            kept = method_fn(train_val, candidates, cv.TARGET)
            results[group][method_name] = sorted(kept)
            rows.append({"feature_group": group, "method": method_name,
                        "n_candidates": len(candidates), "n_kept": len(kept)})
            print(f"{group} / {method_name}: {len(candidates)} -> {len(kept)}")

    pd.DataFrame(rows).to_csv("results/pruning_comparison_counts.csv", index=False)
    with open("results/pruning_comparison_features.json", "w") as fh:
        json.dump(results, fh, indent=2)
    print("done")


if __name__ == "__main__":
    main()
