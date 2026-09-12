"""Combine the 12 (3 model x 4 feature-group) results into one comparison
table, matching against the original optical-only baseline (SDD-weighted)
already reported."""
import json

import pandas as pd

from features import SITE_COLS, WEATHER_PREFIXES

OUT_DIR = "results"
MODELS = ["xgboost", "lightgbm", "nn"]
GROUPS = ["optical", "optical+site", "optical+weather", "optical+site+weather"]


def main():
    rows = []
    for m in MODELS:
        for g in GROUPS:
            tag = g.replace("+", "_")
            with open(f"{OUT_DIR}/fg_result_{m}_{tag}.json") as fh:
                r = json.load(fh)
            rows.append({
                "model": m, "feature_group": g,
                "n_features_selected": len(r["features"]),
                "n_extra_offered": r["n_extra_feats"],
                **r["test_metrics"],
            })
    summary = pd.DataFrame(rows)
    summary.to_csv(f"{OUT_DIR}/feature_group_comparison.csv", index=False)
    print(summary.to_string(index=False))

    # baseline (original optical-only, SDD-weighted) for reference
    baseline_rows = []
    for m in MODELS:
        with open(f"{OUT_DIR}/weighted_comparison_{m}.json") as fh:
            r = json.load(fh)
        baseline_rows.append({"model": m, "feature_group": "optical (original baseline)",
                              **r["weighted_test_metrics"]})
    baseline = pd.DataFrame(baseline_rows)
    print("\n=== original baseline (for reference) ===")
    print(baseline[["model", "rmse", "r2", "bias"]].to_string(index=False))

    # which extra (non-optical) features actually survived correlation pruning?
    site_cols = set(SITE_COLS)
    print("\n=== extra (site/weather) features that survived pruning ===")
    for m in MODELS:
        for g in ["optical+site", "optical+weather", "optical+site+weather"]:
            tag = g.replace("+", "_")
            with open(f"{OUT_DIR}/fg_result_{m}_{tag}.json") as fh:
                r = json.load(fh)
            extra_kept = [f for f in r["features"]
                          if f in site_cols or f.startswith(WEATHER_PREFIXES)]
            print(f"{m} / {g}: {extra_kept if extra_kept else '(none)'}")


if __name__ == "__main__":
    main()
