"""Combine all 12 (model x feature_group) SHAP results into one summary
table: how much of the model's total attribution comes from optical vs.
site vs. weather features, in each configuration."""
import json

import pandas as pd

OUT_DIR = "results"
MODELS = ["xgboost", "lightgbm", "nn"]
GROUPS = ["optical", "optical+site", "optical+weather", "optical+site+weather"]


def main():
    rows = []
    top_features = {}
    weather_directions = {}
    for m in MODELS:
        for g in GROUPS:
            tag = g.replace("+", "_")
            try:
                with open(f"{OUT_DIR}/shap_{m}_{tag}.json") as fh:
                    r = json.load(fh)
            except FileNotFoundError:
                continue
            pct = r["group_contribution_pct"]
            rows.append({"model": m, "feature_group": g,
                        "optical_pct": pct.get("optical", 0),
                        "site_pct": pct.get("site", 0),
                        "weather_pct": pct.get("weather", 0)})
            top_features[(m, g)] = r["per_feature"][:8]
            if r["weather_value_shap_correlation"]:
                weather_directions[(m, g)] = r["weather_value_shap_correlation"]

    summary = pd.DataFrame(rows)
    summary.to_csv(f"{OUT_DIR}/shap_group_contribution.csv", index=False)
    print(summary.to_string(index=False))

    print("\n=== top features by mean|SHAP| ===")
    for (m, g), feats in top_features.items():
        print(f"{m} / {g}:")
        for f in feats:
            print(f"    {f['feature']:20s} ({f['group']:8s}) mean|SHAP|={f['mean_abs_shap']:.4f}")

    print("\n=== weather value-SHAP correlation (negative = higher value -> lower predicted clarity) ===")
    for (m, g), d in weather_directions.items():
        print(f"{m} / {g}: {d}")


if __name__ == "__main__":
    main()
