"""Assemble the ls8_harmonization report, inlining figures as base64 data
URIs (same pattern as every prior report in this project)."""
import base64
from pathlib import Path

ROOT = Path(__file__).parent
FIG_DIR = ROOT.parent / "figures"
OUT = ROOT.parent / "report.html"


def data_uri(path: Path) -> str:
    b64 = base64.b64encode(path.read_bytes()).decode("ascii")
    return f"data:image/png;base64,{b64}"


figures = {
    "sensor_row_counts": data_uri(FIG_DIR / "sensor_row_counts.png"),
    "controlled_comparison": data_uri(FIG_DIR / "controlled_comparison.png"),
    "huc4_comparison_ab": data_uri(FIG_DIR / "huc4_comparison_ab.png"),
    "feature_stability": data_uri(FIG_DIR / "feature_stability.png"),
    "pred_vs_obs_arms": data_uri(FIG_DIR / "pred_vs_obs_arms.png"),
    "l89_basin_matched": data_uri(FIG_DIR / "l89_basin_matched.png"),
    "ls8ref_weighting_rmse": data_uri(FIG_DIR / "ls8ref_weighting_rmse.png"),
    "ls8ref_pred_vs_obs_weighted": data_uri(FIG_DIR / "ls8ref_pred_vs_obs_weighted.png"),
    "overtraining_gap_by_arm": data_uri(FIG_DIR / "overtraining_gap_by_arm.png"),
    "ls8ref_shap_aggregate": data_uri(FIG_DIR / "ls8ref_shap_aggregate.png"),
    "ls8ref_shap_by_huc4": data_uri(FIG_DIR / "ls8ref_shap_by_huc4.png"),
    "ls8ref_shap_beeswarm": data_uri(FIG_DIR / "ls8ref_shap_beeswarm.png"),
}

html = (ROOT / "report_template.html").read_text()
for key, tag in figures.items():
    html = html.replace("{{" + key + "}}", tag)

remaining = html.count("{{")
assert remaining == 0, f"{remaining} unfilled placeholders remain"

OUT.write_text(html)
print(f"wrote {OUT} ({len(html)/1e6:.2f} MB)")
