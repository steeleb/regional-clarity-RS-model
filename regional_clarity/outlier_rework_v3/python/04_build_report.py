"""Assemble the v3 workflow report, inlining figures as base64 data URIs
(same pattern as outlier_investigation/partition_sensitivity: placeholders
sit inside the template's own <img src="..."> tags, so each substitution
must be a bare data URI, not a full <img> tag)."""
import base64
from pathlib import Path

ROOT = Path(__file__).parent
FIG_DIR = ROOT.parent / "figures"
OUT = ROOT.parent / "report.html"


def data_uri(path: Path) -> str:
    b64 = base64.b64encode(path.read_bytes()).decode("ascii")
    return f"data:image/png;base64,{b64}"


figures = {
    "feature_stability_gapaware": data_uri(FIG_DIR / "v3_feature_stability_gapaware.png"),
    "controlled_comparison": data_uri(FIG_DIR / "v3_controlled_comparison.png"),
    "per_seed_rmse": data_uri(FIG_DIR / "v3_per_seed_rmse.png"),
    "pred_vs_obs": data_uri(FIG_DIR / "v3_pred_vs_obs.png"),
    "compression_bias": data_uri(FIG_DIR / "v3_compression_bias.png"),
    "huc4_rmse": data_uri(FIG_DIR / "v3_huc4_rmse.png"),
    "weighting_train_val_test": data_uri(FIG_DIR / "v3_weighting_train_val_test.png"),
    "three_way_comparison": data_uri(FIG_DIR / "v3_three_way_comparison.png"),
}

html = (ROOT / "report_template.html").read_text()
for key, tag in figures.items():
    html = html.replace("{{" + key + "}}", tag)

remaining = html.count("{{")
assert remaining == 0, f"{remaining} unfilled placeholders remain"

OUT.write_text(html)
print(f"wrote {OUT} ({len(html)/1e6:.2f} MB)")
