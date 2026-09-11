"""Assemble the final HTML report, inlining every figure as a base64 data
URI (Artifacts can't reference local image files)."""
import base64
import json
from pathlib import Path

ROOT = Path(__file__).parent
R_FIG = ROOT.parent / "report_figures"
PY_FIG = ROOT / "results" / "figures"
OUT = ROOT / "report.html"


def b64(path: Path) -> str:
    return base64.b64encode(path.read_bytes()).decode("ascii")


def img_tag(path: Path, alt: str, css_class: str = "fig") -> str:
    return f'<img class="{css_class}" src="data:image/png;base64,{b64(path)}" alt="{alt}">'


with open(ROOT / "results" / "model_comparison.json") as fh:
    results = json.load(fh)

figures = {
    "final_counts": img_tag(R_FIG / "final_row_count_comparison.png", "Final training row count comparison"),
    "ablation": img_tag(R_FIG / "filter_ablation.png", "QA filter ablation"),
    "vote_frac": img_tag(R_FIG / "band_ransac_vote_frac.png", "Band RANSAC vote fraction distribution"),
    "sites_map": img_tag(R_FIG / "retained_sites_map.png", "Map of retained sites"),
    "pred_vs_obs": img_tag(PY_FIG / "pred_vs_obs.png", "Predicted vs observed Secchi depth by model"),
    "comparison_bars": img_tag(PY_FIG / "model_comparison_bars.png", "Model comparison metrics"),
    "residuals_huc4": img_tag(PY_FIG / "residuals_by_huc4.png", "Residuals by HUC4"),
    "timeseries": img_tag(PY_FIG / "timeseries_examples.png", "Timeseries examples"),
    "weighted_bars": img_tag(PY_FIG / "weighted_vs_unweighted_rmse.png", "Weighted vs unweighted RMSE by SDD range"),
    "weighted_pred_vs_obs": img_tag(PY_FIG / "weighted_pred_vs_obs.png", "Weighted vs unweighted predicted vs observed"),
    "fold_diagnostics": img_tag(PY_FIG / "fold_diagnostics.png", "Per-fold train vs validation RMSE"),
}

with open(ROOT / "report_template.html") as fh:
    html = fh.read()

for key, tag in figures.items():
    html = html.replace("{{" + key + "}}", tag)

OUT.write_text(html)
print(f"wrote {OUT} ({len(html)/1e6:.1f} MB)")
