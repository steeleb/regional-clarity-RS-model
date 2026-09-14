"""Assemble the outlier-HUC investigation report, inlining figures as base64
data URIs. Unlike outlier_rework/python/build_report.py, this template's
{{key}} placeholders sit inside the report's own <img src="..." alt="...">
tags (so each figure keeps a hand-written, descriptive alt), so each
placeholder must be replaced with a bare data URI - not a full <img> tag,
which would nest a second <img> inside the src attribute."""
import base64
from pathlib import Path

ROOT = Path(__file__).parent
FIG_DIR = ROOT.parent / "figures"
OUT = ROOT.parent / "report.html"


def data_uri(path: Path) -> str:
    b64 = base64.b64encode(path.read_bytes()).decode("ascii")
    return f"data:image/png;base64,{b64}"


figures = {
    "compression_bias": data_uri(FIG_DIR / "compression_bias.png"),
    "partition_composition": data_uri(FIG_DIR / "partition_composition.png"),
    "huc4_test_rmse": data_uri(FIG_DIR / "huc4_test_rmse.png"),
    "lake_powell_pred_vs_obs": data_uri(FIG_DIR / "lake_powell_pred_vs_obs.png"),
    "wbtype_test_error": data_uri(FIG_DIR / "wbtype_test_error.png"),
    "literature_context": data_uri(FIG_DIR / "literature_context.png"),
    "powell_interannual": data_uri(FIG_DIR / "powell_interannual.png"),
    "catchment_envelope_powell": data_uri(FIG_DIR / "catchment_envelope_powell.png"),
    "forest_envelope_1701": data_uri(FIG_DIR / "forest_envelope_1701.png"),
    "overview_pred_vs_obs": data_uri(FIG_DIR / "overview_pred_vs_obs.png"),
    "overview_timeseries_examples": data_uri(FIG_DIR / "overview_timeseries_examples.png"),
}

html = (ROOT / "report_template.html").read_text()
for key, tag in figures.items():
    html = html.replace("{{" + key + "}}", tag)

OUT.write_text(html)
print(f"wrote {OUT} ({len(html)/1e6:.2f} MB)")
