"""Assemble the partition-sensitivity report, inlining figures as base64
data URIs (same approach as outlier_investigation/python/03_build_report.py -
placeholders sit inside the template's own <img src="..."> tags, so each
substitution must be a bare data URI, not a full <img> tag)."""
import base64
from pathlib import Path

ROOT = Path(__file__).parent
FIG_DIR = ROOT.parent / "figures"
OUT = ROOT.parent / "report.html"


def data_uri(path: Path) -> str:
    b64 = base64.b64encode(path.read_bytes()).decode("ascii")
    return f"data:image/png;base64,{b64}"


figures = {
    "rotation_by_partition": data_uri(FIG_DIR / "rotation_by_partition.png"),
    "cluster_concentration": data_uri(FIG_DIR / "cluster_concentration.png"),
    "scheme_spread": data_uri(FIG_DIR / "scheme_spread.png"),
    "feature_stability": data_uri(FIG_DIR / "feature_stability.png"),
}

html = (ROOT / "report_template.html").read_text()
for key, tag in figures.items():
    html = html.replace("{{" + key + "}}", tag)

remaining = html.count("{{")
assert remaining == 0, f"{remaining} unfilled placeholders remain"

OUT.write_text(html)
print(f"wrote {OUT} ({len(html)/1e6:.2f} MB)")
