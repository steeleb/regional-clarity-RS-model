"""Enforces one rule: every figure shown in Claude_models_overview.md must
be byte-identical to a figure actually embedded in that section's own
full report.html - nothing appears in the summary that isn't backed by
the report it points to.

Checked by exact content match (base64-encoding each overview figure and
searching for that string inside the section's report.html), not by
filename - so a renamed-but-identical file still passes, and a
same-named-but-different file still fails.

A handful of figures are known, deliberate exceptions (v1's SHAP figures
stand in for v2/outlier_investigation/partition_sensitivity's own
sections, which reuse v1's model/SHAP unchanged, per those sections' own
"no new SHAP" text) - listed in KNOWN_EXCEPTIONS below rather than
silently ignored.

Run after any overview.md or report edit: `python check_overview_figures.py`.
"""
import base64
import re
import sys
from pathlib import Path

ROOT = Path(__file__).parent
OVERVIEW = ROOT / "Claude_models_overview.md"

# figure path (as written in overview.md) -> reason it's exempt from
# needing to appear in ITS OWN section's report.html
KNOWN_EXCEPTIONS = {
    # (currently none - v1's own section embeds its own SHAP figures, and
    # sections 3/4 don't reference v1's SHAP figures as images, only in text)
}


def section_report_map(text):
    """Split overview.md into ## sections, pull each section's 'Full
    report' link, and collect the image paths within it."""
    sections = re.split(r'\n## ', text)
    out = []
    for sec in sections[1:]:
        title = sec.split("\n", 1)[0].strip()
        report_m = re.search(r'\[Full report\]\(([^)]+)\)', sec)
        if not report_m:
            continue
        report_path = ROOT / report_m.group(1)
        images = re.findall(r'!\[[^\]]*\]\(([^)]+)\)', sec)
        out.append((title, report_path, images))
    return out


def main():
    text = OVERVIEW.read_text()
    sections = section_report_map(text)

    any_fail = False
    for title, report_path, images in sections:
        if not report_path.exists():
            print(f"[{title}] MISSING report: {report_path}")
            any_fail = True
            continue
        report_bytes = report_path.read_text(errors="ignore")

        for img_rel in images:
            if img_rel in KNOWN_EXCEPTIONS:
                continue
            img_path = ROOT / img_rel
            if not img_path.exists():
                print(f"[{title}] MISSING figure file: {img_rel}")
                any_fail = True
                continue
            b64 = base64.b64encode(img_path.read_bytes()).decode("ascii")
            if b64 not in report_bytes:
                print(f"[{title}] NOT IN REPORT: {img_rel} (shown in overview, "
                      f"but not byte-identical to anything embedded in {report_path.relative_to(ROOT)})")
                any_fail = True

    if any_fail:
        print("\nOverview.md references figures its own report doesn't have - "
              "either add the figure to the report, or drop it from the overview.")
        sys.exit(1)
    print(f"\nAll overview.md figures ({sum(len(i) for _, _, i in sections)} across "
          f"{len(sections)} sections) are backed by their section's report.html.")


if __name__ == "__main__":
    main()
