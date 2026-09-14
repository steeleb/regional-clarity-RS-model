"""Sanity check: is every report.html in this project actually built from
its current inputs (template + figures + build script), or does it
predate an edit that was never rebuilt into it?

Explicit per-report config rather than parsing each build_report.py's
source - this project's six build scripts don't share one convention for
where figures/templates live, and a generic parser silently producing
false positives/negatives would be worse than no check at all.

Flags a report as STALE if report.html is older than its template, its
build script, or any figure file under its figures directory (recursive,
.png only) - or if any of those are missing. Doesn't know which figures
a given template actually embeds, so a stray unused .png in the figures
dir can trigger a false "stale" - acceptable, since the fix (rerun
build_report.py) is cheap and safe either way.

Run after any figure regeneration or template edit, before trusting a
report.html's contents: `python check_report_freshness.py`.
"""
import sys
from pathlib import Path

ROOT = Path(__file__).parent

# name -> (report.html, template.html, build_script.py, figures_dir)
REPORTS = {
    "outlier_rework": (
        "outlier_rework/python/report.html",
        "outlier_rework/python/report_template.html",
        "outlier_rework/python/build_report.py",
        "outlier_rework/python/results/figures",
    ),
    "outlier_rework_v2": (
        "outlier_rework_v2/python/report.html",
        "outlier_rework_v2/python/report_template.html",
        "outlier_rework_v2/python/build_report.py",
        "outlier_rework_v2/python/results/figures",
    ),
    "outlier_investigation": (
        "outlier_investigation/report.html",
        "outlier_investigation/python/report_template.html",
        "outlier_investigation/python/03_build_report.py",
        "outlier_investigation/figures",
    ),
    "partition_sensitivity": (
        "partition_sensitivity/report.html",
        "partition_sensitivity/python/report_template.html",
        "partition_sensitivity/python/04_build_report.py",
        "partition_sensitivity/figures",
    ),
    "outlier_rework_v3": (
        "outlier_rework_v3/report.html",
        "outlier_rework_v3/python/report_template.html",
        "outlier_rework_v3/python/04_build_report.py",
        "outlier_rework_v3/figures",
    ),
    "ls8_harmonization": (
        "ls8_harmonization/report.html",
        "ls8_harmonization/python/report_template.html",
        "ls8_harmonization/python/05_build_report.py",
        "ls8_harmonization/figures",
    ),
}


def main():
    any_stale = False
    for name, (report_rel, template_rel, script_rel, figdir_rel) in REPORTS.items():
        report_html = ROOT / report_rel
        template = ROOT / template_rel
        script = ROOT / script_rel
        figdir = ROOT / figdir_rel

        missing = [str(p) for p in (report_html, template, script, figdir) if not p.exists()]
        if missing:
            print(f"[{name}] MISSING: {missing}")
            any_stale = True
            continue

        report_mtime = report_html.stat().st_mtime
        stale = []
        if template.stat().st_mtime > report_mtime:
            stale.append(template_rel)
        if script.stat().st_mtime > report_mtime:
            stale.append(script_rel)
        for fig in figdir.glob("*.png"):
            if fig.stat().st_mtime > report_mtime:
                stale.append(str(fig.relative_to(ROOT)))

        if stale:
            print(f"[{name}] STALE - report.html predates: {stale}")
            any_stale = True
        else:
            n_figs = len(list(figdir.glob("*.png")))
            print(f"[{name}] OK - report.html is newer than template, build script, and all {n_figs} figures")

    if any_stale:
        print("\nSome reports need rebuilding - rerun the relevant build script.")
        sys.exit(1)
    print("\nAll reports up to date.")


if __name__ == "__main__":
    main()
