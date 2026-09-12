"""Generate the optical-features HTML table directly from features.py's
actual formulas, so the report table can never drift out of sync with the
code again (which is exactly how the G_BR/G_BS/N_GS bugs went unnoticed
for as long as they did)."""

DEFINITIONS = {
    "red_corr7": "corrected red reflectance",
    "green_corr7": "corrected green reflectance",
    "blue_corr7": "corrected blue reflectance",
    "nir_corr7": "corrected NIR reflectance",
    "swir1_corr7": "corrected SWIR1 reflectance",
    "swir2_corr7": "corrected SWIR2 reflectance",
    "temp_corr7": "corrected surface temperature",
    "atm_corr_LaSRC": "1 = LaSRC (LC08/09), 0 = LEDAPS (LT04/05/LE07)",
    "BR": "blue / red", "BG": "blue / green", "NR": "nir / red", "GR": "green / red",
    "fai": "floating algae index (nir − (red + (swir1−red)·(830−660)/(1650−660)))",
    "NDVI": "(nir−red) / (nir+red)",
    "NDSSI": "(blue−nir) / (blue+nir)",
    "NDWI": "(green−nir) / (green+nir)",     # McFeeters 1996
    "MNDWI": "(green−swir1) / (green+swir1)",  # Xu 2006
}


def main():
    import spatial_cv as cv
    import features
    from features import SITE_COLS, weather_cols

    df = cv.load("data/modeling_dataset_expanded.parquet")
    df = features.add_spectral_indices(df)
    all_extra = set(SITE_COLS) | set(weather_cols(df))
    optical_feats = sorted(f for f in features.candidate_feature_list(df) if f not in all_extra)

    missing = [f for f in optical_feats if f not in DEFINITIONS]
    extra = [f for f in DEFINITIONS if f not in optical_feats]
    if missing or extra:
        raise SystemExit(f"DEFINITIONS out of sync: missing={missing} extra={extra}")

    # two-column-pair table, balanced as evenly as possible
    half = (len(optical_feats) + 1) // 2
    left, right = optical_feats[:half], optical_feats[half:]
    rows = []
    for i in range(half):
        l = left[i]
        l_cell = f"<tr><td>{l}</td><td>{DEFINITIONS[l]}</td>"
        if i < len(right):
            r = right[i]
            r_cell = f"<td>{r}</td><td>{DEFINITIONS[r]}</td></tr>"
        else:
            r_cell = "<td></td><td></td></tr>"
        rows.append(l_cell + r_cell)

    html = f'<h3>Optical ({len(optical_feats)} candidates)</h3>\n'
    html += ('<p>7 Gardner-corrected reflectance/temperature bands, 1 atmospheric-correction '
             'indicator, and the remaining band-ratio/normalized-difference indices, replicated '
             'from <code>04_make_models.Rmd</code> with the <code>G_BR</code>/<code>N_GS</code> '
             'fixes from above applied.</p>\n')
    html += '<div class="table-wrap">\n<table>\n'
    html += '<thead><tr><th>Name</th><th>Definition</th><th>Name</th><th>Definition</th></tr></thead>\n<tbody>\n'
    html += "\n".join(f"      {r}" for r in rows)
    html += '\n</tbody>\n</table>\n</div>'

    with open("optical_feature_table.html", "w") as fh:
        fh.write(html)
    print(f"wrote optical_feature_table.html ({len(optical_feats)} features)")


if __name__ == "__main__":
    main()
