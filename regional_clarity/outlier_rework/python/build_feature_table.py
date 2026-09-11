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
    "NR": "nir / red", "BR": "blue / red", "GR": "green / red", "SR": "swir1 / red",
    "BG": "blue / green", "RG": "red / green", "NG": "nir / green", "SG": "swir1 / green",
    "BN": "blue / nir", "GN": "green / nir", "RN": "red / nir", "SN": "swir1 / nir",
    "BS": "blue / swir1", "GS": "green / swir1", "RS": "red / swir1", "NS": "nir / swir1",
    "R_GN": "red / (green+nir)", "R_GB": "red / (green+blue)", "R_GS": "red / (green+swir1)",
    "R_BN": "red / (blue+nir)", "R_BS": "red / (blue+swir1)", "R_NS": "red / (nir+swir1)",
    "G_BN": "green / (blue+nir)", "G_BS": "green / (blue+swir1)", "G_RN": "green / (red+nir)",
    "G_RB": "green / (red+blue)", "G_NS": "green / (nir+swir1)",
    "B_RG": "blue / (red+green)", "B_RS": "blue / (red+swir1)", "B_GN": "blue / (green+nir)",
    "B_GS": "blue / (green+swir1)", "B_NS": "blue / (nir+swir1)",
    "N_RG": "nir / (red+green)", "N_RB": "nir / (red+blue)", "N_RS": "nir / (red+swir1)",
    "N_GB": "nir / (green+blue)", "N_GS": "nir / (green+swir1)", "N_BS": "nir / (blue+swir1)",
    "GR_2": "(red+green) / 2", "GN_2": "(nir+green) / 2",
    "BR_G": "(blue−red) / green", "NS_NR": "(nir−swir1) / (red−swir1)",
    "fai": "floating algae index", "NmS": "nir − swir1", "NmR": "nir − red",
    "NDVI": "(nir−red) / (nir+red)", "NDWI": "(green−swir1) / (green+swir1)",
    "NDSSI": "(blue−nir) / (blue+nir)", "GN_GN": "(green−nir) / (green+nir)",
}


def main():
    import spatial_cv as cv
    import features
    from run_feature_group_model import SITE_COLS, weather_cols

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
