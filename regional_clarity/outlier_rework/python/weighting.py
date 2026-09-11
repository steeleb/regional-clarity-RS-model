"""SDD-weighted training: same linear weighting scheme prototyped in
04_make_models.Rmd's "Re-weight training toward high-SDD values" section,
addressing the ceiling effect where squared-error loss is dominated by the
dense low/mid-range cluster and never pushed hard on the sparse high-SDD
tail."""
import numpy as np


def make_sdd_weight_fn(k: float = 2.0):
    """At weight_strength=k, the highest-SDD training observation in a fold
    is weighted (1+k) times as heavily as the lowest. Range is computed
    PER FOLD from that fold's own training data only - consistent with the
    no-leakage discipline used everywhere else here."""
    def weight_fn(y: np.ndarray) -> np.ndarray:
        lo, hi = np.min(y), np.max(y)
        if hi == lo:
            return np.ones_like(y)
        return 1 + k * (y - lo) / (hi - lo)
    return weight_fn
