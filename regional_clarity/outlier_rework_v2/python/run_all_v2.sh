#!/bin/bash
# Full v2 pipeline: shoreline_flag feature + gap-aware tuning at every
# tuning step, mirroring outlier_rework's stage order. Each model runs as
# its own process (xgboost/lightgbm/torch OpenMP conflict on this machine -
# see run_single_model.py's docstring), so this is a plain sequential shell
# script rather than a single python entrypoint.
set -e
cd "$(dirname "$0")"
source /Users/steeleb/miniconda3/etc/profile.d/conda.sh
conda activate regional_clarity
export KMP_DUPLICATE_LIB_OK=TRUE
export OMP_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1

log() { echo "[$(date +%H:%M:%S)] $*"; }

log "=== STAGE 1: single-model optical-only baseline (gap-aware) ==="
for m in xgboost lightgbm nn; do
  log "run_single_model.py $m"
  python run_single_model.py "$m"
done
python aggregate_results.py

log "=== STAGE 2: weighted-training variant ==="
for m in xgboost lightgbm nn; do
  log "run_weighted_variant.py $m"
  python run_weighted_variant.py "$m"
done

log "=== STAGE 3: per-fold overfitting diagnostics (optical-only baseline) ==="
for m in xgboost lightgbm nn; do
  log "run_fold_diagnostics.py $m"
  python run_fold_diagnostics.py "$m"
done

log "=== STAGE 4: feature-group ablation (gap-aware), 3 models x 4 groups ==="
for m in xgboost lightgbm nn; do
  for g in optical optical+site optical+weather optical+site+weather; do
    log "run_feature_group_model.py $m $g"
    python run_feature_group_model.py "$m" "$g"
  done
done
python aggregate_feature_group_results.py

log "=== STAGE 5: backward feature elimination (writes selected_features.json) ==="
python run_backward_elimination.py

log "=== STAGE 6: final tuning on the selected feature set (gap-aware) ==="
python run_final_tuning.py

log "=== STAGE 7: final evaluation (test partition, one look) ==="
python run_final_evaluation.py

log "=== STAGE 8: SHAP, per feature-group config (test set) ==="
for m in xgboost lightgbm nn; do
  for g in optical optical+site optical+weather optical+site+weather; do
    log "run_shap_analysis.py $m $g"
    python run_shap_analysis.py "$m" "$g"
  done
done
python aggregate_shap_results.py

log "=== STAGE 9: SHAP, out-of-fold CV on backward-elim candidate sets ==="
python run_shap_cv.py

log "=== STAGE 10: figures ==="
python make_report_figures.py
python make_weighted_figures.py
python make_fold_diagnostics_figure.py
python make_feature_group_figures.py
python make_shap_figures.py
python build_feature_table.py

log "ALL STAGES DONE"
