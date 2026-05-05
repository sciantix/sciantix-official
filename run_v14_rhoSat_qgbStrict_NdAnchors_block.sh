#!/usr/bin/env bash
set -euo pipefail

N_TRIALS="${1:-100}"

python UN_M7_optuna_calibration_v14_rhoSat_qgbStrict_NdAnchors_STANDALONE.py \
  --family capture_only \
  --n-trials "${N_TRIALS}" \
  --full-exp-fast \
  --n-top-final 5 \
  --rho-scale-min 0.5 \
  --rho-scale-max 2.0 \
  --Kd-min 3e5 \
  --Kd-max 8e5 \
  --exp-swelling-weight 0.75 \
  --exp-rd-weight 0.90 \
  --exp-nd-level-weight 0.00 \
  --base-nd-drop-weight 0.00 \
  --nd-drop-target-weight 0.00 \
  --nd-highT-target-weight 0.00 \
  --nd-early-exp-weight 1.20 \
  --nd-highT-anchor-weight 1.40 \
  --nd-early-max-T 1507.5 \
  --nd-anchor-1800 8e18 \
  --nd-anchor-1900 4e18 \
  --nd-anchor-2000 1.5e18 \
  --pressure-weight 0.22 \
  --highT-pressure-weight 0.22 \
  --rizk-prior-weight 0.08 \
  --v13-partition-weight 0.95 \
  --v13-qgb-weight 2.50 \
  --qgb-1p1-low-mid 5 \
  --qgb-1p1-high 5 \
  --qgb-3p2-low-mid 8 \
  --qgb-3p2-high 12 \
  --qgb-penalty-scale 25 \
  --fig8-disl-radius-weight 0.75 \
  --fig8-bulk-radius-weight 0.04 \
  --fig6-burnup-weight 0.02 \
  --radius-guard-weight 0.25 \
  --rd2000-max-nm 1400 \
  --rd1800-soft-max-nm 1100 \
  --rd1900-soft-max-nm 1300 \
  --output-dir UN_M7_optuna_v14_rhoSat_qgbStrict_NdAnchors_results/capture_only \
  2>&1 | tee -a v14_rhoSat_qgbStrict_NdAnchors_run.log

# Regenerate final plots/summary from the same study.
python UN_M7_optuna_calibration_v14_rhoSat_qgbStrict_NdAnchors_STANDALONE.py \
  --family capture_only \
  --n-trials 0 \
  --full-exp-fast \
  --n-top-final 5 \
  --rho-scale-min 0.5 \
  --rho-scale-max 2.0 \
  --Kd-min 3e5 \
  --Kd-max 8e5 \
  --exp-swelling-weight 0.75 \
  --exp-rd-weight 0.90 \
  --exp-nd-level-weight 0.00 \
  --base-nd-drop-weight 0.00 \
  --nd-drop-target-weight 0.00 \
  --nd-highT-target-weight 0.00 \
  --nd-early-exp-weight 1.20 \
  --nd-highT-anchor-weight 1.40 \
  --nd-early-max-T 1507.5 \
  --nd-anchor-1800 8e18 \
  --nd-anchor-1900 4e18 \
  --nd-anchor-2000 1.5e18 \
  --pressure-weight 0.22 \
  --highT-pressure-weight 0.22 \
  --rizk-prior-weight 0.08 \
  --v13-partition-weight 0.95 \
  --v13-qgb-weight 2.50 \
  --qgb-1p1-low-mid 5 \
  --qgb-1p1-high 5 \
  --qgb-3p2-low-mid 8 \
  --qgb-3p2-high 12 \
  --qgb-penalty-scale 25 \
  --fig8-disl-radius-weight 0.75 \
  --fig8-bulk-radius-weight 0.04 \
  --fig6-burnup-weight 0.02 \
  --radius-guard-weight 0.25 \
  --rd2000-max-nm 1400 \
  --rd1800-soft-max-nm 1100 \
  --rd1900-soft-max-nm 1300 \
  --output-dir UN_M7_optuna_v14_rhoSat_qgbStrict_NdAnchors_results/capture_only \
  2>&1 | tee -a v14_rhoSat_qgbStrict_NdAnchors_run.log
