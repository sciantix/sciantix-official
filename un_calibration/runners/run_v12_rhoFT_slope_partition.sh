#!/usr/bin/env bash
set -euo pipefail

(
python UN_M7_optuna_calibration_v12_rhoFT_slope_partition_STANDALONE_separate.py \
  --family capture_only \
  --n-trials 500 \
  --full-exp-fast \
  --n-top-final 5 \
  --no-plots \
  --rho-global-min 0.7 \
  --rho-global-max 2.0 \
  --rho-slope-min 0.5 \
  --rho-slope-max 5.0 \
  --rho-transition-end-K 1600 \
  --Kd-min 3e5 \
  --Kd-max 8e5 \
  --v11-qgb-weight 1.10 \
  --v11-partition-weight 0.85 \
  --qgb-max-low-mid 18 \
  --qgb-max-high 22 \
  --fig8-disl-radius-weight 0.35 \
  --fig8-bulk-radius-weight 0.06 \
  --fig6-burnup-weight 0.02 \
  --radius-guard-weight 0.30 \
  --rd2000-max-nm 1400 \
  --rd1800-soft-max-nm 1100 \
  --rd1900-soft-max-nm 1300 \
  --output-dir UN_M7_optuna_v12_rhoFT_slope_partition_results/capture_only \
&& python UN_M7_optuna_calibration_v12_rhoFT_slope_partition_STANDALONE_separate.py \
  --family capture_only \
  --n-trials 0 \
  --full-exp-fast \
  --n-top-final 5 \
  --rho-global-min 0.7 \
  --rho-global-max 2.0 \
  --rho-slope-min 0.5 \
  --rho-slope-max 5.0 \
  --rho-transition-end-K 1600 \
  --Kd-min 3e5 \
  --Kd-max 8e5 \
  --v11-qgb-weight 1.10 \
  --v11-partition-weight 0.85 \
  --qgb-max-low-mid 18 \
  --qgb-max-high 22 \
  --fig8-disl-radius-weight 0.35 \
  --fig8-bulk-radius-weight 0.06 \
  --fig6-burnup-weight 0.02 \
  --radius-guard-weight 0.30 \
  --rd2000-max-nm 1400 \
  --rd1800-soft-max-nm 1100 \
  --rd1900-soft-max-nm 1300 \
  --output-dir UN_M7_optuna_v12_rhoFT_slope_partition_results/capture_only
) 2>&1 | tee v12_rhoFT_slope_partition_run_and_plot.log
