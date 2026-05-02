# UN M7 v5 Rizk-base/no-capture structured run report

Family used: `baseline`.

Physics switches expected for `baseline`: phi gas re-solution OFF, nucleation mass coupling OFF, bulk→dislocation capture OFF. Dislocation coalescence remains controlled by `coalescence_d_scale` and is not disabled.

Total fast rows: 3300

## Selected candidates by category

| category | block | label | score | physical_metric | Rd2000 | ratio20/18 | pd1600 | bulk1600 | qgb1600 |
|---|---|---|---:|---:|---:|---:|---:|---:|---:|
| best_score_total | all_blocks | baseline_v5_D4_diffusion_resolution_pair_trial_00199 | 1.964 | 1.964 | 279.9 | 1.956 | 1.078 | 62.23 | 5.358 |
| best_physical_metric | all_blocks | baseline_v5_D4_diffusion_resolution_pair_trial_00199 | 1.964 | 1.964 | 279.9 | 1.956 | 1.078 | 62.23 | 5.358 |
| best_radius_saturated | all_blocks | baseline_v5_A_nominal_near_trial_00034 | 2.391 | 2.391 | 122.6 | 1.357 | 1.004 | 57.75 | 9.05 |
| best_partition_qgb | all_blocks | baseline_v5_B_standard_wide_trial_00280 | 2.12 | 2.12 | 155.8 | 1.211 | 1.152 | 63.59 | 5.315 |
| best_pressure | all_blocks | baseline_v5_A_nominal_near_trial_00139 | 2.083 | 2.083 | 209.1 | 1.565 | 1.046 | 61.35 | 8.161 |
| best_rizk_near | all_blocks | baseline_v5_A_nominal_near_trial_00000 | 3.778 | 3.778 | 96.6 | 1.641 | 1 | 84.34 | 9.32 |
| best_hard_physical_filter | all_blocks | baseline_v5_D4_diffusion_resolution_pair_trial_00199 | 1.964 | 1.964 | 279.9 | 1.956 | 1.078 | 62.23 | 5.358 |

## Files

- `all_blocks_fast_trials.csv`
- `top_by_category.csv`
- `top50_physical_metric.csv`
- `top50_score_total.csv`
- `block_overview.csv`
- optional final plots under `final_selected/`

## Reminder

This is an automatic no-Codex run. Final scientific selection should still be done by inspecting plots, especially radius, pressure ratio, gas partition, and concentration.
