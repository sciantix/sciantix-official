# UN M7 Codex capture_only Dv-profile report

## Run scope

- Family: `capture_only`.
- Fixed `Dv_scale`: 0.03, 0.05, 0.1, 0.15, 0.3, 0.5, 1.
- Trials requested per Dv value: 100.
- Scoring options: `pressure_weight=0.6`, `pressure_free_factor=3`, `rizk_prior_weight=0.2`, `bulk_shape_weight=0.05`.
- Each Dv value was run in a separate subfolder under `UN_M7_codex_results/capture_only_dv_profile/` to avoid mixing Optuna SQLite studies.
- Original notebooks/scripts were not modified; model equations are reused from `UN_M7_optuna_calibration_v2.py` and `UN_M7_optuna_calibration.py`.
- Matplotlib is not installed in the active virtual environment, so final PNG diagnostics were produced with the Codex gnuplot fallback from final-resolution model evaluations.

## Best fixed-Dv candidates

| profile_Dv_fixed | score_total | score_data | score_pressure | score_prior | swD_1p3_1600K | swB_1p3_1600K | Rd_1p3_1600K | Nd_1p3_1600K | Nd_drop_1725_over_1400_log10 | p_d_over_eq_1p3_1600K | p_d_over_eq_max_1p3_1200_1700K |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| 0.03 | 1.476 | 0.921 | 0.7287 | 0.2723 | 2.603 | 0.03264 | 84.56 | 1.0277e+19 | -0.7596 | 4.46 | 29.19 |
| 0.05 | 1.317 | 0.8987 | 0.5221 | 0.2037 | 2.653 | 0.07286 | 86.15 | 9.9079e+18 | -0.927 | 3.586 | 17.04 |
| 0.1 | 1.353 | 0.9808 | 0.4164 | 0.2143 | 2.838 | 0.2516 | 98.53 | 7.0830e+18 | -0.8497 | 3.32 | 13.19 |
| 0.15 | 1.395 | 1.02 | 0.3567 | 0.2657 | 2.74 | 0.946 | 101.8 | 6.1952e+18 | -0.8739 | 2.944 | 11.87 |
| 0.3 | 1.342 | 1.212 | 0.00841 | 0.2439 | 2.584 | 0.2788 | 84.52 | 1.0215e+19 | -0.7478 | 1.356 | 3.647 |
| 0.5 | 1.375 | 1.174 | 0.03239 | 0.2073 | 2.365 | 0.5118 | 89.06 | 7.9925e+18 | -0.8887 | 1.293 | 3.185 |
| 1 | 1.398 | 1.141 | 0.06608 | 0.2306 | 2.752 | 0.4427 | 85.92 | 1.0358e+19 | -0.9656 | 1.155 | 2.449 |

## Candidate categories

- Best numerical fit: `capture_only_v2_trial_00098`, Dv=0.05, score=1.317.
- Best pressure-friendly candidate: `capture_only_v2_trial_00098`, Dv=1, max p_d/p_d_eq=2.213.
- Best Rizk-near candidate: `capture_only_v2_trial_00071`, Dv=0.5, prior distance=0.1539.
- Best balanced candidate: `capture_only_v2_trial_00086`, Dv=1, balanced metric=1.227.

## Final reruns

| selection_category | score_total | score_data | score_pressure | score_rizk_prior | Dv_scale | f_n | K_d | rho_d | fission_rate | Dg_scale | b_scale | gb_scale | gd_scale | coalescence_d_scale | capture_scale | swD_1p3_1600K | swB_1p3_1600K | Rd_1p3_1600K | Nd_1p3_1600K | Nd_drop_1725_over_1400_log10 | p_d_over_eq_1p3_1600K | p_d_over_eq_max_1p3_1200_1700K |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| best_overall_score | 1.286 | NA | 0.4584 | 0.2037 | 0.05 | 1.3333e-08 | 7.1430e+05 | 4.6135e+13 | 4.9558e+19 | 2.012 | 1.181 | 0.8994 | 3.641 | 10.99 | 3.247 | 2.538 | 0.05394 | 83.47 | 1.0418e+19 | -0.9083 | 3.273 | 15.36 |
| best_balanced | 1.337 | NA | 0.005634 | 0.2306 | 1 | 2.5466e-07 | 6.4846e+05 | 3.5350e+13 | 7.4799e+19 | 0.5333 | 3.066 | 0.4625 | 4.507 | 6.997 | 0.1401 | 2.456 | 0.312 | 80.4 | 1.1282e+19 | -0.8431 | 1.11 | 2.337 |
| best_pressure_friendly | 1.398 | NA | 0 | 0.2563 | 1 | 1.9043e-07 | 6.3368e+05 | 4.8099e+13 | 6.8347e+19 | 0.3921 | 3.704 | 0.3881 | 3.375 | 7.005 | 0.0608 | 2.235 | 0.2174 | 69.34 | 1.6007e+19 | -0.7379 | 1.063 | 2.084 |
| best_rizk_near | 1.464 | NA | 0.08804 | 0.1539 | 0.5 | 1.3237e-08 | 5.5892e+05 | 2.4815e+13 | 7.6621e+19 | 1.307 | 1.296 | 0.4151 | 1.963 | 9.772 | 0.1557 | 2.388 | 0.2279 | 102.4 | 5.3021e+18 | -1.015 | 1.526 | 3.976 |

## Answers

1. Is `capture_only` competitive with `M7_no_phi` when pressure and Rizk-prior are considered? Not as the best final model; it remains useful as a simpler backup/diagnostic. Best final capture_only score is 1.286; previous `M7_no_phi` best final score is 1.182.
2. Which `Dv_scale` gives the best trade-off? The balanced selection points to Dv=1; the pure scalar best points to Dv=0.05.
3. Can acceptable `p_d/p_d_eq` be found without destroying P2 fit? Yes: the balanced max over 1200-1700 K at 1.3% FIMA is 2.449, with swD1600=2.752%.
4. How far are the best candidates from Rizk nominal values? The best balanced prior-distance metric is 0.2306; key deviations include f_n=2.5466e-07, K_d=6.4846e+05, rho_d=3.5350e+13, Dv=1, coalescence_d_scale=6.997, capture_scale=0.1401.
5. Is the bulk swelling shape problem fatal or only diagnostic? Diagnostic for this reduced model. Bulk/grain-boundary behavior is incomplete, so it should not dominate over P2/dislocation swelling, radius, concentration, and pressure sanity.
6. Does `capture_only` reduce dislocation overpressure compared with `M7_no_phi`? Previous `M7_no_phi` final rows have score_pressure around 0.3924; the best balanced capture_only score_pressure is 0.06608. Use the explicit p_d/p_d_eq diagnostics above for the stronger comparison.
7. Does the model need `Dv_scale` far below 1 to fit the data? Not strictly once pressure is treated seriously: the scalar best is at Dv=0.05, but the balanced pressure-friendly candidate is at Dv=1. Dv remains a suspicious compensating parameter because the lowest scalar scores still prefer reduced Dv.
8. Which final model family is recommended? M7_no_phi remains the primary recommendation, with capture_only as a physically simpler backup.
9. What additional physics or parameter audit is needed if no acceptable candidate exists? Audit the vacancy diffusivity scale/fit, f_n closure, capture law calibration, coalescence scale, K_d and rho_d interpretation, and pressure/vacancy absorption assumptions. Do not hide failure by changing equations silently.

## Rejections and cautions

- Candidates with broad `p_d/p_d_eq > 10` are rejected as physically suspicious even if their scalar score is good.
- Candidates with very low `Dv_scale`, extreme `coalescence_d_scale`, or very small/large `capture_scale` are treated as compensating fits.
- Candidates near Rizk parameters are preferred when score and pressure are similar.
