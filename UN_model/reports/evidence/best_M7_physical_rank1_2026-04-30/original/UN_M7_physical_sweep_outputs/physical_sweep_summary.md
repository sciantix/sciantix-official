# M7 physical sweep summary

This sweep keeps the physical M7 equations fixed and does not disable dislocation coalescence.

## Fixed modelling choices

- M7 = nucleation mass coupling + phi re-solution + bulk-dislocation capture.
- Dislocation-bubble coalescence is fixed ON: `coalescence_d_scale = 1`.
- `Dg_scale = b_scale = gb_scale = gd_scale = nu_scale = 1`.
- Capture law is linear clipped Barani/Rizk-like: `f_cap = min(1, max(0, capture_scale*N_d*DeltaVcap))`.
- `capture_fraction_sum` is only a cumulative diagnostic and can exceed 1. It is not a physical fraction.

## Best FINAL candidate

- `label` = FINAL_rank2_PHYS_0045
- `score_total` = 1.7068047916474136
- `score_swd` = 0.8264825611639194
- `score_Nd` = 0.4877610182736616
- `score_Rd` = 0.10101100031512107
- `score_pressure` = 0.07207034824953623
- `f_n` = 3e-07
- `K_d` = 300000.0
- `rho_d` = 30000000000000.0
- `fission_rate` = 7.5e+19
- `Dv_scale` = 0.3
- `capture_scale` = 1.0
- `swD_1p3_1600K` = 2.668592103851043
- `swB_1p3_1600K` = 1.254397391878499
- `Rd_1p3_1600K` = 92.43879195817759
- `Nd_1p3_1600K` = 8.065490864498744e+18
- `swD_3p2_1600K` = 5.061468420296727
- `swB_3p2_1600K` = 5.172271860209517
- `max_f_cap_step_3p2_1600K` = 3.456627451336176e-05
- `capture_fraction_sum_3p2_1600K` = 0.09439361668117116
- `capture_raw_sum_3p2_1600K` = 0.09439361668117116

## All FINAL candidates

| rank | score | f_n | K_d | rho_d | Fdot | Dv_scale | capture_scale | swD_1.3_1600 | Rd_1.3_1600 | Nd_1.3_1600 |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 1.7068 | 3.000e-07 | 3.000e+05 | 3.000e+13 | 7.500e+19 | 0.3 | 1 | 2.669 | 92.44 | 8.0655e+18 |
| 2 | 1.76252 | 3.000e-07 | 5.000e+05 | 3.000e+13 | 7.500e+19 | 0.3 | 0.3 | 2.757 | 78.93 | 1.3385e+19 |
| 3 | 1.78031 | 1.000e-08 | 5.000e+05 | 1.000e+13 | 7.500e+19 | 0.3 | 0 | 2.188 | 104.6 | 4.5697e+18 |
| 4 | 1.80544 | 1.000e-08 | 8.000e+05 | 1.000e+13 | 5.000e+19 | 0.3 | 0 | 2.751 | 97.26 | 7.1383e+18 |
| 5 | 1.81547 | 1.000e-08 | 8.000e+05 | 1.000e+13 | 5.000e+19 | 0.3 | 1 | 2.78 | 97.58 | 7.1429e+18 |
| 6 | 1.82148 | 1.000e-06 | 3.000e+05 | 3.000e+13 | 7.500e+19 | 1 | 0 | 2.735 | 93.32 | 8.0362e+18 |
| 7 | 1.99132 | 1.000e-08 | 3.000e+05 | 1.000e+13 | 5.000e+19 | 0.3 | 0 | 2.37 | 127.6 | 2.7208e+18 |
| 8 | 2.03734 | 3.000e-08 | 3.000e+05 | 1.000e+13 | 7.500e+19 | 1 | 0 | 2.308 | 126.4 | 2.7280e+18 |
