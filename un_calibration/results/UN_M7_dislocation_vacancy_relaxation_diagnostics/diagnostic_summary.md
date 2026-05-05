# UN M7 dislocation vacancy relaxation diagnostic

This is diagnostic only. It tests whether stronger vacancy absorption in dislocation bubbles can relieve overpressure when the grain-face sink is weakened.

## Candidate

- `label` = M7_no_phi_Dv05_trial_00097_balanced
- `f_n` = 1.1149e-08
- `K_d` = 534290.727568
- `rho_d` = 27788924849394.254
- `fission_rate` = 6.930584923894175e+19
- `Dv_scale` = 0.5
- `Dg_scale` = 1.315673
- `b_scale` = 2.578705
- `gb_scale` = 2.010723
- `gd_scale` = 4.852245
- `coalescence_d_scale` = 6.966378
- `capture_scale` = 2.482966
- `D2_xe_scale` = 1.361065

Family: `M7_no_phi`

## Best diagnostic combinations

| rank | boundary_sink_scale | dislocation_vacancy_scale | combined | p_d/p_eq max | bulk1600 3.2 | disl2200 3.2 | qgb2200 3.2 |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 0 | 1 | 4.542 | 321.4 | 81.3 | 95.2 | 1.5 |
| 2 | 0 | 5 | 5.897 | 656.6 | 81.2 | 95.4 | 1.5 |
| 3 | 0 | 20 | 7.252 | 1234 | 81.1 | 95.5 | 1.5 |
| 4 | 0.5 | 1 | 93.94 | 97.79 | 53.7 | 42.8 | 57 |
| 5 | 0.5 | 5 | 95.01 | 194.6 | 53.5 | 42.8 | 57 |
| 6 | 0.5 | 20 | 96.18 | 355.5 | 53.2 | 42.9 | 57 |
| 7 | 1 | 1 | 168.3 | 53.3 | 34.7 | 27.9 | 72.1 |
| 8 | 1 | 5 | 169.3 | 105.4 | 34.5 | 27.9 | 72.1 |
| 9 | 1 | 20 | 170.4 | 191.2 | 34.3 | 27.9 | 72.1 |

## Interpretation guide

- If low boundary_sink_scale plus high dislocation_vacancy_scale gives Rizk-like partition and acceptable p_d/p_eq, then the missing/underestimated mechanism is likely pressure relaxation/vacancy absorption at dislocation bubbles.
- If pressure remains high even with large dislocation_vacancy_scale, then the issue is not only vacancy absorption; look at EOS, coalescence/Nd, or missing microstructural evolution.
- If partition only becomes Rizk-like at boundary_sink_scale near zero, then the reduced grain-face sink remains a major structural limitation.

## Output files

- `candidate_parameters.csv`
- `point_diagnostics.csv`
- `combo_summary.csv`
