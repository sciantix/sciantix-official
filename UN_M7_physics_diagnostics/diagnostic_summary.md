# UN M7 physics diagnostics

## Candidate

- `label` = diagnostic_M7_no_phi_Dv05_trial_00097
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

## What this script checks

- gas conservation / partition including q_gb;
- current pressure formula vs ideal pressure using total bubble volume;
- sensitivity to initial dislocation-bubble seed radius, if --with-init is used;
- numerical convergence from dt=1 h, modes=40 to dt=0.5 h, modes=80, if --with-convergence is used.

## Automatic flags

- High early q_gb=39.5% at T=1600 K, burnup=1.1%.
- High p_d/p_eq=18 at T=2200 K, burnup=1.1%.
- High early q_gb=51.4% at T=1600 K, burnup=3.2%.
- High p_d/p_eq=53 at T=2200 K, burnup=3.2%.

## Output files

- `candidate_parameters.csv`
- `point_diagnostics.csv`
- `initialization_sensitivity.csv`
- `history_sample_diagnostics.csv`
- `history_diagnostics.csv`
- `numerical_convergence_check.csv`
