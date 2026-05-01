# UN M7 boundary sink diagnostics

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

## Meaning of boundary_sink_scale

This is a diagnostic-only multiplier on the spectral diffusion-to-boundary eigenvalue term. It is not a Rizk parameter and not a proposed final calibration parameter.

## Quick flags

- scale=0.0: p_d/p_eq=11.1 at T=2000 K, burnup=1.1%
- scale=0.0: p_d/p_eq=38.3 at T=2200 K, burnup=1.1%
- scale=0.0: dislocation gas=75.0% at T=2000 K, burnup=3.2%
- scale=0.0: p_d/p_eq=55.5 at T=2000 K, burnup=3.2%
- scale=0.0: p_d/p_eq=321.4 at T=2200 K, burnup=3.2%
- scale=0.25: q_gb=16.3% at T=1700 K, burnup=1.1%
- scale=0.25: dislocation gas=81.6% at T=2000 K, burnup=1.1%
- scale=0.25: dislocation gas=81.8% at T=2200 K, burnup=1.1%
- scale=0.25: p_d/p_eq=29.4 at T=2200 K, burnup=1.1%
- scale=0.25: q_gb=17.4% at T=1600 K, burnup=3.2%
- scale=0.25: q_gb=23.8% at T=1700 K, burnup=3.2%
- scale=0.25: dislocation gas=54.6% at T=2000 K, burnup=3.2%
- scale=0.25: p_d/p_eq=36.9 at T=2000 K, burnup=3.2%
- scale=0.25: dislocation gas=59.5% at T=2200 K, burnup=3.2%
- scale=0.25: p_d/p_eq=157.4 at T=2200 K, burnup=3.2%
- scale=0.5: q_gb=20.3% at T=1400 K, burnup=1.1%
- scale=0.5: q_gb=24.1% at T=1600 K, burnup=1.1%
- scale=0.5: q_gb=27.0% at T=1700 K, burnup=1.1%
- scale=0.5: dislocation gas=70.9% at T=2000 K, burnup=1.1%
- scale=0.5: dislocation gas=71.0% at T=2200 K, burnup=1.1%
- scale=0.5: p_d/p_eq=24.0 at T=2200 K, burnup=1.1%
- scale=0.5: q_gb=24.3% at T=1400 K, burnup=3.2%
- scale=0.5: q_gb=30.8% at T=1600 K, burnup=3.2%
- scale=0.5: q_gb=40.4% at T=1700 K, burnup=3.2%
- scale=0.5: dislocation gas=41.3% at T=2000 K, burnup=3.2%
- scale=0.5: p_d/p_eq=25.2 at T=2000 K, burnup=3.2%
- scale=0.5: dislocation gas=42.8% at T=2200 K, burnup=3.2%
- scale=0.5: p_d/p_eq=97.8 at T=2200 K, burnup=3.2%
- scale=0.75: q_gb=28.2% at T=1400 K, burnup=1.1%
- scale=0.75: q_gb=32.5% at T=1600 K, burnup=1.1%
- scale=0.75: q_gb=35.2% at T=1700 K, burnup=1.1%
- scale=0.75: dislocation gas=63.2% at T=2000 K, burnup=1.1%
- scale=0.75: dislocation gas=63.3% at T=2200 K, burnup=1.1%
- scale=0.75: p_d/p_eq=20.3 at T=2200 K, burnup=1.1%
- scale=0.75: q_gb=34.0% at T=1400 K, burnup=3.2%
- scale=0.75: q_gb=42.1% at T=1600 K, burnup=3.2%
- scale=0.75: q_gb=52.7% at T=1700 K, burnup=3.2%
- scale=0.75: dislocation gas=33.0% at T=2000 K, burnup=3.2%
- scale=0.75: p_d/p_eq=18.4 at T=2000 K, burnup=3.2%
- scale=0.75: dislocation gas=33.6% at T=2200 K, burnup=3.2%
- scale=0.75: p_d/p_eq=69.4 at T=2200 K, burnup=3.2%
- scale=1.0: q_gb=35.2% at T=1400 K, burnup=1.1%
- scale=1.0: q_gb=39.5% at T=1600 K, burnup=1.1%
- scale=1.0: q_gb=41.5% at T=1700 K, burnup=1.1%
- scale=1.0: dislocation gas=57.3% at T=2000 K, burnup=1.1%
- scale=1.0: dislocation gas=57.4% at T=2200 K, burnup=1.1%
- scale=1.0: p_d/p_eq=17.7 at T=2200 K, burnup=1.1%
- scale=1.0: q_gb=42.6% at T=1400 K, burnup=3.2%
- scale=1.0: q_gb=51.4% at T=1600 K, burnup=3.2%
- scale=1.0: q_gb=61.7% at T=1700 K, burnup=3.2%
- scale=1.0: dislocation gas=27.6% at T=2000 K, burnup=3.2%
- scale=1.0: p_d/p_eq=14.4 at T=2000 K, burnup=3.2%
- scale=1.0: dislocation gas=27.9% at T=2200 K, burnup=3.2%
- scale=1.0: p_d/p_eq=53.3 at T=2200 K, burnup=3.2%

## Output files

- `candidate_parameters.csv`
- `diffusion_timescales.csv`
- `boundary_sink_scale_summary.csv`
- `boundary_sink_scale_history.csv` if --with-history was used
