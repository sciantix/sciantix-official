# Optuna 4test_UN calibration summary
- Best trial: `28`
- Best objective: `21.867831`

## Best parameters
- `rho_rizk2023_fit_variant` = `shifted`
- `rho_rizk2023_fit_scale` = `0.06126782693339066`
- `rho_rizk2023_fit_cap` = `1408210982105133.2`
- `K_d` = `297016.8784455785`
- `f_n` = `1.1372654476581536e-07`
- `Dg_dislocation_scale` = `4.303831651238942`
- `Dv_dislocation_scale` = `1.8740854310088817`
- `first_gas_pressure_factor` = `0.52601257081018`

## Best component scores
- `score_Nd_drop` = `0.0`
- `score_guards` = `0.02687573320442662`
- `score_partition` = `4.351976292859688`
- `score_pressure` = `0.04575117576286474`
- `score_radius_T` = `0.011834240914749624`
- `score_swelling_T` = `0.3748451987914411`
- `score_swelling_burnup` = `0.7798410519228209`

## Fixed parameters in this run
- `gb_scale = 1.0`
- `gd_scale = 1.0`
- `b_scale = 1.0`
- `coalescence_d_scale = 1.0`
- `USE_BULK_DISLOCATION_CAPTURE = False`
- `capture_scale = 0.0`

## Notebook paste block for manual rerun
```python
CASE_LABEL = "optuna_best_00028"
RHO_MODE = "constant"  # Optuna script monkey-patches deformed rho; see params below
FIRST_GAS_PRESSURE_FACTOR = 0.52601257081
MANUAL_PARAMS.update({
    "f_n": 1.137265447658e-07,
    "K_d": 2.970168784456e+05,
    "rho_d": 3.000000000000e+13,  # not used directly for rizk2023_fit
    "Dg_dislocation_scale": 4.30383165124,
    "Dv_dislocation_scale": 1.87408543101,
    "gb_scale": 1.0,
    "gd_scale": 1.0,
    "b_scale": 1.0,
    "coalescence_d_scale": 1.0,
    "capture_scale": 0.0,
})
```

**Nota:** se il best usa `blank_sat`, `blank_sat_deformed` o `rizk2023_fit`, riportare anche la modalità/parametri rho nel notebook manuale. `rizk2023_fit` è già presente in 5test_UN.
