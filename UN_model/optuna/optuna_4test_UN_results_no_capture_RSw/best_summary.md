# Optuna 4test_UN calibration summary
- Best trial: `247`
- Best objective: `41.893099`

## Best parameters
- `rho_mode` = `blank_sat`
- `rho_blank_scale` = `1.6553317915147303`
- `K_d` = `2300628.536649826`
- `f_n` = `5.1964546749383095e-08`
- `Dg_dislocation_scale` = `1.2270380693289118`
- `Dv_dislocation_scale` = `7.529941775692336`
- `first_gas_pressure_factor` = `0.47077626562557995`

## Best component scores
- `score_Nd_drop` = `0.09256453753889604`
- `score_guards` = `0.0`
- `score_partition` = `3.7273188832433357`
- `score_pressure` = `9.99396498487518e-05`
- `score_radius_T` = `0.810828571790844`
- `score_swelling_T` = `0.301445854759534`
- `score_swelling_burnup` = `0.31098555486737417`

## Fixed parameters in this run
- `gb_scale = 1.0`
- `gd_scale = 1.0`
- `b_scale = 1.0`
- `coalescence_d_scale = 1.0`
- `USE_BULK_DISLOCATION_CAPTURE = False`
- `capture_scale = 0.0`

## Notebook paste block for manual rerun
```python
CASE_LABEL = "optuna_best_00247"
RHO_MODE = "constant"  # Optuna script monkey-patches deformed rho; see params below
FIRST_GAS_PRESSURE_FACTOR = 0.470776265626
MANUAL_PARAMS.update({
    "f_n": 5.196454674938e-08,
    "K_d": 2.300628536650e+06,
    "rho_d": 3.000000000000e+13,  # not used directly for blank_sat
    "Dg_dislocation_scale": 1.22703806933,
    "Dv_dislocation_scale": 7.52994177569,
    "gb_scale": 1.0,
    "gd_scale": 1.0,
    "b_scale": 1.0,
    "coalescence_d_scale": 1.0,
    "capture_scale": 0.0,
})
```

**Nota:** se il best usa `blank_sat` o `blank_sat_deformed`, bisogna riportare anche la stessa funzione rho del runner Optuna oppure aggiungere quella modalità al notebook manuale.
