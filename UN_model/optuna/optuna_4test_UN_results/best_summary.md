# Optuna 4test_UN calibration summary
- Best trial: `267`
- Best objective: `3.7751715`

## Best parameters
- `rho_mode` = `constant`
- `rho_const_scale` = `4.195160164942472`
- `K_d` = `2869991.9368489077`
- `f_n` = `1.0609704427004406e-06`
- `Dg_dislocation_scale` = `1.762525632685351`
- `Dv_dislocation_scale` = `7.044950531210412`
- `first_gas_pressure_factor` = `0.21623790810835175`

## Best component scores
- `score_Nd_drop` = `0.11451504133247974`
- `score_guards` = `4.4055746177703147e-07`
- `score_partition` = `1.0987806029547127`
- `score_pressure` = `7.284492746592773e-05`
- `score_radius_T` = `2.353422055787738`
- `score_swelling_T` = `0.1997576789977684`
- `score_swelling_burnup` = `0.3341147120988067`

## Fixed parameters in this run
- `gb_scale = 1.0`
- `gd_scale = 1.0`
- `b_scale = 1.0`
- `coalescence_d_scale = 1.0`
- `capture_scale = 1.0`

## Notebook paste block for manual rerun
```python
CASE_LABEL = "optuna_best_00267"
RHO_MODE = "constant"  # Optuna script monkey-patches deformed rho; see params below
FIRST_GAS_PRESSURE_FACTOR = 0.216237908108
MANUAL_PARAMS.update({
    "f_n": 1.060970442700e-06,
    "K_d": 2.869991936849e+06,
    "rho_d": 1.258548049483e+14,
    "Dg_dislocation_scale": 1.76252563269,
    "Dv_dislocation_scale": 7.04495053121,
    "gb_scale": 1.0,
    "gd_scale": 1.0,
    "b_scale": 1.0,
    "coalescence_d_scale": 1.0,
    "capture_scale": 1.0,
})
```

**Nota:** se il best usa `blank_sat` o `blank_sat_deformed`, bisogna riportare anche la stessa funzione rho del runner Optuna oppure aggiungere quella modalità al notebook manuale.
