# Optuna 4test_UN calibration summary
- Best trial: `247`
- Best objective: `37.172984`

## Best parameters
- `rho_mode` = `constant`
- `rho_const_scale` = `3.413095975131213`
- `K_d` = `2621677.73617003`
- `f_n` = `1.2899987629574664e-07`
- `Dg_dislocation_scale` = `1.4477213168163199`
- `Dv_dislocation_scale` = `4.302780566294486`
- `first_gas_pressure_factor` = `0.49384240460136286`

## Best component scores
- `score_Nd_drop` = `0.11547849827672796`
- `score_guards` = `0.0018817200247795726`
- `score_partition` = `2.3489872013057966`
- `score_pressure` = `0.003308734814974107`
- `score_radius_T` = `0.9179183309500204`
- `score_swelling_T` = `0.11906465490922069`
- `score_swelling_burnup` = `0.13672272217406878`

## Fixed parameters in this run
- `gb_scale = 1.0`
- `gd_scale = 1.0`
- `b_scale = 1.0`
- `coalescence_d_scale = 1.0`
- `capture_scale = 1.0`

## Notebook paste block for manual rerun
```python
CASE_LABEL = "optuna_best_00247"
RHO_MODE = "constant"  # Optuna script monkey-patches deformed rho; see params below
FIRST_GAS_PRESSURE_FACTOR = 0.493842404601
MANUAL_PARAMS.update({
    "f_n": 1.289998762957e-07,
    "K_d": 2.621677736170e+06,
    "rho_d": 1.023928792539e+14,
    "Dg_dislocation_scale": 1.44772131682,
    "Dv_dislocation_scale": 4.30278056629,
    "gb_scale": 1.0,
    "gd_scale": 1.0,
    "b_scale": 1.0,
    "coalescence_d_scale": 1.0,
    "capture_scale": 1.0,
})
```

**Nota:** se il best usa `blank_sat` o `blank_sat_deformed`, bisogna riportare anche la stessa funzione rho del runner Optuna oppure aggiungere quella modalità al notebook manuale.
