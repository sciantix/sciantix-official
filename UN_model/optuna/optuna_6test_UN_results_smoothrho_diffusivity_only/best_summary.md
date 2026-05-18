# Optuna 6test_UN smooth-rho diffusivity-only calibration summary
- Best trial: `97`
- Best objective: `39.15979`

## Best parameters
- `Dg_scale` = `0.4360260702654266`
- `Dv_scale` = `0.22527322193875118`
- `Dg_dislocation_scale` = `2.6910810613710288`
- `Dv_dislocation_scale` = `0.5963941841990619`

## Best component scores
- `score_Nd_drop` = `0.0`
- `score_guards` = `3.477828159954734`
- `score_partition` = `0.44963531617926134`
- `score_pressure` = `0.073388901301253`
- `score_radius_T` = `0.42199037778436593`
- `score_swelling_T` = `0.366082968516319`
- `score_swelling_burnup` = `0.36698388173971513`

## Fixed parameters in this run
- `gb_scale = 1.0`
- `gd_scale = 1.0`
- `b_scale = 1.0`
- `coalescence_d_scale = 1.0`
- `USE_BULK_DISLOCATION_CAPTURE = False`
- `capture_scale = 0.0`

## Notebook paste block for manual rerun
```python
CASE_LABEL = "optuna_best_00097"
RHO_MODE = "rizk2023_smooth"
USE_BULK_DISLOCATION_CAPTURE = False
MANUAL_PARAMS.update({
    "Dg_scale": 0.436026070265,
    "Dv_scale": 0.225273221939,
    "Dg_dislocation_scale": 2.69108106137,
    "Dv_dislocation_scale": 0.596394184199,
    "gb_scale": 1.0,
    "gd_scale": 1.0,
    "b_scale": 1.0,
    "coalescence_d_scale": 1.0,
    "capture_scale": 0.0,
})
```

**Nota:** la dislocation density è fissa: `RHO_MODE = rizk2023_smooth`, con i parametri della prima cella di `6test_UN.ipynb`.
