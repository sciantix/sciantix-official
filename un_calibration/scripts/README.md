# UN calibration scripts

All scripts are stand-alone Python files. Run each from the repo root, e.g.:

```bash
python un_calibration/scripts/smoke_test.py
python un_calibration/scripts/fig3_swelling_vs_T.py
python un_calibration/scripts/fig78_NdRd_vs_T.py
python un_calibration/scripts/fig9_gas_partition.py
python un_calibration/scripts/rho_d_diagnostic.py
python un_calibration/scripts/flag_ablation.py
```

Each script:

1. Bootstraps `model/` and `config/` onto `sys.path`.
2. Reads its run parameters (T-grids, burnups, dt, n_modes) from constants
   at the top of the file — **no `argparse`**. Edit the constants and rerun.
3. Writes outputs (PNG + CSV) under `un_calibration/reports/<script_name>/`.

## Where the parameters live

- **Rizk 2025 literature constants** → `un_calibration/config/rizk_constants.py`.
  Single source of truth. K_D = 5e5 (Rizk 2025 Sec. 4 calibration).
- **Free-fit / scales** → `un_calibration/config/manual_params.py`.
- **ρ_d laws** → `un_calibration/config/rho_d_laws.py`
  (constant default, `rho_d_FT` Blank-saturating, `rho_d_exp` Rizk-NEAMS 2023).
- **Builders** → `un_calibration/config/builder.py`.
  `build_un_params(T, bu, ...)` and `model_runner(T, bu, ...)` are the
  single entry points used by every script.
- **Physics flags** → `un_calibration/model/un_model.py` (module-level booleans).
  Scripts override them by assignment, e.g. `m.USE_PHI_GAS_RESOLUTION = False`.

## What each script does

| Script | Reproduces | Notes |
|---|---|---|
| `smoke_test.py` | – | Single point at T=1600 K, 1.3% FIMA. Reference numbers in docstring. |
| `fig3_swelling_vs_T.py` | Rizk 2025 Fig. 3 | swelling_b + swelling_d vs T at 1.1 / 1.3 / 3.2 % FIMA. Experimental points include ±10% Ronchi 1978 statistical error bars. |
| `fig4_diffusivity_vs_T.py` | Rizk 2025 Fig. 4 | Decomposed D_Xe and D_v vs T (D1, D3, total). Includes a "broken Tab. 2 D2_v" diagnostic curve. |
| `fig78_NdRd_vs_T.py` | Rizk 2025 Fig. 7+8 | N_d (m^-3) and R_d (nm) vs T at 1.3 % FIMA, with ±10% experimental error bars. |
| `fig9_gas_partition.py` | Rizk 2025 Fig. 9 | Stacked-area gas fractions vs T at 1.3 % FIMA |
| `rho_d_diagnostic.py` | – | 3D surface of the active ρ_d(F, T) law + Blank 1984 Table 3 anchors |
| `flag_ablation.py` | – | 2×2 study of (φ, mass-coupling) flags. CSV with RMSE + 4-panel figure. |

The notebook `notebooks/UN_clean.ipynb` is archived in `notebooks/archive/UN_clean_v0.ipynb`
and is no longer the source of truth.
