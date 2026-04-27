# UO2 pO2 Verification

This folder contains the verification workflow for the UO2 oxygen partial
pressure and oxygen potential models implemented in SCIANTIX.

## Verification Scope

The UO2 workflow compares SCIANTIX against two external references:

- Thermo-Calc/OpenCalphad point files in
  `TEMPERATURES_THERMOCALC/` (`800.csv`, `1000.csv`, ..., `2600.csv`)
- the analytical Blackburn correlation

## Workflow

The verification is split into two main steps.

1. `run_temperature_sweep.py`
   - copies the template `input_*` files into one case directory per
     temperature from `800 K` to `2700 K`
   - updates the temperature prescribed in `input_history.txt`
   - runs `sciantix.x` for each case
   - collects all `output.txt` files into `temperature_sweep_summary.tsv`
   - produces the parent-folder SCIANTIX sweep plots:
     - `fuel_oxygen_partial_pressures_vs_ou_ratio.png`
     - `fuel_oxygen_potentials_vs_ou_ratio.png`

2. `sciantix_verification/compare_sciantix_with_oc_csv.py`
   - loads `temperature_sweep_summary.tsv`
   - reads the reference CSV files
     `TEMPERATURES_THERMOCALC/800.csv`, `1000.csv`, ..., `2600.csv`
   - rebuilds the analytical Blackburn reference
   - writes merged comparison tables and summary metrics:
     - `sciantix_verification/sciantix_vs_oc_csv.tsv`
     - `sciantix_verification/sciantix_vs_oc_csv_summary.tsv`
     - `sciantix_verification/sciantix_vs_oc_csv_summary.txt`
     - `sciantix_verification/sciantix_vs_blackburn_formula.tsv`
     - `sciantix_verification/sciantix_vs_blackburn_formula_summary.tsv`
     - `sciantix_verification/sciantix_vs_blackburn_formula_summary.txt`
   - produces comparison plots between SCIANTIX and the two references:
     - `sciantix_verification/sciantix_vs_oc_csv_pO2.png`
     - `sciantix_verification/sciantix_vs_oc_csv_log_pO2_error.png`
     - `sciantix_verification/sciantix_vs_oc_csv_log_pO2_error_absolute.png`
     - `sciantix_verification/sciantix_vs_oc_csv_log_pO2_error_relative_percent.png`
     - `sciantix_verification/sciantix_vs_oc_csv_linear_pO2_error.png`
     - `sciantix_verification/sciantix_vs_oc_csv_linear_pO2_error_absolute.png`
     - `sciantix_verification/sciantix_vs_oc_csv_linear_pO2_error_relative_percent.png`
     - `sciantix_verification/sciantix_vs_blackburn_formula_pO2.png`
     - `sciantix_verification/sciantix_vs_blackburn_formula_log_pO2_error.png`
     - `sciantix_verification/sciantix_vs_blackburn_formula_log_pO2_error_absolute.png`
     - `sciantix_verification/sciantix_vs_blackburn_formula_log_pO2_error_relative_percent.png`
     - `sciantix_verification/sciantix_vs_blackburn_formula_linear_pO2_error.png`
     - `sciantix_verification/sciantix_vs_blackburn_formula_linear_pO2_error_absolute.png`
     - `sciantix_verification/sciantix_vs_blackburn_formula_linear_pO2_error_relative_percent.png`

## Typical Usage

Run the full SCIANTIX temperature sweep and then the verification:

```bash
python3 run_temperature_sweep.py
```

If the sweep summary already exists and only the post-processing needs to be
refreshed, rerun the comparison stage directly:

```bash
python3 sciantix_verification/compare_sciantix_with_oc_csv.py
```

## Summary Metrics

The two text reports in `sciantix_verification/` provide compact metrics.

- `sciantix_vs_oc_csv_summary.txt`
- `sciantix_vs_blackburn_formula_summary.txt`

Each report includes:

- the number of compared points
- mean signed `log10(pO2 / p_ref)` error
- mean and maximum absolute `log10(pO2 / p_ref)` error
- mean and maximum relative `log10(pO2 / p_ref)` error
- a per-temperature summary table
