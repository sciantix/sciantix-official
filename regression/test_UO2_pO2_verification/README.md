# UO2 pO2 Verification

This folder contains a regression-style verification workflow for the UO2 oxygen
partial pressure and oxygen potential models implemented in SCIANTIX.

## What This Folder Does

The workflow is split into two steps:

1. `run_temperature_sweep.py`
   - Copies the template `input_*` files into one case directory per
     temperature from `800K` to `2700K`
   - Updates the temperature prescribed in `input_history.txt`
   - Runs `sciantix.x` for each case
   - Collects all `output.txt` files into `temperature_sweep_summary.tsv`
   - Produces:
     - `fuel_oxygen_partial_pressures_vs_ou_ratio.png`
     - `fuel_oxygen_potentials_vs_ou_ratio.png`

2. `sciantix_verification/compare_sciantix_with_oc_csv.py`
   - Loads `temperature_sweep_summary.tsv`
   - Reads the standalone OpenCalphad CSV files in
     `sciantix_verification/`
   - Reconstructs OpenCalphad oxygen partial pressure and oxygen potential
     from oxygen activity
   - Produces comparison plots between SCIANTIX and OpenCalphad:
     - `sciantix_verification/sciantix_vs_oc_csv_pO2.png`
     - `sciantix_verification/sciantix_vs_oc_csv_pO2_2.png`
