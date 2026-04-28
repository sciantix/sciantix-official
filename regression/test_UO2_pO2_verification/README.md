# UO2 pO2 Verification

This folder verifies the UO2 oxygen partial pressure and oxygen potential
models in SCIANTIX. It runs a temperature sweep and compares SCIANTIX against external
OpenCalphad/Thermo-Calc data and the Blackburn analytical correlation.

- the parent folder contains the input templates and the script that creates
  all SCIANTIX cases;
- the `800K`, `1000K`, ..., `2600K` folders are the actual generated cases;
- `sciantix_verification/` compares the generated SCIANTIX summary with the
  independent references.

## Imposed History

The template history is `input_history.txt`. Its columns are:

```text
time (h)    temperature (K)    fission rate (fiss/m3/s)    hydrostatic stress (MPa)    system pressure (Pa)
```

The template contains:

```text
0      800    0    0    1e5
300    800    0    0    1e5
```

`run_temperature_sweep.py` copies the template inputs into one folder per
temperature and overwrites the second column of `input_history.txt`. Therefore
each generated case has the same story:

- fixed temperature for `300 h`;
- no imposed fission rate;
- zero hydrostatic stress;
- system pressure fixed at `1e5 Pa`.

The initial fuel state is in `input_initial_conditions.txt`. The relevant
oxygen starting point is `initial fuel stoichiometry deviation = -0.10`, so the
case begins from hypostoichiometric UO2-x.

## OpenCalphad Setup

`input_thermochemistry_settings.txt` enables the OpenCalphad matrix solve:

```text
matrix.module = OPENCALPHAD
matrix.database = OU
matrix.elements = U, O
matrix.locations = matrix
```

The fission-product thermochemistry module is disabled. 

## Oxygen Quantities

In every generated `output.txt`, the important oxygen columns are:

- `O/U ratio (/)`;
- `Fuel oxygen potential (KJ/mol)`;
- `Fuel oxygen potential - CALPHAD (KJ/mol)`;
- `Fuel oxygen potential - Blackburn (KJ/mol)`;
- `Fuel oxygen potential - ML (KJ/mol)`;
- `Fuel oxygen partial pressure (MPa)`;
- `Fuel oxygen partial pressure - CALPHAD (MPa)`;
- `Fuel oxygen partial pressure - Blackburn (MPa)`;
- `Fuel oxygen partial pressure - ML (MPa)`.

`thermochemistry_output.txt` inside each temperature folder contains the raw
OpenCalphad equilibrium printout for that case.

## Temperature Sweep

Run from `regression/test_UO2_pO2_verification/` after building SCIANTIX:

```bash
python3 run_temperature_sweep.py
```

The script copies the compiled executable, prepares the temperature folders,
runs SCIANTIX, and writes:

- `temperature_sweep_summary.tsv`, the combined table for all temperatures;
- `fuel_oxygen_partial_pressures_OC_Blackburn_ML.png`;
- `fuel_oxygen_potentials_vs_ou_ratio.png`;
- pairwise pressure plots comparing OpenCalphad, Blackburn, and ML.

## Reference Verification

The reference data are in `TEMPERATURES_THERMOCALC/` as one CSV file per
temperature. `sciantix_verification/compare_sciantix_with_oc_csv.py` reads
those CSV files, reads `temperature_sweep_summary.tsv`, rebuilds the analytical
Blackburn pressure, and writes comparison tables, plots, and compact summaries.

Run it from this folder:

```bash
python3 sciantix_verification/compare_sciantix_with_oc_csv.py
```

The two summary files are:

- `sciantix_verification/sciantix_vs_oc_csv_summary.tsv`;
- `sciantix_verification/sciantix_vs_blackburn_formula_summary.tsv`.

Gold copies of those summaries are stored in `sciantix_verification/gold/`.
The comparison script now checks the regenerated summaries against the gold
files and prints `PASS` if the aggregate metrics have not changed.
