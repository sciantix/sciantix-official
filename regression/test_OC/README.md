This is a regression test for the SCIANTIX/OpenCalphad coupling.
It is a UO2 matrix calculation: SCIANTIX evolves one simple history, calls
OpenCalphad for the matrix thermochemistry, then writes the oxygen potential
and oxygen partial pressure in `output.txt`.

- `input_history.txt` is the imposed external history.
- `input_initial_conditions.txt` is the initial fuel state.
- `input_settings.txt` enables the SCIANTIX physical models.
- `input_thermochemistry_settings.txt` tells SCIANTIX details on the coupling with OC.
- `input_thermochemistry.txt` says which OpenCalphad phases/species are copied
  into the SCIANTIX thermochemistry output.
- `run_test.py` runs the case, checks the final result against a gold file, and
  regenerates the plot.

## Imposed History

The history is in `input_history.txt`. The columns are the standard SCIANTIX
history columns:

```text
time (h)    temperature (K)    fission rate (fiss/m3/s)    hydrostatic stress (MPa)    system pressure (Pa)
```

This test imposes two points:

- at `0 h`: `T = 2000 K`, fission rate `1e19 fiss/m3/s`, stress `0 MPa`,
  system pressure `1e5 Pa`;
- at `25000 h`: the same values.

Please note that system pressure is required only when iTHERMOCHIMICA > 0 (i.e. when the coupling with OC is required).

The oxygen state starts from
`initial fuel stoichiometry deviation = 0.0` in `input_initial_conditions.txt`.

## OpenCalphad Setup

`input_thermochemistry_settings.txt` selects OpenCalphad only for the matrix:

```text
matrix.module = OPENCALPHAD
matrix.database = OU
matrix.elements = U, O
matrix.locations = matrix
```

The fission-product thermochemistry module is disabled here. This means the
OpenCalphad equilibrium is solved for the U-O matrix system using the `OU`
database. The OpenCalphad folder is assumed to be `OC/` at the SCIANTIX project
root unless an explicit `opencalphad.path` is provided.

## Oxygen Quantities

The oxygen quantities to inspect are in `output.txt`:

- `Fuel oxygen potential (KJ/mol)`: SCIANTIX selected fuel oxygen potential.
- `Fuel oxygen potential - CALPHAD (KJ/mol)`: value coming from the
  OpenCalphad matrix calculation.
- `Fuel oxygen potential - Blackburn (KJ/mol)`: Blackburn correlation.
- `Fuel oxygen potential - ML (KJ/mol)`: machine-learning model.
- `Fuel oxygen partial pressure (MPa)`: SCIANTIX selected oxygen pressure.
- `Fuel oxygen partial pressure - CALPHAD (MPa)`: OpenCalphad oxygen pressure.
- `Fuel oxygen partial pressure - Blackburn (MPa)`: Blackburn pressure.
- `Fuel oxygen partial pressure - ML (MPa)`: machine-learning pressure.

`thermochemistry_output.txt` contains the detailed OpenCalphad text output for
the requested thermochemistry variables. 

## Gold Check

The gold files are:

- `output_gold.txt`: expected SCIANTIX tabular output.
- `thermochemistry_output_gold.txt`: expected OpenCalphad text output.

`run_test.py` compares `output.txt` against `output_gold.txt` with a numerical
tolerance, then compares `thermochemistry_output.txt` against
`thermochemistry_output_gold.txt`. The script prints `PASS` or `FAIL`.

From `sciantix-official/`:

```bash
cmake --build build
python3 regression/test_OC/run_test.py
```

To re-check an existing `output.txt` without rerunning SCIANTIX:

```bash
python3 regression/test_OC/run_test.py --no-run
```

The script also writes `po2.png`, which compares the SCIANTIX, OpenCalphad,
Blackburn, and ML oxygen potentials over time.