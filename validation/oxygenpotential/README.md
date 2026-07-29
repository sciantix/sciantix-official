# MOX Oxygen Potential Validation

Validates SCIANTIX's oxygen potential (Kato and OpenCalphad paths) against
experimental datasets.

Experimental measurements digitized from the NEA/NSC/R(2024)1 review are
grouped by (temperature, Pu/M) into two catergories:

- `freshfuel/` -- 23 sources on unirradiated fuel (Markin1965 ... Hirooka2022,
  Kato2005/2011a/2011b, ...)
- `burnup/` -- 8 sources on irradiated or simulated high-burnup fuel
  (Ewart1979a/b, Ewart1984, Johnson1973, Matzke1988, Sato1997, Tetenbaum1977,
  Woodley1978)

```bash
python3 -m testing.runner --oxygenpotential   # gold-diff for both groups
```

## Figures

Each group's `plot.py` reads case outputs (does not run SCIANTIX) and writes,
separately for the Kato (`figures_Kato/`) and CALPHAD (`figures_OC/`)
columns:

- `parity_oxygen_potential.png` -- calculated vs experimental, colored by
  temperature
- `mean_residual_oxygen_potential_by_source.png` -- mean (calc - exp) per
  source
- `sources/<Source>.png` -- per-source curves vs experimental points
- `oxygen_potential_plot_data.tsv` -- all points and residuals

```bash
cd freshfuel && python3 plot.py
cd burnup    && python3 plot.py
```

`combined_parity_plot.py` (this directory) combines both groups into the
paper's fresh/irradiated x Kato/OC parity figure.

## Graceful degradation without OpenCalphad

Each case's `output.txt` carries Kato-path and CALPHAD-path columns side by
side. Without OpenCalphad, the CALPHAD columns default to `0.0`; the runner
excludes them from the gold comparison and warns instead of failing. Passing
`--oc` to the runner asserts OpenCalphad is expected, so a forgotten
`Allmake.sh --oc` build fails loudly instead of silently degrading.
