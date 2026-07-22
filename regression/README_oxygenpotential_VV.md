# MOX oxygen potential — standardized verification & validation

This document describes the standardized V&V process for the MOX oxygen
potential implemented in SCIANTIX, covering both computation paths:

- **Kato analytic correlation** (NEA/NSC/R(2024)1), `Solver::BisectionKato`
  (`src/classes/Solver.C`), selected for MOX fuel (`q = Pu/(U+Pu) > 0`) by the
  `StoichiometryDeviation` model;
- **OpenCalphad coupling** via the OCASI/liboctq interface
  (`src/coupling/OCUtilsCoupling.C`), Gibbs-energy minimization on the U-Pu-O
  CALPHAD database `upuo-v21.TDB`, enabled by `iThermochimica = 2`.

Run everything with a single command (binary in `build/sciantix.x` required):

```bash
python3 regression/run_oxygenpotential_vv.py
```

Stages can be skipped with `--skip-verification`, `--skip-validation`,
`--skip-plots`.

---

## 1. Verification — `regression/test_MOX_pO2_verification/`

Checks that SCIANTIX reproduces its own reference models exactly:

- **vs the explicit Kato equation**: the SCIANTIX Kato pO2/oxygen potential is
  compared point-by-point with a standalone Python implementation of the NEA
  correlation (`sciantix_verification/compare_sciantix_with_kato.py`);
- **vs Thermo-Calc reference tables**: the SCIANTIX-CALPHAD pO2/potential is
  compared with independent equilibrium tables computed with Thermo-Calc on the
  same system (`TEMPERATURES_THERMOCALC_Q_{10,20,30}/*.csv`, one file per
  temperature), via `sciantix_verification/compare_sciantix_with_oc_csv.py`.

Setup: `iFuelMatrix = 2` (MOX), Pu contents q = 0.10/0.20/0.30, temperatures
800-2600 K (200 K grid), `iStoichiometryDeviation = 8` (linear O/M ramp in
time, `x = x0 + 0.001 t[h]`), so each transient sweeps O/M at fixed T and q.
Comparison domain: T = 753-2550 K, Pu/M = 0.10-0.32, O/M = 1.92-2.08.

```bash
cd regression/test_MOX_pO2_verification
python3 run_temperature_sweep.py            # full sweep + comparisons + plots
python3 run_temperature_sweep.py --temperatures 1600 --q-values 0.2   # subset
```

Outputs: `temperature_sweep_summary.tsv`, plots in `plots/`, residual metrics
in `sciantix_verification/sciantix_vs_kato_summary.txt` and
`sciantix_vs_oc_csv_summary.txt` (+ `.tsv` per-(q,T) tables).

### OpenCalphad re-equilibration settings (important)

`input_thermochemistry_settings.txt` disables the lazy
re-equilibration cache for verification work:

```
coupling.max_stale_steps = 0
coupling.composition_tolerance = 0
coupling.temperature_tolerance = 0
```

By default SCIANTIX reuses the previous OpenCalphad equilibrium while the
composition stays within 1e-3 (relative), T within 2 K, for up to 20 steps.
The verification O/M ramp moves the composition by only ~3e-5 per time step,
so with default tolerances the CALPHAD pO2 is held constant for ~13 steps at a
time and the pO2 vs O/M curve becomes a staircase. With the settings above,
OpenCalphad is re-equilibrated at every time step (~24 ms/step; a full case is
~30 s instead of ~4 s).

Effect on the metrics (sweep of 2026-07-03, same binary):

| Metric (SCIANTIX-CALPHAD vs Thermo-Calc)   | cached (default) | every step |
|--------------------------------------------|------------------|------------|
| Mean abs log10(pO2/p_ref) error             | 2.38e-1          | 3.83e-2    |
| Mean relative log10(pO2/p_ref) error        | 2.42 %           | 0.32 %     |
| Max abs oxygen-potential error (kJ/mol)     | 185              | 107        |

The residual maxima concentrate at 800 K near O/M = 2.0, where pO2 varies by
orders of magnitude over a tiny O/M interval (interpolation, not model, error).
The Kato path is unaffected by the cache (analytic): mean abs log10 error
1.4e-5, bounded by the bisection tolerance.

### Acceptance criteria

- Kato path: max abs log10(pO2/p_ref) error < 1e-3 over the whole domain
  (pure numerics: bisection + interpolation).
- CALPHAD path: mean abs log10(pO2/p_ref) error < 0.05 and mean abs
  oxygen-potential error < 2 kJ/mol per (q, T) group for T >= 1000 K.

## 2. Validation — experimental datasets

Experimental oxygen-potential measurements digitized from the NEA/NSC/R(2024)1
review live in `validation_dataset/oxygenpotential/` (one file per literature
source + `ReadMe` with per-source notes). Cases are **generated, never edited
by hand**:

```bash
python3 validation_dataset/generate_cases.py --write-gold
```

For every source, the points are grouped by (temperature, Pu/M); each group
becomes a case `test_<Source>/T_<T>K_q_<Pu>/` containing the template inputs
of `test_MOX_pO2_verification` with `iStoichiometryDeviation = 9` (prescribed
O/M history sweeping the group's O/M range at fixed T) and the group's
`experimental_subset.txt`. `--write-gold` runs SCIANTIX in every case and
refreshes `output_gold.txt`.

The generator splits the sources into two regression groups:

- `regression/oxygenpotential_freshfuel/` — 23 sources on unirradiated fuel
  (Markin1965 ... Hirooka2022, Kato2005/2011a/2011b, ...);
- `regression/oxygenpotential_burnup/` — 8 sources on irradiated or simulated
  high-burnup fuel: Ewart1979a, Ewart1979b, Ewart1984, Johnson1973,
  Matzke1988, Sato1997, Tetenbaum1977, Woodley1978 (classification per the
  dataset `ReadMe`).

## 3. Figures

Each group directory has a `plot.py` that reads the case outputs (it does not
run SCIANTIX) and writes, separately for the Kato (`figures_Kato/`) and
CALPHAD (`figures_OC/`) columns of `output.txt`:

- `parity_oxygen_potential.png` — calculated vs experimental oxygen potential,
  colored by temperature;
- `mean_residual_oxygen_potential_by_source.png` — mean (calc - exp) per
  source;
- `sources/<Source>.png` — per-source curves vs experimental points;
- `oxygen_potential_plot_data.tsv` — all points and residuals (source, case,
  T, Pu/M, O/M, exp/calc potential, residual).

```bash
cd regression/oxygenpotential_freshfuel && python3 plot.py
cd regression/oxygenpotential_burnup   && python3 plot.py
```

## Notes

- These groups are standalone: they are not part of `python -m regression.runner`.
- `opencalphad.path` in the template `input_thermochemistry_settings.txt`
  must point to an OpenCalphad checkout whose `data/` contains `upuo-v21.TDB`.
- The template inputs are the single source of truth: changing
  `input_thermochemistry_settings.txt` there propagates to every regenerated
  validation case.
