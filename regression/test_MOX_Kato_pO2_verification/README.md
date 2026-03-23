# MOX Kato pO2 Verification

This folder contains a solver-only verification workflow for the MOX oxygen
partial pressure and oxygen potential model implemented in SCIANTIX through the
Kato formulation.

Unlike [test_UO2_pO2_verification](/home/ecappellari/transparant/sciantix-official/regression/test_UO2_pO2_verification), this workflow does not use
OpenCalphad or a CALPHAD database. The comparison is only between:

- SCIANTIX
- the explicit NEA Kato correlation sampled over oxygen partial pressure / oxygen potential

For now the verification assumes:

- minor actinides content = `0`
- plutonium isotopic vector = pure `Pu239`
- prescribed `O/M` history (`iStoichiometryDeviation = 9`)

## What This Folder Does

1. `run_kato_sweep.py`
   - copies the template `input_*` files into one case directory per
     `(temperature, q, O/M)` combination
   - updates the prescribed temperature, `O/M`, and MOX content
   - runs `sciantix.x`
   - collects the final row of every `output.txt` into `kato_sweep_summary.tsv`
   - calls the plotting/comparison script

2. `sciantix_verification/compare_sciantix_with_kato.py`
   - loads `kato_sweep_summary.tsv`
   - samples the explicit NEA Kato equation over oxygen partial pressure
   - reconstructs `O/M`, `pO2`, and oxygen potential reference points without an inverse solver
   - produces both global plots with all `(T, q)` together and detailed plots one per `q`
   - writes comparison and residual summaries

## Default Sweep

- Temperatures: `800, 1000, 1200, 1400, 1600, 1800, 2000, 2200, 2400, 2600 K`
- Pu contents `q`: `0.10, 0.15, 0.20, 0.25, 0.30, 0.35`
- `O/M`: `1.95, 1.97, 1.99, 2.01, 2.03, 2.05, 2.07`

These defaults are chosen to stay inside a common `O/M` interval covered by the
explicit Kato correlation for all default `(T, q)` combinations. In the current
range, the limiting case is the lower `O/M` bound, which must remain at or above
`1.95` to avoid out-of-domain points for some low-temperature / low-plutonium cases.

These defaults are easy to change directly in `run_kato_sweep.py` or from the
command line.

## Typical Usage

Run the full sweep:

```bash
python3 run_kato_sweep.py
```

Regenerate only plots from an existing summary:

```bash
python3 run_kato_sweep.py --plot-only
```
