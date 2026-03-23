# MOX pO2 Verification

This folder contains the verification workflow for the MOX oxygen partial
pressure and oxygen potential model implemented in SCIANTIX.

## Verification Scope

This case is a solver-only verification. Unlike the UO2 case, it does not use
OpenCalphad and a CALPHAD database. The comparison is between:

- SCIANTIX
- the explicit NEA Kato correlation sampled over oxygen partial pressure (NEA (2025), Recommendations on Fuel Properties for Fuel Performance Codes, OECD Publishing, Paris)

For the current setup, the verification assumes:

- minor actinides content = `0`, not yet implemented a MA-MOX matrix
- evolving `O/M` with `iStoichiometryDeviation = 8`
- Temperatures: `800, 900, 1000, ..., 2600 K`
- Pu contents `q`: `0.10, 0.15, 0.20, 0.25, 0.30, 0.35`


## Workflow

The verification is split into two main steps.

1. `run_temperature_sweep.py`
   - copies the template `input_*` files into one case directory per
     `(temperature, q)` combination, organised as `temperature/q`
     (for example `1600K/q_0p20`)
   - updates only the prescribed temperature and the MOX `q` value
   - leaves the `O/M` transient entirely defined by the template
     `input_history.txt` and `input_initial_conditions.txt`
   - runs `sciantix.x` for each case
   - collects all `output.txt` files into
     `temperature_sweep_summary.tsv`
   - calls the comparison and plotting script

2. `sciantix_verification/compare_sciantix_with_kato.py`
   - loads `temperature_sweep_summary.tsv`
   - samples the explicit NEA Kato equation over oxygen partial pressure
   - writes merged comparison tables and summary metrics:
     - `sciantix_verification/sciantix_vs_kato_points.tsv`
     - `sciantix_verification/sciantix_vs_kato_comparison.tsv`
     - `sciantix_verification/sciantix_vs_kato_residuals.tsv`
     - `sciantix_verification/sciantix_vs_kato_summary.txt`
   - produces detailed plots, one per `q`, for:
     - oxygen partial pressure
     - oxygen potential
     - signed absolute log-pressure error
     - unsigned absolute log-pressure error
     - signed relative log-pressure error
     - unsigned relative log-pressure error


## Typical Usage

Run the full SCIANTIX temperature sweep and then the verification:

```bash
python3 run_temperature_sweep.py
```

Regenerate only the comparison tables and plots from an existing summary:

```bash
python3 run_temperature_sweep.py --plot-only
```

## Summary Metrics

After the comparison step, the file
`sciantix_verification/sciantix_vs_kato_summary.txt` provide compact metrics, including:

- the number of compared points
- mean signed `log10(pO2 / p_ref)` error
- mean and maximum absolute `log10(pO2 / p_ref)` error
- mean absolute relative `log10(pO2 / p_ref)` error
- mean and maximum absolute oxygen-potential error
- a per-`(q, temperature)` summary table
