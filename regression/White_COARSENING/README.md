# White coarsening regression

Three configurations share the same 38 White locations:

| Folder | Configuration |
|---|---|
| legacy | Native SCIANTIX |
| barani | Frozen Hart a=1.5b, constant rho; SCIANTIX_NUCLEATION=control |
| analytic_rho | Selected analytic rho(T,B) and gas-limited births; executable default |

The published Table 4 contains 40 locations; histories for 4136-E and 4140-C are
unavailable. cases.csv keeps experimental data separate from published predictions.
Published intergranular swelling is unavailable and is never filled with local values.

## Build and run (Linux/WSL, from repository root)

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j 6
ctest --test-dir build --output-on-failure
python3 regression/White_COARSENING/run_all.py --jobs 6
```

The runner sanitizes inherited SCIANTIX_* environment overrides, verifies scientific
sources and inputs, runs 114 simulations, checks byte-exact reproduction and
regenerates figures. It does not overwrite existing analytic inputs.

reference_manifest.json records hashes from the original local analytic_rho campaign
and frozen legacy/Hart outputs. Text sources and inputs normalize CRLF to LF;
output hashes use raw bytes. verify_reference.py fails on any changed source,
input or output. Recheck saved results with:

```bash
python3 regression/White_COARSENING/verify_reference.py
python3 regression/White_COARSENING/plot.py
```

reproduction_validation.json records exact agreement; validation.json records
physical/numerical checks. results.csv, metrics.csv and inventory.csv summarize
all configurations. The reference manifest must not be regenerated merely to
accept a changed model.

The independent direct-workspace audit is saved in local_parity_validation.json:
79 source files, 342 scientific inputs, 114 output files and 114 analytic trace
files match the original local work (only source/input line endings are normalized).
The auto-format workflow skips this research branch to preserve audited source
hashes; CI reruns both the standard suite and this coarsening comparison.

The 12 November clean Release build passed all three CTest targets and all
113 standard regression cases without changing gold files. The 114 White runs
and analytic diagnostic traces were reproduced exactly. Generated case outputs,
traces and figures remain available locally and are excluded from commits.

## Model and scope

The selected model uses direct fitted rho(T,B), Hart weighting with a=1.5b, and
births K*max[rho(B_new,T_new)-rho(B_old,T_new),0], K=1e6 m^-1. Births consume two
solution-gas atoms per embryo and start with zero vacancies. The one-off initial
seed remains independent of rho. Cooling alone does not nucleate bubbles.
Coverage-triggered release, KJMA kinetics and loop punching are absent.

Native bulk transport, EOS, original coalescence and absorption-only uptake
(0.2 nm/call and 250 nm during uptake) remain unchanged. Fit extrapolation outside
approximately 3-93 GWd/tHM and 900-1400 K is unvalidated. Exact reproduction
establishes numerical parity with the saved local work, not full physical
validation or timestep convergence. See [context](../../context/README.md).

## Figures

- figures/01_parity_comparison.png: all three configurations plus published predictions.
- figures/02_gas_inventory.png: three gas-inventory series.
- figures/03_transient_4005A.png: selected analytic model, retaining the original time window.
- figures/04_error_metrics.png and error_metrics.csv: errors against experiments.

The transient display origin is +290.8 h; elapsed durations are preserved.
Generated figures and runtime logs are ignored by Git.
