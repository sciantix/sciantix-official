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
sources and inputs (hashes), runs 114 simulations, compares the outputs with
reference_outputs.json (final row and column maximum, relative tolerance 1e-4) and
regenerates figures. Byte-exact output hashes were replaced on 14 November 2026: 14 of
114 outputs differed byte-wise on the CI runner (different compiler). A -ffast-math build
stays within 3.6e-5. After a deliberate model change, inspect the differences, then run
verify_reference.py --write-outputs and record the change here. It does not overwrite existing analytic inputs.

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

## Reference regenerated on 12 November 2026

The reference (reference_manifest.json) was regenerated after the corrections of the White (2006)
check (see the "regenerated" entry of the manifest). The legacy outputs are unchanged; the barani
and analytic_rho outputs changed. The original reference is commit bbbf85dc. input_check.txt files
were removed from the recorded inputs: SCIANTIX writes them during the run and Git ignores them, so
the pre-run input check failed on a clean checkout. RMSE log10 R/Nd/IG/GB of analytic_rho:
0.299 / 0.666 / 0.504 / 0.306 (bbbf85dc: 0.225 / 0.637 / 0.594 / 0.375).

## Model and scope

The selected model uses rho(T,B) from the DislocationDensity model (fit of Veshchunov-Shestak
2009 Fig. 5, T held at 1500 K above it), line capture with the matrix diffusivity (D_d = D), and
births K*max[rho(B_new,T_new)-rho(B_old,T_new),0], K=1e5 m^-1 (calibrated on White 2006). Births consume two
solution-gas atoms per embryo and start with zero vacancies. The one-off initial
seed remains independent of rho. Cooling alone does not nucleate bubbles.
Coverage-triggered release, KJMA kinetics and loop punching are absent.

Vacancy uptake (cell geometric factor zeta) runs only above 1873 K and stops at mechanical
equilibrium in each step (no 0.2 nm/call limiter; the 250 nm guard remains). Gas in dislocation
bubbles follows Eq. (1) with Solver::Decay; beta_d + beta' enters the trapping of the dissolved gas.
Native bulk transport, EOS and coalescence are unchanged. Fit extrapolation above 1500 K is
unvalidated. Exact reproduction establishes numerical parity with the saved reference, not full
physical validation or timestep convergence. See [context](../../context/README.md).

## Figures

- figures/01_parity_comparison.png: all three configurations plus published predictions.
- figures/02_gas_inventory.png: three gas-inventory series.
- figures/03_transient_4005A.png: selected analytic model, retaining the original time window.
- figures/04_error_metrics.png and error_metrics.csv: errors against experiments.

The transient display origin is +290.8 h; elapsed durations are preserved.
Generated figures and runtime logs are ignored by Git.
