# Intragranular coarsening — Nicodemo 2026

This branch starts from official SCIANTIX `2344575b7d246d02205f503b541b74f914f5f6b3`.
It contains one additional intragranular model (`iIntraGranularBubbleBehavior=4`),
with the same three gas inventories throughout the full history. Original
SCIANTIX bubble options remain unchanged. There is no executable reconstruction
of Barani 2020: its published Table 4 is retained only as comparison data.

## Model and files

- `IntraGranularBubbleBehavior.C`: conservative recruitment, implicit spectral
  equations for solution/small bubbles/large bubbles, vacancy root, coalescence,
  adaptive integration, and SCIANTIX state adapter.
- `IntraGranularBubbleCoarsening.C/h`: thermally accessible sites, gas and vacancy
  diffusivities, resolution, pressure and geometry.
- `DislocationDensity.C/h`: option **0**, constant `4e13 m^-2` (Barani density
  only); option **1**, the frozen White-fitted Nicodemo 2026 correlation. The
  full signed derivative includes temperature and burnup. It is the derivative
  of an algebraic correlation, not a kinetic equation for moving dislocations.

New routines and integration changes are marked `#COARSENING`.

## Fixed closure and scaling factors

`Kmax=3e6 m^-1`. A compact C2 sigmoid rises from zero at 1800 K to one at 1950 K;
`Katt=max_past K(T)`. Existing small bubbles, and their gas, are recruited through
`rho*dKatt + Katt*positive_density_generation*dt`. Cooling rebound does not
create sites; equal-temperature cycles do not reseed the same sites.

Line capture uses `(1-f)Dbulk+f*Dd`, with `f=pi*b^2*rho`, `b=0.385 nm`, and
`Dd=7.74e-6 exp[-1.21e-19/(kBT)] m²/s`. This Hart-type geometric closure applies
only to line capture; radial diffusion and small-bubble trapping retain the
native bulk diffusivity. An atomic-scale pipe radius is an assumption. A
homogenized diffusion average does not independently derive a local sink flux;
the pipe term in the line-capture rate scales approximately as `rho²`.

Vacancy mobility is `3*7.12e-7 exp[-4.72eV/(kBT)] m²/s`, without a separate
temperature gate. Pressure is ideal with `V=Omega*(n+nv)`, `Omega=4.09e-29 m³`;
`Peq=2*gamma/R-stress`, `gamma=0.7 J/m²`. Uptake uses only positive overpressure.
Pure merging conserves gas and total vacancy inventory. There is no emission law.

The three gas equations contain atomic resolution without `phi`. The separate
large-bubble number loss retains `phi=1/max(1,n-1)`. Small bubbles retain native
Turnbull resolution and the native number source; explicit dimer gas terms from
the printed Barani equations are not introduced. Stable gas yield is fixed at 0.27.

The optional trailing entry 29 in `input_settings.txt` is `iDislocationDensity`;
the CLI defaults it to1 for model 4. Three optional trailing entries after the
nine native values in `input_scaling_factors.txt` are:

| Index | Name | Default | Applies to |
|---|---|---|---|
|9|`sf_coarsening_K`|1|`Kmax`|
|10|`sf_coarsening_D`|1|the complete line-capture diffusivity|
|11|`sf_coarsening_Dv`|1|the complete vacancy diffusivity|

Existing nine-entry files remain valid. Coupled API callers selecting model 4
must provide these three unit factors and option 29 explicitly. Mode blocks 18–19
store Xe/Kr in large bubbles; state variables 170–190 store inventories, access
memory, derivatives and accepted source/escape ledgers for restarts.

The current scope is stable Xe/Kr in UO2, diffusion solver2, native transport
options1, physical production-proportional initial Xe/Kr composition, and no
He, HBS, boundary sweeping or non-uniform source. Unsupported combinations and
non-unit native kinetic scalings are rejected explicitly. Temperature and
fission-rate scalings retain their native meaning.

## Numerics and provenance

Implicit positive three-inventory solve; implicit vacancy root; adaptive local
error control, without a per-call radius cap. The40-mode native finite source
projection is retained. The public produced-gas balance and the accepted
projected-source/escape ledger are separate checks. Coarsening uses stable
history interpolation; legacy golds retain their original arithmetic. Controls
explicitly request the same stable interpolation.

Coarsening defaults to 200 outer steps per history interval. Numerical refinement
may use `SCIANTIX_TIME_STEPS` and `SCIANTIX_COARSENING_TOLERANCE`; these do not
select different physical laws. Kernel tolerance defaults to 2e-5.

`K`, the thermal window and effective mobility magnitude were constrained using
White data and rounded; they are not independently measured constants. The
density fit was also calibrated on White. Endpoint agreement is calibration,
not independent validation. The narrow predicted number dispersion remains a
limitation of the two mean-size populations and their survival/coalescence law.

`dislocation_density/fit.py` reproduces both the Veshchunov foundation and its
White adaptation from bundled digitized data. Veshchunov remains an internal
foundation for the fit, not a third selectable density option.

## Reproduction

```sh
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j2
ctest --test-dir build --output-on-failure
python3 context/dislocation_density/fit.py --cpp-grid build/coarsening_tests.x
python3 regression/White_Coarsening/run.py --binary build/sciantix.x
```

The regression includes 38 ramp histories and six 4070 constant-temperature
proxies, original legacy controls, four parity plots, gas inventories and 38
individual histories. Only measured White values are validation targets;
Barani values are published predictions. Two histories are unavailable, the
4163 histories lack the 41 h post-ramp hold, and original local 4070 histories
were not recovered. No fabricated Barani GB values or time curves are used.

## Cleanup verification

The clean port reproduces all 38 selected Hart endpoints (R,N,IG swelling)
exactly. The original 113 SCIANTIX gold comparisons pass unchanged. Kernel tests
cover conservation, full density derivatives, access memory, pure-merging
vacancy conservation and refinement. Separate-process native restarts are
byte-identical before, during and after recruitment. All three scaling factors,
both density options, old-file defaults and rejection of obsolete coarsening
selectors are checked by `run.py --check --verify --no-plots`, also run in CI.

On four representative cases, doubling the outer resolution and tightening the
adaptive tolerance changes R by at most 0.0019%, N 0.00022%, and IG swelling 0.0034%.
The preramp adaptive/legacy inventory difference is at most 0.032 percentage
points; on 4005-A it decreases from 0.0313 to 0.0159 under refinement. Physical
equivalence of the bulk limit does not imply bitwise equality of two solvers.
`clang-format18` is applied before committing; the upstream auto-format action
remains enabled. The regression gallery is generated and versioned only as
300 dpi PNG files, with the model labelled Nicodemo 2026.
