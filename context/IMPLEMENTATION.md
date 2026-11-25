# Selected coarsening implementation: file map

Comparison base: official SCIANTIX main, commit
`2344575b7d246d02205f503b541b74f914f5f6b3`. This is the complete added-model
diff, not only the last changes relative to the development branch.

## Retained model

`iIntraGranularBubbleBehavior=4` uses radial line capture from a cylindrical
cell with two diffusion regions and a fast-region outer radius of
`50b = 19.25 nm`. It combines direct capture at `R_d + 0.21 nm` with the
original spatial coalescence law. Its defaults include
`iDislocationDensity=1`, `iCoarseningLineCapture=1`, K=1.6e6 m^-1, D_v=30 D_v,bulk and a 1500 K
vacancy threshold. The optional scaling factors default to one. Density
option 0 supplies a constant 4e13 m^-2 for the same coarsening model.
Line-capture option 0 supplies Barani's Eq. (6) within the same solver.
Original SCIANTIX options remain available.

## Physics and solver

|File|Addition relative to nominal SCIANTIX|
|---|---|
|`src/models/IntraGranularBubbleBehavior.C`|Dispatch of option4; simultaneous implicit C/Mb/Md solve for each radial spectral mode; native bulk kinetics; density-driven number source; dislocation re-solution; implicit vacancy uptake; conservative coalescence and bulk engulfment; adaptive substeps; public-state adapter and gas-balance diagnostics. Native options retain their existing path.|
|`src/models/IntraGranularBubbleCoarsening.C`|Gas bulk diffusivity, selectable line capture (two-region radial cell or Barani Eq. 6), vacancy mobility, Setyawan dislocation re-solution, CS pressure, additive gas co-volume, bubble geometry, equilibrium pressure, vacancy flux and observations.|
|`include/models/IntraGranularBubbleCoarsening.h`|Selected constants and defaults; kernel state, environment and diagnostic types; public kernel declarations.|
|`src/models/DislocationDensity.C`|Frozen Veshchunov-White correlation and analytic derivatives in burnup and temperature; constant-density control. The retained fit includes its900K anchor and smoothed stretched thermal decay.|
|`include/models/DislocationDensity.h`|Density selector, response interface and oxide-to-heavy-metal burnup conversion.|
|`src/models/GasDiffusion.C`|Avoid advancing the gas a second time for option4; extract the existing native grain-boundary balance into a shared method.|
|`include/classes/Simulation.h`|Declarations for the coarsening adapter, diagnostics and shared GB balance; accessors for Xe/Kr dislocation spectral blocks.|
|`src/classes/Simulation.C`|Call coarsening inventory diagnostics after native GB storage and gas release.|

Nucleation adds empty trapping nuclei through K times the positive density
increment during irradiation at nondecreasing temperature. Fresh histories
start with Nd=0. There is no negative-density number sink and no cooling
rebound recruitment. Atomic re-solution has no phi; effective number loss
retains phi, separately from the atomic balance. See `COARSENING.md` for
equations, units and the explicit cavity-vacancy return closure.

## Inputs, state and numerics

|File|Addition relative to nominal SCIANTIX|
|---|---|
|`include/MainVariables.h`|Extend diffusion-mode blocks18 to20 for stable Xe and Kr in dislocation bubbles; keep the standard state-array capacity.|
|`src/file_manager/InputReading.C`|Optional trailing density and line-capture selectors at indices 29–30 and K/D/Dv scaling factors at indices 9–11. Older positional input files remain readable.|
|`src/operations/SetVariablesFunctions.C`|Register the new options and scaling-factor names.|
|`src/operations/SetVariables.C`|Register physical coarsening states, inventories, rho/rates, cumulative births, packing and flux ledgers; reserve native array positions.|
|`src/operations/UpdateVariables.C`|Write the extra state back to public arrays for restart/coupling. Slot184 is reserved and zero; no capture-activation state remains.|
|`src/MainSCIANTIX.C`|Use200 outer steps per interval for model4; allow explicit numerical refinement; enable stable local-coordinate interpolation for coarsening.|
|`include/file_manager/InputInterpolation.h`, `src/file_manager/InputInterpolation.C`|Optional local-coordinate linear interpolation to avoid cancellation at late-history knots. The native interpolation path remains the default for native runs.|
|`src/file_manager/Output.C`|Write17-digit coarsening output for conservation/refinement diagnostics; retain native output precision for other models.|
|`utilities/InputExplanation.md`|Selected option, constants, supported input structure and scaling documentation.|
|`utilities/inputExample/print_input_settings.py`, `print_input_scaling_factors.py`|Generate the optional density and line-capture selectors and trailing coarsening factors.|

Slots170-190 contain the extra coarsening diagnostics/state, with184 reserved;
blocks18-19 contain the dislocation gas modes. Existing modes are not
renumbered. Fresh runs and current-format separate-process restarts are
verified; outputs/checkpoints from older experimental versions must be
regenerated.

## Regression and reproducibility

|File or directory|Purpose|
|---|---|
|`regression/White_Coarsening/run.py`|Run the actual binary for38 published input histories, once with option4 and once with native single-size; hash inputs/source/binary; summarize physical endpoints and inventories.|
|`regression/White_Coarsening/verify.py`|Scaling controls, unsupported-option checks, K=0 native limit, packing and conservation, outer-step/tolerance refinement, and separate-process public-array restarts. Pre-ramp physical differences are reported rather than forced to zero.|
|`regression/White_Coarsening/plot.py`|PNG comparisons with SCIANTIX single-size and the published Barani endpoints, bubble radii and gas inventories. Detailed temperature, pressure, vacancy, radius and swelling histories cover all 38 cases, with nine ramp zooms.|
|`regression/White_Coarsening/inputs`, `data`, input templates|Published histories, White measurements and published Barani endpoints for the merit comparison.|
|`regression/White_Coarsening/expected_endpoints.csv`|Numerical references for76 runs. These are regression golds, not independent experimental validation.|
|Other CSV/JSON/PNG files in that group|Regenerated endpoints, gas inventories, metrics, source/binary provenance and figures. Raw run caches remain ignored.|
|`tests/coarsening_tests.C`|Kernel invariants, conservation, EOS/packing, density derivatives and number generation, vacancy threshold and isolated-sphere flux limit, coalescence and refinement; shares the production kernel.|
|`tests/coarsening_restart.C`|Separate-process restart driver using public state/mode arrays.|
|`CMakeLists.txt`|Build and register coarsening kernel tests.|
|`.github/workflows/ci.yml`|Run native regressions plus the selected White regression and verification after the build.|
|`.gitignore`|Ignore binaries, raw runs and temporary reports; retain selected PNGs and exclude restricted data.|
|`context/dislocation_density/fit.py`, `selected_parameters.json`, public data and figures|Reproduce the public Veshchunov fit and selected correlation; verify Python/C++ parity. Restricted White observations and refit diagnostics are excluded.|

The regression uses the existing38 histories. In particular, the4163 input
histories omit the41h hold listed in Barani Table3; the regression README flags
this coverage limitation. No experimental observation is assigned an invented
transient timestamp. All exploratory parameter scans remain outside the
public checkout.
