# UN SCIANTIX Implementation Notes

## Files modified

- `src/models/IntraGranularBubbleBehavior.C`
- `src/classes/Simulation.C`
- `include/classes/Simulation.h`
- `src/classes/System.C`
- `src/classes/Solver.C`
- `include/classes/Solver.h`
- `src/models/GrainGrowth.C`
- `src/models/GasDiffusion.C`
- `include/models/GasDiffusion.h`
- `src/operations/SetMatrix.C`
- `include/operations/SetMatrix.h`
- `src/operations/SetSystem.C`
- `include/operations/SetSystem.h`
- `src/operations/SetVariables.C`
- `src/operations/SetVariablesFunctions.C`
- `src/operations/UpdateVariables.C`
- `src/file_manager/InputReading.C`
- `include/file_manager/InputReading.h`
- `src/coupling/TUSrcCoupling.C`
- `include/operations/SetVariablesFunctions.h`

## Comment markers

All UN-specific implementation and plumbing blocks are marked with
`// UN AD URANIUMNITRIDE`. Long blocks also use
`// END UN AD URANIUMNITRIDE`.

## Switches

- `iIntraGranularBubbleBehavior = 5`: UN notebook-8 intragranular plus grain-face/FGR model.
- `iUNDislocationDensity`:
  - `0`: constant matrix dislocation density.
  - `1`: smooth notebook-8 dynamic `rho_d(F,T)` law, default.
- `iUNVacancyDiffusivity`:
  - `0`: old A20-only code refit.
  - `1`: Rizk-2025 A20-only refit.
  - `2`: notebook-8 full vacancy refit, default.
- `iUNInterGranularBehavior`:
  - `0`: skip UN grain-face/FGR split.
  - `1`: enable UN grain-face/FGR split, default.

The existing solver switch remains `iDiffusionSolver = 4`; the existing
`SpectralDiffusion3equationsExchange` solver is reused.

## Variables added

Dedicated persistent UN block:

```text
170 Xe in dislocation bubbles
171 Dislocation bubble concentration
172 Dislocation bubble radius
173 Dislocation bubble volume
174 Dislocation gas atoms per bubble
175 Dislocation vacancies per bubble
176 Dislocation gas bubble swelling
177 Intragranular bulk gas bubble swelling
178 Bulk vacancies per bubble
179 Bulk bubble pressure
180 Bulk bubble equilibrium pressure
181 Dislocation bubble pressure
182 Dislocation bubble equilibrium pressure
183 Dislocation density
184 UN gas to grain boundary diagnostic
185 Grain-face bubble concentration
186 Grain-face atoms per bubble
187 Grain-face vacancies per bubble
188 Grain-face bubble radius
189 Grain-face bubble area
190 Grain-face bubble volume
191 Grain-face fractional coverage
192 Grain-face gas swelling
193 UN grain-face gas
194 UN released gas
195 UN fission gas release
196 UN total gas swelling
197 UN bulk nucleation rate
```

`Xe in dislocation bubbles` is now scalar-persistent through
`Sciantix_variables[170]`. Kr and He are not driven in the UN notebook-8 path;
the UN system registration now pushes Xe only.

## Equations implemented

- Bulk nucleation:
  `nu_b = 8*pi*f_n*D_g*Omega_fg^(1/3)*c^2`, with notebook-8 `f_n = 5.5e-4`.
- Gas solver nucleation mass coupling:
  `source_c = beta - 2*nu_b`, `source_mb = 2*nu_b`, `source_md = 0`.
  The same stored `UN bulk nucleation rate` is used by the gas solver and by
  the `N_b` update. It is computed while defining the UN 3x3 gas-diffusion
  model parameters, before `SpectralDiffusion3equationsExchange` is called.
- Bulk number:
  `N_b = (N_b_old + dt*nu_b)/(1 + dt*b_b*phi_b)`.
- Vacancy absorption:
  implicit solution of
  `dn_v/dt = 2*pi*D_v*delta*N/(kB*T*zeta)*(p - p_eq)` for bulk and
  dislocation bubbles.
- Bubble volume:
  `V_i = (Omega_fg*m_i + Omega*n_vi)/N_i`; radii from spherical volume.
- Dislocation coalescence:
  `N_d = N_d/(1 + 4*lambda*N_d*dV_d_positive)`.
- Dynamic dislocation density:
  smooth notebook-8 `rho_d(F,T)` update after coalescence; `N_d` scales by
  `rho_next/rho_old`.
- Grain-face bubbles:
  lenticular `cos^3(theta)` geometry, `N_gf` areal density,
  `q_gf`, `q_rel`, vacancy absorption with `D_v^gb = 1e6*D_v1`.
- FGR:
  once grain-face coverage reaches `F_c,sat = 0.5`, excess grain-face gas is
  moved to `UN released gas` and `Xe released`.

The timestep order for case 5 is:

```text
GasProduction/GasDecay -> GasDiffusion 3x3 -> UN growth/vacancies
-> dislocation coalescence -> rho_d update -> grain-face/FGR split
```

## Calibrated vs Rizk nominal parameters

- `f_n`: Rizk nominal `1.0e-6`; used/calibrated notebook-8 value `5.5e-4`.
- `K_d`: Rizk nominal `5.0e5`; used/calibrated value `3.0e5`.
- Dynamic `rho_d(F,T)`: Rizk nominal constant `rho_d = 3.0e13 m^-2`;
  used/calibrated smooth law with floor `3.0e13`, amplitude `7.5e14`,
  scale `0.10`, burnup scale `3.0 FIMA percent`, temperature half-point
  `1550 K`, width `120 K`, high-temperature floor factor `0.08`, cap `4.0e15`.
- Dislocation vacancy diffusivity multiplier: Rizk nominal multiplier `1.0`;
  used/calibrated value `10.0`.
- Vacancy diffusivity option `0`: Rizk nominal for U vacancies
  `A20 = 1.32e-19`, `B21 = -0.62`, `B22 = -0.04`;
  used/calibrated old refit `A20 = 4.6304523933553033e-29`,
  `B21 = -0.62`, `B22 = -0.04`.
- Vacancy diffusivity option `1`: Rizk nominal for U vacancies
  `A20 = 1.32e-19`, `B21 = -0.62`, `B22 = -0.04`;
  used/calibrated A20-only refit `A20 = 1.386341579723e-28`,
  `B21 = -0.62`, `B22 = -0.04`.
- Vacancy diffusivity option `2`: Rizk nominal for U vacancies
  `A20 = 1.32e-19`, `B21 = -0.62`, `B22 = -0.04`;
  used/calibrated notebook-8 full refit
  `A20 = 7.805188680989e-28`, `B21 = 9.932675113163e-01`,
  `B22 = 2.082395503235e-02`.
- Grain-face/FGR parameters not specified by Rizk intragranular nominal model:
  used/calibrated `N_gf,0 = 2.0e13 m^-2`, `delta_gb = 4.0e-10 m`,
  `F_c,sat = 0.5`, `theta = 1.0297442586766543 rad`,
  `R_gf,0 = 2.42e-10 m`, `D_v^gb multiplier = 1.0e6`,
  initial pressure factor `1.0/3.0`.

## Tests run

- Direct build check with `g++ -std=c++17 ... -o /tmp/sciantix_build_check.x`: passed.
- `cmake -S . -B build`: not run because `cmake` is not installed in this environment.
- Minimal UN case in `/tmp/sciantix_un_case`:
  - `iFuelMatrix=2`
  - `iFissionGasDiffusivity=11`
  - `iDiffusionSolver=4`
  - `iIntraGranularBubbleBehavior=5`
  - `iResolutionRate=4`
  - `iTrappingRate=2`
  - output finite; Xe mass balance relative residual about `2.2e-7`.
- UO2 smoke case copied from `regression/kashibe/test_Kashibe1991_2073K_23_4`:
  - executable completed and wrote `output.txt`.
- After the `// UN AD URANIUMNITRIDE` marker cleanup:
  - direct `g++` build check passed again;
  - minimal UN smoke case completed again;
  - UO2 smoke case completed again.

## Residual technical notes

- Dislocation-bubble diffusion modes remain internal to `Simulation`; the
  scalar `m_d` is persisted, but the public `Sciantix_diffusion_modes` array is
  unchanged for backward ABI compatibility.
- No UO2 coarsening physics/constants were copied.
