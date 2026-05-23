# CODEX TASK — Implement the UN fission-gas model in SCIANTIX

**Target repository/branch:** `sciantix/sciantix-official`, branch `development/nitride`  
**Reference branch:** `development/CoarseningUO2`  
**Reference theory files to read first:**  
- `CONTEXT.md`
- `UNcode.md`
- `UNmodel.md`
- `UNintergranular_theory.md`
- `UO2_COARSENING_TO_UN_SCIANTIX_REPORT.md`

**Important rule:** the UO2 coarsening branch is a **software architecture template only**. Do not merge it into `development/nitride` and do not copy UO2 physics/constants into UN.

---

## 0. What Codex must do before coding

1. Confirm current branch:
   ```bash
   git status -sb
   git branch --show-current
   ```
   The branch should be:
   ```text
   development/nitride
   ```

2. Read these local/project theory files:
   ```text
   CONTEXT.md
   UNcode.md
   UNmodel.md
   UNintergranular_theory.md
   UO2_COARSENING_TO_UN_SCIANTIX_REPORT.md
   ```

3. Inspect, but do not merge, the UO2 coarsening branch:
   ```text
   development/CoarseningUO2
   ```

4. Produce a short implementation map before patching:
   ```text
   context/CODEX_PREPATCH_UN_AUDIT.md
   ```
   It must list:
   - current UN files already modified;
   - current solver status;
   - current variable-index status;
   - new files/variables/switches to add;
   - exact patch plan.

---

## 1. Current state of `development/nitride` from read-only audit

### 1.1 Already present: UN matrix selection

`src/operations/SetMatrix.C` already has:

```cpp
case 2:
{
    matrices.push(UN(...));
}
```

The `UN(...)` matrix already sets some UN values:

```text
name = UN
theoretical_density = 14300 kg/m3   # marked TODO in code
lattice_parameter = 4.889e-10 m
dislocation_density = 3.0e13 m^-2
dislocation_core_radius = 3.46e-10 m
surface_tension = 1.11 J/m2
grain_boundary_thickness = 4.0e-10 m
semi-dihedral angle = 59 deg
```

But the current code still has TODO/placeholders:

```text
SchottkyVolume = 4.09e-29 m3       # copied from UO2 / TODO
OctahedralInterstitialSite = 0.0   # TODO
Elastic modulus / Poisson ratio = placeholders
```

**Codex action:** keep the existing UN matrix hook, but cleanly centralize UN constants and avoid leaving misleading UO2 placeholders for quantities actually used by the UN model.

---

### 1.2 Already present: UN system selection

`src/operations/SetSystem.C` already has:

```cpp
case 2: // UN
    Xe_in_UN(...)
    Kr_in_UN(...)
    He_in_UN(...)
```

For the first implementation, focus on Xe. Kr and He can remain formal, but should not drive the calibration unless explicitly requested.

Current `Xe_in_UN(...)` sets:

```text
yield = 0.24
radius_in_lattice = 0.21e-9 m
volume_in_lattice = matrices["UN"].getSchottkyVolume()
bulk_nucleation_factor = 1.0e-6
iFissionGasDiffusivity = user option, expected case 11
iResolutionRate = 4 for UN bulk/dislocation
iTrappingRate = 2 for UN bulk/dislocation
```

**Codex action:** keep Xe yield default at `0.24`. Do not change default to `0.475` unless a new explicit yield-mode switch is added and documented as "volatile-equivalent Rizk yield".

---

### 1.3 Already present: UN Xe diffusivity

`src/classes/System.C` already has:

```cpp
case 11:
```

for Xe in UN:

```text
D_g = D1 + D3
D1 = 1.56e-3 * exp(-4.94/(kB*T))
D2 = 0
D3 = 1.85e-39 * Fdot
```

This is acceptable as the first default.

**Codex action:** leave this available, but add documentation and optionally expose D2_Xe later as a switch. Do not silently change the default.

---

### 1.4 Already present: UN resolution rates

`src/classes/System.C` already contains:

```cpp
System::setResolutionRatesUN(...)
```

with:

```text
b_b = Fdot * b0(R_b)
b_d = Fdot * b0(R_d)
```

and:

```text
b0(R) = 1.0e-25 * (2.64 - 2.02 * exp(-2.61e-9/R))
```

**Codex action:** reuse this. Verify whether the code uses `R + radius_in_lattice`; if so, keep convention consistent and document it.

---

### 1.5 Already present: UN trapping rates

`src/classes/System.C` already contains:

```cpp
System::setTrappingRatesUN(...)
```

with:

```text
g_b = 4*pi*D_g*R_b_eff*N_b
```

and:

```text
g_d =
4*pi*D_g*R_d_eff*N_d
+
(2*pi*D_g/den) * free_dislocation
```

where:

```text
Gamma_d = 1/sqrt(pi*rho_d)
free_dislocation = max(rho_d - 2*R_d*N_d, 0)
den = log(Gamma_d/(Z_d*r_d)) - 3/5
Z_d = 5
```

**Codex action:** reuse this structure. Add strong guards against invalid denominators and negative free-dislocation length.

---

### 1.6 Already present: 3-equation UN gas solver

The 3-equation gas solver already exists.

In `src/models/GasDiffusion.C`, `iDiffusionSolver = 4` calls:

```cpp
defineSpectralDiffusion3EquationsExchange(...)
```

and then:

```cpp
solver.SpectralDiffusion3equationsExchange(...)
```

with:

```text
c
m_b
m_d
```

In `src/classes/Solver.C`, the function already solves the backward-Euler 3x3 exchange problem:

```text
dc/dt   = D_g ∇²c - (g_b + g_d)c + b_b*m_b + b_d*m_d + beta
dm_b/dt = g_b*c - b_b*m_b
dm_d/dt = g_d*c - b_d*m_d
```

Parameter vector:

```text
0 n_modes
1 D_g
2 grain_radius
3 beta
4 g_b
5 g_d
6 b_b
7 b_d
```

**Codex action:** do not rewrite this solver unless a bug is found. The missing part is not the 3x3 gas solver; the missing part is the bubble microstructure evolution coupled to it.

---

### 1.7 Current serious gap: `IntraGranularBubbleBehavior.C` has no UN two-population model

Current `src/models/IntraGranularBubbleBehavior.C` in `development/nitride` only has legacy cases:

```text
0 constant bubble concentration/radius
1 Pizzocri et al.
2 White and Tucker
3 helium/similarity ratio
99 no intragranular bubbles
```

There is no implemented UN case for:

```text
bulk bubbles + dislocation bubbles
N_b evolution
N_d evolution
vacancy absorption
pressure diagnostics
dislocation coalescence
bulk/dislocation swelling split
```

**Codex action:** this is the central patch. Add a UN-specific behavior case, preferably:

```text
iIntraGranularBubbleBehavior = 5
```

Do not use UO2 `case 4` physics. `case 4` in `development/CoarseningUO2` is only a software template.

---

### 1.8 Current serious gap: dislocation variables are not cleanly persisted

Current `development/nitride` already defines:

```text
"Dislocation bubble concentration"
"Dislocation bubble radius"
```

but the initialization uses:

```cpp
Sciantix_variables[19]
Sciantix_variables[20]
```

which are also the legacy bulk bubble indices:

```text
19 Intragranular bubble concentration
20 Intragranular bubble radius
```

Meanwhile `UpdateVariables.C` maps:

```text
163 -> "Dislocation bubble concentration"
164 -> "Dislocation bubble radius"
```

This is inconsistent: initialization and update use different index logic.

Also, gas variables such as:

```text
"Xe in dislocation bubbles"
```

are initialized to zero in `SetVariablesFunctions.C`, but they do not appear to have a persistent external index in `UpdateVariables.C`. This can cause `m_d` to reset or fail to survive across externally-managed timesteps depending on SCIANTIX execution mode.

**Codex action:** fix variable plumbing before implementing physics. Use dedicated indices for all dislocation-gas and microstructure states. Do not reuse 19/20.

---

## 2. UO2 coarsening branch: what to reuse and what not to reuse

### 2.1 Reuse software architecture

From `development/CoarseningUO2`, reuse only the implementation pattern:

```text
extra second-population variables
toOutput flag for second population
state persistence through UpdateVariables.C
optional input setting for density model
post-processing after legacy bulk bubble update
pressure/equilibrium pressure diagnostics
coalescence update form
regression-case organization
```

The UO2 branch adds second-population variables with indices 170–179:

```text
170 Intragranular coarsened bubble concentration
171 Intragranular coarsened bubble radius
172 Intragranular coarsened atoms per bubble
173 Intragranular coarsened vacancies per bubble
174 Intragranular coarsened gas bubble swelling
175 Intragranular bulk gas bubble swelling
176 Intragranular gas in coarsened bubbles
177 Intragranular coarsened bubble pressure
178 Intragranular coarsened bubble equilibrium pressure
179 Dislocation density
```

This is a good model for UN variable plumbing.

### 2.2 Do not copy UO2 physics

Do not copy these into UN:

```text
Barani/Setyawan UO2 resolution constants
UO2 pipe diffusivity constants
UO2 vacancy pipe diffusivity constants
Carnahan-Starling UO2 hard-sphere EOS
UO2 surface energy = 0.7 J/m2
UO2 Schottky volume = 4.09e-29 m3 unless explicitly justified
Zullo/Zullo-Nicodemo UO2 dislocation density correlations
UO2 growth limiter values unless explicitly justified as numerical guards
UO2 bubbles_per_dislocation = 1e6 bubble/m
```

### 2.3 Do not merge branch directly

`development/CoarseningUO2` and `development/nitride` are diverged. The coarsening branch is ahead by 22 commits and behind by 225 relative to `development/nitride`. It also contains an unresolved merge-conflict marker in `IntraGranularBubbleBehavior.C`.

**Codex action:** inspect only. Do not merge.

---

## 3. Recommended implementation strategy

Implement in stages. Do not try to implement everything in one fragile patch.

### Patch 1 — mandatory: robust UN intragranular two-population model

Implement:

```text
gas in solution c
gas in bulk bubbles m_b
gas in dislocation bubbles m_d
bulk bubble number N_b
dislocation bubble number N_d
bulk/dislocation radii
bulk/dislocation swelling
bulk/dislocation pressure diagnostics
gas to grain boundary q_gb by mass balance
```

Do not implement grain-face bubbles/FGR yet unless Patch 1 is stable.

### Patch 2 — vacancy absorption and dislocation coalescence refinement

Add/refine:

```text
vacancy absorption for bulk and dislocation bubbles
pressure equilibrium
dynamic rho_d(T,F)
dislocation coalescence
single-size validity guards
```

### Patch 3 — intergranular grain-face bubbles and FGR

Implement:

```text
q_gb -> q_gf + q_rel
grain-face bubbles
coverage
coalescence
saturation
release
```

This uses `UNintergranular_theory.md`.

---

## 4. Input switches

Keep existing switches:

```text
iFuelMatrix = 2                  # UN
iFissionGasDiffusivity = 11      # Xe in UN
iDiffusionSolver = 4             # c, m_b, m_d exchange solver
iResolutionRate = 4              # UN b_b, b_d
iTrappingRate = 2                # UN g_b, g_d
```

Add optional UN switches using `ReadOptionalSetting(...)` style from UO2 coarsening branch, so old input files still work.

Recommended optional switches:

```text
iUNDislocationDensity = 0/1/2
    0 = constant matrix rho_d
    1 = rho_d(T,F) from project UN model, if provided
    2 = user/sensitivity option, future

iUNVacancyDiffusivity = 0/1
    0 = Rizk nominal/table form
    1 = thesis refit form

iUNBubbleCapture = 0/1
    0 = off by default
    1 = optional bulk-to-dislocation capture

iUNYieldMode = 0/1/2
    0 = Xe-only 0.24 default
    1 = Xe+Kr or noble-gas mode, if implemented
    2 = volatile-equivalent Rizk 0.475, explicitly labelled
```

For the first patch, it is acceptable to add only:

```text
iUNDislocationDensity
iUNVacancyDiffusivity
```

and keep capture/yield modes for later.

---

## 5. Variable and index plan

### 5.1 Do not reuse these for dislocation bubbles

Do not use:

```text
19 Intragranular bubble concentration
20 Intragranular bubble radius
```

for dislocation bubbles. These are legacy bulk bubble variables.

### 5.2 Use a dedicated block

Preferred approach: use a dedicated UN/dislocation block similar to UO2 coarsening.

Suggested mapping, subject to Codex checking for free indices:

```text
170 Dislocation bubble concentration                  [bub/m3]
171 Dislocation bubble radius                         [m]
172 Dislocation gas atoms per bubble                  [at/bub]
173 Dislocation vacancies per bubble                  [vac/bub]
174 Dislocation gas bubble swelling                   [-]
175 Intragranular bulk gas bubble swelling            [-]
176 Xe in dislocation bubbles                         [at/m3]
177 Dislocation bubble pressure                       [MPa]
178 Dislocation bubble equilibrium pressure           [MPa]
179 Dislocation density                               [m^-2]
180 Bulk vacancies per bubble                         [vac/bub]
181 Bulk bubble pressure                              [MPa]
182 Bulk bubble equilibrium pressure                  [MPa]
183 Bulk bubble volume                                [m3/bub]
184 Dislocation bubble volume                         [m3/bub]
185 UN gas to grain boundary diagnostic               [at/m3]
```

If 170–179 are already occupied locally, Codex should choose the next free block and document it.

### 5.3 Gas-state persistence

The 3-equation solver uses:

```text
c
m_b
m_d
```

Therefore `m_d` must be persistent.

At minimum for Xe:

```text
"Xe in dislocation bubbles" [at/m3]
```

must be initialized from a Sciantix_variables index and written back in `UpdateVariables.C`.

For Kr/He, either:
- fully plumb them too, or
- explicitly restrict the first patch to Xe and keep other species inactive for UN.

Do not leave `m_d` as an internal zero-initialized variable if repeated external timesteps can reset it.

---

## 6. UN equations to implement

The equations below are the source of truth for UN. They come from `UNmodel.md`, not the UO2 coarsening branch.

### 6.1 Gas diffusivity

Default:

```text
D_g = D1 + D3
D1 = D10 * exp(-Q1/(kB*T))
D3 = A30 * Fdot
```

Parameters:

```text
D10 = 1.56e-3 m2/s
Q1  = 4.94 eV
A30 = 1.85e-39 m5
kB  = 8.617333262e-5 eV/K
```

Keep `D2_Xe = 0` by default.

---

### 6.2 Production

```text
beta = Y * Fdot
```

Default:

```text
Y = 0.24 at/fission
```

Do not default to 0.475.

---

### 6.3 Resolution

For each population:

```text
b_i = Fdot * b0(R_i)
```

with:

```text
b0(R) = 1.0e-25 * (2.64 - 2.02 * exp(-2.61e-9/R))
```

Thus:

```text
b_b = b0(R_b) * Fdot
b_d = b0(R_d) * Fdot
```

Use `R_eff = R + radius_in_lattice` only if this is the existing SCIANTIX convention; document it.

---

### 6.4 Trapping

Bulk:

```text
g_b = 4*pi*D_g*R_b_eff*N_b
```

Dislocation:

```text
Gamma_d = 1/sqrt(pi*rho_d)

free_dislocation = max(rho_d - 2*R_d*N_d, 0)

den = log(Gamma_d/(Z_d*r_d)) - 3/5

g_d = 4*pi*D_g*R_d_eff*N_d
      + (2*pi*D_g/den)*free_dislocation
```

Constants:

```text
Z_d = 5
r_d = 3.46e-10 m
rho_d nominal = 3.0e13 m^-2
```

Add guards:

```text
rho_d > 0
Gamma_d > 0
den > 0
N_d >= 0
R_d >= 0
free_dislocation >= 0
```

---

### 6.5 3-equation gas system

Use the existing `SpectralDiffusion3equationsExchange`:

```text
dc/dt   = D_g ∇²c - (g_b + g_d)c + b_b*m_b + b_d*m_d + beta
dm_b/dt = g_b*c - b_b*m_b
dm_d/dt = g_d*c - b_d*m_d
```

Codex must verify mass balance for:

```text
gas produced ≈ gas in solution + gas in bulk bubbles + gas in dislocation bubbles + gas at grain boundary + released
```

---

### 6.6 Bulk bubble nucleation

```text
nu_b = 8*pi*f_n*D_g*Omega_fg^(1/3)*c^2
```

Nominal:

```text
f_n = 1.0e-6
Omega_fg = 8.5e-29 m3/atom
```

Bulk bubble concentration:

```text
dN_b/dt = nu_b - b_b*phi_b*N_b
```

where:

```text
m_b' = m_b/N_b
phi_b = 1/(m_b' - 1)
```

Add guards:

```text
if N_b <= tiny: handle safely
if m_b' <= 1: phi_b = 0 or guarded value
N_b_new >= 0
```

---

### 6.7 Dislocation bubble concentration

Initial:

```text
N_d0 = K_d * rho_d
```

Nominal:

```text
K_d = 5.0e5 bubble/m
rho_d = 3.0e13 m^-2
N_d0 = 1.5e19 bub/m3
```

Evolution:

```text
dN_d/dt =
(N_d/rho_d)*d(rho_d)/dt
-
4*lambda*N_d^2*dV_d/dt
```

with:

```text
xi = V_d*N_d
lambda = (2 - xi)/(2*(1 - xi)^3)
```

Time-splitting requested for the single-size implementation:

```text
1. grow existing bubbles and update V_d
2. apply coalescence to existing grown bubbles
3. update rho_d(T,F) and add/remove bubbles through N_d/rho_d*d(rho_d)/dt
4. recompute average V_d and R_d
```

For constant rho_d, step 3 simply keeps `rho_d` constant.

Robust coalescence update:

```text
if dV_d > 0:
    N_d = N_d / (1 + 4*lambda*N_d*dV_d)
```

Guard:

```text
xi < 1
lambda finite
N_d not allowed to collapse to nonphysical tiny values without diagnostic
```

---

### 6.8 Bubble volume and swelling

Use concentration-based gas and vacancy inventories.

For population `i = b,d`:

```text
m_i' = m_i/N_i
n_vi' = n_vi/N_i
V_i = Omega_fg*m_i' + Omega*n_vi'
R_i = (3*V_i/(4*pi))^(1/3)
swelling_i = N_i*V_i
```

where:

```text
Omega = a^3/4
a = 4.889e-10 m
Omega ≈ 2.92e-29 m3
Omega_fg = 8.5e-29 m3/atom
```

Avoid using UO2 Schottky volume for UN swelling unless explicitly justified.

---

### 6.9 Pressure and equilibrium pressure

For population `i = b,d`:

```text
p_i = kB_J*T*m_i'/(n_vi'*Omega)
```

or equivalently with per-bubble vacancy count.

Equilibrium pressure:

```text
p_eq_i = 2*gamma/R_i - sigma_h
```

Default:

```text
gamma = 1.11 J/m2
sigma_h = history_variable["Hydrostatic stress"] in Pa
```

SCIANTIX stores hydrostatic stress in MPa. Convert consistently:

```text
sigma_h_Pa = hydrostatic_stress_MPa * 1.0e6
```

If vacancy count is zero at nucleation, seed or guard it to avoid infinite pressure. Do not hide this: add a diagnostic or TODO.

---

### 6.10 Vacancy diffusivity

Use U-vacancy diffusivity for UN.

Implement coefficients centrally with a switch:

```text
iUNVacancyDiffusivity = 0: nominal/literature Rizk form
iUNVacancyDiffusivity = 1: thesis refit form
```

Known current thesis refit form:

```text
D_v = D_v1 + D_v2

D_v1 = 1.35e-2 * exp(-5.66/(kB*T))

D_v2 = sqrt(Fdot) * A20_refit
       * exp(B21_refit/(kB*T) + B22_refit/(kB*T)^2)
```

with:

```text
A20_refit = 4.6304523933553033e-29
B21_refit = -0.62
B22_refit = -0.04
```

Codex must check `UNmodel.md` for the final selected form before coding. Do not hard-code undocumented calibration values.

---

### 6.11 Vacancy absorption

For population `i = b,d`:

```text
dn_vi/dt =
(2*pi*D_v*delta_i*N_i)/(kB_J*T*zeta_i)
*
(p_i - p_eq_i)
```

Use the exact `delta_i` and `zeta_i` definitions from `UNmodel.md`.

Guards:

```text
if p_i <= p_eq_i: allow no absorption or allow emission only if explicitly modeled
n_vi >= 0
zeta_i > 0
delta_i > 0
```

---

### 6.12 Optional bulk-to-dislocation capture

Do not enable by default.

If implemented:

```text
iUNBubbleCapture = 0/1
```

Default:

```text
iUNBubbleCapture = 0
```

Document it as optional Barani-like/capture extension, not pure Rizk.

---

## 7. Intergranular model for later patch

The intergranular model is in `UNintergranular_theory.md`. It should be implemented after the intragranular model is stable.

### 7.1 State variables

```text
q_gf   gas in grain-face bubbles [at/m3 fuel]
q_rel  released gas [at/m3 fuel]
N_gf   grain-face bubble areal density [bub/m2]
N_gfV  equivalent volumetric density [bub/m3]
n_g    gas atoms per grain-face bubble [at/bub]
n_v    vacancies per grain-face bubble [vac/bub]
R_gf   grain-face bubble curvature radius [m]
A_gf   projected area [m2]
V_gf   lenticular bubble volume [m3]
F_c    coverage = N_gf*A_gf
```

### 7.2 Nominal parameters

```text
N_gf0 = 2.0e13 bub/m2
r_gr = 6.0e-6 m
D_v_gb = 1e6 * D_U1
delta_gb = 4.0e-10 m
F_c_sat = 0.5
theta = 59 deg
R_gf0 = 2.42e-10 m
gamma_b = 1.11 J/m2
gamma_GB = 1.1391 J/m2
Omega_fg = 8.5e-29 m3/atom
a = 4.889e-10 m
Omega = a^3/4
```

### 7.3 Lenticular geometry

Use:

```text
cos(theta) = gamma_GB/(2*gamma_b)

f_theta = 1 - 1.5*cos(theta) + 0.5*cos(theta)^3

V_gf = (4/3)*pi*R_gf^3*f_theta
```

Use `cos^3(theta)`, not `cos^2(theta)`, unless explicitly reproducing Rizk/Pastore printed form for sensitivity. Document this discrepancy.

Area:

```text
A_gf = pi*(R_gf*sin(theta))^2
```

Coverage:

```text
F_c = N_gf*A_gf
```

Equivalent volumetric density:

```text
N_gfV = (3/(2*r_gr))*N_gf
```

Swelling:

```text
swelling_gf = N_gfV*V_gf
```

### 7.4 Grain-face coalescence and release

Before saturation:

```text
dN_gf/dt =
-[6*N_gf^2/(3 + 4*N_gf*A_gf)] * dA_gf/dt
```

At saturation:

```text
F_c = F_c_sat
dN_gf/dt = -(N_gf/A_gf)*dA_gf/dt
```

Release:

```text
dq_rel/dt =
(3/(2*r_gr)) * n_g * (N_gf/A_gf) * dA_gf/dt
```

Gas balance:

```text
q_prod = c + m_b + m_d + q_gf + q_rel
q_gb = q_gf + q_rel
```

Again: implement this only after intragranular model works.

---

## 8. Required code files to modify for Patch 1

Codex should expect to modify these files:

```text
src/models/IntraGranularBubbleBehavior.C
src/operations/SetVariablesFunctions.C
src/operations/UpdateVariables.C
src/operations/SetVariables.C
src/file_manager/InputReading.C
src/operations/SetMatrix.C
src/classes/System.C
include/classes/System.h
include/classes/Matrix.h
include/classes/Solver.h
include/classes/Simulation.h
```

Potentially also:

```text
src/models/GasDiffusion.C
src/classes/Solver.C
```

but only if the existing 3x3 solver has bugs or missing persistence.

---

## 9. Patch 1 detailed implementation checklist

### 9.1 Variable plumbing

- [ ] Add persistent `Xe in dislocation bubbles`.
- [ ] Add persistent dislocation bubble concentration/radius/volume.
- [ ] Add persistent dislocation vacancies.
- [ ] Add persistent bulk vacancies if pressure/vacancy growth is implemented.
- [ ] Add bulk/dislocation swelling outputs.
- [ ] Add pressure and equilibrium pressure outputs.
- [ ] Add dislocation density output.
- [ ] Remove unsafe initialization from indices 19/20 for dislocation variables.

### 9.2 Solver plumbing

- [ ] Verify `iDiffusionSolver = 4` works for Xe in UN.
- [ ] Verify `m_d` is not reset each timestep.
- [ ] Verify dislocation modes are either persisted correctly or intentionally internal for full-run execution.
- [ ] Add mass-balance diagnostic.

### 9.3 Intragranular microstructure

- [ ] Implement `case 5` in `IntraGranularBubbleBehavior.C`.
- [ ] Compute/update `N_b`.
- [ ] Initialize/update `N_d`.
- [ ] Compute `m_b'`, `m_d'`.
- [ ] Compute/update vacancies.
- [ ] Compute `V_b`, `V_d`.
- [ ] Compute `R_b`, `R_d`.
- [ ] Compute pressure and equilibrium pressure.
- [ ] Apply dislocation coalescence.
- [ ] Compute swelling split and total swelling.

### 9.4 Constants

- [ ] Centralize UN constants.
- [ ] Remove or clearly label placeholders.
- [ ] Use `Omega = a^3/4`, not UO2 Schottky volume, for UN matrix vacancy volume if this is the chosen model.
- [ ] Use `Omega_fg = 8.5e-29 m3/atom`.

### 9.5 Tests

- [ ] Add one minimal UN regression case.
- [ ] Confirm no NaN/inf.
- [ ] Confirm finite radii and concentrations.
- [ ] Confirm gas mass balance.
- [ ] Confirm outputs include both bulk and dislocation bubble quantities.

---

## 10. Minimal input case for regression

Use a simple Xe-focused UN case.

Suggested target:

```text
T = 1600 K
Fdot = 5.0e19 fiss/m3/s
grain radius = 6.0e-6 m
target FIMA ≈ 1.1%
Y_Xe = 0.24
iFuelMatrix = 2
iFissionGasDiffusivity = 11
iDiffusionSolver = 4
iIntraGranularBubbleBehavior = 5
iResolutionRate = 4
iTrappingRate = 2
iNucleationRate = appropriate UN setting or unused if case 5 handles N_b internally
```

Expected qualitative behavior:

```text
c >= 0
m_b >= 0
m_d >= 0
N_b >= 0
N_d > 0
R_b > 0 when m_b > 0
R_d > 0 when m_d > 0
swelling_total finite
gas balance approximately closes
```

---

## 11. Numerical diagnostics to add

At least internally, add guards/diagnostics for:

```text
psi_b = R_b/delta_b
psi_d = R_d/delta_d
xi_b = V_b*N_b
xi_d = V_d*N_d
p_b/p_eq_b
p_d/p_eq_d
mass balance residual
N_d floor/invalid single-size
lambda finite
coverage only in future intergranular model
```

The Python notebook showed that the dislocation single-size model can become invalid at high temperature/high burnup when `psi_d > 0.8`, `xi_d` grows, or `N_d` collapses. Do not hide this. Report it.

---

## 12. Codex guardrails

Codex must not:

```text
merge development/CoarseningUO2 into development/nitride
copy UO2 constants into UN
copy unresolved conflict markers
reuse Sciantix_variables[19]/[20] for dislocation bubbles
silently change Xe yield from 0.24 to 0.475
default-enable bulk-to-dislocation capture
disable coalescence just to improve stability
implement grain-face release before intragranular mass balance works
break UO2 regression behavior
hide NaN/inf by clipping without reporting
```

Codex should:

```text
make a small targeted patch
commit nothing unless explicitly asked by the user
list all modified files
document all new variables and indices
document all new switches and defaults
add a minimal regression case
run build/tests
write implementation notes
```

---

## 13. Suggested exact Codex prompt

Copy/paste this to Codex:

```text
Read `CODEX_UN_SCIANTIX_FULL_IMPLEMENTATION_TASK.md` first.

You are on branch `development/nitride` of `sciantix/sciantix-official`.

Use `development/CoarseningUO2` only as a read-only software architecture reference. Do not merge it and do not copy UO2 physics constants.

Read these theory/context files:
- CONTEXT.md
- UNcode.md
- UNmodel.md
- UNintergranular_theory.md
- UO2_COARSENING_TO_UN_SCIANTIX_REPORT.md

First create `context/CODEX_PREPATCH_UN_AUDIT.md` summarizing:
1. current UN code already present;
2. whether the 3x3 solver exists;
3. current variable-index issues;
4. exact implementation plan.

Then implement Patch 1 only:
- add a UN-specific intragranular two-population case, preferably `iIntraGranularBubbleBehavior = 5`;
- keep existing UN `iFissionGasDiffusivity = 11`, `iDiffusionSolver = 4`, `iResolutionRate = 4`, `iTrappingRate = 2`;
- add clean persistent state variables for gas in dislocation bubbles and dislocation bubble microstructure;
- stop reusing indices 19/20 for dislocation variables;
- implement bulk bubble nucleation/evolution;
- implement dislocation bubble initialization/evolution;
- implement vacancy content, pressure/equilibrium pressure, radius, swelling;
- implement dislocation coalescence with update order:
  growth -> coalescence -> rho_d update/nucleation -> recompute averages;
- keep bulk-to-dislocation capture disabled unless a separate switch is added;
- do not implement grain-face/FGR yet unless Patch 1 is stable.

After patching, create `context/UN_SCIANTIX_IMPLEMENTATION_NOTES.md` with:
- files modified;
- variables and indices added;
- switches added;
- equations implemented;
- tests run;
- known TODOs.

Add one minimal UN regression case at 1600 K, Fdot = 5e19 fiss/m3/s, grain radius = 6e-6 m, Xe yield 0.24, and verify finite outputs and mass balance.
```

---

## 14. Final practical note

The current branch already has a useful part of the UN implementation: the 3x3 gas exchange solver. The missing core is the **microstructure update**: `N_b`, `N_d`, radii, volumes, vacancies, pressure, swelling, persistence of `m_d`, and clean variable indices. Codex should focus there first.
