# Selected intragranular coarsening model - Nicodemo 2026

The additional model is `iIntraGranularBubbleBehavior=4`. Its selected defaults
are K=1.6e6 m^-1, line-capture D=3 Dbulk, vacancy Dv=30 Dvbulk, and a 1500 K
vacancy-uptake threshold. The standard input structure retains multiplicative
scaling factors for K and the two mobilities. There are no Hart, pipe,
capture-activation or alternative EOS selectors. Original SCIANTIX options and
their gold outputs remain unchanged. Barani 2020 supplies published comparison
values, not an executable historical reconstruction. Calibration is not
independent validation. See [the file map](IMPLEMENTATION.md) for the diff from
official SCIANTIX main at `2344575`.

New routines are marked `#COARSENING`. The solver, vacancy root, nucleation,
coalescence and state adapter are in `IntraGranularBubbleBehavior.C`;
correlations and geometry are in `IntraGranularBubbleCoarsening.C/h`;
`DislocationDensity.C/h` supplies rho and its signed derivatives.

## Three atomic inventories

For each radial spectral mode, solution C, bulk-bubble gas Mb and
dislocation-bubble gas Md are solved simultaneously by implicit elimination:

```
dC/dt  = source - diffusion_escape - (beta_b+beta_d+beta_line) C
         + alpha_b Mb + alpha_d Md
dMb/dt = beta_b C - alpha_b Mb
dMd/dt = (beta_d+beta_line) C - alpha_d Md
```

There is no phi in these atomic re-solution terms. Yield remains 0.27.
Bulk bubbles retain the native Turnbull law and native number source.
No artificial gas source or explicit two-atom nucleation transfer is added.
The number source represents initially empty trapping nuclei; treating its
number as a population of fully occupied bubbles at arbitrarily small mean
occupancy is a limitation of this mean-population closure.

## Dislocation density and number evolution

Density option 0 is constant 4e13 m^-2; option 1 is the frozen White refit.
B is GWd/tHM, T is K:

```
s(T) = logistic((T-1050.3656167)/189.8145825)
n(T) = 0.5271037725 + (1.0488163957-0.5271037725) s(T)
rho_V(B,T) = 10^14.63699581559 * (B/20)^n(T) * [1-(1-0.06994900781)s(T)]
rho_inf100 = 1e13 * 5^1.0488163957
u_T = sqrt((T-900)^2+20^2)-20
R100(T) = rho_inf100 + [rho_V(100,900)-rho_inf100]
          * exp[-(u_T/92.96144907268)^0.55]
rho(B,T) = rho_V(B,T)                     for T<900 K
           (B/100)^n(T) * R100(T)         for T>=900 K
```

The burnup exponent is retained from the public Veshchunov foundation.
The thermal dependence was refitted to White 2006 data that cannot be shown.
Only frozen coefficients are distributed; their complete precision is in
`dislocation_density/selected_parameters.json`. The 900 K anchor, stretched
thermal decay and 20 K smoothing are part of the retained fit, shared by C++
and Python. Full signed analytic derivatives are provided, with the selected
right branch at the anchor. At fresh B=0 the density is zero; a sublinear burnup exponent has
an integrable singular right derivative there. Birth integration uses finite
density increments, not that singular point derivative.

K is globally fixed at **1.6e+06 m^-1**. During irradiation at nondecreasing T,
`nu_d = K*max(d(rho)/dt,0)`. The derivative includes burnup and temperature.
The positive algebraic rebound during cooling is excluded from new nucleation,
as is generation at zero fission rate. There is no automatic Nd=K*rho seed.
Fresh full histories start with rho=Nd=0. A finite-burnup start needs a
specified physical population or a restart; the source does not determine
the integration constant. Constant density with Nd initially zero creates
no dislocation bubbles.

Number loss includes coalescence and effective homogeneous disappearance
`-alpha_d*phi_d*Nd`, where `phi_d=1/max(1,n_d-1)` is used only for mean
occupancy n_d>=2. Phi is not reintroduced in the atomic equations. Eliminated
cavities return their proportional vacancy inventory to the matrix. This is
an explicit additional number/volume closure, not a unique consequence of
the atomic re-solution equation. Pure merging conserves gas and total Qv.
Nd is never reset to K*rho after number loss.

## Gas capture and vacancy kinetics

The selected line-supply closure is D_capture=L*D_bulk with **L=3**.
Only beta_line=2*pi*rho*D_capture/[ln(1/sqrt(pi*rho)/(5b))-0.6] uses this effective coefficient; b=0.385nm.
Radial diffusion and direct bubble trapping retain D_bulk. There is no chi(T).
L is an explicitly calibrated transport factor, not a volume-weighted pipe fraction or an independently derived homogenization.
K, L and the vacancy multiplier are global across all38 cases. They were selected from endpoint log errors with pre-ramp diagnostics; this is calibration, not independent validation or proof of a global optimum.

Vacancy diffusivity uses the bulk law with an explicit effective prefactor:

```
Dv_bulk = 7.12e-7 exp[-4.72 eV/(kB*T)]
Dv = 30 * Dv_bulk
```

The factor30 preserves the preceding calibrated kinetics with less than0.5%
change in the38 final swelling predictions. It is a kinetic calibration,
not an independently measured material prefactor. No pipe mixing exponent
is retained. Uptake is
enabled only for **T>=1500 K** and positive CS overpressure. No temperature
threshold acts on gas capture or gas re-solution. The gate crossing is split
at its exact time within an interpolated history interval. There is no
vacancy-emission law and no per-call radius-growth cap.

The vacancy rate is `2*pi*Dv*delta*(P-Peq)/(kB*T*zeta)` for positive overpressure,
with x=R/delta and the corrected geometry
`zeta=(-x^6+5*x^2-9*x+5)/(10*x*(1+x^3))`. This is the reciprocal of the expression
printed in Barani2020 Eq.9. It recovers the isolated-sphere flux
`4*pi*Dv*R*(P-Peq)/(kB*T)` as x tends to zero; the kernel test checks that
independent diffusion limit. The numerator is factored near x=1 for stability.
K and both mobility multipliers are unchanged to isolate this correction.

## Consistent CS pressure and reference volume

```
dHS = 4.45e-10*(0.8542-0.03996*ln(T/231.2))
u(T) = 2*pi*dHS^3/3 = 4*vHS
V = n*u(T)+nv*Omega, Omega=4.09e-29 m3
ytilde = pi*dHS^3*n/(6*V) = n*u/(4*V)
P = n*kB*T/V * (1+ytilde+ytilde^2-ytilde^3)/(1-ytilde)^3
Peq = 1.4/R - sigma_h
```

For nonnegative gas and vacancies, **0<=ytilde<=1/4**. It needs no cap.
The physical inventory and packing-domain checks are retained; clipping
would hide a broken state. `Coarsening packing fraction` is exported and
checked over all histories. This bound follows from the additive reference
co-volume, not from Carnahan-Starling by itself. The co-volume is the second
virial coefficient; it is not a total gas-specific volume from EOS inversion.

The same volume is used in pressure, radius, swelling, vacancy uptake, cell
bounds and coalescence. Thermal co-volume changes enter geometric growth
without creating atoms or vacancies. Bulk bubbles retain Omega per atom.
The effective Xe diameter is applied to combined stable Xe/Kr; this is not
a mixture-specific EOS. Only this pressure/volume closure is executable.

## Scaling, numerics and validation

Optional input setting 29 is iDislocationDensity. Existing density options
and the three trailing scaling factors remain: index9 K, index10 complete
line-capture diffusivity, index11 complete vacancy diffusivity. Native nine-
factor files remain valid. The factors multiply the selected defaults,
not unit bulk mobilities.

Mode blocks18-19 store Xe/Kr in dislocation bubbles. Slot187 stores cumulative
nucleated nuclei, slot188 packing. Slot184 is reserved and written as zero;
the obsolete capture-activation state and output were removed. Source and escape ledgers, gas, Qv,
Nd and modal inventories persist across native restarts. Checkpoints from
the former site-persistence model must be regenerated from initial history.

The scope remains stable Xe/Kr in UO2, diffusion solver2, native kinetic
options1, and no He/HBS/sweeping/nonuniform source. Unsupported combinations
are rejected. The positive three-inventory solver and vacancy root use
adaptive local error control (2e-5 nominal). Outer histories use 200 steps
per interval. Kernel and outer-step refinement, conservation, ytilde bounds,
scaling controls and separate-process restart are checked. Original legacy
gold comparisons are kept independently of new-model endpoint references.

The 38-case regression compares published White observations and Barani2020
Table4 predictions. Calibration is not independent validation. A comparison
with pre-ramp information was also performed; those observations are not
distributed. The present ungated-capture model does not reproduce the former
pre-ramp gas partition. The regression records that discrepancy instead of
declaring pre-ramp agreement a numerical invariant.

```
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j6
ctest --test-dir build --output-on-failure
python3 context/dislocation_density/fit.py --cpp-grid build/coarsening_tests.x
python3 regression/White_Coarsening/run.py --binary build/sciantix.x --check --verify
```
