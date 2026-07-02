# Equivalence between SCIANTIX `R/B` and the Turnbull / ANS-5.4 release-to-birth ratio

## 1. Context

GitHub issue [#61](https://github.com/sciantix/sciantix-official/issues/61) raised the question whether the release-to-birth ratio (R/B) computed in SCIANTIX for short-lived radioactive species (Xe-133, Kr-85m) - coded in [`GasRelease.C:51-67`](src/models/GasRelease.C#L51-L67) as

```
R/B = released / (produced − decayed)
```

- corresponds to the rate-ratio definition used in the CONTACT experiment literature and codified in the ANS-5.4 standard (Turnbull and Beyer, *Background and Derivation of ANS-5.4 Standard Fission Product Release Model*, NUREG/CR-7003, 2010), where R/B is defined as

$$\left(\frac{R}{B}\right)_{\text{Turnbull}} \;=\; \frac{\text{release rate}\,(\text{atoms/s})}{\text{production rate}\,(\text{atoms/s})} \;=\; \frac{\dot R}{yF}$$

at radioactive equilibrium.

The two definitions look different - one is a ratio of cumulative quantities, the other is a ratio of rates - and the equivalence is not obvious. This document proves analytically that the two are **identical at radioactive equilibrium in the intra-granular Booth limit**, and verifies the result numerically using the SCIANTIX regression case `test_CONTACT1`.

## 2. SCIANTIX accounting

For a radioactive species with decay constant `λ` and yield `y`, SCIANTIX integrates three cumulative scalar variables:

- `P(t) = produced(t) = ∫₀ᵗ yF(t') dt'` - total atoms ever created by fission. Updated in [`GasProduction.C:41-44`](src/models/GasProduction.C#L41-L44) by the simple integrator `P_{n+1} = P_n + yF · Δt`.
- `D(t) = decayed(t)` - solved by the ODE
$$\frac{dD}{dt} \;=\; \lambda\,(P - D)$$
in [`GasDecay.C:26-30`](src/models/GasDecay.C#L26-L30) via the closed-form `solver.Decay`. Note the source term `λ·P` (not `λ·N_grain`): SCIANTIX accounts decay against *all atoms ever produced minus those already decayed*, so `D` represents cumulative decay losses across **the entire system** (matrix + grain boundary + released), not just the grain matrix.
- `R(t) = released(t)` - when `iGrainBoundaryBehaviour = 0` it is closed by mass balance in [`GasDiffusion.C:179-182`](src/models/GasDiffusion.C#L179-L182):
$$R \;=\; P - D - N$$
where `N(t)` is the current intra-granular inventory ("Xe133 in grain"). When `iGrainBoundaryBehaviour = 1` (Pastore 2013), additional grain-boundary storage and venting dynamics intervene and `R` is no longer a simple closure - see §5 below.

Rearranging the closure: `P − D = R + N`, that is, `(P − D)` is the **total alive inventory anywhere in the system** (released plus retained-in-grain), while `D` is everything that has decayed wherever it was at the moment of decay.

## 3. Booth diffusion problem

For the intra-granular Booth limit (no grain-boundary storage, atoms reaching the grain surface are immediately released), the governing equation is Turnbull 2010 Eq. 1:

$$\frac{\partial C}{\partial t} \;=\; yF - \lambda C + \frac{D}{r}\,\frac{\partial^2 (rC)}{\partial r^2}$$

with boundary condition `C(a, t) = 0` (perfect sink at the grain surface, radius `a`). The volume-integrated balance over the grain reads

$$\frac{dN}{dt} \;=\; yF\,V_{\text{grain}} \;-\; \lambda N \;-\; \dot R$$

where `Ṙ` is the volumetric release rate (atoms per unit fuel volume per second). At radioactive equilibrium `dN/dt = 0`, hence

$$\boxed{\;\lambda N_{\text{eq}} \;=\; yF - \dot R_{\text{eq}}\;} \tag{1}$$

This is the standard secular-equilibrium relation: the production rate is exactly partitioned between in-grain decay and release. The Turnbull R/B is then

$$\left(\frac{R}{B}\right)_{\text{Turnbull}} \;=\; \frac{\dot R_{\text{eq}}}{yF} \;=\; 1 - \frac{\lambda N_{\text{eq}}}{yF} \tag{2}$$

The classical Booth result (Turnbull 2010 Eq. 2) gives the same quantity in closed form as a function of `μ = λa²/D`:

$$\left(\frac{R}{B}\right)_{\text{Turnbull}} \;=\; 3\left[\frac{1}{\sqrt\mu}\,\coth\!\sqrt\mu \;-\; \frac{1}{\mu}\right]$$

## 4. Equivalence proof

Let us evaluate the SCIANTIX cumulative ratio `R/(P−D)` at radioactive equilibrium under constant fission rate `F` and temperature `T`, for `iGrainBoundaryBehaviour = 0`.

**Step 1 - solve `D(t)` analytically.** Assume constant fission rate `F` and zero initial inventory, so that `P(t) = yF · t`. The decay ODE in [`GasDecay.C`](src/models/GasDecay.C#L26-L30) reads

$$\frac{dD}{dt} + \lambda\,D \;=\; \lambda\,yF\,t$$

and the exact solution is

$$D(t) \;=\; yF\,t \;-\; \frac{yF}{\lambda}\,\bigl(1 - e^{-\lambda t}\bigr).$$

Subtracting from `P(t)`:

$$P(t) - D(t) \;=\; \frac{yF}{\lambda}\,\bigl(1 - e^{-\lambda t}\bigr).$$

In the radioactive-equilibrium limit `λt ≫ 1` the exponential term vanishes, and we obtain the central identity used in the rest of the proof:

$$P(t) - D(t) \;\longrightarrow\; \frac{yF}{\lambda} \qquad (\lambda t \gg 1). \tag{3}$$

That is, at radioactive equilibrium `(P − D) → yF/λ`, a *constant*. (This is the same as the secular-equilibrium inventory `λN_eq = yF` would give *if release were absent*. With release, eq. (1) shows the difference is exactly `Ṙ_eq/λ`; see Step 3.)

**Step 2 - apply the SCIANTIX closure.** From [`GasDiffusion.C:179-182`](src/models/GasDiffusion.C#L179-L182),

$$R(t) \;=\; P(t) - D(t) - N(t)$$

Hence

$$\frac{R(t)}{P(t) - D(t)} \;=\; 1 \;-\; \frac{N(t)}{P(t) - D(t)} \tag{4}$$

**Step 3 - take the equilibrium limit.** At radioactive equilibrium `(P − D) → yF/λ` from eq. (3) and `N → N_eq` from the Booth solution. Using eq. (1):

$$\frac{N_{\text{eq}}}{(P-D)\big|_{\text{eq}}} \;=\; \frac{N_{\text{eq}}}{yF/\lambda} \;=\; \frac{\lambda N_{\text{eq}}}{yF} \;=\; \frac{yF - \dot R_{\text{eq}}}{yF} \;=\; 1 - \frac{\dot R_{\text{eq}}}{yF}$$

Substituting in eq. (4):

$$\frac{R}{P-D}\bigg|_{\text{eq}} \;=\; 1 \;-\; \left(1 - \frac{\dot R_{\text{eq}}}{yF}\right) \;=\; \frac{\dot R_{\text{eq}}}{yF} \;=\; \boxed{\left(\frac{R}{B}\right)_{\text{Turnbull}}} \tag{5}$$

The SCIANTIX cumulative ratio `released / (produced − decayed)` is identically equal to the Turnbull / ANS-5.4 rate-ratio `Ṙ/yF` at radioactive equilibrium in the intra-granular Booth limit.

The key - and previously unappreciated - point is that `released` in SCIANTIX is **not** an integrated release rate `∫Ṙ dt` (which would grow linearly with time and cause the ratio to diverge). It is an instantaneous mass-balance closure that *implicitly applies the global decay accounting* to the released fraction as well, so it represents the **alive inventory currently outside the grain** (which at equilibrium tends to a constant `Ṙ_eq/λ`). The denominator `(P − D)` likewise represents the alive total inventory `(N + R)`, which at equilibrium tends to `yF/λ`. The ratio of the two constants is exactly the Turnbull rate-ratio.

## 5. Behaviour with grain-boundary storage active (`iGrainBoundaryBehaviour = 1`)

With Pastore 2013 grain-boundary bubbles enabled, atoms accumulate on the grain boundary before being released, and `released` is updated along the path `grain → GB → released` rather than by direct mass-balance closure. The bilancio becomes

$$P - D \;=\; N_{\text{grain}} + N_{\text{GB}} + R$$

The closure `R = P − D − N_grain` no longer holds because `N_GB > 0`. At secular equilibrium `(P−D)` still tends to `yF/λ`, but `R(t)` now contains a transient of grain-boundary accumulation followed by venting bursts, and the ratio `R/(P−D)` includes those dynamics. It is still a meaningful release fraction, but it is **not** strictly the Turnbull / ANS-5.4 R/B which derives from the pure intra-granular Booth problem.

For direct comparison with CONTACT R/B data, the recommended approach is therefore one of:

1. Run SCIANTIX with `iGrainBoundaryBehaviour = 0` so that the closure used in this proof applies. The `Xe133 R/B` and `Kr85m R/B` output columns are then the Turnbull R/B as defined in CONTACT.
2. Run with `iGrainBoundaryBehaviour = 1` and confirm that the simulation has reached steady state in both decay and grain-boundary venting, after which `R/(P−D)` again converges to the experimental quantity (with a residual offset due to GB storage that depends on burnup and venting model).

## 6. Numerical verification

The proof has been verified numerically by running [`regression/contact/test_CONTACT1`](regression/contact/test_CONTACT1) with `iGrainBoundaryBehaviour` switched from 1 to 0. The Xe-133 and Kr-85m R/B output columns are constant in time during periods of constant temperature and fission rate, exactly as Turnbull predicts:

**Xe-133** at `T ≈ 1100 K`, plateau between 20 d and 144 d (`λt` from 2.6 to 19, more than 16 half-lives apart):

| t (d) | T (K) | F (fiss/m³/s) | R/(P−D) Xe-133 |
|---:|---:|---:|---:|
| 20.08 | 1099 | 1.78·10¹⁹ | 5.44·10⁻² |
| 144.20 | 1107 | 1.76·10¹⁹ | 5.66·10⁻² |

→ variation of 4% over 124 days. The naïve cumulative-ratio interpretation would predict a factor-of-7 increase over the same window; the actual SCIANTIX behaviour confirms the Turnbull equivalence.

**Kr-85m** at the same plateau (3000 hours ≈ 670 half-lives apart):

| t (h) | T (K) | F (fiss/m³/s) | R/(P−D) Kr-85m |
|---:|---:|---:|---:|
| 481.9 | 1099 | 1.78·10¹⁹ | 2.019·10⁻² |
| 3460.8 | 1107 | 1.76·10¹⁹ | 2.031·10⁻² |

→ variation of 0.6% over 670 half-lives. Identically constant, as expected for a true rate-ratio observable.

## 7. References

- Turnbull J.A., Beyer C.E., *Background and Derivation of ANS-5.4 Standard Fission Product Release Model*, NUREG/CR-7003, PNNL-18490, U.S. NRC, January 2010 (PDF in repository root).
- ANS-5.4 standard, *Method for Calculating the Fractional Release of Volatile Fission Products from Oxide Fuel*.
- Booth A.H., *A Method of Calculating Fission Gas Diffusion from UO₂ Fuel and its Application to the X-2-f Loop Test*, AECL-496, 1957.
- SCIANTIX source code:
  - [`src/models/GasProduction.C`](src/models/GasProduction.C) - production integrator.
  - [`src/models/GasDecay.C`](src/models/GasDecay.C) - decay ODE `dD/dt = λ(P−D)`.
  - [`src/models/GasDiffusion.C:148-186`](src/models/GasDiffusion.C#L148-L186) - mass-balance closure for `released` when `iGrainBoundaryBehaviour = 0`.
  - [`src/models/GasRelease.C:51-67`](src/models/GasRelease.C#L51-L67) - R/B output.
