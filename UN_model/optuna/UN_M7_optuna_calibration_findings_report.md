# UN M7 — Optuna Calibration Findings Report

**Working title:** *Optuna Calibration Findings Report for the Reduced UN M7 Fission Gas Swelling Model*  
**Short name:** `UN_M7_optuna_calibration_findings_report`  
**Date:** 2026-05-08  
**Scope:** this note summarizes what was learned from the Optuna calibration campaigns. It is **not** a general literature note on \(P_1/P_2\), Ray--Blank, or Rizk. Those topics are treated in separate notes.

---

## 1. Purpose of this report

The purpose of this report is to preserve, in a single document, the practical lessons learned from the Optuna calibration campaigns performed on the reduced UN/M7 fission gas swelling model.

The model is a reduced, local intragranular implementation inspired by Rizk/SIFGRS/BISON. It tracks:

$$
c
$$

gas in solution,

$$
m_b
$$

gas in bulk bubbles, and

$$
m_d
$$

gas in dislocation bubbles.

The main calibration target is the experimentally observed large intragranular bubble population, represented in the model as dislocation bubbles:

$$
P_2 \rightarrow \text{dislocation bubbles}.
$$

The key quantities optimized or monitored were:

$$
\mu_d(T),\quad N_d(T),\quad R_d(T),
$$

together with:

$$
q_{gb},\quad \text{gas partition},\quad p_d/p_{d,eq},\quad p_b/p_{b,eq}.
$$

The main conclusion is that the current reduced model can produce good fits to several experimental and diagnostic targets, but the calibration also exposed structural limitations. In particular, even after allowing many scaling factors to vary, the model does not fully reproduce a clean high-temperature saturation/equilibration of the dislocation bubble population at \(2000\,\mathrm{K}\).

---

## 2. Nomenclature used in this report

To avoid ambiguity, the term **capture** is avoided for the interaction between bulk bubbles and dislocation bubbles.

In the code this parameter was previously called:

```text
capture_scale
```

In this report it is renamed:

$$
s_{bd}
$$

or:

```text
bulk-dislocation coalescence factor
```

because it controls the geometrical incorporation of bulk bubbles into growing dislocation bubbles. It is **not** the same as the gas-atom trapping rates \(g_b\) and \(g_d\), which describe capture/trapping of mobile gas atoms from the matrix.

Therefore:

| Old name in code | Name used in this report | Meaning |
|---|---|---|
| `capture_scale` | bulk-dislocation coalescence factor, \(s_{bd}\) | geometric bulk-bubble-to-dislocation-bubble interaction |
| `gb_scale` | bulk trapping scale | gas atom trapping from matrix to bulk bubbles |
| `gd_bubble_scale`, `gd_line_scale` | dislocation trapping scales | gas atom trapping from matrix to dislocation bubbles and dislocation line sink |
| `coalescence_d_scale` | dislocation-dislocation coalescence scale | coalescence among dislocation bubbles |

---

## 3. Final calibrated family considered here: v14

The latest useful calibration family discussed here is:

```text
v14 rhoSat qgbStrict NdAnchors
```

The best analyzed candidate was:

```text
trial 313
final score ≈ 1.751
```

This result was obtained only after letting Optuna continue to about 500 total trials. Earlier results at 100--200 trials were not converged. The progression was approximately:

| Total trials | Best score | Interpretation |
|---:|---:|---|
| 100 | \(2.35\) | useful but not converged |
| 200 | \(1.92\) | much better basin found |
| 249 | \(1.80\) | still improving |
| 313 | \(1.74\) fast / \(1.751\) final | best candidate found |
| 500 | best remained near trial 313 | useful convergence indication |

The important methodological lesson is:

$$
\boxed{
\text{For this model, 100--200 trials are not enough to judge a scoring formulation.}
}
$$

Changing the scoring too early can be misleading because the optimizer may not yet have found the relevant basin.

---

## 4. Scaling factors and where they enter the equations

### 4.1 Gas diffusivity scaling

The Xe diffusivity was split into separate physical contributions:

$$
D_g^{eff}
=
s_{g,1}D_{g,1}
+
s_{g,2}D_{g,2}
+
s_{g,3}D_{g,3}.
$$

In the base Rizk-like UN model:

$$
D_{g,1}=D_{10}\exp\left(-\frac{Q_1}{k_BT}\right),
$$

$$
D_{g,3}=A_{30}\dot F.
$$

The term \(D_{g,2}\) for Xe is retained only as a diagnostic placeholder. Rizk states that irradiation-enhanced Xe diffusion is negligible compared with \(D_1+D_3\), so in the current working model:

$$
D_{g,2}^{Xe}\simeq 0.
$$

The corresponding scaling factors are:

| Code name | Symbol | Meaning |
|---|---|---|
| `Dg_D1_scale` | \(s_{g,1}\) | thermal Xe diffusion scale |
| `D2_xe_scale` | \(s_{g,2}\) | irradiation-enhanced Xe diffusion diagnostic scale |
| `Dg_D3_scale` | \(s_{g,3}\) | athermal irradiation-induced Xe mixing scale |

In v14, `D2_xe_scale` remained fixed at 1, because the unscaled term is effectively negligible.

---

### 4.2 Vacancy diffusivity scaling

The uranium vacancy diffusivity was written as:

$$
D_v^{eff}
=
s_{v,1}D_{v,1}
+
s_{v,2}D_{v,2}.
$$

where:

$$
D_{v,1}
=
D_{10}^{vU}
\exp\left(-\frac{Q_1^{vU}}{k_BT}\right),
$$

and the fitted irradiation-enhanced contribution was used in the form:

$$
D_{v,2}
=
\sqrt{\dot F}\,
A_{20}^{refit}
\exp\left(
\frac{B_{21}}{k_BT}
+
\frac{B_{22}}{(k_BT)^2}
\right).
$$

The scaling factors are:

| Code name | Symbol | Meaning |
|---|---|---|
| `Dv_D1_scale` | \(s_{v,1}\) | thermal uranium vacancy diffusivity scale |
| `Dv_D2_scale` | \(s_{v,2}\) | irradiation-enhanced vacancy diffusivity scale |

A crucial point is that the coefficient used to fit the Rizk vacancy diffusivity plot was:

$$
A_{20}^{refit}
=
4.6304523933553033\times10^{-29}.
$$

The tabulated Rizk value for \(V_U\) was instead:

$$
A_{20}^{Rizk}
=
1.32\times10^{-19}.
$$

Therefore:

$$
\frac{A_{20}^{refit}}{A_{20}^{Rizk}}
=
\frac{4.6304523933553033\times10^{-29}}{1.32\times10^{-19}}
\simeq
3.51\times10^{-10}.
$$

Equivalently, the refitted coefficient is lower by:

$$
\frac{A_{20}^{Rizk}}{A_{20}^{refit}}
\simeq
2.85\times10^{9}.
$$

So the coefficient used to reproduce the plotted Rizk vacancy diffusivity is about:

$$
\boxed{
9.5\ \text{orders of magnitude lower}
}
$$

than the tabulated coefficient.

This is one of the most important calibration findings: even after adopting the lower fitted vacancy diffusivity and allowing scaling factors to vary, the model still does not naturally relax the dislocation bubble pressure to equilibrium at high temperature.

---

### 4.3 Re-solution scaling

For each bubble population:

$$
b_i
=
\dot F\,b_0(R_i),
\qquad
i=b,d,
$$

with:

$$
b_0(R)
=
10^{-25}
\left[
2.64
-
2.02\exp\left(-\frac{2.61\times10^{-9}}{R}\right)
\right].
$$

In the calibrated model the bulk and dislocation re-solution rates were scaled separately:

$$
b_b^{eff}
=
s_{b,b}\dot F\,b_0(R_b),
$$

$$
b_d^{eff}
=
s_{b,d}\dot F\,b_0(R_d).
$$

| Code name | Symbol | Meaning |
|---|---|---|
| `b_bulk_scale` | \(s_{b,b}\) | bulk bubble re-solution scale |
| `b_dislocation_scale` | \(s_{b,d}\) | dislocation bubble re-solution scale |

A repeated trend in good candidates is that both re-solution scales tend to be below 1, especially for bulk bubbles.

---

### 4.4 Bulk gas trapping scale

The trapping rate of gas atoms into bulk bubbles is:

$$
g_b
=
4\pi D_g R_b N_b.
$$

With scaling:

$$
g_b^{eff}
=
s_{gb}\,4\pi D_g R_b N_b.
$$

| Code name | Symbol | Meaning |
|---|---|---|
| `gb_scale` | \(s_{gb}\) | gas trapping into bulk bubbles |

In v14, good candidates often used a large \(s_{gb}\), meaning that the optimizer compensated part of the gas balance by increasing matrix-to-bulk trapping.

---

### 4.5 Dislocation gas trapping scales

The dislocation trapping term is split into a bubble-surface sink and a line sink.

The bubble contribution is:

$$
g_{d,bub}
=
4\pi D_g R_d N_d.
$$

The line-sink contribution is:

$$
g_{d,line}(\alpha)
=
\frac{2\pi D_g}{den}
\left(
\rho_d-\alpha\,2R_dN_d
\right)_+,
$$

where:

$$
den
=
\ln\left(\frac{\Gamma_d}{Z_dr_d}\right)-\frac{3}{5},
$$

and:

$$
\Gamma_d
=
\frac{1}{\sqrt{\pi\rho_d}}.
$$

Thus:

$$
g_d^{eff}
=
s_{d,bub}g_{d,bub}
+
s_{d,line}g_{d,line}(\alpha).
$$

The parameter \(\alpha\) interpolates between two limiting assumptions:

$$
\alpha = 1:
\quad
\text{covered dislocation length is removed as } \rho_d-2R_dN_d,
$$

$$
\alpha = 0:
\quad
\text{full line sink remains active, Barani-like limit}.
$$

| Code name | Symbol | Meaning |
|---|---|---|
| `gd_bubble_scale` | \(s_{d,bub}\) | gas trapping into existing dislocation bubbles |
| `gd_line_scale` | \(s_{d,line}\) | gas trapping by bare dislocation line sink |
| `gd_line_alpha` | \(\alpha\) | dislocation coverage interpolation |

A strong lesson from v14 is that the optimizer tends to increase dislocation trapping, especially through the existing dislocation-bubble sink.

---

### 4.6 Dislocation density scaling

The final v13/v14 formulation did not use two independent low/high temperature scaling factors. It used a single global scale factor multiplying a saturating temperature shape:

$$
\rho_d(F,T)
=
s_\rho\,
\rho_{d,0}(F)\,
f_T^{sat}(T).
$$

The burnup-dependent base function is:

$$
\rho_{d,0}(F)
=
\max\left[
\rho_{\mathrm{fab}},
1.6\times10^{14}(F-2.4)
\right],
$$

with \(F\) expressed in \(\%\) FIMA / a/o.

The temperature shape is:

$$
f_T^{sat}(T)
=
\frac{\rho_{\mathrm{shape}}(T)}
{\rho_{\mathrm{shape}}(1025)},
$$

with:

$$
\rho_{\mathrm{shape}}(T)
=
\rho_\infty
-
(\rho_\infty-\rho_{940})
\exp\left[-\frac{T-940}{\tau}\right].
$$

The fitted constants were:

$$
\rho_{940}=6.3571,
\qquad
\rho_\infty=9.1036,
\qquad
\tau=203.76\ \mathrm{K}.
$$

The v14 best candidate used:

$$
s_\rho \simeq 1.635.
$$

This means that the model preferred a relatively high effective dislocation density, but with a bounded high-temperature shape rather than a linearly increasing high-temperature extrapolation.

---

### 4.7 Dislocation-dislocation coalescence scale

The coalescence of dislocation bubbles is represented as:

$$
\left(
\frac{\partial N_d}{\partial t}
\right)_{coal}
=
-4\lambda N_d^2
\frac{\partial V_d}{\partial t},
$$

with:

$$
\lambda
=
\frac{2-\xi}{2(1-\xi)^3},
$$

and:

$$
\xi=V_dN_d.
$$

With a scale factor:

$$
\left(
\frac{\partial N_d}{\partial t}
\right)_{coal}^{eff}
=
-s_{coal,d}
4\lambda N_d^2
\frac{\partial V_d}{\partial t}.
$$

In the integrated numerical form:

$$
N_d^{new}
=
\frac{N_d^{old}}
{1+4s_{coal,d}\lambda N_d^{old}\max(V_d^{new}-V_d^{old},0)}.
$$

| Code name | Symbol | Meaning |
|---|---|---|
| `coalescence_d_scale` | \(s_{coal,d}\) | dislocation-dislocation coalescence strength |

The v14 best candidate used:

$$
s_{coal,d}\simeq 1.81.
$$

So the final calibrated solution kept coalescence active and even moderately stronger than the nominal formulation.

---

### 4.8 Bulk-dislocation coalescence factor

This term was previously called `capture_scale`. In this report it is renamed:

$$
s_{bd}
=
\text{bulk-dislocation coalescence factor}.
$$

The differential capture/coalescence volume is:

$$
\Delta V_{bd}
=
4\pi
(R_d^{old}+R_b^{old})^2
\max(R_d^{new}-R_d^{old},0).
$$

The transferred fraction is:

$$
f_{bd}
=
\min\left[
1,
\,
s_{bd}N_d\Delta V_{bd}
\right].
$$

Then, schematically:

$$
m_b \rightarrow (1-f_{bd})m_b,
$$

$$
m_d \rightarrow m_d + f_{bd}m_b,
$$

with analogous transfer for bubble vacancies and bulk bubble number.

| Code name | Symbol | Meaning |
|---|---|---|
| `capture_scale` | \(s_{bd}\) | bulk-dislocation coalescence factor |

The v14 best candidate used:

$$
s_{bd}\simeq 0.065.
$$

This is very low. Therefore, the calibrated model is telling us that a strong bulk-dislocation geometrical coalescence term is not needed to obtain the best v14 score. The dominant high-temperature behavior comes instead from dislocation density, dislocation trapping, vacancy/gas transport, and dislocation-dislocation coalescence.

---

## 5. v14 best candidate: numerical summary

The v14 best candidate was approximately:

| Quantity | Value |
|---|---:|
| trial | 313 |
| final score | \(1.751\) |
| \(f_n\) | \(1.32\times10^{-6}\) |
| \(K_d\) | \(3.10\times10^5\ \mathrm{bubble/m}\) |
| \(\dot F\) | \(7.72\times10^{19}\ \mathrm{fiss\,m^{-3}\,s^{-1}}\) |
| \(s_\rho\) | 1.635 |
| `rho_shape` | saturating Ray--Blank form |

The scaling factors were:

| Scaling factor | v14 best value |
|---|---:|
| `D2_xe_scale` | 1.000 |
| `Dg_D1_scale` | 0.5995 |
| `Dg_D3_scale` | 1.2414 |
| `Dv_D1_scale` | 0.2026 |
| `Dv_D2_scale` | 0.5742 |
| `b_bulk_scale` | 0.0625 |
| `b_dislocation_scale` | 0.209 |
| `gb_scale` | 5.558 |
| `gd_bubble_scale` | 9.648 |
| `gd_line_scale` | 3.397 |
| `gd_line_alpha` | 0.102 |
| `coalescence_d_scale` | 1.810 |
| `bulk-dislocation coalescence factor` \(s_{bd}\) | 0.0652 |

The top v14 candidates occupied a fairly narrow basin:

| Parameter | Approximate top-candidate basin |
|---|---:|
| \(s_\rho\) | \(1.57{-}1.64\) |
| \(K_d\) | \(\sim 3.0{-}3.4\times10^5\ \mathrm{bubble/m}\) |
| \(s_{coal,d}\) | \(\sim 1.7{-}1.8\) |
| \(s_{bd}\) | \(\sim 0.065{-}0.10\) |
| \(b_{bulk}\) scale | \(\ll 1\) |
| \(b_{dislocation}\) scale | \(<1\) |
| \(g_{d,bub}\) scale | high, order \(10\) |
| \(g_{d,line}\) scale | high, order \(3{-}4\) |

This is a physically interpretable basin:

$$
\rho_d \uparrow,\qquad
g_d \uparrow,\qquad
b \downarrow,\qquad
s_{coal,d}\gtrsim1,\qquad
s_{bd}\ll1.
$$

In words: the optimizer wants a high density of effective dislocation sites, strong trapping toward the dislocation population, reduced re-solution, active dislocation-dislocation coalescence, and almost no bulk-dislocation coalescence.

---

## 6. v14 best candidate: \(N_d(T)\), \(R_d(T)\), and gas partition

For trial 313:

### 6.1 Dislocation bubble concentration

$$
N_d(1400\,\mathrm{K})
\simeq
1.76\times10^{19}\ \mathrm{m^{-3}},
$$

$$
N_d(1550\,\mathrm{K})
\simeq
1.71\times10^{19}\ \mathrm{m^{-3}},
$$

$$
N_d(1650\,\mathrm{K})
\simeq
1.51\times10^{19}\ \mathrm{m^{-3}},
$$

$$
N_d(1800\,\mathrm{K})
\simeq
8.29\times10^{18}\ \mathrm{m^{-3}},
$$

$$
N_d(1900\,\mathrm{K})
\simeq
3.79\times10^{18}\ \mathrm{m^{-3}},
$$

$$
N_d(2000\,\mathrm{K})
\simeq
1.04\times10^{18}\ \mathrm{m^{-3}}.
$$

The high-temperature decrease is now present:

$$
\log_{10}\left[
\frac{N_d(2000)}{N_d(1400)}
\right]
\simeq
-1.23.
$$

This is a major improvement over earlier calibrations, where \(N_d\) remained almost flat.

However, the model still does not reproduce a fully satisfactory high-temperature saturation/plateau. \(N_d\) continues to be actively driven by coalescence over the high-temperature range, and the exact high-temperature shape remains sensitive to the scoring and the coalescence/trapping balance.

### 6.2 Dislocation bubble radius

For the same candidate:

$$
R_d(1600\,\mathrm{K})
\simeq
64.6\ \mathrm{nm},
$$

$$
R_d(1800\,\mathrm{K})
\simeq
142\ \mathrm{nm},
$$

$$
R_d(1900\,\mathrm{K})
\simeq
222\ \mathrm{nm},
$$

$$
R_d(2000\,\mathrm{K})
\simeq
395\ \mathrm{nm}.
$$

The main limitation is that:

$$
\boxed{
R_d(T)\ \text{is still increasing significantly at }2000\,\mathrm{K}.
}
$$

The model does not clearly reach a high-temperature radius saturation at \(2000\,\mathrm{K}\). Alternative high-radius candidates can reach:

$$
R_d(2000\,\mathrm{K})\sim580{-}660\ \mathrm{nm},
$$

but they generally pay a price in pressure, \(N_d\), or total score. This confirms that \(R_d\) is one of the hardest quantities to fit simultaneously with \(N_d\) and \(q_{gb}\).

### 6.3 Gas partition and \(q_{gb}\)

The v14 best candidate gave good gas partition.

At \(1.1\%\) FIMA:

| Temperature | \(q_{gb}\) |
|---:|---:|
| 1600 K | \(\sim3.6\%\) |
| 1800 K | \(\sim4.7\%\) |
| 1900 K | \(\sim4.9\%\) |
| 2000 K | \(\sim5.0\%\) |

At \(3.2\%\) FIMA:

| Temperature | \(q_{gb}\) |
|---:|---:|
| 1600 K | \(\sim3.0\%\) |
| 1800 K | \(\sim3.5\%\) |
| 1900 K | \(\sim3.6\%\) |
| 2000 K | \(\sim3.7\%\) |

The dislocation gas fraction at \(2000\,\mathrm{K}\) remains high, roughly:

$$
93{-}96\%.
$$

This is one of the strongest successes of v14: the grain-face gas fraction is kept low, while the dislocation bubble population dominates the gas storage at high temperature.

---

## 7. Main calibration lessons

### 7.1 The model can fit several experimental targets, but the solution is not unique

A major Optuna finding is that there are many combinations of scaling factors that produce good fits to the experimental swelling and radius/concentration data.

This indicates an identifiability problem:

$$
\boxed{
\text{different combinations of } D_g,\ D_v,\ b,\ g_b,\ g_d,\rho_d,K_d,\text{ and coalescence can compensate each other.}
}
$$

For example:

- lowering re-solution keeps gas in bubbles;
- increasing dislocation trapping moves gas from matrix to dislocation bubbles;
- increasing \(\rho_d\) and \(K_d\) raises the initial/effective number of dislocation bubbles;
- increasing dislocation-dislocation coalescence lowers \(N_d\) at high temperature;
- lowering bulk-dislocation coalescence preserves the small-bubble/bulk population rather than transferring it geometrically into \(P_2\);
- increasing vacancy diffusivity helps pressure relaxation, but does not by itself solve the high-temperature equilibrium problem.

Therefore, a good Optuna score is not by itself proof of a unique physical parameter set.

---

### 7.2 \(N_d\) and \(R_d\) are coupled and difficult to fit simultaneously

The structural relation is:

$$
\mu_d
=
N_dV_d,
$$

and for spherical bubbles:

$$
R_d
\propto
\left(
\frac{\mu_d}{N_d}
\right)^{1/3}.
$$

Therefore, if \(N_d\) drops strongly while swelling remains high, the mean radius must increase.

This explains a recurrent trade-off in the Optuna campaigns:

$$
N_d \downarrow
\quad\Rightarrow\quad
R_d \uparrow.
$$

The optimizer can improve the high-temperature \(N_d\) decrease only by allowing larger bubbles, unless gas partition, swelling, or pressure are allowed to change.

This is why some earlier candidates produced excellent \(N_d\) drop but unrealistic micrometric or near-micrometric radii. v14 improved this balance, but did not remove the fundamental coupling.

---

### 7.3 The model still does not clearly saturate at \(2000\,\mathrm{K}\)

Even in the best v14 candidate, the high-temperature state is not a clear equilibrium/saturation state.

At \(2000\,\mathrm{K}\):

- \(R_d\) is still increasing strongly with temperature;
- \(N_d\) is still decreasing due to coalescence;
- the dislocation bubbles continue to store most of the gas;
- the pressure diagnostic does not robustly collapse to \(p_d/p_{eq}\simeq1\) in all high-temperature/high-burnup cases.

In other words, even after varying all major scaling factors, the model does not fully reach a trapping--resolution--vacancy equilibrium at \(2000\,\mathrm{K}\).

A better statement is:

$$
\boxed{
\text{The calibrated model produces a good high-temperature trend, but not a demonstrated asymptotic equilibrium by }2000\,\mathrm{K}.
}
$$

This should be investigated explicitly by extending calculations above \(2000\,\mathrm{K}\) and checking whether \(R_d\), \(N_d\), and \(p_d/p_{eq}\) eventually level off.

---

### 7.4 The pressure problem is not solved by scaling factors alone

The pressure behavior remains a major diagnostic limitation.

The current bubble initialization starts from essentially fresh bubbles with no physically relaxed vacancy inventory. As gas enters the bubbles, the internal pressure is initialized or driven above the equilibrium pressure:

$$
p_i > p_{i,eq}.
$$

The vacancy absorption model then tries to relax the bubble by absorbing vacancies, but in the current reduced implementation this does not always bring the dislocation bubbles to equilibrium, especially at high temperature and high burnup.

This means that even after:

- using the refitted vacancy diffusivity coefficient;
- scaling the thermal and irradiation-enhanced vacancy diffusivity components;
- allowing re-solution and trapping scales to vary;
- allowing dislocation density and coalescence to vary;

the model still does not robustly enforce:

$$
p_d/p_{d,eq}\rightarrow 1
$$

at \(2000\,\mathrm{K}\).

This suggests that the issue may not be only parameter calibration. It may involve the initialization of bubble vacancy content, the vacancy absorption formulation, or missing physics related to high-temperature release/interconnection.

---

### 7.5 The refitted vacancy diffusivity is a central uncertainty

The refitted irradiation-enhanced vacancy diffusivity coefficient is almost \(10\) orders of magnitude lower than the tabulated coefficient:

$$
A_{20}^{refit}/A_{20}^{Rizk}\simeq3.5\times10^{-10}.
$$

This was done because the table value did not reproduce the plotted Rizk vacancy diffusivity curve in the previous diagnostic work.

This is a serious uncertainty. It means that the calibrated model is not simply "Rizk with scale factors"; it is already using a vacancy diffusivity interpretation selected to reproduce the plotted behavior.

Yet, even with this much lower vacancy diffusivity scale, the pressure equilibrium issue remains. Therefore, the pressure problem cannot be solved by simply saying that \(D_v\) was too high or too low; it requires a dedicated study.

---

### 7.6 Bulk-dislocation coalescence is not the dominant mechanism in v14

The v14 best basin selected:

$$
s_{bd}\sim0.065{-}0.10.
$$

This is small.

Therefore, the calibrated model does not rely on strong geometrical coalescence of bulk bubbles into dislocation bubbles.

This is useful because it avoids over-interpreting \(P_1\) as a free homogeneous population that is continuously swept into \(P_2\). Numerically, the optimizer preferred to keep this interaction weak and to control the high-temperature \(P_2\)-like population through:

$$
\rho_d,\quad K_d,\quad g_d,\quad b_d,\quad D_v,\quad s_{coal,d}.
$$

---

## 8. Why many scaling-factor combinations can fit the data

The model has several compensating mechanisms.

A simplified view is:

$$
\mu_d \sim N_d R_d^3.
$$

The gas stored in dislocation bubbles depends on:

$$
g_d c - b_d m_d,
$$

while \(R_d\) is also controlled by vacancy absorption and pressure relaxation through \(D_v\). The number density \(N_d\) depends on the initial/effective dislocation site density and on coalescence.

Thus, a similar \(\mu_d(T)\) can be obtained by different combinations:

1. high \(N_d\), moderate \(R_d\);
2. lower \(N_d\), larger \(R_d\);
3. stronger \(g_d\), lower \(b_d\);
4. higher \(\rho_d\), stronger coalescence;
5. higher \(D_v\), different pressure relaxation;
6. stronger or weaker bulk trapping compensated by \(q_{gb}\) and dislocation trapping.

This explains why Optuna can find many "good" candidates. The experimental swelling alone is not enough to uniquely determine the physical parameters. Simultaneous fitting of \(N_d\), \(R_d\), gas partition, and pressure is necessary, but even this does not fully remove non-uniqueness.

---

## 9. What v14 achieved

The v14 calibration achieved several concrete improvements:

1. \(q_{gb}\) was reduced to a physically acceptable low value.
2. The gas partition at high temperature became dominated by dislocation bubbles.
3. \(N_d(T)\) finally showed a significant high-temperature decrease.
4. The effective dislocation density model became more physically defensible by using a saturating \(T\)-dependence.
5. The best candidates no longer required very large bulk-dislocation coalescence.
6. The top candidates became internally coherent after enough trials.

The best v14 candidate is therefore a useful working model.

---

## 10. What v14 did not solve

The v14 calibration did **not** solve all model limitations.

The unresolved points are:

1. \(R_d(T)\) does not clearly saturate at \(2000\,\mathrm{K}\).
2. \(N_d(T)\) improves but remains difficult to fit perfectly over the full high-temperature range.
3. The pressure ratio \(p_d/p_{d,eq}\) does not robustly approach unity at high temperature and high burnup.
4. The pressure initialization likely starts from overpressurized bubbles.
5. The vacancy diffusivity relies on a refitted coefficient about \(2.85\times10^9\) times lower than the tabulated coefficient.
6. Multiple parameter combinations can produce similar fits, so the calibrated parameters are not uniquely identified.
7. The reduced \(q_{gb}\) variable is only gas reaching the grain face, not a complete grain-boundary bubble and fission gas release model.

---

## 11. Future work after 07/05/2026

The following items are postponed to further development after 07/05/2026.

### 11.1 Bubble initialization study

The first follow-up should test whether the pressure problem is partly caused by the initial bubble state.

Current issue:

$$
p_i > p_{i,eq}
$$

appears early because bubbles are initialized without a physically relaxed gas/vacancy inventory.

Future tests:

1. initialize newly nucleated bubbles with a seed radius;
2. initialize vacancy content so that:

$$
p_i(t=0)\simeq p_{i,eq}(t=0);
$$

3. compare pressure, \(R_d\), \(N_d\), and swelling against the current initialization;
4. check whether equilibrium is reached without artificial suppression of growth.

### 11.2 High-temperature equilibrium study above \(2000\,\mathrm{K}\)

The current data and scoring stop mainly around \(2000\,\mathrm{K}\). Since the model still grows at this temperature, it is necessary to test:

$$
T>2000\,\mathrm{K}.
$$

Questions:

1. Does \(R_d\) eventually saturate?
2. Does \(N_d\) reach a lower plateau?
3. Does \(p_d/p_{d,eq}\) approach 1?
4. Does gas remain trapped in dislocation bubbles indefinitely?
5. Does the model require an explicit high-temperature dislocation-density decrease or release path?

### 11.3 Vacancy diffusivity audit

The discrepancy between:

$$
A_{20}^{Rizk}=1.32\times10^{-19}
$$

and:

$$
A_{20}^{refit}=4.63\times10^{-29}
$$

must be revisited.

The next step is to verify whether the difference comes from:

1. a sign convention in the exponent;
2. a unit conversion issue;
3. a transcription error in the table;
4. a plotting/normalization issue;
5. a different assumed fission rate density.

### 11.4 Full grain-boundary/release coupling

The current \(q_{gb}\) is only a reduced accounting variable. A more complete model should distinguish:

$$
q_{gb}
=
q_{\text{grain-boundary bubbles}}
+
q_{\text{released}}.
$$

This could affect high-temperature gas partition and pressure relaxation.

---

## 12. Short thesis-style conclusion

The Optuna calibration campaigns showed that the reduced UN/M7 model is flexible enough to reproduce several experimental and diagnostic features of intragranular swelling in UN, but only through non-trivial compensation among diffusivity, trapping, re-solution, dislocation density, and coalescence scaling factors. The best v14 candidate obtained a good gas partition, low \(q_{gb}\), active high-temperature decrease of \(N_d\), and reasonable dislocation bubble radii. However, the model still does not demonstrate a clear high-temperature saturation of \(R_d\) and \(N_d\) at \(2000\,\mathrm{K}\), and the dislocation bubble pressure does not robustly relax to equilibrium.

The most important calibration lesson is therefore not a single best parameter set, but the identification of the remaining structural problem: the reduced single-size dislocation bubble model can be calibrated to fit the available data, but it still lacks a robust mechanism that naturally brings the high-temperature dislocation bubble population to a saturated, pressure-equilibrated state. Further work should focus on bubble initialization, vacancy absorption, high-temperature equilibration above \(2000\,\mathrm{K}\), and the vacancy diffusivity discrepancy.

---

## 13. Minimal record of the best v14 candidate

For future reference, the candidate to keep as current best working point is:

```text
version: v14 rhoSat qgbStrict NdAnchors
trial: 313
score_final: ~1.751

f_n: 1.3219e-6
K_d: 3.1015e5 bubble/m
fission_rate: 7.7227e19 fiss/m3/s
rho_scale: 1.6348

D2_xe_scale: 1.0000
Dg_D1_scale: 0.5995
Dg_D3_scale: 1.2414
Dv_D1_scale: 0.2026
Dv_D2_scale: 0.5742

b_bulk_scale: 0.06249
b_dislocation_scale: 0.20935

gb_scale: 5.5575
gd_bubble_scale: 9.6480
gd_line_scale: 3.3970
gd_line_alpha: 0.10240

coalescence_d_scale: 1.8104
bulk-dislocation coalescence factor: 0.06521
```

The corresponding key outputs are:

```text
N_d(1400 K): 1.76e19 m^-3
N_d(1550 K): 1.71e19 m^-3
N_d(1650 K): 1.51e19 m^-3
N_d(1800 K): 8.29e18 m^-3
N_d(1900 K): 3.79e18 m^-3
N_d(2000 K): 1.04e18 m^-3

R_d(1600 K): 64.6 nm
R_d(1800 K): 142 nm
R_d(1900 K): 222 nm
R_d(2000 K): 395 nm

q_gb(1.1% FIMA, 2000 K): ~5.0%
q_gb(3.2% FIMA, 2000 K): ~3.7%
dislocation gas fraction at 2000 K: ~93--96%
```
