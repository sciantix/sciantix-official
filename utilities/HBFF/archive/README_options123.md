# HBFF — options 1, 2, 3 and the comparison of the four options
#### Author of this folder: E.Cappellari, POLIMI-CEA 2026

Options 1 (Jernkvist 2019), 2 (Kulacsy 2015) and 3 (LEFM) of `iHighBurnupStructureFragmentation`, implemented in
`src/models/HighBurnupStructureFragmentation.C`, and their comparison with option 4 on the NFIR and IFA-650 cases.
Option 4 is described in `../README_HBFFmodel.md`; the figures of this file are drawn by `plot_options123.py`.

`iHighBurnupStructureFragmentation` computes the fraction of broken pores, the burst release of their gas and the mean fragment size.

Tag:
**[P]** paper, **[R]** reduction into the SCIANTIX state, **[U]** application to the HBS material, **[N]** numerics,
**[?]** unknown parameter, **[E]** deviation from a source.

## Input & regression cases

`iHighBurnupStructureFragmentation` = `Sciantix_options[25]`
```
2    #    iHighBurnupStructureFragmentation (0= off, 1= Jernkvist (2019), 2= Kulacsy (2015), 3= LEFM (Irwin), 4= rupture per pore class)
```

The regression cases `regression/hbs_nfir` and `regression/hbs_ifa650` (`test_UO2HBS_frag_*`, one case per option) use `iHighBurnupStructureFormation = 4`
and `iHighBurnupStructurePorosity = 3`; the model accepts porosity 2 or 3. `./utilities/runHBS.sh` runs them with the `regression/hbs`
cases (runner flags `--hbs_nfir`, `--hbs_ifa650`).

Temperature and hydrostatic pressure come from `input_history.txt` (4th column, MPa, negative in compression): $P_h = -\sigma_h$.
The base irradiation is under compression: NFIR rises from 0 to 70 MPa (the rule of `regression/hbs`, no datum for the disc **[?]**),
IFA-650 is at 20 MPa while at power **[?]**.

Outputs: `HBS fragmented fraction` $D$, `HBS burst release fraction` $G$, `HBS gas retention fraction` $S$,
`HBS fragment size` $d$, `HBS pore pressure` $p_g$. Option 4 adds the state of its pore classes: `HBS broken fraction - class i`
($i=0\dots80$) and `HBS gas retention fraction - second moment`.

Figures (`plot_options123.py`, into `figures/`): `criteria_comparison.png` (critical pressure against the radius and the pressure shape of each option),
`frag_cases.png`, `frag_ifa650.png` (D, G of the four options), `nfirv.png`, `pore_state_*.png`, `pore_distribution_*.png`, `release_budget.png`,
`pressure_time.png`, `kulacsy_class_breakdown.png`.

![criteria](figures/criteria_comparison.png)
![NFIR](figures/frag_cases.png)
![IFA-650](figures/frag_ifa650.png)

In `frag_ifa650.png` the curve of option 4 is not that of its present closure ($a=2$): the IFA-650 cases are not run with it.

## Model
Pore pressure, hard-sphere (Carnahan–Starling) EoS of `HighBurnupStructurePorosity.C`, cases 2 and 3 **[R]**:

$$
p_g = \frac{n\,k_B T}{V_p}\,Z(\eta),\qquad
Z=\frac{1+\eta+\eta^2-\eta^3}{(1-\eta)^3},\qquad
\eta=\min\!\left(\frac{n\,v_{Xe}(T)}{V_p},\,0.65\right)
$$

$$
v_{Xe}=\frac{\pi}{6}d_{Xe}^3,\qquad d_{Xe}=4.45\times10^{-10}\,\bigl[0.8542-0.03996\ln(T/231.2)\bigr]\ \text{m}
$$

with $n$ the Xe atoms per pore and $V_p$ the pore volume. Capillary pressure and overpressure:

$$
P_s=\frac{2\gamma}{R},\quad \gamma=1.1\ \text{N/m},\qquad \Delta p = p_g-P_s-P_h .
$$

Pore classes **[R] [N]**.

- Option 1 uses the mean pore.
- Option 2 uses 81 classes ($\ln r = \ln r_{med}\pm4\sigma_{\ln r}$) as the fixed log-normal of K15; only $\xi$, $T$, $P_h$ of the SCIANTIX state enter (L12).
- Options 3 and 4 use the pore state of SCIANTIX, the two moments of $n$ (mean $\bar n$ and variance $\mathrm{Var}(n)$); the radius is log-normal.
  Option 3 takes a uniform pressure, $R\propto n^{1/3}$, and integrates the log-normal analytically (erfc):

$$
\mathrm{CV}_R=\frac{\sqrt{\mathrm{Var}(n)}}{3\,\bar n},\qquad
\sigma_{\ln R}=\sqrt{\ln\!\left(1+\mathrm{CV}_R^{2}\right)}
$$

  Option 4 sums 81 classes with the gas shared as $n_i\propto R_i^{a}$, $a=2$, and the width from $\mathrm{Var}(n)$ on the classes (`../README_HBFFmodel.md`, 2).

Broken fractions **[N] [U]**. Each criterion returns an instantaneous $D^*$ (pore-number fraction, geometry) and $G^*$
(gas fraction, release). With $\alpha$ the restructured volume fraction:

$$
\text{if }\alpha>\alpha_{old}:\ D\leftarrow D\,\frac{\alpha_{old}}{\alpha},\ G\leftarrow G\,\frac{\alpha_{old}}{\alpha};\qquad
D\leftarrow\max(D,D^*),\quad G\leftarrow\max(G,G^*)
$$

Option 4 keeps a broken fraction per class, diluted and never decreasing in the same way; $D$ and $G$ are its sums over the classes.

Venting **[R]**. A broken pore vents its gas and stays as a void. Options 1, 2, 3: if $G$ grows to $G_{new}$, with
$f=\max\!\bigl[(1-G_{new})/(1-G),\,10^{-12}/S\bigr]$:

$$
A\to fA,\quad B\to f^{2}B,\quad \bar n\to f\bar n,\quad \mathrm{Var}(n)\to f^{2}\mathrm{Var}(n),\quad S\to fS,
$$

$A$ is `Xe in HBS pores`, $B$ its variance; the gas leaves through `Xe released` by the mass balance of `GasRelease.C`. The criteria see the state before the venting, $\bar n/S$ and $\mathrm{Var}(n)/S^{2}$.
Option 4 vents the broken classes only and carries the retained fraction of the second moment of $n$ beside $S$ (`../README_HBFFmodel.md`, 7).

Validity window of the criteria **[?]**: R > 0.1 um and $0<\xi<0.35$.

### Fidelity to the source

| option | core criterion | faithful to the source | our reduction / deviation |
|---|---|---|---|
| 1 Jernkvist (2019) | ligament force balance (Olander), J19 Eq. 22 | the criterion and $\sigma^{cr}_{hbs}=21$ MPa (J19 Table 4) | none beyond the shared hard-sphere pore pressure and porosity/venting bookkeeping |
| 2 Kulacsy (2015) | pore-class punching pressure + tangential-stress rupture, K15 Eq. 3, 7, 11, 15 | the log-normal pore classes (median $e^{-0.5}$ µm, $\sigma_{\ln r}=0.356$), punching pressure, stress and strength equations | Ronchi's tabulated EoS is replaced by van der Waals **[E]**; the Poisson ratio is the matrix constant (K15 Eq. 13 makes it porosity dependent); the transient-start rule; the classes do not follow the SCIANTIX pore state (L12) |
| 3 LEFM | penny-shaped crack, Irwin (1957) / J20 Eq. 8 | the elastic term $\sqrt{\pi E G_{hbs}/[(1-\nu^2)R]}$ | Eq. 8 has no capillary term (flat infinite crack); $2\gamma/R$ is added **[R]**; $G_{hbs}=0.055$ J/m$^2$ is fitted on NFIR and $Y=1$ is left unfitted **[?]** |
| 4 rupture per class | first principal stress at the pore against a rupture stress (OHP 2.2.2), with the mean-field stress concentration factor of J19 Eq. 22 on every pore class | $\sigma^{cr}_{hbs}=21$ MPa (Table 4), the ligament force balance | the gas of the classes follows the closure $n_i\propto R_i^{a}$, $a=2$ **[?]**; percolation at $\xi=0.29$ opens every class **[E]**; $\tau=2$ s is borrowed from the J19 interconnection and microcracking modes **[?]**; no fitted parameter |

Shared across all options: the hard-sphere pore pressure and venting/rescaling bookkeeping (**[R]**, `HighBurnupStructurePorosity.C`), and the strength law $\sigma_f(T,\xi)$ of K15 Eq. 15, used by option 2 under MATPRO's own name FFRACS **[U]**.

### Option 1 - Jernkvist (2019) **[P]**

Ligament force balance across the plane (Olander) with the areal fraction replaced by the volume fraction $\xi$
(Delesse), the far-field stress replaced by $-P_h$ (J19 Eq. 22, J20 Eq. 7):

$$
P_{cr}=P_s+\frac{\sigma^{cr}_{hbs}\,(1-\xi)+P_h}{\xi},\qquad \sigma^{cr}_{hbs}=21\ \text{MPa}
$$

Rupture when $p_g\ge P_{cr}$ on the mean pore: $D^*=G^*=1$.

### Option 2 - Kulacsy (2015) **[P]**

Before the transient every class of radius $r$ is at the dislocation punching pressure (K15 Eq. 3, 12–14):

$$
p_0(r)=P_{h,ss}+\frac{2\gamma_{ss}}{r}+\frac{G\,b}{r},\qquad
\gamma_{ss}=0.41\,(0.85-1.4\times10^{-4}T_{ss}),\qquad
G=\frac{E_{ss}}{2(1+\nu)}
$$

$$
E_{ss}=2.334\times10^{11}\,(1-1.0915\times10^{-4}\,T_{ss})\ \text{Pa},\quad \nu=0.32,\quad b=0.39\ \text{nm}
$$

$T_{ss}$, $P_{h,ss}$ are the last-cycle state. The gas per class $n_i$ follows from $p_0$ with the EoS at $T_{ss}$; during
the transient the pores are rigid and $p_i(T)$ follows the EoS. Van der Waals replaces the Ronchi EoS of the paper **[E]**:

$$
p=\frac{n\,k_B T}{V-n\,b_{w}}-a_w\frac{n^{2}}{V^{2}},\qquad a_w=1.17\times10^{-48}\ \text{J m}^3,\quad b_w=8.49\times10^{-29}\ \text{m}^3 .
$$

Stress and strength (K15 Eq. 7, 11, 15):

$$
\sigma_t=p_i-\left(\tfrac{3}{2}P_h+\frac{2\gamma(T,\xi)}{r}\right),\qquad
\gamma(T,\xi)=0.41\,(0.85-1.4\times10^{-4}T)\,(1-\xi)^{4.025}
$$

$$
\sigma_f(T,\xi)=170\ \text{MPa}\;e^{-191.34/\min(T,\,1000)}\sqrt{1-2.62\,\xi}
$$

Classes are log-normal with median $e^{-0.5} \mu m$ and $\sigma_{\ln r}=0.356$ (K15 Table 2). The classes below the
smallest stable pore, $r_{min}=G\,b/(\sigma_f(T_{ss})+P_{h,ss}/2)$, do not exist. With $w_i$ the renormalised weights:

$$
D^*=\sum_i w_i\,[\sigma_{t,i}>\sigma_f],\qquad
G^*=\frac{\sum_i w_i n_i\,[\sigma_{t,i}>\sigma_f]}{\sum_i w_i n_i}
$$

The transient starts at the first step in which the fission rate falls **[?]**, and the reference state ($T_{ss}$, $P_{h,ss}$) is the
state of the step before. If the fission rate then falls below 1 % of its value at that step and later returns above 50 % of it, the
power-off was an outage and the reference state is tracked again (L7).

### Option 3 - LEFM **[P]**

Penny-shaped crack of radius $R$ under the pore pressure (Irwin 1957; J20 Eq. 8), with capillarity and an interaction
factor $Y$:

$$
P_{cr}(R)=P_h+\frac{2\gamma}{R}+\frac{1}{2Y}\sqrt{\frac{\pi\,E\,G_{hbs}}{(1-\nu^{2})\,R}},\qquad
E=2.237\times10^{11}(1-2.6\xi)\,[1-1.394\times10^{-4}(T-293)]\,[1-0.1506(1-e^{-0.035\,Bu})]\ \text{Pa}
$$

($E$ is the `UO2HBS` modulus of `SetMatrix.C`, $Bu$ in MWd/kgUO$_2$). $P_{cr}$ decreases with $R$; the pores with
$R\ge R_c$, $P_{cr}(R_c)=p_g$, break (bisection on $R_c$). With $z=\ln(R_c/\bar R)/\sigma_{\ln R}$:

$$
D^*=\tfrac12\,\mathrm{erfc}\!\left(\frac{z}{\sqrt2}\right),\qquad
G^*=\tfrac12\,\mathrm{erfc}\!\left(\frac{z-3\sigma_{\ln R}}{\sqrt2}\right)
$$

$G^*$ is the third moment of the log-normal (the gas is proportional to $R^3$ at uniform pressure).

### Option 4 - rupture per pore class, progressive **[P] [R] [?]**

Described in `../README_HBFFmodel.md`.

### Fragment size **[R] [U]**

Cubic fragments bounded by the cracked faces of the Wigner–Seitz cells of the pores:

$$
R_{WS}=\left(\frac{3}{4\pi N_p}\right)^{1/3},\qquad S_v=N_p\,D\,A_f,\qquad
d=\min\!\left(\frac{3}{S_v},\,d_{max}\right),\qquad A_f=2\pi R_{WS}^{2}\ \Rightarrow\ d=\min\!\left(\frac{2R_{WS}}{D},\,d_{max}\right)
$$

$A_f$ is the cracked area per broken pore **[?]**: half of the surface of its Wigner–Seitz sphere, each face being shared with the neighbouring cell
(so that $D\to1$ gives $d=2R_{WS}$, the cell diameter); $d_{max}=500\ \mu m$ (above, pellet cracks);
$d=0$ if $D<10^{-9}$.

### Parameters

Values of the code. Only the parameters tagged fitted are calibrated; options 1, 2 and 4 run with the literature values.

| symbol | value | option | tag | source |
|---|---|---|---|---|
| $\sigma^{cr}_{hbs}$ | 21 MPa | 1, 4 | [P] | J19 Table 4 (local tensile strength of the HBS, calibrated by Jernkvist on IFA-650 and annealing data, not here) |
| $\xi_{perc}$ | 0.29 | 4 | [P] | J19 section 2.3.2 |
| closure exponent $a$ | 2 | 4 | [?] | gas density $\propto1/R$: for an ideal gas the shape of the punching overpressure $c_{dp}/R$ of J19 Eq. 21; 3 is a uniform pressure |
| $\tau$ | 2 s | 4 | [?] | J19 Table 3 (interconnection and microcracking modes) |
| $b$, $\nu$ | 0.39 nm, 0.32 | 2 | [P] | K15 |
| $\ln$-median, $\sigma_{\ln r}$ of $r$ (µm) | $-0.5$, 0.356 | 2 | [P] | K15 Table 2 (the minus sign is lost in the text extraction; Figs 7-8 have modes of 0.3–0.4 µm, consistent with $m=-0.5$) |
| $\gamma_{ss}$, $E_{ss}$ | $0.41(0.85-1.4\times10^{-4}T)$ N/m, $2.334\times10^{11}(1-1.0915\times10^{-4}T)$ Pa | 2 | [P] | K15 Eq. 11, 14 |
| $a_w$, $b_w$ | $1.17\times10^{-48}$ J m$^3$, $8.49\times10^{-29}$ m$^3$ | 2 | [E] | van der Waals xenon, in place of Ronchi's table |
| $G_{hbs}$, $Y$ | 0.055 J/m², 1 | 3 | [?] | $G_{hbs}$ fitted on the NFIR Kr-85 release (30.7 % against 29.8 % at 1200 °C, with the 0–70 MPa base irradiation); the bulk value is 2 J/m²; $Y$ unfitted |
| $E$ | $2.237\times10^{11}(1-2.6\xi)[1-1.394\times10^{-4}(T-293)][1-0.1506(1-e^{-0.035Bu})]$ Pa | 3 | [R] | `UO2HBS` modulus of `SetMatrix.C`, recomputed at each step |
| $r_{min,eval}$, $\xi_{max,eval}$ | 0.1 µm, 0.35 | all | [?] | validity window of the criteria (L9) |
| $A_f$, $d_{max}$ | $2\pi R_{WS}^2$, 500 µm | size | [?] | none; $d_{max}$ from NEA/CSNI/R(2016)16 §3 |
| $\gamma$ | 1.1 N/m | 1, 3, 4 | [R] | `SetMatrix.C` (`UO2HBS`) |
| hard-sphere EoS | $d_{Xe}=4.45\times10^{-10}[0.8542-0.03996\ln(T/231.2)]$ m, $\eta\le0.65$ | 1, 3, 4 | [R] | `HighBurnupStructurePorosity.C` |

The pore state itself (porosity, radius, density, gas per pore) is set by `HighBurnupStructurePorosity.C`, not by this model. Its equilibrium
pressure is $2\gamma/R-\sigma_h$, so the hydrostatic stress of `input_history.txt` during the base irradiation changes the pore state
that every option then evaluates.

## Results (current build, no refit beyond $G_{hbs}$)

| case | 1 Jernkvist | 2 Kulacsy | 3 LEFM | 4 rupture per class |
|---|---|---|---|---|
| NFIR release at 1200 °C (data 29.8 %) | 0 | 56.5 % (all pores before 1000 °C) | 30.7 % | 11.6 % (D = 44 %, G = 21 %, d = 3.4 µm), from room temperature |
| IFA-650.9 (significant fragmentation) | D = 1 at 1128 K | D = 0.98 from 530 K | D = 1 at 1043 K | not run with $a=2$ |
| IFA-650.10 (negative control) | none | D = 0.94 from 558 K | none | not run with $a=2$ |

## Standing limitations

- **L1 [R]** the pressure is uniform over the pore classes in option 3; K15 and J19 use $\Delta p\sim1/R$; in option 4 it follows the closure
  $n_i\propto R_i^{a}$, a gas density $\propto R^{a-3}$ ($a=3$ uniform pressure; $a=2$ density $\sim1/R$, with a hard-sphere pressure steeper than $1/R$).
- **L2 [R]** options 1, 2, 3: the venting scales the gas by $(1-\mathrm dG)$ and its variance by the square, as if every pore lost the same
  fraction, also for the size-dependent options 2 and 3. Option 4 vents the broken classes only.
- **L3 [E]** options 1, 3, 4 use the hard-sphere EoS of the porosity model; option 2 uses van der Waals in place of Ronchi.
- **L4 [?]** the fragments are equiaxed; HBS fragments are acicular (NEA, 3.1.3).
- **L5 [?]** no lower bound on the fragment size: a 40 µm HBS fragment does not fragment further at 1300 °C.
- **L6 [N]** $D$ and $G$ are cumulative maxima: no healing.
- **L7 [?]** option 2 starts the transient at the first step in which the fission rate falls (any fall, so a floating-point difference of about 1e-9 in the interpolated fission rate where the history changes $T$
  or $P_h$ at constant power can start it), keeps the state of the step before as the reference ($T_{ss}$, $P_{h,ss}$), and tracks it again if the fission rate falls below 1 % and later returns above 50 %
  of the value at the start (outage). A power step-down that never reaches 1 % is read as the transient start for the rest of the history.
- **L8 [?]** the porosity model relaxes the vented pores (capillary shrinkage; with partial venting the survivors lose gas). In option 4 the mean
  pore volume falls (4 % on the NFIR anneal) and the gas trapped afterwards goes to the intact classes, which raises their pressure.
- **L9 [?]** the criteria act only for a mean pore radius of at least 0.1 µm and a porosity below 0.35; the window is on the mean pore, the classes of option 4 extend below 0.1 µm.
- **L10 [?]** the free parameter of option 3 ($G_{hbs}$) is fitted on one dataset (NFIR, a slow anneal) and never on a depressurisation; options 1, 2, 4 are not fitted. Option 2 is very sensitive to $P_h$ of the base irradiation.
- **L11 [R]** the pore state that SCIANTIX builds (formation 4, porosity 3) differs from the NFIR measurement: 10.5 % porosity against 11 %, $R=0.35$ µm against 0.88 µm, $N_p=5.8\times10^{17}$ m$^{-3}$ against $4\times10^{16}$. The criteria act on the modelled state, not on the measured one.
- **L12 [?]** option 2 uses only $\xi$, $T$, $P_h$ of the SCIANTIX state: its classes are K15's fixed log-normal, so its $G$ does not follow the gas that SCIANTIX holds in the pores, while the venting scales that gas by $1-G$.
- **L13 [?]** the confining stress of the base irradiation is an input of the history (no PCMI model) and changes the pore state through the equilibrium pressure of the porosity model.
- **L14 [?]** the IFA-650 cases are one rim point with a transient taken from another test: no radial or axial structure, no cladding strain (5–10 % needed for visible fine fragmentation), no real $T$ and $P_h$ traces.
- **L15 [?]** no heating-rate dependence except the relaxation time of option 4 (more release and pulverisation at 20 °C/s than at 0.2 °C/s, NEA §3.3.1.1); only the HBS is fragmented.
- **L16 [?]** option 4 (split in `../README_HBFFmodel.md`): the closure exponent $a=2$ is not determined by the SCIANTIX moments and sets which classes break (NEA reports Hiernaut's estimate that large and small pores have similar pressure, $a=3$; the punching limit of K15 and J19 is $a=2$ for a dilute gas), and with the hard-sphere equation of state it puts the smallest classes above the critical pressure already in the base irradiation; the stress concentration factor is the mean-field one, porosity only, until the MMM tables exist; $\tau$ is borrowed; the pores do not interact after a break; the classes keep their rank while the moments evolve.
