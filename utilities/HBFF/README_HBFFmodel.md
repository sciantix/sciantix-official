# HBFF - rupture per pore class, progressive (`iHighBurnupStructureFragmentation = 4`)
#### Author of this folder: E.Cappellari, POLIMI-CEA 2026

Tag: **[P]** paper, **[R]** reduction into the SCIANTIX state, **[U]** application to the HBS material, **[N]** numerics,
**[?]** unknown parameter, **[E]** deviation from a source. **(SoA)** marks a statement taken from the state-of-the-art review of the
library and not re-checked on the PDF of its source.

### Goal
Per time step the model returns three quantities, written to `output.txt`:

| output | symbol | meaning |
|---|---|---|
| `HBS fragmented fraction` | $D$ | pore-number fraction of the HBS whose pores are broken (drives the fragment size) |
| `HBS burst release fraction` | $G$ | gas-weighted fraction of the broken pores (drives the release) |
| `HBS fragment size` | $d$ | mean fragment size (m) |

Auxiliary: `HBS gas retention fraction` $S$, `HBS pore pressure` $p_g$, and the state of the pore classes: `HBS broken fraction - class i` $\beta_i$ ($i=0\dots80$, from the smallest to the largest radius) and `HBS gas retention fraction - second moment` $S_2$ (7).

Inputs: $T$ and $P_h=-\sigma_h$ from `input_history.txt` ($\sigma_h$ in MPa, negative in compression), and the pore state of `HighBurnupStructurePorosity.C` (option 2 or 3; the run stops otherwise): porosity $\xi$, mean radius $R$, pore volume $\bar V$, Xe atoms per pore $\bar n$ and its variance $\mathrm{Var}(n)$, pore density $N_p$.

Validity window **[?]**, on the mean pore: $R\ge0.1$ µm and $0<\xi<0.35$; outside it no class breaks.

The input line, the regression case and its gates are in `README.md`.

### Position with respect to previous models

Fine fragmentation is attributed to a dense population of overpressurised closed pores, in the HBS and also further inside the pellet (J20; NEA16). The pores hold gas above the equilibrium pressure $P_h+2\gamma/r$, up to about the dislocation-punching limit ($Gb/r$ in K15, $c_{dp}/r$ in J19), and the pressure rises with the temperature at constant pore size in a transient (Aagesen 2021: the radius does not change in a LOCA ramp and the pressure grows linearly with $T$).

| approach | pore population | pressure before the transient | stress at the pore | rupture | outputs | open point |
|---|---|---|---|---|---|---|
| empirical thresholds: NFIR burnup–temperature curve, mass-fraction correlations (SoA) | none | none | none | threshold in local burnup and temperature | pulverised mass fraction | heating rate and microstructure before the transient not resolved |
| K15 | fixed log-normal (median $e^{-0.5}$ µm, $s=0.356$) | every pore at the punching limit $P_h+2\gamma/r+Gb/r$ | tangential stress of an isolated pore against the fracture strength | per class, progressive | released fraction | all the pores at the punching pressure (its declared main limitation); no interaction; no fragment size |
| J19 | one size, grown by punching | empirical partition of the gas into the pores | mean stress of the ligament, $\xi/(1-\xi)$, with $\sigma^{cr}_{hbs}=21$ MPa calibrated | the whole HBS at once | pulverised fraction, transient release | no spread of size, no stress concentration |
| KH18, KH20 | mean pore of the grain boundary and of the HBS | from its fission gas model | fracture ratio calibrated per accident type (1 in RIA, about 0.35 in LOCA) | at once | burst release, fragment size | calibration on the integral tests |
| OHP (MMM) | monodisperse, random positions | from the measured gas density | full elastic field with the interaction of the pores (1.2–3 times the isolated pore) | per pore, the most interacting first | release as broken pore volume | prescribed microstructure, uniform pressure, HPC, $\sigma_R=275$ MPa calibrated |
| phase field (Aagesen 2021) | one pore | prescribed (50–200 MPa) | – | no fracture model | pressure history of the pore | fracture not included |
| this model | 81 classes rebuilt from $\bar V$, $\bar n$, $\mathrm{Var}(n)$ of the porosity model | computed by the formation and porosity models, shared among the classes by the closure (2) | $\hat s$: mean field in the code, MMM tables to come (4) | per class, progressive (5) | $D$, $G$, $d$ from one state | closure exponent, equation of state, coupling with the porosity model (*Limitations*) |

What the model takes from them: the log-normal classes and their rupture one after the other from K15, the criterion of J19 as its mean-field limit, the stress concentration factor and its scatter from OHP (to be tabulated). What it adds: the classes follow the pore state that the code computes instead of a prescribed one; the number of broken pores and their gas are two fractions of the same state, which give the fragment size and the release; only the broken classes vent, and the venting acts back on the pore state.

The analytical criteria disagree on which pores break first: the stress-based ones break the small pores first, those based on fracture mechanics the large ones, and no data discriminate (J20). Here the order follows from the stress criterion and from the closure.

### Equations

#### 1. Pore pressure of the SCIANTIX state

Hard-sphere (Carnahan–Starling) equation of state **[R]**:

$$
p_g=\frac{n\,k_BT}{V_p}\,Z(\eta),\qquad Z(\eta)=\frac{1+\eta+\eta^2-\eta^3}{(1-\eta)^3},\qquad
\eta=\min\!\left(\frac{n\,v_{Xe}}{V_p},\,0.65\right)
$$

$$
v_{Xe}=\frac{\pi}{6}d_{Xe}^3,\qquad d_{Xe}=4.45\times10^{-10}\,\bigl[0.8542-0.03996\ln(T/231.2)\bigr]\ \text{m}
$$

Above $\eta=0.65$ the pressure is linear in the gas, $Z=Z(0.65)$. The criterion sees the gas of the pores before any venting (7).

#### 2. Pore classes

SCIANTIX carries $\bar V$ (radius $R_V=(3\bar V/4\pi)^{1/3}$), $\bar n$ and $\mathrm{Var}(n)$. The pore radius is log-normal (K15 Eq. 6) on 81 nodes $x_i\in[-4,4]$ equally spaced **[N]**, with number weights

$$
w_i=\frac{e^{-x_i^2/2}}{\sum_je^{-x_j^2/2}},\qquad g_i=e^{s\,x_i},\qquad
R_i=R_V\,\frac{g_i}{\bigl(\sum_jw_jg_j^3\bigr)^{1/3}},\qquad V_i=\tfrac43\pi R_i^3
$$

The scale is fixed so that the volume is conserved, $\sum_iw_iV_i=\bar V$, for any width $s$.

The two moments of $n$ do not say how the gas is shared among the radius classes. The closure is **[?]**

$$
n_i=\bar n\,\frac{R_i^{a}}{\sum_jw_jR_j^{a}},\qquad a=2
$$

so $\sum_iw_in_i=\bar n$. The closure fixes the gas density of the classes, $n_i/V_i\propto R_i^{a-3}$. $a=2$, the value of the code, is a density $\propto1/R$: for an ideal gas a pressure $\propto1/R$, the shape of the punching overpressure of K15 and J19. $a=3$ is a uniform density and pressure. Two things separate the pressure of the classes from a $1/R$ overpressure. The hard-sphere pressure is the ideal one times $Z(\eta_i)$, and $\eta_i\propto1/R$ too: at the NFIR state $Z$ goes from 1.2 in the largest class to 4.5 in the smallest, and $p_i$ falls as $R^{-1.2}$, $R^{-1.5}$ and $R^{-2.7}$ at the largest class, at $R_V$ and at the smallest class. And it is the gas pressure that follows the closure, not the overpressure: $p_i-P_h-2\gamma/R_i$ is negative for the classes above 0.48 µm (15 % of the pores) at the 70 MPa of the end of the base irradiation.

The width $s$ is the root of $\sum_iw_i(n_i-\bar n)^2=\mathrm{Var}(n)$ on the 81 truncated nodes: the coefficient of variation of $n$ on the nodes depends on $s$ and $a$ only and grows with $s$, so the root is bracketed between 0 and the value of the continuous log-normal, $s_0=\sqrt{\ln(1+\mathrm{Var}(n)/\bar n^2)}/a$, doubled until it is above the root. For $\mathrm{Var}(n)=0$, $s=0$ and all the classes coincide with the mean pore. The three moments $\bar V$, $\bar n$, $\mathrm{Var}(n)$ hold on the classes to $10^{-8}$ (`check_moments`).

![classes](figures/classes_distributions.png)

> `figures/classes_distributions.png` (`python3 pore_distribution.py`): the classes of the SCIANTIX state at the end of the NFIR base irradiation ($\bar V=2.649\times10^{-19}$ m³, $\bar n=1.20\times10^{9}$, $\mathrm{CV}_n=0.51$, 903 K): radius pdf, pressure and gas per class. Grey: the capillary pressure $2\gamma/R$, and ten times it (dashed) to compare the shapes. Dashed orange: the pressure of the same gas as an ideal gas, $n_ik_BT/V_i$, which is the $1/R$ shape of $a=2$; the hard-sphere pressure $p_i$ leaves it towards the small classes (756 MPa against 170 MPa at 0.14 µm).

At this state $s=0.24$ ($a=2$; 0.19 for $a=2.5$, 0.16 for $a=3$), against 0.36 of the log-normal fit of the pore sizes measured on the NFIR disc (9; about 0.3 from the mean and the standard deviation of OHP Fig. 4) and 0.356 of K15: the variance of $n$ that SCIANTIX carries gives two thirds of the measured width. The radii go from 0.14 to 0.95 µm. A log-normal describes the measured pore size distributions for local burnups of 80–150 GWd/tHM; above about 150 GWd/tHM they become long-tailed and multi-modal, with pore coarsening from about 100 GWd/tHM (SoA: Cappia 2016). The NFIR disc (103 GWd/tHM) is inside this range.

**Consistency with the base irradiation.** A class above the punching limit of J19 Eq. 21, $P_h+2\gamma/R+c_{dp}/R$ ($c_{dp}=55$ N/m), would grow by dislocation punching in steady state. With $a=2$ the hard-sphere pressure of the smallest classes rises above it: at the NFIR state 0.2 % of the pores, up to 756 MPa at 0.14 µm (for $a\ge2.25$ no class is above it).

![closure exponent](figures/classes_closure_exponent.png)

> `figures/classes_closure_exponent.png`: pressure per class for $a$ from 3 to 2 at the same moments, with $2\gamma/R$ (grey) and the punching limit (dashed).

**Closure against measured pores.** The closure gives a volume per gas atom $V_i/n_i\propto R_i^{3-a}$. Dowek 2021 measured it on single bubbles: in the two pairs at one position the smaller bubble holds the denser gas, the direction of $a<3$, with local exponents $a=0.6$ at the rim of sample B (0.585 and 0.655 µm, 25.1 and 9.9 MPa at 293 K) and $a=1.5$ at the centre of the ramped sample A, which is not HBS. Two bubbles are not a fit, and the pressure changes by a factor 2.5 between two rim pores 12 % apart in radius: more than a closure in $R$ alone gives (L16a). Between samples the rim means (mean radius 0.25–1.0 µm) are 149–191 Å³, with no clear trend with the mean bubble size (`section_closure_data`).

#### 3. Pressure per class
The equation of state of 1 on every class, $p_i=p_g(n_i,V_i,T)$, with the packing fraction of the class $\eta_i=\min(n_iv_{Xe}/V_i,\,0.65)$.

The mean pressure $\bar p=p_{\rm EoS}(\bar n,\bar V,T)$ is the pressure of the volume-weighted mean density $\sum_iw_in_i/\sum_iw_iV_i=\bar n/\bar V$ and holds because $\bar n$ and $\bar V$ are conserved. The number-weighted mean of $p_i$ is higher (120.5 against 88.5 MPa at the NFIR state) and is not a conserved quantity.

#### 4. Rupture criterion per class

Class $i$ breaks when the maximum first principal stress at the pore wall exceeds the rupture stress $\sigma_R$ (OHP D5.2 section 2.2.2). With the confinement $P_h$ and the capillary pressure $2\gamma/R_i$ carried by the pore surface, the net pressure on the solid is $p_i-2\gamma/R_i-P_h$, and the linear elastic stress is $\hat s_i$ times it above the confining stress $-P_h$:

$$
\hat s_i\,\Bigl(p_i-\frac{2\gamma}{R_i}-P_h\Bigr)\ \ge\ \sigma_R+P_h\quad\Longleftrightarrow\quad
p_i\ \ge\ P_{cr,i}=P_h+\frac{2\gamma}{R_i}+\frac{\sigma_R+P_h}{\hat s_i},\qquad \gamma=1.1\ \text{N/m}\ (\text{UO2HBS})
$$

The capillary term is not in OHP; it is kept as in J19 **[R]**.

**Stress concentration factor.** $\hat s_i$ is the stress at pore $i$ per unit net pressure, at zero macroscopic stress. It collects the effects of the neighbours, of the radius distribution and of the porosity, and it acts on the rupture only, not on $p_i$ or on the venting. $\hat s$ and $\sigma_R$ come as a pair: they are the stress measure and the strength calibrated with it.

| pair | $\hat s$ | $\sigma_R$ | critical net pressure, $P_h=0$, $\xi\approx0.11$ | $\mathrm dP_{cr}/\mathrm dP_h=1+1/\hat s$ | status |
|---|---|---|---|---|---|
| mean field, J19 Eq. 22 **[P]** | $\xi/(1-\xi)=0.12$, mean stress of the ligament | 21 MPa | 170–180 MPa | $1/\xi\approx9.5$ | in the code |
| MMM, OHP **[P]** | 0.6–1.5, local maximum at the pore (1.2–3 times the isolated pore, 0.5) | 275 MPa | 183 MPa (most interacting pore) to 458 MPa | 1.7–2.7 | monodisperse pores at uniform pressure only; tables for the SCIANTIX states to be computed (9) |

The two pairs agree on the pressure at which the first pores break without confinement and differ by a factor 3.5 to 6 in the effect of the confinement.

**Mean-field pair (in the code).** With $\hat s=\xi/(1-\xi)$ for all the classes and $\sigma_R=\sigma^{cr}_{hbs}=21$ MPa the criterion is J19 Eq. 22 (ligament force balance of Lemoine et al. 2000 / Olander, J19 section 2.3.3 (2)), with $\sigma_1\to-P_h$ as J19 does, on the radius of the class:

$$
P_{cr,i}=\frac{2\gamma}{R_i}+\frac{\sigma^{cr}_{hbs}\,(1-\xi)+P_h}{\xi}
$$

**MMM pair.** MMM solves the linear elastic problem on a periodic RVE of random spherical pores at zero macroscopic strain, with unit pressure in the pores; a hydrostatic stress $p_{hyd}$ arises in the constrained RVE. The factor of the criterion is the stress-free one (the rescaling of OHP),

$$
\hat s_i=\frac{\sigma_I^{max}\big|_i(1,p_{hyd})+p_{hyd}}{1-p_{hyd}}
$$

Its reduction in the SCIANTIX state is a regression of $\ln\hat s_i$ on the relative radius, the neighbours giving the scatter ($c_0$, $c_1$ are named so as not to clash with the closure exponent $a$ of 2):

$$
\ln\hat s_i=c_0+c_1\ln\frac{R_i}{R_V}+\varepsilon,\qquad \varepsilon\sim\mathcal N(0,\sigma_\varepsilon^2)
$$

With a pressure that depends on the radius, the stress field is the superposition of the fields of the single pores loaded by their net pressure $p_j-P_h$: pores at $p_j$ under the confinement $P_h$ are the unconfined RVE with pores at $p_j-P_h$, plus the uniform stress $-P_h$. The ratios of the net pressures of the classes depend on the pressure ratios $p_j/p_i$, fixed by $(\bar\eta,\mathrm{CV}_n,a)$ (9), and on $P_h/\bar p$: a uniform $P_h$ is a rescaling only for a uniform pore pressure ($a=3$, the OHP case) or for $P_h=0$. The maximum principal stress is not linear in the loads: it has to be taken on the superposed field, so every run has its own pressures (9).

$(c_0,c_1,\sigma_\varepsilon)$ are tabulated against $(\xi,\mathrm{CV}_n,\bar\eta)$ from unconfined runs **[R]**: $P_h$ enters the criterion, like the capillary pressure, and is not a load of MMM. Under confinement the net pressure of a neighbour at a lower gas pressure falls faster than that of the pore, $(p_j-P_h)/(p_i-P_h)<p_j/p_i$ for $p_j<p_i$, so the unconfined $\hat s_i$ overestimates the share of the neighbours for the classes above the pressure of their neighbours, those that break first, and underestimates it for the others. The pores break in the NFIR anneal, at $P_h=0$; in the base irradiation $P_h/\bar p$ is 0.79.

**Interconnected pores.** For $\xi\ge\xi_{perc}=0.29$ every class is open, $b^*_i=1$ **[E]**: J19 (end of section 2.3.2) stops the pore growth there and vents only the gas in excess of the overpressure $c_{dp}/r_p$, without fragmentation. The branch is not reached with the SCIANTIX porosity model, which saturates at $\xi=0.18$.

#### 5. Progressive rupture per class

Each class carries its own broken fraction $\beta_i\in[0,1]$. The instantaneous state $b^*_i$ is the probability that a pore of class $i$ meets the criterion. With $\ln\hat s\sim\mathcal N(\mu_i,\sigma_\varepsilon^2)$, $\mu_i=c_0+c_1\ln(R_i/R_V)$ (4):

$$
b^*_i=\tfrac12\,\mathrm{erfc}\!\left(\frac{\ln\hat s_{c,i}-\mu_i}{\sqrt2\,\sigma_\varepsilon}\right),\qquad
\hat s_{c,i}=\frac{\sigma_R+P_h}{p_i-P_h-2\gamma/R_i}\quad(b^*_i=0\ \text{if the denominator}\le0)
$$

For $\sigma_\varepsilon\to0$ it is the step $b^*_i=[p_i\ge P_{cr,i}]$: this is the mean-field pair of the code, where the rupture is progressive in pressure only through the spread of $p_i$ over the classes. The spread of $\hat s$ makes it progressive inside each class (OHP: the slow onset of the FGR curve is the fracture of the most interacting pores).

J19 breaks the HBS instantaneously (pore rupture "is assumed to cause immediate gas release"; Table 3 gives $\tau=0$ to the grain-boundary rupture) and relaxes the interconnection and microcracking modes of the grain faces with $\tau=2$ s. Each class is relaxed with the same first-order law **[?]**, as the intactness ODE of `GrainBoundaryMicroCracking.C`:

$$
\beta_i\leftarrow\beta_i+\max(b^*_i-\beta_i,0)\,\bigl(1-e^{-\Delta t/\tau}\bigr),\qquad \tau=2\ \text{s}
$$

The fractions of the output follow from the classes:

$$
D=\sum_iw_i\beta_i,\qquad G=\frac{\sum_iw_i\,n_i\,\beta_i}{\sum_iw_i\,n_i}
$$

$D$ is a number fraction, $G$ a gas fraction. A broken pore does not heal: $\beta_i$ does not decrease except by the dilution of 6, and $D$, $G$ are cumulative maxima. A class is identified by its node $x_i$: a pore keeps its rank in the size distribution while the moments evolve **[R]**.

#### 6. Dilution

With $\alpha$ the restructured volume fraction, new restructured material is intact, for every class **[R]**:

$$
\alpha>\alpha_{old}:\quad \beta_i\leftarrow\beta_i\,\frac{\alpha_{old}}{\alpha},\qquad
1-S\leftarrow(1-S)\,\frac{\alpha_{old}}{\alpha},\qquad 1-S_2\leftarrow(1-S_2)\,\frac{\alpha_{old}}{\alpha}
$$

The vented fractions $1-S$ and $1-S_2$ of 7 are sums over the broken classes, $\sum_iw_in_i\beta_i/\bar n$ and $\sum_iw_in_i^2\beta_i/\langle n^2\rangle$, and are diluted with them: on given classes $1-S=G$.

#### 7. Venting

A broken pore vents its gas and stays as a void. The gas leaves the broken classes only; the other classes keep their $n_i$. Per original pore, the retained first and second moments of $n$ are

$$
\bar n_{ret}=\sum_iw_i\,n_i\,(1-\beta_i),\qquad \langle n^2\rangle_{ret}=\sum_iw_i\,n_i^2\,(1-\beta_i)
$$

and their ratios to the unvented ones are the retained fractions $S=\bar n_{ret}/\bar n$ and $S_2=\langle n^2\rangle_{ret}/\langle n^2\rangle$. When the broken fractions grow in a step from $\beta_i$ to $\beta_i'$, with

$$
f_1=\frac{\sum_iw_in_i(1-\beta_i')}{\sum_iw_in_i(1-\beta_i)},\qquad f_2=\frac{\sum_iw_in_i^2(1-\beta_i')}{\sum_iw_in_i^2(1-\beta_i)}
$$

the state of the porosity model becomes ($A$ = `Xe in HBS pores`, $B$ its variance, $\bar n$ and $\mathrm{Var}(n)$ here the retained values)

$$
A\to f_1A,\quad \bar n\to f_1\bar n,\quad S\to f_1S,\quad S_2\to f_2S_2,\quad
\mathrm{Var}(n)\to f_2\bigl(\mathrm{Var}(n)+\bar n^2\bigr)-(f_1\bar n)^2,\quad B\to N_p\,\mathrm{Var}(n)
$$

and the gas leaves through `Xe released` by the mass balance of `GasRelease.C`. At the next step the classes are rebuilt from the unvented moments

$$
\bar n_{unv}=\frac{\bar n}{S},\qquad \langle n^2\rangle_{unv}=\frac{\mathrm{Var}(n)+\bar n^2}{S_2}
$$

**[R]**: between two steps the porosity model changes the retained moments, and the unvented ones follow in proportion. The state of the model is $\beta_i$ (81 values), $S$ and $S_2$, all written to `output.txt`: the classes of any step can be rebuilt from it. $1-S$ is the gas vented; $G$ is evaluated on the classes of the current step and differs from it when the gas of the classes changes after a rupture ($G=52.2$ % against $1-S=52.5$ % at the end of the NFIR anneal).

Which classes break first depends on $a$ (2). For $a<3$ the pressure falls with the radius, so the small pores break first and carry little gas ($n_i\propto R_i^{a}$): $D$ leads $G$. At the NFIR state heated at fixed gas to 1200 °C with $P_h=0$ (`section_heatup`), $P_{cr,i}=142$–$156$ MPa and $p_i=49$–$1092$ MPa: the classes below $R\approx0.40$ µm break, $D^*=67$ %, $G^*=49$ %. With $P_h=70$ MPa, $P_{cr,i}\approx690$ MPa and only the smallest classes break ($D^*=0.1$ %).

![rupture](figures/classes_rupture_nfir_j19damage.png)

> `figures/classes_rupture_nfir_j19damage.png` (`python3 pore_distribution.py`): the classes of the SCIANTIX run of the NFIR anneal, rebuilt from `output.txt` at the rows where $D$ is 25, 50 and 100 % of its final value. Pores and gas per µm of radius with their broken part (filled), pressure against critical pressure (dashed) with $2\gamma/R$ in grey, and $D$, $G$ on the heat-up.

![release](figures/classes_fgr_nfir_j19damage.png)

> `figures/classes_fgr_nfir_j19damage.png` (`python3 pore_distribution.py`): fission gas release of the NFIR anneal, in per cent of the gas produced (the release of the base irradiation is subtracted), against the time from the start of the transient, with the Kr-85 release measured in the test (Noirot 2014, Fig. 9).

#### 8. Fragment size

Cubic fragments bounded by the cracked faces of the Wigner–Seitz cells of the pores **[R] [U]**; the specific-surface idea follows Khvostov 2020 (Eq. 2–4):

$$
R_{WS}=\left(\frac{3}{4\pi N_p}\right)^{1/3},\qquad A_f=2\pi R_{WS}^2\ \text{[?]},\qquad S_v=N_p\,D\,A_f,\qquad
d=\min\!\left(\frac{3}{S_v},\,d_{max}\right)=\min\!\left(\frac{2R_{WS}}{D},\,500\ \mu\text{m}\right)
$$

$d=0$ if $D<10^{-9}$. $D\to1$ gives $d=2R_{WS}$, the cell diameter. $d_{max}=500$ µm is where pellet cracks take over (NEA/CSNI/R(2016)16, section 3). $3/S_v$ is the mean spacing of the cracked faces: it is a fragment size once the cracks connect, which the model does not check.

#### 9. Independent inputs and normalised pressure

The four SCIANTIX values $\bar V$ (or $R_V$), $\bar n$, $\mathrm{Var}(n)$, $T$ do not fix the sharing of the gas among the classes, so the closure exponent $a$ of 2 is a fifth input. Given $a$, $\mathrm{Var}(n)$ fixes the radius width $s$.

With $p^*=p\,v_{Xe}/(k_BT)$ the equation of state of 1 has no explicit $T$:

$$
p_i^*=\eta_iZ(\eta_i),\qquad \eta_i=\frac{n_iv_{Xe}(T)}{V_i}\le0.65,\qquad \bar\eta=\frac{\bar n\,v_{Xe}(T)}{\bar V},\qquad \mathrm{CV}_n=\frac{\sqrt{\mathrm{Var}(n)}}{\bar n}
$$

In the variable $x_i=R_i/R_V$, $p_i/\bar p$ and $w_i$ depend on three numbers only, $(\bar\eta,\mathrm{CV}_n,a)$. $R_V$ and $\bar p$ only rescale the axes; $\bar n$, $\bar V$ and $T$ enter only through $\bar\eta$ (verified to $10^{-14}$ by `section_normalised_vs_direct` of `pore_distribution.py`). For $a=2$ the dilute shape $x_i^{a-3}$ is not a usable approximation: the central classes deviate from it by 7 % at $\bar\eta=0.02$ and by 53 % at $\bar\eta=0.11$ (the NFIR state), and a linear interpolation of $\ln(p_i/\bar p)$ on a $6\times6$ grid of $(\mathrm{CV}_n,\bar\eta)$ is off by up to 75 %: the classes are built at the $(\mathrm{CV}_n,\bar\eta)$ of the state.

![coverage](figures/classes_coverage.png)

> `figures/classes_coverage.png` (`python3 pore_distribution.py`): the classes in the relative variables, $a=2$. Left: radius pdf for $\mathrm{CV}_n$ from 0.1 to 1.1 ($s$ from 0.05 to 0.45). Centre: $p_i/\bar p$ against $R_i/R_V$ for $\bar\eta$ from 0.02 to 0.23 at $\mathrm{CV}_n=0.7$. Right: the same for $\mathrm{CV}_n$ from 0.1 to 1.1 at $\bar\eta=0.09$. $\bar\eta$ changes the shape of the pressure; $\mathrm{CV}_n$ changes mostly the range of the radii.

The reduction stops at the gas pressure: the rupture criterion has absolute terms,

$$
P^*_{cr,i}=\frac{v_{Xe}}{k_BT}\Bigl[P_h+\frac{2\gamma}{R_i}+\frac{\sigma_R+P_h}{\hat s_i}\Bigr],
$$

so $D^*$ and $G^*$ depend also on $R_V$, $T$, $\sigma_R$ and $P_h$ separately. The universal curve describes the gas pressure of the classes, not the rupture fraction.

**Runs for MMM.** An RVE is scale-free in length and linear in the load, so $R_V$ and $\bar p$ only set the scale. With $a$ fixed and no confinement (4), the radii $R_k/R_V$, the porosities $\xi_k$ and the pressures $p_k/\bar p$ of the classes depend on three factors: $\xi$, $s$ (which gives $R_k/R_V$ and $\xi_k/\xi$) and $\bar\eta$ (which with $s$ gives $p_k/\bar p$). The SCIANTIX states of the NFIR case and the measured disc in these factors:

| state | $\xi$ | $s$ | $\bar\eta$ | $P_h/\bar p$ | $R_V$ (µm) | $T$ (K) | $\bar p$ (MPa) |
|---|---|---|---|---|---|---|---|
| SCIANTIX, end of base irradiation | 0.130 | 0.24 | 0.107 | 0.79 | 0.40 | 903 | 88 |
| SCIANTIX, anneal at 1200 °C | 0.123 | 0.25 | 0.105 | 0 | 0.38 | 1474 | 152 |
| NFIR disc, as irradiated **[P]** | 0.114 | 0.36 | 0.155 (903 K), 0.144 (1474 K) | | 0.73 | | |

The row of the disc is the log-normal fit of the bars of `data/PoreRadiusASIRRADIATED.txt` (Noirot 2014, Fig. 12) (porosity per 0.2 µm of pore diameter, from 0 to 4 µm), with $\bar\eta$ from the measured volume per gas atom, $1.518\times10^{-28}$ m³ (`section_nfir_data` of `pore_distribution.py`; after the anneal the fit gives 0.126, 0.38, 0.75 µm). The modelled pores have two thirds of the measured width, 0.55 times the measured radius and 0.7 times the measured gas density (L11).

`python3 pore_distribution.py` writes a Latin hypercube of 20 runs (`MMM_RUNS`, with `MMM_CLASSES` and `MMM_SEED`, in its "Model choices") in the ranges **[?]**

| factor | range | SCIANTIX cases | NFIR disc | bounds |
|---|---|---|---|---|
| $\xi$ | 0.02–0.18 | up to 0.133 (NFIR case), 0.165–0.196 (`regression/hbs`) | 0.114 | upper: $\xi_{sat}$ of `HighBurnupStructurePorosity.C`. Lower: the cube of the RVE holds two pores of each class at the ends; below it $\hat s$ tends to that of the isolated pore, 1/2 |
| $s$ | 0.05–0.42 | 0.05–0.42 ($\mathrm{CV}_n$ 0.10–1.00) | 0.36 | K15: 0.356 |
| $\bar\eta$ | 0.045–0.23 | 0.034–0.228 (0.048–0.228 for $\xi\ge0.02$) | 0.144 (1474 K) to 0.183 (293 K) | |

The column of the SCIANTIX cases is the envelope of the rows inside the validity window of the criterion in the outputs of `regression/hbs` (`test_UO2HBS`, `test_UO2HBS_dislocation`, `test_UO2HBS_landau`: 723 K, up to 178–202 MWd/kgUO2) and of the NFIR case. The $\bar\eta$ below 0.045 are NFIR rows with $\xi<0.02$, below the range of the runs. In these cases the wide distributions are the dilute ones ($s>0.33$ with $\bar\eta<0.11$) and the dense ones are narrow ($\bar\eta>0.15$ with $s<0.18$).

![gas volume](figures/mmm_gas_volume.png)

> `figures/mmm_gas_volume.png` (`python3 pore_distribution.py`): volume per gas atom at the temperature of the reference, the end of the NFIR base irradiation (903 K). Left: the measured values against the local burnup, the series of Noirot 2014 Fig. 17 (UO2 and MOX periphery, Pu-rich HBS agglomerates at mid-radius of MOX, the NFIR disc), the rim rows of Dowek 2021 Table 1 (Horvath 2007, Noirot 2009) and every value of Dowek 2021 (means of sample B along the radius, Table 3; single bubbles of samples A and B, Tables 4–5) at the average burnup of its sample (Table 2), filled at the rim; the SCIANTIX states of the NFIR case; the blue band is the range of the runs, $v_{Xe}/\bar\eta$. Right: the classes of the NFIR disc, of the SCIANTIX state at the end of the base irradiation and of every run, area ∝ share of the gas, with the mean $\bar V/\bar n$; grey: the range of the measured values in the HBS (rim, mid-radius agglomerates, NFIR disc; Dowek 2021 out of the rim excluded).

The range of every factor is cut in 20 strata and every stratum is used by one run, at its centre: each factor takes 20 equally spaced levels, whatever the number of factors. The levels of the three factors are paired by `scipy.stats.qmc.LatinHypercube` with `optimization="random-cd"`, which lowers the centred discrepancy of the points (seed 1) **[N]**.

The reference of the MMM geometries is the NFIR disc before the anneal **[R]**: `mmm_inputs/run_NFIR.txt` is the log-normal fit of its measured pores ($\xi=0.114$, $s=0.36$, $R_V=0.73$ µm) with the measured gas density ($\bar\eta=0.155$ at 903 K), and the runs of the hypercube take its $R_V$ and its temperature. Each run is a file of `mmm_inputs/` with one row per class (the 81 nodes grouped in 7 classes that keep $\bar V$ and $\bar n$, hence $\xi$ and $\bar p$): radius (µm), porosity $\xi_k=\xi\,w_kV_k/\bar V$, gas pressure (MPa), confining pressure (MPa, zero in every run). `index.txt` lists the factors of the runs and `eta_max`, the largest packing fraction of their classes. In the corner of wide and dense distributions ($s>0.33$ with $\bar\eta>0.13$–0.2) the smallest class is at the cap $\eta=0.65$ of 1: runs R04 and R05, where its pressure is 52 and 94 times the mean. No SCIANTIX state is in that corner. No SCIANTIX run is needed.

![runs](figures/mmm_runs.png)

The scale of the files is the measured one **[?]**: $R_V=0.73$ µm of the disc, and the temperature of the end of the base irradiation, 903 K, which only sets $\bar p$ (160 MPa for the disc). $\hat s$ does not depend on $R_V$; the scale matters because the lengths of `Merope/distributedpore.py` are in µm. The side of the cube (30 µm, 41 $R_V$), the repulsion distance (0.3 µm, 0.41 $R_V$) and the mesh size (0.15 µm, 0.2 $R_V$) are those of the RVE of OHP, which has pores of 1.75 µm in diameter (repulsion distance 0.34 radii). The class radii of the runs go from 0.18 to 1.85 µm (diameters 0.4–3.7 µm, the range of the bars) and the number of pores, which follows from the porosities of the classes, from 390 at $\xi=0.024$ to 2900 at $\xi=0.176$, with 2 to 18 pores in each class at the ends.

![pore radius](figures/mmm_pore_radius.png)

> `figures/mmm_pore_radius.png` (`python3 pore_distribution.py`): pore radius. Left: porosity per 0.1 µm of radius of the NFIR disc as irradiated (bars) and after the anneal (line) (Noirot 2014, Fig. 12), the log-normal fit of the disc and the log-normal of the SCIANTIX state at the end of the base irradiation; blue band: the radii of the classes of the runs. Right: the classes of the disc, of the SCIANTIX state and of every run, area ∝ share of the porosity, with $R_V$; grey: 90 % of the measured porosity.

From each run the model needs, for every pore of the RVE, its class and $\sigma_I^{max}$ in the surrounding matrix under the pressures $p_k$ of the file, at zero macroscopic stress (the rescaling of 4 from a run at zero macroscopic strain holds for one pressure in all the pores only **[?]**). The factor of the criterion is $\hat s=\sigma_I^{max}/p_k$; its mean and scatter per class are the $c_0$, $c_1$, $\sigma_\varepsilon$ of 4. The capillary pressure $2\gamma/R_k$ and the confinement $P_h$ are in the criterion and are not loads of MMM.

**Size of the RVE.** The cube of OHP is checked there on the FGR curve of pores of one size, with ten seeds. For the classes, `Merope/test_rve.py` writes the cubes and `Merope/analyse_rve.py`, once MMM has solved them, applies the statistical procedure of Kanit 2003 (Sections 5.2 and 6) to $Z_k$, the mean of $\hat s$ over the pores of class $k$ in a cube **[R]** (Kanit takes an elastic modulus of a two-phase material). The variance of $Z_k$ between cubes of volume $V$ is $D_Z^2(V)=S^2A/V$ (Eq. 62, for a quantity additive over $V$ as this mean is), with $A$ the integral range and $S^2$ the point variance, here the variance of $\hat s$ between the pores of the class **[R]** (Eq. 64 gives it for two phases, $p(1-p)(Z_1-Z_2)^2$); $A$ is estimated from the cubes, $D_Z^2V/S^2$, as Kanit does for the volume fraction (Eq. 61, Fig. 9). $n$ cubes give the mean within $2D_Z(V)/\sqrt n$ (Eq. 68), $\varepsilon\bar Z_k$ for the relative precision $\varepsilon$ (Eq. 69, relative to the mean of the class **[R]**); the RVE is the volume for which one cube is enough, $n=1$ (Section 6), $4S^2A/(\varepsilon\bar Z_k)^2$ from Eq. 70 with Eq. 62, the form of Eq. 72. The exponent $\alpha$ of $D_Z^2\sim V^{-\alpha}$ is Kanit's power law for non-additive moduli (Eq. 63): 1 is expected here. For independent pores $A$ is the volume per pore of the class, and the mean needs $4\,\mathrm{CV}^2/\varepsilon^2$ pores of the class, $\mathrm{CV}$ being the scatter of $\hat s$ between them: 64 pores at 5 % if the scatter is 20 %. The check is on the reference run NFIR, the disc before the anneal and not the SCIANTIX state (radii of the classes 0.22–1.70 µm, 1870 pores in the cube, 11.6 in the rarest classes; 3.4 in a cube of 20 µm, so every class is in every cube of the exploration): 10 cubes of 30 µm, the cube of the runs, loaded by one unit pressure in all the pores as in OHP **[R]**. Their scatter answers the question directly: one cube gives the class mean within $2D_Z/\bar Z_k$ (Eq. 68–69 with $n=1$), and the cube is an RVE for the class if that is below 5 %. Otherwise it returns the number of cubes needed (Eq. 70) and, from $A$, the side of the RVE. An optional exploration adds 10 cubes of 20 µm, a second variance for $A$ and the exponent $\alpha$ (known to about ±0.55 between 20 and 30 µm with 10 cubes each; 1 is expected, Eq. 62), and 2 cubes of 40 µm, to check that the 30 µm mean has no bias, which the scatter of one side cannot show. The same script tests the mesh on one pore of the mean size of the disc, $R_V=0.73$ µm, in a periodic cube of 2.43 µm that gives it the porosity of the disc **[R]**, meshed at 0.30, 0.15 and 0.075 µm: $\hat s$ on 0.15 µm, the mesh of the runs, must be within 5 % of the finest **[?]**. It is the mesh density study of Kanit 2003 (Section 4.1.2): one geometry at several resolutions, converged when the apparent modulus moves by less than 1 % (50 quadratic elements per grain); the density he retains gives better than 5 %, the tolerance taken here.

### Implementation

- `src/models/HighBurnupStructureFragmentation.C`, case 4. The state $\beta_i$ is in `Sciantix_variables[213..293]` (`HBFF_CLASS_OFFSET`, `HBFF_N_CLASSES` of `MainVariables.h`), $S_2$ in `[212]`; they are written to `output.txt` only with this option (82 columns).
- `pore_distribution.py` is the Python transcription (`build`, `broken`, `rupture_step`). `python3 pore_distribution.py` replays the recursion on the history of the NFIR case (`NFIR_CASE`) and compares $D$, $G$, $S$, $S_2$ and every $\beta_i$ with `output.txt`: on `regression/hbs_nfir/test_UO2HBS_frag_j19damage` the largest differences over the 10 429 rows are $2\times10^{-7}$ (the state in `output.txt` has 7 digits; the check fails above $10^{-6}$). It also compares the two states that this file quotes (`STATES`) with the output, and fails if they differ by more than $10^{-3}$. The same run draws the figures of this file, the rupture and release figures of the NFIR case included. The checks quoted here that the run does not repeat (`section_truncation`, `section_closure_data`, `section_heatup`, `section_normalised_vs_direct`, with the self-check of `build` on the K15 width, the cumulative moments and the scans of the conserved moments) are kept, not called, at the end of the file.
- Options 1, 2, 3 do not use the classes: their NFIR outputs are identical byte by byte with and without this option in the build.
- `./utilities/HBFF/runHBFF.sh` does the whole sequence for this option: build, NFIR case against its gold file, check against the Python transcription, figures of this file and of `regression/hbs_nfir/figures/j19damage`, inputs for MMM.

### Results

`regression/hbs_nfir/test_UO2HBS_frag_j19damage` (formation 1, porosity 3), $a=2$, mean-field pair. No parameter is fitted. The IFA-650 cases are not run with these parameters.

| case | result | data |
|---|---|---|
| NFIR anneal to 1200 °C, $P_h=0$ | $D=0.14$ % at the start of the anneal; release 1.0 % of the gas produced at 300 °C, 7.3 % at 600 °C, 23.7 % at 900 °C, 29.5 % at 1000 °C, 35.6 % at 1100 °C, 41.9 % from 1170 °C; at 1200 °C $D=71$ %, $G=52$ %, $d=2.2$ µm | Kr-85 release 0.2 % at 900 °C, 1.9 % at 1000 °C, 7.1 % at 1100 °C, 12.8 % at 1150 °C, 29.8 % at 1200 °C; fragments below 100 µm |
| NFIR base irradiation, $P_h$ from 0 to 70 MPa | first class breaks at 27 340 h ($P_h=50$ MPa); $D=0.01$ % at the end | – |

The measured release starts at 880 °C, has its peaks at 1036 and 1180 °C, stops during the hold at 1200 °C and amounts to about one third of the gas of the HBS bubbles (SoA: Noirot 2014; the first three are in `regression/hbs_nfir/data`). The model has $G=52$ % and, like the test, no release during the hold. The fragments of heating tests are typically 20–200 µm (SoA: J20): $d=2.2$ µm is a spacing of cracks, not a fragment size (8).

Pore pressure of the NFIR disc, inferred from the measured gas density ($6.6\times10^{27}$ m$^{-3}$) with a real-gas equation of state, against the hard-sphere equation of state of the code at the same density and against the mean pressure of the modelled pores (density $4.5\times10^{27}$ m$^{-3}$):

| temperature | inferred from the measurement | hard spheres, measured density | model, mean pore |
|---|---|---|---|
| 20 °C | about 11 MPa (Noirot 2014; 7–13 MPa, SoA: Dowek 2021) | 59 MPa | 31 MPa |
| 700 °C | 133 MPa (Noirot 2014) | 171 MPa | 95 MPa |
| 1190 °C | 245 MPa (OHP) | 247 MPa | 139 MPa |

Two errors of opposite sign: the modelled gas density is 0.7 times the measured one, which lowers the pressure at 1190 °C by 44 %, and the hard-sphere equation of state has no attractive term, which overestimates the pressure at low temperature (Limitations, L3).

Sensitivity to the closure exponent at the NFIR state heated at fixed gas to 1200 °C ($P_h=0$): $a=2$ $D^*=67$ %, $G^*=49$ %; $a=2.25$ 64 %, 45 %; $a=2.5$ 60 %, 41 %; $a=2.75$ 40 %, 23 %; $a=3$ no rupture.

The NFIR release at 1200 °C is 1.4 times the measured one, and its shape differs: the model releases from room temperature, one class after the other as the temperature rises, while the measured release starts at about 870 °C and takes place for more than half above 1150 °C. Three things follow from the closure with $a=2$ on the hard-sphere equation of state: the smallest classes are above the critical pressure already in the base irradiation ($D=0.01$ %) and when the confinement is released at room temperature ($D=0.14$ %); the classes break in order of radius over the whole heat-up, down to a pressure of about 1.05 times the mean; and the classes above 0.40 µm, which hold 48 % of the gas, stay below the critical pressure. The mean pressure of the modelled pores at 1200 °C is 152 MPa, against 245 MPa from the measured gas density (OHP), with a critical pressure of 152–166 MPa: the pores of the porosity model are 13 MPa above the mechanical equilibrium $P_h+2\gamma/R$ of the base irradiation (88.5 MPa at 903 K against 75.5 MPa), those of the disc hold 1.5 times the gas density (about 160 MPa at 903 K).

### Parameters

| symbol | value | tag | source |
|---|---|---|---|
| $\hat s$ | $\xi/(1-\xi)$ | [P] | J19 Eq. 22 (mean field); to be replaced by the MMM tables with their $\sigma_R$ |
| $\sigma_R=\sigma^{cr}_{hbs}$ | 21 MPa | [P] | J19 Table 4 (calibrated by Jernkvist on IFA-650 and annealing data, not here) |
| $\sigma_R$ of the MMM pair | 275 MPa | [P] | OHP D5.2 section 2.2.2 (calibrated on the NFIR-V FGR curve with the pressure from the measured gas density); not in the code |
| $c_0,c_1,\sigma_\varepsilon$ | $f(\xi,\mathrm{CV}_n,\bar\eta,P_h/\bar p)$ | [?] | MMM tables, to be computed; in the code $c_0=\ln[\xi/(1-\xi)]$, $c_1=0$, $\sigma_\varepsilon=0$ |
| closure exponent $a$ | 2 | [?] | gas density $\propto1/R$ (2); 3 is a uniform pressure |
| $\xi_{perc}$ | 0.29 | [P] [E] | J19 section 2.3.2 |
| $\tau$ | 2 s | [?] | J19 Table 3, interconnection and microcracking modes |
| $\gamma$ | 1.1 N/m | [R] | `SetMatrix.C`, UO2HBS |
| hard-sphere EoS, $\eta_{max}$ | $d_{Xe}(T)$, 0.65 | [R] | `HighBurnupStructurePorosity.C` |
| classes | 81, $\pm4s$ | [N] | K15 Eq. 6 |
| $A_f$, $d_{max}$ | $2\pi R_{WS}^2$, 500 µm | [?] | none; $d_{max}$ from NEA §3 |
| validity window | $R\ge0.1$ µm, $\xi<0.35$ | [?] | on the mean pore |

### Limitations

The labels are those of the register in the header of `src/models/HighBurnupStructureFragmentation.C`; L16 is split here.

- **L16a [?]** the closure $n_i\propto R_i^a$ is a modelling choice, not a measured distribution; $a$ is not determined by the SCIANTIX moments and sets which classes break and how much gas they hold (NEA reports Hiernaut's estimate that large and small pores have similar pressure, which is $a=3$; the punching shape of 2 is $a=2$). Pressures measured on single pores differ by a factor of several within a sample: 41 and 9 MPa at room temperature for pores of 2.2 and 3.0 µm in diameter, 9–25 MPa for four pores of 0.7–1.3 µm (SoA: Dowek 2021). The pressure is not uniform, and data of this kind can constrain the closure: in the two pairs measured at one position the smaller bubble is the denser, local $a$ of 0.6 (rim) and 1.5 (centre, not HBS), and the spread at nearly equal radius is larger than the closure gives (2). With $a=2$ and the hard-sphere equation of state the smallest classes are above the punching limit (2) and above the critical pressure in the base irradiation and at the release of the confinement (*Results*).
- **L16b [?]** the stress concentration factor in the code is the mean-field one: porosity only, the same for all the classes, no scatter. The MMM factor exists for monodisperse pores at uniform pressure (OHP); its dependence on $R_i/R_V$, $\xi$, $\mathrm{CV}_n$ and $\bar\eta$ needs the runs of 9. The two pairs differ in the effect of $P_h$ (4).
- **[R]** the runs for MMM are unconfined: the same $\hat s_i$ is used under confinement, where the net pressures of the classes are in other ratios (4).
- **[?]** the lengths of the RVE of the runs (side of the cube, repulsion distance, mesh size) are fixed in µm, while the stress factor depends on their ratio to the pore size. The RVE of OHP has a cube of 17.1 pore diameters and a mesh size of 0.086 diameters; the runs have 8.0–17.7 diameters of the largest class and 0.12–0.42 of the smallest, the mesh size being one for all the classes. `Merope/distributedpore.py` flags the two ratios in `summary_<run>.txt`, with the porosity placed against the one asked (Mérope places the integer part of the expected number of pores of a class: 2 for 2.6). Whether one cube is enough for the factor of every class, and whether the mesh of 0.15 µm is fine enough, on a pore of the mean size of the disc (mesh size 0.10 of its diameter; 0.34 for the smallest class of the disc, not tested), are the two tests of `Merope/test_rve.py` and `Merope/analyse_rve.py` (9).
- **[?]** the repulsion distance (0.3 µm between the surfaces of two pores: 0.41 $R_V$ at the measured $R_V$ of the runs, 0.75 $R_V$ at the modelled one, 0.40 µm) is the assumption of OHP that one HBS grain lies between two pores. OHP has no sensitivity study of it, and the factors of the closest pores depend on it. The smallest gap obtained is in `summary_<run>.txt`.
- **[?]** `test-bubble`, the solver of the RVE of OHP, reads one pressure for all the pores and takes the maximum of a pore within one distance of its centre: the pressures and the radii of the classes need a pressure and a distance per pore. In the rescaling of 4, $p_{hyd}$ is the compression of the whole cube, pores included: $p_{hyd}=\xi-(1-\xi)\langle\sigma_h\rangle$ per unit pressure, with $\langle\sigma_h\rangle$ the average over the matrix that the solver writes. OHP does not say which of the two it uses; for a hollow sphere at $\xi=0.11$ the factor is 0.69 with the first, the exact one, and 0.5 with the average over the matrix.
- **L16c [?]** $\tau=2$ s is borrowed; J19 rupture is instantaneous.
- **L16d [?]** the interaction of the pores is that of the intact population; after a break the pressure of the broken pore is not removed from the load of its neighbours (cascade not modelled), and vented pores stay voids.
- **L16e [R]** the classes are rebuilt each step from the unvented moments $\bar n/S$ and $(\mathrm{Var}(n)+\bar n^2)/S_2$; a pore keeps its rank in the size distribution; new restructured material dilutes all the classes alike, with their vented fractions (6). The proportionality holds for the venting itself, not for what the porosity model does afterwards: gas trapped after a partial venting shifts the retained mean and lowers the rebuilt variance. On the NFIR anneal ($1-S=52$ %) the rebuilt $\mathrm{CV}_n$ moves from 0.50 to 0.53.
- **L8 [?]** the porosity model evolves the retained mean gas: after a partial venting the mean pore volume falls (the gas volume of the vented pores is removed, the pores relax by vacancy emission) and the gas trapped afterwards is attributed to the intact pores through $\bar n/S$. Both raise the pressure of the intact classes. On the NFIR anneal $\bar V$ falls by 10 % between 300 and 1150 °C.
- **L3 [R]** the hard-sphere equation of state has no attractive term. Xenon is close to its critical point at room temperature (290 K, 5.8 MPa; critical density about $5\times10^{27}$ m$^{-3}$, that of the pores): at the gas density measured in the NFIR pores the hard-sphere pressure is 5 times the one inferred with a real-gas equation of state at 20 °C, 1.3 times at 700 °C and equal to it at 1190 °C (table of *Results*). The pressures measured in rim pores at room temperature are 7–20 MPa (SoA: Dowek 2021). At the gas density of six single bubbles of Dowek 2021 the hard-sphere pressure is 4–8 times the Soave one at 293 K, 1.5–1.7 times at the rim temperature (653 K), 1.3 times at 1113 K and 1.0–1.1 times at 2151 K (Tables 4–5; temperatures of irradiation of the positions from Table 2: 380 °C at the rim, 840 °C and 1878 °C at the centre of samples B and A). The rupture of the classes at low temperature, hence the release from room temperature of the NFIR case, is overestimated by it; at the temperatures of the measured release the error is small.
- **L9 [?]** the validity window is on the mean pore; the classes extend below 0.1 µm.
- **L15 [?]** no dependence on the heating rate except the relaxation time; higher heating rates give more release and smaller fragments (NEA16; SoA: J20), and the empirical thresholds do not resolve it either.
- **[?]** $\sigma_R$ of either pair has no direct physical meaning: it absorbs the toughness and the spherical idealisation of the pores (OHP), or the stress concentration that the ligament balance ignores (J19).
- **L11 [R]** the criteria act on the modelled pore state (formation 1, porosity 3: 13.0 % porosity, 0.40 µm, gas density $4.5\times10^{27}$ m$^{-3}$ at the end of the NFIR base irradiation), not on the measured one (11 %, 0.88 µm, $6.6\times10^{27}$ m$^{-3}$).
- **L13 [?]** the confining stress of the base irradiation is an input and changes the pore state through the equilibrium pressure of the porosity model.
- **L5 [?]** no lower bound on the fragment size, and $d$ is a crack spacing (8).
- **[N]** the tails beyond $\pm4s$ are dropped; the fraction outside is $1\times10^{-3}$ for the weight of $\mathrm{Var}(n)$ at the NFIR state (`section_truncation` of `pore_distribution.py`). $D$ and $G$ move in steps of one class: up to 4 % of the pores for a class near the median, 3.2 % of the gas produced in the NFIR release curve.
- Cost: 81 classes and one root of $s$ per step (NFIR case, 10 428 steps: about 10 s).

### Design choices

| idea | verdict |
|---|---|
| pressure from the gas in the pore through the equation of state | adopted on every class; the joint $(n_i,V_i)$ is not in the SCIANTIX state (only $\bar n$, $\mathrm{Var}(n)$, $\bar V$), so the closure $n_i\propto R_i^{a}$ is needed |
| fracture mechanics with a crack length $c=\lambda N_p^{-1/3}$ | not adopted: only $K_{Ic}/\sqrt\lambda$ enters (one parameter again) and the local $K_{Ic}$ is missing (J20: the bulk $G$ is about 100 times too strong) |
| crack growth $\mathrm{d}c/\mathrm{d}t=C(K/K_c-1)^m$ | only a relaxation time is used; $C$, $m$ have no data and the release curves at 0.2 and 20 °C/s are not digitised |

### Literature

| key | reference | used for |
|---|---|---|
| J19 | L.O. Jernkvist, *Modelling of fine fragmentation and fission gas release of UO2 fuel in accident conditions*, EPJ Nucl. Sci. Technol. 5 (2019) 11, doi:10.1051/epjn/2019030 | Eq. 22 (rupture), Eq. 21 (punching limit), Table 3 ($\tau$), Table 4 ($\sigma^{cr}_{hbs}$, $c_{dp}$), percolation 0.29 |
| J20 | L.O. Jernkvist, *A review of analytical criteria for fission gas induced fragmentation of oxide fuel in accident conditions*, Prog. Nucl. Energy 119 (2020) 103188, doi:10.1016/j.pnucene.2019.103188 | Eq. 7 (ligament force balance, origin of J19 Eq. 22) |
| K15 | K. Kulacsy, J. Nucl. Mater. 466 (2015) 409 | log-normal pore size distribution (Eq. 6, Tables 1–2) |
| Lem00 | F. Lemoine, J. Papin, J.M. Frizonnet, B. Cazalis, H. Rigat, in *Fission Gas Behaviour in Water Reactor Fuels*, OECD/NEA Seminar, Cadarache 2000, pp. 175–187 | rupture criterion of J19 [22] |
| KH20 | G. Khvostov, *Analytical criteria for fuel fragmentation and burst FGR during a LOCA*, Nucl. Eng. Technol. 52 (2020) 2402 | fragment size from the specific surface of the cracks |
| KH18 | G. Khvostov, *Models for numerical simulation of burst FGR in fuel rods under the conditions of RIA*, Nucl. Eng. Des. 328 (2018) 36 | fracture ratio of the grain boundary in RIA |
| NEA16 | NEA/CSNI/R(2016)16, *Report on Fuel Fragmentation, Relocation and Dispersal (FFRD)* | fragment morphology and size, $d_{max}$, heating-rate dependence |
| OHP | OperaHPC Deliverable D5.2 (2026), section 2.2 | NFIR annealing test, Kr-85 release used as reference; MMM RVE, $\sigma_R$, stress concentration factor $\hat s$ |
| CS69 | N.F. Carnahan, K.E. Starling, J. Chem. Phys. 51 (1969) 635 | hard-sphere equation of state |
| Aagesen 2021 | L.K. Aagesen et al., *Phase-field simulations of fission gas bubbles in high burnup UO2 during steady-state and LOCA transient conditions*, J. Nucl. Mater. 557 (2021) 153267 | pore radius constant and pressure linear in $T$ in a LOCA ramp |
| Noirot 2014 | J. Noirot et al., *Fission gas release behaviour of a 103 GWd/tHM fuel disc during a 1200 °C annealing test*, J. Nucl. Mater. 446 (2014) 163 | NFIR disc: release curve, pore sizes, pore pressure (data in `regression/hbs_nfir/data`) |
| Dowek 2021 | R. Dowek et al., *Determination of the pressure in micrometric bubbles in irradiated nuclear fuels*, J. Nucl. Mater. 543 (2021) 152591 (SoA) | pore pressures at room temperature, single-pore values; Tables 1–5 in `data/` (`Dowek2021_T2`–`T5.txt`, `PorePressureCOLLECTION.txt`): molar volumes, single-bubble pressures (Soave) and radii, against the closure (2) and the equation of state (L3), and the gas volumes of samples A and B in `figures/mmm_gas_volume.png` |
| Cappia 2016 | F. Cappia et al., *Critical assessment of the pore size distribution in the rim region of high burnup UO2 fuels*, J. Nucl. Mater. 480 (2016) 138 (SoA) | range of validity of the log-normal pore size distribution |
| Capps 2021 | N. Capps et al., *A critical review of high burnup fuel fragmentation, relocation, and dispersal under loss-of-coolant accident conditions*, J. Nucl. Mater. 546 (2021) 152750 (SoA) | empirical thresholds and what they do not resolve |
| Kanit 2003 | T. Kanit et al., *Determination of the size of the representative volume element for random composites: statistical and numerical approach*, Int. J. Solids Struct. 40 (2003) 3647 | size of the RVE, Sections 5.2 and 6: variance against volume and integral range (Eq. 61–62), power law and point variance of two phases (Eq. 63–64, fit Eq. 66), precision against number of realisations (Eq. 68–70), RVE of the volume fraction (Eq. 71–72); mesh density, Section 4.1.2 |

PDFs are in `letteratura/fragm/`. J19 Eq. 21–22 and Tables 3–4, OHP section 2.2, the declared limitations of K15 and the two statements of Aagesen 2021 are checked against the PDFs; the entries marked (SoA) are not in that folder. Kanit 2003 is checked on its PDF, `utilities/HBFF/464-kanit03.pdf` (the open copy from the Mines Paris site). El Moumen, Kanit and Imad, Eur. J. Mech. A/Solids 86 (2021) 104181 (`utilities/HBFF/1-s2.0-S099775382030560X-main.pdf`) only restates Eq. 62, 64, 68–70 of Kanit 2003 and is not followed.
