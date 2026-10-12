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

![pore EoS](figures/pore_eos.png)

> `figures/pore_eos.png` (`python3 pore_distribution.py`): $p_g$ against $n$ for $R=0.35$ µm at 600, 1000 and 1500 K. The solid lines are the hard-sphere EoS, the dashed lines the ideal gas, the dots the cap $\eta=0.65$ beyond which $p_g$ is linear in $n$. Already at $n\approx2\times10^{9}$ the hard-sphere pressure is about 3 times the ideal one, and it grows steeply up to the cap.

![pore EoS vs volume](figures/pore_eos_volume.png)

> `figures/pore_eos_volume.png` (`python3 pore_distribution.py`): $p_g$ against $V_p$ at fixed gas ($n=1,2,4\times10^{9}$) and $T=1000$ K, same conventions. At fixed $n$ the cap sets a smallest volume $V_p=n\,v_{Xe}/0.65$ (the dots); the dotted line is the volume of the $R=0.35$ µm pore. The pressure is $\propto1/V_p$ for the ideal gas, steeper than $1/V_p$ for the hard-sphere gas, and again $\propto1/V_p$ below the cap.

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

so $\sum_iw_in_i=\bar n$. The closure fixes the gas density of the classes, $n_i/V_i\propto R_i^{a-3}$. $a=2$, the value of the code, is a density $\propto1/R$: for an ideal gas a pressure $\propto1/R$, the shape of the punching overpressure of K15 and J19. $a=3$ is a uniform density and pressure. Two things separate the pressure of the classes from a $1/R$ overpressure. The hard-sphere pressure is the ideal one times $Z(\eta_i)$, and $\eta_i\propto1/R$ too: at the NFIR state $Z$ goes from 1.1 in the largest class to 10.7 in the smallest, and $p_i$ falls as $R^{-1.1}$, $R^{-1.5}$ and $R^{-4}$ at the largest class, at $R_V$ and at the smallest class. And it is the gas pressure that follows the closure, not the overpressure: $p_i-P_h-2\gamma/R_i$ is negative for the classes above 0.38 µm (23 % of the pores) at the 70 MPa of the end of the base irradiation.

The width $s$ is the root of $\sum_iw_i(n_i-\bar n)^2=\mathrm{Var}(n)$ on the 81 truncated nodes: the coefficient of variation of $n$ on the nodes depends on $s$ and $a$ only and grows with $s$, so the root is bracketed between 0 and the value of the continuous log-normal, $s_0=\sqrt{\ln(1+\mathrm{Var}(n)/\bar n^2)}/a$, doubled until it is above the root. For $\mathrm{Var}(n)=0$, $s=0$ and all the classes coincide with the mean pore. The three moments $\bar V$, $\bar n$, $\mathrm{Var}(n)$ hold on the classes to $10^{-8}$ (`check_moments`).

![classes](figures/classes_distributions.png)

> `figures/classes_distributions.png` (`python3 pore_distribution.py`): the classes of the SCIANTIX state at the end of the NFIR base irradiation ($\bar V=1.825\times10^{-19}$ m³, $\bar n=7.23\times10^{8}$, $\mathrm{CV}_n=0.75$, 903 K): radius pdf, pressure and gas per class. Grey: the capillary pressure $2\gamma/R$, and ten times it (dashed) to compare the shapes. Dashed orange: the pressure of the same gas as an ideal gas, $n_ik_BT/V_i$, which is the $1/R$ shape of $a=2$; the hard-sphere pressure $p_i$ leaves it towards the small classes (2.7 GPa against 0.25 GPa at 0.08 µm).

At this state $s=0.33$ ($a=2$; 0.27 for $a=2.5$, 0.22 for $a=3$), against about 0.3 measured on the NFIR disc (OHP Fig. 4) and 0.356 of K15: the variance of $n$ that SCIANTIX carries gives a realistic width. The radii go from 0.08 to 1.1 µm. A log-normal describes the measured pore size distributions for local burnups of 80–150 GWd/tHM; above about 150 GWd/tHM they become long-tailed and multi-modal, with pore coarsening from about 100 GWd/tHM (SoA: Cappia 2016). The NFIR disc (103 GWd/tHM) is inside this range.

**Consistency with the base irradiation.** A class above the punching limit of J19 Eq. 21, $P_h+2\gamma/R+c_{dp}/R$ ($c_{dp}=55$ N/m), would grow by dislocation punching in steady state. With $a=2$ the hard-sphere pressure of the smallest classes rises above it: at the NFIR state 0.9 % of the pores, up to 2.7 GPa at 0.08 µm (for $a\ge2.25$ no class is above it).

![closure exponent](figures/classes_closure_exponent.png)

> `figures/classes_closure_exponent.png`: pressure per class for $a$ from 3 to 2 at the same moments, with $2\gamma/R$ (grey) and the punching limit (dashed).

#### 3. Pressure per class
The equation of state of 1 on every class, $p_i=p_g(n_i,V_i,T)$, with the packing fraction of the class $\eta_i=\min(n_iv_{Xe}/V_i,\,0.65)$.

The mean pressure $\bar p=p_{\rm EoS}(\bar n,\bar V,T)$ is the pressure of the volume-weighted mean density $\sum_iw_in_i/\sum_iw_iV_i=\bar n/\bar V$ and holds because $\bar n$ and $\bar V$ are conserved. The number-weighted mean of $p_i$ is higher (134 against 73 MPa at the NFIR state) and is not a conserved quantity.

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

With a pressure that depends on the radius, the stress field is the superposition of the fields of the single pores loaded by their net pressure $p_j-P_h$. The ratios of the net pressures of the classes depend on the pressure ratios $p_j/p_i$, fixed by $(\bar\eta,\mathrm{CV}_n,a)$ (9), and on $P_h/\bar p$: a uniform $P_h$ is a rescaling only for a uniform pore pressure ($a=3$, the OHP case) or for $P_h=0$. So $(c_0,c_1,\sigma_\varepsilon)$ are tabulated against $(\xi,\mathrm{CV}_n,\bar\eta)$ and $P_h/\bar p$. The maximum principal stress is not linear in the loads: it has to be taken on the superposed field, so every run has its own pressures and its own $P_h$ (9).

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

**[R]**: between two steps the porosity model changes the retained moments, and the unvented ones follow in proportion. The state of the model is $\beta_i$ (81 values), $S$ and $S_2$, all written to `output.txt`: the classes of any step can be rebuilt from it. $1-S$ is the gas vented; $G$ is evaluated on the classes of the current step and differs from it when the gas of the classes changes after a rupture (20.7 % against 20.6 % at the end of the NFIR anneal).

Which classes break first depends on $a$ (2). For $a<3$ the pressure falls with the radius, so the small pores break first and carry little gas ($n_i\propto R_i^{a}$): $D$ leads $G$. At the NFIR state heated at fixed gas to 1200 °C with $P_h=0$, $P_{cr,i}=180$–$207$ MPa and $p_i=32$–$3500$ MPa: the classes below $R\approx0.28$ µm break, $D^*=44$ %, $G^*=21$ %. With $P_h=70$ MPa, $P_{cr,i}\approx850$ MPa and only the smallest classes break ($D^*=0.5$ %).

![heat-up](figures/classes_heatup.png)

> `figures/classes_heatup.png`: $D^*$ and $G^*$ of the NFIR state heated at fixed gas, for $a$ from 3 to 2. The curves are staircases, one step per class.

![rupture](figures/classes_rupture_nfir_j19damage.png)

> `figures/classes_rupture_nfir_j19damage.png` (`python3 pore_distribution.py`): the classes of the SCIANTIX run of the NFIR anneal, rebuilt from `output.txt` at the rows where $D$ is 25, 50 and 100 % of its final value. Pores and gas with their broken part (filled), pressure against critical pressure (dashed) with $2\gamma/R$ in grey, and $D$, $G$ on the heat-up.

![release](figures/classes_fgr_nfir_j19damage.png)

> `figures/classes_fgr_nfir_j19damage.png` (`python3 pore_distribution.py`): fission gas release of the NFIR anneal, in per cent of the gas produced (the release of the base irradiation is subtracted), against the time and against the temperature of the heat-up, with the Kr-85 curve of the test (put on the temperature axis through the measured temperature history). Right: release, $D$ and $G$ on a logarithmic axis, for the onset.

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

In the variable $x_i=R_i/R_V$, $p_i/\bar p$ and $w_i$ depend on three numbers only, $(\bar\eta,\mathrm{CV}_n,a)$. $R_V$ and $\bar p$ only rescale the axes; $\bar n$, $\bar V$ and $T$ enter only through $\bar\eta$ (verified to $10^{-14}$ by `section_normalised_vs_direct` of `pore_distribution.py`). For $a=2$ the dilute shape $x_i^{a-3}$ is not a usable approximation: the central classes deviate from it by 14 % at $\bar\eta=0.02$ and by a factor 2 at $\bar\eta=0.09$, and a linear interpolation of $\ln(p_i/\bar p)$ on a $6\times6$ grid of $(\mathrm{CV}_n,\bar\eta)$ is off by up to 75 %: the classes are built at the $(\mathrm{CV}_n,\bar\eta)$ of the state.

The reduction stops at the gas pressure: the rupture criterion has absolute terms,

$$
P^*_{cr,i}=\frac{v_{Xe}}{k_BT}\Bigl[P_h+\frac{2\gamma}{R_i}+\frac{\sigma_R+P_h}{\hat s_i}\Bigr],
$$

so $D^*$ and $G^*$ depend also on $R_V$, $T$, $\sigma_R$ and $P_h$ separately. The universal curve describes the gas pressure of the classes, not the rupture fraction.

**Runs for MMM.** An RVE is scale-free in length and linear in the load, so $R_V$ and $\bar p$ only set the scale. With $a$ fixed, the radii $R_k/R_V$, the porosities $\xi_k$ and the pressures $p_k/\bar p$ of the classes depend on four factors: $\xi$, $s$ (which gives $R_k/R_V$ and $\xi_k/\xi$), $\bar\eta$ (which with $s$ gives $p_k/\bar p$) and $P_h/\bar p$ (4). The SCIANTIX states of the NFIR case in these factors:

| state | $\xi$ | $s$ | $\bar\eta$ | $P_h/\bar p$ | $R_V$ (µm) | $T$ (K) | $\bar p$ (MPa) |
|---|---|---|---|---|---|---|---|
| NFIR, end of base irradiation | 0.105 | 0.33 | 0.093 | 0.96 | 0.35 | 903 | 73 |
| NFIR, anneal at 1200 °C | 0.103 | 0.34 | 0.089 | 0 | 0.35 | 1474 | 119 |

`python3 pore_distribution.py --mmm` writes a coarse cover of the four factors, 9 runs: the centre and half of the 16 corners of the ranges **[?]** $\xi$ 0.07–0.145, $s$ 0.21–0.42, $\bar\eta$ 0.07–0.11, $P_h/\bar p$ 0–1, chosen so that every factor is at each level in 4 runs and every pair of factors takes its four combinations. Each run is a file of `mmm_inputs/` with one row per class (the 81 nodes grouped in 7 classes that keep $\bar V$ and $\bar n$, hence $\xi$ and $\bar p$): radius (µm), porosity $\xi_k=\xi\,w_kV_k/\bar V$, gas pressure (MPa), confining pressure (MPa), at the $R_V$ and $T$ of the NFIR anneal; `index.txt` lists the factors of the runs. No SCIANTIX run is needed.

From each run the model needs, for every pore of the RVE, its class and $\sigma_I^{max}$ in the surrounding matrix under the pressures $p_k$ of the file and the macroscopic stress $-P_h$ (at zero macroscopic strain, with the hydrostatic stress that arises to be rescaled as in 4). The factor of the criterion is $\hat s=(\sigma_I^{max}+P_h)/(p_k-P_h)$, defined for the classes with $p_k>P_h$; its mean and scatter per class are the $c_0$, $c_1$, $\sigma_\varepsilon$ of 4. The capillary pressure $2\gamma/R_k$ is subtracted from $p_k$ in the criterion and is not a load of MMM.

### Implementation

- `src/models/HighBurnupStructureFragmentation.C`, case 4. The state $\beta_i$ is in `Sciantix_variables[213..293]` (`HBFF_CLASS_OFFSET`, `HBFF_N_CLASSES` of `MainVariables.h`), $S_2$ in `[212]`; they are written to `output.txt` only with this option (82 columns).
- `pore_distribution.py` is the Python transcription (`build`, `broken`, `rupture_step`). `python3 pore_distribution.py --check <case folder>` replays the recursion on the history of a SCIANTIX case and compares $D$, $G$, $S$, $S_2$ and every $\beta_i$ with `output.txt`: on `regression/hbs_nfir/test_UO2HBS_frag_j19damage` the largest differences over the 10 429 rows are $1\times10^{-7}$ (the state in `output.txt` has 7 digits; the check fails above $10^{-6}$). On that case it also compares the two states that this file quotes (`STATES`) with the output, and fails if they differ by more than $10^{-3}$. Without arguments it draws the figures of this file, with the rupture and release figures of the NFIR case; `--rupture <case folder>` draws those two for another case.
- Options 1, 2, 3 do not use the classes: their NFIR outputs are identical byte by byte with and without this option in the build.
- `./utilities/HBFF/runHBFF.sh` does the whole sequence for this option: build, NFIR case against its gold file, check against the Python transcription, figures of this file and of `regression/hbs_nfir/figures/j19damage`, inputs for MMM.

### Results

`regression/hbs_nfir/test_UO2HBS_frag_j19damage` (formation 4, porosity 3), $a=2$, mean-field pair. No parameter is fitted. The IFA-650 cases are not run with these parameters.

| case | result | data |
|---|---|---|
| NFIR anneal to 1200 °C, $P_h=0$ | $D=0.8$ % at the start of the anneal; release 0.6 % of the gas produced at 300 °C, 2.4 % at 600 °C, 6.2 % at 900 °C, 8.6 % at 1000 °C, 10.0 % at 1100 °C, 11.6 % from 1140 °C; at 1200 °C $D=44$ %, $G=21$ %, $d=3.4$ µm | Kr-85 release 0.2 % at 900 °C, 1.9 % at 1000 °C, 7.1 % at 1100 °C, 12.8 % at 1150 °C, 29.8 % at 1200 °C; fragments below 100 µm |
| NFIR base irradiation, $P_h$ from 0 to 70 MPa | first class breaks at 31 650 h ($P_h=58$ MPa); $D=0.16$ % at the end | – |

The measured release starts at 880 °C, has its peaks at 1036 and 1180 °C, stops during the hold at 1200 °C and amounts to about one third of the gas of the HBS bubbles (SoA: Noirot 2014; the first three are in `regression/hbs_nfir/data`). The model has $G=21$ % and, like the test, no release during the hold. The fragments of heating tests are typically 20–200 µm (SoA: J20): $d=3.4$ µm is a spacing of cracks, not a fragment size (8).

Pore pressure of the NFIR disc, inferred from the measured gas density ($6.6\times10^{27}$ m$^{-3}$) with a real-gas equation of state, against the hard-sphere equation of state of the code at the same density and against the mean pressure of the modelled pores (density $4.0\times10^{27}$ m$^{-3}$):

| temperature | inferred from the measurement | hard spheres, measured density | model, mean pore |
|---|---|---|---|
| 20 °C | about 11 MPa (Noirot 2014; 7–13 MPa, SoA: Dowek 2021) | 59 MPa | 25 MPa |
| 700 °C | 133 MPa (Noirot 2014) | 171 MPa | 78 MPa |
| 1190 °C | 245 MPa (OHP) | 247 MPa | 115 MPa |

Two errors of opposite sign: the modelled gas density is 0.6 times the measured one, which halves the pressure at 1200 °C, and the hard-sphere equation of state has no attractive term, which overestimates the pressure at low temperature (Limitations, L3).

Sensitivity to the closure exponent at the NFIR state heated at fixed gas to 1200 °C ($P_h=0$): $a=2$ $D^*=44$ %, $G^*=21$ %; $a=2.25$ 23 %, 7.8 %; $a=2.5$ 3.2 %, 0.6 %; $a=2.75$ and 3 no rupture.

The NFIR release at 1200 °C is 39 % of the measured one, and its shape differs: the model releases from room temperature, one class after the other as the temperature rises, while the measured release starts at about 870 °C and takes place for more than half above 1150 °C. Three things follow from the closure with $a=2$ on the hard-sphere equation of state: the smallest classes are above the critical pressure already in the base irradiation ($D=0.16$ %) and when the confinement is released at room temperature ($D=0.8$ %); the classes break in order of radius over the whole heat-up, down to a pressure of about 1.6 times the mean; and the classes above 0.28 µm, which hold 79 % of the gas, stay below the critical pressure. The mean pressure of the modelled pores at 1200 °C is 119 MPa, against 245 MPa from the measured gas density (OHP), with a critical pressure of 180–207 MPa: the pores of the porosity model are close to the mechanical equilibrium $P_h+2\gamma/R$ of the base irradiation (73 MPa at 903 K against 76 MPa), those of the disc hold 1.7 times the gas density (about 160 MPa at 903 K).

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

- **L16a [?]** the closure $n_i\propto R_i^a$ is a modelling choice, not a measured distribution; $a$ is not determined by the SCIANTIX moments and sets which classes break and how much gas they hold (NEA reports Hiernaut's estimate that large and small pores have similar pressure, which is $a=3$; the punching shape of 2 is $a=2$). Pressures measured on single pores differ by a factor of several within a sample: 41 and 9 MPa at room temperature for pores of 2.2 and 3.0 µm in diameter, 9–25 MPa for four pores of 0.7–1.3 µm (SoA: Dowek 2021). The pressure is not uniform, and data of this kind can constrain the closure. With $a=2$ and the hard-sphere equation of state the smallest classes are above the punching limit (2) and above the critical pressure in the base irradiation and at the release of the confinement (*Results*).
- **L16b [?]** the stress concentration factor in the code is the mean-field one: porosity only, the same for all the classes, no scatter. The MMM factor exists for monodisperse pores at uniform pressure (OHP); its dependence on $R_i/R_V$, $\mathrm{CV}_n$, $\bar\eta$ and $P_h/\bar p$ needs new runs. The two pairs differ in the effect of $P_h$ (4).
- **L16c [?]** $\tau=2$ s is borrowed; J19 rupture is instantaneous.
- **L16d [?]** the interaction of the pores is that of the intact population; after a break the pressure of the broken pore is not removed from the load of its neighbours (cascade not modelled), and vented pores stay voids.
- **L16e [R]** the classes are rebuilt each step from the unvented moments $\bar n/S$ and $(\mathrm{Var}(n)+\bar n^2)/S_2$; a pore keeps its rank in the size distribution; new restructured material dilutes all the classes alike, with their vented fractions (6). The proportionality holds for the venting itself, not for what the porosity model does afterwards: gas trapped after a partial venting shifts the retained mean and lowers the rebuilt variance. On the NFIR anneal ($1-S=21$ %, little trapping) the rebuilt $\mathrm{CV}_n$ moves from 0.749 to 0.755.
- **L8 [?]** the porosity model evolves the retained mean gas: after a partial venting the mean pore volume falls (the gas volume of the vented pores is removed, the pores relax by vacancy emission) and the gas trapped afterwards is attributed to the intact pores through $\bar n/S$. Both raise the pressure of the intact classes. On the NFIR anneal $\bar V$ falls by 4 % between 300 and 1150 °C.
- **L3 [R]** the hard-sphere equation of state has no attractive term. Xenon is close to its critical point at room temperature (290 K, 5.8 MPa; critical density about $5\times10^{27}$ m$^{-3}$, that of the pores): at the gas density measured in the NFIR pores the hard-sphere pressure is 5 times the one inferred with a real-gas equation of state at 20 °C, 1.3 times at 700 °C and equal to it at 1190 °C (table of *Results*). The pressures measured in rim pores at room temperature are 7–20 MPa (SoA: Dowek 2021). The rupture of the classes at low temperature, hence the release from room temperature of the NFIR case, is overestimated by it; at the temperatures of the measured release the error is small.
- **L9 [?]** the validity window is on the mean pore; the classes extend below 0.1 µm.
- **L15 [?]** no dependence on the heating rate except the relaxation time; higher heating rates give more release and smaller fragments (NEA16; SoA: J20), and the empirical thresholds do not resolve it either.
- **[?]** $\sigma_R$ of either pair has no direct physical meaning: it absorbs the toughness and the spherical idealisation of the pores (OHP), or the stress concentration that the ligament balance ignores (J19).
- **L11 [R]** the criteria act on the modelled pore state (formation 4, porosity 3: 10.5 % porosity, 0.35 µm, gas density $4.0\times10^{27}$ m$^{-3}$ at the end of the NFIR base irradiation), not on the measured one (11 %, 0.88 µm, $6.6\times10^{27}$ m$^{-3}$).
- **L13 [?]** the confining stress of the base irradiation is an input and changes the pore state through the equilibrium pressure of the porosity model.
- **L5 [?]** no lower bound on the fragment size, and $d$ is a crack spacing (8).
- **[N]** the tails beyond $\pm4s$ are dropped; the fraction outside is $4\times10^{-3}$ for the weight of $\mathrm{Var}(n)$ at the NFIR state (`section_truncation` of `pore_distribution.py`). $D$ and $G$ move in steps of one class: up to 4 % of the pores for a class near the median, 1.5 % of the gas produced in the NFIR release curve.
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
| Dowek 2021 | R. Dowek et al., *Determination of the pressure in micrometric bubbles in irradiated nuclear fuels*, J. Nucl. Mater. 543 (2021) 152591 (SoA) | pore pressures at room temperature, single-pore values |
| Cappia 2016 | F. Cappia et al., *Critical assessment of the pore size distribution in the rim region of high burnup UO2 fuels*, J. Nucl. Mater. 480 (2016) 138 (SoA) | range of validity of the log-normal pore size distribution |
| Capps 2021 | N. Capps et al., *A critical review of high burnup fuel fragmentation, relocation, and dispersal under loss-of-coolant accident conditions*, J. Nucl. Mater. 546 (2021) 152750 (SoA) | empirical thresholds and what they do not resolve |

PDFs are in `letteratura/fragm/`. J19 Eq. 21–22 and Tables 3–4, OHP section 2.2, the declared limitations of K15 and the two statements of Aagesen 2021 are checked against the PDFs; the entries marked (SoA) are not in that folder.
