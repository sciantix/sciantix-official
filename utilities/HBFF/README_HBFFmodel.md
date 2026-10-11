# HBFF - rupture per pore class, progressive (`iHighBurnupStructureFragmentation = 4`)
#### Author of this folder: E.Cappellari, POLIMI-CEA 2026

Tag: **[P]** paper, **[R]** reduction into the SCIANTIX state, **[U]** application to the HBS material, **[N]** numerics,
**[?]** unknown parameter, **[E]** deviation from a source.

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

> `figures/pore_eos.png` (`python3 plot.py --figure pore_eos`): $p_g$ against $n$ for $R=0.35$ µm at 600, 1000 and 1500 K. The solid lines are the hard-sphere EoS, the dashed lines the ideal gas, the dots the cap $\eta=0.65$ beyond which $p_g$ is linear in $n$. Already at $n\approx2\times10^{9}$ the hard-sphere pressure is about 3 times the ideal one, and it grows steeply up to the cap.

![pore EoS vs volume](figures/pore_eos_volume.png)

> `figures/pore_eos_volume.png` (`python3 plot.py --figure pore_eos_volume`): $p_g$ against $V_p$ at fixed gas ($n=1,2,4\times10^{9}$) and $T=1000$ K, same conventions. At fixed $n$ the cap sets a smallest volume $V_p=n\,v_{Xe}/0.65$ (the dots); the dotted line is the volume of the $R=0.35$ µm pore. The pressure is $\propto1/V_p$ for the ideal gas, steeper than $1/V_p$ for the hard-sphere gas, and again $\propto1/V_p$ below the cap.

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

so $\sum_iw_in_i=\bar n$. In the dilute limit $p_i\propto R_i^{a-3}$: $a=3$ is a uniform pressure, $a=2$ an overpressure $\propto1/R$, the shape of the punching limit of K15 and J19. The width $s$ is the root of $\sum_iw_i(n_i-\bar n)^2=\mathrm{Var}(n)$ on the 81 truncated nodes: the coefficient of variation of $n$ on the nodes depends on $s$ and $a$ only and grows with $s$, so the root is bracketed between 0 and the value of the continuous log-normal, $s_0=\sqrt{\ln(1+\mathrm{Var}(n)/\bar n^2)}/a$, doubled until it is above the root. For $\mathrm{Var}(n)=0$, $s=0$ and all the classes coincide with the mean pore. The three moments $\bar V$, $\bar n$, $\mathrm{Var}(n)$ hold on the classes to $10^{-8}$ (`check_moments`).

![classes](figures/classes_distributions.png)

> `figures/classes_distributions.png` (`python3 pore_distribution.py`): the classes of the SCIANTIX state at the end of the NFIR base irradiation ($\bar V=1.825\times10^{-19}$ m³, $\bar n=7.23\times10^{8}$, $\mathrm{CV}_n=0.75$, 903 K): radius pdf, pressure and gas per class. Grey: the capillary pressure $2\gamma/R$, and ten times it (dashed) to compare the shapes: $2\gamma/R$ falls as $1/R$, $p_i$ less steeply ($R^{a-3}$ in the dilute limit).

At this state $s=0.27$ ($a=2.5$; 0.22 for $a=3$, 0.33 for $a=2$), against about 0.3 measured on the NFIR disc (OHP Fig. 4) and 0.356 of K15: the variance of $n$ that SCIANTIX carries gives a realistic width.

**Consistency with the base irradiation.** A class above the punching limit of J19 Eq. 21, $P_h+2\gamma/R+c_{dp}/R$ ($c_{dp}=55$ N/m), would grow by dislocation punching in steady state. At the NFIR state no class is above it for $a\ge2.25$; for $a=2$ the smallest classes are (0.9 % of the pores, up to 2.7 GPa).

![closure exponent](figures/classes_closure_exponent.png)

> `figures/classes_closure_exponent.png`: pressure per class for $a$ from 3 to 2 at the same moments, with $2\gamma/R$ (grey) and the punching limit (dashed).

#### 3. Pressure per class
Hard-sphere equation of state on every class:

$$
p_i=\frac{n_ik_BT}{V_i}\,Z(\eta_i),\qquad \eta_i=\min\!\left(\frac{n_iv_{Xe}}{V_i},\,0.65\right)
$$

The mean pressure $\bar p=p_{\rm EoS}(\bar n,\bar V,T)$ is the pressure of the volume-weighted mean density $\sum_iw_in_i/\sum_iw_iV_i=\bar n/\bar V$ and holds because $\bar n$ and $\bar V$ are conserved. The number-weighted mean of $p_i$ is higher (86 against 73 MPa at the NFIR state) and is not a conserved quantity.

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

With a pressure that depends on the radius, the stress field is the superposition of the fields of the single pores loaded by their net pressure $p_j-P_h$. The ratios of the net pressures of the classes depend on the pressure ratios $p_j/p_i$, fixed by $(\bar\eta,\mathrm{CV}_n,a)$ (9), and on $P_h/\bar p$: a uniform $P_h$ is a rescaling only for a uniform pore pressure ($a=3$, the OHP case) or for $P_h=0$. So $(c_0,c_1,\sigma_\varepsilon)$ are tabulated against $(\xi,\mathrm{CV}_n,\bar\eta)$ and $P_h/\bar p$. The maximum principal stress is not linear in the loads: it has to be taken on the superposed field (`mmm_cases.md`, *What to return*).

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
\alpha>\alpha_{old}:\quad \beta_i\leftarrow\beta_i\,\frac{\alpha_{old}}{\alpha}
$$

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

**[R]**: between two steps the porosity model changes the retained moments, and the unvented ones follow in proportion. The state of the model is $\beta_i$ (81 values), $S$ and $S_2$, all written to `output.txt`: the classes of any step can be rebuilt from it. $1-S$ is the gas vented; $G$ is evaluated on the classes of the current step and differs from it when the gas of the classes changes after a rupture (0.786 % against 0.774 % at the end of the NFIR anneal).

Which classes break first depends on $a$ (2). For $a<3$ the pressure falls with the radius, so the small pores break first and carry little gas ($n_i\propto R_i^{a}$): $D$ leads $G$. At the NFIR state heated at fixed gas to 1200 °C with $P_h=0$, $P_{cr,i}=181$–$199$ MPa and $p_i=65$–$308$ MPa for $a=2.5$: the classes below $R\approx0.2$ µm break, $D^*=3.2$ %, $G^*=0.6$ %. With $P_h=70$ MPa, $P_{cr,i}\approx850$ MPa and no class breaks.

![heat-up](figures/classes_heatup.png)

> `figures/classes_heatup.png`: $D^*$ and $G^*$ of the NFIR state heated at fixed gas, for $a$ from 3 to 2. The curves are staircases, one step per class.

![rupture](figures/classes_rupture_nfir_j19damage.png)

> `figures/classes_rupture_nfir_j19damage.png` (`python3 pore_distribution.py --rupture ../../regression/hbs_nfir/test_UO2HBS_frag_j19damage`): the classes of the SCIANTIX run of the NFIR anneal, rebuilt from `output.txt` at the rows where $D$ is 25, 50 and 100 % of its final value. Pores and gas with their broken part (filled), pressure against critical pressure (dashed) with $2\gamma/R$ in grey, and $D$, $G$ on the heat-up.

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

In the variable $x_i=R_i/R_V$, $p_i/\bar p$ and $w_i$ depend on three numbers only, $(\bar\eta,\mathrm{CV}_n,a)$. $R_V$ and $\bar p$ only rescale the axes; $\bar n$, $\bar V$ and $T$ enter only through $\bar\eta$ (verified to $10^{-14}$ in the notebook, *Normalised against direct*). The shape $p_i/\bar p$ is the dilute one, $x_i^{a-3}$, only for $\bar\eta\lesssim0.02$; at $\bar\eta=0.09$ it deviates from it by 20 % in the central classes.

The reduction stops at the gas pressure: the rupture criterion has absolute terms,

$$
P^*_{cr,i}=\frac{v_{Xe}}{k_BT}\Bigl[P_h+\frac{2\gamma}{R_i}+\frac{\sigma_R+P_h}{\hat s_i}\Bigr],
$$

so $D^*$ and $G^*$ depend also on $R_V$, $T$, $\sigma_R$ and $P_h$ separately. The universal curve describes the gas pressure of the classes, not the rupture fraction.

**Cases for MMM.** A run of MMM is fixed by the geometry, $\xi$ and $s(\mathrm{CV}_n,a)$, and by the load, $p_i/\bar p$ as a function of $(\bar\eta,\mathrm{CV}_n,a)$ and $P_h/\bar p$ (4). The SCIANTIX states of the regression cases give the ranges:

| state | $\xi$ | $\mathrm{CV}_n$ | $\bar\eta$ | $R_V$ (µm) | $T$ (K) | $\bar p$ (MPa) | $P_h/\bar p$ |
|---|---|---|---|---|---|---|---|
| NFIR, end of base irradiation | 0.105 | 0.75 | 0.093 | 0.35 | 903 | 73 | 0.96 |
| NFIR, anneal at 1200 °C | 0.105 | 0.75 | 0.087 | 0.35 | 1474 | 116 | 0 |
| IFA-650.9, onset of the rupture | 0.145 | 0.44 | 0.098 | 0.43 | 1045 | 93 | 0.05 |
| IFA-650.10, peak of the transient | 0.068 | 0.47 | 0.082 | 0.38 | 1133 | 80 | 0.00 |

`mmm_cases.md` (`python3 pore_distribution.py --mmm`) lists the cases, these four and one-at-a-time variations of $\xi$, $\mathrm{CV}_n$, $\bar\eta$ and $a$ around the NFIR anneal, and for each the table of the pore radii with their pressures: the 81 nodes grouped in 7 classes that keep $\bar V$ and $\bar n$ (hence $\xi$ and $\bar p$), with the number of spheres for an RVE of 1200 pores. The 81-node tables are in `mmm_cases.csv`. The classes of the NFIR anneal:

| class | $x$ range | number fraction | spheres | $R_k/R_V$ | $R_k$ (µm) | $p_k/\bar p$ | $p_k$ (MPa) | $2\gamma/R_k$ (MPa) |
|---|---|---|---|---|---|---|---|---|
| 1 | −4.0 … −2.5 | 0.0062 | 8 | 0.426 | 0.150 | 2.011 | 233.5 | 14.7 |
| 2 | −2.5 … −1.5 | 0.0607 | 73 | 0.552 | 0.194 | 1.639 | 190.3 | 11.3 |
| 3 | −1.5 … −0.5 | 0.2417 | 290 | 0.708 | 0.249 | 1.357 | 157.6 | 8.8 |
| 4 | −0.5 … +0.5 | 0.3827 | 459 | 0.906 | 0.318 | 1.135 | 131.8 | 6.9 |
| 5 | +0.5 … +1.5 | 0.2417 | 290 | 1.159 | 0.407 | 0.956 | 111.1 | 5.4 |
| 6 | +1.5 … +2.5 | 0.0607 | 73 | 1.484 | 0.521 | 0.811 | 94.1 | 4.2 |
| 7 | +2.5 … +4.0 | 0.0062 | 7 | 1.926 | 0.677 | 0.683 | 79.3 | 3.3 |

### Implementation

- `src/models/HighBurnupStructureFragmentation.C`, case 4. The state $\beta_i$ is in `Sciantix_variables[213..293]` (`HBFF_CLASS_OFFSET`, `HBFF_N_CLASSES` of `MainVariables.h`), $S_2$ in `[212]`; they are written to `output.txt` only with this option (82 columns).
- `pore_distribution.py` is the Python transcription (`build`, `broken`, `rupture_step`), with the sections of `pore_distribution.ipynb`. `python3 pore_distribution.py --check <case folder>` replays the recursion on the history of a SCIANTIX case and compares $D$, $G$, $S$, $S_2$ and every $\beta_i$ with `output.txt`: on `regression/hbs_nfir/test_UO2HBS_frag_j19damage` the largest differences over the 140012 steps are $5\times10^{-9}$ on $D$ and $G$ and $5\times10^{-8}$ on $S$, $S_2$ and $\beta_i$ (the state in `output.txt` has 7 digits). `--rupture <case folder>` draws the distribution and rupture of the classes of a case.
- Options 1, 2, 3 do not use the classes: their NFIR outputs are identical byte by byte with and without this option in the build.

### Results

`regression/hbs_nfir` and `regression/hbs_ifa650`, `test_UO2HBS_frag_*_j19damage` (formation 4, porosity 3), $a=2.5$, mean-field pair. No parameter is fitted.

| case | result | data |
|---|---|---|
| NFIR anneal to 1200 °C, $P_h=0$ | first class breaks at 623 °C; $D=1$ % at 1047 °C; at 1200 °C $D=4.0$ %, $G=0.77$ %, release 0.44 % of the gas produced, $d=37$ µm | Kr-85 release 0.1 % at 870 °C, 29.8 % at 1200 °C; fragments below 100 µm |
| IFA-650.9 (90 MWd/kgU) | first class breaks at 1045 K with $P_h=4$ MPa; $D=0.1$ at 1092 K, 0.5 at 1119 K ($G=0.38$), 0.9 at 1125 K; all the classes at the end | significant fine fragmentation |
| IFA-650.10 (61 MWd/kgU) | no class breaks | limited fine fragmentation, coarser fragments |

Sensitivity to the closure exponent at the NFIR state heated at fixed gas to 1200 °C ($P_h=0$): $a=3$ and 2.75 no rupture; $a=2.5$ $D^*=3.2$ %, $G^*=0.6$ %; $a=2.25$ 23 %, 7.8 %; $a=2$ 44 %, 21 %.

The NFIR release is about 70 times below the measurement. The mean pressure of the modelled pores at 1200 °C is 116 MPa, against 245 MPa from the measured gas density (OHP), with a critical pressure of about 180–200 MPa: the pores of the porosity model are close to the mechanical equilibrium $P_h+2\gamma/R$ of the base irradiation (73 MPa at 903 K against 76 MPa), those of the disc hold 1.7 times the gas density (about 160 MPa at 903 K), an overpressure of the order of the punching limit $c_{dp}/R$. The model breaks only the classes that the closure puts above about 1.6 times the mean pressure.

### Parameters

| symbol | value | tag | source |
|---|---|---|---|
| $\hat s$ | $\xi/(1-\xi)$ | [P] | J19 Eq. 22 (mean field); to be replaced by the MMM tables with their $\sigma_R$ |
| $\sigma_R=\sigma^{cr}_{hbs}$ | 21 MPa | [P] | J19 Table 4 (calibrated by Jernkvist on IFA-650 and annealing data, not here) |
| $\sigma_R$ of the MMM pair | 275 MPa | [P] | OHP D5.2 section 2.2.2 (calibrated on the NFIR-V FGR curve with the pressure from the measured gas density); not in the code |
| $c_0,c_1,\sigma_\varepsilon$ | $f(\xi,\mathrm{CV}_n,\bar\eta,P_h/\bar p)$ | [?] | MMM tables, to be computed; in the code $c_0=\ln[\xi/(1-\xi)]$, $c_1=0$, $\sigma_\varepsilon=0$ |
| closure exponent $a$ | 2.5 | [?] | none; 3 is a uniform pressure, 2 the punching shape |
| $\xi_{perc}$ | 0.29 | [P] [E] | J19 section 2.3.2 |
| $\tau$ | 2 s | [?] | J19 Table 3, interconnection and microcracking modes |
| $\gamma$ | 1.1 N/m | [R] | `SetMatrix.C`, UO2HBS |
| hard-sphere EoS, $\eta_{max}$ | $d_{Xe}(T)$, 0.65 | [R] | `HighBurnupStructurePorosity.C` |
| classes | 81, $\pm4s$ | [N] | K15 Eq. 6 |
| $A_f$, $d_{max}$ | $2\pi R_{WS}^2$, 500 µm | [?] | none; $d_{max}$ from NEA §3 |
| validity window | $R\ge0.1$ µm, $\xi<0.35$ | [?] | on the mean pore |

### Limitations

The labels are those of the register of `README.md`; L16 is split here.

- **L16a [?]** the closure $n_i\propto R_i^a$ is a modelling choice, not a measured distribution; $a$ is not determined by the SCIANTIX moments and sets which classes break and how much gas they hold (NEA reports Hiernaut's estimate that large and small pores have similar pressure, which is $a=3$; K15 and J19 put the pores at the punching limit, which is $a\approx2$).
- **L16b [?]** the stress concentration factor in the code is the mean-field one: porosity only, the same for all the classes, no scatter. The MMM factor exists for monodisperse pores at uniform pressure (OHP); its dependence on $R_i/R_V$, $\mathrm{CV}_n$, $\bar\eta$ and $P_h/\bar p$ needs new runs. The two pairs differ in the effect of $P_h$ (4).
- **L16c [?]** $\tau=2$ s is borrowed; J19 rupture is instantaneous.
- **L16d [?]** the interaction of the pores is that of the intact population; after a break the pressure of the broken pore is not removed from the load of its neighbours (cascade not modelled), and vented pores stay voids.
- **L16e [R]** the classes are rebuilt each step from the unvented moments $\bar n/S$ and $(\mathrm{Var}(n)+\bar n^2)/S_2$; a pore keeps its rank in the size distribution; new restructured material dilutes all the classes alike. The proportionality holds for the venting itself, not for what the porosity model does afterwards: gas trapped after a partial venting shifts the retained mean and lowers the rebuilt variance. On IFA-650.9 the rebuilt $\mathrm{CV}_n$ falls from 0.44 before the rupture to 0.32 at $D=0.5$ and 0.20 at $D=0.9$ (`figures/classes_rupture_ifa650_ifa6509_j19damage.png`); on NFIR it does not move.
- **L8 [?]** the porosity model evolves the retained mean gas: after a partial venting the mean pore volume falls (the gas volume of the vented pores is removed, the pores relax by vacancy emission) and the gas trapped afterwards is attributed to the intact pores through $\bar n/S$. Both raise the pressure of the intact classes. On IFA-650.9, while $D$ goes from 0.1 to 0.9, $\bar V$ falls by 14 % and $\bar n/S$ grows by 8 % (19 % from the first break): with the narrowing of L16e, the completion of the rupture within about 30 K is not a result of the criterion alone; the result is reliable up to $D\approx0.1$–$0.25$. On NFIR ($G<1$ %) the effect is negligible.
- **L9 [?]** the validity window is on the mean pore; the classes extend below 0.1 µm.
- **[?]** $\sigma_R$ of either pair has no direct physical meaning: it absorbs the toughness and the spherical idealisation of the pores (OHP), or the stress concentration that the ligament balance ignores (J19).
- **L11 [R]** the criteria act on the modelled pore state (formation 4, porosity 3: 10.5 % porosity, 0.35 µm, gas density $4.0\times10^{27}$ m$^{-3}$ at the end of the NFIR base irradiation), not on the measured one (11 %, 0.88 µm, $6.6\times10^{27}$ m$^{-3}$).
- **L13 [?]** the confining stress of the base irradiation is an input and changes the pore state through the equilibrium pressure of the porosity model.
- **L5 [?]** no lower bound on the fragment size, and $d$ is a crack spacing (8).
- **[N]** the tails beyond $\pm4s$ are dropped; the fraction outside is $4\times10^{-3}$ for the weight of $\mathrm{Var}(n)$ at the NFIR state (notebook, *Truncation of the log-normal tails*). $D$ and $G$ move in steps of one class (about 1 % of the pores near $x=-1.7$).
- Cost: 81 classes and one root of $s$ per step (NFIR case: 140 s).

### Literature

| key | reference | used for |
|---|---|---|
| J19 | L.O. Jernkvist, *Modelling of fine fragmentation and fission gas release of UO2 fuel in accident conditions*, EPJ Nucl. Sci. Technol. 5 (2019) 11, doi:10.1051/epjn/2019030 | Eq. 22 (rupture), Eq. 21 (punching limit), Table 3 ($\tau$), Table 4 ($\sigma^{cr}_{hbs}$, $c_{dp}$), percolation 0.29 |
| J20 | L.O. Jernkvist, *A review of analytical criteria for fission gas induced fragmentation of oxide fuel in accident conditions*, Prog. Nucl. Energy 119 (2020) 103188, doi:10.1016/j.pnucene.2019.103188 | Eq. 7 (ligament force balance, origin of J19 Eq. 22) |
| K15 | K. Kulacsy, J. Nucl. Mater. 466 (2015) 409 | log-normal pore size distribution (Eq. 6, Tables 1–2) |
| Lem00 | F. Lemoine, J. Papin, J.M. Frizonnet, B. Cazalis, H. Rigat, in *Fission Gas Behaviour in Water Reactor Fuels*, OECD/NEA Seminar, Cadarache 2000, pp. 175–187 | rupture criterion of J19 [22] |
| KH20 | G. Khvostov, *Analytical criteria for fuel fragmentation and burst FGR during a LOCA*, Nucl. Eng. Technol. 52 (2020) 2402 | fragment size from the specific surface of the cracks |
| NEA16 | NEA/CSNI/R(2016)16, *Report on Fuel Fragmentation, Relocation and Dispersal (FFRD)* | fragment morphology and size, $d_{max}$, heating-rate dependence |
| OHP | OperaHPC Deliverable D5.2 (2026), section 2.2 | NFIR annealing test, Kr-85 release used as reference; MMM RVE, $\sigma_R$, stress concentration factor $\hat s$ |
| CS69 | N.F. Carnahan, K.E. Starling, J. Chem. Phys. 51 (1969) 635 | hard-sphere equation of state |

PDFs are in `letteratura/fragm/`. J19 Eq. 21–22 and Tables 3–4, and OHP section 2.2, are checked against the PDFs.
