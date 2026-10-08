# HBFF - Jernkvist (2019) force balance per pore class, progressive rupture
#### Author of this folder: E.Cappellari, POLIMI-CEA 2026


### Goal
Per time step the model returns three quantities, written to `output.txt`:

| output | symbol | meaning |
|---|---|---|
| `HBS fragmented fraction` | $D$ | pore-number fraction of the HBS whose pores are broken (drives the fragment size) |  
| `HBS burst release fraction` | $G$ | gas-weighted fraction released by the broken pores (drives the release) |  
| `HBS fragment size` | $d$ | mean fragment size (m) |  

Auxiliary: `HBS gas retention fraction` $S$, `HBS pore pressure` $p_g$.

Inputs: $T$ and $P_h=-\sigma_h$ from `input_history.txt` ($\sigma_h$ in MPa, negative in compression), and the pore state of `HighBurnupStructurePorosity.C` (option 2 or 3; stop the run otherwise): porosity $\xi$, mean radius $R$, pore volume $V_p$, Xe atoms per pore $\bar n$ and its variance $\mathrm{Var}(n)$, pore density $N_p$. 

Validity window **[?]**: $R\ge0.1$ µm and $0<\xi<0.35$; outside it $D^*=G^*=0$.

### Equations

#### 1. Pore pressure of the SCIANTIX state

Hard-sphere (Carnahan–Starling) equation of state:

$$
p_g=\frac{n\,k_BT}{V_p}\,Z(\eta),\qquad Z(\eta)=\frac{1+\eta+\eta^2-\eta^3}{(1-\eta)^3},\qquad
\eta=\min\!\left(\frac{n\,v_{Xe}}{V_p},\,0.65\right)
$$

$$
v_{Xe}=\frac{\pi}{6}d_{Xe}^3,\qquad d_{Xe}=4.45\times10^{-10}\,\bigl[0.8542-0.03996\ln(T/231.2)\bigr]\ \text{m}
$$

$n=\bar n/S$ is the gas of the pore before any venting. Above $\eta=0.65$ the pressure is linear in the gas, $Z=Z(0.65)$.
![pore EoS](figures/pore_eos.png)

> `figures/pore_eos.png` (`python3 plot.py --figure pore_eos`): $p_g$ against $n$ for $R=0.35$ µm at 600, 1000 and 1500 K. The solid lines are the hard-sphere EoS, the dashed lines the ideal gas, the dots the cap $\eta=0.65$ beyond which $p_g$ is linear in $n$. Already at $n\approx2\times10^{9}$ the hard-sphere pressure is about 3 times the ideal one, and it grows steeply up to the cap.

![pore EoS vs volume](figures/pore_eos_volume.png)

> `figures/pore_eos_volume.png` (`python3 plot.py --figure pore_eos_volume`): $p_g$ against $V_p$ at fixed gas ($n=1,2,4\times10^{9}$) and $T=1000$ K, same conventions. At fixed $n$ the cap sets a smallest volume $V_p=n\,v_{Xe}/0.65$ (the dots); the dotted line is the volume of the $R=0.35$ µm pore. The pressure is $\propto1/V_p$ for the ideal gas, steeper than $1/V_p$ for the hard-sphere gas, and again $\propto1/V_p$ below the cap.

#### 2. Pore classes

SCIANTIX carries $\bar V$ (radius $R_V=(3\bar V/4\pi)^{1/3}$), $\bar n$ and $\mathrm{Var}(n)$. The pore radius is log-normal (K15 Eq. 6) on 81 nodes $x_i\in[-4,4]$ equally spaced (one class if $\mathrm{Var}(n)=0$), with number weights

$$
w_i=\frac{e^{-x_i^2/2}}{\sum_je^{-x_j^2/2}},\qquad g_i=e^{s\,x_i},\qquad
R_i=R_V\,\frac{g_i}{\bigl(\sum_jw_jg_j^3\bigr)^{1/3}},\qquad V_i=\tfrac43\pi R_i^3
$$

The scale is fixed so that the volume is conserved, $\sum_iw_iV_i=\bar V$, for any width $s$.

The two moments of $n$ do not say how the gas is shared among the radius classes. The closure is

$$
n_i=\bar n\,\frac{R_i^{a}}{\sum_jw_jR_j^{a}},\qquad a=2.5\ \text{(test case; $a=3$ is a uniform pressure)}
$$

so $\sum_iw_in_i=\bar n$. The width $s$ is the root of $\sum_iw_i(n_i-\bar n)^2=\mathrm{Var}(n)$ on the 81 truncated nodes, started from the continuous value $s_0=\sqrt{\ln(1+\mathrm{Var}(n)/\bar n^2)}/a$ and bracketed in $[0.5\,s_0,\,2\,s_0]$. The three moments $\bar V$, $\bar n$, $\mathrm{Var}(n)$ hold on the classes to $10^{-8}$ (`check_moments`).

#### 3. Pressure per class 
Hard-sphere equation of state on every class:

$$
p_i=\frac{n_ik_BT}{V_i}\,Z(\eta_i),\qquad \eta_i=\min\!\left(\frac{n_iv_{Xe}}{V_i},\,0.65\right)
$$

The mean pressure $\bar p=p_{\rm EoS}(\bar n,\bar V,T)$ is the pressure of the volume-weighted mean density $\sum_iw_in_i/\sum_iw_iV_i=\bar n/\bar V$ and holds because $\bar n$ and $\bar V$ are conserved.

#### 4. Rupture criterion per class

Class $i$ breaks when the maximum first principal stress at the pore wall exceeds the rupture stress $\sigma_R$ (OHP D5.2 section 2.2.2). The linear elastic stress is $\hat s_i\,(p_i-P_h)$ above the confining stress, with $\hat s_i$ the **stress concentration factor** of the pore, so

$$
\hat s_i\,(p_i-P_h)\ \ge\ \sigma_R+P_h\quad\Longleftrightarrow\quad
P_{cr,i}=P_h+\frac{2\gamma}{R_i}+\frac{\sigma_R+P_h}{\hat s_i},\qquad \gamma=1.1\ \text{N/m}\ (\text{UO2HBS})
$$

The capillary term $2\gamma/R_i$ is not in OHP **[?]**; it is kept as in J19.

**Stress concentration factor.** $\hat s_i=\sigma_I^{max}|_i/p_{in}$ is computed by MMM on a periodic RVE of random spherical pores, linear elasticity, zero macroscopic strain, unit pressure in the pores. It collects the effects of the neighbours, the radius distribution and the porosity. The heterogeneity of the pore arrangement is the main source of the spread of the critical pressures. $\hat s_i$ is the only entry of the micromechanics in the model and acts on the rupture only, not on $p_i$ or on the venting.

**Reduction in the SCIANTIX state.** Regression of $\ln\hat s_i$ on what SCIANTIX knows, the neighbours giving the scatter ($c_0$, $c_1$ are named so as not to clash with the closure exponent $a$ of 2):

$$
\ln\hat s_i=c_0+c_1\ln\frac{R_i}{R_V}+\varepsilon,\qquad \varepsilon\sim\mathcal N(0,\sigma_\varepsilon^2)
$$
$$
 (c_0,c_1,\sigma_\varepsilon)=f(\xi,\mathrm{CV}_n,\bar\eta)\ \text{tabulated offline from MMM}
$$

The stress on pore $i$ is $\sigma_i=\sum_jS_{ij}(p_j-P_h)$ with the influence coefficients $S_{ij}$ of the RVE, hence $\hat s_i=S_{ii}+\sum_{j\ne i}S_{ij}\,(p_j-P_h)/(p_i-P_h)$. It depends on the pressure ratios $p_j/p_i$ of the classes, and these are fixed by $(\bar\eta,\mathrm{CV}_n,a)$ (9). For $a=3$ (uniform pressure) it is the OHP case.

**Mean-field limit.** With $\hat s=\xi/(1-\xi)$ and $\sigma_R\to\sigma^{cr}_{hbs}=21$ MPa the criterion is J19 Eq. 22 (Lemoine et al. 2000 / Olander, J19 section 2.3.3 (2)), with $\sigma_1\to-P_h$ as J19 does:

$$
P_{cr,i}=\frac{2\gamma}{R_i}+\frac{\sigma^{cr}_{hbs}\,(1-\xi)+P_h}{\xi}
$$

$\hat s_i$ replaces $\xi/(1-\xi)$. 

Interconnected pores, $\xi\ge\xi_{perc}=0.29$ (J19, end of section 2.3.2): the whole population is open, $D^*=G^*=1$.

$$
D^*=\sum_i w_i\,b^*_i,\qquad
G^*=\frac{\sum_i w_i\,n_i\,b^*_i}{\sum_i w_i\,n_i},\qquad b^*_i=\text{broken probability of class }i\ (5)
$$

$D^*$ is a number fraction while $G^*$ a gas fraction.

#### 5. Progressive rupture per class

Each class carries its own broken fraction $\beta_i\in[0,1]$. The instantaneous state $b^*_i$ is the probability that a pore of class $i$ has $\hat s\,(p_i-P_h)\ge\sigma_R+P_h$, with $\ln\hat s\sim\mathcal N(\mu_i,\sigma_\varepsilon^2)$, $\mu_i=c_0+c_1\ln(R_i/R_V)$ (4):

$$
b^*_i=\tfrac12\,\mathrm{erfc}\!\left(\frac{\ln\hat s_{c,i}-\mu_i}{\sqrt2\,\sigma_\varepsilon}\right),\qquad
\hat s_{c,i}=\frac{\sigma_R+P_h}{p_i-P_h-2\gamma/R_i}\quad(b^*_i=0\ \text{if the denominator}\le0)
$$

For $\sigma_\varepsilon\to0$ it is the step $b^*_i=[p_i\ge P_{cr,i}]$; $b^*_i=1$ for all classes if $\xi\ge\xi_{perc}$. The spread of $\hat s$ makes the rupture progressive in pressure inside each class (OHP: the slow onset of the FGR curve is the fracture of the most interacting pores). J19 breaks the HBS instantaneously (Table 3 gives $\tau=0$ for the grain-boundary rupture) and relaxes the interconnection and microcracking modes with $\tau=2$ s. Option 4 applies the same first-order relaxation to each class, as the intactness ODE of `GrainBoundaryMicroCracking.C`:

$$
\beta_i\leftarrow\beta_i+\max(b^*_i-\beta_i,0)\,\bigl(1-e^{-\Delta t/\tau}\bigr),\qquad \tau=2\ \text{s}
$$

The fractions of the output follow from the classes:

$$
D=\sum_iw_i\beta_i,\qquad G=\frac{\sum_iw_i\,n_i\,\beta_i}{\sum_iw_i\,n_i}
$$

A broken pore does not heal (L6): $\beta_i$ does not decrease except by the dilution of 2.6.

#### 6. Dilution

With $\alpha$ the restructured volume fraction, new restructured material is intact, for every class:

$$
\alpha>\alpha_{old}:\quad \beta_i\leftarrow\beta_i\,\frac{\alpha_{old}}{\alpha}
$$

#### 7. Venting 

A broken pore vents its gas and stays as a void. The classes that break are the ones whose pressure exceeds their own $P_{cr,i}$, so the gas leaves those classes only; the other classes keep their $n_i$. The retained moments per original pore are

$$
\bar n_{ret}=\sum_iw_i\,n_i\,(1-\beta_i),\qquad \langle n^2\rangle_{ret}=\sum_iw_i\,n_i^2\,(1-\beta_i),\qquad S=\frac{\bar n_{ret}}{\bar n}
$$

and the released gas is $\bar n-\bar n_{ret}$ per pore.


Which classes break first depends on $a$ (2.2). At the test state of the notebook ($\xi=0.105$, $P_h=70$ MPa, fixed gas, $a=2.5$) the margin $p_i-P_{cr,i}$ at 1000 K is $+532$ MPa at $R=0.1$ µm, $-556$ MPa at 0.35 µm and $-707$ MPa at 0.8 µm: the small pores break first and carry little gas ($n_i\propto R_i^{a}$), so $D$ leads $G$. For $a=3$ the pressure is uniform and the margins of the classes are close.

**Open point [?].** The classes are rebuilt each step from moments, so $\beta_i$ (81 numbers) and the unvented moments must be stored as state; scaling the retained $\bar n$, $\mathrm{Var}(n)$ by a common factor does not reproduce the removal of selected classes. How the porosity model evolves the gas of the unvented classes while some are open is not defined here.

#### 8. Fragment size

Cubic fragments bounded by the cracked faces of the Wigner–Seitz cells of the pores; the specific-surface idea follows Khvostov 2020 (Eq. 2–4):

$$
R_{WS}=\left(\frac{3}{4\pi N_p}\right)^{1/3},\qquad A_f=2\pi R_{WS}^2\ \text{[?]},\qquad S_v=N_p\,D\,A_f,\qquad
d=\min\!\left(\frac{3}{S_v},\,d_{max}\right)=\min\!\left(\frac{2R_{WS}}{D},\,500\ \mu\text{m}\right)
$$

$d=0$ if $D<10^{-9}$. $D\to1$ gives $d=2R_{WS}$, the cell diameter. $d_{max}=500$ µm is where pellet cracks take over (NEA/CSNI/R(2016)16, section 3).

#### 9. Independent inputs and normalised pressure 

The four SCIANTIX values $\bar V$ (or $R_V$), $\bar n$, $\mathrm{Var}(n)$, $T$ do not fix the sharing of the gas among the classes, so the closure exponent $a$ of 2.2 is a fifth input. Given $a$, $\mathrm{Var}(n)$ fixes the radius width $s$.

With $p^*=p\,v_{Xe}/(k_BT)$ the equation of state of 2.1 has no explicit $T$:

$$
p_i^*=\eta_iZ(\eta_i),\qquad \eta_i=\frac{n_iv_{Xe}(T)}{V_i}\le0.65,\qquad \bar\eta=\frac{\bar n\,v_{Xe}(T)}{\bar V},\qquad \mathrm{CV}_n=\frac{\sqrt{\mathrm{Var}(n)}}{\bar n}
$$

In the variable $x_i=R_i/R_V$, $p_i/\bar p$ and $w_i$ depend on three numbers only, $(\bar\eta,\mathrm{CV}_n,a)$. $R_V$ and $\bar p$ only rescale the axes; $\bar n$, $\bar V$ and $T$ enter only through $\bar\eta$.
The reduction stops at the gas pressure, the rupture criterion has absolute terms,

$$
P^*_{cr,i}=\frac{v_{Xe}}{k_BT}\Bigl[P_h+\frac{2\gamma}{R_i}+\frac{\sigma_R+P_h}{\hat s_i}\Bigr],
$$

so $D^*$ and $G^*$ depend also on $R_V$, $T$, $\sigma_R$ and $P_h$ separately. The universal curve describes the gas pressure of the classes, not the rupture fraction. The factor $\hat s_i$ depends on the pressure ratios of the classes, so its table is indexed by the same normalised variables, $(\xi,\mathrm{CV}_n,\bar\eta,x_i)$ at the given $a$, and not by $R_V$, $\bar n$, $T$ separately.

### Parameters

| symbol | value | tag | source | code line |
|---|---|---|---|---|
| $\sigma_R$ | 275 MPa | [P] | OHP D5.2 section 2.2.2 (calibrated on the NFIR-V FGR curve, so the option is not fit-free) | — |
| $\sigma^{cr}_{hbs}$ | 21 MPa | [P] | J19 Table 4, mean-field baseline of 4 (calibrated by Jernkvist on IFA-650 and annealing data, not here) | 406 |
| $\xi_{perc}$ | 0.29 | [P] | J19 section 2.3.2 | 408 |
| $\tau$ | 2 s | [?] | J19 Table 3, interconnection and microcracking modes | 409 |
| $\gamma$ | 1.1 N/m | [R] | `SetMatrix.C`, UO2HBS | 410 |
| hard-sphere EoS, $\eta_{max}$ | $d_{Xe}(T)$, 0.65 | [R] | `HighBurnupStructurePorosity.C` | 114–116 |
| $c_0,c_1,\sigma_\varepsilon$ | $f(\xi,\mathrm{CV}_n,\bar\eta)$ | [?] | MMM tables, to be computed | — |
| closure exponent $a$ | 2.5 | [?] | none; $a=3$ is a uniform pressure | — |
| classes | 81, $\pm4s$ | [N] | K15 Eq. 6 | — |
| $A_f$, $d_{max}$ | $2\pi R_{WS}^2$, 500 µm | [?] | none; $d_{max}$ from NEA §3 | 593–594 |
| validity window | $R\ge0.1$ µm, $\xi<0.35$ | [?] | — | 165–166 |


### Limitations

- **L1/L16 [?]** the closure $n_i\propto R_i^a$ is a modelling choice, not a measured distribution; $a$ is not determined by the SCIANTIX moments (NEA reports Hiernaut's estimate that large and small pores have similar pressure, which is $a=3$).
- **L16 [?]** $\tau=2$ s is borrowed; J19 rupture is instantaneous.
- **L16 [?]** the interaction of the pores is in $\hat s_i$ of the intact population; after a break the pressure $p_k\to P_h$ of the broken pore is not removed from $\hat s_i$ of its neighbours (cascade not modelled), and vented pores stay voids.
- **[?]** $\hat s_i$ comes from an RVE with monodisperse pores and uniform pressure (OHP); the dependence on $R_i/R_V$, $\mathrm{CV}_n$ and on the pressure ratios of the classes needs new MMM runs, and is tabulated, not measured.
- **[?]** $\sigma_R=275$ MPa has no direct physical meaning (OHP): it absorbs the toughness and the spherical idealisation of the pores.
- **L2 [?]** venting acts on the broken classes only (2.7); the evolution of the moments by the porosity model for a partly open population is not defined, and $\beta_i$ is a new state of 81 values.
- **L11 [R]** the criteria act on the modelled pore state (formation 4, porosity 3: about 10.5 % porosity, 0.35 µm at the end of NFIR base irradiation), not on the measured one (11 %, 0.88 µm).
- **L13 [?]** the confining stress of the base irradiation is an input and changes the pore state through the equilibrium pressure of the porosity model.
- **[N]** the tails beyond $\pm4s$ are dropped; the fraction outside is $1.3\times10^{-2}$ for the weight of $\mathrm{Var}(n)$ at the test case (notebook, *Truncation of the log-normal tails*).
- Cost: 81 classes and one root of $s$ per step.

### Literature

| key | reference | used for |
|---|---|---|
| J19 | L.O. Jernkvist, *Modelling of fine fragmentation and fission gas release of UO2 fuel in accident conditions*, EPJ Nucl. Sci. Technol. 5 (2019) 11, doi:10.1051/epjn/2019030 | Eq. 22 (rupture), Table 3 ($\tau$), Table 4 ($\sigma^{cr}_{hbs}$), percolation 0.29 |
| J20 | L.O. Jernkvist, *A review of analytical criteria for fission gas induced fragmentation of oxide fuel in accident conditions*, Prog. Nucl. Energy 119 (2020) 103188, doi:10.1016/j.pnucene.2019.103188 | Eq. 7 (ligament force balance, origin of J19 Eq. 22) |
| K15 | K. Kulacsy, J. Nucl. Mater. 466 (2015) 409 | log-normal pore size distribution (Eq. 6, Tables 1–2) |
| Lem00 | F. Lemoine, J. Papin, J.M. Frizonnet, B. Cazalis, H. Rigat, in *Fission Gas Behaviour in Water Reactor Fuels*, OECD/NEA Seminar, Cadarache 2000, pp. 175–187 | rupture criterion of J19 [22] |
| KH20 | G. Khvostov, *Analytical criteria for fuel fragmentation and burst FGR during a LOCA*, Nucl. Eng. Technol. 52 (2020) 2402 | fragment size from the specific surface of the cracks |
| NEA16 | NEA/CSNI/R(2016)16, *Report on Fuel Fragmentation, Relocation and Dispersal (FFRD)* | fragment morphology and size, $d_{max}$, heating-rate dependence |
| OHP | OperaHPC Deliverable D5.2 (2026), section 2.2 | NFIR annealing test, Kr-85 release used as reference; MMM RVE, $\sigma_R$, stress concentration factor $\hat s$ |
| CS69 | N.F. Carnahan, K.E. Starling, J. Chem. Phys. 51 (1969) 635 | hard-sphere equation of state |

PDFs are in `letteratura/fragm/`. J19 Eq. 22 and Tables 3–4 were checked against the PDF.
