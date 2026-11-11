# HBFF — fine fragmentation of the high-burnup structure: model, micromechanics runs and results
#### E. Cappellari, POLIMI-CEA 2026 — technical report of the folder, state of 10/11/2026

This is the only document of the work. It covers the SCIANTIX model (`iHighBurnupStructureFragmentation = 4`), the
micromechanics chain that feeds it (Mérope → gmsh → MMM), the numerical choices, the results obtained so far and the
decisions still to take. The solver itself is described in one other file,
`mm-opera-hpc/polydispersebubble/POLYDISPERSE.md`. 

Contents: [1 Summary](#1-summary-and-status) · [2 Map](#2-map-of-the-work) · [3 SCIANTIX model](#3-the-sciantix-model-option-4)
· [4 Design of the runs](#4-design-of-the-mmm-runs) · [5 Geometry](#5-geometry-mérope) · [6 Mesh](#6-mesh-gmsh)
· [7 Solver](#7-solver-mmm-test-polybubble) · [8 Split and cluster](#8-mesh-split-and-cluster-runs)
· [9 Results](#9-results) · [10 How to proceed](#10-how-to-proceed) · [11 How to run](#11-how-to-run)
· [12 Errors found](#12-errors-and-inconsistencies-found) · [A Literature review](#appendix-a--literature-review)

---

## 1. Summary and status

**Goal.** Option 4 of SCIANTIX breaks the HBS pores class by class with the criterion
$\hat s\,(p-2\gamma/R-P_h)\ge\sigma_R+P_h$. Today $\hat s$ is the mean field of Jernkvist 2019, $\xi/(1-\xi)$, the same
for every pore. The aim is to replace it with the stress concentration factor computed by MMM (OperaHPC) on periodic
cubes of random pores that have the size distribution SCIANTIX sees, with its scatter from pore to pore, as a table
(then a surrogate) over the states SCIANTIX can reach.

**Where we are.**

| step | state |
|---|---|
| option 4 in the C++, with its Python transcription and the NFIR regression case | done; transcription and C++ agree to $2\times10^{-7}$ over 10 429 rows |
| design of the MMM runs: 3 factors, Latin hypercube of 20 runs + the reference run NFIR | done (`mmm_inputs/`) |
| polydisperse MMM (`test-polybubble`): one pressure per pore, search radius per pore, AMG, split mesh | done |
| tests of the RVE on run NFIR: cube size, mesh rule, mesh size, search distance δ | done: 10 cubes of 30 µm, three mesh rules, three mesh sizes, two δ |
| $\hat s$ of run NFIR as SCIANTIX needs it (log-normal per class) and open-loop replay of the anneal | [TBC, FUTURE] done (`mmm_outputs/shat_NFIR.txt`); $\sigma_R$ = 219 MPa matches the Kr-85 release |
| Latin hypercube: geometry | 20 cubes written at unit pressure (`.geo`) and 20 with the class pressures; **gmsh running on Pleiades (10/11)**; nothing solved yet |
| surrogate over the 20 runs, table in the C++ | not started |

**Decisions taken.**

| decision | evidence |
|---|---|
| cube of 30 µm | 10 µm cubes hold no pore of the largest class (7.8 % of the pore volume), porosity 0.101 instead of 0.114, class means off by up to 16 % |
| P2 meshes need `Mesh.HighOrderOptimize`; `inverted_elements.py` only on the cubes `run_gmsh.sh` flags | inverted curved elements stop the solver|
| preconditioner BoomerAMG instead of diagonal scaling | 18–54 PCG iterations whatever the mesh, against 1355 on the single-bubble mesh |
| mesh graded next to the pores (0.15 → 0.45 µm), finer next to the pores below 0.45 µm (`graded_rfine0.45`) | same class means (≤ 0.10 %) and FGR (≤ 0.27 %) as the uniform 0.15 µm mesh at 1/3 of the cost (275 against 811 core-hours for 10 cubes); the uniform mesh needs twice the memory | 
| search distance δ = 0.15 µm = distMin/2 (`-dl 0.15`) instead of 0.3 µm | with δ = distMin, 5 % of the pores took their maximum on the wall of a neighbour |
| [TBC] statistical outputs only (class means, log-normal per class, FGR), never the identity of single pores | mesh noise per pore ≈ 1 % median, 4 % at the 95th percentile: 15 % of the first pores to break change with the mesh, the statistics do not | 

Open questions in Section 10.

---

## 2. Map of the work

Three repositories and one results folder, all under `~/mmm`.

```
~/mmm/
├── sciantix-official/utilities/HBFF/      (this folder; branch development/HBSformation-EC)
│   ├── README.md                 this report
│   ├── pore_distribution.py      Python transcription of option 4, checks, figures, design of the MMM runs, replay
│   ├── runHBFF.sh                build + regression case + transcription check + figures
│   ├── mmm_inputs/               run_NFIR.txt, run_R00…R19.txt, index.txt   (written by pore_distribution.py)
│   ├── mmm_surrogate.py          pores.csv of a run → log-normal of s_hat per class
│   ├── mmm_outputs/              shat_NFIR.txt, shat_NFIR.png
│   ├── data/                     measured pores (Noirot 2014, Dowek 2021)
│   ├── figures/                  classes_*.png, mmm_*.png (png are git-ignored in this repository)
│   ├── abstract_MRS_HBFF/, presentation/      abstract and slides (not updated with this report)
│   └── archive/                  options 1, 2, 3 and their comparison
├── Merope/mmm_files/                      (fork of Mérope; geometry and mesh)
│   ├── merope_pore_pressure.py   pores of a run in a periodic cube, .geo with the graded mesh; cubes of the hypercube
│   ├── test_rve.py               cubes of the RVE tests (10 seeds, mesh sizes)
│   ├── run_gmsh.sh               .geo → .msh (MSH 2.2, P2, high-order optimisation)
│   ├── inverted_elements.py      det J < 0 in the curved elements, with the nearest pore
│   ├── lhs_side30_mesh0.15_graded_rfine0.45/{unit,pk}/run_R00…R19/side30_seed01/   cubes of the hypercube
│   └── archive/                  superseded cubes (side 10; side 30 uniform)
├── mm-opera-hpc/polydispersebubble/       (fork of mm-opera-hpc; solver and analysis)
│   ├── POLYDISPERSE.md           the solver: equations, discretisation, options, verification
│   ├── src/ include/ Elasticity.mfront CMakeLists.txt        test-polybubble
│   ├── tools/PartitionMesh.cxx   partition-mesh: whole Gmsh mesh → one MFEM file per MPI process
│   ├── verification/mmm_vs_analytical.py
│   └── scripts/                  copied into the build folder at every build; run the copies of the build
│       ├── run_mmm.sh            MMM on a tree of cubes, this PC
│       ├── set_pressures.py      unit cubes → cubes with the class pressures (pk/)
│       ├── plot_bubbles.py       mesh test, RVE size (Kanit), FGR per cube
│       ├── break_order.py        which pores break first and why; writes pores.csv
│       ├── mesh_compare.py       the same cubes on two meshes (or two δ), with the cost on the cluster
│       └── npoint.py             n-point probabilities, intact chords (fragment size)
├── build/                         CMake build tree: executables and script copies only
├── results/                       MMM results (since 10/11/2026; before, inside the build tree)
│   ├── test_rve_side30_mesh0.15_graded_rfine0.45_delta0.15/run_NFIR/     the latest: 10 cubes + 2 mesh variants
│   └── archive/                   side 10; side 30 with δ = 0.3; uniform / notuniform (zip); transfers from the cluster
├── archive/                       old cheat sheets and scripts (not in any repository)
├── env.sh                         environment of MMM on PC-EC
├── spack/, spack-repo-mfem-mgis/, gmsh-4.15.2-Linux64/
```

**Workflow**

| # | step | script (where) | reads | writes |
|---|---|---|---|---|
| 1 | classes and design | `pore_distribution.py` (HBFF) | SCIANTIX state, `data/` | `mmm_inputs/run_<id>.txt`: radius, porosity, pressure, confinement per class; `index.txt` |
| 2 | geometry | `test_rve.py`, `merope_pore_pressure.py` (Mérope) | `run_<id>.txt` | per cube: `bubbles_unit_<id>.txt` (id x y z R p), `mesh_unit_<id>.geo`, `summary_unit_<id>.txt`; `cubes.txt`, `unit_<id>.txt` per run |
| 2b | class pressures | `set_pressures.py` | unit cubes, `run_<id>.txt` | `pk/…/bubbles_pk_<id>.txt`, `summary_pk_<id>.txt`, `mesh_case.txt` (points to the unit cube: same mesh) |
| 3 | mesh | `run_gmsh.sh` | `.geo` | `mesh_unit_<id>.msh` (MSH 2.2, P2), `.log` |
| 4 | mesh check, only for the cubes flagged by `run_gmsh.sh` ("To check" > 0) | `inverted_elements.py` | `.msh`, bubbles | report, exit 1 if an element is inverted |
| 5 | split (cluster) | `partition-mesh`, one batch job per cube | `.msh` | `mesh_par<NP>/mesh_par.000000 …`, `done` |
| 6 | solve | `test-polybubble` via `run_mmm.sh` (workstation) or one batch job per cube (cluster) | mesh, bubbles | `bubbles_and_stresses_selected.txt` (σ_I max per pore), `sig_hydro.txt`; with `-p 1` ParaView folders |
| 7 | RVE analysis | `plot_bubbles.py ROOT` | 2 + 6 | `mesh_test`, `rve_size`, `rve_fgr` (.txt, .png); exit 1 if the cube is not an RVE or the mesh is not fine enough |
| 8 | per-pore analysis | `break_order.py ROOT` | 2 + 6 | `pores.csv` (one row per pore: the interface with SCIANTIX), `break_order.txt/png` |
| 9 | comparisons | `mesh_compare.py REF OTHER`, `npoint.py ROOT` | 2 + 6, job logs | `mesh_compare.txt/png`, `npoint.txt/png` |
| 10 | reduction [TBC] | `mmm_surrogate.py ROOT` (HBFF) | `pores.csv`, `index.txt` | `mmm_outputs/shat_<id>.txt`: u = ln(R/R_V), μ, σ of ln ŝ per class |
| 11 | use  [TBC] | `pore_distribution.py` (`section_mmm_replay`) | `shat_NFIR.txt`, `output.txt` of the NFIR case | open-loop replay, `figures/classes_fgr_nfir_mmm.png` |
| 12 | C++  [TBC] | `src/models/HighBurnupStructureFragmentation.C`, l. 483 | — | not done: "the MMM table of s_hat enters here" |

ROOT is a run folder holding `side<L>_seed<NN>` (one cube per seed) and `side<L>_seed01_<variant>` (the pores of seed 1 on another mesh). The folder name carries the choices: side, mesh size next to the pores, mesh rule, δ.

---

## 3. The SCIANTIX model (option 4)

Tags: **[P]** from a paper, **[R]** our reduction, **[?]** unknown parameter, **[N]** numerics.

### 3.1 Input, outputs, regression case

```
4    #    iHighBurnupStructureFragmentation (0= off, 1= Jernkvist (2019), 2= Kulacsy (2015), 3= LEFM (Irwin), 4= rupture per pore class)
```
read by name in `input_settings.txt`; it needs `iHighBurnupStructurePorosity` = 2 or 3. Temperature and hydrostatic
stress come from `input_history.txt` (4th column, MPa, negative in compression): $P_h=-\sigma_h$.

| output | symbol | meaning |
|---|---|---|
| `HBS fragmented fraction` | $D$ | number fraction of broken pores (drives the fragment size) |
| `HBS burst release fraction` | $G$ | gas-weighted fraction of the broken pores (drives the release) |
| `HBS fragment size` | $d$ | mean fragment size (m) |
| `HBS broken fraction - class i`, `HBS gas retention fraction` (and its second moment) | $\beta_i$, $S$, $S_2$ | state of the 81 classes |

Regression case `regression/hbs_nfir/test_UO2HBS_frag_j19damage`: the NFIR disc (103 GWd/tHM), base irradiation
(903 K, $2\times10^{19}$ fiss/(m³ s), 38 000 h, confining stress rising linearly from 0 to 70 MPa — no datum for the
disc, an unknown of the case), unloading, then the measured anneal to 1200 °C at $P_h=0$, with formation model 1
(Barani 2020) and porosity model 3. Gates:

1. gold file: `python3 -m regression.runner --hbs_nfir.j19damage`;
2. transcription: `python3 utilities/HBFF/pore_distribution.py` replays the case and compares C++ and Python at every step (it also rewrites `mmm_inputs/`: see the warning in section 11.4);
3. options 1, 2, 3 do not use the classes: their outputs are identical byte by byte with and without option 4.

`./utilities/HBFF/runHBFF.sh` does 1 and 2 and draws the figures (choices BUILD, RUN, GOLD at its top).

### 3.2 Equations

**Pore pressure [R]** — hard spheres (Carnahan–Starling), as `HighBurnupStructurePorosity.C`:

$$p_g=\frac{n\,k_BT}{V_p}Z(\eta),\quad Z=\frac{1+\eta+\eta^2-\eta^3}{(1-\eta)^3},\quad \eta=\min\!\Bigl(\frac{n\,v_{Xe}}{V_p},0.65\Bigr),\quad
v_{Xe}=\frac{\pi}{6}d_{Xe}^3,\quad d_{Xe}=4.45\times10^{-10}\bigl[0.8542-0.03996\ln(T/231.2)\bigr]\ \text{m}$$

**Pore classes.** SCIANTIX carries the mean pore volume $\bar V$ (radius $R_V$), the mean gas per pore $\bar n$ and
its variance. The radius is log-normal (Kulacsy 2015) on 81 nodes $x_i\in[-4,4]$ **[N]**:

$$w_i=\frac{e^{-x_i^2/2}}{\sum_je^{-x_j^2/2}},\qquad R_i=R_V\frac{e^{s x_i}}{\bigl(\sum_jw_je^{3sx_j}\bigr)^{1/3}},\qquad
n_i=\bar n\,\frac{R_i^{a}}{\sum_jw_jR_j^{a}},\quad a=2\ \textbf{[?]}$$

$\sum w_iV_i=\bar V$ and $\sum w_in_i=\bar n$ by construction; the width $s$ is the root of
$\sum w_i(n_i-\bar n)^2=\mathrm{Var}(n)$. The closure exponent $a$ is not fixed by the moments: $a=2$ is a gas
density $\propto1/R$ (the shape of the punching overpressure), $a=3$ a uniform pressure. Each class has its pressure
$p_i=p_g(n_i,V_i,T)$.

**Rupture criterion.** Class $i$ breaks when the largest first principal stress at the pore wall exceeds the rupture
stress (OperaHPC D5.2). The net pressure on the solid is $p_i-2\gamma/R_i-P_h$ and the elastic stress is $\hat s_i$
times it, above the confining stress $-P_h$:

$$\hat s_i\Bigl(p_i-\frac{2\gamma}{R_i}-P_h\Bigr)\ \ge\ \sigma_R+P_h,\qquad \gamma=1.1\ \text{N/m}$$

$\hat s$ is the stress at the pore per unit net pressure **at zero macroscopic stress**. $\hat s$ and $\sigma_R$ come
as a pair: the stress measure and the strength calibrated with it.

| pair | $\hat s$ | $\sigma_R$ | $\mathrm dP_{cr}/\mathrm dP_h=1+1/\hat s$ | status |
|---|---|---|---|---|
| mean field, J19 Eq. 22 **[P]** | $\xi/(1-\xi)$ = 0.12 at ξ = 0.11, the same for all the pores | 21 MPa | ≈ 9.5 | in the code |
| MMM **[P]** | per pore; run NFIR: mean 1.18–1.28 per class, 5th–95th percentile 0.89–1.77 | 275 MPa in D5.2 (monodisperse); 219 MPa fits the NFIR release with our table | ≈ 1.8 | table of run NFIR done, not in the code |

The two pairs put the first ruptures at nearly the same pressure without confinement (J19: every class at 165 MPa;
MMM with 275 MPa: FGR 5 % at 168 MPa, 50 % at 225 MPa, 90 % at 281 MPa, `mmm_surrogate.py`) and differ by a factor 5 in
the effect of the confinement.

**Progressive rupture.** With $\ln\hat s\sim\mathcal N(\mu_i,\sigma_i^2)$ in class $i$ (what `mmm_surrogate.py`
tabulates), the share of the class that meets the criterion is

$$b^*_i=1-\Phi\!\left(\frac{\ln t_i-\mu_i}{\sigma_i}\right),\qquad t_i=\frac{\sigma_R+P_h}{p_i-2\gamma/R_i-P_h}
\quad(b^*_i=0\ \text{if the denominator}\le0)$$

For $\sigma_i\to0$ it is the step of the mean-field pair. For $\xi\ge0.29$ (percolation, J19) every class is open.
Each class relaxes to it, $\beta_i\leftarrow\beta_i+\max(b^*_i-\beta_i,0)(1-e^{-\Delta t/\tau})$, $\tau=2$ s **[?]**, and

$$D=\sum_iw_i\beta_i,\qquad G=\frac{\sum_iw_in_i\beta_i}{\sum_iw_in_i}$$

**Dilution.** New restructured material is intact: $\beta_i\leftarrow\beta_i\,\alpha_{old}/\alpha$ **[R]**.

**Venting.** A broken pore vents its gas and stays as a void; only the broken classes vent. With
$f_1=\frac{\sum w_in_i(1-\beta_i')}{\sum w_in_i(1-\beta_i)}$ and $f_2$ the same with $n_i^2$, the state of the porosity
model becomes $\bar n\to f_1\bar n$, $\mathrm{Var}(n)\to f_2(\mathrm{Var}(n)+\bar n^2)-(f_1\bar n)^2$, and the gas
leaves through `Xe released`. At the next step the classes are rebuilt from the unvented moments $\bar n/S$ and
$(\mathrm{Var}(n)+\bar n^2)/S_2$.

**Fragment size [R][?].** Cracked faces of the Wigner–Seitz cells: $R_{WS}=(3/4\pi N_p)^{1/3}$,
$d=\min(2R_{WS}/D,\ 500\ \mu\text{m})$. It is a spacing of cracks, a fragment size only once the cracks connect.

### 3.3 Parameters

| symbol | value | tag | source |
|---|---|---|---|
| $\hat s$, $\sigma_R$ | $\xi/(1-\xi)$, 21 MPa | [P] | J19 Eq. 22, Table 4; to be replaced by the MMM table and its $\sigma_R$ |
| closure exponent $a$ | 2 | [?] | gas density ∝ 1/R |
| $\xi_{perc}$ | 0.29 | [P] | J19 §2.3.2 |
| $\tau$ | 2 s | [?] | J19 Table 3 (borrowed) |
| $\gamma$ | 1.1 N/m | [R] | `SetMatrix.C`, UO2HBS |
| EoS, $\eta_{max}$ | hard spheres, 0.65 | [R] | `HighBurnupStructurePorosity.C` |
| classes | 81, ±4 s | [N] | K15 Eq. 6 |
| $A_f$, $d_{max}$ | $2\pi R_{WS}^2$, 500 µm | [?] | $d_{max}$ from NEA §3 |
| validity window | $R\ge0.1$ µm, $\xi<0.35$ | [?] | on the mean pore |

### 3.4 State of the NFIR case and result with the mean-field pair

Read from the gold output (`STATES` of `pore_distribution.py`, agreement $4\times10^{-4}$):

| state | ξ | $R_V$ (µm) | $\bar n$ | CV$_n$ | s | T (K) | $P_h$ (MPa) |
|---|---|---|---|---|---|---|---|
| SCIANTIX, end of base irradiation | 0.130 | 0.40 | $1.20\times10^{9}$ | 0.51 | 0.24 | 903 | 70 |
| SCIANTIX, anneal at 1200 °C | 0.123 | 0.38 | $1.15\times10^{9}$ | 0.53 | 0.25 | 1474 | 0 |
| NFIR disc as irradiated, measured | 0.114 | 0.73 | | | 0.36 | | |

The modelled pores have 0.55 times the measured radius, two thirds of the measured width and 0.7 times the measured
gas density. With the mean-field pair and no fitted parameter, the anneal ends at $D$ = 70.9 %, $G$ = 52.2 %, release
of the transient 41.9 % of the gas produced, against 29.8 % of Kr-85 measured; the model releases from room
temperature while the measured release starts at about 880 °C.

### 3.5 Limitations (short register)

- the closure $n_i\propto R_i^a$ is a choice, not a measurement; it sets which classes break and how much gas they hold;
- hard-sphere EoS without attraction: pressure overestimated at low temperature (5 times at 20 °C at the NFIR density, equal at 1190 °C);
- $\hat s$ in the code is the mean field; the MMM runs are unconfined and the same $\hat s$ is used under confinement;
- no cascade: after a rupture the pressure of the broken pore is not removed from the load of its neighbours;
- the criterion acts on the modelled pore state, not on the measured one (table above);
- $\sigma_R$ of either pair has no direct physical meaning (it absorbs toughness and the spherical idealisation);
- $\tau$ borrowed, no heating-rate dependence; $d$ is a crack spacing with no lower bound.

---

## 4. Design of the MMM runs

**Reduction.** A cube is scale-free in length and linear in the load, so $R_V$ and the mean pressure $\bar p$ only set
the scales. With $p^*=p\,v_{Xe}/k_BT$ the EoS has no explicit temperature, and in $x_i=R_i/R_V$ the classes depend on
three numbers (with $a$ fixed and no confinement):

| factor | sets | range | SCIANTIX cases | NFIR disc |
|---|---|---|---|---|
| ξ | porosity, distances between the pores | 0.02–0.18 | up to 0.133 (NFIR), 0.165–0.196 (`regression/hbs`) | 0.114 |
| s (from CV$_n$) | $R_k/R_V$ and $\xi_k/\xi$ | 0.05–0.42 | 0.05–0.42 | 0.36 (K15: 0.356) |
| $\bar\eta=\bar n v_{Xe}/\bar V$ | with s, the pressures $p_k/\bar p$ | 0.045–0.23 | 0.034–0.228 | 0.144–0.183 |

**Runs.** `pore_distribution.py` writes a Latin hypercube of 20 runs (`scipy.stats.qmc.LatinHypercube`,
`optimization="random-cd"`, seed 1; each factor takes 20 equally spaced levels) plus the reference run **NFIR**: the
log-normal fit of the pores measured on the disc before the anneal (ξ = 0.1138, s = 0.3636, $R_V$ = 0.732 µm) with
the measured gas density ($\bar\eta$ = 0.1555 at 903 K, $\bar p$ = 160 MPa). The 81 nodes are grouped in **7 classes**
that keep $\bar V$ and $\bar n$. All the runs take the $R_V$ and the temperature of the reference. `index.txt`:

| run | ξ | s | $\bar\eta$ | $\bar p$ (MPa) | | run | ξ | s | $\bar\eta$ | $\bar p$ (MPa) |
|---|---|---|---|---|---|---|---|---|---|---|
| NFIR | 0.1138 | 0.364 | 0.156 | 160 | | R10 | 0.080 | 0.078 | 0.068 | 48 |
| R00 | 0.168 | 0.374 | 0.133 | 124 | | R11 | 0.104 | 0.392 | 0.077 | 56 |
| R01 | 0.024 | 0.170 | 0.114 | 98 | | R12 | 0.112 | 0.133 | 0.096 | 76 |
| R02 | 0.120 | 0.226 | 0.198 | 248 | | R13 | 0.040 | 0.355 | 0.105 | 86 |
| R03 | 0.144 | 0.300 | 0.059 | 40 | | R14 | 0.176 | 0.189 | 0.179 | 206 |
| R04 | 0.136 | 0.337 | 0.225 | 324 | | R15 | 0.096 | 0.152 | 0.216 | 297 |
| R05 | 0.064 | 0.411 | 0.170 | 187 | | R16 | 0.048 | 0.207 | 0.050 | 32 |
| R06 | 0.056 | 0.096 | 0.188 | 227 | | R17 | 0.032 | 0.281 | 0.207 | 272 |
| R07 | 0.088 | 0.263 | 0.151 | 153 | | R18 | 0.160 | 0.244 | 0.087 | 66 |
| R08 | 0.128 | 0.115 | 0.124 | 110 | | R19 | 0.152 | 0.059 | 0.161 | 170 |
| R09 | 0.072 | 0.318 | 0.142 | 138 | | | | | | |

Classes of run NFIR (`run_NFIR.txt`): radii 0.219, 0.311, 0.436, 0.610, 0.852, 1.192, 1.702 µm; shares of the pore
volume 0.02, 0.47, 5.1, 22.2, 38.2, 26.2, 7.8 %; class pressures 7647, 1647, 601, 281, 152, 90, 54 MPa (closure
$a=2$: the small classes are far above the mean). In runs R04 and R05 the smallest class is at the cap η = 0.65.

**Two loads per cube** (docstring of `merope_pore_pressure.py`, `set_pressures.py`):

- `unit/`: pressure 1 in every pore. It gives $\hat s_1$, the factor of the geometry alone. All the RVE tests of
  section 9 are unit runs.
- `pk/`: the pressure $p_k$ of its class in every pore, on the same mesh (no Mérope, no gmsh: `mesh_case.txt` names
  the unit cube). It gives the factor with the load of the neighbours, the original plan of 22/10 ("plan B").

**From the MMM stress to ŝ.** MMM solves at zero macroscopic *strain*, so the cube is in compression. Per unit
pressure, with $\langle\sigma_h\rangle$ the mean hydrostatic stress of the matrix (`sig_hydro.txt`) and ξ the porosity
of the cube, $h=\xi-(1-\xi)\langle\sigma_h\rangle$ (0.253 for `side30_seed01`), and

$$\sigma=\frac{\sigma_I^{max}}{p},\qquad \hat s=\frac{\sigma+h}{1-h}\quad\text{(unit runs, the rescaling of D5.2)}$$

For the pk runs `plot_bubbles.py` uses $\hat s=\sigma+h\,p_{ref}(1+\hat s_1)/p$ with $p_{ref}=\sum Vp/\sum V$: exact
when the pressures are equal, otherwise it adds the maxima of the two runs as if they were at the same point. A pore
breaks at the load $x_c=p_{ref}/(\hat s\,p)$ ($1/\hat s$ at one pressure), and FGR($x$) = volume of the broken pores /
volume of all the pores is a function of $x=p_{ref}/\sigma_R$ only. [TBC]

---

## 5. Geometry (Mérope)

`merope_pore_pressure.generate()`:

1. reads the classes of the run file, sorted by decreasing radius; expected pores of a class in the cube
   $=\xi_k/V_k\cdot L^3$;
2. throws the spheres with `sac_de_billes.throwSpheres_3D`, algorithm **RSA**, periodic cube (`NameShape.Tore`),
   with a repulsion distance between surfaces **distMin = 0.3 µm** (one HBS grain between two pores, the assumption
   of D5.2, never tested there);
3. every sphere gets its own phase id (2 … N+1), which becomes its `Physical Surface`; the pores are not meshed
   (`do_not_mesh`); the matrix is phase 1;
4. writes the pore list (id, x, y, z, R, p), the `.geo` of Mérope with mesh order 2, and a summary (side, distMin,
   mesh rule, pores and porosity per class, asked and obtained).

| choice | value | why |
|---|---|---|
| side of the cube | 30 µm (41 $R_V$ of the disc) | as D5.2; the 10 µm cubes are biased (section 9.1) |
| distMin | 0.3 µm (0.41 $R_V$) | D5.2; the first pores to break sit at gaps ≈ distMin, so the onset depends on it (9.6) |
| seeds | 10 for the RVE test, 1 per run of the hypercube | D5.2 uses 10 |

Mérope places the **integer part** of the expected number of pores of every class: run NFIR has 1867 pores per cube
instead of 1870 (11, 113, 452, 715, 452, 113, 11 per class from the largest to the smallest) and porosity 0.1131
instead of 0.1138; the largest class loses 0.6 of its 11.6 pores (7.4 % of the pore volume instead of 7.8 %).
In the hypercube the cubes hold from 390 pores (R01, ξ = 0.024) to 2888 (R14, ξ = 0.176); the porosity lost is at
most 1.5 % of the asked one.

---

## 6. Mesh (gmsh)

gmsh 4.15.2, format MSH 2.2 (the one MFEM reads with the periodic section), tetrahedra of order 2 curved on the
spheres, periodic.

**Rule of the mesh size** (`refine()` of `merope_pore_pressure.py`, folder suffix `_graded_rfine0.45`). Mérope gives
one size to the whole cube; `refine()` replaces it in the `.geo` with one gmsh `Ball` field per pore (and per
periodic image that reaches the cube), combined by a `Min` field used as background field:

| zone | size | parameter |
|---|---|---|
| up to 0.3 µm from a pore wall | h = 0.15 µm | `mesh_size`, `D_FINE` = distMin: every ligament between two close pores is fine |
| from 0.3 to 0.9 µm from the wall | linear growth | `D_TRANS` = 0.6 µm |
| beyond | 0.45 µm | `FAR_TO_FINE` = 3 |
| next to a pore with R < 0.45 µm | h = 0.15 × R / 0.45 (0.073 µm at R = 0.219) | `R_FINE` = 0.45 µm: every pore has R/h ≥ 3 |

`Mesh.MeshSizeFromPoints`, `FromCurvature`, `ExtendFromBoundary` are switched off so that only the field sets the
size. The rule scales with `mesh_size`, so the meshes of a mesh test stay similar. `check_rule()` refuses to mix
cubes of different rules in one folder.

Why `R_FINE`: with one size for all the pores the two smallest classes (R/h < 2) moved by 53 % and 43 % between 0.3
and 0.15 µm, while class 0.436 (R/h = 3) moved by 1.3 %.

**High-order optimisation.** Order-2 elements curved on a sphere can be inverted (det J < 0) where two pores are
close. `run_gmsh.sh` passes `-setnumber Mesh.HighOrderOptimize`: **1** on the graded meshes (optimisation of the bad
patches only; on `side30_seed01` at 0.3 µm 242 inverted elements, all fixed, 440 s, 2 GB), **2** on the uniform
meshes (elastic + optimisation; on a graded mesh its global system went over 14 GB). `run_gmsh.sh` reads the log of gmsh after the
optimisation and flags a cube (`WARNING | possible inverted elements`, line `To check` of its summary) when gmsh
still reports `jac. < 0` or the optimisation failed. **With `To check` = 0 no further check is needed.** Only for
the flagged cubes, `inverted_elements.py <cube folder>` samples det J densely and says whether the elements are
really inverted (the count of gmsh is a conservative bound) and next to which pore. It is not optional there: an
inverted element can leave the solver converging with a wrong stress (9.1). `run_gmsh.sh` skips the `.geo` already meshed (`FORCE=1` to redo).

**Sizes obtained** (logs of `partition-mesh` and of MMM, run NFIR, side 30):

| mesh | vertices | P2 tetrahedra | unknowns | PCG iterations |
|---|---|---|---|---|
| graded 0.15–0.45, rfine 0.45 (10 cubes) | 2.465–2.470 M | 13.37–13.40 M | 56.9–57.1 M | 27–54 |
| graded 0.212–0.636 (seed 1) | 1.00 M | 5.19 M | 22.7 M | 51 |
| graded 0.3–0.9 (seed 1) | 0.40 M | 1.93 M | 8.8 M | 31 |
| uniform 0.15 (10 cubes, archive) | 6.0 M | | | |

(The MMM log prints 2.78 M "vertices" for the first mesh: it sums the vertices of the 512 parts, shared ones counted
more than once.)

---

## 7. Solver (MMM, `test-polybubble`)

Careful description in `mm-opera-hpc/polydispersebubble/POLYDISPERSE.md`. In short:

- **Problem**: linear elasticity of the matrix (E = 150 GPa written as 150e-3 N/µm², ν = 0.3; with pressure loads
  only and one material σ does not depend on E), periodic cube, **zero macroscopic strain**, one uniform pressure
  boundary condition per pore (`pref × p_i`, $p_i$ = 6th column of the bubble file).
- **Discretisation**: MFEM through mfem-mgis, H1 elements of order 2 on the P2 tetrahedra, 3 unknowns per node; law
  from MFront (`Elasticity.mfront`).
- **Linear solver**: one Newton iteration (the problem is linear); `HyprePCG`, relative tolerance $10^{-10}$, at most
  5000 iterations (`-li`), preconditioner **`HypreBoomerAMG`** (`-pc`), which in mfem-mgis master is the "systems"
  variant for a vector field.
- **Why this preconditioner**: the original `bubble/` case uses `HypreDiagScale`, whose iterations grow with the
  mesh (1355 on the default single-bubble mesh). With BoomerAMG: 18–29 iterations on the side-10 cubes, 27–54 on the
  side-30 cubes of 57 M unknowns (most cubes 27–36; seed 9: 54; the coarser 0.212 mesh: 51), final relative residual
  $9\times10^{-11}$, Newton residual $3.5\times10^{-10}$. A direct solver (MUMPS) does not fit in memory at this size.
- **Post-processing**: σ_I at every Gauss point; for every pore the maximum within $R_i+\delta$ of its centre
  (`-dl δ`), over all the points (`-sf 0`); each process searches its own points and rank 0 keeps the largest
  (`MPI_Gather`). Outputs: `bubbles_and_stresses_selected.txt` (`bubble_id, radius, pressure, sigma_I_max, x, y, z`)
  and `sig_hydro.txt` (`Volume, Integral, Average` of the hydrostatic stress of the matrix).
- **Options of all our runs**: `-sf 0 -dl 0.15 -v 1`, `-p 1` (ParaView fields) or `-p 0`; on the cluster `-pm 1`.

**δ, the search distance.** The maximum of a pore is taken in the shell $R<r<R+\delta$. With δ = distMin = 0.3 µm
the shell of a large pore reaches the wall of a smaller neighbour, where the stress belongs to the neighbour. δ must
stay at or below distMin/2; we use **δ = 0.15 µm** (9.3). The true maxima sit 0.006 µm (median) from the own
wall, i.e. at the first Gauss points (≈ 0.04 h).

---

## 8. Mesh split and cluster runs

**Why the mesh is split.** With `-pm 0` every MPI process reads the whole Gmsh mesh and then keeps its part: about
0.37 kB per Gmsh node *per process* in that phase, several GB per process for a side-30 cube. On the cluster (nodes of
128 cores and ≈ 240 GB, 1867 MB per core) it does not fit. With `-pm 1` each process reads its own
file.

**How.** `partition-mesh` (`tools/PartitionMesh.cxx`) runs on **one** process: it reads the Gmsh 2.2 mesh as
mfem-mgis does (`mfem::Mesh(file, 0, 1, true)`), partitions it with METIS through `mfem::MeshPartitioner` (the same
partitioning `mfem::ParMesh` would do), and writes one MFEM parallel mesh file per part, `mesh_par.000000` …, with the
order-2 periodic nodes. The number of files must equal the number of MPI processes of the run. Files split by gmsh
(`-part N`) are another format and cannot be used. Verified: `-pm 1` = `-pm 0` to $3\times10^{-10}$ (the solver
tolerance) on the CI mesh and on side-10 cubes.

**Jobs that produced the latest results** (10/11/2026; batch files and logs in every cube folder of the results):

| job | resources | measured |
|---|---|---|
| split, one per cube | 1 process, 32 cores reserved (only for their 60 GB of memory) | 10.5–11.5 min, 14.0–14.7 GB, 512 files |
| MMM, one per cube | **512 MPI processes × 2 cores = 1024 cores, 8 nodes** | 73–88 s; 1.02 GB per process at most, 532 GB in all |

The second core per process is only there for its memory (2 × 1867 MB per process); the code is not threaded
(`OMP_NUM_THREADS=1`). The graded mesh uses 1.02 GB per process, so it would fit one core per process (512 cores, 4 nodes) with
45 % of margin; the uniform 0.15 µm mesh went out of memory with one core per process (17 failed jobs).

Cost of the 10 cubes: partition 57 core-hours, MMM 218 core-hours, **275 in all** (uniform mesh: 811, of which 222
in failed jobs; first graded rule: 203). The MMM job spends ≈ 40–50 s in the linear solve, the rest in reading the
mesh and in the output.

**Sizing rule.** The run uses about 21 kB per Gmsh node in total (measured on a side-10 cube, `-sf 0 -p 0`); with
40 % of margin and one core per process, NP ≥ nodes × 21 kB / (0.6 × 1867 MB): up to ≈ 14 M Gmsh nodes with 256
processes, 27 M with 512, 55 M with 1024. (The side-30 graded cube: 532 GB measured.)

**Do not refine in MMM a split mesh.** `-r k` with `-pm 1` gives wrong stresses (maxima inside the matrix, up to
+77 % on some pores of `side10_seed01_meshdouble_refinement`), while `-pm 0 -r 1` matches the gmsh mesh of the same
size; cause not found. On the cluster the `_refinement` folders are skipped.

---

## 9. Results

All on the reference run NFIR at unit pressure. Unless said otherwise: 10 cubes of 30 µm (18 670 pores), mesh
`graded_rfine0.45` at 0.15 µm, δ = 0.15 µm. The verdicts follow Kanit et al. 2003:
$Z_k$ = mean of ŝ over the pores of class $k$ in a cube; $D$ = standard deviation of $Z_k$ between cubes; one cube
gives the mean within $2D/\bar Z$; the RVE is the side at which that is 5 %, with $D^2\propto1/V$ (α = 1). Classes
below 5 % of the pore volume and the FGR levels outside 5–95 % (below 5 %: the tail of the first pores) are reported
but kept out of the verdicts (the FGR range was 10–90 % until 11/11/2026).

### 9.1 First campaign: cubes of 10 µm, uniform mesh 0.3 µm (26–29/10, `results/archive/test_rve_side10_mesh0.3`)

- A 10 µm cube holds 66 pores: none of class 1.70 µm (0.4 expected; 7.8 % of the pore volume) nor of 0.219 µm;
  porosity 0.1007 instead of 0.1138.
- Scatter between cubes 5–22 % on the class means: one cube is not an RVE; RVE side 27 µm (21–40), 18 µm on the FGR (levels 10–90 %, not recomputed).
- Mesh: from 0.4 to 0.3 µm the class means still move by up to 20 %, not monotone: 0.3 µm uniform is too coarse.
- Inverted elements: without the high-order optimisation only seed 1 converged; its 7 inverted elements, all on pores
  of 0.311 µm, gave σ_I/p ≈ 1.65 on three pores against 0.59 with the corrected mesh (other pores < 1 %).
- Refinement inside MMM (`-r 1`, whole mesh) of the 0.6 µm mesh recovers ≈ 93 % of the gap to the 0.3 µm gmsh mesh,
  but is not equivalent at 2 %.
- Against the later side-30 results the 10 µm cubes are biased (class 1.19 µm: +16 % on its mean).

Consequence: side 30 µm, 0.15 µm at the pores, optimisation and check of the curved elements.

### 9.2 Mesh rule: uniform, graded, graded with `R_FINE` (09–10/11)

The same pores (identical bubbles files) on three meshes, δ = 0.3 µm at that time. Class means of ŝ and FGR against
the uniform 0.15 µm mesh (`mesh_compare.txt` in `results/archive/test_rve_side30_mesh0.15_graded_rfine0.45/run_NFIR`
and in `transfers/test_rve_side30_mesh0.15_notuniform.zip`):

| | uniform 0.15 | graded 0.15–0.45 ("notuniform") | graded + rfine 0.45 |
|---|---|---|---|
| vertices | 6.0 M | 2.4 M | 2.47 M |
| classes ≥ 5 % of the volume, largest change of the mean | reference | 0.3 % | 0.10 % |
| FGR levels 5–95 %, largest change of $x_c$ | reference | 0.19 % | 0.27 % |
| class 0.219 / 0.311 µm (0.5 % of the volume) | reference | −4.1 / −0.5 % | −6.4 / −1.4 % |
| per pore: median, 95th percentile of the change | | ≈ 1 %, 4 % | 1.09 %, 4.04 % |
| RVE side on the classes / on the FGR (5–95 %, set by the 5 % level) | 73 / 36 µm | 74 / 36 µm | 73 / 33 µm |
| core-hours, 10 cubes | 811 (222 in failed jobs) | 203 | 275 |
| memory | out of memory with one core (1.9 GB) per process | fits | fits |

![uniform against graded](figures/mmm_mesh_compare_uniform_vs_graded.png)

> `mesh_compare.png` (`mesh_compare.py <uniform ROOT> <graded_rfine ROOT>`): left, ŝ of every pore on the graded mesh
> against the uniform one, with the ±5 % lines (median change 1.1 %, 95 % of the pores within 4.0 %; the few points far
> from the diagonal are pores of the smallest class); centre, the mean FGR of the 10 cubes on the two meshes, which
> overlap within the spread of one cube; right, the core-hours of the 10 cubes (light: mesh split, full: MMM solved,
> hatched: jobs that failed for memory). The change of the class means is in the table above.

Coarser meshes give a higher ŝ, so on the small pores (R/h < 2 with one size) the uniform mesh overestimates and the
`rfine` rule is the most accurate. The uniform mesh at 0.3 µm does not even converge on side 30.
**Verdict: `graded_rfine0.45` replaces the uniform mesh** (tolerance 2 %), at one third of the cost.

### 9.3 Search distance δ: 0.3 → 0.15 µm (10/11)

Same meshes (same split files), only `-dl` changes (`delta_compare.txt`, made with `mesh_compare.py --labels … --name delta_compare --no-cost`).

| class R (µm) | share of volume | ŝ mean, δ = 0.3 | ŝ mean, δ = 0.15 | change |
|---|---|---|---|---|
| 0.219–0.436 | 5.6 % | 1.246–1.281 | same | 0.0 % |
| 0.610 | 22.2 % | 1.2723 | 1.2708 | −0.1 % |
| 0.852 | 38.2 % | 1.2637 | 1.2455 | −1.4 % |
| 1.192 | 26.2 % | 1.3103 | 1.2182 | −7.0 % |
| 1.702 | 7.8 % | 1.4854 | 1.1813 | −20.5 % |

FGR: $x_c$ +8.7 % at FGR 5 % (the largest change in the range 5–95 %), +7.5 % at 10 %, +7.3 % at 20 %, +4.7 % at 40 %,
+1.2 % at 80 %, +0.1 % at 95 % (up to 9 points of FGR between the two curves). RVE
side set by class 1.70: 73 → 56 µm. With δ = 0.3, 4.8 % of the pores had their maximum within 0.075 µm of the wall of
*another* pore (a smaller one in 97 % of the cases; 70 % of the pores of class 1.70); with δ = 0.15, none.

![delta 0.3 against 0.15](figures/mmm_delta_compare.png)

> `delta_compare.png` (`mesh_compare.py`, written with `delta_compare.txt` in the δ = 0.15 folder): left, ŝ of every
> pore with δ = 0.15 µm against δ = 0.3 µm (reference): the pores off the diagonal are those whose maximum was taken
> on a neighbour, almost all of the large classes; right, the FGR of the same 10 cubes. The change of the class means
> is the table above (section 2 of `delta_compare.txt`); its panel was removed on 11/11. To regenerate it:
>
> ```bash
> cd ~/mmm/mm-opera-hpc/polydispersebubble/scripts/      # or the copy in ~/mmm/build/polydispersebubble
> R=~/mmm/results
> python3 mesh_compare.py \
>     $R/archive/test_rve_side30_mesh0.15_graded_rfine0.45/run_NFIR \
>     $R/test_rve_side30_mesh0.15_graded_rfine0.45_delta0.15/run_NFIR \
>     --labels "delta = 0.3 um" "delta = 0.15 um" --name delta_compare --no-cost
> ```
>
> It ends with exit code 1 and "delta = 0.15 um can NOT replace delta = 0.3 um (tolerance 2 %)": that is the verdict
> (the two δ differ by more than 2 %), not an error. The figure of this report is a copy,
> `figures/mmm_delta_compare.png`.

Residual at δ = 0.15: 39 pores (31 of the 110 of class 1.70, 8 of class 1.19) have their maximum at the edge of the
shell (≈ 0.145 µm) towards a small neighbour at ≈ distMin; bound of the effect: class 1.70 ≤ −6 %, FGR $x_c$ ≤ 0.5 %
(1 % at 90 %). The true maxima are within 0.013 µm of the wall (99th percentile), so δ = 0.05 µm would remove the
residual. **δ = 0.15 µm was chosen for the hypercube** and is set in both run scripts.

### 9.4 Mesh size: 0.3 / 0.212 / 0.15 µm on seed 1 (`mesh_test.txt`)

The graded rule at three sizes in the ratio √2, the same 1867 pores; change of the class means against the 0.15 mesh:

| mesh next to the pores | 0.219 | 0.311 | 0.436 | 0.610 | 0.852 | 1.192 | 1.702 | all | per pore: median, largest |
|---|---|---|---|---|---|---|---|---|---|
| 0.212 µm | +1.66 | +0.59 | +0.95 | +1.30 | +1.29 | +1.73 | +1.75 | +1.20 % | 1.76 %, 20.7 % |
| 0.3 µm | +4.05 | +3.00 | +2.49 | +2.37 | +2.95 | +4.12 | +7.65 | +2.72 % | 3.44 %, 41.4 % |

![mesh test](figures/mmm_mesh_test.png)

> `mesh_test.png` (`plot_bubbles.py`): change of the class means of ŝ on the two coarser meshes against the 0.15 µm mesh, with the ±2 % tolerance. 

All the changes are positive (ŝ decreases with refinement) but the decrease does not slow down as a second-order
method would: observed orders 0.7–1.4, class 0.610 not yet asymptotic, so `plot_bubbles.py` refuses the Richardson
extrapolation. The classes of the verdict move by at most 1.75 % from 0.212 to 0.15 µm (tolerance 2 %: passed);
the **error of the 0.15 mesh itself is not measured**, estimated +1.5 … +5 % (ŝ overestimated). FGR: $x_c$ moves by
−0.5 % (onset) to −1.6 % (50–90 %) from 0.212 to 0.15. Part of the error is first order by construction: the
quantity is a maximum over Gauss points, and the point sits ≈ 0.04 h from the wall.

A bias common to all the pores is absorbed by the calibration of $\sigma_R$; what matters for the surrogate is that
it is the same in all the runs, which the rule scaled on the pore radius is meant to give.

### 9.5 Size of the RVE (`rve_size.txt`, `rve_fgr.txt`)

| class R (µm) | pores per cube | share of volume | ŝ mean ± 2D/√n | D between cubes | S between pores | error of one cube | RVE side (95 %) |
|---|---|---|---|---|---|---|---|
| 0.219 | 11 | 0.02 % | 1.246 ± 0.056 | 0.089 | 0.262 | 14.2 % | 60 µm (47–90) |
| 0.311 | 113 | 0.5 % | 1.259 ± 0.014 | 0.023 | 0.260 | 3.6 % | 24 (19–36) |
| 0.436 | 452 | 5.2 % | 1.281 ± 0.006 | 0.010 | 0.255 | 1.5 % | 14 (11–20) |
| 0.610 | 715 | 22.3 % | 1.271 ± 0.005 | 0.009 | 0.252 | 1.3 % | 13 (10–19) |
| 0.852 | 452 | 38.4 % | 1.246 ± 0.005 | 0.008 | 0.230 | 1.4 % | 13 (10–19) |
| 1.192 | 113 | 26.2 % | 1.218 ± 0.010 | 0.016 | 0.207 | 2.6 % | 19 (15–29) |
| 1.702 | 11 | 7.4 % | 1.181 ± 0.047 | 0.075 | 0.180 | 12.7 % | 56 (43–83) |

| FGR level | $x_c=p/\sigma_R$ (mean ± 2D/√n) | p at $\sigma_R$ = 275 MPa | error of one cube | RVE side (95 %) |
|---|---|---|---|---|
| 1 % | 0.514 ± 0.011 | 141 MPa | 7.0 % | 38 µm (29–56) |
| 1.5 % | 0.531 ± 0.007 | 146 | 4.2 % | 27 (21–40) |
| 2 % | 0.544 ± 0.007 | 150 | 3.9 % | 26 (20–38) |
| 5 % | 0.596 ± 0.007 | 164 | 3.5 % | 24 (19–36) |
| 10 % | 0.645 ± 0.005 | 177 | 2.6 % | 19 (15–29) |
| 20 % | 0.705 ± 0.005 | 194 | 2.1 % | 17 (13–25) |
| 40 % | 0.797 ± 0.004 | 219 | 1.5 % | 13 (10–20) |
| 80 % | 0.956 ± 0.005 | 263 | 1.8 % | 15 (12–22) |
| 95 % | 1.057 ± 0.003 | 291 | 0.9 % | 9 (7–14) |

The levels below 5 % are outside the verdict (the tail of the first pores).

![RVE size](figures/mmm_rve_size.png)

> `rve_size.png` (`plot_bubbles.py`): left, the mean of ŝ of every class in each of the 10 cubes, with the spread of
> one cube (±2D) and the uncertainty of the mean of the cubes; right, the side of a cube that gives 5 % on the class
> mean. The third panel (ŝ of every pore against gap / R) was removed on 11/11: with distMin fixed in µm the classes
> sit side by side on that axis, and ŝ correlates better with the gap in µm (Spearman −0.61 over all the pores,
> −0.35 with gap / R). ŝ against the gap in µm is the first panel of `break_order.png` (9.6).

![FGR of the cubes](figures/mmm_rve_fgr.png)

> `rve_fgr.png` (`plot_bubbles.py`): left, the FGR of each of the 10 cubes against $p/\sigma_R$ (top axis: pressure for
> $\sigma_R$ = 275 MPa), their mean and the band ±2D of one cube, with a zoom on the first 30 %; right, the error of one
> cube on the load of every FGR level (grey band: below 5 %, one cube is an RVE), labelled with the side of the RVE;
> hollow markers are the levels below 5 %, outside the verdict.

- **On the FGR curve one cube of 30 µm is an RVE** from FGR 1.3 % upwards (9.2 % with the upper bound of the
  interval): RVE side 24 µm (19–36) for the levels 5–95 %, set by the 5 % level (error of one cube 3.5 %). The best
  estimate is below the 30 µm of the cubes; the upper bound of its interval is not. From 10 % upwards the RVE side is
  below 20 µm.
- **On the class means it is not**, because of the two classes with 11 pores per cube: class 1.70 µm (7.4 % of the
  volume) needs 56 µm (43–83), i.e. the mean of 7 cubes of 30 µm for 5 %. The five central classes are within 1.3–3.6 %
  with one cube.
- $A\times$ pore density is 0.6–1.9 (1 for independent pores): the pores of a class are close to independent, which
  supports α = 1; only one side was run, so α is assumed, not measured.
- With `mmm_surrogate.py` (bootstrap over the cubes): one cube gives μ of ln ŝ within 0.11 and σ within 0.07 on the
  main classes (0.036 and 0.023 with ten), and the critical load of every FGR level within 1.2–1.9 %.

### 9.6 ŝ per class and which pores break first (`break_order.txt`, `shat_NFIR.txt`)

In every class ŝ is closer to a log-normal than to a normal (Kolmogorov–Smirnov 0.03–0.10 against 0.06–0.13). The
table SCIANTIX reads, `mmm_outputs/shat_NFIR.txt`:

| R (µm) | u = ln(R/R_V) | share | pores | μ | σ | mean | q05 | q50 | q95 |
|---|---|---|---|---|---|---|---|---|---|
| 0.219 | −1.206 | 0.0002 | 110 | 0.198 | 0.206 | 1.246 | 0.890 | 1.241 | 1.742 |
| 0.311 | −0.856 | 0.005 | 1130 | 0.210 | 0.199 | 1.259 | 0.921 | 1.207 | 1.745 |
| 0.436 | −0.517 | 0.052 | 4520 | 0.229 | 0.192 | 1.281 | 0.937 | 1.240 | 1.769 |
| 0.610 | −0.182 | 0.223 | 7150 | 0.221 | 0.191 | 1.271 | 0.937 | 1.235 | 1.736 |
| 0.852 | 0.152 | 0.384 | 4520 | 0.204 | 0.176 | 1.246 | 0.944 | 1.205 | 1.675 |
| 1.192 | 0.488 | 0.262 | 1130 | 0.184 | 0.160 | 1.218 | 0.959 | 1.180 | 1.607 |
| 1.702 | 0.844 | 0.074 | 110 | 0.156 | 0.145 | 1.181 | 0.958 | 1.149 | 1.554 |

![s_hat per class](figures/mmm_shat_NFIR.png)

> `shat_NFIR.png` (`mmm_surrogate.py`): top left, μ and σ of ln ŝ of every class against u = ln(R/R_V) (bars: 2
> standard deviations over the cubes, by bootstrap); top right, the FGR of the cubes against the FGR rebuilt from the
> seven log-normals (what SCIANTIX sees); bottom left, the error of a run made of 1 to 10 cubes on the critical load
> of the FGR levels and on μ of the classes; bottom right, the rupture at $P_h$ = 0 with the MMM classes
> ($\sigma_R$ = 275 MPa) against the step of the J19 mean field.

- **The class explains almost nothing, the neighbourhood almost everything.** Out-of-sample R² of ŝ: 0.005 with the
  class alone, 0.92 with the neighbourhood (gaps and radii of the two nearest pores, number of close pores,
  $F=\sum_j(R_j/d_{ij})^3$). Spearman correlation with the gap to the nearest pore −0.43 … −0.76 inside a class. The
  mean of ŝ varies by 8 % over a factor 8 in radius, with a slow decrease above 0.44 µm (after the δ correction).
- **First pores to break**: at FGR 1 % the broken pores have a median gap of 0.34 µm to the nearest pore (0.48 for
  all the pores; distMin = 0.30) and ŝ ≥ 1.94; at 10 %, 0.36 µm and ŝ ≥ 1.55. The onset of the release is therefore
  set by distMin. Broken pores form clusters: the largest holds 16 % of the broken volume at FGR 1 %, 5 % at FGR 50 %.
- The broken volume follows the volume shares of the classes at every level: no class leads once δ is right (with
  δ = 0.3 the largest class led the onset).
- **Closure of the reduction**: the FGR rebuilt from the seven log-normals against the FGR of the cubes differs by at
  most 4.4 points of FGR; critical loads within 2.6 % (5 %), 1.5 % (10 %), 0.2 % (25 %), 1.8 % (50 %), 1.2 % (90 %).
  The only approximation is the log-normal shape.

![break order](figures/mmm_break_order.png)

> `break_order.png` (`break_order.py`): left, ŝ of every pore against the gap to its nearest pore in µm, with the
> median of all the pores and the line at distMin; right, the broken share of every class against
> $p/\sigma_R$ (solid) with its log-normal fit (dashed), i.e. the $b^*_k$ SCIANTIX uses. The bottom row (FGR stacked
> by class, largest cluster of broken pores per cube) was removed on 11/11: those numbers are in section 2 of
> `break_order.txt` and in the bullets above.

### 9.7 n-point probabilities and fragment size (`npoint.txt`; δ = 0.3 results, not redone with δ = 0.15)

Idea: the probability that $n$ points of a segment are all in the intact phase tends, for $n\to\infty$, to the lineal
path $L(z)$; $1-L(z)$ is the probability that a segment of length $z$ is cut by a broken pore. Broken pore = sphere of
radius $(1+k)R$, $k$ a free parameter for the cracked zone.

- Pores: $S_2$ reaches $\xi^2$ by ≈ 2.5 µm (no long-range order); mean matrix chord 8.4 µm (exact: $4(1-\xi)/s$ = 8.6).
- **Broken pores come in close pairs**: P(both broken) over the same for pores broken at random within their class,
  at gaps ≈ distMin: 14.6 at FGR 10 %, 6.4 at 25 %, 3.0 at 50 %; ≈ 1 above ≈ 0.7 µm. SCIANTIX assumes random within
  the class.
- Fragment size: with $k=0$ the mean intact chord is $4(1-\phi_b)/s_b$ whatever the arrangement, ≈ 4.3 times the
  intercept $2d/3$ of the SCIANTIX fragment $d=2R_{WS}/D$ at every $D$ (e.g. $D$ = 49 %: 18.6 µm against 4.1 µm).
  SCIANTIX matches a cracked zone of $k$ ≈ 0.75–1, i.e. ≈ $R_{WS}$ (1.51 µm). For $k>0$ the grouping makes the intact
  chords 15–25 % longer than random.
- Limit: lines of 30 µm cap the chords above ≈ 15 µm ($k=0$ measurable only for $D$ ≥ 75 %).

![n-point](figures/mmm_npoint_delta0.3.png)

> `npoint.png` (`npoint.py`, on the δ = 0.3 results): top, the two-point probability of the pores, the n-point
> probabilities of the matrix up to the lineal path, and the ratio of broken pairs against the gap; bottom, the lineal
> path of the intact phase at the FGR levels and the mean intact chords (number- and length-weighted) against the
> fraction of broken pores, for cracked zones (1 + k) R, with the SCIANTIX fragment size for comparison.

### 9.8 SCIANTIX with the MMM table: open-loop replay of the NFIR anneal (`section_mmm_replay`, run again on 10/11)

The table of run NFIR is used whatever the ξ and s of the SCIANTIX state, and the venting does not act back on the
state (open loop). Release of the transient in % of the gas produced; measured Kr-85: 29.8 %.

| | $\sigma_R$ (MPa) | D | G | release |
|---|---|---|---|---|
| C++, mean field J19 | 21 | 0.709 | 0.522 | 41.9 % |
| MMM table | 150 | 0.844 | 0.714 | 56.7 % |
| MMM table | 200 | 0.630 | 0.454 | 36.2 % |
| MMM table | **219 (fit)** | | | **29.8 %** |
| MMM table | 250 | 0.434 | 0.269 | 21.4 % |
| MMM table | 275 (D5.2) | 0.354 | 0.205 | 16.3 % |
| MMM table | 300 | 0.288 | 0.156 | 12.4 % |

![replay](figures/classes_fgr_nfir_mmm.png)

> `classes_fgr_nfir_mmm.png` (`pore_distribution.py`, `section_mmm_replay`): left, D and G on the heat-up for the
> C++ (J19 mean field) and for the MMM table at 275 MPa and at the fitted $\sigma_R$; right, the release of the
> anneal against the Kr-85 measured (Noirot 2014). This copy was drawn on 30/10 with the δ = 0.3 table (fit ≈ 221
> MPa); the numbers of the table above are those of the δ = 0.15 table (219 MPa), computed without redrawing it.

So $\sigma_R$ = 275 MPa of D5.2 (monodisperse pores at the measured pressure) gives half of the measured release
here; 219 MPa matches its final value. This is a fit of one number on one test with the *modelled* pore state (0.7
times the measured gas density), not a validation.

---

## 10. How to proceed

Status of the hypercube on 10/11/2026: `Merope/mmm_files/lhs_side30_mesh0.15_graded_rfine0.45/` holds, for R00–R19,
one unit cube (seed 1: pores, `.geo`, summary) and its pk twin; gmsh is running on the other PC; nothing is split or
solved. `run_mmm.sh` and the batch script of the cluster already find any `bubbles_<name>.txt` and follow
`mesh_case.txt`, so they run both loads without changes.

| # | decision | options | what the results say | recommendation |
|---|---|---|---|---|
| 1 | **Load of the hypercube runs** | (a) unit only: ŝ depends on (ξ, s), 2 factors; (b) unit + pk ("plan B"): ŝ with the class pressures, 3 factors (ξ, s, $\bar\eta$) | The pk cubes cost one more MMM job on the same split (≈ 22 core-h each), no mesh. With $a=2$ the class pressures of run NFIR span 54–7647 MPa: the neighbours' load is far from uniform, so (a) may miss a first-order effect. But (b) is not ready: the superposition of the two maxima (unit run and pk run) is approximate, `mmm_surrogate.py` and the criterion $b^*_i$ are written for one pressure | Run both loads (the cost is small), build the surrogate on unit first, and use pk on run NFIR to measure how much the class pressures change the FGR curve before investing in (b) |
| 2 | **Cubes per run** | 1 cube per run (20 cubes) or more seeds for the wide runs | FGR 5–95 %: one 30 µm cube gives ≈ 2–6 % (the 1.4–4.3 % estimated for 10–90 % by scaling run NFIR with the number of pores, times 3.5/2.6 for the 5 % level; not recomputed run by run): just above 5 % at the 5 % level for the runs with the fewest pores (R01, R17, R13). The largest class with ≥ 5 % of the volume needs more for 5 %: R13 11 cubes, R05 7, R09 6, R11 5, NFIR/R04 4, R00/R03 3 | 1 cube per run, pass the per-class noise ($2D$, from the number of pores of the class) to the fit; add seeds only where the surrogate shows it needs them. Densify by doubling the hypercube (new runs appended), **not** by raising `MMM_RUNS`, which rewrites the whole design |
| 3 | **δ** | keep 0.15 µm, or 0.05 µm | residual at 0.15: ≤ 6 % on class 1.70, ≤ 0.5–1 % on the FGR | keep 0.15 for the hypercube (chosen 10/11); δ only needs a new MMM job on the same split if it is revisited. In dense runs 38 % of the pores have a gap < 0.35 µm (17 % in NFIR): keep δ < distMin/2 |
| 4 | **Error of the 0.15 µm mesh** | (a) accept; (b) fourth mesh at 0.106 µm on seed 1 (≈ 7 M vertices: gmsh on the big PC, split ≈ 40 GB, MMM with 2 cores per process); (c) change the quantity (stress on the wall, or an average over a small volume, instead of the maximum over Gauss points) | not asymptotic at 0.15; estimated +1.5 … +5 %, mostly common to all the pores | (a) for the hypercube, since a common bias goes into $\sigma_R$; (b) once, before publishing numbers of ŝ; if 0.610 still moves by ≈ 1.4 %, go to (c) |
| 5 | **distMin** | keep 0.3 µm; or one sensitivity run (0.2, 0.4 µm) on NFIR | the onset of the release is made by pores at gaps ≈ distMin; D5.2 never tested it; the hypercube keeps 0.3 µm while $R_V$ is fixed, so distMin/$R_V$ is constant in the design but not in SCIANTIX (0.75 $R_V$ at the modelled radius) | one sensitivity on run NFIR (2 × 1 cube, ≈ 55 core-h) before the surrogate is frozen; report the onset (FGR < 10 %) as uncertain meanwhile |
| 6 | **Surrogate** | Gaussian process of μ and σ of ln ŝ on (ξ, s, u) [+ $\bar\eta$ with plan B], tabulated on a regular grid for a linear interpolation in the C++ | on run NFIR μ varies by 0.07 and σ by 0.06 over the classes: a smooth, weak dependence on u; the 7 log-normals reproduce the FGR within 4.4 points | as planned in `mmm_surrogate.py`; entry point `HighBurnupStructureFragmentation.C` l. 483–552 (the step `broken` becomes $b^*_i$); the Python transcription already has it (`broken_fraction`) |
| 7 | **$\sigma_R$** | 275 (D5.2) or recalibrated | 219 MPa fits the NFIR release in open loop | recalibrate after the surrogate, in closed loop, and on more than one test (Hiernaut 2008 is digitised in `regression/hbs_hiernaut`) |
| 8 | **Fragment size** | keep $d=2R_{WS}/D$, or use the MMM geometry | SCIANTIX's $d$ corresponds to a cracked zone ≈ $R_{WS}$; broken pores are grouped | redo `npoint.py` with δ = 0.15; test $k_i$ = gap/R from `pores.csv` (no free parameter) |

Order proposed: finish the meshes → line `To check` of `run_gmsh.sh` (if not 0, `inverted_elements.py` on the flagged cubes; the dense runs R14, R00, R18, R19 are the likely ones) → split + unit runs on the cluster
(≈ 20 × 27 core-h; the wall mesh goes from 0.23 to 1.65 times that of NFIR with the number of pores) → pk runs →
`break_order.py` and `mmm_surrogate.py` per run → surrogate → C++.

Scaling to check before the surrogate is used at the SCIANTIX radius: side, distMin, δ and mesh are fixed in µm at
$R_V$ = 0.732 µm; ŝ depends on their ratio to the pore size.

---

## 11. How to run

### 11.1 Environment (every new terminal)

```bash
source ~/mmm/env.sh                  # mfem-mgis, MFront, OpenMPI of Spack: to build AND to run MMM
source ~/mmm/Merope/Env_Merope.sh    # only to generate cubes with Mérope
```
Never inside a pipe (`source env.sh | tail` runs in a sub-shell). If `env.sh` prints "mfront non viene da Spack" the
environment is not loaded. `env.sh` removes the Dakota and miniconda libraries from `LD_LIBRARY_PATH`.

### 11.2 Build and check

```bash
cmake --build ~/mmm/build -j 8                                        # also copies scripts/ into the build folder
ctest --test-dir ~/mmm/build -R PolyBubbleTest --output-on-failure    # 5 s
# only after deleting ~/mmm/build or adding a source file:
cmake -S ~/mmm/mm-opera-hpc -B ~/mmm/build -DCMAKE_BUILD_TYPE=Release
cmake --build ~/mmm/build --target lib-elasticity-poly                # without -j (MFront)
```
It needs mfem-mgis@master. Edit the scripts in `mm-opera-hpc/polydispersebubble/scripts/`, never the copies of the build.

| build error | cause | fix |
|---|---|---|
| `undefined reference to PMIx_...` | Dakota MPI libraries from `~/.bashrc` | `source ~/mmm/env.sh` in a new terminal |
| `undefined reference to curl_...@CURL_OPENSSL_4` | `libcurl` of miniconda | same |
| `mfront: command not found`, TFEL or MFEMMGIS not found | environment not loaded | same |
| `invalid --jobserver-auth` | MFront built with `-j` | build `lib-elasticity-poly` without `-j` first |

### 11.3 RVE test or hypercube, from the pores to the tables

```bash
cd ~/mmm/Merope/mmm_files
python3 test_rve.py                                   # cubes of the RVE tests (settings at its top) → test_rve_side30_mesh0.15_graded_rfine0.45/
python3 merope_pore_pressure.py [R05 R13] [--seeds 3] # cubes of the hypercube → lhs_side30_mesh0.15_graded_rfine0.45/unit/
python3 ~/mmm/build/polydispersebubble/set_pressures.py lhs_side30_mesh0.15_graded_rfine0.45/unit   # → pk/
bash run_gmsh.sh <folder>                             # meshes; skips those already done (FORCE=1 to redo)
python3 inverted_elements.py <cube folder>            # only if run_gmsh.sh ends with "To check" > 0: must end with "0 with inverted elements"

cd ~/mmm/build/polydispersebubble
bash run_mmm.sh ~/mmm/Merope/mmm_files/<folder>       # this PC: copies the cubes to ~/mmm/results/<folder>, solves the unsolved ones
                                                      # NPROC=8 POST=0 bash run_mmm.sh <cubes> [<results>]
R=~/mmm/results/<folder>/run_<id>
python3 plot_bubbles.py $R                            # mesh_test, rve_size, rve_fgr; exit 1 = not an RVE or mesh not fine (not a crash)
python3 break_order.py $R                             # pores.csv, break_order.txt/png
python3 mesh_compare.py <reference ROOT> <other ROOT> # two meshes of the same cubes; two δ: add --labels A B --name delta_compare --no-cost
python3 npoint.py $R [RAYS]                           # ≈ 10 min at 1e5 lines per cube
python3 ~/mmm/sciantix-official/utilities/HBFF/mmm_surrogate.py $R    # mmm_outputs/shat_<id>.txt/png
```

Cubes already listed in `cubes.txt` are kept: more seeds or a mesh size added to `MESH_SIZES` add their cubes only.
The same pores on another mesh: `cmp` of the two `bubbles_*.txt` must print nothing.

Memory on PC-EC (WSL sees 16 GB): a side-10 cube at 0.3 µm (336 k Gmsh nodes) takes ≈ 7 GB whatever the number of
processes; anything at 0.15 µm on side 30 goes to the cluster. One run at a time. A process killed ≈ 1 minute after the
launch (`exited on signal 9`) is out of memory (`dmesg | grep -i "killed process"`).

"solve: the non linear solver did not converge" means the PCG did not reach the tolerance: first check the mesh for
inverted elements (`inverted_elements.py`), then `-v 1` and `-li 20000`.

### 11.4 SCIANTIX side

```bash
cd ~/mmm/sciantix-official && ./utilities/HBFF/runHBFF.sh     # build, NFIR case against its gold, transcription check, figures
```
**Warning:** `pore_distribution.py` (called by `runHBFF.sh`) rewrites `mmm_inputs/` at every run. With the same
settings the files are identical; if `MMM_RUNS`, `MMM_SEED`, `MMM_CLASSES` or `MMM_FACTORS` change, the design of all
the runs changes and the cubes already made no longer match (`merope_pore_pressure.py` and `set_pressures.py` detect
it and stop).

### 11.5 Cluster runs

The side-30 cubes are solved on an HPC cluster in two batch jobs per cube: `partition-mesh` on one process (with
enough cores reserved for ≈ 15 GB), then `test-polybubble -pm 1` on as many MPI processes as mesh files (512 for the
graded 0.15 µm mesh, about 1 GB each). The batch script, the commands of the machine and its settings are not part
of this repository (private notes of the author). What does not depend on the machine:

```bash
./partition-mesh -m <cube>/mesh_unit_<id>.msh -np 512 -o <cube>/mesh_par512/mesh_par.     # the folder must exist
mpirun -n 512 ./test-polybubble -l <build>/polydispersebubble/src/libBehaviour.so -pm 1 \
    -m <cube>/mesh_par512/mesh_par. -f <cube>/bubbles_unit_<id>.txt -sf 0 -dl 0.15 -v 1 -p 0
```

What to check in the log: `PCG Iterations` (a few tens), `Newton iteration 1 … ||r||/||r_0||` ≈ 1e-10,
`[End] memory footprint` below the memory reserved; no `nan` in `bubbles_and_stresses_selected.txt`. For the
analysis, copy `bubbles_and_stresses_selected.txt`, `sig_hydro.txt` and the job logs of every cube next to its
`bubbles_*.txt` and `summary_*.txt` under `~/mmm/results/<folder>/` (the meshes are not needed; `mesh_compare.py`
reads the cost from the SLURM summary at the end of the `.err` files).

| message | cause | fix |
|---|---|---|
| `Checkpoint file ... not found` | MPI processes ≠ mesh files | split again with the number of processes of the run |
| job gone, no `[End]` in the log | error, time limit or memory | more time, more memory per process or more processes |
| `the non linear solver did not converge` | inverted elements, or too few iterations | `inverted_elements.py`; `-li 20000` |

---

## 12. Errors and inconsistencies found

Found while writing this report (10/11/2026). "Fixed" = changed in the working tree, not committed.

| # | what | where | effect | status |
|---|---|---|---|---|
| 1 | The cluster jobs of the latest results ran with 2 cores per process (1024 cores, 8 nodes), while the batch script and the documents said 512 cores on 4 nodes | batch files in the results | the documented cost was half the real one | fixed in the script (private) and in the numbers of section 8 |
| 2 | `delta_compare.txt/png` was an output of `mesh_compare.py` used to compare two δ on the same meshes: its labels said "mesh" and carried the folder names, which squeezed the axes of the figure until it was unreadable, and its cost line (493 against 275 core-hours) counted both MMM jobs of every cube | results folder, `scripts/mesh_compare.py` | misleading table, broken figure | fixed: `mesh_compare.py REF OTHER --labels A B --name delta_compare --no-cost`; table and figure regenerated on 10/11 (same numbers). Its block "5. Cost" is still printed and still counts both jobs for δ = 0.15; the verdict line "can NOT replace" only means that the two δ differ by more than 2 % |
| 3 | The batch files and logs inside the latest folder carry the path `…_delta0.1/` (the folder was renamed to `_delta0.15` after the runs); `delta_compare.txt`, `rve_*.txt`, `mesh_test.txt` carry the path of the build tree where they were made | results folder | none on the numbers; the runs really used `-dl 0.15` (checked in the logs: `--delta 0.15`) | left as records |
| 4 | The latest folder holds, in every cube, the logs of two MMM jobs: `-dl 0.3` (09:25) and `-dl 0.15` (10:45); the outputs are those of the second | results folder | `mesh_compare.py` doubles the MMM cost there (item 2) | noted |
| 5 | Mesh convergence not shown: Richardson not possible, class 0.610 not monotone; error of the 0.15 µm mesh not measured | `mesh_test.txt` | ŝ overestimated by an estimated 1.5–5 % | open (section 10, decision 4) |
| 6 | Residual of the search distance at δ = 0.15: 39 pores take the maximum at the edge of the shell | `break_order.py` on the latest folder | class 1.70 up to 6 % too high | open, accepted (decision 3) |
| 7 | Plan B: the zero-stress correction of the pk runs adds two maxima (pk run and unit run) that are not at the same point (`plot_bubbles.py` prints their distance), never tried on a solved cube; `mmm_surrogate.py`, `shat_<run>.txt` and the criterion $b^*_i$ assume one pressure in all the pores | `plot_bubbles.py`, `mmm_surrogate.py`, `pore_distribution.py` | the pk runs cannot be reduced for SCIANTIX yet | open (decision 1) |
| 8 | `shat_NFIR.txt`, `rve_fgr.txt` and `mmm_surrogate.py` quote $\sigma_R$ = 275 MPa, calibrated in D5.2 on monodisperse pores at the measured pressure | headers and `SIGMA_R_MMM` | with our table 275 MPa gives 16.3 % release instead of 29.8 %; 219 MPa fits | open (decision 7) |
| 9 | `-r k` with `-pm 1` gives wrong stresses (up to +77 %) | `test-polybubble` / mfem-mgis restart mode | refinement in MMM unusable on the cluster | open, cause not found; the script skips those folders |
| 10 | The MMM log prints 2.78 M "vertices" where the mesh has 2.47 M | `print_mesh_information`, `-pm 1` | none (shared vertices summed over the parts) | explained (section 6) |
| 11 | SCIANTIX gas columns do not close on the NFIR case: "Xe in HBS pores" is 2.1 × "Xe produced in HBS"; the release of the transient is 0.799 × the vented fraction (the replay uses this factor from the C++) | `output.txt` of the case | the release of the replay relies on an empirical factor | open, to understand in the C++ |
| 12 | The open-loop replay uses the table of run NFIR (ξ = 0.114, s = 0.36) for a SCIANTIX state with ξ = 0.12–0.13, s = 0.24–0.25 | `section_mmm_replay` | the fitted $\sigma_R$ is indicative | by design until the surrogate exists |
| 13 | This README pointed to `README_HBFFmodel.md` (deleted on 20/10) and to `figures/mmm_design.png`, which does not exist | old README; `presentation/mmm_roadmap.tex` l. ≈ 200 includes `mmm_design.png`, `classes_moment_errors.png`, `classes_heatup.png`, none in `figures/` | the roadmap slides do not compile from a clean tree | README fixed; slides not touched (as asked) |
| 14 | The slides of 14/10 quote an older state (s = 0.33, $R_V$ = 0.35 µm, ξ = 0.105, release "39 % of the measured") against the current gold (s = 0.24, $R_V$ = 0.40 µm, ξ = 0.130, release 41.9 % against 29.8 % measured, i.e. 1.4 times) | `presentation/mmm_roadmap.tex` | slides out of date | not touched |
| 15 | Defaults pointing to old cases: `run_mmm.sh` (side-10 folder inside the build), `plot_bubbles.py` ROOT (`test_rve_side10`) | `scripts/` | a run without arguments worked on old data | fixed: `run_mmm.sh CUBES [RESULTS]`, default ROOT = latest results |
| 16 | The same in the Mérope scripts: `run_gmsh.sh` has an absolute `GMSH=/home/ecappellari/.local/bin/gmsh` and a default folder that is not on this PC; `generate()` writes by default in `mmm_inputs/<run>_seed_…`; the docstring of `merope_pore_pressure.py` cites an old cheat sheet; `test_rve.py` looks for the run files in a `../mmm_inputs` that no longer exists (fallback) | `Merope/mmm_files/` | none while the arguments are given | **left untouched on purpose** while gmsh runs on the other PC; to fix afterwards |
| 17 | Results lived inside the CMake build tree; the build folder had hand-edited, older copies of `plot_bubbles.py`, `break_order.py`, `run_mmm*.sh`, three scripts deleted from the repository, and no `set_pressures.py` | `~/mmm/build/polydispersebubble` | `rm -rf build` would have erased the results; the latest tables were made with the older copies | fixed: results in `~/mmm/results`; build refreshed. The tables were recomputed with the current scripts on a copy: **identical numbers**, only the wording of two header lines differs |
| 18 | `pores.csv` of the latest folder was written by the older `break_order.py` (22 columns); the current one writes 26 (`p_MPa`, `p_ref_MPa`, `s_hat_1`, `max_shift_um`) | results folder | none: same values in the 22 common columns, same `shat_NFIR.txt` | fixed on 11/11: `break_order.py` rerun, `pores.csv` now has the 26 columns |
| 19 | `npoint.py` was run with δ = 0.3 only | `results/archive/…_graded_rfine0.45/run_NFIR/npoint.txt` | the pair ratios at the onset may change with δ = 0.15 | open (≈ 10 min) |
| 20 | The old cheat sheets pointed to scripts deleted on 27/10 (`fgr_population.py`, `check_msh.sh`, `run_vtk.sh`) | archived cheat sheets | — | folded into section 11, corrected |
| 21 | Cubes of the current RVE test (`.geo`, `.msh`) are not on PC-EC: `Merope/mmm_files/test_rve_side30_mesh0.15_graded_rfine0.45` exists only on the other PC and on the scratch space of the cluster (emptied periodically) | — | the meshes of the reference results could be lost; they can be regenerated (same seeds) | open: copy the `.geo` at least |
| 22 | `*.png` is git-ignored in sciantix-official, so the figures of this report are not versioned; `mmm_outputs/` and `mmm_surrogate.py` are untracked | `.gitignore` l. 98 | figures missing on a fresh clone | open |
| 23 | Mérope places the integer part of the expected pores per class | `summary_*.txt` | porosity 0.1131 against 0.1138; largest class 7.4 % of the volume instead of 7.8 % | by construction of the RSA call; noted in the tables |
| 24 | The search of the maximum is not periodic: for a pore cut by a face of the cube (12.4 % of the pores; 28 % of class 1.70 µm) only the points on the side of its centre are searched, not the part of its wall that re-enters from the opposite face | `src/Bubble.cxx`, per-pore search (distance to the centre without periodic images) | the maximum of those pores can be missed. Measured on the latest results: class means of the cut pores against the others −0.1 … +0.8 % (not significant), class 1.70: −3.6 % ± 6.0 % (31 pores) | open: found on 10/11 while writing; small, but a periodic distance in the search would remove it |

---

## Appendix A — literature review

Sources in `letteratura/fragm/`. Items marked "own calculation" are derivations from paper formulas and need re-checking.
Equation and figure numbers are those of the papers. The equations implemented are in section 3 (option 4) and in
`archive/README_options123.md` (options 1, 2, 3) and are not repeated here. The review is common to the four options.

## B1. Sources

| source | scale / object | type |
|---|---|---|
| Jernkvist, EPJ N 5 (2019) 11, doi:10.1051/epjn/2019030 (J19) | grain-face bubbles and HBS pores, full chain in FRAPCON/FRAPTRAN, LOCA and RIA | integrated engineering model |
| Jernkvist, Prog. Nucl. Energy 119 (2020) 103188 (J20) | grain-face bubbles; 7 criteria + 1 proposed | analytical review |
| Kulacsy, JNM 466 (2015) 409 (K15) | HBS pores, LOCA | mechanistic, pore classes |
| Khvostov, NED 328 (2018) 36 | grain boundary and HBS, RIA | model (FALCON/GRSW-A) |
| Khvostov, NET 52 (2020) 2402 | grain boundary, LOCA with ballooning | analytical criteria + fragment size |
| Aagesen et al., JNM 557 (2021) 153267 | single HBS bubble, phase-field | response to a transient |
| Gamble et al., JNM 556 (2021) 153163 | macroscopic thermal radial cracks | XFEM, comparison with correlations |
| Gencturk et al., Materials 18 (2025) 1162 | pellet, thermal-gradient fractures | stochastic phase-field |
| OperaHPC D5.2 (2026), §2.2 | RVE with HBS pores, NFIR-V | meso-scale, HPC |
| NEA/CSNI/R(2016)16 | FFRD, data and state of knowledge | synthesis report |

## B2. What "fragmentation" means (NEA §3)

- *Pellet cracking*: thermal cracks, fragments 500 µm – mm; *fragmentation*: 100–500 µm; *pulverising / fine fragmentation*:
  a few µm to 100 µm, specific to high burnup.
- All the fine fragmentation observed occurs where bubbles visible in optical microscopy exist (HBS and the central
  precipitation rings), not only in the HBS. The rim fragments are acicular, the central ones are not (NEA §3.1.3). Only the
  HBS is treated here.

## B3. Approaches and models

### B3.1 Empirical thresholds

NFIR (Turnbull, Yagnik 2014-15): 71 MWd/kgHM and 645 °C. EPRI (Yueh 2014): threshold in burnup and last-cycle power. NRC:
size distribution linearly interpolated between 55 and 72 MWd/kgU, zero burnup gives only fragments above 4 mm.
Kulacsy–Molnar: below 66 MWd/kgU and 600 °C no micro-fragmentation; above 66 MWd/kgU, local release 20% for
600–1050 °C and 90% above 1050 °C (NEA §7.2.2.1). Preliminary NEA thresholds (Table 6.4-1): burnup above 65–70 MWd/kg,
cladding strain above 5–10%, $T>750$ °C. Limit: no link with the pore state.

### B3.2 Analytical rupture criteria, grain-face bubbles (J20)

All give the critical bubble gas pressure $P_g^{cr}$ with $P_h$, $P_s$ and an idealised geometry (periodic array of equal
lenticular bubbles, semi-dihedral angle $\theta=35$–$55°$, $V_b=\tfrac{4\pi}{3}\tilde\eta(\theta)r_b^3$ with
$\tilde\eta=\frac{2-3\cos\theta+\cos^3\theta}{2\sin^3\theta}$; above $\varphi_2\gtrsim0.2$ the bubbles coalesce and the
lenticular shape fails).

| criterion | type | $P_g^{cr}$ | verdict of the review |
|---|---|---|---|
| Gruber 1973 | local stress | $P_s+\dfrac{\sigma^{cr}_{gb}+P_hF_2}{F_1}$ | discarded: $F_1,F_2$ wrong (spherical pore: $F_1=\tfrac12$, $F_2=\tfrac32$); no fractional coverage |
| Olander 1997 | mean stress in the ligament | $P_s+\dfrac{\sigma^{cr}_{gb}(1-\varphi_2)-\sigma_\infty}{\varphi_2}$ | **suitable**; used in SCANAIR, FALCON, RANNS; valid for HBS pores with $\varphi_2\to\varphi_3$ (Delesse) |
| DiMelfi–Deitrich 1979 | LEFM, vermicular bubble | $P_h+\dfrac{G_{gb}\sin\theta}{2r_b(1-\cos\theta)}$ | discarded: ruptures without heating ($T/T_0<1$) |
| Harwell (Finnis; Matthews 1990) | LEFM | $P_h+P_s+\dfrac{1}{F_3}\sqrt{\dfrac{\pi E G_{gb}}{(1-\nu^2)r_b}}$, $F_3\in[2,2.61]$ | unsuitable: no dependence on $\varphi_2$ |
| Chakraborty–Tonks–Pastore 2014 | LEFM + 2D FE, $\theta=50°$ | $P_h+P_s+\dfrac{1}{F_4}\sqrt{\dfrac{\pi E G_{gb}}{(1-\nu^2)r_b}}$, $F_4=\pi(0.568\varphi_2^2+0.059\varphi_2+0.5587)$ | **suitable**; restricted to $\theta=50°$ |
| Likhanskii–Matveev 1999 | LEFM, stable cracks | 3 equations, Boyle/VdW | discarded: rupture easier with growing $\varphi_2$ is inverted |
| Worledge 1980 (original) | total energy | $\dfrac{G_{gb}}{h_c\left[1-\frac{3h_c(l^2-r_b^2)}{8r_b^3\tilde\eta}\right]}$ | unsuitable: independent of $P_h$ |
| Worledge improved (J20) | total energy, $\delta V=\tfrac{4l^3}{3\mu}(P_g-P_h)$ | Newton–Raphson | **suitable**; unphysical for $\varphi_2<0.25$ |
| Irwin 1957 (penny crack) | LEFM | $P_h+\tfrac12\sqrt{\dfrac{\pi E G}{(1-\nu^2)r_b}}$ | comparable with Harwell/CTP |

Lessons: (i) stress criteria give reasonable results with the bulk strength, LEFM criteria with the bulk $G$ overestimate the
strength by about 2 orders of magnitude ($T/T_0\gtrsim20$ against 2–3 observed): the local $G\ll$ the bulk $G$; (ii) the trends
with $r_b$ and $\varphi_2$ diverge between the two families (stress: small bubbles break first; LEFM: large ones); (iii) the
pre-accident $P_h$ is a first-order parameter ($T/T_0\propto1/(P_h+P_s)$); (iv) local data for $\sigma_{gb}$ and $G_{gb}$ are
missing.

### B3.3 Mechanistic HBS pore model: Kulacsy 2015

- Pressure: equilibrium $p_{eq}=P_h+2\gamma/r$ (Eq. 1); punching limit $p_{ex}=Gb/r$ (Eq. 2); maximum
  $p=P_h+2\gamma/r+Gb/r$ (Eq. 3), $b=0.39$ nm; typical values 20–40 MPa ($P_h$), 5–0.5 MPa (capillarity), 300–15 MPa
  (punching) for $r=0.1$–$2$ µm.
- Tangential stress (Olander 1980): $\sigma_t=p-(\tfrac32P_h+2\gamma/r)$ (Eq. 7); at the maximum pressure
  $\sigma_t=Gb/r-\tfrac12P_h$ (Eq. 8), read on the PDF (p. 412). J20 notes that the factor on the net pressure is 1 in
  Eq. 7 while it is $\tfrac12$ for a spherical pore.
- Distribution: log-normal, $m=-0.5$, $s=0.356$ ($r$ in µm), average of data at 60–150 MWd/kgU, no clear trend with burnup.
- EoS: Ronchi for Xe (tabulated), van der Waals at low pressure; at high initial pressure the ideal gas gives pressures close
  to Ronchi with more than 3 times the gas, van der Waals much higher pressures.
- Predictions: minimum stable pore in irradiation (0.27–0.33 µm), progressive fragmentation (small pores first), suppression
  by the transient $P_h$ (FGR against $T$ for $P_h=0.1,10,20$ MPa and tension), lower burnup threshold with high last-cycle
  power. §4.1.3 gives 0.32 µm at 0.1 MPa and 0.33 µm at 10 MPa and says the minimum size "increases with decreasing
  restraint": the numbers contradict the statement. The porosity of the fictive sample is not stated.
- Validation: Hiernaut 2008 anneal (0.5 K/s; outer part 160 MWd/kgU, $\xi=17\%$, 800 K and 60 MPa assumed at end of life;
  inner part 105 MWd/kgU, $\xi=10\%$, 900 K and 40 MPa; Hiernaut's own paper gives 240 and 160, K15 re-evaluates them from the burnup profile):
  outer release complete at 900 K measured / 1000 K calculated, inner at 1500 K in both; one measured curve for the whole sample (K15 Fig. 9).
- Declared limits: all pores assumed at punching; not all pores burst (fragments of a few hundred µm contain intact pores);
  no pore interaction; heating rate ignored.

### B3.4 Integrated model: Jernkvist 2019 (parameters in Tables 1–4)

- Grain-face bubbles: Speight–Beeré with $L(\varphi_2)=\varphi_2-\tfrac{3+\varphi_2^2+2\ln\varphi_2}{4}$; White
  $n_b(A_p)=n_b^0\,[1+2n_b^0(A_p-A_p^0)]^{-1}$, $n_b^0=3\times10^{13}$ m$^{-2}$, $A_p^0=2.83\times10^{-15}$ m$^2$; venting at
  $\varphi_2=0.48$; rupture with C-T-P (Eq. 18) and $G_{gb}=4\times10^{-3}$ J/m$^2$; microcracking at $\upsilon P^{cr}$ ($\tau=2$ s,
  healing if $P_g<\upsilon P^{cr}$).
- HBS: formation threshold (Eq. 19) and $T<1400$ K,
  $$E_{th}=\frac{2.94\times10^{4}}{\varphi_f^{2/15}}\left(\frac{S_0}{10^{-5}}\right)^{1/10}\ \text{MWd/kgU};$$
  pores of one size, born at $r_0=0.5$ µm with $P_g=P_h+2\gamma/r_0+\Delta P_0$, $\Delta P_0=40$ MPa; $N_p$ linear up to
  $10^{17}$ m$^{-3}$ at 100 MWd/kgU, no Ostwald ripening; matrix Xe (Eq. 20)
  $$C_g(E)=C_g^\infty+(C_g^0-C_g^\infty)\exp\!\left[-\frac{C_g^0}{C_g^\infty}\left(\frac{E}{E_0}-1\right)\right],\quad C_g^\infty=150\ \text{mol/m}^3;$$
  80% of the gas leaving the matrix goes to the pores, 20% to the free volume; growth by punching (Eq. 21)
  $$\frac{\mathrm dr_p}{\mathrm dt}=k_{dp}\,\langle P_{ex}-c_{dp}/r_p\rangle,\quad k_{dp}=10^{-17}\ \text{m/(s Pa)},\ c_{dp}=55\ \text{N/m},$$
  stopping at $\xi=0.29$ (percolation); rupture Eq. 22; $P_h=\tfrac23(P_{gap}+P_{contact})$ in steady state (Eq. 23) and, in
  transient, minus $c_{th}\alpha_lE_Y|\partial T/\partial r|$, $c_{th}=2\times10^{-5}$ m (Eq. 24).
- Results: IFA-650.9 (90 MWd/kgU): pulverisation from about 50 s after blowdown at about 800 K (porosity 24%), from 5 to 39%
  at once on loss of $P_h$, 41% final; IFA-650.10 (61): about 10% pulverisation and TFGR 1.1%, $\xi_{HBS}<10\%$. RIA: CIP0-1
  TFGR 15% measured, 9.3% calculated; REP-Na11 6.8% against 3.2%. TFGR grows above about 60 MWd/kgU because the rim is
  wider and more porous.
- Declared shortcomings: hydrostatic pressure estimated, not calculated; grain-boundary release underestimated in the
  non-restructured fuel (lenticular geometry, fabrication pores neglected); simplified intragranular diffusion; volatile Cs
  not considered.

### B3.5 Khvostov (GRSW-A/FALCON)

- 2018 (RIA): $R_F=\sigma_d/\sigma_{B,gb}\ge1$, $\sigma_d=\Delta P_g\,f/[1-(f+f_v)]$ (Lemoine 2000 / Olander),
  $\sigma_{B,gb}=1.7\times10^{8}(1-2.62f_{n0})^{1/2}\exp[-1590/(RT_{SST})]$ (MATPRO FFRACS); release after rupture with Poiseuille
  law, $\tau_1=1$ s, $\tau_2=0.1$ s, fraction of opened pores $k_1=0.25$; trapping by pellet–cladding bonding. Fig. 2 draws the
  spherical HBS pores explicitly.
- 2020 (LOCA): fragmentation as a prerequisite of the burst; $a_{frag}=0.9249\,R_g/f_{vn}$ (Eq. 4); condition
  $a_{frag}\le k_Br$, $P_{con}=0$, $k_B\Delta G/R_0>0$; burst threshold $[R_F]_{LOCA}\approx0.35$ against 1.0 in RIA, calibrated
  on IFA-650.12; $\tau_1=\tau_2=50$ s (against 0.1–1 s in RIA). FGR 13.8% calculated against 13.3% estimated (650.12), 19%
  measured (650.14).

### B3.6 Meso-scale: OperaHPC (MMM, CEA)

Periodic RVE 30 µm, about 1200 spherical pores of Ø 1.75 µm, $\xi\approx11\%$, minimum distance 0.3 µm, 32M elements, 1024 MPI
processes; linear elastic, zero macroscopic strain, hence a compressive $P_h$. Criterion: first principal stress at the pore
above $\sigma_R=275$ MPa (single calibrated parameter). FGR $=\sum V_{broken}/\sum V$. Maximum stress about $1.5\,p_{in}$
(3 times the isolated pore; Biot: +20%; interaction 1.2–3). Scaling law:
$p^c_{in}\leftrightarrow(p_{in}-P_h)/(\sigma_R+P_h)$. Pore pressure from the measured atomic density, hard-sphere/VdW EoS. Ten
random realisations give close curves. MMM does not evolve the microstructure.

### B3.7 Phase-field

Aagesen 2021: KKS with U vacancies and Xe, surface tension for arbitrary curvature, covers isolated pores and the merging of
two. Bubble $R=500$ nm, $p_0=100$ MPa in irradiation: the pressure falls with growth but stays above equilibrium. LOCA
(700→1400 K at 5 K/s, $R=250$–$1000$ nm, $p_0=200,100,50$ MPa from punching, $P_{ext}=0,30,60$ MPa): the radius does not change,
$p\propto T$ (VdW at constant density), also with boundary diffusivity ×$10^4$. No fracture model yet (future work).
Gencturk 2025: fracture as a stochastic phase transition (Allen–Cahn with ±1% noise on the elastic energy),
$\sigma_f=200$ MPa, $E=200$ GPa, $\nu=0.33$, $l=0.3$ mm; pellet-scale cracks from thermal gradients ($k(T,Bu)$ of Wiesenack):
it does not model the pore pressure. Useful as an idea (distributed strength), not as an HBS mechanism.

### B3.8 Macroscopic thermal fractures: Gamble 2021

Correlations for the number of radial cracks: Barani 2017 $n_f=1+11[1-\exp(-(q'-5)/21)]$ (0 if $q'<5$ kW/m); Walton–Matheson
$n_f=0.8(Bu+3.3\langle q'-6\rangle)$; Coindreau $n_f=\min[n^0_f+(16-n^0_f)Bu/50,\,16]$. XFEM with random tensile strength
(uniform ±2.5% or volume Weibull); the uncertainty envelopes the correlations; the strength randomisation is the most
influential parameter. Relevance: interface with the "macro" sizes (mm cracks), not with fine fragmentation.

### B3.9 NEA/CSNI/R(2016)16 (experimental synthesis)

Facts that constrain the model:

- Burnup threshold 60 MWd/kgU (Halden, segment average), 55–69 (Studsvik); transition to fine fragmentation between 60 and
  80 MWd/kgU; significant dispersal only above about 80 MWd/kg.
- Threshold temperature about 750 °C (EPRI); visible fragmentation at about 900 °C, but release seen by the rod pressure
  already at about 650 °C fuel temperature; anneal at 0.2 °C/s: bulk of the release at 1110–1200 °C; at 20 °C/s more
  pulverisation.
- $P_h\gtrsim40$–$60$ MPa suppresses the release at 1500 °C (NFIR); cladding strain above 5–10% needed for visible fine
  fragmentation; relocation from about 2–10%.
- Bubbles are not interconnected: after one breaks, the others keep loading the matrix; a 40 µm HBS fragment at 1300 °C did not
  fragment further, so pressurisation alone does not explain everything (§3.1.3).
- HBS pore pressures: 65–80 MPa at 377 °C; EXAFS 2–4 GPa and TEM 1.6–15 GPa for nano-bubbles at 427 °C (a different
  population); CEA calculation (MARGARET) at 700 °C: about 71 MPa in HBS pores against 227 MPa in grain-face bubbles and
  7.28 GPa in intragranular ones.
- No dependence on oxidation, quench or burst (apart from the loss of $P_h$).
- The role of Cs and volatile fission products is unresolved.

Additional facts used by the regression cases (NEA/CSNI/R(2016)16):

- **IFA-650.9 rod pressure** (Fig. 2.1-6, read by eye): about 73 bar before the burst, 62 bar at the burst, then a slow decay (18 bar at 25 s, 12 bar at 60 s, 9 bar at 90 s); three pellets stayed in contact with the cladding,
  hydraulic diameter about 25 µm (Fig. 2.1-7). Jernkvist uses 5.9 → 0.3 MPa. Not used in the cases.
- **Typical IFA-650 transient** (Fig. 2.1-2, unspecified rod): blowdown at 0 s, cladding temperature 179 °C minimum at 79 s, ballooning at about 250 s, failure at 298 s, scram at 432 s, peak about 860 °C; rod pressure 3.5 MPa → 0.4 MPa at the failure. Used for both rods.
- **Rim temperature** (Fig. 3.3-1, IFA-650.4 and Studsvik 192): base-irradiation rim 300–330 °C (last cycle); about 730 °C at the failure; fuel temperature at the burst about 670 °C, 350 °C above the base-irradiation rim.
- **Preconditioning** (§2.1.1): reactor at 15 MW (about 85 W/cm) for some hours, then the test at 4 MW and 10–30 W/cm.
- **Hydrostatic pressure** (§3.3.4, §3.4): FGR and fragmentation of 71 MWd/kgU samples at 1500 °C are inhibited above about 40–60 MPa (NFIR); the HBS at the periphery of EPRI samples with intact cladding did not fragment
  (§2.2.2); a Halden sibling pair with and without burst has similar fragmentation (§3.3.3).
- **Onset temperatures** (§3.3.1, §6.1.2.2): 550–850 °C (Studsvik, about 70 MWd/kgU, cladding with slit), about 640 °C (NFIR), about 750 °C (EPRI), about 900 °C visible in Halden with little strain, FGR onset about 500 °C cladding / 650 °C fuel.
- **HBS** (Appendix 9.1): porosity 10–15 %, pore pressure 65–80 MPa at 377 °C, Xe outside the pores 0.1–0.2 wt%, width 70–300 µm at 60 MWd/kgU average, growing with burnup (Fig. 9.1-1); formation from 60 MWd/kgU local.
- **Last cycle** (§3.2.2): a higher last-cycle LHGR enhances fragmentation (Studsvik 189/192).

## B4. Parameters and gaps

| parameter | values | source |
|---|---|---|
| $\sigma^{cr}_{hbs}$ (ligament) | 21 MPa | J19 |
| $\sigma_f$ bulk MATPRO | $170\sqrt{1-2.62\phi}\,e^{-191.34/T}$ MPa | K15, Khvostov 2018 |
| $\sigma_R$ micro (RVE) | 275 MPa | OperaHPC |
| $\sigma_f$ (micro-mechanics) | 200 MPa | Gencturk 2025 |
| $\sigma^{cr}_{gb}$ bulk | 55 MPa | J20 |
| local $G_{gb}$ | $4\times10^{-3}$ J/m² (bulk 2 J/m²) | J19, J20 |
| $c_{dp}$ (punching) | 55 N/m | J19 (from Gao 2013) |
| $Gb/r$ | 55 MPa at 0.5 µm ($G=70$ GPa, $b=0.39$ nm) | J19 |
| $\Delta P_0$ at formation | 40 MPa | J19 |
| $\xi$ percolation | 0.29 (J19), 0.25–0.30 (percolation), 0.18 (SCIANTIX) | J19, code |
| burnup threshold | 60–65 (segment) | NEA, J19 |
| $R_F$ critical | 1.0 (RIA), about 0.35 (LOCA) | Khvostov 2018/2020 |
| emptying time | 0.1–1 s (RIA), 50 s (LOCA) | Khvostov 2018/2020 |
| $k_{open}$ | 0.25 | Khvostov 2018 |
| HBS pore pressure at 25–377 °C | 30–80 MPa | NEA |

**Main gaps**

1. The pre-transient pressure is not computed by a growth model in the works with rupture criteria: Kulacsy puts it at
   punching, MMM derives it from the data, Jernkvist uses an empirical model. None of the three has gas–vacancy–coalescence
   dynamics.
2. No coupling of HBS formation, pores and fracture ($\alpha$ diluting $D$, new material intact).
3. The local strength ($\sigma$, $G$) is not measured: it is calibrated case by case (21, 200, 275 MPa; $G$ differing by 2
   orders of magnitude).
4. Pore distribution: only Kulacsy uses it; Jernkvist and MMM use a mean pore or a random arrangement.
5. The fragment scale is not predicted (only pulverised fraction or release).
6. $P_h$ depends on the host code and the PCMI model and dominates the result (J19: dependence $\propto1/(P_h+P_s)$).
7. Conflict on the dependence on $R$: stress ⇒ small pores first; LEFM ⇒ large pores first (J20); no data discriminates.
8. The physical limit of fragmentation (40 µm fragment stable at 1300 °C) has no criterion.
9. Role of volatile fission products (Cs) and of nano-bubbles: open.
10. Fragmentation outside the HBS (grain-face bubbles, precipitation rings): treated in the integrated models (J19,
    Khvostov) with simplified bubble dynamics.

## B5. Experimental data catalogue

The sources are those cited in the papers read; the numbers are those reported there and must be re-checked on the original
works.

**Separate effect, priority 1 — anneal of a uniform HBS**

- **NFIR-V, IFA-649 disc, 103.5 GWd/tHM** (OperaHPC §2.2.1, NEA Fig. 2.3-8): pores $\xi\approx11\%$, Ø 1.75 µm, gas volume
  per atom $1.518\times10^{-28}$ m³, ramp 0.2 °C/s to 1200 °C, online $^{85}$Kr release up to about 30%, sharp threshold; main
  peak between 1110 and 1200 °C. Uniform microstructure, so the 0-D model applies without radial approximations. Already used
  by MMM ($\sigma_R=275$ MPa). Read from OperaHPC Fig. 4–6 (accuracy about 0.5 percentage points): weighted mean diameter
  1.72 µm, standard deviation about 0.55 µm (Fig. 4); cumulated release 0.1% at 870 °C, 0.8% at 930, 2.4% at 990, 5.7% at 1050,
  11.8% at 1130, 16.7% at 1160, 22.8% at 1190 °C, 30% at 1200 °C (Fig. 5–6); pressure derived from the atomic density: 185 MPa
  at 870 °C, 245 MPa at 1190 °C.
- **Hiernaut et al. 2008** (Knudsen cell, HBS sample about 200 MWd/kgHM, 0.5 K/s): release in three steps, 330–530 °C (1%),
  630–730 °C (20%), 1130–1230 °C (70%); fragments 200×300 µm after 650 °C, 10–20 µm after 1230 °C (NEA §3.3.1.1); used by
  Kulacsy 2015 with an outer part ($r/r_0=0.94$–$1$, 160 MWd/kgU, $\xi=17\%$) and an inner part (0.82–0.94, 105 MWd/kgU,
  $\xi=10\%$); Hiernaut's own burnups are 240 and 160. It contains the dependence on $\xi$. Digitised in `regression/hbs_hiernaut/data/MeasuredRelease.txt`.
- **CEA (Noirot et al.)**: 83 and 71.8 MWd/kgU, ramps 0.2 and 20 °C/s to 1200 °C, samples with and without a cladding slit;
  a 140 MWd/kgU HBS fragment at 1330 °C without further fragmentation (constraint on a lower size).
- **EPRI/Studsvik tube furnace (Yueh et al.)**: start 550 °C, end 850 °C, threshold 750 °C, about 20 °C/s; last-cycle power
  effect (samples 189 above/below).
- **NFIR (Turnbull, Yagnik)**: threshold 71 MWd/kgHM and 645 °C; pulverisation suppressed by $P_h\gtrsim40$–$60$ MPa at
  1500 °C. Validates the dependence on $P_h$ of the criteria.
- Une et al. 2002, 2005, 2006 (JNST): release from high burnup under rapid heating (cited by K15 and J19; to be retrieved).

**Prerequisite — microstructure and pore pressure**

- `regression/hbs` (Cappia, Spino, Noirot, Walker, Manzel): $N_p$, $R_p$, $\xi$, Xe depletion. Gate: no shift of the golds.
- Pore pressure: 65–80 MPa at 377 °C for 62 and 78 MWd/kgU (NEA §9.1); 30 MPa at 25 °C, 90 MPa at 650 °C, 150 MPa at 1230 °C
  from Knudsen (NEA §3.3.1.2); CEA calculation at 700 °C about 71 MPa; Cagna 2016 (SEM-SIMS-EPMA).

**Integral tests (phase 3; need local $T$ and $P_h$ from a performance code)**

- Halden IFA-650.4/.5/.9/.10/.7/.12/.13/.14: FGR 13.5% (650.12) and 19% (650.14, no burst); fragment size distributions of
  650.12–.14 (NEA Table 2.1-1).
- Studsvik NRC 189, 191–193, 196, 198: size distributions; at least 60% by mass below 1 mm above 75 MWd/kgU, almost only above
  2 mm at 55–60.
- ANL ICL No.2; FLASH-5 (HBS fragments below 20 µm and above 100 µm); MIR/LOCA-50/60/72 (VVER).
- RIA: CABRI REP-Na11 (TFGR 6.8%) and CIP0-1 (15%); NSRR FK-1/2, LS-1/2/3; TFGR against enthalpy (J19 Fig. 16).
- Validity limit: SCIANTIX is 0-D; integral tests need one run per radial position with local histories from FRAPCON/FRAPTRAN,
  TRANSURANUS or OFFBEAT (couplings already in the OperaHPC project).

**Metrics**: (a) $T$ (or $p$) of release onset and its slope against NFIR-V/Hiernaut; (b) released fraction at the end of the
ramp; (c) $d$ against the scales above; (d) sensitivity to $P_h$ (0, 20, 40–60 MPa); (e) comparison of the three options and
of the full J19 chain.

Each entry must report: local burnup, irradiation $T$, $P_h$, ramp, atmosphere, restraint, measurement (online release,
size distribution, ceramography). Primary references to retrieve: Hiernaut 2008 (JNM 377, 313), Une 2002/2005/2006, Turnbull
2015 (Nucl. Sci. Eng. 179, 477), Yueh 2014 (WRFPM), Flanagan 2013 (NUREG-2160), Puranen 2013, Oberländer–Wiesenack 2014,
Bianco 2015, Noirot 2008/2014, Cagna 2016, Spino 2006, Nogita–Une 1995, Walker 2005/2009, Cappia 2016.
