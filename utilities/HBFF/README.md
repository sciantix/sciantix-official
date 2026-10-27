# HBFF — fine fragmentation of the high-burnup structure
#### Author of this folder: E.Cappellari, POLIMI-CEA 2026

`iHighBurnupStructureFragmentation = 4` applies a rupture criterion to 81 pore classes rebuilt from the pore state of the HBS porosity model,
and returns the fraction of broken pores, the burst release of their gas and the mean fragment size.

| file | content |
|---|---|
| `README_HBFFmodel.md` | the model: equations, parameters, results on the NFIR anneal, limitations |
| `pore_distribution.py` | Python transcription of the option, with its checks, the figures of `README_HBFFmodel.md` and the inputs for MMM |
| `runHBFF.sh` | build, run of the regression case against its gold, check of the C++ against the transcription, figures and tables |
| `mmm_inputs/` | one text file per MMM run of the Latin hypercube in the three factors of an RVE, porosity, width of the radii and mean packing fraction (`run_R00.txt` … `run_R19.txt`: radius, porosity, pressure and confining pressure of every pore class), and `index.txt`, written by `pore_distribution.py` with the figures `figures/mmm_design.png` and `figures/mmm_runs.png` |
| MMM runs | the scripts of the MMM runs are not in this folder. Geometry and mesh, in the repository of Mérope (`mmm_files/`): `merope_pore_pressure.py` throws the pores of a run file of `mmm_inputs/` in a periodic cube and writes them with their pressure (`bubbles_<run>.txt`), the geometry for gmsh (`mesh_<run>.geo`) and the summary per class (`summary_<run>.txt`); `test_rve.py` the cubes of the tests of the RVE (size of the cube, size of the mesh); `run_gmsh.sh` the P2 meshes; `inverted_elements.py` the check of the curved elements. Runs and analysis, in mm-opera-hpc (`build/`): `run_mmm.sh` runs MMM on every cube, `plot_bubbles.py` the mesh test, the FGR of every cube and the size of the RVE (Kanit et al. 2003) |
| `data/` | measured HBS pores read by `pore_distribution.py`: volume per gas atom against the local burnup (`AtomicVolume*.txt`, Noirot 2014 Fig. 17), pore sizes of the NFIR disc before and after the anneal (`PoreRadius*.txt`, Noirot 2014 Fig. 12), and Dowek 2021 Tables 1–5 (`PorePressureCOLLECTION.txt`, `Dowek2021_T*.txt`: molar volumes, single-bubble pressures and radii) |
| `archive/` | options 1, 2, 3 (Jernkvist 2019, Kulacsy 2015, LEFM) and the comparison of the four options |
| `abstract_MRS_HBFF/`, `presentation/` | abstract and slides |

The literature review below is common to the four options.

## Input

The option is an entry of `input_settings.txt` read by name, placed after the positional entries:
```
4    #    iHighBurnupStructureFragmentation (0= off, 1= Jernkvist (2019), 2= Kulacsy (2015), 3= LEFM (Irwin), 4= rupture per pore class)
```
It needs `iHighBurnupStructurePorosity` = 2 or 3. Temperature and hydrostatic stress come from `input_history.txt`
(4th column, MPa, negative in compression): $P_h=-\sigma_h$.

## Regression case

`regression/hbs_nfir/test_UO2HBS_frag_j19damage`: the NFIR disc (103 GWd/tHM), base irradiation and anneal to 1200 °C, with
`iHighBurnupStructureFormation = 1` (Barani et al. 2020) and `iHighBurnupStructurePorosity = 3`. The data and their source are in `regression/hbs_nfir/data`.

- Base irradiation: 903 K, $2\times10^{19}$ fiss/(m³ s), 38 000 h, confining stress rising linearly from 0 to 70 MPa (the rule of
  `regression/hbs`; no datum for the disc, an unknown of the case). The history gives it in 25 intervals of 200 steps: a step of 7.6 h.
- Unloading: 36 s in which the temperature falls to 20 °C, the power to zero and the stress to zero.
- Anneal at $P_h=0$: the measured temperature history (Fig. 9 of the source) in 26 intervals of 200 steps.

Gates of the case:

1. gold file: `python3 -m regression.runner --hbs_nfir.j19damage` (`output.txt` has 10 429 rows);
2. transcription: `python3 utilities/HBFF/pore_distribution.py` replays `regression/hbs_nfir/test_UO2HBS_frag_j19damage` and compares the C++ with the
   Python at every step, and the states that `README_HBFFmodel.md` quotes with the output;
3. options 1, 2, 3 do not use the pore classes: their outputs on the NFIR history are identical byte by byte before and after a change of option 4.

`./utilities/HBFF/runHBFF.sh` does 1 and 2 and draws the figures and the tables; its choices (build, run, re-bless the gold) are the three variables at its top.
`python3 regression/hbs_nfir/plot.py j19damage` draws the comparison with the data into `regression/hbs_nfir/figures/j19damage`.

---

# Literature review

Sources in `letteratura/fragm/`. Items marked "own calculation" are derivations from paper formulas and need re-checking.
Equation and figure numbers are those of the papers. The equations implemented are in `README_HBFFmodel.md` (option 4)
and in `archive/README_options123.md` (options 1, 2, 3) and are not repeated here.

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
