# NotebookLM prompt — UN model state of the art + experimental data request

## SHORT VERSION (paste this into NotebookLM)

---

We have built a Python implementation of a UN fission-gas-behaviour model
based on Rizk 2025 (JNM 606, 155604) with these upgrades:

- Spectral 3-equation intragranular solver for `(c, m_b, m_d)` (Rizk's "correct
  approach" Eq. 3.34 of the 2023 NEAMS report).
- Trapping/resolution per Rizk Eq. 8, 22, 23 (typo `ρ_d²` on `g_d` corrected).
- Homogeneous nucleation `ν_b ∝ f_n D_g c²` (Eq. 26) with `f_n` **recalibrated
  to 3×10⁻⁶** vs Ronchi 1978 (vs Rizk's U₃Si₂-inherited 1×10⁻⁶).
- φ-correction (Olander 2006) extended to the gas balance, not only to
  `dN_b/dt` as in Rizk.
- `D_v = D_1` thermal + **D_3 athermal from Schneider 2024**
  (`A_30^V_U = 4.96×10⁻⁴¹ m⁵`).  D_3 for U vacancies is forced to zero in
  BISON SIFGRS — we add it.  Rizk Tab. 2 D_2_v parameters are
  mathematically broken and dropped.
- Inter-granular grain-face bubbles per Rizk §A.2 + Eqs. 39, 40, with
  `F_c,sat = 0.5` triggering FGR.
- Solid fission product swelling (Rizk Eq. 19, 0.5·B per FIMA).
- Coalescence via implicit-Euler quadratic, not the analytical Eq. 15.

**Performance**: RMSE 0.98 % Sw on 39 Ronchi 1978 dislocation-swelling
anchors at 1.1 / 1.3 / 3.2 % FIMA.  Total swelling at T=1600 K, 1.3 % FIMA
is 4.58 % (3.93 % gas + 0.65 % solid), in line with Rizk Fig. 6.

**Data we need from your bibliography** (please scan all uploaded sources
and report citations + figure/table + conditions + data type):

(A) **FGR measurements** in UN or (U,Pu)N beyond SP-1 / SNAP50 / JOYO
    already mediated by Rizk 2023/2025.  Look in Wallenius 2022,
    Storms 1988, Tanaka 2004, Grachev 2020, IAEA TECDOC 2003,
    Ekberg 2018, Matthews 1993 (LA-UR-93-2392).

(B) **Microscopic swelling and bubble microstructure** (N_b/N_d, R_b/R_d)
    vs T or burnup beyond Ronchi 1978 and Rizk Fig. 7-8.  In particular,
    what does **Kosmidou 2025** (Kr-irradiated UN, in-situ TEM) report
    quantitatively, and at what conditions?

(C) **Total / radial / linear swelling** correlations and data.  What
    functional form does **Ross 1988** use, what is its calibration
    range and uncertainty?  Anything in Honda 1969, Storms 1988?

(D) **Integral pin tests** beyond Rizk's summaries.  For SP-1 (NBU-2/3
    EBR-II), SNAP50 (capsule 5-742), JOYO L413/L414 (U,Pu)N — what do
    the original papers report (T, fission rate, burnup, FGR, ΔL/L,
    geometry, cladding)?  Any other UN / (U,Pu)N pin tests
    (BR-2, BOR-60, K4 EBR-II, Phenix nitride…)?

(E) **Other UN-specific physics or data** that might be missing: He
    production, restructuring (grain growth, central voids), volatile FP
    swelling beyond Eq. 19, dislocation density `ρ_d` measurements
    (Ray-Blank 1985 is UC; any UN-specific data?), material constants
    (`γ_b`, `δ_gb`, Ω) from direct measurement.

For each entry give: paper + page/figure/table + experimental conditions
(T, fission rate, burnup) + data type (table to read directly, scatter
to digitise) + estimated effort.

---

## LONG VERSION (kept as reference)

## CONTEXT — what we have built

We have implemented in Python a mechanistic fission-gas-behaviour (FGB) model
for uranium nitride (UN), baselined on Rizk et al. 2025 (J. Nucl. Mater. 606,
155604) with several improvements. The model is parameter-free and modular,
with all literature constants in a single source-of-truth file.

**Intra-granular physics** (per Rizk 2025 §3 and Appendix A.1):

- Three reservoirs evolved per timestep: gas atoms in solution `c`, in bulk
  bubbles `m_b`, in dislocation-bound bubbles `m_d`.
- Spectral 3-equation diffusion solver (spherical grain, backward Euler) for
  the joint `(c, m_b, m_d)` system — Rizk's "correct approach" (Eq. 3.34 of
  Rizk 2023 NEAMS).
- Trapping rates `g_b`, `g_d` from Ham/Smoluchowski (Rizk Eq. 22, 23). The
  printing typo on `g_d` (extra `ρ_d` on numerator) is corrected.
- Re-solution rates `b_b`, `b_d` via Matthews 2014 / Nelson 1969 form
  (Rizk Eq. 8).
- Homogeneous bulk nucleation `ν_b ∝ f_n · D_g · c²` (Rizk Eq. 26), with
  `f_n` recalibrated to **3×10⁻⁶** from a fine logarithmic scan against
  the Ronchi 1978 dataset (the U₃Si₂-inherited Rizk default of 1×10⁻⁶
  systematically over-predicts at 1.3 % FIMA).
- φ-correction (Olander 2006 / Pizzocri 2020) applied consistently in **both**
  bubble-count and gas-balance equations — Rizk applies it only in
  Eq. 21c (dN_b/dt), not in Eqs. 21a-b. This is the single largest
  improvement over the published Rizk model (RMSE drops from ~2.0 to
  ~1.0 % Sw on Ronchi 1978).
- Mass-coupling `±2ν_b` between `c` and `m_b` (Pizzocri 2020 mass
  conservation when dimers nucleate).
- Vacancy ODE (Rizk Eq. 21f, Speight-Beere) integrated implicitly via
  a quadratic backward-Euler step.
- Coalescence on dislocation bubbles `dN_d/dV_d = −4λN_d²` (Rizk Eq. 11)
  via implicit Euler, NOT via the analytical Eq. 15 (which assumes constant
  λ over the timestep — only valid in the limit dt → 0).

**Diffusivities**:

- `D_Xe = D_1 (thermal) + D_3 (athermal mixing)`, parameters from Rizk
  2025 Tab. 2.  D_2 (irradiation-enhanced) is computed as a diagnostic
  but not summed — Rizk's own statement that "D_2 is negligible for Xe"
  is verified by Centipede (the curve sits ~85 OOM below D_1 + D_3
  in the temperature range 800–2200 K).
- `D_v_U = D_1 (thermal, Rizk Tab. 2) + D_3 (Schneider 2024 athermal)`
  with `A_30^{V_U} = 4.96×10⁻⁴¹ m⁵` (from D_3 = 2.48×10⁻²² m²/s at the
  Schneider/Matzke reference F = 5×10¹⁸ fiss/m³/s).  We **drop** Rizk's
  Tab. 2 D_2 vacancy parameters, which are mathematically broken
  (yielding ~10⁻⁶ m²/s at 1500 K, ≥14 OOM unphysical).
- We **add** the D_3 athermal contribution to D_v that Rizk 2023/2025
  forces to zero in BISON SIFGRS (the A_3 cell for V_U is empty in
  Rizk Tab. 2).  This is a documented physics upgrade, becoming dominant
  at T < 1300 K where the thermal D_1 collapses by orders of magnitude.

**Inter-granular physics** (per Rizk 2025 §A.2 + Eqs. 39, 40):

- Grain-face lenticular bubbles, semi-dihedral angle θ = 59°, area-density
  N_gf = 2×10¹³ m⁻² of grain face (Rizk Tab. 1).
- Vacancy diffusion in GBs `D_v_gb = 10⁶ × D_1_v_thermal` (Rizk Tab. 1, from
  UO₂ ref [48]).
- Sink-strength geometric factor `ζ_gf = −[(3-F_c)(1-F_c) + 2 ln F_c]/4`
  (Rizk Eq. 39).
- Lenticular volume-radius relation
  `R_gf = (3 V_gf / [4π·g(θ)])^{1/3}` with `g(θ) = 1 - 1.5 cosθ + 0.5 cos²θ`
  (Rizk Eq. 40).
- Coverage `F_c = π N_gf R_gf² sin²θ`; saturation at `F_c,sat = 0.5`
  (Rizk Tab. 1, from UO₂).  At saturation the bubble is capped and
  excess gas is released to the plenum (FGR).

**Solid fission-product swelling** (Rizk Eq. 19): we are about to add
`(ΔV/V)_solid = 0.5 · B` where B is burnup in % FIMA.

## Performance vs Ronchi 1978

39 anchor points across 1.1 / 1.3 / 3.2 % FIMA, microscopic dislocation
swelling Sw_d:
- RMSE = 0.98 % Sw, bias = −0.06 % Sw
- Per-burnup max |bias| = 0.33 % Sw (essentially symmetric)

## DATA REQUEST — what we need

Please scan the entire UN bibliography I have uploaded and report data points
suitable for further model validation, organised by category. For each item,
cite the source (paper, page/figure/table), the experimental conditions
(fuel composition, T (K) range, burnup % FIMA or MWd/kgHM, fission rate,
linear power, cladding, geometry), and how to access the values (table to
read directly, scatter plot to digitise, etc.).

### A — Fission gas release (FGR) measurements

We currently have only the SP-1 / SNAP50 / JOYO data mediated by Rizk 2023
and 2025.  Beyond those:
1. Are there UN or (U,Pu)N FGR measurements vs T or vs burnup in
   Wallenius 2022, Storms 1988, Tanaka 2004, Grachev 2020, IAEA TECDOC 2003,
   Ekberg 2018, Carvajal-Nuñez 2014, Ross 1988, Honda 1969, Baranov 2016,
   Juárez 2019, ElJamal 2023, Matthews 1993 (LA-UR-93-2392)?
2. For each campaign that includes FGR, list: pin ID or specimen, fuel
   composition (UN, (U,Pu)N, mixed nitride…), nominal T (centerline / surface
   / average), end-of-life burnup, measured FGR (%), FGR measurement
   technique, and the original paper that reports the value.
3. Note any "Vitanza-curve-style" plot for UN (FGR threshold vs T at fixed
   burnup, or vs burnup at fixed T).

### B — Microscopic swelling and bubble microstructure (intra-granular)

We have Ronchi 1978 (mixed nitride at 1.1 / 1.3 / 3.2 % FIMA, dislocation
bubble swelling vs T) and the N_d / R_d datasets digitised from Rizk 2025
Fig. 7-8.  Beyond those:
1. Are there other microscopic swelling vs T datasets in UN (or (U,Pu)N) at
   different burnups in Ronchi's other papers, Ray-Blank 1985, IAEA TECDOC
   2003, Tanaka 2004, Honda 1969, Ross 1988?
2. Bubble number density (N_b, N_d) and bubble radius (R_b, R_d) measured
   by TEM or REM — beyond Ronchi 1978? Specifically what does **Kosmidou
   2025** (Kr-irradiated UN dislocation evolution by in-situ TEM) measure,
   and at what conditions? Are the data digitisable?
3. Any data on grain-face / inter-granular bubble density and size?

### C — Total volumetric / radial / linear swelling

For pin-level model validation we need total swelling (gas + solid + thermal).
1. What functional form does the **Ross 1988** UN swelling correlation take?
   What is its calibration dataset, range of validity (T, burnup), and
   reported uncertainty?
2. Does **Honda 1969** (porosity / elastic modulus) report swelling vs
   irradiation?
3. Are there in-pile swelling-vs-burnup curves for UN or (U,Pu)N pins in
   IAEA TECDOC 2003, Storms 1988, Ekberg 2018, Wallenius 2022, Tanaka 2004,
   Grachev 2020?

### D — Integral pin assessments

For integral validation we want to reproduce Rizk-style FGR + swelling vs
end-of-life conditions for whole pin tests.
1. For SP-1 (NBU-2 / NBU-3, EBR-II): what does the original Matthews 1993
   LA-UR-93-2392 report (T, F, burnup, FGR, ΔL/L)?
2. For SNAP50 (capsule 5-742): what does Rizk 2023 cite as the original
   source?  Are details available beyond Rizk's summary?
3. For JOYO L413/L414 ((U,Pu)N): what is the original reference?  Is the gas
   release vs burnup time history available, or only the end-of-life value?
4. Other historical UN / (U,Pu)N fuel pin tests (BR-2 mixed nitride, K4 EBR-II,
   BOR-60, JOYO besides L413/L414, Phenix nitride, …) — what data are
   available?

### E — Other physics / data we may be missing

1. Helium production in UN: relevant alpha decay sources, He generation rate,
   measured He release?
2. Restructuring (grain growth, central-void formation) in UN — any
   morphological vs T,F data?
3. Cesium and other volatile fission products — any swelling contribution
   beyond the Eq. 19 0.5·B that Rizk uses?
4. Any UN-specific ρ_d (dislocation density) measurements vs burnup
   beyond Ray-Blank 1985 (UC) and Kosmidou 2025?
5. Material properties critical to the model (γ_b surface tension, GB
   thickness δ_gb, Wigner-Seitz cell volume Ω) — any paper that reports
   directly-measured values vs Rizk-published estimates?

## DELIVERABLE

A structured report with sections A-E. For each entry: paper citation +
location (page/figure/table) + conditions (T, F, burnup) + data type +
estimated digitisation effort.  Flag any source that contradicts Rizk
2025 / 2023, or that suggests a UN-specific calibration different from
the U₃Si₂ / UO₂-inherited values.
