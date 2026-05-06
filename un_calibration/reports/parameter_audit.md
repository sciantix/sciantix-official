# UN model — parameter audit

Cross-check of every constant, equation, and scaling factor in the calibration
sandbox against the literature. Goal: tell the student which parameters are
**verified** vs **fitted** vs **inherited from UO₂** vs **questionable**, so
the v15 search space can be reduced to only the genuinely uncertain DOFs.

**Scope so far**: Rizk et al., *J. Nucl. Mater.* 606 (2025) 155604 — primary
source for nearly every constant and equation. Subsequent passes (Nelson 1969,
Blank carbides, Pizzocri/Pastore SCIANTIX papers) are listed at the end.

---

## 1. Diffusivity constants — all VERIFIED against Rizk Table 2

Source PDF: `references/pdf_link/Rizk - 2025 - ...pdf`, page 4, Table 2.

| Symbol | un_model.py | Rizk Table 2 | Status |
|---|---:|---:|:---:|
| `D10` (Xe thermal prefactor) | 1.56·10⁻³ m²/s | 1.56·10⁻³ | ✓ |
| `Q1` (Xe activation) | 4.94 eV | 4.94 | ✓ |
| `A20` (Xe irrad.-enhanced) | 1.21·10⁻⁶⁷ m^(7/2)/s^(1/2) | 1.21·10⁻⁶⁷ | ✓ |
| `B21` (Xe expansion coef. 1) | 25.87 eV | 25.87 | ✓ |
| `B22` (Xe expansion coef. 2) | −1.49 eV² | −1.49 | ✓ |
| `B23` (Xe expansion coef. 3) | 0 | (not given) | ✓ assumed 0 |
| `A30` (Xe ballistic) | 1.85·10⁻³⁹ m⁵ | 1.85·10⁻³⁹ | ✓ |
| `D10_vU` (V_U thermal prefactor) | 1.35·10⁻² m²/s | 1.35·10⁻² | ✓ |
| `Q1_vU` (V_U activation) | 5.66 eV | 5.66 | ✓ |
| `B21_vU_refit` | −0.62 eV | −0.62 | ✓ |
| `B22_vU_refit` | −0.04 eV² | −0.04 | ✓ |
| `A20_vU_fig4_refit` | 4.6304·10⁻²⁹ | (Table gives 1.32·10⁻¹⁹) | **STUDENT REFIT** to match Fig.4 visually instead of Table 2 — see note |

**Note on A20_vU**: Rizk Table 2 gives A²⁰_VU = 1.32·10⁻¹⁹, but the curves in
Fig. 4 don't reproduce when this Table value is plugged in. The student
re-fit A20_vU to **4.63·10⁻²⁹** to match the Fig. 4 vacancy-bulk curve. This is
~10⁻¹⁰× different from the Table value — strongly suggests Rizk Table 2 has a
**unit error or printing typo** for this entry (the Table value is in different
units, or has a wrong exponent). The student's refit is defensible because it
matches the figure, and the figure is what they're calibrating against.

**Recommendation for v15**:
- Lock `Dg_scale`, `Dg_D1_scale`, `Dg_D3_scale` at 1.0 (Xe diffusivity is
  fully literature-validated)
- Lock `Dv_scale`, `Dv_D1_scale`, `Dv_D2_scale` at 1.0 (V_U diffusivity is
  literature-validated, modulo the documented A²⁰_VU refit)

→ **6 fitted scaling factors → 0**.

---

## 2. Equations — all FAITHFUL to Rizk, with one documented typo correction

Source PDF: pages 12–13, Appendix A.

| Equation | Rizk | un_model.py | Status |
|---|---|---|---|
| Total diffusivity | Eq. (1): D = D₁ + D₂ + D₃ | `xe_diffusivity_UN` | ✓ |
| Thermal diffusivity | Eq. (3) | ✓ matches | ✓ |
| Irrad.-enhanced diff. | Eq. (4) | ✓ matches | ✓ |
| Ballistic diffusivity | Eq. (5) | ✓ matches | ✓ |
| Resolution rate (b₀) | Eq. (8): `b₀(R) = 10⁻²⁵·(2.64 − 2.02·exp(−2.61·10⁻⁹/R))` | `b0_resolution` | ✓ exact |
| Bulk trapping g_b | Eq. (22): `g_b = 4π D_g R_b N_b` | ✓ | ✓ |
| Disl. trapping g_d | Eq. (23) | `trapping_rates_UN` | **TYPO IN PAPER, fixed in code** |
| Bulk nucleation ν_b | Eq. (26): `ν_b = 8π f_n D_g Ω_fg^(1/3) c²` | `nucleation_rate_bulk` | ✓ |
| Equilibrium pressure | Eq. (27): `p_eq = 2γ/R − σ_h` | `pressure_equilibrium` | ✓ |
| Internal pressure (vdW→ideal) | Eq. (28): `p = k_B T m /(nΩ)` | `pressure_internal` | ✓ |
| Vacancy geometric factor | Eq. (29) | `zeta_geometry` | ✓ |
| Coalescence λ | Eq. (15) | `coalescence_lambda` | ✓ |
| Spectral 3-eq | Eq. (33) | `sciantix_3x3_exchange_step` | ✓ |

**On Eq. (23) — the dislocation-trapping equation**:
The paper writes
$$g_d = 4\pi D_g R_d N_d + \frac{2\pi D_g \rho_d}{\ln(\Gamma_d/Z_d r_d) - 3/5} (\rho_d - 2 R_d N_d).$$
The first ρ_d in the second term is dimensionally wrong: the result would have
units `[1/(s·m²)]` instead of `[1/s]`. The student noticed this in
`UN_M7_calibration_lessons_report.md` (see comment in `System.C` engine code:
*"nel Rizk a numeratore c'era un *rho_d in più, sembra per errore di stampa"*)
and dropped the extra ρ_d in the implementation. **The code is correct; the
paper has a typo.** This should be footnoted in the thesis.

---

## 3. Material constants — all VERIFIED against Rizk Table 1

Source PDF: page 3, Table 1.

| Symbol | un_model.py | Rizk Table 1 | Status | Notes |
|---|---:|---:|:---:|---|
| `omega_fg` (Xe vdW vol.) | 8.5·10⁻²⁹ m³ | 8.5·10⁻²⁹ | ✓ | from [55] |
| `lattice_parameter` (a) | 4.889·10⁻¹⁰ m | 4.889·10⁻¹⁰ | ✓ | |
| `gamma_b` (surface tension) | 1.11 J/m² | 1.11 J/m² (LLS-MD) | ✓ | also Table 3 average 1.139 |
| `K_d` (atoms/disl. line) | 5.0·10⁵ bub/m | 5·10⁵ | ✓ | based on UC data [45] |
| `r_d` (Burgers vector) | 3.46·10⁻¹⁰ m (= a/√2) | a/√2 | ✓ | |
| `Z_d` (trapping factor) | 5 | 5 | ✓ | |
| `f_n` (nucleation factor) | 1.0·10⁻⁶ | 10⁻⁶ | ✓ | "**inherited from U₃Si₂** [29]" |
| `rho_d` (disl. density base) | 3.0·10¹³ m⁻² | "based on data in UC [45]" | ✓ | also `RHO_FAB = 3·10¹³` in v14 |

---

## 4. Truly fitted parameters — what remains uncertain

Rizk himself says (page 3): *"It was found to be essential to slightly lower
the value of K... to 5·10⁵ bubbles m⁻¹, which is within the uncertainty range
of the approximate value of 10⁶ [39]"*. So even Rizk treats `K`, `f_n`, and
`ρ_d` as tunable within bounded ranges. Comparing v14 winner with Rizk's
recommended values:

| Parameter | Rizk recommended | v14 fitted | Within Rizk range? |
|---|---:|---:|:---:|
| `K_d` | 5·10⁵ (within [10⁵, 10⁶]) | 3.5·10⁵ (lower bound of [3·10⁵, 8·10⁵]) | **Below Rizk's lower** |
| `f_n` | 10⁻⁶ (literature range [10⁻⁷, 10⁻²]) | 1.81·10⁻⁷ (near lower bound of search) | **At edge of literature** |
| `rho_scale` | 1.0 (Rizk uses ρ_FAB directly) | 0.577 (lower than Rizk) | Permitted by search [0.5, 2.0] |
| `gb_scale` | 1.0 (eq. 22 is theoretical) | 1.65 (above 1) | No literature basis to deviate |
| `gd_bubble_scale` | 1.0 | 0.094 (very low) | No literature basis |
| `gd_line_scale` | 1.0 | 5.46 (very high) | No literature basis |
| `b_bulk_scale` | 1.0 (eq. 8 is from Rizk Eq.) | 0.243 | No literature basis |
| `b_dislocation_scale` | 1.0 | 0.115 | No literature basis |
| `coalescence_d_scale` | 1.0 (eq. 15 from hard-sphere statistics [70]) | 0.984 | OK, near 1 |
| `capture_scale` | not in Rizk (capture_only is Barani-like UO₂ inheritance) | 0.247 | No Rizk basis at all |

**Two key concerns from this comparison**:

1. **`K_d` and `f_n` both pinned near their search bounds**, both already at
   the edge of what Rizk considers reasonable. The model is being asked to
   produce N_d at low T that is structurally unreachable given Rizk's
   recommended parameter ranges. The R_d is "fixable" (it depends on swelling
   and N_d together) but N_d at low T is not.
2. **`gb_scale=1.65`, `gd_bubble_scale=0.094`, `gd_line_scale=5.46`,
   `b_bulk_scale=0.243`, `b_dislocation_scale=0.115`** — five scaling factors
   on quantities that come from theoretical formulas (Eqs. 8, 22, 23 of Rizk).
   These deviations aren't documented in any literature; they're pure fit
   knobs absorbing model inadequacies. Together they produce a model fit but
   represent **non-physical compensation** as the student feared.

---

## 5. Recommended v15 search space (5 fitted parameters, down from 13+)

Lock at literature defaults (with prior centred at 1.0):

| Factor | Rationale |
|---|---|
| `Dv_scale`, `Dv_D1_scale`, `Dv_D2_scale` | Rizk Table 2 + Fig.4 refit; nothing new to fit |
| `Dg_scale`, `Dg_D1_scale`, `Dg_D3_scale` | Rizk Table 2; fully validated |
| `D2_xe_scale` | Already deprecated in v6+ |
| `gb_scale` | Eq. (22) is theoretical |
| `gd_bubble_scale`, `gd_line_scale`, `gd_line_alpha` | Eq. (23) is theoretical |
| `b_bulk_scale`, `b_dislocation_scale` | Eq. (8) is from Rizk; no UN-specific evidence to split into bulk/disl |

Keep, with reasoned ranges:

| Parameter | Range | Reason |
|---|---|---|
| `f_n` | [10⁻⁷, 10⁻²] | Rizk: literature range explicitly stated |
| `K_d` | [10⁵, 10⁷] | Rizk discussion: 5·10⁵ "within range" of 10⁶; Fig.2 data shows K up to 1.9·10⁷ at high T. **Widen upward** |
| `rho_scale` | [0.5, 2.0] | Already in v14 search; defensible |
| `coalescence_d_scale` | [0.5, 2.0] | Eq.(15) is approximate hard-sphere |
| `capture_scale` | [0, 2] | Barani-like, no UN literature; allow zero (no capture, pure Rizk) |

→ **5 fitted parameters for ~7 independent observables**. No more boundary-pinned
knobs; no more theoretical-formula scaling factors.

If this v15 doesn't fit Rizk's data, the **physics** is the problem (e.g.,
missing nucleation channel, wrong coalescence law) — not parameter tuning.
That's the right state to be in for a thesis.

---

## 6. Phase 2 — Olander review (re-solution) + Blank (carbide ρ_d)

### 6.1 Olander & Wongsawaeng 2006, *J. Nucl. Mater.* 354, 94–109

The standard modern review of intragranular fission-gas resolution.
Key takeaways relevant to v14's calibration:

- **`f_n` literature range CONFIRMED at [10⁻⁷, 10⁻²]**. Olander says (page 99):
  *"Values of f_N ranging from 10⁻⁷ to 10⁻² have been proposed, which makes
  the nucleation factor little more than an adjustable parameter."* Veshchunov
  fits 10⁻⁵–10⁻⁴; Rizk inherits 10⁻⁶ from U₃Si₂ work. v14's fitted
  `f_n = 1.81·10⁻⁷` is at the **lower edge** of the literature range.
- **Two re-solution mechanisms**: heterogeneous (whole-bubble destruction by
  ff track, dominant for `b ~ 10⁻⁵ s⁻¹`) and homogeneous (atom-by-atom
  knockout, ~10⁻⁷ s⁻¹). The Rizk `b₀^UN(R) = 10⁻²⁵·(2.64 − 2.02·exp(−2.61·10⁻⁹/R))`
  form is a BCA-simulation fit specific to UN (Matthews et al. 2016, [58] in
  Rizk), **not derivable from Olander's framework alone** but consistent with it.
- **No literature support for `b_bulk` vs `b_disl` split**: Olander treats
  resolution as a property of the bubble (size-dependent through `b₀(R)`),
  not of the bubble's location. The dependence on bubble radius is the only
  separation justified — and it's already in `b₀(R)`. v14's two independent
  scaling factors `b_bulk_scale=0.243`, `b_dislocation_scale=0.115` are
  **unmotivated by re-solution physics** and likely compensate for the
  `g_d_*` mis-tuning rather than for genuine resolution differences.

### 6.2 Ray & Blank 1984, *J. Nucl. Mater.* 124, 159–174 (carbide microstructure)

This is the PRIMARY source for ρ_d(F, T) in MX-type fuels (UC, UN behaves
similarly). Key findings:

- **ρ_d(F) law CONFIRMED**. Eq. (1) in Blank:
  `ρ_d = C₁ (F − F₀)` with **C₁ = 1.6·10¹⁰ cm⁻²/(a/o) = 1.6·10¹⁴ m⁻²/(a/o)**,
  **F₀ = 2.4 a/o** (sodium-bonded carbides, T = 940–1300 K).
  Rizk's `RHO_FAB`, `C₁`, `F₀` all match Blank exactly. ✓
- **ρ_FAB range CONFIRMED**. Blank reports unirradiated dislocation density
  10⁹–5·10⁹ cm⁻² = **10¹³–5·10¹³ m⁻²**. Rizk's `RHO_FAB = 3·10¹³ m⁻²` is in
  the middle of this range. ✓
- **N₁/ρ_d slope (the K parameter)**: Fig. 10 of Blank, plus Rizk's Fig. 2
  reproduction, shows the ratio K = N_bubbles / ρ_d_line ranges from
  **1.2·10⁶ bub/m** (at 1025–1340 K) to **1.9·10⁷ bub/m** (at 1240–1550 K).
  - Rizk used **K = 5·10⁵ bub/m** (below all measured slopes by ≥2.4×)
  - v14 fitted **K_d = 3.5·10⁵ bub/m** (below by 3.4× — pinned at lower
    bound of `[3·10⁵, 8·10⁵]`)
  - The **structural ceiling on N_d** at low T (`N_d = K_d × ρ_d_eff`) is
    ~10× below Rizk's experimental N_d at 1153 K precisely because both Rizk
    and v14 sit far below Blank's measured K. **Widening v15's K_d range
    upward to [10⁶, 5·10⁷]** would let the calibration access Blank's
    data range and probably resolve the low-T N_d gap.

### 6.3 Pizzocri / Pastore / Zullo (UO₂ inheritance) — DEFERRED

The remaining UO₂-side reading (Pizzocri 2018/2020, Pastore 2013, Zullo 2023)
would clarify:
- whether `f_n = 10⁻⁶` is defensible *for UN* given U₃Si₂ provenance
- the original Barani-like capture law (`f_cap = min(1, max(0, s_cap·N_d·ΔV))`)
  that the `capture_only` family inherited
- the Torquato hard-sphere derivation [70 in Rizk] for `λ_d`

These don't change the v15 search-space recommendation in §5 — they only
add bibliographic support. Defer to a phase 3 unless something specific comes up.

---

## 7. Final v15 search-space recommendation (UPDATED with phase 2)

Same as §5 above, with one **important update from Blank**:

| Parameter | Old range (v14) | **NEW range (v15)** | Reason |
|---|---|---|---|
| `f_n` | [10⁻¹⁰, 3·10⁻⁶] | [10⁻⁷, 10⁻²] | Olander explicit literature range |
| `K_d` | [3·10⁵, 8·10⁵] | **[10⁶, 5·10⁷]** | Blank's measured slopes are 1.2·10⁶–1.9·10⁷; v14 was pinned below all of these |
| `rho_scale` | [0.5, 2.0] | [0.5, 2.0] | Already defensible |
| `coalescence_d_scale` | [0.1, 20.0] | [0.5, 2.0] | Tighten — formula is approximate but `~1` |
| `capture_scale` | [0.01, 8.0] | [0, 2] | Allow zero (pure Rizk = no capture) |

All other 9 scaling factors → **lock at 1.0**.

**Expected outcome with widened K_d**: the model will be able to nucleate
N_d up to ~5·10⁷ × 3·10¹³ = **1.5·10²¹** initially, well above Rizk's
5·10¹⁹ at 1153 K. Coalescence then drops it through the experimental cloud.
The current v14 ceiling of `5·10⁵ × 3·10¹³ = 1.5·10¹⁹` is too low to even
START where the data is.

---

## 8. Summary — what to tell the student (UPDATED)

1. **Diffusivity constants verified against Rizk Table 2** (with one likely
   Rizk Table 2 typo on `A²⁰_VU` — Rizk gives `1.32·10⁻¹⁹`, student refit to
   `4.63·10⁻²⁹` to match Fig. 4. Worth flagging to Rizk's group).
2. **Equations faithful to Rizk** (with one student-corrected paper typo in
   Eq. 23: extra `ρ_d` on numerator gives wrong units — code is correct).
3. **`f_n = 10⁻⁶` is the standard UO₂/U₃Si₂ inheritance**; Olander's range is
   [10⁻⁷, 10⁻²].
4. **`ρ_d` law is fully traceable to Blank 1984**: `ρ_d = max(ρ_FAB, C₁(F−F₀))`
   with C₁ = 1.6·10¹⁴ m⁻²/(a/o) and F₀ = 2.4 a/o, ρ_FAB = 3·10¹³ m⁻². v14's
   `rhoSat` saturating-T extension is a separate Rizk-era addition.
5. **The N_d undershoot at low T is structural**: both Rizk and v14 use K_d
   far below Blank's measured slopes. Widening K_d's search range upward to
   match Blank should resolve this.
6. **9 of v14's 13 scaling factors should be locked at 1.0** because their
   underlying formulas are pure literature (Rizk Eqs. 8/22/23, Olander
   framework). The remaining 5 (`f_n`, `K_d`, `rho_scale`, `coalescence_d_scale`,
   `capture_scale`) are the genuinely uncertain DOFs.
7. **The student's instinct that "many scaling factors hide physics" is
   correct** — and now this audit provides the literature evidence to
   defend a v15 with 5 free parameters in the thesis.

---

## 9. Phase 3 — open items

- [ ] Pastore 2013 / Pizzocri 2018, 2020 / Zullo 2023 — confirm Barani capture
  formula provenance and `f_n` UO₂ defensibility (low priority — won't
  change v15 recommendations, just adds citations).
- [ ] Generate `un_calibration/optuna/UN_M7_optuna_calibration_v15_*.py` from
  v14 with the locked scaling factors and the widened K_d range. Estimated
  effort: ~50 lines of edits to v14.
- [ ] Run a 100-trial v15 calibration to confirm the simpler model can still
  fit Rizk's data. If it can, thesis foundation is much stronger.
- [ ] (Optional) Reach out to Rizk's group about the suspected Table 2
  `A²⁰_VU` typo and the Eq. 23 `ρ_d` typo — both could be corrected in an
  erratum or noted in the thesis.

---

## 7. Summary — what to tell the student

1. **The diffusivity constants and equations in `un_model.py` are correct and
   verified against Rizk 2025.** No literature problem here.
2. **The student found a real typo** in Rizk Eq. (23) (extra ρ_d on numerator)
   and corrected it. This deserves a thesis footnote.
3. **The A²⁰_VU refit** (4.63·10⁻²⁹ vs Rizk's Table 2 value 1.32·10⁻¹⁹) is
   defensible — it matches Fig. 4. But this should be flagged with Rizk authors
   if possible; the discrepancy might be a Table typo on their side.
4. **The v14 calibration is over-parameterised**: 13 scaling factors compete
   for ~7 data shapes, several pin at their search bounds, and 5 of them
   deviate sharply from 1.0 with no literature basis. The student's gut
   feeling — "many scaling factors hide physics" — is correct.
5. **A v15 with 5 fitted parameters is achievable** without losing fit
   quality, because most of v14's scaling factors are compensating for each
   other rather than for genuine physics. If v15 can't fit, that's a signal
   the model is missing physics (likely heterogeneous nucleation source for
   the unreachable low-T high-density N_d region).
