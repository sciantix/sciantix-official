# Codex task — UN Thesis / M7 fission gas swelling calibration

## Context
You are working in the repository `~/sciantix-official` on the UN Thesis project. The goal is to automate calibration and diagnosis of a mechanistic fission gas swelling model for uranium nitride (UN), based mainly on Rizk et al. 2025 and Barani/SCIANTIX-type extensions.

This is not a generic ML task. The physical model must remain interpretable. Use Optuna and scripted automation to explore uncertain parameters and identify balanced candidates.

## Files to read first
Read these files before modifying or running anything:

1. `UN_Thesis_M7_context_for_new_chat.md`
2. `UNmodel.md`
3. `UNcode.md`
4. `b_g_nu_comparison.md`
5. `UNcoalescence_comparison.md`
6. `UN_sensitivity_analysis_summary.md`
7. `UN_model_variants_report_thesis.md`
8. `deep-research-report.md`, if present
9. `UN_M7_optuna_calibration.py`
10. `UN_M7_optuna_calibration_v2.py`
11. Existing result folders, if present:
   - `UN_M7_optuna_results/`
   - `UN_M7_global_sensitivity_fullcell/`
   - `UN_M7_optuna_v2_results/`

If some files are missing, continue with the available ones and explicitly record what was missing.

## Critical constraints
Do not change the physical equations casually. Do not invent new mechanisms unless you clearly mark them as a proposed variant and do not use them as the main model.

Do not modify the original notebook or original scripts in-place. Work only in new files:

- `UN_M7_codex_runner.py`
- `UN_M7_codex_analysis.py`
- `UN_M7_codex_report.md`
- output folder: `UN_M7_codex_results/`

Do not commit to git. Do not push. Do not create a new branch unless explicitly asked. Do not modify files that the other PC may also be using.

The other PC is running `M7_no_phi` Dv-profile studies. Therefore, on this PC focus primarily on:

1. `capture_only` as the physically simpler backup model;
2. balanced candidates that reduce dislocation bubble overpressure;
3. comparison to `M7_no_phi` only if useful, without overwriting its results.

## Physical target
The target is not to clone Rizk exactly. The main target is experimental P2 / large intragranular bubbles:

- dislocation swelling `swelling_d`;
- dislocation bubble radius `R_d`;
- dislocation bubble concentration `N_d`;
- high-temperature drop in `N_d`.

Rizk curves should be used as physical diagnostics, not as rigid targets, especially for bulk and grain-boundary curves, because the reduced Python model does not implement the full grain-boundary bubble/release model.

## Important current findings
Previous runs showed:

- `M7_no_phi` outperforms `M7_full` and `capture_only` slightly by score.
- `M7_full` does not justify using `phi` in the gas re-solution term.
- Best numerical candidates often use `Dv_scale ≈ 0.15`, high `coalescence_d_scale`, and produce good P2 fit.
- However, they produce excessive dislocation bubble pressure ratio `p_d / p_d_eq`, sometimes tens of times equilibrium.
- Therefore, pressure must be treated as a serious physical sanity constraint.
- `D2_Xe` should remain included, even if it is numerically tiny.
- `Dv` is suspicious because the vacancy diffusivity coefficients/table/figure are already known to be fragile and possibly inconsistent.

## Models/families
Use the existing model families in `UN_M7_optuna_calibration_v2.py`:

1. `M7_no_phi`: nucleation mass coupling + capture + coalescence, but no `phi` in gas re-solution.
2. `capture_only`: Barani-like bulk-dislocation capture + coalescence, without the more aggressive M7 features.
3. `M7_full`: keep only as diagnostic, not the priority.

For this PC, prioritize `capture_only`.

## Calibration philosophy
We need a model that is not only best-fit but also defensible:

- good P2 data fit;
- `R_d` and `N_d` close to measurements;
- `N_d` decreases at high temperature due to coalescence;
- bulk and dislocation bubbles are not wildly overpressurized;
- parameters stay as close to Rizk as possible, unless the parameter is known to be uncertain or inherited.

Use a fit-versus-physics frontier, not only the lowest scalar score.

## Suggested scoring priorities
Treat pressure as a strong but not absolute constraint:

- good: `p_d / p_d_eq` within roughly factor 3–5 in 1200–1700 K;
- suspicious: `p_d / p_d_eq > 10` over a broad range;
- unacceptable as final: ratio tens to hundreds without explanation.

Use or implement if needed:

- `pressure_weight` around `0.6` to `1.0`;
- `pressure_free_factor` around `2` or `3`;
- `rizk_prior_weight` around `0.1` to `0.5`;
- `bulk_shape_weight` small, e.g. `0.02` to `0.08`.

Do not make bulk swelling a strong target. Use it only as weak shape diagnostic.

## Parameters and priors
Near-fixed / do not calibrate unless absolutely necessary:

- lattice parameter;
- `Omega_fg`;
- matrix atomic volume;
- `gamma` unless doing a clearly labeled sensitivity;
- `r_d`;
- `Z_d`;
- `grain_radius` unless tied to data.

Medium prior / calibrate but prefer near Rizk:

- `K_d`;
- `rho_d`;
- `Dg_scale`;
- `b_scale`;
- `gb_scale`;
- `gd_scale`.

Weak prior / uncertain closure:

- `f_n`;
- `Dv_scale`;
- `coalescence_d_scale`;
- `capture_scale`.

But even weak-prior parameters should not be allowed to hide physical problems such as huge pressure ratio.

## Concrete work plan

### Step 1 — Verify the script and environment
Run:

```bash
cd ~/sciantix-official
source .venv/bin/activate
python UN_M7_optuna_calibration_v2.py --help
```

If `python` is not found, use `.venv/bin/python`.

### Step 2 — Run capture_only Dv-profile
Run capture-only with fixed `Dv_scale` values:

- 0.03
- 0.05
- 0.10
- 0.15
- 0.30
- 0.50
- 1.00

Recommended command template:

```bash
python UN_M7_optuna_calibration_v2.py \
  --family capture_only \
  --dv-fixed DV_VALUE \
  --pressure-weight 0.6 \
  --pressure-free-factor 3 \
  --rizk-prior-weight 0.2 \
  --bulk-shape-weight 0.05 \
  --n-trials 100 \
  --n-top-final 0 \
  --no-plots \
  --output-dir UN_M7_codex_results/capture_only_dv_profile
```

If too slow, use `--n-trials 50`.

### Step 3 — Analyze fast results
Create `UN_M7_codex_analysis.py` to aggregate all CSVs and produce:

- `UN_M7_codex_results/capture_only_dv_profile_summary.csv`
- `UN_M7_codex_results/top_balanced_candidates.csv`
- `UN_M7_codex_report.md`

For each fixed `Dv_scale`, report:

- best total score;
- best data score if available;
- pressure score;
- prior score;
- `swelling_d` at 1600 K, 1.3% FIMA;
- `R_d` at 1600 K, 1.3% FIMA;
- `N_d` at 1600 K, 1.3% FIMA;
- `Nd_drop_log10`;
- `p_d/p_d_eq` behavior if stored;
- parameters of the best candidate.

If the current script does not store enough pressure diagnostics, add non-invasive post-processing or extend a copied script. Do not overwrite original files.

### Step 4 — Pick candidates for accurate final rerun
Pick 3–5 candidates according to these categories:

1. best scalar score;
2. best pressure-friendly score;
3. closest-to-Rizk balanced score;
4. best `Dv_scale` near 0.3–1.0, if not terrible;
5. best `Dv_scale` low, if clearly needed.

Run final reruns only for these candidates with:

```bash
--final-dt-h 1 --final-n-modes 40
```

Generate plots for:

- swelling vs T at 1.1, 1.3, 3.2% FIMA;
- radius vs T at 1.3% FIMA;
- concentration vs T at 1.3% FIMA;
- pressure and pressure ratio at 3.2% FIMA;
- gas partition at 1.1 and 3.2% FIMA.

### Step 5 — Optional adaptive iteration
After the first capture_only Dv-profile:

- If all good candidates have high pressure ratio, increase pressure weight to 1.0 and rerun fewer trials.
- If all good candidates need `Dv_scale < 0.15`, extend the low-Dv range but explicitly flag overpressure.
- If candidates with `Dv_scale ≈ 0.5–1.0` are only slightly worse but much more physical, prefer them as balanced candidates.
- If capture_only cannot match the data without bad pressure, recommend fallback to `M7_no_phi` or identify the missing physics.

Do not change physical equations automatically.

## Deliverables
At the end, create:

1. `UN_M7_codex_report.md`
2. `UN_M7_codex_results/top_balanced_candidates.csv`
3. `UN_M7_codex_results/capture_only_dv_profile_summary.csv`
4. plots for final candidates, if reruns were performed
5. a short terminal-ready list of recommended next commands

The report must answer:

- Is `capture_only` competitive with `M7_no_phi` once pressure and Rizk-prior are included?
- What `Dv_scale` gives the best trade-off?
- Can we find candidates with acceptable pressure ratio without destroying P2 fit?
- How far from Rizk are the parameters?
- Is the bulk swelling shape problem fatal or only diagnostic?
- What is the recommended final model family?

## Definition of success
A good final candidate is not necessarily the lowest score. It should satisfy as many as possible:

- P2/dislocation swelling close to data;
- `R_d` close to large-bubble data;
- `N_d` reasonable and decreasing at high temperature;
- `p_d/p_d_eq` not tens of times equilibrium over the fitted range;
- no disabled coalescence;
- linear/clipped capture only;
- parameters not needlessly far from Rizk;
- D2_Xe included;
- no undocumented equation changes.

If no such candidate exists, say so clearly and identify the limiting physics/parameter.
