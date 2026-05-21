"""2x2x2 ablation study of the three physics-extension flags in un_model.

Flags toggled (True/False each):
  - USE_PHI_GAS_RESOLUTION       (φ-correction on resolution; Olander 2006 / Pizzocri 2020)
  - USE_NUCLEATION_MASS_COUPLING (±2ν_b coupling between c and m_b; Pizzocri 2020)
  - USE_BULK_DISLOCATION_CAPTURE (Barani-like sweeping; not in Rizk, audit-2026-05-09
                                  default OFF; here we re-include it for the ablation)

Label encoding is phi+mass+capture, e.g. "101" = φ on, mass off, capture on.
Run 000 collapses the system to the bare 3-equation form (paper-faithful for
the gas equations).
Run 011 is the current default (φ off, mass on, capture on) — per-atom b
from Setyawan/Matthews, φ only on the bubble-count equation (which is always
applied at the solver level), plus Barani-like sweeping capture re-introduced
2026-05-21 as a thesis extension (Olander §10.4 cross-section, mass-conserving).
Run 110 / 111 are the alternative Barani 2019/2020 "φ everywhere" moment closure
(now substantially over-predicting after the 2026-05-21 ρ_d=1e14 calibration).

For each combination, the model is run at the experimental (T, burnup) anchor
points of `un_data.EXP_SWELLING_T` (Rizk Fig. 3) and RMSE / bias are computed
on dislocation-bubble swelling vs the measurements.

Output:
    un_calibration/reports/flag_ablation/flag_ablation.csv     (per-combo metrics)
    un_calibration/reports/flag_ablation/flag_ablation_curves.png
                                                                (8 swelling-vs-T panels)
"""

import sys
import pathlib
import math
import itertools

ROOT = pathlib.Path(__file__).resolve().parents[1]
for sub in ("model", "config"):
    sys.path.insert(0, str(ROOT / sub))

import csv                                              # noqa: E402
import numpy as np                                      # noqa: E402
import matplotlib.pyplot as plt                         # noqa: E402

import un_model as m                                    # noqa: E402
import un_data                                          # noqa: E402
from builder import model_runner                        # noqa: E402

DT_HOURS = 12.0
N_MODES = 25
BURNUPS = [1.1, 1.3, 3.2]

OUT_DIR = ROOT / "reports" / "flag_ablation"

FLAG_NAMES = ("phi", "mass", "capture")


def _set_flags(phi: bool, mass: bool, capture: bool):
    """Override module-level flags before calling the solver."""
    m.USE_PHI_GAS_RESOLUTION = phi
    m.USE_NUCLEATION_MASS_COUPLING = mass
    m.USE_BULK_DISLOCATION_CAPTURE = capture


def _flag_label(flags):
    return "".join("1" if f else "0" for f in flags)


def _rmse(residuals):
    if not residuals:
        return float("nan")
    return math.sqrt(sum(r * r for r in residuals) / len(residuals))


def _mean(xs):
    return sum(xs) / len(xs) if xs else float("nan")


def run():
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    # All 8 combinations: (phi, mass, capture)
    combos = list(itertools.product([False, True], repeat=3))

    # --- Per-combo metrics + curve data ---
    rows = []                                           # for CSV
    curves = {}                                         # combo -> list of dict

    for combo in combos:
        _set_flags(*combo)
        label = _flag_label(combo)

        # Run model at every experimental anchor for the residuals.
        results_at_exp = []
        for exp_pt in un_data.EXP_SWELLING_T:
            T = float(exp_pt["T"])
            bu = float(exp_pt["burnup"])
            out = model_runner(T, bu, dt_h=DT_HOURS, n_modes=N_MODES)
            results_at_exp.append({
                "T": T, "burnup": bu,
                "exp_swelling": exp_pt["swelling"],
                "model_swelling_d": out["swelling_d_percent"],
                "model_swelling_b": out["swelling_b_percent"],
                "residual": out["swelling_d_percent"] - exp_pt["swelling"],
            })

        # Smooth grid for curves
        T_grid = np.arange(900, 1750, 50)
        smooth = []
        for bu in BURNUPS:
            for T in T_grid:
                out = model_runner(float(T), bu, dt_h=DT_HOURS, n_modes=N_MODES)
                smooth.append({
                    "T": float(T), "burnup": bu,
                    "swD": out["swelling_d_percent"],
                    "swB": out["swelling_b_percent"],
                })
        curves[label] = smooth

        # Per-burnup metrics + global
        for bu in BURNUPS:
            res_bu = [r["residual"] for r in results_at_exp
                      if abs(r["burnup"] - bu) < 0.01]
            rows.append({
                "label": label,
                "phi": combo[0], "mass": combo[1], "capture": combo[2],
                "burnup_percent": bu,
                "n_points": len(res_bu),
                "rmse_swelling_d": _rmse(res_bu),
                "bias_swelling_d": _mean(res_bu),
            })

        # Global (all burnups)
        all_res = [r["residual"] for r in results_at_exp]
        rows.append({
            "label": label,
            "phi": combo[0], "mass": combo[1], "capture": combo[2],
            "burnup_percent": "all",
            "n_points": len(all_res),
            "rmse_swelling_d": _rmse(all_res),
            "bias_swelling_d": _mean(all_res),
        })

    # --- CSV dump ---
    csv_path = OUT_DIR / "flag_ablation.csv"
    with csv_path.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        for r in rows:
            w.writerow(r)
    print(f"  wrote {csv_path}")

    # --- Console table ---
    print()
    print(f"{'label':>5}  {'phi':>4}  {'mass':>4}  {'cap':>4}  {'bu':>4}  "
          f"{'n':>3}  {'RMSE Sw_d':>10}  {'bias Sw_d':>10}")
    print("-" * 64)
    for r in rows:
        print(f"  {r['label']:>3}  {str(r['phi'])[0]:>4}  {str(r['mass'])[0]:>4}  "
              f"{str(r['capture'])[0]:>4}  "
              f"{str(r['burnup_percent']):>4}  {r['n_points']:>3}  "
              f"{r['rmse_swelling_d']:>10.3f}  {r['bias_swelling_d']:>10.3f}")

    # --- 8-panel figure of swelling vs T (2 rows × 4 cols) ---
    fig, axes = plt.subplots(2, 4, figsize=(18, 9), sharex=True, sharey=True)
    axes = axes.flatten()
    colors_bu = {1.1: "tab:blue", 1.3: "tab:orange", 3.2: "tab:red"}

    for ax, combo in zip(axes, combos):
        label = _flag_label(combo)
        smooth = curves[label]
        for bu in BURNUPS:
            rs = [s for s in smooth if abs(s["burnup"] - bu) < 0.01]
            Ts = [s["T"] for s in rs]
            swD = [s["swD"] for s in rs]
            ax.plot(Ts, swD, "-", color=colors_bu[bu], linewidth=1.6,
                    label=f"model {bu}% FIMA")

            exps = [p for p in un_data.EXP_SWELLING_T
                    if abs(p["burnup"] - bu) < 0.01]
            ax.scatter([p["T"] for p in exps], [p["swelling"] for p in exps],
                       marker="o", s=22, color=colors_bu[bu],
                       edgecolor="black", linewidth=0.4, zorder=5)

        flag_str = " ".join(
            f"{n}={'on' if v else 'off'}"
            for n, v in zip(FLAG_NAMES, combo)
        )
        ax.set_title(f"{label}  ({flag_str})", fontsize=9)
        ax.grid(alpha=0.3)
        if ax is axes[0]:
            ax.legend(fontsize=8, loc="upper left")

    for ax in axes[4:]:
        ax.set_xlabel("Temperature (K)")
    for ax in (axes[0], axes[4]):
        ax.set_ylabel("Swelling (%)")
    fig.suptitle(
        "Flag ablation 2x2x2 — dislocation-bubble swelling vs experiment\n"
        "label = (phi)(mass)(capture)   |   011 = current default (per-atom + capture)   |   "
        "110/111 = Barani moment closure (phi everywhere; now over-predicts)",
        fontsize=11
    )
    plt.tight_layout(rect=[0, 0, 1, 0.93])

    png_path = OUT_DIR / "flag_ablation_curves.png"
    plt.savefig(png_path, dpi=140, bbox_inches="tight")
    print(f"  wrote {png_path}")
    plt.close(fig)


if __name__ == "__main__":
    run()
