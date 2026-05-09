"""Compare the three rho_d laws: constant (Rizk 2025), Blank-FT, Rizk-NEAMS exp.

For each law, runs the model on a T-grid at 1.1 / 1.3 / 3.2 % FIMA and
computes RMSE vs Ronchi 1978 dislocation swelling data. Produces a 4-panel
figure: 3 swelling-vs-T panels (one per burnup, with all three laws overlapped)
plus a diagnostic ρ_d(T) panel at 1.3 % FIMA.

Goal: test whether the asymmetric +1.24 bias on Sw_d at 1.3 % FIMA observed
with constant ρ_d is reduced by a T-dependent ρ_d law.

Output:
    un_calibration/reports/rho_d_laws_comparison/rho_d_laws_comparison.csv
    un_calibration/reports/rho_d_laws_comparison/rho_d_laws_comparison.png
"""

import sys
import pathlib
import math

ROOT = pathlib.Path(__file__).resolve().parents[1]
for sub in ("model", "config"):
    sys.path.insert(0, str(ROOT / sub))

import csv                                              # noqa: E402
import numpy as np                                      # noqa: E402
import matplotlib.pyplot as plt                         # noqa: E402

import un_data                                          # noqa: E402
from manual_params import MANUAL_PARAMS as MP_DEFAULT   # noqa: E402
from rho_d_laws import resolve_rho_d                    # noqa: E402
from builder import model_runner                        # noqa: E402

T_GRID = np.arange(900, 1750, 50)
BURNUPS = [1.1, 1.3, 3.2]
DT_HOURS = 12.0
N_MODES = 25
EXPERIMENTAL_ERROR_FRACTION = 0.10

LAWS = [
    ("constant", "Constant rho_d (Rizk 2025)",
        {"USE_RHO_FT": False, "USE_RHO_EXP": False}),
    ("FT",       "Blank-saturating + Ray-Blank",
        {"USE_RHO_FT": True,  "USE_RHO_EXP": False}),
    ("exp",      "Rizk-NEAMS exp (Eq. 3.38)",
        {"USE_RHO_FT": False, "USE_RHO_EXP": True}),
]

OUT_DIR = ROOT / "reports" / "rho_d_laws_comparison"


def _rmse(residuals):
    if not residuals:
        return float("nan")
    return math.sqrt(sum(r * r for r in residuals) / len(residuals))


def _mean(xs):
    return sum(xs) / len(xs) if xs else float("nan")


def _build_mp(flags):
    mp = dict(MP_DEFAULT)
    mp.update(flags)
    return mp


def run():
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    # --- Per-law data + metrics ---
    data = {}                  # law_key -> {bu -> [{T, swD, swB}]}
    metrics = []               # CSV rows
    for key, label, flags in LAWS:
        mp = _build_mp(flags)

        smooth = {}
        for bu in BURNUPS:
            smooth[bu] = []
            for T in T_GRID:
                out = model_runner(float(T), bu, dt_h=DT_HOURS,
                                   n_modes=N_MODES, manual_params=mp)
                smooth[bu].append({
                    "T": float(T),
                    "swD": out["swelling_d_percent"],
                    "swB": out["swelling_b_percent"],
                })
        data[key] = smooth

        residuals_per_bu = {bu: [] for bu in BURNUPS}
        all_residuals = []
        for exp in un_data.EXP_SWELLING_T:
            T = float(exp["T"])
            bu = float(exp["burnup"])
            out = model_runner(T, bu, dt_h=DT_HOURS,
                               n_modes=N_MODES, manual_params=mp)
            res = out["swelling_d_percent"] - exp["swelling"]
            residuals_per_bu[bu].append(res)
            all_residuals.append(res)

        for bu in BURNUPS:
            metrics.append({
                "law": key, "label": label,
                "burnup_percent": bu,
                "n_points": len(residuals_per_bu[bu]),
                "rmse_swelling_d": _rmse(residuals_per_bu[bu]),
                "bias_swelling_d": _mean(residuals_per_bu[bu]),
            })
        metrics.append({
            "law": key, "label": label,
            "burnup_percent": "all",
            "n_points": len(all_residuals),
            "rmse_swelling_d": _rmse(all_residuals),
            "bias_swelling_d": _mean(all_residuals),
        })

    # --- CSV ---
    csv_path = OUT_DIR / "rho_d_laws_comparison.csv"
    with csv_path.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(metrics[0].keys()))
        w.writeheader()
        for r in metrics:
            w.writerow(r)
    print(f"  wrote {csv_path}")

    # --- Console table ---
    print()
    print(f"{'law':>10}  {'bu':>4}  {'n':>3}  {'RMSE':>8}  {'bias':>8}")
    print("-" * 50)
    for r in metrics:
        print(f"  {r['law']:>8}  {str(r['burnup_percent']):>4}  {r['n_points']:>3}  "
              f"{r['rmse_swelling_d']:>8.3f}  {r['bias_swelling_d']:>8.3f}")

    # --- Figure: 4 panels ---
    fig, axes = plt.subplots(1, 4, figsize=(22, 5))
    colors_law = {"constant": "tab:blue", "FT": "tab:orange", "exp": "tab:green"}

    for ax, bu in zip(axes[:3], BURNUPS):
        for key, label, _ in LAWS:
            smooth = data[key][bu]
            Ts = [s["T"] for s in smooth]
            swD = [s["swD"] for s in smooth]
            ax.plot(Ts, swD, "-", color=colors_law[key], linewidth=1.8,
                    label=f"{key}")

        if abs(bu - 1.1) < 0.01:
            exps = [p for p in un_data.EXP_SWELLING_T
                    if abs(p["burnup"] - 1.1) < 0.01]
        else:
            exps = [p for p in un_data.EXP_SWELLING_T
                    if abs(p["burnup"] - bu) < 0.01]
        if exps:
            ts  = [p["T"] for p in exps]
            sw  = [p["swelling"] for p in exps]
            err = [EXPERIMENTAL_ERROR_FRACTION * s for s in sw]
            ax.errorbar(ts, sw, yerr=err, fmt="o", markersize=5,
                        color="black", ecolor="black",
                        elinewidth=0.6, capsize=1.5,
                        label="Ronchi ±10%", zorder=5)

        ax.set_xlabel("Temperature (K)")
        ax.set_title(f"Sw_d vs T at {bu}% FIMA")
        ax.grid(alpha=0.3)
        if ax is axes[0]:
            ax.set_ylabel("Swelling (%)")
        ax.legend(fontsize=8, loc="upper left")

    # Diagnostic panel: rho_d(T) at 1.3 % FIMA
    ax_rho = axes[3]
    T_diag = np.linspace(800, 2100, 200)
    for key, label, flags in LAWS:
        mp = _build_mp(flags)
        rhos = [resolve_rho_d(float(T), 1.3, mp) for T in T_diag]
        ax_rho.semilogy(T_diag, rhos, "-", color=colors_law[key],
                        linewidth=1.8, label=key)
    ax_rho.set_xlabel("Temperature (K)")
    ax_rho.set_ylabel(r"$\rho_d$ (m$^{-2}$)")
    ax_rho.set_title("ρ_d(T) at 1.3 % FIMA — diagnostic")
    ax_rho.grid(alpha=0.3, which="both")
    ax_rho.legend(fontsize=8, loc="upper left")

    fig.suptitle(
        "ρ_d law comparison — dislocation swelling vs Ronchi 1978",
        fontsize=12,
    )
    plt.tight_layout(rect=[0, 0, 1, 0.95])

    png_path = OUT_DIR / "rho_d_laws_comparison.png"
    plt.savefig(png_path, dpi=140, bbox_inches="tight")
    print(f"  wrote {png_path}")
    plt.close(fig)


if __name__ == "__main__":
    run()
