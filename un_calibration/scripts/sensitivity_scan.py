"""1D sensitivity scan of the three free parameters: K_d, f_n, rho_d.

For each parameter, vary the value across ~3 orders of magnitude around the
reference (Rizk-nominal), keeping the other two fixed. For every value, run
the model at the Ronchi 1978 experimental anchor points and compute RMSE +
bias on dislocation swelling.

Output: a CSV with one row per (parameter, value, burnup, n, RMSE, bias),
plus a 3-panel figure showing RMSE vs log10(parameter), one curve per
burnup (1.1 / 1.3 / 3.2 % FIMA) and a thicker "global" curve over all
39 points. The reference value is marked with a vertical line.

Reference (Rizk-nominal):
  K_d   = 5e5 bub/m              (Rizk 2025 Sec. 4 calibration)
  f_n   = 1e-6                   (inherited from U3Si2; Olander 1e-7..1e-2)
  rho_d = 3e13 m^-2              (Rizk 2025 Tab. 1)

Output:
    un_calibration/reports/sensitivity_scan/sensitivity_scan.csv
    un_calibration/reports/sensitivity_scan/sensitivity_scan.png
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
from rizk_constants import RIZK_CONSTANTS as RK_DEFAULT # noqa: E402
from builder import model_runner                        # noqa: E402

DT_HOURS = 12.0
N_MODES = 25
BURNUPS = [1.1, 1.3, 3.2]

# Parameter scans: (axis_name, reference_value, scan_values, manual_or_rizk_key)
# manual_or_rizk_key = "mp:<key>"  (override in MANUAL_PARAMS)
#                  or "rk:<key>"   (override in RIZK_CONSTANTS)
SCANS = [
    # (axis, REFERENCE value to mark on plot, scan values, where-to-override)
    ("K_d",   5.0e5,   [1.0e4, 5.0e4, 1.0e5, 5.0e5, 1.0e6, 5.0e6, 1.0e7],   "rk:K_D"),
    # f_n reference is the UN-recalibrated value (was 1e-6 inherited from U3Si2;
    # see calibrate_f_n.py for the fine scan that picked 3e-6).
    ("f_n",   3.0e-6,  [1.0e-7, 1.0e-6, 3.0e-6, 1.0e-5, 1.0e-4, 1.0e-3],    "mp:f_n"),
    ("rho_d", 3.0e13,  [1.0e12, 5.0e12, 1.0e13, 3.0e13, 1.0e14, 3.0e14, 1.0e15], "mp:rho_d"),
]

OUT_DIR = ROOT / "reports" / "sensitivity_scan"


def _rmse(residuals):
    if not residuals:
        return float("nan")
    return math.sqrt(sum(r * r for r in residuals) / len(residuals))


def _mean(xs):
    return sum(xs) / len(xs) if xs else float("nan")


def _build_overrides(scan_key, value):
    """Return (manual_params, rizk_constants) with the override applied."""
    mp = dict(MP_DEFAULT)
    rk = dict(RK_DEFAULT)
    where, key = scan_key.split(":")
    if where == "mp":
        mp[key] = value
    elif where == "rk":
        rk[key] = value
    else:
        raise ValueError(f"unknown scan_key prefix: {where}")
    return mp, rk


def run():
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    rows = []
    series = {}                # axis_name -> {bu_or_all -> [(value, rmse, bias)]}

    for axis_name, ref_value, values, scan_key in SCANS:
        series[axis_name] = {bu: [] for bu in BURNUPS}
        series[axis_name]["all"] = []

        print(f"\n=== {axis_name}  (reference {ref_value:.2e}) ===")
        for v in values:
            mp, rk = _build_overrides(scan_key, v)

            residuals_per_bu = {bu: [] for bu in BURNUPS}
            all_residuals = []
            for exp in un_data.EXP_SWELLING_T:
                T = float(exp["T"])
                bu = float(exp["burnup"])
                out = model_runner(T, bu, dt_h=DT_HOURS, n_modes=N_MODES,
                                   manual_params=mp, rizk_constants=rk)
                res = out["swelling_d_percent"] - exp["swelling"]
                residuals_per_bu[bu].append(res)
                all_residuals.append(res)

            for bu in BURNUPS:
                rmse_bu = _rmse(residuals_per_bu[bu])
                bias_bu = _mean(residuals_per_bu[bu])
                rows.append({
                    "axis": axis_name, "value": v,
                    "burnup_percent": bu,
                    "n_points": len(residuals_per_bu[bu]),
                    "rmse_swelling_d": rmse_bu,
                    "bias_swelling_d": bias_bu,
                })
                series[axis_name][bu].append((v, rmse_bu, bias_bu))

            rmse_all = _rmse(all_residuals)
            bias_all = _mean(all_residuals)
            rows.append({
                "axis": axis_name, "value": v,
                "burnup_percent": "all",
                "n_points": len(all_residuals),
                "rmse_swelling_d": rmse_all,
                "bias_swelling_d": bias_all,
            })
            series[axis_name]["all"].append((v, rmse_all, bias_all))
            print(f"  {axis_name} = {v:.2e}   global RMSE = {rmse_all:.3f}, "
                  f"bias = {bias_all:+.3f}")

    # --- CSV ---
    csv_path = OUT_DIR / "sensitivity_scan.csv"
    with csv_path.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        for r in rows:
            w.writerow(r)
    print(f"\n  wrote {csv_path}")

    # --- Figure: 3 panels (one per parameter), RMSE vs log10(parameter) ---
    fig, axes = plt.subplots(1, 3, figsize=(18, 5))
    colors_bu = {1.1: "tab:blue", 1.3: "tab:orange", 3.2: "tab:red", "all": "black"}

    for ax, (axis_name, ref_value, values, scan_key) in zip(axes, SCANS):
        for bu in BURNUPS:
            pts = series[axis_name][bu]
            xs = [p[0] for p in pts]
            ys = [p[1] for p in pts]
            ax.semilogx(xs, ys, "o-", color=colors_bu[bu],
                        linewidth=1.4, markersize=4,
                        label=f"{bu}% FIMA")
        # Global
        pts = series[axis_name]["all"]
        xs = [p[0] for p in pts]
        ys = [p[1] for p in pts]
        ax.semilogx(xs, ys, "s-", color=colors_bu["all"],
                    linewidth=2.0, markersize=5,
                    label="all (39 pts)")
        ax.axvline(ref_value, color="grey", linestyle="--", alpha=0.7,
                   label=f"reference = {ref_value:.1e}")
        ax.set_xlabel(f"{axis_name}")
        ax.set_ylabel("RMSE on Sw_d (% Sw)")
        ax.set_title(f"Sensitivity to {axis_name}")
        ax.grid(alpha=0.3, which="both")
        ax.legend(fontsize=8, loc="upper left")

    fig.suptitle(
        "1D sensitivity scan — RMSE on dislocation swelling vs Ronchi 1978",
        fontsize=12,
    )
    plt.tight_layout(rect=[0, 0, 1, 0.95])

    png_path = OUT_DIR / "sensitivity_scan.png"
    plt.savefig(png_path, dpi=140, bbox_inches="tight")
    print(f"  wrote {png_path}")
    plt.close(fig)


if __name__ == "__main__":
    run()
