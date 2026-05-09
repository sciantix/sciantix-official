"""Fine 1D scan of f_n (bulk nucleation factor) to identify the UN-specific optimum.

Background: Rizk 2025 inherits f_n = 1×10⁻⁶ from the U3Si2 work of Barani 2019;
this is not a UN-specific calibration. The coarse `sensitivity_scan.py` indicated
a minimum between 1e-6 and 1e-5. This script does a finer logarithmic scan
in that region and reports two figures of merit:

  - global RMSE (all 39 Ronchi anchors)
  - max |bias| across the three burnup bins (asymmetry metric)

Output:
    un_calibration/reports/calibrate_f_n/calibrate_f_n.csv
    un_calibration/reports/calibrate_f_n/calibrate_f_n.png

The "best" f_n is picked by *minimum global RMSE*; the "balanced" f_n is the
value that minimises the largest per-burnup |bias|.
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
from builder import model_runner                        # noqa: E402

DT_HOURS = 12.0
N_MODES = 25
BURNUPS = [1.1, 1.3, 3.2]

# Logarithmic scan with denser sampling between 1e-6 and 1e-5
F_N_VALUES = [
    1.0e-7, 3.0e-7,
    1.0e-6, 1.5e-6, 2.0e-6, 3.0e-6, 5.0e-6, 7.0e-6,
    1.0e-5, 1.5e-5, 2.0e-5, 3.0e-5,
    1.0e-4,
]

OUT_DIR = ROOT / "reports" / "calibrate_f_n"


def _rmse(rs):
    return math.sqrt(sum(r * r for r in rs) / len(rs)) if rs else float("nan")


def _mean(xs):
    return sum(xs) / len(xs) if xs else float("nan")


def run():
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    rows = []
    series = {bu: {"f_n": [], "rmse": [], "bias": []} for bu in BURNUPS}
    series["all"] = {"f_n": [], "rmse": [], "bias": []}

    print(f"\n=== f_n scan (Rizk reference 1.0e-06) ===")
    print(f"{'f_n':>10}  {'RMSE 1.1':>9}  {'RMSE 1.3':>9}  {'RMSE 3.2':>9}  "
          f"{'RMSE all':>9}  {'bias 1.1':>9}  {'bias 1.3':>9}  {'bias 3.2':>9}  "
          f"{'bias all':>9}")
    print("-" * 110)

    for v in F_N_VALUES:
        mp = dict(MP_DEFAULT)
        mp["f_n"] = v

        residuals_per_bu = {bu: [] for bu in BURNUPS}
        all_residuals = []
        for exp in un_data.EXP_SWELLING_T:
            T = float(exp["T"])
            bu = float(exp["burnup"])
            out = model_runner(T, bu, dt_h=DT_HOURS, n_modes=N_MODES,
                               manual_params=mp)
            res = out["swelling_d_percent"] - exp["swelling"]
            residuals_per_bu[bu].append(res)
            all_residuals.append(res)

        rmse_bu = {bu: _rmse(residuals_per_bu[bu]) for bu in BURNUPS}
        bias_bu = {bu: _mean(residuals_per_bu[bu]) for bu in BURNUPS}
        rmse_all = _rmse(all_residuals)
        bias_all = _mean(all_residuals)

        for bu in BURNUPS:
            rows.append({
                "f_n": v, "burnup_percent": bu,
                "n_points": len(residuals_per_bu[bu]),
                "rmse_swelling_d": rmse_bu[bu],
                "bias_swelling_d": bias_bu[bu],
            })
            series[bu]["f_n"].append(v)
            series[bu]["rmse"].append(rmse_bu[bu])
            series[bu]["bias"].append(bias_bu[bu])
        rows.append({
            "f_n": v, "burnup_percent": "all",
            "n_points": len(all_residuals),
            "rmse_swelling_d": rmse_all,
            "bias_swelling_d": bias_all,
        })
        series["all"]["f_n"].append(v)
        series["all"]["rmse"].append(rmse_all)
        series["all"]["bias"].append(bias_all)

        print(f"{v:>10.2e}  {rmse_bu[1.1]:>9.3f}  {rmse_bu[1.3]:>9.3f}  "
              f"{rmse_bu[3.2]:>9.3f}  {rmse_all:>9.3f}  "
              f"{bias_bu[1.1]:>+9.3f}  {bias_bu[1.3]:>+9.3f}  "
              f"{bias_bu[3.2]:>+9.3f}  {bias_all:>+9.3f}")

    # --- Optima ---
    fns_arr   = np.array(series["all"]["f_n"])
    rmse_arr  = np.array(series["all"]["rmse"])
    bias_arr_per_bu = np.array([series[bu]["bias"] for bu in BURNUPS])  # (3, N)
    max_abs_bias = np.max(np.abs(bias_arr_per_bu), axis=0)

    i_rmse_opt = int(np.argmin(rmse_arr))
    i_bal_opt  = int(np.argmin(max_abs_bias))

    print()
    print(f"Optimum by global RMSE         : f_n = {fns_arr[i_rmse_opt]:.2e}  "
          f"(RMSE={rmse_arr[i_rmse_opt]:.3f}, max |bias|={max_abs_bias[i_rmse_opt]:.3f})")
    print(f"Optimum by max |bias|/burnup   : f_n = {fns_arr[i_bal_opt]:.2e}  "
          f"(RMSE={rmse_arr[i_bal_opt]:.3f}, max |bias|={max_abs_bias[i_bal_opt]:.3f})")

    # --- CSV ---
    csv_path = OUT_DIR / "calibrate_f_n.csv"
    with csv_path.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        for r in rows:
            w.writerow(r)
    print(f"\n  wrote {csv_path}")

    # --- Figure: 2 panels (RMSE, bias) vs log(f_n) ---
    fig, (axR, axB) = plt.subplots(1, 2, figsize=(14, 5))
    colors_bu = {1.1: "tab:blue", 1.3: "tab:orange", 3.2: "tab:red"}

    for bu in BURNUPS:
        axR.semilogx(series[bu]["f_n"], series[bu]["rmse"], "o-",
                     color=colors_bu[bu], linewidth=1.4, markersize=5,
                     label=f"{bu}% FIMA")
        axB.semilogx(series[bu]["f_n"], series[bu]["bias"], "o-",
                     color=colors_bu[bu], linewidth=1.4, markersize=5,
                     label=f"{bu}% FIMA")
    axR.semilogx(series["all"]["f_n"], series["all"]["rmse"], "s-",
                 color="black", linewidth=2.0, markersize=6,
                 label="all (39 pts)")
    axB.semilogx(series["all"]["f_n"], series["all"]["bias"], "s-",
                 color="black", linewidth=2.0, markersize=6,
                 label="all (39 pts)")

    axR.axvline(1.0e-6, color="grey", linestyle="--", alpha=0.7,
                label="Rizk reference 1e-6")
    axR.axvline(fns_arr[i_rmse_opt], color="green", linestyle=":", alpha=0.9,
                label=f"RMSE opt {fns_arr[i_rmse_opt]:.1e}")
    axB.axvline(1.0e-6, color="grey", linestyle="--", alpha=0.7)
    axB.axvline(fns_arr[i_bal_opt], color="green", linestyle=":", alpha=0.9,
                label=f"|bias|-opt {fns_arr[i_bal_opt]:.1e}")
    axB.axhline(0.0, color="black", linewidth=0.5, alpha=0.5)

    axR.set_xlabel("f_n")
    axR.set_ylabel("RMSE on Sw_d (% Sw)")
    axR.set_title("RMSE vs f_n")
    axR.grid(alpha=0.3, which="both")
    axR.legend(fontsize=8, loc="upper right")

    axB.set_xlabel("f_n")
    axB.set_ylabel("bias on Sw_d (% Sw)")
    axB.set_title("Bias vs f_n")
    axB.grid(alpha=0.3, which="both")
    axB.legend(fontsize=8, loc="upper right")

    fig.suptitle("Fine f_n calibration scan against Ronchi 1978", fontsize=12)
    plt.tight_layout(rect=[0, 0, 1, 0.95])

    png_path = OUT_DIR / "calibrate_f_n.png"
    plt.savefig(png_path, dpi=140, bbox_inches="tight")
    print(f"  wrote {png_path}")
    plt.close(fig)


if __name__ == "__main__":
    run()
