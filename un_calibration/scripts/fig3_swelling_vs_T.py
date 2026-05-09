"""Reproduce Rizk 2025 Fig. 3 — intra-granular swelling vs T at 1.1 / 1.3 / 3.2 % FIMA.

Three side-by-side panels, one per burnup. Model curves: dislocation (P2) +
bulk swelling. Experimental data from Ronchi 1978 (Rizk Ref. [44]) digitised
in `un_data.EXP_SWELLING_T`.

Output:
    un_calibration/reports/fig3_swelling_vs_T/fig3_swelling_vs_T.png
    un_calibration/reports/fig3_swelling_vs_T/fig3_swelling_vs_T.csv
"""

import sys
import pathlib

ROOT = pathlib.Path(__file__).resolve().parents[1]
for sub in ("model", "config"):
    sys.path.insert(0, str(ROOT / sub))

import csv                                              # noqa: E402
import numpy as np                                      # noqa: E402
import matplotlib.pyplot as plt                         # noqa: E402

import un_data                                          # noqa: E402
from builder import model_runner                        # noqa: E402

# --- Parameters at top of script (no argparse) ---
T_GRID = np.arange(900, 1750, 50)
BURNUPS = [1.1, 1.3, 3.2]
DT_HOURS = 12.0
N_MODES = 25

OUT_DIR = ROOT / "reports" / "fig3_swelling_vs_T"


def run():
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    points = [(float(T), bu) for bu in BURNUPS for T in T_GRID]
    results = [model_runner(T, bu, dt_h=DT_HOURS, n_modes=N_MODES)
               for (T, bu) in points]

    # --- CSV dump ---
    csv_path = OUT_DIR / "fig3_swelling_vs_T.csv"
    with csv_path.open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["T_K", "burnup_percent_FIMA",
                    "swelling_dislocation_percent",
                    "swelling_bulk_percent",
                    "swelling_intragranular_percent"])
        for r in results:
            w.writerow([r["T"], r["burnup"],
                        r["swelling_d_percent"],
                        r["swelling_b_percent"],
                        r["swelling_ig_percent"]])
    print(f"  wrote {csv_path}")

    # --- Figure ---
    fig, axes = plt.subplots(1, 3, figsize=(15, 4.5), sharey=True)
    for ax, bu in zip(axes, BURNUPS):
        rs = [r for r in results if abs(r["burnup"] - bu) < 0.01]
        Ts  = [r["T"] for r in rs]
        swD = [r["swelling_d_percent"] for r in rs]
        swB = [r["swelling_b_percent"] for r in rs]
        ax.plot(Ts, swD, "--", linewidth=2, color="goldenrod",
                label="dislocation/P2 swelling")
        ax.plot(Ts, swB, ":", linewidth=2, color="forestgreen",
                label="bulk swelling")

        if abs(bu - 1.1) < 0.01:
            exp_100 = [p for p in un_data.EXP_SWELLING_T
                       if abs(p["burnup"] - 1.1) < 0.01 and p["series"] == "100 kW/m"]
            exp_119 = [p for p in un_data.EXP_SWELLING_T
                       if abs(p["burnup"] - 1.1) < 0.01 and p["series"] == "119 kW/m"]
            ax.scatter([p["T"] for p in exp_100], [p["swelling"] for p in exp_100],
                       marker="s", s=50, color="black",
                       label="Exp P2 100 kW/m", zorder=5)
            ax.scatter([p["T"] for p in exp_119], [p["swelling"] for p in exp_119],
                       marker="^", s=50, facecolor="white", edgecolor="black",
                       linewidth=1.3, label="Exp P2 119 kW/m", zorder=5)
        else:
            exp = [p for p in un_data.EXP_SWELLING_T if abs(p["burnup"] - bu) < 0.01]
            ax.scatter([p["T"] for p in exp], [p["swelling"] for p in exp],
                       marker="s", s=50, color="black", label="Exp P2", zorder=5)

        ax.set_xlabel("Temperature (K)")
        ax.set_title(f"{bu} % FIMA")
        ax.grid(alpha=0.3)
        ax.legend(fontsize=9, loc="upper left")

    axes[0].set_ylabel("Swelling (%)")
    plt.tight_layout()

    png_path = OUT_DIR / "fig3_swelling_vs_T.png"
    plt.savefig(png_path, dpi=150, bbox_inches="tight")
    print(f"  wrote {png_path}")
    plt.close(fig)


if __name__ == "__main__":
    run()
