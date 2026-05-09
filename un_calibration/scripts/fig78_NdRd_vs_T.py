"""Reproduce Rizk 2025 Fig. 7 (N_d vs T) and Fig. 8 (R_d vs T) at 1.3 % FIMA.

Two side-by-side panels: dislocation-bubble radius (log Y) and concentration
(log Y) vs temperature. Experimental anchors from `un_data.EXP_RD_T_13` and
`EXP_ND_T_13` (digitised from Rizk Fig. 7 / 8; original data: Ronchi 1978).

Output:
    un_calibration/reports/fig78_NdRd_vs_T/fig78_NdRd_vs_T.png
    un_calibration/reports/fig78_NdRd_vs_T/fig78_NdRd_vs_T.csv
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

T_GRID = np.arange(900, 2100, 50)
BURNUP_PERCENT_FIMA = 1.3
DT_HOURS = 12.0
N_MODES = 25

OUT_DIR = ROOT / "reports" / "fig78_NdRd_vs_T"


def run():
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    results = [model_runner(float(T), BURNUP_PERCENT_FIMA,
                            dt_h=DT_HOURS, n_modes=N_MODES)
               for T in T_GRID]
    Ts  = [r["T"] for r in results]
    Rds = [r["Rd_nm"] for r in results]
    Nds = [r["Nd"] for r in results]

    # --- CSV ---
    csv_path = OUT_DIR / "fig78_NdRd_vs_T.csv"
    with csv_path.open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["T_K", "Rd_nm", "Nd_per_m3"])
        for T, R, N in zip(Ts, Rds, Nds):
            w.writerow([T, R, N])
    print(f"  wrote {csv_path}")

    # --- Figure ---
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 4.2))

    ax1.plot(Ts, Rds, "-", linewidth=2, label="Model R_d")
    ax1.scatter([p["T"] for p in un_data.EXP_RD_T_13],
                [p["R_nm"] for p in un_data.EXP_RD_T_13],
                color="red", s=40, marker="^",
                label="Rizk Fig. 8 exp (1.3% FIMA)", zorder=5)
    ax1.set_xlabel("Temperature (K)")
    ax1.set_ylabel("R_d (nm)")
    ax1.set_yscale("log")
    ax1.set_title(f"Dislocation-bubble radius ({BURNUP_PERCENT_FIMA}% FIMA)")
    ax1.legend()
    ax1.grid(alpha=0.3, which="both")

    ax2.semilogy(Ts, Nds, "-", linewidth=2, label="Model N_d")
    ax2.scatter([p["T"] for p in un_data.EXP_ND_T_13],
                [p["N"] for p in un_data.EXP_ND_T_13],
                color="red", s=40, marker="^",
                label="Rizk Fig. 7 exp (1.3% FIMA)", zorder=5)
    ax2.set_xlabel("Temperature (K)")
    ax2.set_ylabel("N_d (m^-3)")
    ax2.set_title(f"Dislocation-bubble concentration ({BURNUP_PERCENT_FIMA}% FIMA)")
    ax2.legend()
    ax2.grid(alpha=0.3, which="both")

    plt.tight_layout()
    png_path = OUT_DIR / "fig78_NdRd_vs_T.png"
    plt.savefig(png_path, dpi=150, bbox_inches="tight")
    print(f"  wrote {png_path}")
    plt.close(fig)


if __name__ == "__main__":
    run()
