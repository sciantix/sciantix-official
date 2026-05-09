"""Reproduce Rizk 2025 Fig. 9 — gas partition vs T at 1.3 % FIMA.

Stacked-area plot showing the fraction of generated fission gas that ends up
in: matrix (in solution), bulk bubbles, dislocation bubbles, q_gb (everything
that left the grain interior).

Output:
    un_calibration/reports/fig9_gas_partition/fig9_gas_partition.png
    un_calibration/reports/fig9_gas_partition/fig9_gas_partition.csv
"""

import sys
import pathlib

ROOT = pathlib.Path(__file__).resolve().parents[1]
for sub in ("model", "config"):
    sys.path.insert(0, str(ROOT / sub))

import csv                                              # noqa: E402
import numpy as np                                      # noqa: E402
import matplotlib.pyplot as plt                         # noqa: E402

from builder import model_runner                        # noqa: E402

T_GRID = np.arange(900, 2100, 50)
BURNUP_PERCENT_FIMA = 1.3
DT_HOURS = 12.0
N_MODES = 25

OUT_DIR = ROOT / "reports" / "fig9_gas_partition"


def run():
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    results = [model_runner(float(T), BURNUP_PERCENT_FIMA,
                            dt_h=DT_HOURS, n_modes=N_MODES)
               for T in T_GRID]
    Ts     = [r["T"] for r in results]
    matrix = [r["matrix_gas_percent"] for r in results]
    bulk   = [r["bulk_gas_percent"] for r in results]
    disl   = [r["dislocation_gas_percent"] for r in results]
    inter  = [r["intergranular_gas_percent"] for r in results]
    rel    = [r["released_gas_percent"] for r in results]

    # --- CSV ---
    csv_path = OUT_DIR / "fig9_gas_partition.csv"
    with csv_path.open("w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["T_K", "matrix_percent", "bulk_percent",
                    "dislocation_percent", "intergranular_percent",
                    "released_FGR_percent"])
        for T, mat, b, d, i, fgr in zip(Ts, matrix, bulk, disl, inter, rel):
            w.writerow([T, mat, b, d, i, fgr])
    print(f"  wrote {csv_path}")

    # --- Figure ---
    fig, ax = plt.subplots(figsize=(10, 5))
    ax.stackplot(Ts, matrix, bulk, disl, inter, rel,
                 labels=["Matrix (in solution)", "Bulk bubbles",
                         "Dislocation bubbles", "Inter-granular bubbles",
                         "Released (FGR)"],
                 alpha=0.85)
    ax.set_xlabel("Temperature (K)")
    ax.set_ylabel("Fraction of generated gas (%)")
    ax.set_title(f"Gas partition vs T at {BURNUP_PERCENT_FIMA} % FIMA")
    ax.set_ylim(0, 100)
    ax.legend(loc="center right")
    ax.grid(alpha=0.3)
    plt.tight_layout()

    png_path = OUT_DIR / "fig9_gas_partition.png"
    plt.savefig(png_path, dpi=150, bbox_inches="tight")
    print(f"  wrote {png_path}")
    plt.close(fig)


if __name__ == "__main__":
    run()
