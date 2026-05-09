"""Vitanza-style curve: fission gas release (FGR) vs burnup at fixed T.

Reproduces the spirit of Rizk 2025 Fig. 10/11 — but for our UN model.
For each T, we run the model up to a target burnup and record the cumulative
FGR fraction. A family of curves is plotted, each at a different fuel
centerline temperature (with parabolic radial profile assumed implicitly:
here we use a single uniform T, i.e. point-estimate curve).

Output:
    un_calibration/reports/fig11_FGR_vs_burnup/fig11_FGR_vs_burnup.png
    un_calibration/reports/fig11_FGR_vs_burnup/fig11_FGR_vs_burnup.csv
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

# Centerline temperatures (Vitanza-curve style: each curve = one T).
TEMPERATURES = [1200.0, 1400.0, 1600.0, 1800.0, 2000.0, 2200.0]
BURNUP_GRID = np.linspace(0.1, 6.0, 24)   # % FIMA

DT_HOURS = 12.0
N_MODES = 25

OUT_DIR = ROOT / "reports" / "fig11_FGR_vs_burnup"


def run():
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    results = []
    for T in TEMPERATURES:
        for bu in BURNUP_GRID:
            out = model_runner(float(T), float(bu),
                               dt_h=DT_HOURS, n_modes=N_MODES)
            results.append({
                "T_K": float(T),
                "burnup_percent_fima": float(bu),
                "fgr_percent": out["fgr_percent"],
                "swelling_total_percent": out["swelling_total_percent"],
                "swelling_gas_total_percent": out["swelling_gas_total_percent"],
                "swelling_solid_percent": out["swelling_solid_percent"],
                "F_c": out["F_c"],
            })

    # --- CSV ---
    csv_path = OUT_DIR / "fig11_FGR_vs_burnup.csv"
    with csv_path.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(results[0].keys()))
        w.writeheader()
        for r in results:
            w.writerow(r)
    print(f"  wrote {csv_path}")

    # --- Figure ---
    fig, (ax_fgr, ax_sw) = plt.subplots(1, 2, figsize=(14, 5))

    cmap = plt.get_cmap("viridis")
    for i, T in enumerate(TEMPERATURES):
        rs = [r for r in results if abs(r["T_K"] - T) < 1e-3]
        bus = [r["burnup_percent_fima"] for r in rs]
        fgrs = [r["fgr_percent"] for r in rs]
        sws  = [r["swelling_total_percent"] for r in rs]
        color = cmap(i / max(1, len(TEMPERATURES) - 1))
        ax_fgr.plot(bus, fgrs, "-", linewidth=1.8, color=color,
                    label=f"T={T:.0f} K")
        ax_sw.plot(bus, sws, "-", linewidth=1.8, color=color,
                   label=f"T={T:.0f} K")

    # Solid swelling reference (T-independent, just 0.5·B per Rizk Eq. 19)
    bu_ref = sorted({r["burnup_percent_fima"] for r in results})
    solid_ref = [0.5 * b for b in bu_ref]
    ax_sw.plot(bu_ref, solid_ref, "--", color="black", linewidth=1.4, alpha=0.6,
               label="solid only (Eq. 19, 0.5·B)")

    # 1% FGR threshold reference (Vitanza-style)
    ax_fgr.axhline(1.0, color="grey", linestyle="--", alpha=0.6,
                   label="1% FGR threshold")

    ax_fgr.set_xlabel("Burnup (% FIMA)")
    ax_fgr.set_ylabel("Fission gas release FGR (%)")
    ax_fgr.set_title("Fission gas release vs burnup")
    ax_fgr.grid(alpha=0.3)
    ax_fgr.legend(fontsize=9, loc="upper left")

    ax_sw.set_xlabel("Burnup (% FIMA)")
    ax_sw.set_ylabel("Total swelling (%) — gas + solid")
    ax_sw.set_title("Total swelling (bulk + disl + GB + solid Eq. 19) vs burnup")
    ax_sw.grid(alpha=0.3)
    ax_sw.legend(fontsize=9, loc="upper left")

    fig.suptitle(
        "FGR + total swelling vs burnup at constant T  "
        "(Vitanza-style for UN; cf. Rizk 2025 Fig. 10/11)",
        fontsize=12,
    )
    plt.tight_layout(rect=[0, 0, 1, 0.95])

    png_path = OUT_DIR / "fig11_FGR_vs_burnup.png"
    plt.savefig(png_path, dpi=140, bbox_inches="tight")
    print(f"  wrote {png_path}")
    plt.close(fig)


if __name__ == "__main__":
    run()
