"""Reproduce Rizk 2025 Fig. 4 — diffusivities of Xe and U-vacancies vs T.

Two-panel figure:
  - Left:  D_Xe = D1 + D3 (we don't sum D2; Rizk Sec. 3.1.1: "D2 negligible
           for Xe"). The D2 curve is plotted dashed-grey for diagnostic.
  - Right: D_v = D1 + D3 (D2 dropped entirely after the 2026-05-09 audit;
           Rizk Tab. 2 D2_v parameters are mathematically broken — the
           "Tab. 2 raw" curve is plotted dashed-grey to visualise the bug.

Comparison with Rizk Fig. 4:
  - At T > 1300 K our D_v matches the published Fig. 4 (because Fig. 4
    actually shows D_tot dominated by D1 thermal, not D2 as the caption
    might suggest).
  - At T < 1300 K our D_v is higher than Rizk Fig. 4 because we add the
    Schneider 2024 athermal D3 contribution (constant in T) which keeps
    the total above ~2.5e-22 m^2/s.

Output:
    un_calibration/reports/fig4_diffusivity_vs_T/fig4_diffusivity_vs_T.png
    un_calibration/reports/fig4_diffusivity_vs_T/fig4_diffusivity_vs_T.csv
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

import un_model as m                                    # noqa: E402
from rizk_constants import RIZK_CONSTANTS               # noqa: E402
from builder import build_un_params                     # noqa: E402

T_MIN_K = 800.0
T_MAX_K = 2200.0
N_T = 200

# Reference fission-rate density used for D2 / D3 evaluation.
# All scripts use this value (DN1 / Rizk validation: LHR=100 kW/m, d=8.30 mm).
F_REF = 5.0e19

OUT_DIR = ROOT / "reports" / "fig4_diffusivity_vs_T"


def _d2_vu_paper_raw(T, F):
    """Rizk 2025 Tab. 2 V_U D2 with the paper's Eq. 4 form (negative signs):

        D2 = sqrt(F) * A20 * exp(-B1/kT - B2/(kT)^2)

    Plugged with the V_U row coefficients (A20=1.32e-19, B1=-0.62, B2=-0.04).
    The result is unphysical (~1e-6 m^2/s at T=1500 K) — included here to
    document the published bug, not to be used in any calculation.
    """
    A20 = 1.32e-19
    B1  = -0.62
    B2  = -0.04
    kBT = RIZK_CONSTANTS["KB_EV"] * T
    expo = -B1 / kBT - B2 / (kBT ** 2)
    expo = max(min(expo, 700.0), -745.0)
    return math.sqrt(F) * A20 * math.exp(expo)


def _d2_xe_centipede(T, F, p):
    """D2 for Xe per Rizk Eq. 4 + Tab. 2 Xe coefficients.

    This curve is finite and small but is NOT summed into D_g per Rizk
    Sec. 3.1.1 ("D2 negligible for Xe").
    """
    kBT = RIZK_CONSTANTS["KB_EV"] * T
    expo = (-p.B21_xe / kBT
            -p.B22_xe / (kBT ** 2)
            -p.B23_xe / (kBT ** 3))
    expo = max(min(expo, 700.0), -745.0)
    return math.sqrt(F) * p.A20_xe * math.exp(expo)


def run():
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    T_grid = np.linspace(T_MIN_K, T_MAX_K, N_T)

    rows = []
    for T in T_grid:
        # Build a UNParameters at this T (any burnup; only T enters D1, D3).
        p = build_un_params(float(T), 1.3, dt_h=12.0, n_modes=22)
        # Override fission_rate to the reference value
        p.fission_rate = F_REF
        Dg, Dg_parts = m.xe_diffusivity_UN(p)
        Dv, Dv_parts = m.vacancy_diffusivity_UN(p)
        d2_xe = _d2_xe_centipede(float(T), F_REF, p)
        d2_vu_broken = _d2_vu_paper_raw(float(T), F_REF)

        rows.append({
            "T_K": float(T),
            "D_Xe_tot": Dg,
            "D1_Xe": Dg_parts["D1_Xe"],
            "D3_Xe": Dg_parts["D3_Xe"],
            "D2_Xe_diagnostic": d2_xe,
            "D_v_tot": Dv,
            "D1_v": Dv_parts["Dv1"],
            "D3_v": Dv_parts["Dv3"],
            "D2_v_paper_broken": d2_vu_broken,
        })

    # --- CSV ---
    csv_path = OUT_DIR / "fig4_diffusivity_vs_T.csv"
    with csv_path.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        for r in rows:
            w.writerow(r)
    print(f"  wrote {csv_path}")

    Ts          = [r["T_K"] for r in rows]
    Dxe_tot     = [r["D_Xe_tot"] for r in rows]
    D1_xe       = [r["D1_Xe"] for r in rows]
    D3_xe       = [r["D3_Xe"] for r in rows]
    D2_xe_diag  = [r["D2_Xe_diagnostic"] for r in rows]
    Dv_tot      = [r["D_v_tot"] for r in rows]
    D1_v        = [r["D1_v"] for r in rows]
    D3_v        = [r["D3_v"] for r in rows]
    D2_v_broken = [r["D2_v_paper_broken"] for r in rows]

    # --- Figure ---
    fig, (axL, axR) = plt.subplots(1, 2, figsize=(14, 5.5))

    # Y-axis floor: D2 contributions go to ~1e-107 (Xe Centipede min) and
    # ~1e+50 (V_U Tab. 2 raw maximum), which would crush everything else on
    # a log scale. Clamp to a readable physical range.
    Y_FLOOR = 1.0e-30
    Y_CEIL  = 1.0e-12

    # Xe panel
    axL.plot(Ts, Dxe_tot, "-", linewidth=2.5, color="black",
             label=r"$D_g = D_1 + D_3$ (used)")
    axL.plot(Ts, D1_xe, "--", linewidth=1.8, color="tab:orange",
             label=r"$D_1$ thermal (Rizk Tab. 2)")
    axL.plot(Ts, D3_xe, ":", linewidth=2.0, color="tab:purple",
             label=r"$D_3$ athermal (Rizk Tab. 2)")
    axL.plot(Ts, D2_xe_diag, "--", linewidth=1.0, color="grey", alpha=0.6,
             label=r"$D_2$ Centipede ($< 10^{-85}$, off scale)")
    axL.set_xlabel("Temperature (K)")
    axL.set_ylabel(r"$D_{Xe}$ (m$^2$/s)")
    axL.set_yscale("log")
    axL.set_ylim(Y_FLOOR, Y_CEIL)
    axL.set_title(rf"Xenon diffusivity in UN at $\dot{{F}}={F_REF:.0e}$ fiss/m$^3$/s")
    axL.grid(alpha=0.3, which="both")
    axL.legend(fontsize=9, loc="lower right")

    # V_U panel
    axR.plot(Ts, Dv_tot, "-", linewidth=2.5, color="black",
             label=r"$D_v = D_1 + D_3$ (used)")
    axR.plot(Ts, D1_v, "--", linewidth=1.8, color="tab:orange",
             label=r"$D_1$ thermal (Rizk Tab. 2)")
    axR.plot(Ts, D3_v, ":", linewidth=2.0, color="tab:purple",
             label=r"$D_3$ athermal (Schneider 2024)")
    axR.plot(Ts, D2_v_broken, "--", linewidth=1.0, color="red", alpha=0.5,
             label=r"$D_2$ from Tab. 2 raw (BROKEN, off scale, $> 10^{-6}$ at 1500 K)")
    axR.set_xlabel("Temperature (K)")
    axR.set_ylabel(r"$D_v$ (m$^2$/s)")
    axR.set_yscale("log")
    axR.set_ylim(Y_FLOOR, Y_CEIL)
    axR.set_title(rf"U-vacancy diffusivity in UN at $\dot{{F}}={F_REF:.0e}$ fiss/m$^3$/s")
    axR.grid(alpha=0.3, which="both")
    axR.legend(fontsize=9, loc="lower right")

    fig.suptitle(
        "Diffusivity vs T — our model decomposed (cf. Rizk 2025 Fig. 4)",
        fontsize=12,
    )
    plt.tight_layout(rect=[0, 0, 1, 0.96])

    png_path = OUT_DIR / "fig4_diffusivity_vs_T.png"
    plt.savefig(png_path, dpi=150, bbox_inches="tight")
    print(f"  wrote {png_path}")
    plt.close(fig)

    # --- Console table at probe T values ---
    probe_T = [1000.0, 1300.0, 1500.0, 1700.0, 2000.0]
    print()
    print(f"{'T':>6}  {'D_g_tot':>10}  {'D1_Xe':>10}  {'D3_Xe':>10}  "
          f"{'D_v_tot':>10}  {'D1_v':>10}  {'D3_v':>10}")
    print("-" * 78)
    for T in probe_T:
        # nearest-T row
        i = min(range(len(rows)), key=lambda k: abs(rows[k]["T_K"] - T))
        r = rows[i]
        print(f"{r['T_K']:>6.0f}  {r['D_Xe_tot']:>10.2e}  {r['D1_Xe']:>10.2e}  "
              f"{r['D3_Xe']:>10.2e}  {r['D_v_tot']:>10.2e}  "
              f"{r['D1_v']:>10.2e}  {r['D3_v']:>10.2e}")


if __name__ == "__main__":
    run()
