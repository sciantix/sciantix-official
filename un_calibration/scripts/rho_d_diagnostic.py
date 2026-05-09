"""Diagnostic 3D surface of the active rho_d(F, T) law vs Blank 1984 anchors.

Picks the active law from MANUAL_PARAMS:
  - Both flags False     -> constant rho_d (Rizk 2025, default)
  - USE_RHO_FT = True    -> Blank-saturating + Ray-Blank (config.rho_d_laws.rho_d_FT)
  - USE_RHO_EXP = True   -> Rizk-NEAMS 2023 exponential (config.rho_d_laws.rho_d_exp)

Plots log10(rho_d) over (T, burnup-MWd/kgHM) and overlays Blank 1984 Table 3
anchors (specimen C3/1, 6.8 a/o ≈ 63.8 MWd/kgHM).

Output:
    un_calibration/reports/rho_d_diagnostic/rho_d_diagnostic.png
"""

import sys
import pathlib

ROOT = pathlib.Path(__file__).resolve().parents[1]
for sub in ("model", "config"):
    sys.path.insert(0, str(ROOT / sub))

import numpy as np                                      # noqa: E402
import matplotlib.pyplot as plt                         # noqa: E402
from mpl_toolkits.mplot3d import Axes3D                 # noqa: F401, E402

from manual_params import MANUAL_PARAMS                 # noqa: E402
from rho_d_laws import rho_d_FT, rho_d_exp              # noqa: E402

# 1 % FIMA (a/o) ≈ 9.38 MWd/kgHM for U-based fuels at 200 MeV/fission.
AO_TO_MWD_PER_KGHM = 9.38

OUT_DIR = ROOT / "reports" / "rho_d_diagnostic"


def run():
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    rs = MANUAL_PARAMS["rho_scale"]

    # --- Pick the active law ---
    if MANUAL_PARAMS["USE_RHO_EXP"]:
        law_fn   = lambda T, F: rho_d_exp(T, F, rs)
        law_name = "Rizk-NEAMS 2023 exponential (Eq. 3.38)"
    elif MANUAL_PARAMS["USE_RHO_FT"]:
        law_fn   = lambda T, F: rho_d_FT(T, F, rs)
        law_name = "Blank-saturating + Ray-Blank"
    else:
        rho_const = MANUAL_PARAMS["rho_d"]
        law_fn    = lambda T, F: rho_const
        law_name  = f"constant rho_d = {rho_const:.2e} m^-2 (Rizk 2025 Sec. 2.2.2)"

    # --- Surface grid ---
    T_vals = np.linspace(800.0, 2100.0, 60)
    F_vals_ao = np.linspace(0.1, 10.0, 60)
    T_mesh, F_mesh_ao = np.meshgrid(T_vals, F_vals_ao)
    F_mesh_MWd = F_mesh_ao * AO_TO_MWD_PER_KGHM

    RHO_grid = np.array([[law_fn(T_mesh[i, j], F_mesh_ao[i, j])
                          for j in range(T_mesh.shape[1])]
                         for i in range(T_mesh.shape[0])])
    log_RHO = np.log10(RHO_grid)

    # --- Blank Table 3 anchors (specimen C3/1, F = 6.8 a/o) ---
    blank_T      = [940.0, 990.0, 1015.0, 1100.0, 1300.0]
    blank_rho_m2 = [6.4e14, 7.0e14, 7.0e14, 8.0e14, 8.6e14]
    blank_F_ao   = 6.8
    blank_F_MWd  = blank_F_ao * AO_TO_MWD_PER_KGHM

    fig = plt.figure(figsize=(11, 7))
    ax = fig.add_subplot(111, projection="3d")
    surf = ax.plot_surface(T_mesh, F_mesh_MWd, log_RHO,
                           cmap="viridis", alpha=0.78, edgecolor="none",
                           rstride=2, cstride=2)
    ax.scatter(blank_T, [blank_F_MWd] * len(blank_T), np.log10(blank_rho_m2),
               color="red", s=80, edgecolor="black", linewidth=0.8,
               depthshade=False,
               label=f"Blank 1984 Table 3 (C3/1, 6.8 a/o = {blank_F_MWd:.1f} MWd/kgHM)",
               zorder=10)
    ax.set_xlabel("Temperature (K)")
    ax.set_ylabel("Burnup (MWd/kgHM)")
    ax.set_zlabel(r"$\log_{10} \rho_d$  (m$^{-2}$)")
    ax.set_title(f"$\\rho_d(F, T)$ -- {law_name}  [rho_scale={rs:.2f}]",
                 fontsize=11)
    ax.view_init(elev=22, azim=-58)
    fig.colorbar(surf, ax=ax, shrink=0.55, pad=0.10).set_label(
        r"$\log_{10} \rho_d$  (m$^{-2}$)")
    ax.legend(loc="upper left", fontsize=9)
    plt.tight_layout()

    png_path = OUT_DIR / "rho_d_diagnostic.png"
    plt.savefig(png_path, dpi=150, bbox_inches="tight")
    print(f"  wrote {png_path}")
    plt.close(fig)

    # --- Console table ---
    print()
    print(f"Active law: {law_name}")
    print(f"Conversion: 1 a/o = {AO_TO_MWD_PER_KGHM:.2f} MWd/kgHM   |   "
          f"Blank C3/1 (6.8 a/o) = {blank_F_MWd:.1f} MWd/kgHM")
    print()
    print(f"rho_d at Blank's anchor temperatures (F = 6.8 a/o, active law):")
    print(f"  {'T (K)':>6}  {'Blank meas.':>11}  {'active law':>11}")
    for T_a, blk in zip(blank_T, blank_rho_m2):
        val = law_fn(T_a, blank_F_ao)
        print(f"  {T_a:>6.0f}  {blk:>11.2e}  {val:>11.2e}")


if __name__ == "__main__":
    run()
