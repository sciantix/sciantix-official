#!/usr/bin/env python3
"""Check of the grain-boundary sink strength used in the exchange term of the FMP model (EC November 2026).

Problem (README, "Exchange between the zones"): quasi-steady diffusion in a spherical grain of radius a with uniform
production P and the intragranular particles as uniform sinks of strength k1^2, zero concentration at r = a:
    D (c'' + 2 c'/r) - D k1^2 c + P = 0,   c'(0) = 0,   c(a) = 0.
The boundary takes what the particles do not, P - D k1^2 c_mean = D kGB^2 c_mean, so kGB^2 = P / (D c_mean) - k1^2.
Analytical result: kGB^2 = k1^2 F / (1 - F), F = 3 (coth z - 1/z) / z, z = k1 a.

The script solves the problem with finite volumes, compares kGB^2 with the analytical expression and with its two
limits (15/a^2 without particles, 3 k1/a for z >> 1), and writes figures/exchange_check.png.
"""

import os

import numpy as np
from scipy.sparse import diags
from scipy.sparse.linalg import spsolve

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

from plot import C

HERE = os.path.dirname(os.path.abspath(__file__))
A_GRAIN = 4.4e-6  # grain radius of the TEM cases (m), grain size 8.8 um (Angleraud 2024)
D, P = 1.0, 1.0    # kGB^2 does not depend on D and P
Z_R000 = 144.0     # z = k1 a of r000 at 54.1 MWd/kgUO2 (three-zone model, literature parameters)


def kgb2_numerical(k1, a=A_GRAIN, n=20000):
    """kGB^2 from a finite-volume solution, cells refined near the boundary for large z."""
    s = np.linspace(0.0, 1.0, n + 1)
    faces = a * (1.0 - (1.0 - s) ** 3) if k1 * a > 5.0 else a * s
    centres = 0.5 * (faces[1:] + faces[:-1])
    volumes = (faces[1:] ** 3 - faces[:-1] ** 3) / 3.0  # per unit solid angle
    conductance = D * faces[1:-1] ** 2 / np.diff(centres)
    main = D * k1**2 * volumes
    main[:-1] += conductance
    main[1:] += conductance
    main[-1] += D * faces[-1] ** 2 / (faces[-1] - centres[-1])  # c = 0 at r = a
    c = spsolve(diags([-conductance, main, -conductance], [-1, 0, 1], format="csc"), P * volumes)
    c_mean = np.sum(c * volumes) / np.sum(volumes)
    return P / (D * c_mean) - k1**2


def kgb2_analytical(k1, a=A_GRAIN):
    z = k1 * a
    F = 3.0 / z * (1.0 / np.tanh(z) - 1.0 / z)
    return k1**2 * F / (1.0 - F)


def profile(z, x):
    """c / (P / D k1^2) at distance x = a - r from the boundary; z = 0: c / c(0) without particles."""
    r = A_GRAIN - x
    if z == 0.0:
        return 1.0 - (r / A_GRAIN) ** 2
    k = z / A_GRAIN
    # a sinh(k r) / (r sinh(k a)), written with exponentials to avoid overflow
    ratio = np.exp(-k * x) * (1.0 - np.exp(-2.0 * k * r)) / (1.0 - np.exp(-2.0 * z))
    return 1.0 - A_GRAIN / r * ratio


def main():
    a = A_GRAIN
    z_values = np.array([0.1, 0.3, 1.0, 3.0, 10.0, 30.0, 100.0, Z_R000])
    numerical = np.array([kgb2_numerical(z / a) for z in z_values])
    analytical = np.array([kgb2_analytical(z / a) for z in z_values])
    print(f"Grain-boundary sink strength with intragranular particles, a = {a * 1e6:.1f} um")
    print(f"{'z = k1 a':>9} {'numerical (1/m2)':>18} {'analytical (1/m2)':>18} {'rel. diff':>10} {'L1 = 3/(a kGB2)':>16}")
    for z, nu, an in zip(z_values, numerical, analytical):
        print(f"{z:9g} {nu:18.5e} {an:18.5e} {abs(nu / an - 1):10.1e} {3 / (a * an) * 1e9:13.1f} nm")
    print(f"limits: 15/a^2 = {15 / a**2:.4e} 1/m2 (L1 = a/5 = {a / 5 * 1e9:.0f} nm); "
          f"z = {Z_R000:.0f} (r000, 54.1 MWd/kgUO2): 3 k1/a = {3 * Z_R000 / a**2:.4e} 1/m2 (L1 = 1/k1 = {a / Z_R000 * 1e9:.0f} nm)")

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(10, 3.9))

    x = np.geomspace(1e-10, a * 0.999, 2000)
    for z, colour, label in [(0.0, C[0], "no particles: c / c(0) = 1 - (r/a)$^2$"),
                             (Z_R000, C[1], f"r000 at 54.1 MWd/kgUO2, z = {Z_R000:.0f}")]:
        ax1.plot(x * 1e9, profile(z, x), color=colour, linewidth=2, label=label)
    ax1.axvline(a / Z_R000 * 1e9, color="0.5", linewidth=1, linestyle=":")
    ax1.text(a / Z_R000 * 1e9 * 1.15, 0.08, f"1/k$_1$ = {a / Z_R000 * 1e9:.0f} nm", fontsize=8, color="0.3")
    ax1.set(xscale="log", xlim=(1e-1, a * 1e9), ylim=(0, 1.05), xlabel="Distance from the boundary, a - r (nm)",
            ylabel="Dissolved metal (normalised)", title="Profile in the grain (zero at the boundary)")
    ax1.legend(fontsize=7.5, loc="upper left", frameon=False)

    z_line = np.geomspace(0.05, 500, 400)
    ax2.plot(z_line, [kgb2_analytical(z / a) * a**2 for z in z_line], color=C[0], linewidth=2,
             label="analytical, $k_1^2 F/(1-F)$")
    ax2.plot(z_values, numerical * a**2, "o", color=C[1], markersize=8, markeredgecolor="white",
             label="finite volumes")
    ax2.plot(z_line, np.full_like(z_line, 15.0), "--", color="0.55", linewidth=1, label="15 (no particles, Booth)")
    ax2.plot(z_line[z_line > 3], 3 * z_line[z_line > 3], ":", color="0.35", linewidth=1.2, label="3z (particles dominate)")
    ax2.set(xscale="log", yscale="log", xlabel="z = k$_1$ a (-)", ylabel="k$_{GB}^2$ a$^2$ (-)",
            title="Grain-boundary sink strength")
    ax2.legend(fontsize=7.5, loc="upper left", frameon=False)
    for ax in (ax1, ax2):
        ax.grid(True, color="0.9", linewidth=0.6)
        ax.set_axisbelow(True)

    fig.tight_layout()
    path = os.path.join(HERE, "figures", "exchange_check.png")
    fig.savefig(path, dpi=170, bbox_inches="tight", facecolor="white")
    print(f"Figure: {path}")


if __name__ == "__main__":
    main()
