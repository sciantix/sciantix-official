#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import matplotlib.tri as mtri
import numpy as np
import pandas as pd


INPUT_FILE = Path(__file__).resolve().with_name("U0p78-Pu0p22-O_1bar.csv")
CMAP = "viridis"
FIGSIZE = (8, 6)
VIEW_ELEV = 28
VIEW_AZIM = -130
LINEWIDTH = 0.08
ANTIALIASED = True


df = pd.read_csv(INPUT_FILE, sep=None, engine="python")

mu = pd.to_numeric(df["Chemical potential of O [J/mol]"], errors="coerce").to_numpy(dtype=float)
om = pd.to_numeric(df["Amount of O [mol]"], errors="coerce").to_numpy(dtype=float)
temp = pd.to_numeric(df["Temperature [K]"], errors="coerce").to_numpy(dtype=float)

# U + Pu = 1 => O/M is numerically equal to Amount of O [mol].
o_over_m = om

# O
z = mu / 1000.0
z_label = "Chemical potential of O (kJ/mol)"

mask = np.isfinite(temp) & np.isfinite(o_over_m) & np.isfinite(z)
x = o_over_m
y = temp

tri = mtri.Triangulation(x, y)

fig = plt.figure(figsize=FIGSIZE)
ax = fig.add_subplot(1, 1, 1, projection="3d")
surf = ax.plot_trisurf(
    tri,
    z,
    cmap=CMAP,
    linewidth=LINEWIDTH,
    antialiased=ANTIALIASED,
)

ax.view_init(elev=VIEW_ELEV, azim=VIEW_AZIM)
fig.colorbar(surf, ax=ax, shrink=0.65, pad=0.08, label=z_label)
ax.set_ylabel("Temperature (K)")
ax.set_xlabel("Oxygen-to-metal ratio (-)")
ax.set_zlabel(z_label)
ax.set_title("Pu/M = 0.22, 1 bar")
fig.tight_layout(rect=[0, 0, 1, 0.97])
plt.savefig("plot/OxygenPotentialO")

# O2
z = 2 * mu / 1000.0
z_label = "Chemical potential of O2 (kJ/mol)"

tri = mtri.Triangulation(x, y)

fig = plt.figure(figsize=FIGSIZE)
ax = fig.add_subplot(1, 1, 1, projection="3d")
surf = ax.plot_trisurf(
    tri,
    z,
    cmap=CMAP,
    linewidth=LINEWIDTH,
    antialiased=ANTIALIASED,
)

ax.view_init(elev=VIEW_ELEV, azim=VIEW_AZIM)
fig.colorbar(surf, ax=ax, shrink=0.65, pad=0.08, label=z_label)
ax.set_ylabel("Temperature (K)")
ax.set_xlabel("Oxygen-to-metal ratio (-)")
ax.set_zlabel(z_label)
ax.set_title("Pu/M = 0.22, 1 bar")
fig.tight_layout(rect=[0, 0, 1, 0.97])
plt.savefig("plot/OxygenPotentialO2")
plt.show()