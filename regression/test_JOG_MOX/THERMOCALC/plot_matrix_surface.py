#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
import matplotlib.tri as mtri
import numpy as np
import pandas as pd

plt.style.use("seaborn-v0_8-whitegrid")
plt.rcParams.update({
    "figure.figsize": (10, 7),
    "font.size": 12,
    "axes.labelsize": 15,
    "axes.titlesize": 12,
    "xtick.labelsize": 12,
    "ytick.labelsize": 12,
    "legend.fontsize": 12,
    "figure.dpi": 300,
    "axes.grid": True,
    "grid.alpha": 0.5,
    "grid.linestyle": "--",
    "lines.linewidth": 2,
    "lines.markersize": 6,
    "legend.frameon": False,
})


INPUT_FILE = Path(__file__).resolve().with_name("U0p78-Pu0p22-O_1bar.csv")
PLOT_DIR = Path(__file__).resolve().with_name("plot")
BOUNDARY_SOURCE_STEM = "1bar_Cs0p17_Mo0p22"
REGION_BOUNDARY_SPECS = [
    ("Cs2MoO4 gas", ("Cs2MoO4(gas)", "Cs2MoO4_gas"), "black"),
    ("Cs2MoO4 solid S1", ("Cs2MoO4(solid_S1)", "Cs2MoO4_solid_S1"), "red"),
    ("Cs2MoO4 solid S2", ("Cs2MoO4(solid_S2)", "Cs2MoO4_solid_S2"), "magenta"),
    ("Cs + MOO4 liquid", ("CsMOO4(liquid)", "Cs_MOO4_liquid"), "orange"),
]
CMAP = "viridis"
FIGSIZE = (8, 6)
VIEW_ELEV = 28
VIEW_AZIM = -130
LINEWIDTH = 0.08
ANTIALIASED = True
CONTOUR_LEVELS = 100
CONTOUR_LINE_INTERVAL_KJ_MOL = 25.0
# Radius-temperature profile provided by user.
RADIUS = np.array([0.4, 0.7, 0.9, 1.2, 1.5, 1.7, 2.0, 2.3, 2.5, 2.7], dtype=float)
TEMP_PROFILE = np.array([1853, 1834, 1795, 1733, 1651, 1560, 1428, 1290, 1140, 979], dtype=float)


def contour_line_levels(z: np.ndarray, interval: float = CONTOUR_LINE_INTERVAL_KJ_MOL) -> np.ndarray:
    """Return contour-line levels at fixed kJ/mol intervals within the z range."""
    z_min = np.nanmin(z)
    z_max = np.nanmax(z)
    first_level = np.ceil(z_min / interval) * interval
    last_level = np.floor(z_max / interval) * interval
    return np.arange(first_level, last_level + interval * 0.5, interval)


def draw_mask_boundary_on_surface(
    ax: plt.Axes,
    tri: mtri.Triangulation,
    x: np.ndarray,
    y: np.ndarray,
    z: np.ndarray,
    mask: np.ndarray,
    *,
    color: str = "black",
    linewidth: float = 2.0,
    label: str | None = None,
) -> bool:
    """Draw boundary segments between True/False mask regions on a triangulated 3D surface."""
    triangles = tri.triangles
    if triangles is None or triangles.size == 0:
        return False

    drew_line = False
    label_used = False

    for i0, i1, i2 in triangles:
        ids = np.array([i0, i1, i2], dtype=int)
        vals = mask[ids]
        true_count = np.count_nonzero(vals)
        if true_count == 0 or true_count == 3:
            continue

        pts = []
        for a, b in ((0, 1), (1, 2), (2, 0)):
            va = vals[a]
            vb = vals[b]
            if va == vb:
                continue

            ia = ids[a]
            ib = ids[b]
            px = 0.5 * (x[ia] + x[ib])
            py = 0.5 * (y[ia] + y[ib])
            pz = 0.5 * (z[ia] + z[ib])
            pts.append((px, py, pz))

        if len(pts) != 2:
            continue

        xs, ys, zs = zip(*pts)
        ax.plot(
            xs,
            ys,
            zs,
            color=color,
            linewidth=linewidth,
            label=label if label and not label_used else None,
        )
        drew_line = True
        label_used = True

    return drew_line


def draw_mask_boundary_on_contour(
    ax: plt.Axes,
    tri: mtri.Triangulation,
    x: np.ndarray,
    y: np.ndarray,
    mask: np.ndarray,
    *,
    color: str = "black",
    linewidth: float = 2.0,
    label: str | None = None,
) -> bool:
    """Draw boundary segments between True/False mask regions on a triangulated 2D contour plot."""
    triangles = tri.triangles
    if triangles is None or triangles.size == 0:
        return False

    drew_line = False
    label_used = False

    for i0, i1, i2 in triangles:
        ids = np.array([i0, i1, i2], dtype=int)
        vals = mask[ids]
        true_count = np.count_nonzero(vals)
        if true_count == 0 or true_count == 3:
            continue

        pts = []
        for a, b in ((0, 1), (1, 2), (2, 0)):
            va = vals[a]
            vb = vals[b]
            if va == vb:
                continue
            ia = ids[a]
            ib = ids[b]
            px = 0.5 * (x[ia] + x[ib])
            py = 0.5 * (y[ia] + y[ib])
            pts.append((px, py))

        if len(pts) != 2:
            continue

        xs, ys = zip(*pts)
        ax.plot(
            xs,
            ys,
            color=color,
            linewidth=linewidth,
            label=label if label and not label_used else None,
        )
        drew_line = True
        label_used = True

    return drew_line


def load_region_bounds(bounds_file: Path) -> tuple[float, float, float, float] | None:
    if not bounds_file.exists():
        return None
    try:
        bounds_df = pd.read_csv(bounds_file)
    except Exception as exc:
        print(f"Could not read boundary file '{bounds_file}': {exc}")
        return None
    required_cols = {
        "mu_o_min_kJ_per_mol",
        "mu_o_max_kJ_per_mol",
        "temperature_min_K",
        "temperature_max_K",
    }
    if bounds_df.empty or not required_cols.issubset(bounds_df.columns):
        print(f"Boundary file '{bounds_file}' is empty or missing required columns.")
        return None
    row = bounds_df.iloc[0]
    return (
        float(row["mu_o_min_kJ_per_mol"]),
        float(row["mu_o_max_kJ_per_mol"]),
        float(row["temperature_min_K"]),
        float(row["temperature_max_K"]),
    )

df = pd.read_csv(INPUT_FILE, sep=None, engine="python")

mu = pd.to_numeric(df["Chemical potential of O [J/mol]"], errors="coerce").to_numpy(dtype=float)
om = pd.to_numeric(df["Amount of O [mol]"], errors="coerce").to_numpy(dtype=float)
temp = pd.to_numeric(df["Temperature [K]"], errors="coerce").to_numpy(dtype=float)

# U + Pu = 1 => O/M is numerically equal to Amount of O [mol].
o_over_m = om
mask = np.isfinite(temp) & np.isfinite(o_over_m) & np.isfinite(mu)
x = o_over_m[mask]
y = temp[mask]
mu_o_kj = mu[mask] / 1000.0
df_valid = df.loc[mask].copy()
df_valid["O/M"] = x
df_valid["mu(O) [kJ/mol]"] = mu_o_kj
mu_o2_kj = 2.0 * mu_o_kj
shared_cbar_min = min(mu_o_kj.min(), mu_o2_kj.min())
shared_cbar_max = max(mu_o_kj.max(), mu_o2_kj.max())
region_masks: list[tuple[str, str, np.ndarray]] = []
for region_label, region_tags, region_color in REGION_BOUNDARY_SPECS:
    bounds = None
    chosen_bounds_file: Path | None = None
    for region_tag in region_tags:
        bounds_file = PLOT_DIR / f"{BOUNDARY_SOURCE_STEM}_{region_tag}_formation_boundaries.csv"
        bounds = load_region_bounds(bounds_file)
        if bounds is not None:
            chosen_bounds_file = bounds_file
            break
    if bounds is None:
        continue
    mu_min, mu_max, t_min, t_max = bounds
    region_mask = (
        (mu_o_kj >= mu_min)
        & (mu_o_kj <= mu_max)
        & (y >= t_min)
        & (y <= t_max)
    )
    region_masks.append((region_label, region_color, region_mask))
    print(
        f"{region_label} boundaries from file: "
        f"{chosen_bounds_file.name}, "
        f"mu(O) {mu_min:.3f}..{mu_max:.3f} kJ/mol, "
        f"T {t_min:.1f}..{t_max:.1f} K, points in matrix={int(np.count_nonzero(region_mask))}"
    )
if not region_masks:
    print("No region boundary files found. Contours will be plotted without region margins.")

# O
z = mu_o_kj
z_label = "Chemical potential of O (kJ/mol)"

tri = mtri.Triangulation(x, y)

fig = plt.figure(figsize=FIGSIZE)
ax = fig.add_subplot(1, 1, 1, projection="3d")
surf = ax.plot_trisurf(
    tri,
    z,
    cmap=CMAP,
    linewidth=LINEWIDTH,
    alpha=1.0
)

ax.view_init(elev=VIEW_ELEV, azim=VIEW_AZIM)
fig.colorbar(surf, ax=ax, shrink=0.65, pad=0.08, label=z_label, alpha=0.7)
ax.set_ylabel("Temperature (K)")
ax.set_xlabel("Oxygen-to-metal ratio (-)")
ax.set_zlabel(z_label)
ax.set_title("Pu/M = 0.22, 1 bar")
fig.tight_layout(rect=[0, 0, 1, 0.97])
plt.savefig("plot/OxygenPotentialO")

# O contour with boundaries
figc, axc = plt.subplots(figsize=FIGSIZE)
contour = axc.tricontourf(
    tri,
    z,
    levels=CONTOUR_LEVELS,
    cmap=CMAP,
    alpha=1.0,
)
contour_lines = axc.tricontour(
    tri,
    z,
    levels=contour_line_levels(z),
    colors="k",
    linewidths=0.2,
)
axc.clabel(contour_lines, inline=True, fontsize=7, fmt="%.0f")
contour_boundary_drawn = False
for region_label, region_color, region_mask in region_masks:
    if np.any(region_mask) and np.any(~region_mask):
        drew = draw_mask_boundary_on_contour(
            axc,
            tri,
            x,
            y,
            region_mask,
            color=region_color,
            linewidth=2.0,
            label=region_label,
        )
        contour_boundary_drawn = contour_boundary_drawn or drew
figc.colorbar(contour, ax=axc, shrink=0.85, pad=0.02, label=z_label)
axc.set_ylabel("Temperature (K)")
axc.set_xlabel("Oxygen-to-metal ratio (-)")
axc.set_title("Pu/M = 0.22, 1 bar")
if contour_boundary_drawn:
    axc.legend(loc="upper right", frameon=False)
figc.tight_layout(rect=[0, 0, 1, 0.97])
figc.savefig("plot/OxygenPotentialO_contour")

# O2
z = mu_o2_kj
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
    vmin=shared_cbar_min,
    vmax=shared_cbar_max,
)

ax.view_init(elev=VIEW_ELEV, azim=VIEW_AZIM)
fig.colorbar(surf, ax=ax, shrink=0.65, pad=0.08, label=z_label)
ax.set_ylabel("Temperature (K)")
ax.set_xlabel("Oxygen-to-metal ratio (-)")
ax.set_zlabel(z_label)
ax.set_title("Pu/M = 0.22, 1 bar")
fig.tight_layout(rect=[0, 0, 1, 0.97])
plt.savefig("plot/OxygenPotentialO2")

# O2 contour with boundaries
figc, axc = plt.subplots(figsize=FIGSIZE)
contour = axc.tricontourf(
    tri,
    z,
    levels=CONTOUR_LEVELS,
    cmap=CMAP,
    vmin=shared_cbar_min,
    vmax=shared_cbar_max,
)
contour_lines = axc.tricontour(
    tri,
    z,
    levels=contour_line_levels(z),
    colors="k",
    linewidths=0.2,
    vmin=shared_cbar_min,
    vmax=shared_cbar_max,
)
axc.clabel(contour_lines, inline=True, fontsize=7, fmt="%.0f")
contour_boundary_drawn = False
for region_label, region_color, region_mask in region_masks:
    if np.any(region_mask) and np.any(~region_mask):
        drew = draw_mask_boundary_on_contour(
            axc,
            tri,
            x,
            y,
            region_mask,
            color=region_color,
            linewidth=2.0,
            label=region_label,
        )
        contour_boundary_drawn = contour_boundary_drawn or drew
figc.colorbar(contour, ax=axc, shrink=0.85, pad=0.02, label=z_label)
axc.set_ylabel("Temperature (K)")
axc.set_xlabel("Oxygen-to-metal ratio (-)")
axc.set_title("Pu/M = 0.22, 1 bar")
if contour_boundary_drawn:
    axc.legend(loc="upper right",frameon=False)
figc.tight_layout(rect=[0, 0, 1, 0.97])
figc.savefig("plot/OxygenPotentialO2_contour")

plt.show()

print("Plotting complete.")
print("Margin for other calculations:")
print(f"  O/M: {x.min():.3f} to {x.max():.3f}")
print(f"  Temperature: {y.min():.1f} to {y.max():.1f} K")
print(f"  mu(O): {mu_o_kj.min():.5f} to {mu_o_kj.max():.5f} kJ/mol")

global_margin_mask = (
    np.isclose(x, x.min())
    | np.isclose(x, x.max())
    | np.isclose(y, y.min())
    | np.isclose(y, y.max())
    | np.isclose(mu_o_kj, mu_o_kj.min())
    | np.isclose(mu_o_kj, mu_o_kj.max())
)
global_margin_df = df_valid.loc[
    global_margin_mask,
    [
        "O/M",
        "Temperature [K]",
        "mu(O) [kJ/mol]",
        "Chemical potential of O [J/mol]",
        "Amount of O [mol]",
    ],
].copy()
print("Dataframe rows where global margins are reached:")
print(global_margin_df.to_string(index=True))
