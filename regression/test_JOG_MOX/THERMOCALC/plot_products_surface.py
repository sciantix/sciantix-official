#!/usr/bin/env python3
from __future__ import annotations

import re
from pathlib import Path

import matplotlib.pyplot as plt
import matplotlib.tri as mtri
import numpy as np
import pandas as pd
from matplotlib.backends.backend_pdf import PdfPages

# =========================
# USER SETTINGS
# =========================
INPUT_FILE = ["1bar_Cs0p17_Mo0p22.csv", 
              "10bar_Cs0p17_Mo0p22.csv",
              "70bar_Cs0p17_Mo0p22.csv"]
INCLUDE_KEYWORDS: list[str] = []
EXCLUDE_KEYWORDS: list[str] = []

# Global default appearance (used for every plot unless overridden below).
DEFAULT_PLOT_SETTINGS: dict[str, object] = {
    "enabled": True,
    "cmap": "viridis",
    "figsize": (8, 6),
    "linewidth": 0.08,
    "antialiased": True,
    "view_elev": 28,
    "view_azim": -130,
    "title": None,  # None -> automatic title
    "colorbar_label": None,  # None -> column name
    "zscale": "linear",  # "linear" or "log"
    "xlim": None,  # tuple(min, max) or None
    "ylim": None,  # tuple(min, max) or None
    "zlim": (0.0, 1.0),  # tuple(min, max) or None
    "contour_enabled": True,
    "contour_levels": 20,
}

# Output collection options (to avoid too many PNG files).
SAVE_INDIVIDUAL_PNG = False
SAVE_MULTIPAGE_PDF = True

# Derived quantity: fraction of CS2MOO4 gas over total gas
# = Site fraction of CS2MOO4 on GAS sublattice 1 * Mole fraction of all components in GAS.
ADD_CS2MOO4_OVER_TOTAL_GAS = True
CS2MOO4_SITE_FRACTION_KEY = "site fraction of cs2moo4 on sublattice 1 in gas"
TOTAL_GAS_MOLE_FRACTION_KEY = "Mole fraction of GAS"
CS2MOO4_OVER_TOTAL_LABEL = "Fraction of CS2MOO4 in total GAS"

# Derived quantity: fraction of MOO3 gas over total gas
# = Site fraction of MOO3 on GAS sublattice 1 * Mole fraction of all components in GAS.
ADD_MOO3_OVER_TOTAL_GAS = True
MOO3_SITE_FRACTION_KEY = "site fraction of moo3 on sublattice 1 in gas"
MOO3_OVER_TOTAL_LABEL = "Fraction of MOO3 in total GAS"

# Derived quantity: fraction of MO gas over total gas
# = Element fraction of MO in GAS * Mole fraction of all components in GAS.
ADD_MO_OVER_TOTAL_GAS = True
MO_SITE_FRACTION_KEY = "Mole fraction of mo in gas"
MO_OVER_TOTAL_LABEL = "Fraction of MO in total GAS"

def canonical_name(name: str) -> str:
    """Drop duplicated-column suffixes like ' [1]'."""
    return re.sub(r"\s*\[\d+\]\s*$", "", name).strip()


def find_axis_column(columns: list[str], key: str) -> str:
    for col in columns:
        if canonical_name(col).lower().startswith(key.lower()):
            return col
    raise ValueError(f"Could not find axis column starting with: {key}")


def find_column_by_canonical_contains(columns: list[str], key: str) -> str:
    key_l = key.lower()
    for col in columns:
        if key_l in canonical_name(col).lower():
            return col
    raise ValueError(f"Could not find column containing: {key}")


def should_keep_column(
    name: str,
    x_name: str,
    y_name: str,
    include_keywords: list[str],
    exclude_keywords: list[str],
) -> bool:
    lname = name.lower()
    if lname.startswith("chemical potential"):
        return False
    if lname == x_name.lower() or lname == y_name.lower():
        return False

    if include_keywords and not any(k in lname for k in include_keywords):
        return False
    if exclude_keywords and any(k in lname for k in exclude_keywords):
        return False
    return True


def get_plot_settings(column_name: str) -> dict[str, object]:
    settings = dict(DEFAULT_PLOT_SETTINGS)
    return settings

plot_dir = Path(__file__).resolve().with_name("plot")
plot_dir.mkdir(exist_ok=True)

for file in INPUT_FILE:
    pdf_path = plot_dir / f"{file}_plots.pdf"
    pdf = PdfPages(pdf_path) if SAVE_MULTIPAGE_PDF else None

    df = pd.read_csv(Path(__file__).resolve().with_name(file), sep=None, engine="python")
    columns = list(df.columns)

    x_col = find_axis_column(columns, "Chemical potential of O")
    y_col = find_axis_column(columns, "Temperature")

    # Convert everything to numeric where possible.
    numeric_df = df.apply(pd.to_numeric, errors="coerce")

    if ADD_CS2MOO4_OVER_TOTAL_GAS:
        cs2moo4_col = find_column_by_canonical_contains(columns, CS2MOO4_SITE_FRACTION_KEY)
        total_gas_col = find_column_by_canonical_contains(columns, TOTAL_GAS_MOLE_FRACTION_KEY)
        numeric_df[CS2MOO4_OVER_TOTAL_LABEL] = numeric_df[cs2moo4_col] * numeric_df[total_gas_col]
        columns.append(CS2MOO4_OVER_TOTAL_LABEL)

    if ADD_MOO3_OVER_TOTAL_GAS:
        moo3_col = find_column_by_canonical_contains(columns, MOO3_SITE_FRACTION_KEY)
        total_gas_col = find_column_by_canonical_contains(columns, TOTAL_GAS_MOLE_FRACTION_KEY)
        numeric_df[MOO3_OVER_TOTAL_LABEL] = numeric_df[moo3_col] * numeric_df[total_gas_col]
        columns.append(MOO3_OVER_TOTAL_LABEL)

    if ADD_MO_OVER_TOTAL_GAS:
        mo_col = find_column_by_canonical_contains(columns, MO_SITE_FRACTION_KEY)
        total_gas_col = find_column_by_canonical_contains(columns, TOTAL_GAS_MOLE_FRACTION_KEY)
        numeric_df[MO_OVER_TOTAL_LABEL] = numeric_df[mo_col] * numeric_df[total_gas_col]
        columns.append(MO_OVER_TOTAL_LABEL)

    x = numeric_df[x_col].to_numpy(dtype=float) / 1000.0
    y = numeric_df[y_col].to_numpy(dtype=float)

    x_name = canonical_name(x_col).replace("[J/mol]", "[kJ/mol]")
    y_name = canonical_name(y_col)

    include_keywords = [k.lower() for k in INCLUDE_KEYWORDS]
    exclude_keywords = [k.lower() for k in EXCLUDE_KEYWORDS]

    z_columns: list[str] = []
    for col in columns:
        c_name = canonical_name(col)
        if should_keep_column(c_name, x_name, y_name, include_keywords, exclude_keywords) and numeric_df[col].notna().any():
            z_columns.append(col)

    if not z_columns:
        raise ValueError("No plottable Z columns found after excluding x/y.")

    print("Plottable Z columns:")
    for col in z_columns:
        print(f"- {canonical_name(col)}")

    for z_col in z_columns:
        c_name = canonical_name(z_col)
        settings = get_plot_settings(c_name)
        if not bool(settings["enabled"]):
            continue

        z = numeric_df[z_col].to_numpy(dtype=float)
        mask = np.isfinite(x) & np.isfinite(y) & np.isfinite(z)

        if str(settings["zscale"]).lower() == "log":
            mask = mask & (z > 0.0)

        if np.count_nonzero(mask) < 3:
            print(f"Skipping '{z_col}' (not enough valid points).")
            continue

        x_m = x[mask]
        y_m = y[mask]
        z_m = z[mask]

        tri = mtri.Triangulation(x_m, y_m)

        fig = plt.figure(figsize=tuple(settings["figsize"]))
        ax = fig.add_subplot(1, 1, 1, projection="3d")
        surf = ax.plot_trisurf(
            tri,
            z_m,
            cmap=str(settings["cmap"]),
            linewidth=float(settings["linewidth"]),
            antialiased=bool(settings["antialiased"]),
        )
        ax.view_init(elev=float(settings["view_elev"]), azim=float(settings["view_azim"]))
        if str(settings["zscale"]).lower() == "log":
            ax.set_zscale("log")

        if settings["xlim"] is not None:
            ax.set_xlim(tuple(settings["xlim"]))
        if settings["ylim"] is not None:
            ax.set_ylim(tuple(settings["ylim"]))
        if settings["zlim"] is not None:
            ax.set_zlim(tuple(settings["zlim"]))

        colorbar_label = c_name if settings["colorbar_label"] is None else str(settings["colorbar_label"])
        fig.colorbar(surf, ax=ax, shrink=0.65, pad=0.08, label=colorbar_label)
        ax.set_xlabel(x_name)
        ax.set_ylabel(y_name)
        ax.set_zlabel(c_name)
        title = f"3D Surface: {c_name}" if settings["title"] is None else str(settings["title"])
        ax.set_title(title)
        fig.tight_layout(rect=[0, 0, 1, 0.97])
        if SAVE_INDIVIDUAL_PNG:
            fig.savefig(plot_dir / f"{file}_{c_name}.png")
        if pdf is not None:
            pdf.savefig(fig)
        plt.close(fig)

        if bool(settings["contour_enabled"]):
            fig2, ax2 = plt.subplots(figsize=tuple(settings["figsize"]))
            contour = ax2.tricontourf(
                tri,
                z_m,
                levels=int(settings["contour_levels"]),
                cmap=str(settings["cmap"]),
            )

            if settings["xlim"] is not None:
                ax2.set_xlim(tuple(settings["xlim"]))
            if settings["ylim"] is not None:
                ax2.set_ylim(tuple(settings["ylim"]))

            colorbar_label = c_name if settings["colorbar_label"] is None else str(settings["colorbar_label"])
            cbar = fig2.colorbar(contour, ax=ax2, shrink=0.85, pad=0.02, label=colorbar_label)
            if str(settings["zscale"]).lower() == "log":
                contour_lines = ax2.tricontour(tri, z_m, levels=int(settings["contour_levels"]), colors="k", linewidths=0.2)
                cbar.ax.set_yscale("log")
                cbar.add_lines(contour_lines)
            else:
                ax2.tricontour(tri, z_m, levels=int(settings["contour_levels"]), colors="k", linewidths=0.2)

            ax2.set_xlabel(x_name)
            ax2.set_ylabel(y_name)
            contour_title = f"Contour: {c_name}" if settings["title"] is None else f"{settings['title']} (contour)"
            ax2.set_title(contour_title)
            fig2.tight_layout(rect=[0, 0, 1, 0.97])
            if SAVE_INDIVIDUAL_PNG:
                fig2.savefig(plot_dir / f"{file}_{c_name}_contour.png")
            if pdf is not None:
                pdf.savefig(fig2)
            plt.close(fig2)

    if pdf is not None:
        pdf.close()
