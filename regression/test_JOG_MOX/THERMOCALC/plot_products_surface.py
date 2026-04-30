#!/usr/bin/env python3
from __future__ import annotations

import re
from pathlib import Path

import matplotlib.pyplot as plt
import matplotlib.tri as mtri
import numpy as np
import pandas as pd
from matplotlib.backends.backend_pdf import PdfPages

plt.style.use("seaborn-v0_8-whitegrid")
plt.rcParams.update({
    "figure.figsize": (10, 7),
    "font.size": 12,
    "axes.labelsize": 13,
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

# =========================
# USER SETTINGS
# =========================
INPUT_FILE = ["1bar_Cs0p17_Mo0p22.csv", "1bar_Cs0p17_Mo0p22_2.csv", "70bar_Cs0p17_Mo0p22.csv"]
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

TOTAL_LIQUID_MOLE_FRACTION_KEY = "Mole fraction of LIQUID"
TOTAL_GAS_MOLE_FRACTION_KEY = "Mole fraction of GAS"

# Derived quantity: fraction of CS2MOO4 gas over total gas
# = Site fraction of CS2MOO4 on GAS sublattice 1 * Mole fraction of all components in GAS.
ADD_CS2MOO4_OVER_TOTAL_GAS = True
CS2MOO4_SITE_FRACTION_KEY = "site fraction of cs2moo4 on sublattice 1 in gas"
CS2MOO4_OVER_TOTAL_LABEL = "Cs2MoO4 (gas)"

# Derived quantity: fraction of MOO3 gas over total gas
# = Site fraction of MOO3 on GAS sublattice 1 * Mole fraction of all components in GAS.
ADD_MOO3_OVER_TOTAL_GAS = True
MOO3_SITE_FRACTION_KEY = "site fraction of moo3 on sublattice 1 in gas"
MOO3_OVER_TOTAL_LABEL = "MoO3 (gas)"

# Derived quantity: fraction of MO gas over total gas
# = Element fraction of MO in GAS * Mole fraction of all components in GAS.
ADD_MO_OVER_TOTAL_GAS = True
MO_SITE_FRACTION_KEY = "Mole fraction of mo in gas"
MO_OVER_TOTAL_LABEL = "Mo in gaseous phase"

ADD_CS_OVER_TOTAL_GAS = True
CS_SITE_FRACTION_KEY = "Mole fraction of cs in gas"
CS_OVER_TOTAL_LABEL = "Cs in gaseous phase"

# Derived quantity: fraction of CS2MOO4 liquid over total liquid
# = Site fraction of MOO4 on LIQUID sublattice 1 * Mole fraction of all components in LIQUID.
ADD_MOO4_OVER_TOTAL_LIQUID = True
MOO4_SITE_FRACTION_KEY = "site fraction of moo4-2"
MOO4_OVER_TOTAL_LABEL = "MoO4-2 (liquid, sublattice 2)"


CS2MOO4_FORMATION_TOL = 0.1
CS2MOO4_SOLID_S1_KEY = "mole fraction of cs2moo4_s1"
CS2MOO4_SOLID_S2_KEY = "mole fraction of cs2moo4_s2"

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


def save_formation_boundaries(
    *,
    plot_dir: Path,
    file_stem: str,
    tag: str,
    x: np.ndarray,
    y: np.ndarray,
    indicators: dict[str, np.ndarray],
    require_all_positive: bool,
    tol: float = 0.0,
) -> None:
    if not indicators:
        print(f"[{file_stem}] No indicators found for '{tag}'; boundary files were not written.")
        return

    finite_mask = np.isfinite(x) & np.isfinite(y)
    positive_masks: list[np.ndarray] = []
    for values in indicators.values():
        finite_mask = finite_mask & np.isfinite(values)
        positive_masks.append(values > tol)

    if require_all_positive:
        indicator_mask = np.ones_like(x, dtype=bool)
        for m in positive_masks:
            indicator_mask = indicator_mask & m
        condition_mode = "all_positive"
    else:
        indicator_mask = np.zeros_like(x, dtype=bool)
        for m in positive_masks:
            indicator_mask = indicator_mask | m
        condition_mode = "any_positive"

    formation_mask = finite_mask & indicator_mask
    formation_points = np.count_nonzero(formation_mask)
    if formation_points == 0:
        print(f"[{file_stem}] No formation points found for '{tag}'; boundary files were not written.")
        return

    mu_min = x[formation_mask].min()
    mu_max = x[formation_mask].max()
    temp_min = y[formation_mask].min()
    temp_max = y[formation_mask].max()

    bounds_data: dict[str, object] = {
        "tag": tag,
        "condition_mode": condition_mode,
        "mu_o_min_kJ_per_mol": mu_min,
        "mu_o_max_kJ_per_mol": mu_max,
        "temperature_min_K": temp_min,
        "temperature_max_K": temp_max,
        "formation_points": int(formation_points),
    }
    for name, values in indicators.items():
        bounds_data[f"{name}_min"] = float(values[formation_mask].min())
        bounds_data[f"{name}_max"] = float(values[formation_mask].max())

    bounds_path = plot_dir / f"{file_stem}_{tag}_formation_boundaries.csv"
    pd.DataFrame([bounds_data]).to_csv(bounds_path, index=False)

    margin_mask = formation_mask & (
        np.isclose(x, mu_min)
        | np.isclose(x, mu_max)
        | np.isclose(y, temp_min)
        | np.isclose(y, temp_max)
    )
    margin_rows_data: dict[str, np.ndarray] = {
        "mu_o_kJ_per_mol": x[margin_mask],
        "temperature_K": y[margin_mask],
    }
    for name, values in indicators.items():
        margin_rows_data[name] = values[margin_mask]

    margin_rows_df = pd.DataFrame(margin_rows_data, index=np.where(margin_mask)[0])
    margin_rows_path = plot_dir / f"{file_stem}_{tag}_formation_margin_rows.csv"
    margin_rows_df.to_csv(margin_rows_path, index=True)

    print(f"[{file_stem}] Saved {tag} formation boundaries: {bounds_path}")
    print(f"[{file_stem}] Saved {tag} formation margin rows: {margin_rows_path}")

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
    cs2moo4_indicator_col: str | None = None
    solid_s1_col: str | None = None
    solid_s2_col: str | None = None

    if ADD_CS2MOO4_OVER_TOTAL_GAS:
        cs2moo4_col = find_column_by_canonical_contains(columns, CS2MOO4_SITE_FRACTION_KEY)
        total_col = find_column_by_canonical_contains(columns, TOTAL_GAS_MOLE_FRACTION_KEY)
        numeric_df[CS2MOO4_OVER_TOTAL_LABEL] = numeric_df[cs2moo4_col] * numeric_df[total_col]
        columns.append(CS2MOO4_OVER_TOTAL_LABEL)
        cs2moo4_indicator_col = CS2MOO4_OVER_TOTAL_LABEL
    try:
        solid_s1_col = find_column_by_canonical_contains(columns, CS2MOO4_SOLID_S1_KEY)
    except ValueError:
        solid_s1_col = None
    try:
        solid_s2_col = find_column_by_canonical_contains(columns, CS2MOO4_SOLID_S2_KEY)
    except ValueError:
        solid_s2_col = None

    if ADD_MOO3_OVER_TOTAL_GAS:
        moo3_col = find_column_by_canonical_contains(columns, MOO3_SITE_FRACTION_KEY)
        total_col = find_column_by_canonical_contains(columns, TOTAL_GAS_MOLE_FRACTION_KEY)
        numeric_df[MOO3_OVER_TOTAL_LABEL] = numeric_df[moo3_col] * numeric_df[total_col]
        columns.append(MOO3_OVER_TOTAL_LABEL)

    if ADD_MO_OVER_TOTAL_GAS:
        mo_col = find_column_by_canonical_contains(columns, MO_SITE_FRACTION_KEY)
        total_col = find_column_by_canonical_contains(columns, TOTAL_GAS_MOLE_FRACTION_KEY)
        numeric_df[MO_OVER_TOTAL_LABEL] = numeric_df[mo_col] * numeric_df[total_col]
        columns.append(MO_OVER_TOTAL_LABEL)

    if ADD_CS_OVER_TOTAL_GAS:
        cs_col = find_column_by_canonical_contains(columns, CS_SITE_FRACTION_KEY)
        total_col = find_column_by_canonical_contains(columns, TOTAL_GAS_MOLE_FRACTION_KEY)
        numeric_df[CS_OVER_TOTAL_LABEL] = numeric_df[cs_col] * numeric_df[total_col]
        columns.append(CS_OVER_TOTAL_LABEL)
    
    
    if ADD_MOO4_OVER_TOTAL_LIQUID:
        moo4_col = find_column_by_canonical_contains(columns, MOO4_SITE_FRACTION_KEY)
        total_col = find_column_by_canonical_contains(columns, TOTAL_LIQUID_MOLE_FRACTION_KEY)
        numeric_df[MOO4_OVER_TOTAL_LABEL] = numeric_df[moo4_col] * numeric_df[total_col]
        columns.append(MOO4_OVER_TOTAL_LABEL)

    x = numeric_df[x_col].to_numpy(dtype=float) / 1000.0
    y = numeric_df[y_col].to_numpy(dtype=float)
    if cs2moo4_indicator_col is not None:
        save_formation_boundaries(
            plot_dir=plot_dir,
            file_stem=Path(file).stem,
            tag="Cs2MoO4(gas)",
            x=x,
            y=y,
            indicators={cs2moo4_indicator_col: numeric_df[cs2moo4_indicator_col].to_numpy(dtype=float)},
            require_all_positive=True,
            tol=CS2MOO4_FORMATION_TOL,
        )

    if solid_s1_col is not None:
        save_formation_boundaries(
            plot_dir=plot_dir,
            file_stem=Path(file).stem,
            tag="Cs2MoO4(solid_S1)",
            x=x,
            y=y,
            indicators={"MoleFraction_CS2MOO4_S1": numeric_df[solid_s1_col].to_numpy(dtype=float)},
            require_all_positive=True,
            tol=CS2MOO4_FORMATION_TOL,
        )
    if solid_s2_col is not None:
        save_formation_boundaries(
            plot_dir=plot_dir,
            file_stem=Path(file).stem,
            tag="Cs2MoO4(solid_S2)",
            x=x,
            y=y,
            indicators={"MoleFraction_CS2MOO4_S2": numeric_df[solid_s2_col].to_numpy(dtype=float)},
            require_all_positive=True,
            tol=CS2MOO4_FORMATION_TOL,
        )

    liquid_indicators: dict[str, np.ndarray] = {}
    if CS_OVER_TOTAL_LABEL in numeric_df.columns:
        liquid_indicators[CS_OVER_TOTAL_LABEL] = numeric_df[CS_OVER_TOTAL_LABEL].to_numpy(dtype=float)
    if MOO4_OVER_TOTAL_LABEL in numeric_df.columns:
        liquid_indicators[MOO4_OVER_TOTAL_LABEL] = numeric_df[MOO4_OVER_TOTAL_LABEL].to_numpy(dtype=float)
    save_formation_boundaries(
        plot_dir=plot_dir,
        file_stem=Path(file).stem,
        tag="CsMOO4(liquid)",
        x=x,
        y=y,
        indicators=liquid_indicators,
        require_all_positive=True,
        tol=CS2MOO4_FORMATION_TOL,
    )

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
            vmin=0,
            vmax=1
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
                contour_lines = ax2.tricontour(tri, z_m, levels=int(settings["contour_levels"]), colors="k", linewidths=0.2, vmin=0, vmax=1.0)
                cbar.ax.set_yscale("log")
                cbar.add_lines(contour_lines)
            else:
                ax2.tricontour(tri, z_m, levels=int(settings["contour_levels"]), colors="k", linewidths=0.2, vmin=0, vmax=1.0)

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
