#!/usr/bin/env python3
"""Plot phase diagrams and gas-species maps from Thermo-Calc results (hardcoded settings).

Edit USER SETTINGS and run:
    python3 plot.py
"""

from __future__ import annotations

import math
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Sequence, Tuple

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import BoundaryNorm, ListedColormap
from matplotlib.patches import Patch


# =========================
# USER SETTINGS (edit here)
# =========================
INPUT_FILE = Path(__file__).resolve().with_name(
    "results_T500-3000K_dT50_ACRseries_1p0em16_to_1p0em02_ppd1_P_1p0e05_NCS_0.1700_NMO_0.2200.txt"
)
OUTPUT_DIR = Path(__file__).resolve().parent

# AUTO: choose Y=P if P varies, else Y=ACR(O) if ACR varies.
# Force options: "P", "ACR(O)", "AUTO"
Y_AXIS_MODE = "ACR(O)"

# axis scaling
USE_LOG_PRESSURE = True
USE_LOG_ACR = True

TOP_GAS_SPECIES = 6
SHOW_PLOTS = False


PhaseAssemblage = Tuple[str, ...]


@dataclass
class DataPoint:
    t_k: float
    p_pa: float
    assemblage: PhaseAssemblage
    values: Dict[str, float]


def parse_col_header(header_line: str) -> List[str]:
    matches = list(re.finditer(r"col-\d+=", header_line))
    if not matches:
        return []

    names: List[str] = []
    for i, m in enumerate(matches):
        start = m.start()
        end = matches[i + 1].start() if i + 1 < len(matches) else len(header_line)
        seg = header_line[start:end].strip().rstrip(",")
        eq = seg.find("=")
        if eq != -1:
            names.append(seg[eq + 1 :].strip().rstrip(","))
    return names


def parse_results_file(path: Path) -> List[DataPoint]:
    lines = path.read_text(encoding="utf-8").splitlines()
    points: List[DataPoint] = []

    i = 0
    n = len(lines)
    while i < n:
        if not lines[i].strip().startswith("Phase Region for:"):
            i += 1
            continue

        i += 1
        phases: List[str] = []
        while i < n:
            s = lines[i].strip()
            if not s:
                i += 1
                continue
            if s.startswith("col-1="):
                break
            if s.startswith("Phase Region for:"):
                break
            phases.append(s)
            i += 1

        if i >= n or not lines[i].lstrip().startswith("col-1="):
            continue

        col_names = parse_col_header(lines[i].strip())
        assemblage: PhaseAssemblage = tuple(phases)

        i += 1
        while i < n:
            s = lines[i].strip()
            if not s:
                i += 1
                continue
            if s.startswith("Phase Region for:"):
                break

            tokens = s.split()
            if len(tokens) < 2:
                i += 1
                continue

            try:
                vals = [float(tok) for tok in tokens]
            except ValueError:
                i += 1
                continue

            values: Dict[str, float] = {}
            for k, name in enumerate(col_names):
                if k < len(vals):
                    values[name] = vals[k]

            points.append(DataPoint(t_k=vals[0], p_pa=vals[1], assemblage=assemblage, values=values))
            i += 1

    if not points:
        raise ValueError(f"No points parsed from {path}")

    # Deduplicate exact duplicates on (T, P, ACR(O)) keeping first
    dedup: Dict[Tuple[float, float, float], DataPoint] = {}
    for p in points:
        acr = p.values.get("ACR(O)", np.nan)
        key = (round(p.t_k, 6), round(p.p_pa, 2), round(acr, 14) if np.isfinite(acr) else np.nan)
        if key not in dedup:
            dedup[key] = p

    return list(dedup.values())


def assemblage_label(assemblage: Sequence[str]) -> str:
    return " + ".join(assemblage)


def centers_to_edges(vals: np.ndarray, log_scale: bool) -> np.ndarray:
    vals = np.asarray(vals, dtype=float)
    if vals.size == 1:
        delta = max(abs(vals[0]) * 0.1, 1.0e-12)
        return np.array([vals[0] - delta, vals[0] + delta], dtype=float)

    if log_scale:
        safe = np.clip(vals, 1e-300, None)
        mids = np.sqrt(safe[:-1] * safe[1:])
        first = safe[0] / np.sqrt(safe[1] / safe[0])
        last = safe[-1] * np.sqrt(safe[-1] / safe[-2])
    else:
        mids = 0.5 * (vals[:-1] + vals[1:])
        first = vals[0] - (mids[0] - vals[0])
        last = vals[-1] + (vals[-1] - mids[-1])

    return np.concatenate(([first], mids, [last]))


def choose_y_axis(points: Sequence[DataPoint]) -> str:
    mode = Y_AXIS_MODE.upper()
    if mode in ("P", "ACR(O)"):
        return mode

    p_unique = len({round(p.p_pa, 2) for p in points})
    acr_unique = len({round(p.values.get("ACR(O)", np.nan), 14) for p in points if np.isfinite(p.values.get("ACR(O)", np.nan))})

    if p_unique > 1:
        return "P"
    if acr_unique > 1:
        return "ACR(O)"
    return "P"


def resolve_input_file(path: Path) -> Path:
    """Use configured file if present; otherwise pick newest results*.txt in the folder."""
    if path.exists():
        return path

    candidates = sorted(
        path.parent.glob("results*.txt"),
        key=lambda p: p.stat().st_mtime,
        reverse=True,
    )
    if not candidates:
        raise FileNotFoundError(f"Input file not found: {path}")
    return candidates[0]


def axis_label_and_values(points: Sequence[DataPoint], y_axis: str) -> Tuple[np.ndarray, str, bool, Dict[int, float]]:
    if y_axis == "P":
        y_vals = np.array(sorted({p.p_pa for p in points}), dtype=float)
        label = "Pressure [bar]"
        log_scale = USE_LOG_PRESSURE
        display_map = {i: y / 1e5 for i, y in enumerate(y_vals)}
    else:
        y_vals = np.array(sorted({p.values.get("ACR(O)") for p in points if np.isfinite(p.values.get("ACR(O)", np.nan))}), dtype=float)
        label = "ACR(O) [-]"
        log_scale = USE_LOG_ACR
        display_map = {i: y for i, y in enumerate(y_vals)}

    return y_vals, label, log_scale, display_map


def build_grids(
    points: Sequence[DataPoint],
    y_axis: str,
) -> Tuple[np.ndarray, np.ndarray, np.ndarray, List[PhaseAssemblage], Dict[str, np.ndarray], str, bool, Dict[int, float]]:
    t_vals = np.array(sorted({p.t_k for p in points}), dtype=float)
    y_vals, y_label, y_log, y_display = axis_label_and_values(points, y_axis)

    assemblages = sorted({p.assemblage for p in points}, key=assemblage_label)
    asm_to_idx = {a: i for i, a in enumerate(assemblages)}

    t_to_i = {v: i for i, v in enumerate(t_vals)}
    y_to_j = {v: j for j, v in enumerate(y_vals)}

    asm_grid = np.full((y_vals.size, t_vals.size), -1, dtype=int)

    gas_cols = sorted({k for p in points for k in p.values if re.match(r"Y\(GAS[^,]*,", k)})
    gas_grids: Dict[str, np.ndarray] = {
        g: np.full((y_vals.size, t_vals.size), np.nan, dtype=float) for g in gas_cols
    }

    for p in points:
        y = p.p_pa if y_axis == "P" else p.values.get("ACR(O)", np.nan)
        if not np.isfinite(y) or y not in y_to_j:
            continue
        i = t_to_i[p.t_k]
        j = y_to_j[y]
        asm_grid[j, i] = asm_to_idx[p.assemblage]
        for g in gas_cols:
            if g in p.values:
                gas_grids[g][j, i] = p.values[g]

    return t_vals, y_vals, asm_grid, assemblages, gas_grids, y_label, y_log, y_display


def tab20_extended(n: int) -> ListedColormap:
    if n <= 20:
        return ListedColormap(plt.cm.tab20(np.linspace(0, 1, n)))
    return ListedColormap(plt.cm.gist_ncar(np.linspace(0, 1, n)))


def gas_species_name(col: str) -> str:
    m = re.match(r"Y\(GAS[^,]*,(.+)\)", col)
    return m.group(1) if m else col


def plot_phase_diagram(
    t_vals: np.ndarray,
    y_vals: np.ndarray,
    asm_grid: np.ndarray,
    assemblages: Sequence[PhaseAssemblage],
    y_label: str,
    y_log: bool,
    y_display: Dict[int, float],
    out_path: Path,
) -> None:
    t_edges = centers_to_edges(t_vals, log_scale=False)
    y_plot_vals = np.array([y_display[i] for i in range(len(y_vals))], dtype=float)
    y_edges = centers_to_edges(y_plot_vals, log_scale=y_log)

    n_asm = len(assemblages)
    cmap = tab20_extended(n_asm)
    norm = BoundaryNorm(np.arange(-0.5, n_asm + 0.5, 1.0), cmap.N)

    fig, ax = plt.subplots(figsize=(12, 7), constrained_layout=True)
    mesh = ax.pcolormesh(t_edges, y_edges, asm_grid, cmap=cmap, norm=norm, shading="flat")

    if y_log:
        ax.set_yscale("log")

    ax.set_xlabel("Temperature [K]")
    ax.set_ylabel(y_label)
    ax.set_title("Phase Diagram (Stable Assemblages)")
    ax.grid(True, alpha=0.15)

    labels = [assemblage_label(a) for a in assemblages]
    handles = [Patch(facecolor=cmap(i), edgecolor="black", label=labels[i]) for i in range(n_asm)]
    if n_asm <= 24:
        ax.legend(handles=handles, loc="center left", bbox_to_anchor=(1.01, 0.5), frameon=True)
    else:
        cbar = fig.colorbar(mesh, ax=ax, pad=0.02)
        cbar.set_label("Assemblage index")

    fig.savefig(out_path, dpi=240)
    plt.close(fig)


def plot_dominant_gas_species(
    t_vals: np.ndarray,
    y_vals: np.ndarray,
    gas_grids: Dict[str, np.ndarray],
    y_label: str,
    y_log: bool,
    y_display: Dict[int, float],
    out_path: Path,
) -> None:
    if not gas_grids:
        return

    species_cols = sorted(gas_grids.keys())
    cube = np.stack([gas_grids[s] for s in species_cols], axis=0)

    valid = np.isfinite(cube)
    cube_safe = np.where(valid, cube, -np.inf)
    dom_idx = np.argmax(cube_safe, axis=0)
    any_valid = np.any(valid, axis=0)
    dom_idx = np.where(any_valid, dom_idx, -1)

    t_edges = centers_to_edges(t_vals, log_scale=False)
    y_plot_vals = np.array([y_display[i] for i in range(len(y_vals))], dtype=float)
    y_edges = centers_to_edges(y_plot_vals, log_scale=y_log)

    cmap = tab20_extended(len(species_cols))
    norm = BoundaryNorm(np.arange(-0.5, len(species_cols) + 0.5, 1.0), cmap.N)

    fig, ax = plt.subplots(figsize=(12, 7), constrained_layout=True)
    ax.pcolormesh(t_edges, y_edges, dom_idx, cmap=cmap, norm=norm, shading="flat")

    if y_log:
        ax.set_yscale("log")

    ax.set_xlabel("Temperature [K]")
    ax.set_ylabel(y_label)
    ax.set_title("Dominant GAS Species")
    ax.grid(True, alpha=0.15)

    labels = [gas_species_name(s) for s in species_cols]
    handles = [Patch(facecolor=cmap(i), edgecolor="black", label=labels[i]) for i in range(len(labels))]
    if len(labels) <= 24:
        ax.legend(handles=handles, loc="center left", bbox_to_anchor=(1.01, 0.5), frameon=True)

    fig.savefig(out_path, dpi=240)
    plt.close(fig)


def plot_top_gas_maps(
    t_vals: np.ndarray,
    y_vals: np.ndarray,
    gas_grids: Dict[str, np.ndarray],
    y_label: str,
    y_log: bool,
    y_display: Dict[int, float],
    out_path: Path,
    top_n: int,
) -> None:
    if not gas_grids:
        return

    species_ranked = sorted(
        gas_grids.keys(),
        key=lambda s: np.nanmax(gas_grids[s]) if np.isfinite(gas_grids[s]).any() else -np.inf,
        reverse=True,
    )
    species = [s for s in species_ranked if np.isfinite(gas_grids[s]).any()][: max(1, top_n)]

    n = len(species)
    ncols = min(3, n)
    nrows = int(math.ceil(n / ncols))

    t_edges = centers_to_edges(t_vals, log_scale=False)
    y_plot_vals = np.array([y_display[i] for i in range(len(y_vals))], dtype=float)
    y_edges = centers_to_edges(y_plot_vals, log_scale=y_log)

    fig, axes = plt.subplots(nrows, ncols, figsize=(4.8 * ncols, 3.8 * nrows), constrained_layout=True)
    axes_list = list(axes.flatten()) if hasattr(axes, "flatten") else [axes]

    for ax, s in zip(axes_list, species):
        z = gas_grids[s]
        z_plot = np.log10(np.clip(z, 1e-30, 1.0))
        m = ax.pcolormesh(t_edges, y_edges, z_plot, cmap="viridis", vmin=-30, vmax=0, shading="flat")

        if y_log:
            ax.set_yscale("log")

        ax.set_title(gas_species_name(s))
        ax.set_xlabel("T [K]")
        ax.set_ylabel(y_label)
        ax.grid(True, alpha=0.12)

    for ax in axes_list[n:]:
        ax.axis("off")

    cbar = fig.colorbar(m, ax=axes_list[:n], shrink=0.92)
    cbar.set_label("log10(Y(GAS,species))")
    fig.suptitle("Top GAS Species Abundance Maps", fontsize=13)

    fig.savefig(out_path, dpi=240)
    plt.close(fig)


def main() -> None:
    input_path = resolve_input_file(INPUT_FILE)
    points = parse_results_file(input_path)
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    y_axis = choose_y_axis(points)
    t_vals, y_vals, asm_grid, assemblages, gas_grids, y_label, y_log, y_display = build_grids(points, y_axis)

    axis_tag = "pt" if y_axis == "P" else "t_acr"
    out_phase = OUTPUT_DIR / f"phase_diagram_{axis_tag}.png"
    out_gas_dom = OUTPUT_DIR / f"dominant_gas_species_{axis_tag}.png"
    out_gas_maps = OUTPUT_DIR / f"gas_species_maps_{axis_tag}.png"

    plot_phase_diagram(t_vals, y_vals, asm_grid, assemblages, y_label, y_log, y_display, out_phase)
    plot_dominant_gas_species(t_vals, y_vals, gas_grids, y_label, y_log, y_display, out_gas_dom)
    plot_top_gas_maps(t_vals, y_vals, gas_grids, y_label, y_log, y_display, out_gas_maps, TOP_GAS_SPECIES)

    print(f"Input: {input_path}")
    print(f"Parsed points: {len(points)}")
    print(f"Y-axis mode: {y_axis}")
    print(f"Assemblages: {len(assemblages)}")
    print(f"GAS species columns: {len(gas_grids)}")
    print(f"Saved: {out_phase}")
    print(f"Saved: {out_gas_dom}")
    print(f"Saved: {out_gas_maps}")

    if SHOW_PLOTS:
        plt.show()


if __name__ == "__main__":
    main()
