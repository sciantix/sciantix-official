#!/usr/bin/env python3
import argparse
import csv
import math
import os
import re
import sys
import warnings
import shutil
import subprocess
from collections import defaultdict
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib")

import matplotlib.pyplot as plt
import numpy as np

TEST_DIR = Path(__file__).resolve().parent
RUN_LOG = "sciantix.log"
REPO_ROOT = TEST_DIR.parents[2]
BUILD_DIR = REPO_ROOT / "build"
BUILD_EXECUTABLE = BUILD_DIR / "sciantix.x"
ALLMAKE_OC_SCRIPT = REPO_ROOT / "Allmake_OC.sh"
OXIRED_SCRIPT = REPO_ROOT / "oxired_lib" / "examples" / "PHENIXpins.py"
OXIRED_HISTORY_DIR = OXIRED_SCRIPT.parent / "PHENIXpins_history"
CSRED_SCRIPT = REPO_ROOT / "csred_lib" / "examples" / "PHENIXpins.py"
CS_PRODUCTION_SCALING_FACTOR_INDEX = 4
RUN_SUMMARY = TEST_DIR / "run_summary.txt"
RESULTS_SUMMARY = TEST_DIR / "results_summary.txt"
MAIN_OUTPUT_NAME = "output.txt"
THERMO_OUTPUT_NAME = "thermochemistry_output.txt"
PHASE_SUBLATTICE_OUTPUT_NAME = "phase_sublattice_composition.txt"
THERMOCHEMISTRY_MANIFEST_FILE = TEST_DIR / "input_thermochemistry.txt"

# Number of top constituents (by amount) shown in parentheses next to each
# phase's legend label in the radial phase-fraction plot. Kept short so the
# legend fits inside the plot area without covering the point-1 markers.
PHASE_LABEL_TOP_CONSTITUENTS = 3
PLOTS_DIR = TEST_DIR.parents[4] / "OverLeaf/JOGSCIANTIX/Images/SCIANTIX"
GOLD_DIR = TEST_DIR / "gold"
EXP_DATA_DIR = TEST_DIR / "exp_data"
PELLET_RADIUS_M = 2.719e-3
AVOGADRO_NUMBER = 6.02214076e23
BURNUP_DATA_LABEL = "Burnup (MWd/kgUO2)"
BURNUP_LABEL = "Burnup (MWd/kgMOX)"
BURNUP_COLUMN_LABELS = (BURNUP_LABEL, BURNUP_DATA_LABEL)
BURNUP_UNIT = "MWd/kgMOX"
TIME_LABEL = "Time (h)"
TEMPERATURE_LABEL = "Temperature (K)"
FIMA_LABEL = "FIMA (%)"
COOLDOWN_START = 25200.0
COOLDOWN_END = 25224.0
COOLDOWN_SNAPSHOTS = (
    ("pre_cooldown", "pre-cooldown", COOLDOWN_START, "before"),
    ("post_cooldown", "post-cooldown", COOLDOWN_END, "after"),
)
JOG_OUTER_NODE_COUNT = 2
SNAPSHOT_BURNUP_COUNT = 7
TOURASSE_OUTER_DIAMETER_UM = 5430.0
HCP_A3_COMPARISON_ELEMENTS = ("MO", "PD", "RH", "RU", "TC")
METALLIC_INCLUSION_ELEMENTS = HCP_A3_COMPARISON_ELEMENTS
METALLIC_INCLUSION_ELEMENT_SET = set(METALLIC_INCLUSION_ELEMENTS)

SHARED_INPUT_FILES = (
    "input_settings.txt",
    "input_initial_conditions.txt",
    "input_thermochemistry.txt",
    "input_thermochemistry_settings.txt",
)
CASE_TEMPORARY_FILES = SHARED_INPUT_FILES + (
    "execution.txt",
    "overview.txt",
    "OCinput_matrix.OCM",
    "OCinput_grain_boundary.OCM",
    "OCoutput_matrix.DAT",
    "OCoutput_grain_boundary.DAT",
)

PAPER_PALETTE = [
    "#736F3F", "#BFAE56", "#B29DA6", "#D9AF32", "#A66226", "#733426",
    "#737675", "#9D6953", "#363726",  "#785C2D",
]

PAPER_PALETTE = [
    "#736F3F",  # olive
    "#BFAE56",  # muted mustard
    "#B29DA6",  # dusty mauve
    "#D9AF32",  # muted gold
    "#A66226",  # burnt orange
    "#733426",  # dark terracotta
    "#737675",  # warm grey
    "#9D6953",  # muted clay
    "#363726",  # dark olive
    "#785C2D",  # earthy brown

    "#789174",  # sage green
    "#657D8A",  # dusty blue
    "#6F918B",  # muted teal
    "#765D78",  # muted plum
    "#A9797F",  # dusty rose
    "#BD8460",  # soft copper
    "#8B7966",  # warm taupe
    "#668278",  # eucalyptus
    "#596B7B",  # slate blue
    "#91869B",  # lavender grey

    "#A88A4F",  # antique ochre
    "#59634C",  # moss green
    "#81959D",  # blue grey
    "#925548",  # muted rust
    "#B8A98C",  # warm sand
    "#575A57",  # charcoal grey
    "#B67768",  # muted coral
    "#70969A",  # desaturated cyan
    "#704D59",  # muted wine
    "#9A9B68",  # pale olive
]
plt.style.use("seaborn-v0_8-whitegrid")
plt.rcParams.update({
    "figure.figsize": (10, 7),
    "font.family": "serif",
    "font.serif": ["Times New Roman", "Times", "Nimbus Roman", "DejaVu Serif"],
    "mathtext.fontset": "dejavuserif",
    "font.size": 20,
    "axes.labelsize": 20,
    "axes.titlesize": 20,
    "xtick.labelsize": 20,
    "ytick.labelsize": 20,
    "legend.fontsize": 20,
    "figure.dpi": 300,
    "axes.grid": True,
    "grid.alpha": 0.5,
    "grid.linestyle": "--",
    "lines.linewidth": 3,
    "lines.markersize": 6,
    "legend.frameon": False,
})

COLORS = PAPER_PALETTE
PALETTE_COLORS = PAPER_PALETTE
METALLIC_ELEMENTS_EXCLUDED_FROM_FILTERED_PIE = {"RU", "PD", "RH", "TC"}

# Reuse the Cappia et al. composition data straight from its own plotting
# script, rather than embedding the saved PNG (which has a different
# rendering format from the vector pies drawn here).
sys.path.insert(0, str(TEST_DIR / "support"))
from FPs_plot import ATOMIC_PERCENT_COMPOSITION as CAPPIA_ATOMIC_PERCENT_COMPOSITION  # noqa: E402

# Fixed element -> color assignment shared by every JOG composition pie
# (Cappia reference, SCIANTIX, GERMINAL/Oulfarsi), so the same element always
# gets the same color regardless of which subset of elements a given pie
# happens to show. Keep this in sync with the copy in support/FPs_plot.py.
ELEMENT_COLORS = {
    "Cs": "#BFAE56",
    "O": "#A66226",
    "Mo": "#B29DA6",
    "Ba": "#D9AF32",
    "I": "#733426",
    "Rb": "#737675",
    "Zr": "#9D6953",
    "Fe": "#363726",
    "Co": "#785C2D",
    "Pd": "#736F3F",
    "Rh": "#4F6D5A",
    "Ru": "#5B7A99",
    "Tc": "#8A4F7D",
    "Te": "#C1666B",
}


def element_pie_color(element: str) -> str:
    return ELEMENT_COLORS.get(element, "#999999")


def ensure_executable(path: Path) -> None:
    if not path.exists():
        raise FileNotFoundError(f"SCIANTIX executable not found: {path}")
    if not path.is_file():
        raise FileNotFoundError(f"SCIANTIX executable path is not a file: {path}")


def ensure_output_file(path: Path) -> None:
    if not path.exists():
        raise FileNotFoundError(f"Required file not found: {path}")
    if not path.is_file():
        raise FileNotFoundError(f"Required path is not a file: {path}")


def load_output_data(output_file: Path) -> tuple[list[str], np.ndarray]:
    with output_file.open(newline="") as handle:
        reader = csv.reader(handle, delimiter="\t")
        rows = [[cell.strip() for cell in row if cell.strip()] for row in reader]

    if len(rows) < 2:
        raise ValueError(f"Not enough rows found in {output_file}")

    headers = rows[0]
    values = np.array(rows[1:], dtype=float)
    if values.ndim != 2 or values.shape[1] != len(headers):
        raise ValueError(f"Malformed SCIANTIX output in {output_file}")
    return headers, values


def relative_difference(diff: np.ndarray, reference: np.ndarray, abs_tol: float) -> np.ndarray:
    return diff / np.maximum(abs_tol, np.abs(reference))


def compare_tabular_outputs(
    old_path: Path,
    new_path: Path,
    abs_tol: float = 1e-8,
    rel_tol: float = 1e-6,
    top: int = 20,
) -> tuple[bool, list[str]]:
    old_header, old_data = load_output_data(old_path)
    new_header, new_data = load_output_data(new_path)

    old_cols = {name: index for index, name in enumerate(old_header)}
    new_cols = {name: index for index, name in enumerate(new_header)}

    removed = [name for name in old_header if name not in new_cols]
    added = [name for name in new_header if name not in old_cols]
    common = [name for name in old_header if name in new_cols]

    lines = [
        f"Comparing {old_path} -> {new_path}",
        f"Rows: {old_data.shape[0]} -> {new_data.shape[0]}",
        f"Columns: {len(old_header)} -> {len(new_header)}",
    ]

    if removed:
        lines.append("")
        lines.append("Columns only in gold output:")
        lines.extend(f"  - {name}" for name in removed)

    if added:
        lines.append("")
        lines.append("Columns only in new output:")
        lines.extend(f"  + {name}" for name in added)

    if old_data.shape[0] != new_data.shape[0]:
        lines.append("")
        lines.append("Cannot compare common columns: row counts differ.")
        return False, lines

    differences = []
    failing = []
    for name in common:
        old_values = old_data[:, old_cols[name]]
        new_values = new_data[:, new_cols[name]]
        diff = np.abs(new_values - old_values)
        rel = relative_difference(diff, old_values, abs_tol)
        finite = np.isfinite(diff) & np.isfinite(rel)

        if not np.any(finite):
            row = -1
            max_abs = np.nan
            max_rel = np.nan
        else:
            scored = np.where(finite, diff, -np.inf)
            row = int(np.argmax(scored))
            max_abs = float(diff[row])
            max_rel = float(rel[row])

        bad = (diff > abs_tol) & (rel > rel_tol)
        n_bad = int(np.count_nonzero(bad))
        if n_bad:
            failing.append(name)

        differences.append({
            "name": name,
            "row": row,
            "max_abs": max_abs,
            "max_rel": max_rel,
            "n_bad": n_bad,
            "old": float(old_values[row]) if row >= 0 else np.nan,
            "new": float(new_values[row]) if row >= 0 else np.nan,
        })

    differences.sort(
        key=lambda item: (
            item["n_bad"] > 0,
            np.nan_to_num(item["max_abs"], nan=-1.0),
        ),
        reverse=True,
    )

    lines.append("")
    lines.append(f"Common columns compared: {len(common)}")
    lines.append(f"Columns outside tolerance: {len(failing)}")
    lines.append("")
    lines.append(f"Top {min(top, len(differences))} column differences:")
    for item in differences[:top]:
        lines.append(
            f"  {item['name']}: row={item['row']}, "
            f"gold={item['old']:.8e}, new={item['new']:.8e}, "
            f"abs={item['max_abs']:.8e}, rel={item['max_rel']:.8e}, "
            f"bad_rows={item['n_bad']}"
        )

    return not removed and not added and not failing, lines


def column_map(headers: list[str]) -> dict[str, int]:
    return {name: index for index, name in enumerate(headers)}


def burnup_column_name(columns: dict[str, int] | dict[str, np.ndarray]) -> str:
    for label in BURNUP_COLUMN_LABELS:
        if label in columns:
            return label
    available = ", ".join(BURNUP_COLUMN_LABELS)
    raise KeyError(f"Missing burnup column. Expected one of: {available}")


def burnup_from_columns(columns: dict[str, int], values: np.ndarray) -> np.ndarray:
    return values[:, columns[burnup_column_name(columns)]]


def burnup_from_history(case_history: dict[str, np.ndarray]) -> np.ndarray:
    return case_history[burnup_column_name(case_history)]


def monotonic_interp_points(x_values: np.ndarray, y_values: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    x_array = np.asarray(x_values, dtype=float)
    y_array = np.asarray(y_values, dtype=float)
    finite = np.isfinite(x_array) & np.isfinite(y_array)
    x_array = x_array[finite]
    y_array = y_array[finite]
    if x_array.size == 0:
        return x_array, y_array

    order = np.argsort(x_array)
    x_array = x_array[order]
    y_array = y_array[order]
    unique_x, unique_indices = np.unique(x_array, return_index=True)
    return unique_x, y_array[unique_indices]


def add_burnup_secondary_axis(axis: plt.Axes, time: np.ndarray, burnup: np.ndarray) -> None:
    time_for_burnup, burnup_for_time = monotonic_interp_points(time, burnup)
    burnup_for_inverse, time_for_inverse = monotonic_interp_points(burnup, time)
    if (
        time_for_burnup.size < 2
        or burnup_for_inverse.size < 2
        or np.isclose(time_for_burnup[0], time_for_burnup[-1])
        or np.isclose(burnup_for_inverse[0], burnup_for_inverse[-1])
    ):
        return

    def time_to_burnup(x):
        return np.interp(x, time_for_burnup, burnup_for_time)

    def burnup_to_time(x):
        return np.interp(x, burnup_for_inverse, time_for_inverse)

    axis.secondary_xaxis("top", functions=(time_to_burnup, burnup_to_time)).set_xlabel(BURNUP_LABEL)


def configure_time_axis(axis: plt.Axes, time: np.ndarray, burnup: np.ndarray, *, secondary: bool = True) -> None:
    axis.set_xlabel(TIME_LABEL)
    if time.size:
        axis.set_xlim(float(np.nanmin(time)), float(np.nanmax(time)))
    if secondary:
        add_burnup_secondary_axis(axis, time, burnup)


def configure_burnup_axis(axis: plt.Axes, burnup: np.ndarray) -> None:
    axis.set_xlabel(BURNUP_LABEL)
    if burnup.size:
        axis.set_xlim(float(np.nanmin(burnup)), float(np.nanmax(burnup)))


def cooldown_mask(time: np.ndarray) -> np.ndarray:
    return (time >= COOLDOWN_START) & (time <= COOLDOWN_END)


def before_cooldown_mask(time: np.ndarray) -> np.ndarray:
    mask = time < COOLDOWN_START
    if np.count_nonzero(mask) >= 2:
        return mask
    return np.ones_like(time, dtype=bool)


def format_time_label(time_h: float) -> str:
    return f"{time_h:.0f} h" if np.isclose(time_h, round(time_h)) else f"{time_h:.2f} h"


def select_snapshot_value(values: np.ndarray, target_value: float, side: str) -> float:
    finite_values = np.unique(np.asarray(values, dtype=float)[np.isfinite(values)])
    if finite_values.size == 0:
        raise ValueError("Cannot select a snapshot from an empty value array.")

    tolerance = 1.0e-6
    if side == "before":
        eligible = finite_values[finite_values <= target_value + tolerance]
        if eligible.size:
            return float(eligible[-1])
    elif side == "after":
        eligible = finite_values[finite_values >= target_value - tolerance]
        if eligible.size:
            return float(eligible[0])
    else:
        raise ValueError(f"Unsupported snapshot side: {side}")

    return float(finite_values[int(np.argmin(np.abs(finite_values - target_value)))])


def snapshot_index(time: np.ndarray, target_time: float, side: str) -> int:
    selected_time = select_snapshot_value(time, target_time, side)
    return int(np.argmin(np.abs(time - selected_time)))


def phase_rows_at_snapshot(
    rows: list[dict[str, object]],
    target_time: float,
    side: str,
) -> tuple[float, list[dict[str, object]]]:
    if not rows:
        return target_time, []

    available_times = np.array([float(row["time"]) for row in rows], dtype=float)
    selected_time = select_snapshot_value(available_times, target_time, side)
    return selected_time, [
        row
        for row in rows
        if np.isclose(float(row["time"]), selected_time)
    ]


def radial_snapshot_entries(time: np.ndarray, burnup: np.ndarray) -> list[tuple[int, str]]:
    entries: list[tuple[int, str]] = []
    snapshot_targets = np.linspace(
        float(np.nanmin(burnup)),
        float(np.nanmax(burnup)),
        SNAPSHOT_BURNUP_COUNT,
    )

    for target in snapshot_targets:
        index = int(np.argmin(np.abs(burnup - target)))
        if time[index] >= COOLDOWN_START - 1.0e-6:
            continue
        if any(existing_index == index for existing_index, _ in entries):
            continue
        entries.append((index, f"{burnup[index]:.0f} {BURNUP_UNIT}"))

    for _, _, target_time, side in COOLDOWN_SNAPSHOTS:
        index = snapshot_index(time, target_time, side)
        entries = [
            entry
            for entry in entries
            if entry[0] != index
        ]
        entries.append((
            index,
            f"{burnup[index]:.0f} {BURNUP_UNIT}, {format_time_label(time[index])}",
        ))

    return entries


def grain_boundary_phase_styles(variables: list[str]) -> tuple[list[str], list[str]]:
    phase_hatch = {
        "gas": "...",
        "liquid": "///",
        "condensed": "",
        "unknown": "\\\\\\",
    }
    labels = []
    hatches = []
    for variable in variables:
        species = grain_boundary_species(variable)
        phase = grain_boundary_phase(variable)
        if phase == "gas":
            labels.append(f"{species} (g)")
        elif phase == "liquid":
            labels.append("Liquid")
        else:
            labels.append(f"{species}")
        hatches.append(phase_hatch.get(phase, phase_hatch["unknown"]))
    return labels, hatches


def save_figure(fig: plt.Figure, path: Path, saved_paths: list[Path]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with warnings.catch_warnings():
        warnings.filterwarnings(
            "ignore",
            message="The figure layout has changed to tight",
            category=UserWarning,
        )
        warnings.filterwarnings(
            "ignore",
            message="This figure includes Axes that are not compatible with tight_layout",
            category=UserWarning,
        )
        fig.tight_layout()
    fig.savefig(path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(path)


def load_experimental_jog_data(data_file: Path) -> tuple[np.ndarray, np.ndarray]:
    fima_values: list[float] = []
    thickness_values: list[float] = []

    with data_file.open() as handle:
        for raw_line in handle:
            line = raw_line.strip()
            if not line or line.startswith("#") or line.startswith("FIMA("):
                continue

            fima_str, thickness_str = [item.strip() for item in line.split(";")]
            fima_values.append(float(fima_str))
            thickness_values.append(float(thickness_str))

    return np.array(fima_values, dtype=float), np.array(thickness_values, dtype=float)


def load_samuelsson_simulation_jog_data(data_file: Path) -> dict[str, tuple[np.ndarray, np.ndarray]]:
    data: dict[str, tuple[list[float], list[float]]] = {}
    current_section: str | None = None

    with data_file.open() as handle:
        for raw_line in handle:
            line = raw_line.strip()
            if not line:
                continue

            if line.startswith("#"):
                comment = line.lstrip("#").strip().upper()
                if "GERMINAL" in comment:
                    current_section = "\nGERMINAL correlation"
                    data.setdefault(current_section, ([], []))
                elif "OC STAND-ALONE" in comment or "TAFID" in comment:
                    current_section = "\nOC stand-alone + TAF-ID"
                    data.setdefault(current_section, ([], []))
                continue

            if line.startswith("Burnup("):
                continue
            if current_section is None:
                continue

            fima_str, thickness_str = [item.strip() for item in line.split(";")]
            data[current_section][0].append(float(fima_str))
            data[current_section][1].append(float(thickness_str))

    return {
        section: (np.array(fima_values, dtype=float), np.array(thickness_values, dtype=float))
        for section, (fima_values, thickness_values) in data.items()
    }


def load_experimental_fmp_mo_ru_data(data_file: Path) -> dict[str, tuple[np.ndarray, np.ndarray]]:
    data: dict[str, tuple[list[float], list[float]]] = {
        "burnup": ([], []),
        "radial": ([], []),
    }
    current_section: str | None = None
    radial_coordinate = "R-to-Ro(-)"

    with data_file.open() as handle:
        for raw_line in handle:
            line = raw_line.strip()
            if not line or line.startswith("#"):
                continue

            if line.startswith("FIMA("):
                current_section = "burnup"
                continue
            if line.startswith("R-to-Ro("):
                current_section = "radial"
                radial_coordinate = "R-to-Ro(-)"
                continue
            if line.lower().startswith("um/outerrim"):
                current_section = "radial"
                radial_coordinate = "um/outerrim"
                continue
            if current_section is None:
                continue

            x_str, y_str = [item.strip() for item in line.split(";")]
            data[current_section][0].append(float(x_str))
            data[current_section][1].append(float(y_str))

    parsed_data = {
        section: (np.array(x_values, dtype=float), np.array(y_values, dtype=float))
        for section, (x_values, y_values) in data.items()
    }
    if radial_coordinate == "um/outerrim":
        outer_radius_um = 0.5 * TOURASSE_OUTER_DIAMETER_UM
        radial_distance_um, mo_ru_ratio = parsed_data["radial"]
        parsed_data["radial"] = (1.0 - radial_distance_um / outer_radius_um, mo_ru_ratio)
    return parsed_data


def load_matzke_oxygen_potential_data(data_file: Path) -> dict[str, np.ndarray]:
    temperatures: list[float] = []
    burnup_values: list[float] = []
    oxygen_potentials: list[float] = []
    radial_positions: list[str] = []
    germinal_temperatures: list[float] = []
    germinal_oxygen_potentials: list[float] = []
    in_germinal_section = False

    with data_file.open() as handle:
        for raw_line in handle:
            line = raw_line.strip()
            if not line:
                continue

            if line.startswith("#"):
                in_germinal_section = "GERMINAL" in line.upper()
                continue

            if in_germinal_section and ";" in line:
                parts = [item.strip() for item in line.split(";")]
                if len(parts) >= 2:
                    try:
                        germinal_temperatures.append(float(parts[0].replace(",", ".")))
                        germinal_oxygen_potentials.append(float(parts[1].replace(",", ".")))
                    except ValueError:
                        pass
                continue

            fields = line.split()
            if len(fields) < 5:
                continue

            try:
                temperatures.append(float(fields[0].replace(",", ".")))
                burnup_values.append(float(fields[3].replace(",", ".")))
                oxygen_potentials.append(float(fields[4].replace(",", ".")))
            except ValueError:
                continue

            radial_positions.append(fields[5].strip().lower() if len(fields) >= 6 else "unspecified")

    if not temperatures:
        raise ValueError(f"No oxygen potential points parsed from {data_file}")

    return {
        "temperature": np.array(temperatures, dtype=float),
        "burnup": np.array(burnup_values, dtype=float),
        "oxygen_potential": np.array(oxygen_potentials, dtype=float),
        "radial_position": np.array(radial_positions, dtype=object),
        "germinal_temperature": np.array(germinal_temperatures, dtype=float),
        "germinal_oxygen_potential": np.array(germinal_oxygen_potentials, dtype=float),
    }


def is_all_zero(series: np.ndarray, atol=1e-8) -> bool:
    return np.allclose(series, 0.0, atol=atol)


def delete_file_if_exists(path: Path) -> None:
    if path.exists() and path.is_file():
        path.unlink()


def save_gold_outputs(case_dir: Path) -> Path:
    gold_case_dir = GOLD_DIR / case_dir.name
    gold_case_dir.mkdir(parents=True, exist_ok=True)

    for filename in (MAIN_OUTPUT_NAME, THERMO_OUTPUT_NAME):
        source = case_dir / filename
        if source.exists():
            shutil.copy2(source, gold_case_dir / filename)

    return gold_case_dir


def compare_case_outputs_with_gold(case_dir: Path, gold_case_dir: Path) -> tuple[bool, Path]:
    report_lines: list[str] = []
    ok = True

    for filename in (MAIN_OUTPUT_NAME, THERMO_OUTPUT_NAME):
        old_path = gold_case_dir / filename
        new_path = case_dir / filename
        if not old_path.exists():
            report_lines.append(f"No gold {filename} available for {case_dir.name}.")
            report_lines.append("")
            continue
        if not new_path.exists():
            report_lines.append(f"No new {filename} produced for {case_dir.name}.")
            report_lines.append("")
            ok = False
            continue

        file_ok, lines = compare_tabular_outputs(old_path, new_path)
        ok = ok and file_ok
        report_lines.extend(lines)
        report_lines.append("")

    report_path = gold_case_dir / "comparison_report.txt"
    report_path.write_text("\n".join(report_lines))
    return ok, report_path


def cleanup_case_directory(case_dir: Path) -> None:
    for filename in CASE_TEMPORARY_FILES:
        delete_file_if_exists(case_dir / filename)





def radial_integral_over_radius(profile: np.ndarray, radii_m_array: np.ndarray) -> np.ndarray:
    if radii_m_array.size == 1:
        # With only one radial point, approximate the point as the center of a
        # finite outer/inner shell. This keeps single-point runs meaningful.
        shell_half_width = min(float(radii_m_array[0]), max(0.0, PELLET_RADIUS_M - float(radii_m_array[0])))
        shell_thickness_m = 2.0 * shell_half_width
        if shell_thickness_m <= 0.0:
            return np.zeros(profile.shape[1], dtype=float)
        return profile[0, :] * float(radii_m_array[0]) * shell_thickness_m / PELLET_RADIUS_M

    # Volume-conserving equivalent layer at the pellet outer surface:
    # V_JOG per unit length = 2*pi * integral(f*r dr); deposited as a thin
    # annulus at r = PELLET_RADIUS_M, its thickness is
    # t = V_JOG / (2*pi*r_fuel_outer) = integral(f*r dr) / r_fuel_outer.
    integral = np.trapezoid(profile * radii_m_array[:, np.newaxis], x=radii_m_array, axis=0)
    return integral / PELLET_RADIUS_M


def radial_volume_average(profile: np.ndarray, radii_m_array: np.ndarray) -> np.ndarray:
    if radii_m_array.size == 1:
        return profile[0, :].copy()

    r_min_m = radii_m_array[0]
    r_max_m = radii_m_array[-1]
    shell_edges = np.empty(len(radii_m_array) + 1, dtype=float)
    shell_edges[0] = r_min_m
    shell_edges[-1] = r_max_m
    shell_edges[1:-1] = 0.5 * (radii_m_array[:-1] + radii_m_array[1:])
    denominator = (r_max_m ** 2 - r_min_m ** 2)
    if denominator <= 0.0:
        return np.zeros(profile.shape[1], dtype=float)
    shell_weights = (shell_edges[1:] ** 2 - shell_edges[:-1] ** 2) / denominator
    return np.tensordot(shell_weights, profile, axes=(0, 0))


def radial_integral_masked_to_full_radius(
    profile: np.ndarray,
    radii_m_array: np.ndarray,
    radial_indices: list[int],
) -> np.ndarray:
    masked_profile = np.zeros_like(profile)
    masked_profile[radial_indices, :] = profile[radial_indices, :]
    return radial_integral_over_radius(masked_profile, radii_m_array)

def case_dirs() -> list[Path]:
    return sorted(path for path in TEST_DIR.glob("point_*") if path.is_dir())


def bootstrap_case_dirs_if_missing() -> list[Path]:
    case_directories = case_dirs()
    if case_directories:
        return case_directories

    print(f"No point_* directories found in {TEST_DIR}; bootstrapping cases from OXIRED histories.", flush=True)

    history_case_dirs = sorted(path for path in OXIRED_HISTORY_DIR.glob("point_*") if path.is_dir())
    if not history_case_dirs:
        print(f"No OXIRED history folders found in {OXIRED_HISTORY_DIR}; running {OXIRED_SCRIPT}.", flush=True)
        run_subprocess_or_raise(
            [sys.executable, str(OXIRED_SCRIPT)],
            cwd=OXIRED_SCRIPT.parent,
            step_name="OXIRED PHENIX history generation (bootstrap)",
        )
        history_case_dirs = sorted(path for path in OXIRED_HISTORY_DIR.glob("point_*") if path.is_dir())

    if not history_case_dirs:
        raise FileNotFoundError(f"No generated point_* histories found in {OXIRED_HISTORY_DIR}")

    scaling_template = TEST_DIR / "input_scaling_factors.txt"
    if not scaling_template.exists():
        raise FileNotFoundError(f"Missing template scaling factors file: {scaling_template}")

    for history_case_dir in history_case_dirs:
        history_file = history_case_dir / "input_history.txt"
        if not history_file.exists():
            raise FileNotFoundError(f"Missing generated OXIRED history file: {history_file}")

        case_dir = TEST_DIR / history_case_dir.name
        case_dir.mkdir(parents=True, exist_ok=True)
        shutil.copy2(history_file, case_dir / "input_history.txt")

        case_scaling_factors = case_dir / "input_scaling_factors.txt"
        if not case_scaling_factors.exists():
            shutil.copy2(scaling_template, case_scaling_factors)

    case_directories = case_dirs()
    if not case_directories:
        raise FileNotFoundError(f"No point_* directories found in {TEST_DIR} after bootstrap")

    print(f"Prepared {len(case_directories)} point_* case directories in {TEST_DIR}.", flush=True)
    return case_directories


def filter_case_dirs(case_directories: list[Path], number: int | None) -> list[Path]:
    if number is None:
        return case_directories

    number_tag = f"point_{number:02d}_"
    filtered = [case_dir for case_dir in case_directories if case_dir.name.startswith(number_tag)]
    if not filtered:
        raise FileNotFoundError(f"No case directory matching {number_tag} found in {TEST_DIR}")
    return filtered


def completed_case_dirs(case_directories: list[Path]) -> list[Path]:
    completed = [
        case_dir
        for case_dir in case_directories
        if (case_dir / MAIN_OUTPUT_NAME).exists() and (case_dir / THERMO_OUTPUT_NAME).exists()
    ]
    if completed:
        return completed

    raise FileNotFoundError(
        f"No completed point_* cases with {MAIN_OUTPUT_NAME} and {THERMO_OUTPUT_NAME} found in {TEST_DIR}"
    )


def parse_radius_mm(case_dir: Path) -> float:
    match = re.search(r"_r_(\d+p\d+)mm$", case_dir.name)
    if not match:
        raise ValueError(f"Could not parse radius from case directory name: {case_dir.name}")
    return float(match.group(1).replace("p", "."))


def build_species_color_map(labels: list[str], palette: list[str] = PAPER_PALETTE) -> dict[str, object]:
    species_names = sorted({label.split(" (", 1)[0] for label in labels})
    if not species_names:
        return {}

    return {
        species: palette[index % len(palette)]
        for index, species in enumerate(species_names)
    }


def build_label_color_map(labels: list[str], palette: list[str] = PAPER_PALETTE) -> dict[str, object]:
    unique_labels = sorted(set(labels))
    if not unique_labels:
        return {}

    species_color_map = build_species_color_map(unique_labels, palette=palette)
    return {
        label: species_color_map[label.split(" (", 1)[0]]
        for label in unique_labels
    }


def place_pie_wedge_labels(
    axis: plt.Axes,
    wedges: list,
    elements: list[str],
    values: list[float],
    value_labels: list[str],
    pie_radius: float,
    *,
    element_fontsize: float = 16,
    value_fontsize: float = 14,
    small_value_threshold: float = 2.0,
) -> None:
    """Label every wedge outside the pie with a leader line.

    ``elements``/``values``/``value_labels`` must be sorted value-descending
    (matching the order the wedges were drawn in), so wedges below
    ``small_value_threshold`` end up consecutive at the end of the sequence:
    they are all narrow and close together in angle, so instead of placing
    them purely by angle (which makes them overlap) they are stacked as an
    evenly spaced label column.
    """
    n_wedges = len(wedges)
    cluster_start = n_wedges
    for index in range(n_wedges - 1, -1, -1):
        if values[index] < small_value_threshold:
            cluster_start = index
        else:
            break

    thetas = [math.radians((wedge.theta1 + wedge.theta2) / 2.0) for wedge in wedges]
    cluster_size = n_wedges - cluster_start
    cluster_sign = 1.0
    cluster_center_y = 0.0
    line_spacing = 0.24
    if cluster_size > 0:
        cluster_sign = 1.0 if sum(math.cos(t) for t in thetas[cluster_start:]) >= 0.0 else -1.0
        cluster_center_y = sum(math.sin(t) for t in thetas[cluster_start:]) / cluster_size

    for index, (wedge, element, value, value_label, theta) in enumerate(
        zip(wedges, elements, values, value_labels, thetas)
    ):
        x, y = math.cos(theta), math.sin(theta)

        if index >= cluster_start:
            cluster_index = index - cluster_start
            text_x = pie_radius * 1.62 * cluster_sign
            text_y = (
                cluster_center_y
                + line_spacing * (cluster_size - 1) / 2.0
                - line_spacing * cluster_index
            )
        else:
            sign = 1.0 if x >= 0.0 else -1.0
            anchor_radius = pie_radius * (1.32 if index % 2 == 0 else 1.58)
            text_x = anchor_radius * sign
            text_y = anchor_radius * y

        ha = "left" if text_x >= 0.0 else "right"
        axis.annotate(
            "",
            xy=(pie_radius * x, pie_radius * y),
            xytext=(text_x, text_y),
            arrowprops=dict(
                arrowstyle="-",
                color="#8a8a83",
                lw=1.0,
                connectionstyle=f"angle,angleA=0,angleB={math.degrees(theta):.1f}",
            ),
            zorder=3,
        )
        axis.text(
            text_x, text_y + 0.065, element,
            ha=ha, va="center", fontsize=element_fontsize, fontweight="bold", color="#171717",
        )
        axis.text(
            text_x, text_y - 0.065, value_label,
            ha=ha, va="center", fontsize=value_fontsize, color="#171717",
        )


def assign_distinct_colors(labels: list[str], palette: list[str] = PAPER_PALETTE) -> dict[str, object]:
    return {
        label: palette[index % len(palette)]
        for index, label in enumerate(dict.fromkeys(labels))
    }


def evenly_spaced_colors(count: int, palette: list[str] = PAPER_PALETTE) -> list[object]:
    if count <= 0:
        return []
    return [palette[index % len(palette)] for index in range(count)]


def grain_boundary_phase(header: str) -> str:
    match = re.search(r"\(([^,]+), at grain boundary\)", header)
    return match.group(1).strip().lower() if match else "unknown"


def grain_boundary_species(header: str) -> str:
    return header.split(" (", 1)[0]


def is_grain_boundary_amount_column(header: str) -> bool:
    return (
        header not in {*BURNUP_COLUMN_LABELS, TIME_LABEL}
        and ", at grain boundary)" in header
    )


def is_jog_column(header: str) -> bool:
    return header.startswith("JOG") and header.endswith("(/)")


def is_jog_contribution_column(header: str) -> bool:
    return header.startswith("JOG from ") and header.endswith("(/)")


def jog_label(header: str) -> str:
    if header == "JOG (/)":
        return "Total"
    if is_jog_contribution_column(header):
        return header.removeprefix("JOG from ").removesuffix(" (/)")

    match = re.match(r"JOG \((.+)\) \(/\)$", header)
    if match:
        return match.group(1)
    return header.removeprefix("JOG ").removesuffix(" (/)")


def sorted_jog_columns(output_profiles: dict[str, np.ndarray]) -> list[str]:
    columns = [
        name
        for name, profile in output_profiles.items()
        if is_jog_column(name) and not is_all_zero(profile)
    ]
    if not columns:
        return []

    def sort_key(name: str) -> tuple[int, str]:
        if name == "JOG (/)":
            return (0, name)
        if name.startswith("JOG ("):
            return (1, name)
        return (2, name)

    return sorted(columns, key=sort_key)


def build_thermochemistry_color_map(case_directories: list[Path]) -> dict[str, object]:
    gb_labels: list[str] = []

    for case_dir in case_directories:
        thermo_file = case_dir / THERMO_OUTPUT_NAME
        ensure_output_file(thermo_file)
        thermochemistry_headers, values = load_output_data(thermo_file)
        thermochemistry_columns = column_map(thermochemistry_headers)

        gb_labels.extend(
            header
            for header in thermochemistry_headers
            if is_grain_boundary_amount_column(header)
            and not is_all_zero(values[:, thermochemistry_columns[header]], atol=1e-8)
        )

    return build_label_color_map(gb_labels, palette=PAPER_PALETTE)


def prepare_case_inputs(case_dir: Path) -> None:
    for filename in SHARED_INPUT_FILES:
        source = TEST_DIR / filename
        target = case_dir / filename
        if source.exists():
            shutil.copy2(source, target)


def run_sciantix_case(case_dir: Path) -> subprocess.CompletedProcess[bytes]:
    return subprocess.run(
        [str(BUILD_EXECUTABLE)],
        cwd=case_dir,
        capture_output=True,
        check=False,
    )


def decode_process_output(output: bytes) -> str:
    return output.decode("utf-8", errors="replace")


def run_subprocess_or_raise(command: list[str], cwd: Path, step_name: str) -> None:
    completed = subprocess.run(command, cwd=cwd, capture_output=True, check=False)
    print(decode_process_output(completed.stdout), end="")
    if completed.returncode != 0:
        raise RuntimeError(
            f"{step_name} failed (returncode={completed.returncode}):\n"
            + decode_process_output(completed.stderr)
        )


def build_sciantix() -> None:
    """Rebuild OpenCalphad/OC coupling and the SCIANTIX executable via Allmake_OC.sh."""
    print(f"Building SCIANTIX + OC ({ALLMAKE_OC_SCRIPT})...", flush=True)
    run_subprocess_or_raise(
        [str(ALLMAKE_OC_SCRIPT)],
        cwd=REPO_ROOT,
        step_name="Allmake_OC.sh",
    )


def generate_oxired_input_histories(case_directories: list[Path]) -> None:
    """Regenerate radial O/M histories with OXIRED and copy them into each case."""
    print(f"Running OXIRED PHENIX history generation ({OXIRED_SCRIPT})...", flush=True)
    run_subprocess_or_raise(
        [sys.executable, str(OXIRED_SCRIPT)],
        cwd=OXIRED_SCRIPT.parent,
        step_name="OXIRED PHENIX history generation",
    )

    history_case_dirs = sorted(path for path in OXIRED_HISTORY_DIR.glob("point_*") if path.is_dir())
    if not history_case_dirs:
        raise FileNotFoundError(f"OXIRED did not produce any point_* directories in {OXIRED_HISTORY_DIR}")

    history_by_name = {path.name: path for path in history_case_dirs}
    history_by_index: dict[int, Path] = {}
    for history_case_dir in history_case_dirs:
        match = re.match(r"point_(\d+)", history_case_dir.name)
        if match:
            history_by_index[int(match.group(1))] = history_case_dir

    for case_dir in case_directories:
        history_case_dir = history_by_name.get(case_dir.name)
        if history_case_dir is None:
            match = re.match(r"point_(\d+)", case_dir.name)
            if match:
                history_case_dir = history_by_index.get(int(match.group(1)))

        if history_case_dir is None:
            available = ", ".join(path.name for path in history_case_dirs)
            raise FileNotFoundError(
                "No OXIRED point_* history directory matches "
                f"{case_dir.name}. Available OXIRED directories: {available}"
            )

        source = history_case_dir / "input_history.txt"
        if not source.exists():
            raise FileNotFoundError(f"OXIRED did not produce {source}")
        shutil.copy2(source, case_dir / "input_history.txt")


def csred_radial_burnup_profile(
    average_burnup_at_percent: float,
    edges: np.ndarray,
    radius: np.ndarray,
    r_outer: float,
    rim_to_center_factor: float,
) -> np.ndarray:
    """Same normalized local-burnup shape used as the CSRED Cs-production proxy."""
    from csred import area_average as csred_area_average

    normalized_radius = radius / r_outer
    shape = 1.0 + (rim_to_center_factor - 1.0) * normalized_radius**2
    shape /= csred_area_average(edges, shape)
    return average_burnup_at_percent * shape


def solve_csred_cs_production_scaling_factors() -> tuple[np.ndarray, np.ndarray]:
    """Solve the radial Cs-production scaling factor with the CSRED model.

    Geometry, burnup, and time settings mirror csred_lib/examples/PHENIXpins.py
    and oxired_lib/examples/PHENIXpins.py so the radial mesh lines up with the
    OXIRED-generated point_* cases.
    """
    from csred import CsRedCylinder, CylinderGeometry, PolynomialProfile

    r_outer = PELLET_RADIUS_M
    burnup_final = 13.28
    max_time_hours = 25200
    n_radial_points = 4
    n_time_points = 10
    rim_to_center_burnup_factor = 1.0

    r_inner = 0.8e-3 # central hole (Inspyre deliverable 7.3)

    profile = PolynomialProfile(
        r_inner=r_inner,
        r_outer=r_outer,
        t_center=2200.0,
        t_surface=800.0,
        power=2.0,
    )
    solver = CsRedCylinder(
        geometry=CylinderGeometry(r_outer=r_outer, r_inner=r_inner),
        temperature_profile=profile,
        n_cells=n_radial_points,
    )
    edges, radius = solver.mesh()
    time_hours = np.linspace(0.0, max_time_hours, n_time_points)
    average_burnup = np.linspace(0.0, burnup_final, n_time_points)
    local_burnup = np.asarray([
        csred_radial_burnup_profile(bu, edges, radius, r_outer, rim_to_center_burnup_factor)
        for bu in average_burnup
    ])

    result = solver.solve_history(time_hours * 3600.0, local_burnup)
    return radius, result.scaling_factor


def update_scaling_factor_value(path: Path, index: int, value: float, comment: str | None = None) -> None:
    """Overwrite one value line (and optionally its comment) of an input_scaling_factors.txt file."""
    lines = path.read_text().splitlines()
    value_line_indices = [i for i, line in enumerate(lines) if not line.strip().startswith("#")]
    value_index = value_line_indices[index]
    lines[value_index] = f"{value:.6f}"
    if comment is not None and value_index + 1 < len(lines) and lines[value_index + 1].strip().startswith("#"):
        lines[value_index + 1] = f"# scaling factor - {comment}"
    path.write_text("\n".join(lines) + "\n")


def generate_csred_scaling_factors(case_directories: list[Path]) -> None:
    """Regenerate the CSRED plot, compute per-radius Cs-production scaling factors, and apply them."""
    print(f"Running CSRED PHENIX scaling-factor generation ({CSRED_SCRIPT})...", flush=True)
    run_subprocess_or_raise(
        [sys.executable, str(CSRED_SCRIPT)],
        cwd=CSRED_SCRIPT.parent,
        step_name="CSRED PHENIX scaling-factor generation",
    )
    print("Solving CSRED Cs-production scaling factors...", flush=True)
    radius, scaling_factor = solve_csred_cs_production_scaling_factors()
    for case_dir in case_directories:
        target_radius_m = parse_radius_mm(case_dir) * 1.0e-3
        nearest_index = int(np.argmin(np.abs(radius - target_radius_m)))
        scaling_factors_path = case_dir / "input_scaling_factors.txt"
        update_scaling_factor_value(
            scaling_factors_path,
            index=CS_PRODUCTION_SCALING_FACTOR_INDEX,
            value=float(scaling_factor[nearest_index]),
            comment="Cs production",
        )


def safe_plot_name(text: str) -> str:
    return (
        text.lower()
        .replace(" ", "_")
        .replace("+", "p")
        .replace("-", "m")
        .replace("/", "_")
        .replace(":", "_")
        .replace("#", "_")
    )


def phase_instance_from_row(row: dict[str, str]) -> str:
    return row.get("Phase instance", "").strip()


def display_phase_label(phase: str, phase_instance: str) -> str:
    if not phase_instance or phase_instance.lower() == phase.lower():
        return phase
    match = re.search(r"#(.+)$", phase_instance)
    if match:
        return f"{phase} #{match.group(1).strip()}"
    return f"{phase} ({phase_instance})"


def phase_instance_plot_suffix(phase: str, phase_instance: str) -> str:
    if not phase_instance or phase_instance.lower() == phase.lower():
        return ""
    match = re.search(r"#(.+)$", phase_instance)
    return match.group(1).strip() if match else phase_instance


# Two-letter symbols first so the greedy formula parser prefers "MO" over "M"+"O".
PRETTY_ELEMENTS = ["BA", "CS", "MO", "PD", "RH", "RU", "TC", "HE", "XE", "KR", "O"]

PRETTY_PHASE_NAMES = {
    "GAS": "Gas",
    "LIQUID": "Liquid",
    "HALITE": "BaO (halite)",
    "HCP_A3": "HCP",
    "HCP_ORD": "HCP (ord.)",
    "HCP_DIS": "HCP (dis.)",
    "FCC_A1": "FCC",
    "BCC_A2": "BCC",
    "PEROVSKITE": "Perovskite",
}


def pretty_formula(formula: str) -> str | None:
    """Render an uppercase chemical formula (e.g. CS2MOO4) with mathtext subscripts."""
    parts: list[tuple[str, str]] = []
    i = 0
    while i < len(formula):
        for element in PRETTY_ELEMENTS:
            if formula.startswith(element, i):
                i += len(element)
                digits = ""
                while i < len(formula) and formula[i].isdigit():
                    digits += formula[i]
                    i += 1
                parts.append((element.capitalize(), digits))
                break
        else:
            return None
    return "".join(f"{element}$_{{{digits}}}$" if digits else element for element, digits in parts)


def pretty_constituent(raw: str) -> str:
    """Human-readable constituent name: BA+2 -> Ba$^{2+}$, MOO4-2 -> MoO$_4^{2-}$, RU -> Ru."""
    token = raw.strip().upper()
    if token == "VA":
        return "Va"
    match = re.match(r"^([A-Z0-9]+?)([+-]\d*)?$", token)
    if not match:
        return raw
    formula = pretty_formula(match.group(1))
    if formula is None:
        return raw
    charge = match.group(2) or ""
    if charge:
        sign = charge[0]
        magnitude = charge[1:]
        formula += f"$^{{{magnitude}{sign}}}$" if magnitude else f"$^{sign}$"
    return formula


def pretty_phase_name(phase: str) -> str:
    """Human-readable phase name: CS2MOO4_S1 -> Cs$_2$MoO$_4$ (S1), LIQUID -> Liquid."""
    key = phase.strip().upper()
    if key in PRETTY_PHASE_NAMES:
        return PRETTY_PHASE_NAMES[key]
    match = re.match(r"^([A-Z0-9]+?)(?:_S(\d+))?$", key)
    if match:
        formula = pretty_formula(match.group(1))
        if formula is not None:
            return f"{formula} (S{match.group(2)})" if match.group(2) else formula
    return phase


def load_phase_sublattice_rows(path: Path) -> list[dict[str, object]]:
    ensure_output_file(path)
    grouped: dict[tuple[float, str, str, str, int, float, str], list[tuple[float, float, float]]] = defaultdict(list)

    with path.open(newline="") as handle:
        reader = csv.DictReader(handle, delimiter="\t")
        for row in reader:
            constituent = row["Constituent"].strip()
            phase_moles = float(row["Moles (mol/m3)"])
            phase_form_units = float(row.get("Form units (mol/m3)") or phase_moles)
            if constituent == "<empty>" or phase_moles <= 0.0:
                continue

            key = (
                float(row["Time (h)"]),
                row["Location"].strip(),
                row["Phase"].strip(),
                phase_instance_from_row(row),
                int(row["Sublattice"]),
                float(row["Sites"]),
                constituent,
            )
            grouped[key].append((phase_moles, phase_form_units, float(row["Site fraction"])))

    rows: list[dict[str, object]] = []
    for (time_h, location, phase, phase_instance, sublattice, sites, constituent), values in grouped.items():
        rows.append({
            "time": time_h,
            "location": location,
            "phase": phase,
            "phase_instance": phase_instance,
            "sublattice": sublattice,
            "sites": sites,
            "constituent": constituent,
            "phase_moles": float(np.mean([value[0] for value in values])),
            "phase_form_units": float(np.mean([value[1] for value in values])),
            "site_fraction": float(np.mean([value[2] for value in values])),
        })

    return rows


def load_phase_sublattice_inventory(path: Path) -> dict[float, dict[str, object]]:
    inventory: dict[float, dict[str, object]] = defaultdict(
        lambda: {
            "metallic_constituents": defaultdict(float),
            "mo_metallic": 0.0,
            "ru_metallic": 0.0,
            "mo_hcp": 0.0,
            "ru_hcp": 0.0,
        }
    )

    for row in load_phase_sublattice_rows(path):
        constituent = str(row["constituent"])
        phase_moles = float(row["phase_moles"])
        phase_form_units = float(row["phase_form_units"])
        site_fraction = float(row["site_fraction"])
        sites = float(row["sites"])
        if constituent == "<empty>" or phase_moles <= 0.0 or site_fraction <= 0.0:
            continue

        time_h = float(row["time"])
        location = str(row["location"])
        constituent_moles = phase_form_units * sites * site_fraction
        time_inventory = inventory[time_h]

        if location == "at grain boundary":
            metallic_counts = {
                element: count
                for element, count in constituent_element_counts(constituent).items()
                if element in METALLIC_INCLUSION_ELEMENT_SET
            }
            if not metallic_counts:
                continue

            normalized_constituent = constituent.upper()
            time_inventory["metallic_constituents"][normalized_constituent] += constituent_moles
            time_inventory["mo_metallic"] += constituent_moles * metallic_counts.get("MO", 0)
            time_inventory["ru_metallic"] += constituent_moles * metallic_counts.get("RU", 0)
            if str(row["phase"]).strip().upper() == "HCP_A3":
                time_inventory["mo_hcp"] += constituent_moles * metallic_counts.get("MO", 0)
                time_inventory["ru_hcp"] += constituent_moles * metallic_counts.get("RU", 0)

    return dict(inventory)


def normalize_constituent_element(constituent: str) -> str | None:
    element = re.sub(r"[+-].*$", "", constituent.strip().upper())
    if not element or element in {"VA", "<EMPTY>"}:
        return None
    return element


def constituent_element_counts(constituent: str) -> dict[str, int]:
    formula = normalize_constituent_element(constituent)
    if formula is None:
        return {}
    formula = formula.split("_", 1)[0]

    element_symbols = ("BA", "CS", "MO", "PD", "RH", "RU", "TC", "O")
    counts: dict[str, int] = defaultdict(int)
    index = 0
    while index < len(formula):
        symbol = next(
            (
                candidate
                for candidate in element_symbols
                if formula.startswith(candidate, index)
            ),
            None,
        )
        if symbol is None:
            return {}

        index += len(symbol)
        digits_start = index
        while index < len(formula) and formula[index].isdigit():
            index += 1
        count = int(formula[digits_start:index]) if index > digits_start else 1
        counts[symbol] += count

    return dict(counts)


def normalize_atomic_percent(elements: dict[str, float]) -> dict[str, float]:
    positive_elements = {
        element: value
        for element, value in elements.items()
        if value > 0.0
    }
    total = sum(positive_elements.values())
    if total <= 0.0:
        return {}
    return {
        element: 100.0 * value / total
        for element, value in positive_elements.items()
    }

def is_outer_node_jog_pie_phase(phase: str, phase_instance: str) -> bool:
    phase_name = phase.strip().upper()
    phase_instance_name = phase_instance.strip().upper()
    ba_molybdate_phases = {"BAMOO4", "BA2MOO5", "BA3MOO6"}
    return (
        phase_name.startswith("CS2MOO4")
        or phase_instance_name.startswith("CS2MOO4")
        or phase_name in ba_molybdate_phases
        or phase_instance_name in ba_molybdate_phases
        or phase_name == "LIQUID"
        or phase_instance_name.startswith("LIQUID")
        or phase_name == "FCC_A1"
        or phase_instance_name == "FCC_A1"
    )


def outer_node_jog_pie_element_moles(
    case_dir: Path,
    target_time: float,
    side: str,
) -> dict[str, float]:
    sublattice_file = case_dir / PHASE_SUBLATTICE_OUTPUT_NAME
    if not sublattice_file.exists():
        return {}

    element_moles: dict[str, float] = defaultdict(float)
    rows = [
        row
        for row in load_phase_sublattice_rows(sublattice_file)
        if row["location"] == "at grain boundary"
        and is_outer_node_jog_pie_phase(str(row["phase"]), str(row["phase_instance"]))
    ]
    if not rows:
        return {}

    _, snapshot_rows = phase_rows_at_snapshot(rows, target_time, side)
    included_elements = {"CS", "BA", "MO", "O"}
    for row in snapshot_rows:
        constituent_moles = (
            float(row["phase_form_units"])
            * float(row["sites"])
            * float(row["site_fraction"])
        )
        if constituent_moles <= 0.0:
            continue

        element_counts = constituent_element_counts(str(row["constituent"]))
        for element, count in element_counts.items():
            if element in included_elements:
                element_moles[element] += constituent_moles * count

    return dict(element_moles)


def mediated_outer_node_atomic_percent(
    case_directories: list[Path],
    target_time: float,
    side: str,
    *,
    excluded_elements: set[str] | None = None,
) -> dict[str, float]:
    node_atomic_percents: list[dict[str, float]] = []
    elements: set[str] = set()

    for case_dir in case_directories:
        node_moles = outer_node_jog_pie_element_moles(case_dir, target_time, side)
        if not node_moles:
            continue
        if excluded_elements:
            node_moles = {
                element: moles
                for element, moles in node_moles.items()
                if element.upper() not in excluded_elements
            }
        node_atomic_percent = normalize_atomic_percent(node_moles)
        if not node_atomic_percent:
            continue
        node_atomic_percents.append(node_atomic_percent)
        elements.update(node_atomic_percent)

    if not node_atomic_percents:
        return {}

    mediated_atomic_percent = {
        element: float(np.mean([
            node_atomic_percent.get(element, 0.0)
            for node_atomic_percent in node_atomic_percents
        ]))
        for element in elements
    }
    return normalize_atomic_percent(mediated_atomic_percent)


def add_atomic_percent_pie(
    axis: plt.Axes,
    composition: dict[str, float],
    color_map: dict[str, object],
    title: str,
) -> None:
    labels = sorted(composition, key=composition.get, reverse=True)
    if not labels:
        axis.axis("off")
        return

    values = [composition[label] for label in labels]
    colors = [color_map[label] for label in labels]
    pie_radius = 1.0
    wedges, _ = axis.pie(
        values,
        labels=["" for _ in labels],
        colors=colors,
        startangle=20,
        counterclock=False,
        wedgeprops={"alpha": 0.75},
        radius=pie_radius,
    )

    place_pie_wedge_labels(
        axis, wedges, labels, values, [f"{value:.1f}%" for value in values], pie_radius,
    )

    axis.set_xlim(-2.1 * pie_radius, 2.1 * pie_radius)
    axis.set_ylim(-1.75 * pie_radius, 1.75 * pie_radius)
    axis.set_title(title, fontsize=14, fontweight="bold", pad=10)
    axis.set_aspect("equal")


def load_oulfarsi_germinal_element_composition(data_file: Path) -> dict[str, float]:
    """Parse the elemental (not compound) at.% block of Oulfarsi2024_composition.txt.

    The file lists two comment-separated blocks under the same
    ``Element,Composition(at.%)`` header: elemental composition first, then a
    compound breakdown. Only the first (elemental) block is read here.
    """
    composition: dict[str, float] = {}
    started = False
    with data_file.open() as handle:
        for raw_line in handle:
            line = raw_line.strip()
            if not line:
                continue
            if line.startswith("#"):
                if started:
                    break
                continue
            if line.startswith("Element,"):
                started = True
                continue
            if not started:
                continue
            name, value_str = [item.strip() for item in line.split(",")]
            composition[name] = float(value_str)
    return composition


def plot_cappia_sciantix_germinal_comparison_pies(
    case_directories: list[Path],
    saved_paths: list[Path],
    *,
    target_time: float,
    side: str,
    snapshot_label: str,
    output_name: str,
) -> None:
    """Side-by-side JOG composition pies: Cappia et al. EPMA, SCIANTIX outer
    radial node, and the GERMINAL-based JOG-NESTOR3 estimate from Oulfarsi
    et al. (2024). The Cappia panel is drawn from its own composition data
    (support/FPs_plot.py), not a saved image, so all three pies share the
    same rendering."""
    cappia_composition = {
        element: value for element, value in CAPPIA_ATOMIC_PERCENT_COMPOSITION if value >= 1.0
    }
    if not cappia_composition:
        return

    germinal_composition = load_oulfarsi_germinal_element_composition(
        EXP_DATA_DIR / "Oulfarsi2024_composition.txt"
    )
    if not germinal_composition:
        return

    sciantix_nodes = (
        case_directories[-JOG_OUTER_NODE_COUNT:] if JOG_OUTER_NODE_COUNT > 0 else case_directories
    )
    sciantix_atomic_percent_raw = mediated_outer_node_atomic_percent(sciantix_nodes, target_time, side)
    if not sciantix_atomic_percent_raw:
        return
    sciantix_atomic_percent = {
        element.capitalize(): value for element, value in sciantix_atomic_percent_raw.items()
    }

    color_map = {
        element: element_pie_color(element)
        for element in set(sciantix_atomic_percent) | set(germinal_composition) | set(cappia_composition)
    }

    fig, axes = plt.subplots(1, 3, figsize=(15, 5.5))

    add_atomic_percent_pie(
        axes[0],
        cappia_composition,
        color_map,
        "EPMA, FFTF JOG\nCappia et al., 2020",
    )

    add_atomic_percent_pie(
        axes[1],
        sciantix_atomic_percent,
        color_map,
        "SCIANTIX, NESTOR-3 JOG\nThis work, post-cooldown",
    )
    add_atomic_percent_pie(
        axes[2],
        germinal_composition,
        color_map,
        "GERMINAL, NESTOR-3 JOG\nOulfarsi et al., 2024",
    )

    fig.subplots_adjust(wspace=0.15, left=0.02, right=0.98, top=0.82, bottom=0.05)
    save_figure(fig, PLOTS_DIR / output_name, saved_paths)


def phase_sublattice_site_fractions(rows: list[dict[str, object]]) -> list[list[tuple[str, float]]]:
    """Per-sublattice (constituent, site fraction) pairs for one phase instance's
    rows at a snapshot, sorted by descending site fraction, dropping vacancies/
    empties. Site fractions are already normalized within their own sublattice
    by OpenCalphad, so this directly gives the per-sublattice ionic/atomic
    composition quoted in the paper (e.g. Ru0.29Mo0.27Pd0.27Rh0.09Tc0.08).

    `phase_rows_at_snapshot`'s tolerance-based time match can pull in several
    near-duplicate rows for the same constituent (consecutive timesteps close
    to the snapshot time), so fractions are averaged per constituent rather
    than listed once per matching row."""
    by_sublattice: dict[int, dict[str, list[float]]] = defaultdict(lambda: defaultdict(list))
    for row in rows:
        constituent = str(row["constituent"])
        if constituent in ("<empty>", "VA"):
            continue
        site_fraction = float(row["site_fraction"])
        if site_fraction <= 1.0e-3:
            continue
        by_sublattice[int(row["sublattice"])][pretty_constituent(constituent)].append(site_fraction)

    return [
        sorted(
            ((name, float(np.mean(fractions))) for name, fractions in by_sublattice[sublattice_index].items()),
            key=lambda item: item[1],
            reverse=True,
        )
        for sublattice_index in sorted(by_sublattice)
    ]


def format_phase_sublattice_composition(rows: list[dict[str, object]]) -> str:
    """E.g. 'Ru0.29Mo0.27Pd0.27Rh0.09Tc0.08' for a single-sublattice metallic
    phase, or 'Ba1.00 | O0.70MoO4_0.29' (sublattices joined by ' | ') for an
    ionic liquid/compound with more than one sublattice."""
    sublattices = phase_sublattice_site_fractions(rows)
    if not sublattices:
        return ""
    return " | ".join(
        "".join(f"{name}{fraction:.2f}" for name, fraction in sublattice)
        for sublattice in sublattices
    )


def top_phase_constituents(rows: list[dict[str, object]], *, top_n: int = PHASE_LABEL_TOP_CONSTITUENTS) -> list[str]:
    """Names of the top-`top_n` constituents of a phase (by amount), for the legend label."""
    amounts: dict[str, float] = defaultdict(float)
    for row in rows:
        constituent = str(row["constituent"])
        if constituent in ("<empty>", "VA"):
            continue

        site_fraction = float(row["site_fraction"])
        if site_fraction <= 0.0:
            continue

        amounts[constituent] += float(row["phase_form_units"]) * float(row["sites"]) * site_fraction

    ranked = sorted(amounts.items(), key=lambda item: item[1], reverse=True)
    return [name for name, _ in ranked[:top_n]]


def phase_moles_at_snapshot(
    case_dir: Path,
    target_time: float,
    side: str,
) -> dict[str, float]:
    sublattice_file = case_dir / PHASE_SUBLATTICE_OUTPUT_NAME
    if not sublattice_file.exists():
        return {}

    rows = [
        row
        for row in load_phase_sublattice_rows(sublattice_file)
        if row["location"] == "at grain boundary"
    ]
    if not rows:
        return {}

    _, snapshot_rows = phase_rows_at_snapshot(rows, target_time, side)

    phase_keys = sorted({
        (str(row["phase"]), str(row["phase_instance"]))
        for row in snapshot_rows
    })

    phase_moles: dict[str, float] = {}
    for phase, phase_instance in phase_keys:
        instance_rows = [
            row for row in snapshot_rows
            if row["phase"] == phase and row["phase_instance"] == phase_instance
        ]
        moles = max((float(row["phase_moles"]) for row in instance_rows), default=0.0)
        if moles <= 0.0:
            continue

        base_label = pretty_phase_name(phase)
        top_constituents = [pretty_constituent(name) for name in top_phase_constituents(instance_rows)]
        phase_label = f"{base_label} ({', '.join(top_constituents)})" if top_constituents else base_label
        phase_moles[phase_label] = max(phase_moles.get(phase_label, 0.0), moles)

    return phase_moles


def phase_mole_fraction_profile_entries(
    case_directories: list[Path],
    target_time: float,
    side: str,
    *,
    min_fraction: float = 1.0e-4,
    max_entries: int = 12,
) -> list[tuple[str, np.ndarray]]:
    phase_moles_by_case = [
        phase_moles_at_snapshot(case_dir, target_time, side)
        for case_dir in case_directories
    ]
    if not any(phase_moles_by_case):
        return []

    phase_names = sorted({
        phase
        for phase_moles in phase_moles_by_case
        for phase in phase_moles
    })
    candidate_entries: list[tuple[str, np.ndarray, float]] = []
    for phase in phase_names:
        fractions = []
        for phase_moles in phase_moles_by_case:
            total_moles = sum(phase_moles.values())
            fractions.append(phase_moles.get(phase, 0.0) / total_moles if total_moles > 0.0 else 0.0)

        values = np.array(fractions, dtype=float)
        max_fraction = float(np.nanmax(values)) if values.size else 0.0
        if max_fraction >= min_fraction:
            candidate_entries.append((phase, values, max_fraction))

    candidate_entries.sort(key=lambda entry: entry[2], reverse=True)
    return [(phase, values) for phase, values, _ in candidate_entries[:max_entries]]


def build_phase_summary_lines(
    ordered_case_directories: list[Path],
    radii_mm_array: np.ndarray,
    target_time: float,
    side: str,
    snapshot_name: str,
    *,
    min_mol_percent: float = 0.5,
) -> list[str]:
    """Per-point grain-boundary phase mole fractions and compositions at one
    snapshot, in the form needed to write the Results section (e.g. 'HCP_A3
    (Ru0.29Mo0.27Pd0.27Rh0.09Tc0.08): 42.1 mol%')."""
    lines = [f"Grain-boundary phases, {snapshot_name}:"]
    any_point = False
    for point_index, case_dir in enumerate(ordered_case_directories, start=1):
        sublattice_file = case_dir / PHASE_SUBLATTICE_OUTPUT_NAME
        if not sublattice_file.exists():
            continue

        rows = [
            row for row in load_phase_sublattice_rows(sublattice_file)
            if row["location"] == "at grain boundary"
        ]
        if not rows:
            continue

        _, snapshot_rows = phase_rows_at_snapshot(rows, target_time, side)
        if not snapshot_rows:
            continue

        phase_keys = sorted({(str(row["phase"]), str(row["phase_instance"])) for row in snapshot_rows})
        phase_entries: list[tuple[str, str, float, list[dict[str, object]]]] = []
        for phase, phase_instance in phase_keys:
            instance_rows = [
                row for row in snapshot_rows
                if row["phase"] == phase and row["phase_instance"] == phase_instance
            ]
            moles = max((float(row["phase_moles"]) for row in instance_rows), default=0.0)
            if moles <= 0.0:
                continue
            phase_entries.append((phase, phase_instance, moles, instance_rows))

        total_moles = sum(entry[2] for entry in phase_entries)
        if total_moles <= 0.0:
            continue

        any_point = True
        lines.append(f"  Point {point_index} ({radii_mm_array[point_index - 1]:.2f} mm):")
        for phase, phase_instance, moles, instance_rows in sorted(
            phase_entries, key=lambda entry: entry[2], reverse=True
        ):
            mol_percent = 100.0 * moles / total_moles
            if mol_percent < min_mol_percent:
                continue
            label = display_phase_label(pretty_phase_name(phase), phase_instance)
            composition = format_phase_sublattice_composition(instance_rows)
            composition_suffix = f" ({composition})" if composition else ""
            lines.append(f"    {label}{composition_suffix}: {mol_percent:.1f} mol%")

    if not any_point:
        return []
    lines.append("")
    return lines


def build_jog_thickness_summary_lines(
    output_profiles: dict[str, np.ndarray],
    radii_m_array: np.ndarray,
    reference_burnup: np.ndarray,
    reference_time: np.ndarray,
    metallic_jog_columns: set[str],
) -> list[str]:
    """JOG thickness split by contribution at the end of irradiation
    (pre-cooldown) and after the final cooldown, plus each contribution's
    onset burnup (first burnup at which it exceeds 5% of its own end-of-life
    value). The pre-cooldown split mirrors the during-irradiation stack of
    JOG.png; the post-cooldown split is the state comparable to PIE."""
    jog_columns = sorted_jog_columns(output_profiles)
    oxide_columns = [column for column in jog_columns if column not in metallic_jog_columns and column != "JOG (/)"]
    if not oxide_columns:
        return []

    outer_node_count = min(JOG_OUTER_NODE_COUNT, len(radii_m_array))
    outer_indices = list(range(len(radii_m_array) - outer_node_count, len(radii_m_array)))

    pre_mask = before_cooldown_mask(reference_time)
    pre_index = int(np.max(np.nonzero(pre_mask)[0]))

    def column_groups() -> list[tuple[str, list[str]]]:
        metallic_present = [column for column in jog_columns if column in metallic_jog_columns]
        groups = [("oxide contributions only", oxide_columns)]
        if metallic_present:
            groups.append(("oxides + metallics", oxide_columns + metallic_present))
        return groups

    lines: list[str] = []
    for group_name, group_columns in column_groups():
        total_profile = np.sum(np.stack([output_profiles[column] for column in group_columns], axis=0), axis=0)
        total_series_um = radial_integral_masked_to_full_radius(total_profile, radii_m_array, outer_indices) * 1.0e6
        pre_total_um = float(total_series_um[pre_index])
        post_total_um = float(total_series_um[-1])

        lines.extend([
            f"JOG thickness ({group_name}, outer {outer_node_count} node(s)):",
            f"  End of irradiation, pre-cooldown ({reference_burnup[pre_index]:.1f} {BURNUP_UNIT}): "
            f"total = {pre_total_um:.1f} um",
            f"  Post-cooldown: total = {post_total_um:.1f} um",
        ])
        for column in group_columns:
            series_um = radial_integral_masked_to_full_radius(
                output_profiles[column], radii_m_array, outer_indices
            ) * 1.0e6
            pre_value_um = float(series_um[pre_index])
            post_value_um = float(series_um[-1])
            if is_all_zero(series_um) or max(pre_value_um, post_value_um) <= 0.0:
                continue

            onset_mask = series_um > 0.05 * max(pre_value_um, post_value_um)
            onset_burnup = float(reference_burnup[int(np.argmax(onset_mask))]) if np.any(onset_mask) else float("nan")
            pre_share = 100.0 * pre_value_um / pre_total_um if pre_total_um > 0.0 else 0.0
            post_share = 100.0 * post_value_um / post_total_um if post_total_um > 0.0 else 0.0
            lines.append(
                f"    {jog_label(column)}: pre-cooldown {pre_value_um:.1f} um ({pre_share:.0f}%), "
                f"post-cooldown {post_value_um:.1f} um ({post_share:.0f}%), "
                f"onset at burnup ~{onset_burnup:.1f} {BURNUP_UNIT}"
            )
        lines.append("")
    return lines


def build_outer_node_composition_summary_lines(ordered_case_directories: list[Path]) -> list[str]:
    """Mediated outer-node JOG atomic composition before/after the final
    cooldown, and the derived Cs/Mo ratio, matching the pie-chart data."""
    sciantix_nodes = (
        ordered_case_directories[-JOG_OUTER_NODE_COUNT:]
        if JOG_OUTER_NODE_COUNT > 0
        else ordered_case_directories
    )

    lines = ["JOG outer-node atomic composition (mediated over outer node(s)):"]
    any_snapshot = False
    for _, snapshot_name, target_time, side in COOLDOWN_SNAPSHOTS:
        composition = mediated_outer_node_atomic_percent(sciantix_nodes, target_time, side)
        if not composition:
            continue

        any_snapshot = True
        composition_text = ", ".join(
            f"{element.capitalize()} {value:.1f}"
            for element, value in sorted(composition.items(), key=lambda item: -item[1])
        )
        lines.append(f"  {snapshot_name}: {composition_text} at.%")

        cs = composition.get("CS", composition.get("Cs", 0.0))
        mo = composition.get("MO", composition.get("Mo", 0.0))
        if mo > 0.0:
            lines.append(f"    Cs/Mo = {cs / mo:.2f}")

    if not any_snapshot:
        return []
    lines.append("")
    return lines


RADIAL_POINT_REGION_COLORS = ["#2a78d6", "#1baf7a", "#eda100", "#008300"]

# Central hole radius (Inspyre deliverable 7.3) used by the CSRED/OXIRED Cs
# and O redistribution preprocessing (see solve_csred_cs_production_scaling_
# factors and oxired_lib/examples/PHENIXpins.py). The true radial domain of
# every SCIANTIX node is [PREPROCESSING_R_INNER_M, PELLET_RADIUS_M], not the
# fuel centerline — region shading and curve extrapolation here must use the
# same mesh, or the "shells" shown won't match what Cs/O were actually solved on.
PREPROCESSING_R_INNER_M = 0.8e-3


def preprocessing_radial_edges_over_ro(n_cells: int) -> np.ndarray:
    """The n_cells uniform mesh edges (R/Ro) used by the CSRED/OXIRED
    preprocessing: the true shell boundaries each SCIANTIX node represents."""
    return np.linspace(PREPROCESSING_R_INNER_M, PELLET_RADIUS_M, n_cells + 1) / PELLET_RADIUS_M


def add_radial_point_regions(axis: plt.Axes, n_cells: int, *, show_labels: bool = True) -> None:
    """Shade the pellet shell each SCIANTIX node represents, using the same
    mesh edges as the Cs/O redistribution preprocessing, and label it
    "Point N"."""
    edges = preprocessing_radial_edges_over_ro(n_cells)
    for i in range(n_cells):
        axis.axvspan(
            edges[i], edges[i + 1],
            color=RADIAL_POINT_REGION_COLORS[i % len(RADIAL_POINT_REGION_COLORS)],
            alpha=0.12, zorder=0, linewidth=0,
        )
        if show_labels:
            axis.text(
                0.5 * (edges[i] + edges[i + 1]), 0.97,
                f"Point {i + 1}", ha="center", va="top", fontsize=13, color="#3a3a3a",
                transform=axis.get_xaxis_transform(), zorder=1,
            )
    axis.set_xlim(edges[0], edges[-1])


def flat_extrapolate_to_full_radius(
    r_over_ro: np.ndarray, values: np.ndarray,
) -> tuple[np.ndarray, np.ndarray]:
    """Hold the outermost node values flat out to the true preprocessing mesh
    edges, so a curve through only the coarse SCIANTIX nodes still spans the
    full radial domain the Cs/O redistribution was actually solved on."""
    order = np.argsort(r_over_ro)
    sorted_r = r_over_ro[order]
    sorted_values = values[order]
    edges = preprocessing_radial_edges_over_ro(len(sorted_r))
    r_full = np.concatenate(([edges[0]], sorted_r, [edges[-1]]))
    values_full = np.concatenate(([sorted_values[0]], sorted_values, [sorted_values[-1]]))
    return r_full, values_full


def plot_radial_phase_mole_fraction_snapshot(
    case_directories: list[Path],
    radii_m_array: np.ndarray,
    output_profiles: dict[str, np.ndarray],
    reference_time: np.ndarray,
    saved_paths: list[Path],
    *,
    target_time: float,
    side: str,
    snapshot_label: str,
    output_name: str,
) -> None:
    entries = phase_mole_fraction_profile_entries(case_directories, target_time, side)
    if not entries or TEMPERATURE_LABEL not in output_profiles:
        return

    snapshot = snapshot_index(reference_time, target_time, side)
    r_over_ro = radii_m_array / PELLET_RADIUS_M
    temperature = output_profiles[TEMPERATURE_LABEL][:, snapshot]
    om_ratio = None
    if "O/M ratio (/)" in output_profiles:
        om_ratio = output_profiles["O/M ratio (/)"][:, snapshot]
    elif "Stoichiometry deviation (/)" in output_profiles:
        om_ratio = 2.0 + output_profiles["Stoichiometry deviation (/)"][:, snapshot]

    fig, axes = plt.subplots(
        2,
        1,
        figsize=(10, 7.5),
        sharex=True,
        gridspec_kw={"height_ratios": [1.15, 0.85], "hspace": 0.28},
    )
    phase_axis, state_axis = axes
    add_radial_point_regions(phase_axis, len(r_over_ro), show_labels=True)
    add_radial_point_regions(state_axis, len(r_over_ro), show_labels=False)

    marker_cycle = ["o", "s", "D", "^", "v", "P", "X", "<", ">"]
    color_map = build_label_color_map([phase for phase, _ in entries], palette=PAPER_PALETTE)
    for index, (phase, values) in enumerate(entries):
        plotted_values = np.where(values > 0.0, values, np.nan)
        phase_axis.plot(
            r_over_ro,
            plotted_values,
            color=color_map[phase],
            marker=marker_cycle[index % len(marker_cycle)],
            linestyle=(0, (2.0, 2.5)),
            linewidth=1.4,
            markersize=8.0,
            label=phase,
            zorder=2,
        )

    phase_axis.set_yscale("log")
    phase_axis.set_ylabel("Phase fraction")
    phase_axis.set_ylim(1.0e-3, 1.5)
    # Legend inside the empty left band (R/Ro < ~0.3, inward of point 1) so
    # the pre- and post-cooldown figures render at identical sizes.
    phase_axis.legend(
        loc="center left",
        bbox_to_anchor=(0.005, 0.5),
        ncol=1,
        fontsize=11.5,
        framealpha=0.85,
        frameon=True,
        edgecolor="none",
        facecolor="none",
        handlelength=1.4,
        handletextpad=0.5,
        borderaxespad=0.2,
        labelspacing=0.4,
    )
    phase_axis.set_title(snapshot_label)

    temperature_color = COLORS[3]
    om_color = COLORS[2]
    state_axis.plot(r_over_ro, temperature, color=temperature_color, linewidth=2.8, zorder=2)
    state_axis.set_ylabel("Temperature (K)", color=temperature_color)
    # Common, fixed temperature scale for the pre- and post-cooldown
    # snapshots, so the two figures are directly comparable (the
    # post-cooldown profile is flat at room temperature).
    state_axis.set_ylim(200.0, 2400.0)
    state_axis.set_yticks(np.arange(400.0, 2401.0, 400.0))
    # No horizontal grid in the temperature panel (vertical grid kept).
    state_axis.grid(False, axis="y")
    state_axis.tick_params(axis="y", labelcolor=temperature_color)
    state_axis.set_xlabel("R/Ro")
    state_axis.set_xlim(0.0, 1.0)

    if om_ratio is not None:
        om_axis = state_axis.twinx()
        om_axis.grid(False)
        om_axis.plot(r_over_ro, om_ratio, color=om_color, linewidth=2.8)
        om_axis.set_ylabel("O/M ratio (-)", color=om_color)
        om_axis.set_ylim(1.975, 2.01)
        om_axis.set_yticks(np.arange(1.975, 2.01, 0.01))
        om_axis.tick_params(axis="y", labelcolor=om_color)

    save_figure(fig, PLOTS_DIR / output_name, saved_paths)


def plot_phase_sublattice_composition(
    case_dir: Path,
    case_plot_dir: Path,
    burnup: np.ndarray,
    time: np.ndarray,
    saved_paths: list[Path],
) -> None:
    for stale_plot in case_plot_dir.glob("phase_sublattice_*.png"):
        stale_plot.unlink()

    sublattice_file = case_dir / PHASE_SUBLATTICE_OUTPUT_NAME
    if not sublattice_file.exists():
        print(f"No {sublattice_file.relative_to(TEST_DIR)} found; skipping phase sublattice plots.")
        return

    rows = load_phase_sublattice_rows(sublattice_file)
    rows = [
        row
        for row in rows
        if float(row["time"]) < COOLDOWN_START
    ]
    if not rows:
        print(f"No pre-cooldown phase sublattice composition rows found in {sublattice_file.relative_to(TEST_DIR)}.")
        return

    phase_keys = sorted({
        (str(row["location"]), str(row["phase"]), str(row["phase_instance"]))
        for row in rows
    })

    for location, phase, phase_instance in phase_keys:
        phase_rows = [
            row for row in rows
            if (
                row["location"] == location
                and row["phase"] == phase
                and row["phase_instance"] == phase_instance
            )
        ]
        if not phase_rows:
            continue

        phase_constituents = sorted({str(row["constituent"]) for row in phase_rows})
        constituent_color_map = build_label_color_map(phase_constituents, palette=PAPER_PALETTE)
        sublattices = sorted({int(row["sublattice"]) for row in phase_rows})
        fig, axes = plt.subplots(
            1,
            len(sublattices),
            figsize=(5.0 * len(sublattices), 5.0),
            sharey=True,
        )
        axes = np.atleast_1d(axes)

        for axis, sublattice in zip(axes, sublattices):
            sublattice_rows = [
                row for row in phase_rows
                if int(row["sublattice"]) == sublattice
            ]
            constituents = sorted({str(row["constituent"]) for row in sublattice_rows})
            for constituent in constituents:
                constituent_rows = sorted(
                    [
                        row for row in sublattice_rows
                        if row["constituent"] == constituent
                    ],
                    key=lambda row: float(row["time"]),
                )
                plot_time = np.array([float(row["time"]) for row in constituent_rows])
                plot_burnup = np.interp(plot_time, time, burnup)
                plot_fraction = np.array([float(row["site_fraction"]) for row in constituent_rows])
                if not is_all_zero(plot_fraction, atol=9e-3):
                    axis.plot(
                        plot_burnup,
                        plot_fraction,
                        color=constituent_color_map[constituent],
                        label=constituent,
                    )

            sites = sublattice_rows[0]["sites"]
            axis.set_title(f"Sublattice {sublattice}, sites = {sites:g}")
            axis.set_ylabel("Site fraction (-)")
            axis.set_xlabel(BURNUP_LABEL)
            axis.set_xlim(0.0, max(burnup))
            axis.set_xticks(np.arange(0.0, max(burnup) + 1.0, 20.0))
            axis.set_ylim(-0.05, 1.05)
            axis.legend(loc="best", ncol=2)
        phase_label = display_phase_label(phase, phase_instance)
        fig.suptitle(f"{phase_label}", y=0.95)
        plot_name_parts = [
            "phase_sublattice",
            safe_plot_name(location),
            safe_plot_name(phase),
        ]
        phase_suffix = phase_instance_plot_suffix(phase, phase_instance)
        if phase_suffix:
            plot_name_parts.append(safe_plot_name(phase_suffix))
        save_figure(
            fig,
            case_plot_dir / ("_".join(plot_name_parts) + ".png"),
            saved_paths,
        )


def plot_case(
    case_dir: Path,
    saved_paths: list[Path],
    gb_color_map: dict[str, object],
) -> None:
    output_file = case_dir / MAIN_OUTPUT_NAME
    thermo_file = case_dir / THERMO_OUTPUT_NAME
    ensure_output_file(output_file)
    ensure_output_file(thermo_file)

    headers, values = load_output_data(output_file)
    columns = column_map(headers)
    thermochemistry_headers, thermochemistry_values = load_output_data(thermo_file)
    thermochemistry_columns = column_map(thermochemistry_headers)

    burnup = burnup_from_columns(columns, values)
    time = values[:, columns[TIME_LABEL]]

    thermochemistry_time = thermochemistry_values[:, thermochemistry_columns[TIME_LABEL]]
    thermochemistry_burnup = np.interp(thermochemistry_time, time, burnup)
    thermochemistry_temperature = None
    if TEMPERATURE_LABEL in thermochemistry_columns:
        thermochemistry_temperature = thermochemistry_values[:, thermochemistry_columns[TEMPERATURE_LABEL]]
    elif TEMPERATURE_LABEL in columns:
        thermochemistry_temperature = np.interp(
            thermochemistry_time,
            time,
            values[:, columns[TEMPERATURE_LABEL]],
        )

    case_plot_dir = PLOTS_DIR / case_dir.name
    case_plot_dir.mkdir(parents=True, exist_ok=True)

    fig, axes = plt.subplots(1, 2, figsize=(10,5))
    axis = axes[0]
    if "O content (mol/m3)" in columns:
        axis.plot(burnup, values[:, columns["O content (mol/m3)"]], color=COLORS[0], label="Oxygen")
    axis.set_xlabel(BURNUP_LABEL)
    axis.set_xlim(0.0, max(burnup))
    axis.set_xticks(np.arange(0.0, max(burnup) + 1.0, 20.0))
    axis.set_ylim(0, 10e4)
    axis.set_ylabel("Concentration (mol m$^{-3}$)")
    axis.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))
    axis.legend(loc="upper left")

    axis = axes[1]
    if "U content (mol/m3)" in columns:
        axis.plot(burnup, values[:, columns["U content (mol/m3)"]], color=COLORS[1], label="Uranium")
    if "Pu content (mol/m3)" in columns:
        axis.plot(burnup, values[:, columns["Pu content (mol/m3)"]], color=COLORS[2], label="Plutonium")
    axis.set_xlabel(BURNUP_LABEL)
    axis.set_xlim(0.0, max(burnup))
    axis.set_xticks(np.arange(0.0, max(burnup) + 1.0, 20.0))
    axis.set_ylim(0, 10e4)
    axis.set_ylabel("Concentration (mol m$^{-3}$)")
    axis.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))
    axis.legend(loc="upper left")
    save_figure(fig, case_plot_dir / "inventory_matrix.png", saved_paths)

    fp_species = ["Xe", "Kr", "Cs", "Mo", "Ba", "Pd", "Tc", "Rh", "Ru"]
    fp_suffixes = [
        ("Produced", COLORS[0], " produced (at/m3)"),
        ("In grain", COLORS[1], " in grain (at/m3)"),
        ("At grain boundary", COLORS[2], " at grain boundary (at/m3)"),
        ("Reacted", COLORS[3], " reacted (at/m3)"),
        ("Released", COLORS[4], " released (at/m3)"),
        ("In solution", COLORS[5], " in solution (at/m3)"),
    ]
    fp_max = 0.0
    for species in fp_species:
        for _, _, suffix in fp_suffixes:
            column_name = f"{species}{suffix}"
            if column_name in columns:
                fp_max = max(fp_max, float(np.nanmax(values[:, columns[column_name]] / AVOGADRO_NUMBER)))

    fig, axes = plt.subplots(3, 3, figsize=(15, 15))
    axes = axes.flatten()
    for axis_index, (axis, species) in enumerate(zip(axes, fp_species)):
        for label, color, suffix in fp_suffixes:
            column_name = f"{species}{suffix}"
            if column_name in columns and is_all_zero(values[:, columns[column_name]]):
                continue
            if column_name in columns:
                axis.plot(
                    burnup,
                    values[:, columns[column_name]] / AVOGADRO_NUMBER,
                    color=color,
                    label=label,
                    linewidth=3,
                )
        axis.set_title(species)
        axis.set_xlabel(BURNUP_LABEL)
        axis.set_xlim(0.0, max(burnup))
        axis.set_xticks(np.arange(0.0, max(burnup) + 1.0, 20.0))
        axis.set_ylabel("Inventory (mol m$^{-3}$)")
        axis.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))
        if fp_max > 0.0:
            axis.set_ylim(0.0, fp_max * 1.05)
        if axis.get_legend_handles_labels()[0]:
            axis.legend(loc="upper left")
    save_figure(fig, case_plot_dir / "inventory_fission_products.png", saved_paths)

    final_ramp = cooldown_mask(time)
    temperature = values[:, columns[TEMPERATURE_LABEL]] if TEMPERATURE_LABEL in columns else np.full_like(time, np.nan)
    if np.count_nonzero(final_ramp) >= 2:
        fig, axes = plt.subplots(3, 3, figsize=(15, 15))
        axes = axes.flatten()
        for axis_index, (axis, species) in enumerate(zip(axes, fp_species)):
            species_max = 0.0
            for label, color, suffix in fp_suffixes:
                column_name = f"{species}{suffix}"
                if column_name in columns and is_all_zero(values[:, columns[column_name]]):
                    continue
                if column_name not in columns:
                    continue
                inventory = values[:, columns[column_name]] / AVOGADRO_NUMBER
                axis.plot(
                    temperature[final_ramp],
                    inventory[final_ramp],
                    color=color,
                    label=label,
                    linewidth=3,
                )
                species_max = max(species_max, float(np.nanmax(inventory[final_ramp])))
            axis.set_title(f"{species}, final ramp")
            axis.set_xlabel("Temperature (K)")
            axis.set_xlim(np.nanmax(temperature[final_ramp]), np.nanmin(temperature[final_ramp]))
            axis.set_xticks(np.arange(np.nanmin(temperature[final_ramp]), np.nanmax(temperature[final_ramp]) + 1.0, 250.0))
            axis.set_ylabel("Inventory (mol m$^{-3}$)")
            axis.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))
            if species_max > 0.0:
                axis.set_ylim(0.0, species_max * 1.05)
            if axis.get_legend_handles_labels()[0]:
                axis.legend(loc="upper left")
        save_figure(fig, case_plot_dir / "inventory_fission_products_final_ramp.png", saved_paths)

    oxide_fraction_columns = [
        ("Mo in oxide fraction (/)", "Mo in oxide", COLORS[0]),
        ("Ba in oxide fraction (/)", "Ba in oxide", COLORS[1]),
    ]
    oxide_valence_columns = [
        ("Mo oxide valence (/)", "Mo valence", COLORS[0]),
        ("Ba oxide valence (/)", "Ba valence", COLORS[1]),
    ]
    available_fraction_columns = [
        item for item in oxide_fraction_columns
        if item[0] in columns
    ]
    available_valence_columns = [
        item for item in oxide_valence_columns
        if item[0] in columns
    ]
    if available_fraction_columns or available_valence_columns:
        fig, axes = plt.subplots(1, 2, figsize=(10, 5))

        axis = axes[0]
        for column_name, label, color in available_fraction_columns:
            axis.plot(burnup, values[:, columns[column_name]], color=color, label=label)
        axis.set_ylabel("Fraction in oxide (-)")
        axis.set_xlabel(BURNUP_LABEL)
        axis.set_xticks(np.arange(0.0, max(burnup) + 1.0, 20.0))
        axis.set_ylim(-0.05, 1.05)
        if axis.get_legend_handles_labels()[0]:
            axis.legend(loc="best")

        axis = axes[1]
        for column_name, label, color in available_valence_columns:
            axis.plot(burnup, values[:, columns[column_name]], color=color, label=label)
        axis.set_ylabel("Mean oxide valence (-)")
        axis.set_xlabel(BURNUP_LABEL)
        axis.set_xticks(np.arange(0.0, max(burnup) + 1.0, 20.0))
        axis.set_ylim(-0.2, 6.2)
        if axis.get_legend_handles_labels()[0]:
            axis.legend(loc="best")

        save_figure(fig, case_plot_dir / "oxide_fraction_valence.png", saved_paths)

    total_mass = np.zeros_like(burnup)

    def add_atom_inventory(column_name: str, atomic_mass: float) -> None:
        if column_name in columns:
            total_mass[:] += values[:, columns[column_name]] * atomic_mass / AVOGADRO_NUMBER

    add_atom_inventory("Cs at grain boundary (at/m3)", 132.90545196)
    add_atom_inventory("Cs reacted (at/m3)", 132.90545196)
    add_atom_inventory("Mo produced (at/m3)", 95.95)
    add_atom_inventory("Ba produced (at/m3)", 137.327)
    add_atom_inventory("Pd produced (at/m3)", 106.42)
    add_atom_inventory("Tc produced (at/m3)", 98.906)
    add_atom_inventory("Rh produced (at/m3)", 102.91)
    add_atom_inventory("Ru produced (at/m3)", 101.07)
    if "O available content (mol/m3)" in columns:
        total_mass += values[:, columns["O available content (mol/m3)"]] * 15.999

    history_before_cooldown = before_cooldown_mask(time)
    thermochemistry_before_cooldown = before_cooldown_mask(thermochemistry_time)
    gb_amount_columns = [
        header
        for header in thermochemistry_headers
        if is_grain_boundary_amount_column(header)
    ]
    gb_variables = [
        header
        for header in gb_amount_columns
        if not is_all_zero(
            thermochemistry_values[thermochemistry_before_cooldown, thermochemistry_columns[header]],
            atol=total_mass[-1]*0.001,
        )
    ]
    gb_sorted_variables = sorted(
        gb_variables,
        key=lambda variable: float(np.nanmax(
            thermochemistry_values[thermochemistry_before_cooldown, thermochemistry_columns[variable]]
        )),
        reverse=True,
    )

    fig, axis = plt.subplots(1, 1, figsize=(8, 5))

    gb_stacked_data = [
        thermochemistry_values[thermochemistry_before_cooldown, thermochemistry_columns[variable]]
        for variable in gb_sorted_variables
    ]
    gb_colors = [gb_color_map[variable] for variable in gb_sorted_variables]
    if gb_stacked_data:
        gb_labels, gb_hatches = grain_boundary_phase_styles(gb_sorted_variables)

        polys = axis.stackplot(
            thermochemistry_burnup[thermochemistry_before_cooldown],
            gb_stacked_data,
            labels=gb_labels,
            colors=gb_colors,
        )
        for poly, hatch in zip(polys, gb_hatches):
            poly.set_hatch(hatch)
            poly.set_edgecolor((0.1, 0.1, 0.1, 0.7))
            poly.set_linewidth(0.1)

    axis.set_xlabel(BURNUP_LABEL)
    axis.set_xlim(0.0, max(burnup))
    axis.set_ylim(0, float(np.nanmax(total_mass[history_before_cooldown])) * 1.05)
    axis.set_ylabel("Mass per fuel volume (g m$^{-3}$)")
    axis.set_xticks(np.arange(0.0, max(burnup) + 1.0, 20.0))
    axis.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))

    handles, labels = axis.get_legend_handles_labels()
    axis.legend(handles, labels, loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
    save_figure(fig, case_plot_dir / "thermochemistry.png", saved_paths)

    if thermochemistry_temperature is not None:
        cooldown = cooldown_mask(thermochemistry_time)
        if gb_amount_columns and np.any(cooldown):
            cooldown_stack_total = np.sum(
                np.vstack([
                    thermochemistry_values[cooldown, thermochemistry_columns[header]]
                    for header in gb_amount_columns
                ]),
                axis=0,
            )
            cooldown_valid = np.zeros_like(cooldown, dtype=bool)
            cooldown_indices = np.where(cooldown)[0]
            cooldown_positive_threshold = float(total_mass[-1]) * 0.001
            cooldown_valid[cooldown_indices] = cooldown_stack_total > cooldown_positive_threshold
            cooldown = cooldown_valid

        cooldown_temperature = thermochemistry_temperature[cooldown]
        if np.count_nonzero(cooldown) >= 2 and np.unique(cooldown_temperature).size >= 2:
            cooldown_atol = float(total_mass[-1]) * 0.001
            cooldown_variables = [
                header
                for header in gb_amount_columns
                if not is_all_zero(
                    thermochemistry_values[cooldown, thermochemistry_columns[header]],
                    atol=cooldown_atol,
                )
            ]
            cooldown_variables = sorted(
                cooldown_variables,
                key=lambda variable: float(np.nanmax(
                    thermochemistry_values[cooldown, thermochemistry_columns[variable]]
                )),
                reverse=True,
            )
            if cooldown_variables:
                order = np.argsort(cooldown_temperature)
                cooldown_x = cooldown_temperature[order]
                cooldown_inventory = np.interp(
                    thermochemistry_time[cooldown],
                    time,
                    total_mass,
                )[order]
                cooldown_stacked_data = [
                    thermochemistry_values[cooldown, thermochemistry_columns[variable]][order]
                    for variable in cooldown_variables
                ]
                cooldown_labels, cooldown_hatches = grain_boundary_phase_styles(cooldown_variables)
                cooldown_colors = [gb_color_map[variable] for variable in cooldown_variables]

                fig, axis = plt.subplots(1, 1, figsize=(8, 5))
                polys = axis.stackplot(
                    cooldown_x,
                    cooldown_stacked_data,
                    labels=cooldown_labels,
                    colors=cooldown_colors,
                )
                for poly, hatch in zip(polys, cooldown_hatches):
                    poly.set_hatch(hatch)
                    poly.set_edgecolor((0.1, 0.1, 0.1, 0.7))
                    poly.set_linewidth(0.1)
                cooldown_total = np.sum(np.vstack(cooldown_stacked_data), axis=0)

                cooldown_ymax = max(
                    float(np.nanmax(cooldown_total)) if cooldown_total.size else 0.0,
                    float(np.nanmax(cooldown_inventory)) if cooldown_inventory.size else 0.0,
                )
                if cooldown_ymax > 0.0:
                    axis.set_ylim(0.0, cooldown_ymax * 1.05)
                axis.set_xlabel(TEMPERATURE_LABEL)
                axis.set_xlim(float(np.nanmax(cooldown_x)), float(np.nanmin(cooldown_x)))
                axis.set_ylabel("Mass per fuel volume (g m$^{-3}$)")
                axis.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))
                handles, labels = axis.get_legend_handles_labels()
                axis.legend(handles, labels, loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
                save_figure(fig, case_plot_dir / "thermochemistry_cooldown_temperature.png", saved_paths)

    if TIME_LABEL not in thermochemistry_columns:
        raise ValueError(f"Missing {TIME_LABEL} in {THERMO_OUTPUT_NAME}")

    plot_phase_sublattice_composition(case_dir, case_plot_dir, burnup, time, saved_paths)


def plot_radial_profiles(
    case_directories: list[Path],
    saved_paths: list[Path],
    gb_color_map: dict[str, object],
) -> list[str]:
    radii_mm: list[float] = []
    output_histories: list[dict[str, np.ndarray]] = []
    thermo_histories: list[dict[str, np.ndarray]] = []
    phase_inventory_histories: list[dict[float, dict[str, object]]] = []

    for case_dir in case_directories:
        headers, values = load_output_data(case_dir / MAIN_OUTPUT_NAME)
        thermo_headers, thermo_values = load_output_data(case_dir / THERMO_OUTPUT_NAME)
        radii_mm.append(parse_radius_mm(case_dir))
        output_histories.append({header: values[:, index] for index, header in enumerate(headers)})
        thermo_histories.append({header: thermo_values[:, index] for index, header in enumerate(thermo_headers)})
        sublattice_file = case_dir / PHASE_SUBLATTICE_OUTPUT_NAME
        if sublattice_file.exists():
            phase_inventory_histories.append(load_phase_sublattice_inventory(sublattice_file))
        else:
            phase_inventory_histories.append({})

    radii_mm_array = np.array(radii_mm, dtype=float)
    order = np.argsort(radii_mm_array)
    radii_mm_array = radii_mm_array[order]
    radii_m_array = radii_mm_array * 1.0e-3
    output_histories = [output_histories[index] for index in order]
    thermo_histories = [thermo_histories[index] for index in order]
    phase_inventory_histories = [phase_inventory_histories[index] for index in order]
    ordered_case_directories = [case_directories[index] for index in order]

    reference_time = output_histories[0]["Time (h)"]
    reference_burnup = burnup_from_history(output_histories[0])
    reference_fima = output_histories[0]["FIMA (%)"]

    for snapshot_slug, snapshot_name, target_time, side in COOLDOWN_SNAPSHOTS:
        fima_at_snapshot = float(np.interp(target_time, reference_time, reference_fima))
        snapshot_label = f"{snapshot_name} ({fima_at_snapshot:.1f} at.%)"
        plot_cappia_sciantix_germinal_comparison_pies(
            ordered_case_directories,
            saved_paths,
            target_time=target_time,
            side=side,
            snapshot_label=snapshot_label,
            output_name=f"cappia_sciantix_germinal_composition_pie_{snapshot_slug}.png",
        )

    output_names = sorted({name for case_history in output_histories for name in case_history})
    thermo_names = sorted({name for case_history in thermo_histories for name in case_history})

    output_profiles: dict[str, np.ndarray] = {}
    for name in output_names:
        aligned_series = []
        for case_history in output_histories:
            if name in case_history:
                aligned_series.append(np.interp(reference_time, case_history["Time (h)"], case_history[name]))
            else:
                aligned_series.append(np.zeros_like(reference_time))
        output_profiles[name] = np.vstack(aligned_series)

    thermo_profiles: dict[str, np.ndarray] = {}
    for name in thermo_names:
        aligned_series = []
        for case_history in thermo_histories:
            if name in case_history:
                aligned_series.append(np.interp(reference_time, case_history["Time (h)"], case_history[name]))
            else:
                aligned_series.append(np.zeros_like(reference_time))
        thermo_profiles[name] = np.vstack(aligned_series)

    mid_irradiation_burnup = 0.5 * float(reference_burnup[-1])
    mid_irradiation_time = float(np.interp(mid_irradiation_burnup, reference_burnup, reference_time))
    radial_phase_snapshots = list(COOLDOWN_SNAPSHOTS) + [
        ("mid_irradiation", "mid-irradiation", mid_irradiation_time, "before"),
    ]
    for snapshot_slug, snapshot_name, target_time, side in radial_phase_snapshots:
        fima_at_snapshot = float(np.interp(target_time, reference_time, reference_fima))
        snapshot_label = f"{snapshot_name} ({fima_at_snapshot:.1f} at.%)"
        plot_radial_phase_mole_fraction_snapshot(
            ordered_case_directories,
            radii_m_array,
            output_profiles,
            reference_time,
            saved_paths,
            target_time=target_time,
            side=side,
            snapshot_label=snapshot_label,
            output_name=f"radial_phase_mole_fraction_{snapshot_slug}.png",
        )

    def aligned_inventory_profile(inventory_key: str) -> np.ndarray:
        aligned_series = []
        for case_history in phase_inventory_histories:
            if not case_history:
                aligned_series.append(np.zeros_like(reference_time))
                continue

            inventory_times = np.array(sorted(case_history), dtype=float)
            values = np.array([
                float(case_history[time_h][inventory_key])
                for time_h in inventory_times
            ])
            aligned_series.append(np.interp(reference_time, inventory_times, values))
        return np.vstack(aligned_series)

    metallic_constituents = sorted({
        constituent
        for case_history in phase_inventory_histories
        for time_inventory in case_history.values()
        for constituent in time_inventory["metallic_constituents"]
    })
    metallic_profiles: dict[str, np.ndarray] = {}
    for constituent in metallic_constituents:
        aligned_series = []
        for case_history in phase_inventory_histories:
            if not case_history:
                aligned_series.append(np.zeros_like(reference_time))
                continue

            inventory_times = np.array(sorted(case_history), dtype=float)
            values = np.array([
                float(case_history[time_h]["metallic_constituents"].get(constituent, 0.0))
                for time_h in inventory_times
            ])
            aligned_series.append(np.interp(reference_time, inventory_times, values))
        metallic_profiles[constituent] = np.vstack(aligned_series)

    mo_metallic_profile = aligned_inventory_profile("mo_metallic")
    ru_metallic_profile = aligned_inventory_profile("ru_metallic")
    mo_hcp_profile = aligned_inventory_profile("mo_hcp")
    ru_hcp_profile = aligned_inventory_profile("ru_hcp")

    snapshot_entries = radial_snapshot_entries(reference_time, reference_burnup)
    snapshot_colors = evenly_spaced_colors(len(snapshot_entries))
    r_over_ro_array = radii_m_array / PELLET_RADIUS_M
    if "Mo produced (at/m3)" in output_profiles:
        tourasse_fmp_data = load_experimental_fmp_mo_ru_data(EXP_DATA_DIR / "Tourasse1992_FMP.txt")
        tourasse_radial_r_over_ro, tourasse_radial_mo_ru = tourasse_fmp_data["radial"]
        mo_metallic_over_ru_metallic = np.divide(
            mo_metallic_profile,
            ru_metallic_profile,
            out=np.zeros_like(mo_metallic_profile),
            where=ru_metallic_profile > 0.0,
        )

        if not is_all_zero(mo_metallic_over_ru_metallic):
            fig, axis = plt.subplots(1, 1, figsize=(9, 5))
            for color, (index, snapshot_label) in zip(snapshot_colors, snapshot_entries):
                if is_all_zero(mo_metallic_over_ru_metallic[:, index]):
                    continue
                if reference_burnup[index] < 1:
                    continue
                axis.plot(
                    r_over_ro_array,
                    mo_metallic_over_ru_metallic[:, index],
                    color=color,
                    marker="o",
                    label=snapshot_label,
                )

            axis.scatter(
                tourasse_radial_r_over_ro,
                tourasse_radial_mo_ru,
                edgecolors=COLORS[7],
                facecolors="none",
                marker="D",
                label="Tourasse et al. (1992)",
                zorder=3,
                linewidths=1.6,
            )
            axis.hlines(21.9/19.8, 0, 1, color="black", label="Theoretical yield ratio")

            axis.set_xlabel("R/Ro")
            axis.set_ylabel("Mo / Ru in metallic inclusions (-)")
            axis.set_ylim(0,1.25)
            axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
            save_figure(fig, PLOTS_DIR / "Mo_Ru_ratio_metallic_inclusions.png", saved_paths)

    def safe_ratio(numerator: np.ndarray, denominator: np.ndarray) -> np.ndarray:
        return np.divide(
            numerator,
            denominator,
            out=np.zeros_like(numerator),
            where=denominator > 0.0,
        )

    if "Mo produced (at/m3)" in output_profiles:
        columnar_index = 0
        jog_node_count = min(JOG_OUTER_NODE_COUNT, mo_hcp_profile.shape[0])

        if "Mo/Ru in HCP_A3 (/)" in output_profiles:
            # Read the SCIANTIX-computed ratio directly for the single
            # columnar node; the outer-node aggregate below still needs the
            # phase_sublattice-derived Mo/Ru moles because a ratio-of-ratios
            # cannot be summed correctly across multiple nodes.
            point1_hcp_mo_ru = output_profiles["Mo/Ru in HCP_A3 (/)"][columnar_index]
        else:
            point1_hcp_mo_ru = safe_ratio(mo_hcp_profile[columnar_index], ru_hcp_profile[columnar_index])
        point1_metallic_mo_ru = safe_ratio(
            mo_metallic_profile[columnar_index], ru_metallic_profile[columnar_index]
        )
        jog_hcp_mo_ru = safe_ratio(
            mo_hcp_profile[-jog_node_count:].sum(axis=0), ru_hcp_profile[-jog_node_count:].sum(axis=0)
        )
        jog_metallic_mo_ru = safe_ratio(
            mo_metallic_profile[-jog_node_count:].sum(axis=0), ru_metallic_profile[-jog_node_count:].sum(axis=0)
        )

        if not (
            is_all_zero(point1_hcp_mo_ru)
            and is_all_zero(point1_metallic_mo_ru)
            and is_all_zero(jog_hcp_mo_ru)
            and is_all_zero(jog_metallic_mo_ru)
        ):
            tourasse_fmp_data = load_experimental_fmp_mo_ru_data(EXP_DATA_DIR / "Tourasse1992_FMP.txt")
            tourasse_burnup_fima, tourasse_burnup_mo_ru = tourasse_fmp_data["burnup"]

            fig, (point1_axis, jog_axis) = plt.subplots(1, 2, figsize=(15, 5), sharey=True)

            point1_axis.plot(reference_fima, point1_hcp_mo_ru, color=COLORS[0], label="HCP")
            point1_axis.plot(reference_fima, point1_metallic_mo_ru, color=COLORS[1], label="Metallic inclusions (general)")
            point1_axis.scatter(
                tourasse_burnup_fima,
                tourasse_burnup_mo_ru,
                edgecolors=COLORS[7],
                facecolors="none",
                marker="D",
                label="Tourasse et al. (1992)",
                zorder=3,
                linewidths=1.6,
            )
            point1_axis.set_title("Columnar region (point 1)")
            point1_axis.set_xlabel(FIMA_LABEL)
            point1_axis.set_ylabel("Mo / Ru (-)")
            point1_axis.legend(loc="upper right")

            jog_axis.plot(reference_fima, jog_hcp_mo_ru, color=COLORS[0], label="HCP")
            jog_axis.plot(reference_fima, jog_metallic_mo_ru, color=COLORS[1], label="Metallic inclusions (general)")
            jog_axis.set_title(f"JOG (outer {jog_node_count} nodes)")
            jog_axis.set_xlabel(FIMA_LABEL)
            jog_axis.legend(loc="upper right")

            save_figure(fig, PLOTS_DIR / "Mo_Ru_ratio_vs_burnup.png", saved_paths)

    oxygen_potential_columns = [
        ("Fuel oxygen potential - Kato (KJ/mol)", "Kato's Equation (NEA/NSC/R(2024)1)"),
        ("Fuel oxygen potential - CALPHAD (KJ/mol)", "CALPHAD"),
        ("Fuel oxygen potential - Blackburn (KJ/mol)", "Blackburn"),
    ]
    available_oxygen_columns = [
        item for item in oxygen_potential_columns
        if item[0] in output_profiles and not is_all_zero(output_profiles[item[0]])
    ]
    if available_oxygen_columns:
        oxygen_column_name, oxygen_column_label = next(
            (item for item in available_oxygen_columns if "CALPHAD" in item[0]),
            available_oxygen_columns[0],
        )

        fig, axis = plt.subplots(1, 1, figsize=(9, 5.5))
        add_radial_point_regions(axis, len(r_over_ro_array))
        for color, (index, snapshot_label) in zip(snapshot_colors, snapshot_entries):
            values_at_nodes = output_profiles[oxygen_column_name][:, index]
            if is_all_zero(values_at_nodes):
                continue
            r_full, values_full = flat_extrapolate_to_full_radius(r_over_ro_array, values_at_nodes)
            axis.plot(r_full, values_full, color=color, linewidth=2.4, zorder=2)
            axis.scatter(
                r_over_ro_array, values_at_nodes,
                color=color, marker="o", s=45, zorder=3, label=snapshot_label,
            )

        axis.set_xlabel("R/Ro")
        axis.set_ylabel("Fuel oxygen potential (kJ/mol O$_2$)")
        axis.set_title(oxygen_column_label)
        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
        save_figure(fig, PLOTS_DIR / "OxygenPotentialRadial.png", saved_paths)

        cooldown = cooldown_mask(reference_time)
        if TEMPERATURE_LABEL in output_profiles and np.count_nonzero(cooldown) >= 2:
            fig, axis = plt.subplots(1, 1, figsize=(9.5, 6.0))

            matzke_data = load_matzke_oxygen_potential_data(EXP_DATA_DIR / "Matzke1988_muO.txt")
            exp_temperature = matzke_data["temperature"]
            exp_burnup = matzke_data["burnup"]
            exp_oxygen_potential = matzke_data["oxygen_potential"]
            exp_radial_position = matzke_data["radial_position"]
            germinal_temperature = matzke_data["germinal_temperature"]
            germinal_oxygen_potential = matzke_data["germinal_oxygen_potential"]

            burnup_color_map = {
                0.0: COLORS[8],
                3.8: COLORS[6],
                7.0: COLORS[7],
                11.2: COLORS[9],
            }
            position_marker_map = {
                "edge": "s",
                "centre": "o",
                "unspecified": "^",
            }

            unique_burnup_values = sorted(set(float(value) for value in exp_burnup))
            for exp_burnup_level in unique_burnup_values:
                burnup_mask = np.isclose(exp_burnup, exp_burnup_level, atol=1.0e-6)
                burnup_color = burnup_color_map.get(exp_burnup_level, COLORS[5])
                for position_name in ("edge", "centre", "unspecified"):
                    position_mask = exp_radial_position == position_name
                    combined_mask = burnup_mask & position_mask
                    if not np.any(combined_mask):
                        continue
                    if position_name != "edge":
                        continue

                    axis.scatter(
                        exp_temperature[combined_mask],
                        exp_oxygen_potential[combined_mask],
                        marker=position_marker_map[position_name],
                        facecolors=burnup_color,
                        edgecolors=burnup_color,
                        zorder=2,
                        label=f"Matzke {exp_burnup_level:.1f} at.%",
                    )

            if germinal_temperature.size and germinal_oxygen_potential.size:
                axis.plot(
                    germinal_temperature,
                    germinal_oxygen_potential,
                    color=COLORS[1],
                    linestyle="--",
                    zorder=3,
                    label="GERMINAL 13.3 at.%",
                )

            cooldown_temperature_profiles = output_profiles[TEMPERATURE_LABEL][:, cooldown]
            cooldown_oxygen_profiles = output_profiles[oxygen_column_name][:, cooldown]
            eol_fima = float(reference_fima[-1])

            for point_index, (temperature_profile, oxygen_profile) in enumerate(
                zip(cooldown_temperature_profiles, cooldown_oxygen_profiles),
                start=1,
            ):
                finite_mask = np.isfinite(temperature_profile) & np.isfinite(oxygen_profile)
                if np.count_nonzero(finite_mask) < 2:
                    continue
                if point_index != 3:
                    continue

                x = temperature_profile[finite_mask]
                y = oxygen_profile[finite_mask]
                order = np.argsort(x)
                axis.plot(
                    x[order],
                    y[order],
                    color=RADIAL_POINT_REGION_COLORS[(point_index - 1) % len(RADIAL_POINT_REGION_COLORS)],
                    zorder=3,
                    label=f"SCIANTIX {eol_fima:.1f} at.%",
                )
            
            axis.set_xlim(900,1500)
            axis.set_ylim(-600, -350)    

            axis.set_xlabel(TEMPERATURE_LABEL)
            axis.set_ylabel("Fuel oxygen potential (kJ/mol O$_2$)")
            axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
            save_figure(fig, PLOTS_DIR / "OxygenPotentialCooldownEOL.png", saved_paths)

    # Metallic ("white metal") precipitate phases carry their own volume
    # contribution to the JOG mixture but are optionally excluded from the
    # plot so the oxide-only picture (comparable to the historical
    # Melis/Tourasse/Samuelsson references, which do not report a metallic
    # contribution) can be shown alongside the fuller oxides+metallics view.
    METALLIC_JOG_COLUMNS = {
        "JOG (HCP) (/)", "JOG (FCC) (/)", "JOG (Sigma) (/)", "JOG (MoPd2) (/)",
        "JOG (liquid metallic) (/)",
    }

    def plot_jog_figure(filename: str, include_metallics: bool, cooldown: bool = False) -> None:
        jog_columns = sorted_jog_columns(output_profiles)
        if not include_metallics:
            jog_columns = [column for column in jog_columns if column not in METALLIC_JOG_COLUMNS]
        if not jog_columns:
            return

        jog_stack_columns = [
            column for column in jog_columns
            if column != "JOG (/)"
        ]
        if "JOG (/)" in jog_columns:
            jog_total_profile = output_profiles["JOG (/)"]
        else:
            jog_total_profile = np.sum(
                np.stack([output_profiles[column] for column in jog_stack_columns], axis=0),
                axis=0,
            )
        outer_node_count = min(JOG_OUTER_NODE_COUNT, len(radii_m_array))
        outer_indices = list(range(len(radii_m_array) - outer_node_count, len(radii_m_array)))
        jog_total_thickness_over_time_um = radial_integral_masked_to_full_radius(
            jog_total_profile,
            radii_m_array,
            outer_indices,
        ) * 1.0e6

        if cooldown:
            time_mask = cooldown_mask(reference_time)
            if np.count_nonzero(time_mask) < 2 or TEMPERATURE_LABEL not in output_profiles:
                return
            x_values = output_profiles[TEMPERATURE_LABEL][outer_indices[-1]]
        else:
            # During-irradiation figure: stop at the end of irradiation so
            # phases that only appear during the final cooldown (BaMoO4,
            # MoO2, ...) do not enter the stack or the legend.
            time_mask = before_cooldown_mask(reference_time)
            x_values = reference_burnup

        x_masked = x_values[time_mask]
        jog_total_series = jog_total_thickness_over_time_um[time_mask]

        # Only phases with a non-zero contribution within the plotted window
        # (burnup range, or cooldown window) are stacked and shown in the
        # legend; a phase absent throughout that window is dropped entirely.
        jog_entries: list[tuple[str, np.ndarray, object]] = []
        jog_colors = assign_distinct_colors([jog_label(column) for column in jog_stack_columns])
        for index, column_name in enumerate(jog_stack_columns):
            label = jog_label(column_name)
            series = radial_integral_masked_to_full_radius(
                output_profiles[column_name],
                radii_m_array,
                outer_indices,
            )[time_mask] * 1.0e6
            if is_all_zero(series):
                continue
            jog_entries.append((label, series, jog_colors.get(label, PAPER_PALETTE[index % len(PAPER_PALETTE)])))

        if not jog_entries:
            return

        jog_labels = [item[0] for item in jog_entries]
        jog_histories = [item[1] for item in jog_entries]
        jog_plot_colors = [item[2] for item in jog_entries]

        fig, axis = plt.subplots(1, 1, figsize=(10, 5))

        axis.stackplot(
            x_masked,
            *jog_histories,
            colors=jog_plot_colors,
            labels=jog_labels,
            alpha=0.9,
        )
        cumulative_histories = np.cumsum(np.vstack(jog_histories), axis=0)
        for boundary in cumulative_histories:
            axis.plot(x_masked, boundary, color="#111827", linewidth=0.25, alpha=0.40)
        axis.plot(
            x_masked,
            jog_total_series,
            color="#111827",
            linewidth=2.6,
            label="Total",
        )

        if not cooldown:
            melis_fima, melis_thickness = load_experimental_jog_data(EXP_DATA_DIR / "Melis1993.txt")
            tourasse_fima, tourasse_thickness = load_experimental_jog_data(EXP_DATA_DIR / "Tourasse1992_JOG.txt")
            samuelsson_simulation_data = load_samuelsson_simulation_jog_data(
                EXP_DATA_DIR / "Samuellson2020_simulation.txt"
            )

            def fima_to_burnup(fima_values: np.ndarray) -> np.ndarray:
                return np.interp(fima_values, reference_fima, reference_burnup)

            axis.scatter(
                fima_to_burnup(melis_fima),
                melis_thickness,
                edgecolors=COLORS[6], facecolors="none",
                marker="o",
                label="Melis et al. (1993)",
                zorder=3,
                linewidths=1.6,
            )
            axis.scatter(
                fima_to_burnup(tourasse_fima),
                tourasse_thickness,
                edgecolors=COLORS[7], facecolors="none",
                marker="D",
                label="Tourasse et al. (1992)",
                zorder=3,
                linewidths=1.6,
            )
            samuelsson_markers = {"\nGERMINAL correlation": "s", "\nOC stand-alone + TAF-ID": "^"}
            samuelsson_colors = {"\nGERMINAL correlation": COLORS[8], "\nOC stand-alone + TAF-ID": COLORS[9]}
            samuelsson_linestyles = {"\nGERMINAL correlation": "--", "\nOC stand-alone + TAF-ID": ":"}
            for section_label, (fima_values, thickness_values) in samuelsson_simulation_data.items():
                axis.plot(
                    fima_to_burnup(fima_values),
                    thickness_values,
                    color=samuelsson_colors.get(section_label, COLORS[10 % len(COLORS)]),
                    marker=samuelsson_markers.get(section_label, "x"),
                    markerfacecolor="none",
                    linestyle=samuelsson_linestyles.get(section_label, "--"),
                    label=f"Samuelsson et al. (2020), {section_label}",
                    zorder=3,
                    linewidth=1.6,
                )
            axis.set_xlabel(BURNUP_LABEL)
            axis.set_xlim(0, max(reference_burnup))
            axis.set_ylim(0, 100)
        else:
            axis.set_xlabel(TEMPERATURE_LABEL)
            axis.set_xlim(float(np.nanmax(x_masked)), float(np.nanmin(x_masked)))
            axis.set_ylim(0, max(float(np.nanmax(jog_total_series)) * 1.1, 1.0e-6))

        axis.set_ylabel("JOG thickness ($\\mu$m)")
        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
        save_figure(fig, PLOTS_DIR / filename, saved_paths)

    plot_jog_figure("JOG.png", include_metallics=False)
    plot_jog_figure("JOG_oxides_metallics.png", include_metallics=True)
    plot_jog_figure("JOG_cooldown.png", include_metallics=True, cooldown=True)

    # ------------------------------------------------------------------
    # Numeric results summary for the paper's Results section: phase mole
    # fractions/compositions at each snapshot, the JOG thickness
    # contributions (end-of-life split and onset burnup), and the outer-node
    # JOG atomic composition. Written to RESULTS_SUMMARY and printed by main().
    # ------------------------------------------------------------------
    results_summary_lines: list[str] = []
    for snapshot_slug, snapshot_name, target_time, side in radial_phase_snapshots:
        results_summary_lines.extend(
            build_phase_summary_lines(ordered_case_directories, radii_mm_array, target_time, side, snapshot_name)
        )
    results_summary_lines.extend(
        build_jog_thickness_summary_lines(
            output_profiles, radii_m_array, reference_burnup, reference_time, METALLIC_JOG_COLUMNS
        )
    )
    results_summary_lines.extend(build_outer_node_composition_summary_lines(ordered_case_directories))
    return results_summary_lines


def main() -> int:
    parser = argparse.ArgumentParser()
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument(
        "--run",
        action="store_true",
        help="Run the SCIANTIX cases before regenerating plots.",
    )
    mode.add_argument(
        "--plot-only",
        action="store_true",
        help="Regenerate plots from existing point outputs only.",
    )
    mode.add_argument(
        "--full-pipeline",
        action="store_true",
        help=(
            "Build OpenCalphad + SCIANTIX with Allmake_OC.sh, regenerate OXIRED "
            "input histories and CSRED scaling factors (and their plots) for "
            "every point_* case, then run the SCIANTIX cases and regenerate plots."
        ),
    )
    parser.add_argument(
        "--runnode",
        action="store_true",
        help="Restrict processing to one point_* case selected with --number.",
    )
    parser.add_argument(
        "--number",
        type=int,
        help="Point number to process when --runnode is set, for example 1 for point_01.",
    )
    parser.add_argument(
        "--all-nodes",
        action="store_true",
        help=(
            "Treat every radial node as JOG-feeding (instead of only the "
            "outermost ones) and save all plots into a separate *_allnodes folder."
        ),
    )
    args = parser.parse_args()

    if args.all_nodes:
        global PLOTS_DIR, JOG_OUTER_NODE_COUNT
        PLOTS_DIR = PLOTS_DIR.parent / (PLOTS_DIR.name + "_allnodes")
        JOG_OUTER_NODE_COUNT = 10**6

    case_directories = bootstrap_case_dirs_if_missing()

    if args.runnode:
        if args.number is None:
            raise ValueError("--runnode requires --number")
        case_directories = filter_case_dirs(case_directories, args.number)

    run_cases = args.run or args.full_pipeline
    if not run_cases:
        case_directories = completed_case_dirs(case_directories)

    if args.full_pipeline:
        build_sciantix()
        generate_oxired_input_histories(case_directories)
        generate_csred_scaling_factors(case_directories)

    PLOTS_DIR.mkdir(parents=True, exist_ok=True)
    saved_paths: list[Path] = []
    case_results: list[tuple[Path, int]] = []
    comparison_results: list[tuple[Path, bool, Path]] = []

    if run_cases:
        ensure_executable(BUILD_EXECUTABLE)
        delete_file_if_exists(RUN_SUMMARY)
        for case_dir in case_directories:
            print(f"Running {case_dir.name}...", flush=True)
            gold_case_dir = save_gold_outputs(case_dir)
            cleanup_case_directory(case_dir)
            prepare_case_inputs(case_dir)
            completed = run_sciantix_case(case_dir)
            RUN_LOG_case = case_dir / RUN_LOG
            RUN_LOG_case.write_text(
                decode_process_output(completed.stdout) + decode_process_output(completed.stderr),
                encoding="utf-8",
            )
            case_results.append((case_dir, completed.returncode))
            if completed.returncode != 0:
                cleanup_case_directory(case_dir)
                raise RuntimeError(f"SCIANTIX failed for {case_dir}")
            comparison_ok, comparison_report = compare_case_outputs_with_gold(case_dir, gold_case_dir)
            comparison_results.append((case_dir, comparison_ok, comparison_report))
            status = "OK" if comparison_ok else "DIFF"
            print(
                f"Compared {case_dir.name} with gold: {status} "
                f"({comparison_report.relative_to(TEST_DIR)})",
                flush=True,
            )

            # Plotting is performed only after all point_* cases have run,
            # so one global thermochemistry color map can be used everywhere.
            cleanup_case_directory(case_dir)

        summary_lines = ["Run summary", ""]
        for case_dir, returncode in case_results:
            summary_lines.append(f"{case_dir.name}: returncode={returncode}")
        summary_lines.append("")
        summary_lines.append("Gold comparison summary")
        for case_dir, comparison_ok, comparison_report in comparison_results:
            status = "OK" if comparison_ok else "DIFF"
            summary_lines.append(
                f"{case_dir.name}: {status}, report={comparison_report.relative_to(TEST_DIR)}"
            )
        RUN_SUMMARY.write_text("\n".join(summary_lines))
    else:
        for case_dir in case_directories:
            ensure_output_file(case_dir / MAIN_OUTPUT_NAME)
            ensure_output_file(case_dir / THERMO_OUTPUT_NAME)

    # Build the map once from all radial points. The same thermochemical
    # phase/species therefore receives the same color in every point_* plot.
    gb_color_map = build_thermochemistry_color_map(case_directories)

    for case_dir in case_directories:
        case_saved_paths: list[Path] = []
        plot_case(case_dir, case_saved_paths, gb_color_map)
        saved_paths.extend(case_saved_paths)
        print(
            f"Generated {len(case_saved_paths)} plots for {case_dir.name} "
            f"in {(PLOTS_DIR / case_dir.name).relative_to(PLOTS_DIR.parent)}",
            flush=True,
        )

    results_summary_lines = plot_radial_profiles(case_directories, saved_paths, gb_color_map)

    print(f"Generated {len(saved_paths)} plots in {PLOTS_DIR}")

    results_summary_text = "\n".join(results_summary_lines)
    RESULTS_SUMMARY.write_text(results_summary_text)
    print("")
    print(f"Results summary (also written to {RESULTS_SUMMARY}):")
    print("")
    print(results_summary_text)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
