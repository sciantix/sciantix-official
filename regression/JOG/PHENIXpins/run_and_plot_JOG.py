#!/usr/bin/env python3
import argparse
import csv
import math
import os
import re
import warnings
import shutil
import subprocess
from collections import defaultdict
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib")

import matplotlib.pyplot as plt
import numpy as np
from matplotlib import colors as mcolors
from matplotlib.lines import Line2D

TEST_DIR = Path(__file__).resolve().parent
RUN_LOG = "sciantix.log"
BUILD_EXECUTABLE = TEST_DIR.parents[2] / "build" / "sciantix.x"
RUN_SUMMARY = TEST_DIR / "run_summary.txt"
MAIN_OUTPUT_NAME = "output.txt"
THERMO_OUTPUT_NAME = "thermochemistry_output.txt"
PHASE_SUBLATTICE_OUTPUT_NAME = "phase_sublattice_composition.txt"
THERMOCHEMISTRY_MANIFEST_FILE = TEST_DIR / "input_thermochemistry.txt"
PLOTS_DIR = TEST_DIR / "plots"
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
COLUMNAR_GRAIN_R_OVER_RO_RANGE = (0.33, 0.63)  # NESTOR 3
JOG_OUTER_NODE_COUNT = 1
SNAPSHOT_BURNUP_COUNT = 7
TOURASSE_OUTER_DIAMETER_UM = 5430.0
HCP_A3_COMPARISON_ELEMENTS = ("MO", "PD", "RH", "RU", "TC")
NON_OXIDE_PHASE_NAMES = {"GAS", "FCC_A1", "HCP_A3", "MOPD2"}

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
        "condensed": "xx",
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


def load_experimental_hcp_a3_composition_data(data_file: Path) -> list[dict[str, object]]:
    entries: list[dict[str, object]] = []
    with data_file.open() as handle:
        reader = csv.DictReader(
            (
                line for line in handle
                if line.strip() and not line.lstrip().startswith("#")
            ),
            delimiter=";",
        )
        for row in reader:
            entries.append({
                "r_over_ro": float(row["R-to-Ro(-)"]),
                "temperature_k": float(row["Temperature(K)"]),
                "composition": {
                    "MO": float(row["Mo(at%)"]),
                    "PD": float(row["Pd(at%)"]),
                    "RH": float(row["Rh(at%)"]),
                    "RU": float(row["Ru(at%)"]),
                    "TC": float(row["Tc(at%)"]),
                },
            })
    return entries


def load_experimental_matzke_mu_o_data(data_file: Path) -> list[dict[str, object]]:
    entries: list[dict[str, object]] = []
    with data_file.open() as handle:
        for raw_line in handle:
            line = raw_line.strip()
            if not line or line.startswith("#") or line.startswith("Temperature ") or line.startswith("K %"):
                continue

            parts = line.split()
            if len(parts) < 5:
                continue

            location = parts[5] if len(parts) > 5 else "Average"
            entries.append({
                "temperature_k": float(parts[0].replace(",", ".")),
                "pu_m_percent": float(parts[1].replace(",", ".")),
                "o_m_ratio": float(parts[2].replace(",", ".")),
                "burnup_fima": float(parts[3].replace(",", ".")),
                "mu_o2_kj_mol": float(parts[4].replace(",", ".")),
                "location": location,
            })
    return entries


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
        return profile[0, :] * shell_thickness_m / 2.0

    r_inner_m = radii_m_array[0]
    r_outer_m = radii_m_array[-1]
    annulus_thickness_m = r_outer_m - r_inner_m
    annulus_area_factor = (r_outer_m ** 2 - r_inner_m ** 2)
    if annulus_thickness_m <= 0.0 or annulus_area_factor <= 0.0:
        return np.zeros(profile.shape[1], dtype=float)

    # Area-average on annulus: <f> = 2/(ro^2-ri^2) * integral(f*r dr)
    # Equivalent half-thickness scaling for hollow domain: <f> * (ro-ri)/2.
    integral = np.trapezoid(profile * radii_m_array[:, np.newaxis], x=radii_m_array, axis=0)
    return integral * annulus_thickness_m / annulus_area_factor


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


def readable_text_color(color: object) -> str:
    red, green, blue = mcolors.to_rgb(color)
    luminance = 0.2126 * red + 0.7152 * green + 0.0722 * blue
    return "#171717" if luminance > 0.45 else "#ffffff"


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
        thermochemistry_headers, _ = load_output_data(thermo_file)

        gb_labels.extend(
            header
            for header in thermochemistry_headers
            if is_grain_boundary_amount_column(header)
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
            "hcp_constituents": defaultdict(float),
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
        phase = str(row["phase"])
        constituent_moles = phase_form_units * sites * site_fraction
        time_inventory = inventory[time_h]

        if location == "at grain boundary" and phase == "HCP_A3":
            normalized_constituent = "MO" if constituent.upper() == "MO" else constituent.upper()
            time_inventory["hcp_constituents"][normalized_constituent] += constituent_moles
            if normalized_constituent == "MO":
                time_inventory["mo_hcp"] += constituent_moles
            if normalized_constituent == "RU":
                time_inventory["ru_hcp"] += constituent_moles

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


def is_oxide_phase(phase: str) -> bool:
    return phase.strip().upper() not in NON_OXIDE_PHASE_NAMES


def hcp_a3_element_atomic_percent(case_dir: Path, target_time: float, side: str) -> dict[str, float]:
    sublattice_file = case_dir / PHASE_SUBLATTICE_OUTPUT_NAME
    if not sublattice_file.exists():
        return {}

    element_moles: dict[str, float] = defaultdict(float)
    rows = [
        row
        for row in load_phase_sublattice_rows(sublattice_file)
        if row["location"] == "at grain boundary" and row["phase"] == "HCP_A3"
    ]
    if not rows:
        return {}

    _, snapshot_rows = phase_rows_at_snapshot(rows, target_time, side)
    for row in snapshot_rows:
        element = normalize_constituent_element(str(row["constituent"]))
        if element is None or element not in HCP_A3_COMPARISON_ELEMENTS:
            continue

        constituent_moles = (
            float(row["phase_form_units"])
            * float(row["sites"])
            * float(row["site_fraction"])
        )
        if constituent_moles > 0.0:
            element_moles[element] += constituent_moles

    return normalize_atomic_percent(element_moles)


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
    for row in snapshot_rows:
        constituent_moles = (
            float(row["phase_form_units"])
            * float(row["sites"])
            * float(row["site_fraction"])
        )
        if constituent_moles <= 0.0:
            continue

        phase_name = str(row["phase"]).strip().upper()
        phase_instance_name = str(row["phase_instance"]).strip().upper()
        if phase_name == "FCC_A1" or phase_instance_name == "FCC_A1":
            included_elements = {"PD"}
        else:
            included_elements = {"CS", "BA", "MO", "O"}

        for element, count in constituent_element_counts(str(row["constituent"])).items():
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


def plot_outer_node_atomic_percent_pie(
    case_source: Path | list[Path],
    saved_paths: list[Path],
    *,
    target_time: float,
    side: str,
    snapshot_label: str,
    nodes: int = JOG_OUTER_NODE_COUNT,
    excluded_elements: set[str] | None = None,
    output_name: str = "outer_radial_node_atomic_percent_pie.svg",
    title_suffix: str = "",
) -> None:
    if isinstance(case_source, Path):
        case_directories = [case_source]
    else:
        case_directories = list(case_source)[-nodes:] if nodes > 0 else list(case_source)

    if not case_directories:
        return

    def plot_atomic_percent_pie(axis: plt.Axes, atomic_percent: dict[str, float], title: str) -> None:
        labels = sorted(atomic_percent, key=atomic_percent.get, reverse=True)
        values = [atomic_percent[label] for label in labels]
        colors = PALETTE_COLORS[:len(labels)]

        outside_labels = ["" for _ in labels]
        wedges, _ = axis.pie(
            values,
            labels=outside_labels,
            colors=colors,
            startangle=20,
            counterclock=False,
            wedgeprops={"alpha": 0.7},
            textprops={"fontweight": "bold", "color": "#171717"},
        )

        external_labels = []
        for wedge, element, value in zip(wedges, labels, values):
            theta = math.radians((wedge.theta1 + wedge.theta2) / 2.0)
            if value <= 2.0:
                external_labels.append({"element": element, "x": math.cos(theta), "y": math.sin(theta)})
                continue

            radius = 0.75
            text_color = "#171717"
            axis.text(
                radius * math.cos(theta),
                radius * math.sin(theta) + 0.05,
                element,
                ha="center",
                va="center",
                fontsize=14,
                fontweight="bold",
                color=text_color,
            )
            axis.text(
                radius * math.cos(theta),
                radius * math.sin(theta) - 0.05,
                f"{value:.0f}%",
                ha="center",
                va="center",
                fontsize=14,
                color=text_color,
            )

        for label in external_labels:
            axis.text(
                1.08 * label["x"],
                1.08 * label["y"],
                label["element"],
                ha="center",
                va="center",
                fontsize=14,
                fontweight="bold",
                color="#171717",
            )

        axis.set_title(title, fontweight="bold", y=0.94, loc="center")
        axis.set_aspect("equal")

    atomic_percent = mediated_outer_node_atomic_percent(
        case_directories,
        target_time,
        side,
        excluded_elements=excluded_elements,
    )
    if not atomic_percent:
        return

    fig, axis = plt.subplots(figsize=(5, 5))
    if len(case_directories) == 1:
        title = f"Outer radial node at.%{title_suffix}\n{snapshot_label}"
    else:
        title = f"Mean outer {len(case_directories)} radial nodes at.%{title_suffix}\n{snapshot_label}"
    plot_atomic_percent_pie(axis, atomic_percent, title)
    save_figure(fig, PLOTS_DIR / output_name, saved_paths)


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

    for wedge, element, value in zip(wedges, labels, values):
        theta = math.radians((wedge.theta1 + wedge.theta2) / 2.0)
        if value >= 10.0:
            radius = 0.58 * pie_radius
            text_color = readable_text_color(wedge.get_facecolor())
            axis.text(
                radius * math.cos(theta),
                radius * math.sin(theta) + 0.04,
                element,
                ha="center",
                va="center",
                fontsize=12,
                fontweight="bold",
                color=text_color,
            )
            axis.text(
                radius * math.cos(theta),
                radius * math.sin(theta) - 0.06,
                f"{value:.1f}%",
                ha="center",
                va="center",
                fontsize=11,
                color=text_color,
            )
        else:
            radius = 1.22 * pie_radius
            axis.annotate(
                element,
                xy=(pie_radius * math.cos(theta), pie_radius * math.sin(theta)),
                xytext=(radius * math.cos(theta), radius * math.sin(theta)),
                ha="center",
                va="center",
                fontsize=11,
                fontweight="bold",
                color="#171717",
                arrowprops={"arrowstyle": "-", "lw": 0.7, "color": "#444444"},
            )
            axis.text(
                radius * math.cos(theta),
                radius * math.sin(theta) - 0.12,
                f"{value:.1f}%",
                ha="center",
                va="center",
                fontsize=10,
                color="#171717",
            )

    axis.set_xlim(-1.35 * pie_radius, 1.35 * pie_radius)
    axis.set_ylim(-1.35 * pie_radius, 1.35 * pie_radius)
    axis.set_title(title, fontsize=14, fontweight="bold", pad=10)
    axis.set_aspect("equal")


def plot_hcp_a3_comparison_pies(
    case_directories: list[Path],
    radii_m_array: np.ndarray,
    saved_paths: list[Path],
    *,
    target_time: float,
    side: str,
    snapshot_label: str,
    output_name: str,
) -> None:
    experimental_entries = load_experimental_hcp_a3_composition_data(
        EXP_DATA_DIR / "Samuelsson2020_HCP_A3.txt"
    )
    if not experimental_entries:
        return

    r_over_ro_array = radii_m_array / PELLET_RADIUS_M
    case_compositions: list[dict[str, float]] = []
    for case_dir in case_directories:
        composition = hcp_a3_element_atomic_percent(case_dir, target_time, side)
        if not composition:
            return
        case_compositions.append(composition)

    color_map = build_label_color_map(list(HCP_A3_COMPARISON_ELEMENTS))
    calc_entries = [
        {
            "r_over_ro": float(r_over_ro),
            "composition": composition,
        }
        for r_over_ro, composition in zip(r_over_ro_array, case_compositions)
    ]
    calc_indexes_to_keep: set[int] = set()
    for entry in experimental_entries:
        experiment_r_over_ro = float(entry["r_over_ro"])
        upper_index = int(np.searchsorted(r_over_ro_array, experiment_r_over_ro, side="left"))
        lower_index = upper_index - 1
        if 0 <= lower_index < len(r_over_ro_array):
            calc_indexes_to_keep.add(lower_index)
        if 0 <= upper_index < len(r_over_ro_array):
            calc_indexes_to_keep.add(upper_index)
    calc_entries = [
        entry
        for index, entry in enumerate(calc_entries)
        if index in calc_indexes_to_keep
    ]
    if not calc_entries:
        return

    experimental_positions = sorted(float(entry["r_over_ro"]) for entry in experimental_entries)
    n_columns = len(experimental_positions)
    fig, axes = plt.subplots(2, n_columns, figsize=(3.1 * n_columns,10))
    axes = np.atleast_2d(axes)

    for column, position in enumerate(experimental_positions):
        exp_entry = min(
            experimental_entries,
            key=lambda entry: abs(float(entry["r_over_ro"]) - position),
        )
        calc_entry = min(
            calc_entries,
            key=lambda entry: abs(float(entry["r_over_ro"]) - position),
        )

        add_atomic_percent_pie(
            axes[0, column],
            dict(calc_entry["composition"]),
            color_map,
            f"SCIANTIX\nR/Ro = {float(calc_entry['r_over_ro']):.2f}",
        )
        add_atomic_percent_pie(
            axes[1, column],
            dict(exp_entry["composition"]),
            color_map,
            f"Experiment\nR/Ro = {float(exp_entry['r_over_ro']):.2f}",
        )

    fig.suptitle(f"White phase (HCP) composition, {snapshot_label}", y=0.95, fontweight="bold")
    fig.subplots_adjust(wspace=0.02, hspace=0.02, left=0.03, right=0.99, top=0.90, bottom=0.06)
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
    axis.set_ylim(0, 9e4)
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
    axis.set_ylim(0, 9e4)
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
            axis.set_xlim(np.nanmin(temperature[final_ramp]), np.nanmax(temperature[final_ramp]))
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
                axis.set_xlim(float(np.nanmin(cooldown_x)), float(np.nanmax(cooldown_x)))
                axis.set_ylabel("Mass per fuel volume (g m$^{-3}$)")
                axis.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))
                axis.set_yticks(np.linspace(0.0, cooldown_ymax, num=6))
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
) -> None:
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
    reference_before_cooldown = before_cooldown_mask(reference_time)

    for snapshot_slug, snapshot_name, target_time, side in COOLDOWN_SNAPSHOTS:
        snapshot_label = f"{snapshot_name} ({format_time_label(target_time)})"
        plot_outer_node_atomic_percent_pie(
            ordered_case_directories,
            saved_paths,
            target_time=target_time,
            side=side,
            snapshot_label=snapshot_label,
            nodes=JOG_OUTER_NODE_COUNT,
            output_name=f"outer_radial_node_atomic_percent_pie_{snapshot_slug}.svg",
        )
        plot_outer_node_atomic_percent_pie(
            ordered_case_directories,
            saved_paths,
            target_time=target_time,
            side=side,
            snapshot_label=snapshot_label,
            nodes=JOG_OUTER_NODE_COUNT,
            excluded_elements=METALLIC_ELEMENTS_EXCLUDED_FROM_FILTERED_PIE,
            output_name=f"outer_radial_node_atomic_percent_pie_without_ru_pd_rh_tc_{snapshot_slug}.svg",
            title_suffix=" w/o Ru, Pd, Rh, Tc",
        )
        plot_hcp_a3_comparison_pies(
            ordered_case_directories,
            radii_m_array,
            saved_paths,
            target_time=target_time,
            side=side,
            snapshot_label=snapshot_label,
            output_name=f"HCP_A3_comparison_pies_{snapshot_slug}.png",
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

    hcp_constituents = sorted({
        constituent
        for case_history in phase_inventory_histories
        for time_inventory in case_history.values()
        for constituent in time_inventory["hcp_constituents"]
    })
    hcp_profiles: dict[str, np.ndarray] = {}
    for constituent in hcp_constituents:
        aligned_series = []
        for case_history in phase_inventory_histories:
            if not case_history:
                aligned_series.append(np.zeros_like(reference_time))
                continue

            inventory_times = np.array(sorted(case_history), dtype=float)
            values = np.array([
                float(case_history[time_h]["hcp_constituents"].get(constituent, 0.0))
                for time_h in inventory_times
            ])
            aligned_series.append(np.interp(reference_time, inventory_times, values))
        hcp_profiles[constituent] = np.vstack(aligned_series)

    mo_hcp_profile = aligned_inventory_profile("mo_hcp")
    ru_hcp_profile = aligned_inventory_profile("ru_hcp")

    snapshot_entries = radial_snapshot_entries(reference_time, reference_burnup)
    snapshot_colors = evenly_spaced_colors(len(snapshot_entries))
    r_over_ro_array = radii_m_array / PELLET_RADIUS_M
    columnar_min_r_over_ro, columnar_max_r_over_ro = COLUMNAR_GRAIN_R_OVER_RO_RANGE
    columnar_region_mask = (
        (r_over_ro_array >= columnar_min_r_over_ro)
        & (r_over_ro_array <= columnar_max_r_over_ro)
    )

    if "Mo produced (at/m3)" in output_profiles:
        tourasse_fmp_data = load_experimental_fmp_mo_ru_data(EXP_DATA_DIR / "Tourasse1992_FMP.txt")
        tourasse_radial_r_over_ro, tourasse_radial_mo_ru = tourasse_fmp_data["radial"]
        tourasse_fima, tourasse_burnup_mo_ru = tourasse_fmp_data["burnup"]
        fayette_fmp_data = load_experimental_fmp_mo_ru_data(EXP_DATA_DIR / "Fayette2026_FMP.txt")
        fayette_fima, fayette_burnup_mo_ru = fayette_fmp_data["burnup"]
        mo_hcp_over_ru_hcp = np.divide(
            mo_hcp_profile,
            ru_hcp_profile,
            out=np.zeros_like(mo_hcp_profile),
            where=ru_hcp_profile > 0.0,
        )
        def fima_to_reference_burnup(fima_values: np.ndarray) -> np.ndarray:
            return np.interp(fima_values, reference_fima, reference_burnup)

        if not is_all_zero(mo_hcp_over_ru_hcp):
            fig, axis = plt.subplots(1, 1, figsize=(9, 5))
            for color, (index, snapshot_label) in zip(snapshot_colors, snapshot_entries):
                if is_all_zero(mo_hcp_over_ru_hcp[:, index]):
                    continue
                if reference_burnup[index] < 1:
                    continue
                axis.plot(
                    r_over_ro_array,
                    mo_hcp_over_ru_hcp[:, index],
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
            axis.set_ylabel("Mo / Ru in FMP (-)")
            axis.set_ylim(0,1.25)
            axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
            save_figure(fig, PLOTS_DIR / "Mo_Ru_ratio_HCP_A3.png", saved_paths)

            fig, axis = plt.subplots(1,1, figsize=(9,5))
            if np.any(columnar_region_mask):
                columnar_mo_ru = np.mean(mo_hcp_over_ru_hcp[columnar_region_mask, :], axis=0)
                axis.plot(
                    reference_burnup,
                    columnar_mo_ru,
                    color=COLORS[0],
                    label="This work",
                )
                axis.scatter(
                    fima_to_reference_burnup(tourasse_fima),
                    tourasse_burnup_mo_ru,
                    edgecolors=COLORS[7],
                    facecolors="none",
                    marker="D",
                    label="Tourasse et al. (1992)",
                    zorder=3,
                    linewidths=1.6,
                )
                axis.scatter(
                    fima_to_reference_burnup(fayette_fima),
                    fayette_burnup_mo_ru,
                    edgecolors=COLORS[6],
                    facecolors="none",
                    marker="s",
                    label="Fayette et al. (2025)",
                    zorder=3,
                    linewidths=1.6,
                )
                axis.hlines(21.9/19.8, min(reference_burnup), max(reference_burnup), color="black", label="Theoretical yield ratio")
                configure_burnup_axis(axis, reference_burnup)
                axis.set_ylabel("Mo / Ru in FMP (-)")
                axis.set_ylim(0,1.25)
                axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
            else:
                axis.text(
                    0.5,
                    0.5,
                    "No radial nodes fall in the columnar region for this mesh.",
                    ha="center",
                    va="center",
                    transform=axis.transAxes,
                )
                axis.set_axis_off()
            save_figure(fig, PLOTS_DIR / "Mo_Ru_ratio_HCP_A3_burnup.png", saved_paths)

    if "Temperature (K)" in output_profiles:
        fig, axis = plt.subplots(1,1,figsize=(9,5))
        temperature_c_profiles = output_profiles["Temperature (K)"] - 273.15
        for color, (index, snapshot_label) in zip(snapshot_colors, snapshot_entries):
            axis.plot(
                radii_mm_array*1e-3/PELLET_RADIUS_M,
                temperature_c_profiles[:, index],
                color=color,
                marker="o",
                label=snapshot_label,
            )
        axis.set_xlabel("R/Ro")
        axis.set_ylabel("Temperature ($^\\circ$C)")
        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
        save_figure(fig, PLOTS_DIR / "Temperature.png", saved_paths)

    if "Stoichiometry deviation (/)" in output_profiles:
        fig, axis = plt.subplots(1,1,figsize=(9,5))
        for color, (index, snapshot_label) in zip(snapshot_colors, snapshot_entries):
            axis.plot(
                radii_mm_array*1e-3/PELLET_RADIUS_M,
                2.0 + output_profiles["Stoichiometry deviation (/)"][:, index],
                color=color,
                marker="o",
                label=snapshot_label,
            )
        axis.set_xlabel("R/Ro")
        axis.set_ylabel("Oxygen-to-Metal ratio (-)")
        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
        save_figure(fig, PLOTS_DIR / "StoichiometryRadial.png", saved_paths)

    if "Fission gas release (/)" in output_profiles:
        fig, axis = plt.subplots(1, 1, figsize=(8, 5))
        for radius_mm, case_history in zip(radii_mm_array, output_histories):
            case_burnup = burnup_from_history(case_history)
            axis.plot(
                case_burnup,
                100.0 * case_history["Fission gas release (/)"],
                label=f"R/Ro = {(radius_mm*1e-3/PELLET_RADIUS_M):.1f}",
                alpha=0.9,
            )
        axis.set_xlabel(BURNUP_LABEL)
        axis.set_ylabel("Fission gas release (%)")
        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
        save_figure(fig, PLOTS_DIR / "FGR.png", saved_paths)

    oxygen_potential_columns = [
        ("Fuel oxygen potential - Kato (KJ/mol)", "Kato's Equation (NEA/NSC/R(2024)1)", "--"),
        ("Fuel oxygen potential - CALPHAD (KJ/mol)", "CALPHAD", "-"),
        ("Fuel oxygen potential - Blackburn (KJ/mol)", "Blackburn", ":"),
    ]
    available_oxygen_columns = [
        item for item in oxygen_potential_columns
        if item[0] in output_profiles and not is_all_zero(output_profiles[item[0]])
    ]
    if available_oxygen_columns:
        fig, axis = plt.subplots(1,1,figsize=(9,5.5))
        preferred_oxygen_column = next(
            (item for item in available_oxygen_columns if "CALPHAD" in item[0]),
            available_oxygen_columns[0],
        )
        oxygen_column_name, _, _ = preferred_oxygen_column
        experimental_mu_o_entries = load_experimental_matzke_mu_o_data(EXP_DATA_DIR / "Matzke1988_muO.txt")
        experimental_burnups = sorted({
            float(entry["burnup_fima"])
            for entry in experimental_mu_o_entries
        })
        burnup_colors = assign_distinct_colors(
            [f"{burnup:.1f}" for burnup in experimental_burnups],
            palette=PAPER_PALETTE,
        )
        location_to_marker = {
            "Centre": "o",
            "Edge": "D",
            "Average": "s",
        }

        for burnup in experimental_burnups:
            burnup_label = f"{burnup:.1f}"
            color = burnup_colors[burnup_label]
            nearest_index = int(np.argmin(np.abs(reference_fima - burnup)))
            radial_temperatures_k = output_profiles["Temperature (K)"][:, nearest_index]
            radial_mu_o2 = output_profiles[oxygen_column_name][:, nearest_index]
            radial_order = np.argsort(radial_temperatures_k)
            axis.plot(
                radial_temperatures_k[radial_order],
                radial_mu_o2[radial_order],
                color=color,
                linewidth=2.8,
                alpha=0.95,
            )

            for location in ("Centre", "Edge", "Average"):
                location_entries = [
                    entry for entry in experimental_mu_o_entries
                    if np.isclose(float(entry["burnup_fima"]), burnup)
                    and str(entry["location"]) == location
                ]
                if not location_entries:
                    continue

                temperatures_k = np.array(
                    [float(entry["temperature_k"]) for entry in location_entries],
                    dtype=float,
                )
                mu_o2_values = np.array(
                    [float(entry["mu_o2_kj_mol"]) for entry in location_entries],
                    dtype=float,
                )
                order = np.argsort(temperatures_k)
                axis.scatter(
                    temperatures_k[order],
                    mu_o2_values[order],
                    facecolors="none",
                    edgecolors=color,
                    marker=location_to_marker.get(location, "s"),
                    linewidths=1.6,
                    s=85,
                    zorder=3,
                )

        axis.set_xlabel("Temperature (K)")
        axis.set_ylim(-650, -200)
        axis.set_ylabel("Fuel oxygen potential (kJ/mol O$_2$)")
        burnup_handles = [
            Line2D([0], [0], color=burnup_colors[f"{burnup:.1f}"], linewidth=2.5, label=f"{burnup:.1f}% FIMA")
            for burnup in experimental_burnups
        ]
        source_handles = [
            Line2D([0], [0], color="#171717", linewidth=2.8, label="SCIANTIX radial profile"),
            Line2D([0], [0], marker="o", color="#171717", markerfacecolor="none", linestyle="None", markersize=7, label="Matzke Centre"),
            Line2D([0], [0], marker="D", color="#171717", markerfacecolor="none", linestyle="None", markersize=7, label="Matzke Edge"),
            Line2D([0], [0], marker="s", color="#171717", markerfacecolor="none", linestyle="None", markersize=7, label="Matzke Average"),
        ]
        burnup_legend = axis.legend(
            handles=burnup_handles,
            title="Burnup",
            loc="upper left",
            bbox_to_anchor=(1.02, 1.0),
            ncol=1,
        )
        axis.add_artist(burnup_legend)
        axis.legend(
            handles=source_handles,
            title="Series",
            loc="upper left",
            bbox_to_anchor=(1.02, 0.45),
            ncol=1,
        )
        save_figure(fig, PLOTS_DIR / "oxygenpotential.png", saved_paths)

    gb_variables = [
        header
        for header in thermo_profiles
        if is_grain_boundary_amount_column(header)
        and not is_all_zero(thermo_profiles[header][:, reference_before_cooldown], 1)
    ]
    gb_sorted_variables = sorted(
        gb_variables,
        key=lambda variable: float(np.nanmax(
            radial_volume_average(thermo_profiles[variable], radii_m_array)[reference_before_cooldown]
        )),
        reverse=True,
    )

    if gb_sorted_variables:
        summary_entries: list[tuple[str, str, np.ndarray]] = []
        for variable in gb_sorted_variables:
            species = grain_boundary_species(variable)
            phase = grain_boundary_phase(variable)
            if phase == "gas":
                continue
            series = radial_volume_average(thermo_profiles[variable], radii_m_array)
            summary_entries.append((variable, species, series))

        fig, axis = plt.subplots(1, 1, figsize=(9, 5))
        gb_labels = [species for _, species, _ in summary_entries]
        gb_radial_histories = [series for _, _, series in summary_entries]
        gb_colors = [gb_color_map[variable] for variable, _, _ in summary_entries]

        if gb_radial_histories:
            axis.stackplot(
                reference_burnup[reference_before_cooldown],
                [series[reference_before_cooldown] for series in gb_radial_histories],
                colors=gb_colors,
                labels=gb_labels,
                alpha=0.9,
            )

            cumulative_histories = np.cumsum(
                np.vstack([series[reference_before_cooldown] for series in gb_radial_histories]),
                axis=0,
            )
            for boundary in cumulative_histories:
                axis.plot(
                    reference_burnup[reference_before_cooldown],
                    boundary,
                    color="#111827",
                    linewidth=0.25,
                    alpha=0.40,
                )

        configure_burnup_axis(axis, reference_burnup[reference_before_cooldown])
        axis.set_ylabel("Mass concentration (g m$^{-3}$)")
        handles, labels = axis.get_legend_handles_labels()
        axis.legend(handles, labels, loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
        axis.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))

        save_figure(fig, PLOTS_DIR / "Thermochemistry_No_Gas.png", saved_paths)

        if TEMPERATURE_LABEL in output_profiles:
            radial_temperature = radial_volume_average(output_profiles[TEMPERATURE_LABEL], radii_m_array)
            cooldown = cooldown_mask(reference_time)
            if np.any(cooldown) and summary_entries:
                cooldown_stack_total = np.sum(
                    np.vstack([series[cooldown] for _, _, series in summary_entries]),
                    axis=0,
                )
                cooldown_valid = np.zeros_like(cooldown, dtype=bool)
                cooldown_indices = np.where(cooldown)[0]
                cooldown_valid[cooldown_indices] = cooldown_stack_total > 1.0e-12
                cooldown = cooldown_valid
            cooldown_temperature = radial_temperature[cooldown]
            if np.count_nonzero(cooldown) >= 2 and np.unique(cooldown_temperature).size >= 2:
                cooldown_entries = [
                    (variable, species, series)
                    for variable, species, series in summary_entries
                    if not is_all_zero(series[cooldown], 1.0e-12)
                ]
                if cooldown_entries:
                    order = np.argsort(cooldown_temperature)
                    cooldown_x = cooldown_temperature[order]
                    cooldown_labels = [species for _, species, _ in cooldown_entries]
                    cooldown_histories = [series[cooldown][order] for _, _, series in cooldown_entries]
                    cooldown_colors = [gb_color_map[variable] for variable, _, _ in cooldown_entries]

                    fig, axis = plt.subplots(1, 1, figsize=(9, 5))
                    axis.stackplot(
                        cooldown_x,
                        cooldown_histories,
                        colors=cooldown_colors,
                        labels=cooldown_labels,
                        alpha=0.9,
                    )
                    cumulative_histories = np.cumsum(np.vstack(cooldown_histories), axis=0)
                    for boundary in cumulative_histories:
                        axis.plot(cooldown_x, boundary, color="#111827", linewidth=0.25, alpha=0.40)
                    cooldown_total = np.sum(np.vstack(cooldown_histories), axis=0)
                    axis.plot(
                        cooldown_x,
                        cooldown_total,
                        color="black",
                        linewidth=1.0,
                        label="Plotted stack",
                    )
                    if cooldown_total.size and float(np.nanmax(cooldown_total)) > 0.0:
                        axis.set_ylim(0.0, float(np.nanmax(cooldown_total)) * 1.05)
                    axis.set_xlabel(TEMPERATURE_LABEL)
                    axis.set_xlim(float(np.nanmin(cooldown_x)), float(np.nanmax(cooldown_x)))
                    axis.set_ylabel("Mass concentration (g m$^{-3}$)")
                    handles, labels = axis.get_legend_handles_labels()
                    axis.legend(handles, labels, loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
                    axis.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))
                    save_figure(fig, PLOTS_DIR / "Thermochemistry_No_Gas_cooldown_temperature.png", saved_paths)

    jog_columns = sorted_jog_columns(output_profiles)
    if jog_columns:
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
        outer_indices = [len(radii_m_array) - 1]
        jog_total_thickness_over_time_um = radial_integral_masked_to_full_radius(
            jog_total_profile,
            radii_m_array,
            outer_indices,
        ) * 1.0e6

        melis_fima, melis_thickness = load_experimental_jog_data(EXP_DATA_DIR / "Melis1993.txt")
        tourasse_fima, tourasse_thickness = load_experimental_jog_data(EXP_DATA_DIR / "Tourasse1992_JOG.txt")

        def fima_to_burnup(fima_values: np.ndarray) -> np.ndarray:
            return np.interp(fima_values, reference_fima, reference_burnup)

        jog_entries: list[tuple[str, np.ndarray, object]] = []
        jog_colors = assign_distinct_colors([jog_label(column) for column in jog_stack_columns])
        for index, column_name in enumerate(jog_stack_columns):
            label = jog_label(column_name)
            series = radial_integral_masked_to_full_radius(
                output_profiles[column_name],
                radii_m_array,
                outer_indices,
            ) * 1.0e6
            if is_all_zero(series):
                continue
            jog_entries.append((label, series, jog_colors.get(label, PAPER_PALETTE[index % len(PAPER_PALETTE)])))

        jog_labels = [item[0] for item in jog_entries]
        jog_histories = [item[1] for item in jog_entries]
        jog_plot_colors = [item[2] for item in jog_entries]

        fig, axis = plt.subplots(1, 1, figsize=(10, 5))

        if jog_histories:
            axis.stackplot(
                reference_burnup,
                *jog_histories,
                colors=jog_plot_colors,
                labels=jog_labels,
                alpha=0.9,
            )
            cumulative_histories = np.cumsum(np.vstack(jog_histories), axis=0)
            for boundary in cumulative_histories:
                axis.plot(reference_burnup, boundary, color="#111827", linewidth=0.25, alpha=0.40)
        axis.plot(
            reference_burnup,
            jog_total_thickness_over_time_um,
            color="#111827",
            linewidth=2.6,
            label="Total, outer radial node",
        )

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
        axis.set_xlabel(BURNUP_LABEL)
        axis.set_xlim(0,max(reference_burnup))
        axis.set_ylabel("JOG thickness ($\\mu$m)")
        axis.set_ylim(0,100)
        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
        save_figure(fig, PLOTS_DIR / "JOG.png", saved_paths)

        nodes = JOG_OUTER_NODE_COUNT
        outer_node_indices = list(range(max(0, len(radii_m_array) - nodes), len(radii_m_array)))
        jog_outer_nodes_total_thickness_um = radial_integral_masked_to_full_radius(
            jog_total_profile,
            radii_m_array,
            outer_node_indices,
        ) * 1.0e6

        jog_outer_nodes_entries: list[tuple[str, np.ndarray, object]] = []
        for index, column_name in enumerate(jog_stack_columns):
            label = jog_label(column_name)
            series = radial_integral_masked_to_full_radius(
                output_profiles[column_name],
                radii_m_array,
                outer_node_indices,
            ) * 1.0e6
            if is_all_zero(series):
                continue
            jog_outer_nodes_entries.append(
                (label, series, jog_colors.get(label, PAPER_PALETTE[index % len(PAPER_PALETTE)]))
            )

        jog_outer_nodes_histories = [item[1] for item in jog_outer_nodes_entries]
        jog_outer_nodes_colors = [item[2] for item in jog_outer_nodes_entries]
        jog_outer_nodes_labels = [item[0] for item in jog_outer_nodes_entries]

        fig, axis = plt.subplots(1,1,figsize=(10,5))

        if jog_outer_nodes_histories:
            axis.stackplot(
                reference_burnup,
                *jog_outer_nodes_histories,
                colors=jog_outer_nodes_colors,
                labels=jog_outer_nodes_labels,
                alpha=0.9,
            )
            cumulative_histories = np.cumsum(np.vstack(jog_outer_nodes_histories), axis=0)
            for boundary in cumulative_histories:
                axis.plot(reference_burnup, boundary, color="#111827", linewidth=0.25, alpha=0.40)

        axis.plot(
            reference_burnup,
            jog_outer_nodes_total_thickness_um,
            color="#111827",
            linewidth=2.6,
            label="Total, outer radial nodes",
        )
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
        axis.set_xlabel(BURNUP_LABEL)
        axis.set_ylabel("JOG thickness ($\\mu$m)")
        axis.set_ylim(0,100)
        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
        save_figure(fig, PLOTS_DIR / "JOG_outer_nodes.png", saved_paths)


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
    args = parser.parse_args()

    case_directories = case_dirs()
    if not case_directories:
        raise FileNotFoundError(f"No point_* directories found in {TEST_DIR}")

    if args.runnode:
        if args.number is None:
            raise ValueError("--runnode requires --number")
        case_directories = filter_case_dirs(case_directories, args.number)

    if not args.run:
        case_directories = completed_case_dirs(case_directories)

    PLOTS_DIR.mkdir(exist_ok=True)
    saved_paths: list[Path] = []
    case_results: list[tuple[Path, int]] = []
    comparison_results: list[tuple[Path, bool, Path]] = []

    run_cases = args.run
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
            f"in {(PLOTS_DIR / case_dir.name).relative_to(TEST_DIR)}",
            flush=True,
        )

    plot_radial_profiles(case_directories, saved_paths, gb_color_map)

    print("Generated plots:")
    for path in saved_paths:
        print(path)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
