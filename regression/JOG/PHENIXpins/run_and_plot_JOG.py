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
from matplotlib.lines import Line2D
import numpy as np

TEST_DIR = Path(__file__).resolve().parent
RUN_LOG = "sciantix.log"
REPO_ROOT = TEST_DIR.parents[2]
BUILD_DIR = REPO_ROOT / "build"
BUILD_EXECUTABLE = BUILD_DIR / "sciantix.x"
ALLMAKE_OC_SCRIPT = REPO_ROOT / "Allmake_OC.sh"
OXIRED_SCRIPT = REPO_ROOT / "preprocessing" / "oxired_lib" / "examples" / "PHENIXpins.py"
OXIRED_HISTORY_DIR = OXIRED_SCRIPT.parent / "PHENIXpins_history"
CSRED_SCRIPT = REPO_ROOT / "preprocessing" / "csred_lib" / "examples" / "PHENIXpins.py"
# Index into the (non-comment) value lines of input_scaling_factors.txt.
# main's 11-slot layout ends with grain_boundary_energy, fabricated_porosity,
# cs_production (index 10) -- was index 4 under JOG's old 10-slot layout.
CS_PRODUCTION_SCALING_FACTOR_INDEX = 10
SCALING_FACTORS_TEMPLATE = TEST_DIR / "input_scaling_factors.txt"
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
FIMA_LABEL = "Burnup (%FIMA)"
COOLDOWN_START = 25200.0
COOLDOWN_END = 25224.0
COOLDOWN_SNAPSHOTS = (
    ("pre_cooldown", "pre-cooldown", COOLDOWN_START, "before"),
    ("post_cooldown", "post-cooldown", COOLDOWN_END, "after"),
)
JOG_OUTER_NODE_COUNT = 2
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


def burnup_column_name(columns: dict[str, int] | dict[str, np.ndarray]) -> str:
    for label in BURNUP_COLUMN_LABELS:
        if label in columns:
            return label
    available = ", ".join(BURNUP_COLUMN_LABELS)
    raise KeyError(f"Missing burnup column. Expected one of: {available}")


def burnup_from_history(case_history: dict[str, np.ndarray]) -> np.ndarray:
    return case_history[burnup_column_name(case_history)]


def cooldown_mask(time: np.ndarray) -> np.ndarray:
    return (time >= COOLDOWN_START) & (time <= COOLDOWN_END)


def before_cooldown_mask(time: np.ndarray) -> np.ndarray:
    mask = time < COOLDOWN_START
    if np.count_nonzero(mask) >= 2:
        return mask
    return np.ones_like(time, dtype=bool)


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

    if not SCALING_FACTORS_TEMPLATE.exists():
        raise FileNotFoundError(f"Missing template scaling factors file: {SCALING_FACTORS_TEMPLATE}")

    for history_case_dir in history_case_dirs:
        history_file = history_case_dir / "input_history.txt"
        if not history_file.exists():
            raise FileNotFoundError(f"Missing generated OXIRED history file: {history_file}")

        case_dir = TEST_DIR / history_case_dir.name
        case_dir.mkdir(parents=True, exist_ok=True)
        shutil.copy2(history_file, case_dir / "input_history.txt")

        case_scaling_factors = case_dir / "input_scaling_factors.txt"
        if not case_scaling_factors.exists():
            shutil.copy2(SCALING_FACTORS_TEMPLATE, case_scaling_factors)

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
    min_spacing: float = 0.42,
) -> None:
    """Label every wedge outside the pie with a leader line.

    Labels are gathered into two columns (left/right of the pie, chosen by
    each wedge's mid-angle), sorted by the wedge's vertical position, and
    then decluttered top-down so that consecutive labels never sit closer
    than ``min_spacing``. This keeps every label legible regardless of how
    many narrow wedges are adjacent in angle.
    """
    thetas = [math.radians((wedge.theta1 + wedge.theta2) / 2.0) for wedge in wedges]
    column_x = pie_radius * 1.45

    for side in (1.0, -1.0):
        side_items = [
            (theta, index)
            for index, theta in enumerate(thetas)
            if (1.0 if math.cos(theta) >= 0.0 else -1.0) == side
        ]
        if not side_items:
            continue
        # Sort by desired vertical position, top first.
        side_items.sort(key=lambda item: -math.sin(item[0]))
        desired_ys = [pie_radius * 1.30 * math.sin(theta) for theta, _ in side_items]
        top_limit = pie_radius * 1.55
        # Declutter: push labels down when they would overlap the one above.
        placed_ys: list[float] = []
        for desired_y in desired_ys:
            y = min(desired_y, top_limit if not placed_ys else placed_ys[-1] - min_spacing)
            placed_ys.append(y)
        # If the column ran too low, shift it up as a block within the limits.
        bottom_limit = -pie_radius * 1.55
        overshoot = bottom_limit - placed_ys[-1]
        if overshoot > 0.0:
            shift = min(overshoot, top_limit - placed_ys[0])
            placed_ys = [y + shift for y in placed_ys]

        for (theta, index), text_y in zip(side_items, placed_ys):
            x, y = math.cos(theta), math.sin(theta)
            text_x = column_x * side
            ha = "left" if side > 0.0 else "right"
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
                text_x + 0.04 * side, text_y, f"{elements[index]} ",
                ha=ha, va="center", fontsize=element_fontsize, fontweight="bold", color="#171717",
            )
            axis.text(
                text_x + 0.04 * side, text_y - 0.10, value_labels[index],
                ha=ha, va="top", fontsize=value_fontsize, color="#171717",
            )


def assign_distinct_colors(labels: list[str], palette: list[str] = PAPER_PALETTE) -> dict[str, object]:
    return {
        label: palette[index % len(palette)]
        for index, label in enumerate(dict.fromkeys(labels))
    }


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


def read_scaling_factor_entries(path: Path) -> list[tuple[float, str]]:
    """Parse an input_scaling_factors.txt file into (value, label) pairs.

    SCIANTIX reads these values purely by position (see InputReading.C), so the
    "# scaling factor - <label>" comments are cosmetic as far as the solver is
    concerned. They're still useful here to recognize a specific factor -- e.g.
    Cs production -- across schema versions where its position has shifted.
    """
    lines = path.read_text().splitlines()
    entries: list[tuple[float, str]] = []
    for index, line in enumerate(lines):
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        label = ""
        if index + 1 < len(lines) and lines[index + 1].strip().startswith("#"):
            label = re.sub(
                r"^#?\s*scaling factor\s*-\s*", "", lines[index + 1].strip(), flags=re.IGNORECASE
            )
        entries.append((float(stripped), label))
    return entries


def sync_case_scaling_factors(case_dir: Path) -> None:
    """Keep a case's input_scaling_factors.txt aligned with the current schema.

    A per-case file left over from an older schema silently misassigns every
    value once new scaling factors are inserted ahead of the existing ones --
    this previously caused an old case's "Cs production" value to be read back
    as "Temperature", corrupting an entire run. Resync structure from the
    shared template whenever the entry count no longer matches it, carrying
    forward the case-specific Cs production value (the only field CSRED
    customizes per radius) by matching its label rather than its position.
    """
    case_path = case_dir / "input_scaling_factors.txt"
    template_entries = read_scaling_factor_entries(SCALING_FACTORS_TEMPLATE)

    cs_production_value = None
    if case_path.exists():
        case_entries = read_scaling_factor_entries(case_path)
        if len(case_entries) == len(template_entries):
            return
        cs_production_value = next(
            (value for value, label in case_entries if "cs production" in label.lower()),
            None,
        )

    shutil.copy2(SCALING_FACTORS_TEMPLATE, case_path)
    if cs_production_value is not None:
        update_scaling_factor_value(
            case_path,
            index=CS_PRODUCTION_SCALING_FACTOR_INDEX,
            value=cs_production_value,
            comment="Cs production",
        )


def prepare_case_inputs(case_dir: Path) -> None:
    sync_case_scaling_factors(case_dir)
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


def phase_instance_from_row(row: dict[str, str]) -> str:
    return row.get("Phase instance", "").strip()


def display_phase_label(phase: str, phase_instance: str) -> str:
    if not phase_instance or phase_instance.lower() == phase.lower():
        return phase
    match = re.search(r"#(.+)$", phase_instance)
    if match:
        return f"{phase} #{match.group(1).strip()}"
    return f"{phase} ({phase_instance})"


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
    # Drop essentially-zero entries (e.g. Ba at 0.0 at.% in the GERMINAL
    # composition): a wedge of zero width only clutters the labelling.
    labels = [
        label
        for label in sorted(composition, key=composition.get, reverse=True)
        if composition[label] >= 0.05
    ]
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
    reference_fima: np.ndarray,
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
            f"  End of irradiation, pre-cooldown ({reference_fima[pre_index]:.1f} %FIMA): "
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
            onset_fima = float(reference_fima[int(np.argmax(onset_mask))]) if np.any(onset_mask) else float("nan")
            pre_share = 100.0 * pre_value_um / pre_total_um if pre_total_um > 0.0 else 0.0
            post_share = 100.0 * post_value_um / post_total_um if post_total_um > 0.0 else 0.0
            lines.append(
                f"    {jog_label(column)}: pre-cooldown {pre_value_um:.1f} um ({pre_share:.0f}%), "
                f"post-cooldown {post_value_um:.1f} um ({post_share:.0f}%), "
                f"onset at burnup ~{onset_fima:.1f} %FIMA"
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

    def short_label(phase: str) -> str:
        # Strip the trailing top-constituents parenthetical (kept only in
        # the plotted phase key, used for colour grouping): the full
        # composition at each point is already given in the Results text,
        # so the legend only needs to identify the phase, not repeat it.
        return re.sub(r"\s*\([^()]*\)\s*$", "", phase)

    # Marker shape is keyed by the short label too, in order of first
    # appearance, so every entry sharing a legend row (e.g. several
    # distinct Liquid compositions) is drawn with the same symbol as well
    # as the same colour (colour is already species-grouped by
    # build_label_color_map).
    marker_map: dict[str, str] = {}
    for phase, _ in entries:
        label = short_label(phase)
        if label not in marker_map:
            marker_map[label] = marker_cycle[len(marker_map) % len(marker_cycle)]

    for phase, values in entries:
        plotted_values = np.where(values > 0.0, values, np.nan)
        label = short_label(phase)
        phase_axis.plot(
            r_over_ro,
            plotted_values,
            color=color_map[phase],
            marker=marker_map[label],
            linestyle=(0, (2.0, 2.5)),
            linewidth=1.4,
            markersize=8.0,
            label=label,
            zorder=2,
        )

    phase_axis.set_yscale("log")
    phase_axis.set_ylabel("Phase fraction")
    phase_axis.set_ylim(1.0e-3, 1.5)
    # Legend inside the empty left band (R/Ro < ~0.3, inward of point 1) so
    # the pre- and post-cooldown figures render at identical sizes.
    # Deduplicated so phases sharing a short label (and now also a colour
    # and marker) appear as a single legend row.
    legend_handles, legend_labels = phase_axis.get_legend_handles_labels()
    unique_legend = dict(zip(legend_labels, legend_handles))
    phase_axis.legend(
        unique_legend.values(),
        unique_legend.keys(),
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


def plot_radial_profiles(
    case_directories: list[Path],
    saved_paths: list[Path],
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

    # Only the post-cooldown pie is used in the paper (Fig. cappia comparison).
    for snapshot_slug, snapshot_name, target_time, side in COOLDOWN_SNAPSHOTS:
        if snapshot_slug != "post_cooldown":
            continue
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
    # Only pre-/post-cooldown are shown in the paper (Fig. radial phase mole
    # fraction); mid-irradiation stays in radial_phase_snapshots for the text
    # results summary below.
    for snapshot_slug, snapshot_name, target_time, side in COOLDOWN_SNAPSHOTS:
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
            x_values = reference_fima

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

        fig, axis = plt.subplots(1, 1, figsize=(11.5, 6))

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

            # Below ~4 %FIMA the measured fuel-cladding gap is likely still
            # open (not yet filled by JOG): those points stay hollow, while
            # measurements at and above the threshold use filled symbols.
            # Each dataset gets a single legend entry with a half-filled
            # marker standing for both point styles.
            open_gap_fima_threshold = 4.0
            experimental_handles: list[Line2D] = []
            for name, exp_fima, exp_thickness, marker, color in (
                ("Melis et al. (1993)", melis_fima, melis_thickness, "o", COLORS[6]),
                ("Tourasse et al. (1992)", tourasse_fima, tourasse_thickness, "D", COLORS[7]),
            ):
                jog_mask = exp_fima >= open_gap_fima_threshold
                if np.any(jog_mask):
                    axis.scatter(
                        exp_fima[jog_mask],
                        exp_thickness[jog_mask],
                        edgecolors="black", facecolors=color,
                        marker=marker,
                        label="_nolegend_",
                        zorder=3,
                        linewidths=1.6,
                    )
                if np.any(~jog_mask):
                    axis.scatter(
                        exp_fima[~jog_mask],
                        exp_thickness[~jog_mask],
                        edgecolors="black", facecolors="none",
                        marker=marker,
                        label="_nolegend_",
                        zorder=3,
                        linewidths=1.6,
                    )
                experimental_handles.append(
                    Line2D(
                        [], [],
                        marker=marker,
                        linestyle="",
                        markeredgecolor="black",
                        markerfacecolor=color,
                        markerfacecoloralt="none",
                        fillstyle="left",
                        markersize=9,
                        label=name,
                    )
                )
            samuelsson_markers = {"\nGERMINAL correlation": "s", "\nOC stand-alone + TAF-ID": "^"}
            samuelsson_colors = {"\nGERMINAL correlation": COLORS[8], "\nOC stand-alone + TAF-ID": COLORS[9]}
            samuelsson_linestyles = {"\nGERMINAL correlation": "--", "\nOC stand-alone + TAF-ID": "-."}
            for section_label, (fima_values, thickness_values) in samuelsson_simulation_data.items():
                axis.plot(
                    fima_values,
                    thickness_values,
                    color=samuelsson_colors.get(section_label, COLORS[10 % len(COLORS)]),
                    marker=samuelsson_markers.get(section_label, "x"),
                    markerfacecolor="none",
                    linestyle=samuelsson_linestyles.get(section_label, "--"),
                    label=f"Samuelsson et al. (2020), {section_label}",
                    zorder=3,
                    linewidth=2.6,
                )
            axis.set_xlabel(FIMA_LABEL)
            eol_fima = float(np.nanmax(reference_fima))
            axis.set_xlim(0, eol_fima)
            # Explicit final tick so the end-of-life burnup (13.3 %FIMA) is
            # readable directly on the axis.
            ticks = [tick for tick in np.arange(0.0, eol_fima, 2.0) if tick <= eol_fima - 1.0]
            axis.set_xticks(ticks + [round(eol_fima, 1)])
            axis.set_ylim(0, 100)
        else:
            axis.set_xlabel(TEMPERATURE_LABEL)
            axis.set_xlim(float(np.nanmax(x_masked)), float(np.nanmin(x_masked)))
            axis.set_ylim(0, max(float(np.nanmax(jog_total_series)) * 1.1, 1.0e-6))

        axis.set_ylabel("JOG thickness ($\\mu$m)")
        legend_handles, _ = axis.get_legend_handles_labels()
        if not cooldown:
            # Insert the half-filled experimental markers between the model
            # stack entries and the Samuelsson curves.
            insert_at = len(legend_handles) - len(samuelsson_simulation_data)
            legend_handles[insert_at:insert_at] = experimental_handles
        axis.legend(
            handles=legend_handles,
            loc="center left",
            bbox_to_anchor=(1.02, 0.5),
            ncol=1,
            fontsize=13,
            labelspacing=0.5,
        )
        # Fixed layout and no tight bounding box: the oxide-only and
        # oxide+metallic figures must have identical canvas and axes sizes
        # regardless of how many legend entries each one carries.
        fig.subplots_adjust(left=0.07, right=0.58, top=0.95, bottom=0.13)
        output_path = PLOTS_DIR / filename
        output_path.parent.mkdir(parents=True, exist_ok=True)
        fig.savefig(output_path)
        plt.close(fig)
        saved_paths.append(output_path)

    plot_jog_figure("JOG.png", include_metallics=False)
    plot_jog_figure("JOG_oxides_metallics.png", include_metallics=True)

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
            output_profiles, radii_m_array, reference_fima, reference_time, METALLIC_JOG_COLUMNS
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

    results_summary_lines = plot_radial_profiles(case_directories, saved_paths)

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
