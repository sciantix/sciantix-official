#!/usr/bin/env python3
"""Regenerate JOG paper figures and results_summary.txt from already-run
validation/jog/test_PHENIXpins_point_* cases."""
import argparse
import csv
import math
import os
import re
import sys
import warnings
from collections import defaultdict
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib")

import matplotlib.pyplot as plt
from matplotlib.colors import to_rgb
from matplotlib.lines import Line2D
import numpy as np

TEST_DIR = Path(__file__).resolve().parent
CASES_DIR = TEST_DIR.parent
REPO_ROOT = TEST_DIR.parents[2]
RESULTS_SUMMARY = TEST_DIR / "results_summary.txt"
MAIN_OUTPUT_NAME = "output.txt"
THERMO_OUTPUT_NAME = "thermochemistry_output.txt"
PHASE_SUBLATTICE_OUTPUT_NAME = "phase_sublattice_composition.txt"

PLOTS_DIR = TEST_DIR / "plots"
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
# (Hoel-Bacle and Cappia references, SCIANTIX, GERMINAL/Oulfarsi), so the same element always
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
    return sorted(path for path in CASES_DIR.glob("test_PHENIXpins_point_*") if path.is_dir())


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

        for rank, ((theta, index), text_y) in enumerate(zip(side_items, placed_ys)):
            x, y = math.cos(theta), math.sin(theta)
            text_x = column_x * side
            ha = "left" if side > 0.0 else "right"
            # Leader line routed outside the pie: a short radial stub, then
            # horizontal and vertical segments to the label. The vertical segment
            # of a lower label sits closer to the pie, so that leader lines of
            # labels pushed down by the decluttering never cross each other.
            stub_x, stub_y = 1.08 * pie_radius * x, 1.08 * pie_radius * y
            elbow_x = side * pie_radius * (1.16 + 0.035 * (len(side_items) - 1 - rank))
            if abs(text_y - stub_y) < 0.1 * pie_radius:
                # Label almost level with its wedge: one straight segment, no step.
                line_x = [pie_radius * x, stub_x, text_x]
                line_y = [pie_radius * y, stub_y, text_y]
            else:
                line_x = [pie_radius * x, stub_x, elbow_x, elbow_x, text_x]
                line_y = [pie_radius * y, stub_y, stub_y, text_y, text_y]
            axis.plot(
                line_x,
                line_y,
                color="#8a8a83",
                lw=1.0,
                zorder=3,
                solid_joinstyle="miter",
            )
            axis.text(
                text_x + 0.04 * side, text_y, f"{elements[index]} ",
                ha=ha, va="center", fontsize=element_fontsize, fontweight="bold", color="#171717",
            )
            axis.text(
                text_x + 0.04 * side, text_y - 0.10, value_labels[index],
                ha=ha, va="top", fontsize=value_fontsize, color="#171717",
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
    # Molybdates plus the binary oxides MoO2 and BaO (HALITE).
    oxide_phases = {"BAMOO4", "BA2MOO5", "BA3MOO6", "MOO2"}
    return (
        phase_name.startswith("CS2MOO4")
        or phase_instance_name.startswith("CS2MOO4")
        or phase_name in oxide_phases
        or phase_instance_name in oxide_phases
        or phase_name == "HALITE"
        or phase_instance_name.startswith("HALITE")
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
    uncertainties: dict[str, float] | None = None,
) -> None:
    # Drop essentially-zero entries (e.g. Ba at 0.0 at.% in the GERMINAL
    # composition): a wedge of zero width only clutters the labelling.
    # Wedges follow the fixed ELEMENT_COLORS order, so the same element sits at
    # the same place in every pie and side-by-side pies can be compared.
    element_order = list(ELEMENT_COLORS)
    labels = [
        label
        for label in sorted(
            composition,
            key=lambda label: (
                element_order.index(label) if label in element_order else len(element_order),
                -composition[label],
            ),
        )
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

    if uncertainties:
        value_labels = [f"{composition[label]:.0f} ± {uncertainties[label]:.0f}%" for label in labels]
    else:
        value_labels = [f"{value:.1f}%" for value in values]
    place_pie_wedge_labels(axis, wedges, labels, values, value_labels, pie_radius)

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


def load_hoelbacle_composition(data_file: Path, dataset: str) -> tuple[dict[str, float], dict[str, float]]:
    """Mean and standard deviation (at.%) of one dataset of HoelBacle2026_composition.txt."""
    composition: dict[str, float] = {}
    uncertainty: dict[str, float] = {}
    with data_file.open() as handle:
        for raw_line in handle:
            line = raw_line.strip()
            if not line or line.startswith("#") or line.startswith("Dataset,"):
                continue
            name, element, value_str, std_str = [item.strip() for item in line.split(",")]
            if name == dataset:
                composition[element] = float(value_str)
                uncertainty[element] = float(std_str)
    return composition, uncertainty


def plot_jog_composition_comparison_pies(
    case_directories: list[Path],
    saved_paths: list[Path],
    *,
    target_time: float,
    side: str,
    snapshot_label: str,
    output_name: str,
) -> None:
    """JOG composition pies. Top row: the SCIANTIX outer radial node between
    the two Hoel-Bacle EPMA samples of the NESTOR-3 JOG (MA04 left, MA07 right,
    Cs-Ba-Mo-O only). Bottom row: Cappia et al. EDS of an FFTF JOG and the
    GERMINAL-based NESTOR-3 estimate of Oulfarsi et al. (2024). The Cappia
    panel is drawn from its own composition data (support/FPs_plot.py), not a
    saved image, so all pies share the same rendering."""
    hoelbacle_file = EXP_DATA_DIR / "HoelBacle2026_composition.txt"
    hoelbacle_samples = {
        sample: load_hoelbacle_composition(hoelbacle_file, f"{sample} EPMA")
        for sample in ("MA04", "MA07")
    }
    if not all(composition for composition, _ in hoelbacle_samples.values()):
        return

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
        for element in set(sciantix_atomic_percent)
        | set(germinal_composition)
        | set(cappia_composition)
        | set(hoelbacle_samples["MA04"][0])
        | set(hoelbacle_samples["MA07"][0])
    }

    # Six-column grid: three pies on top, the two bottom pies centred under them.
    fig = plt.figure(figsize=(13, 9.5))
    grid = fig.add_gridspec(2, 6)
    top_axes = [fig.add_subplot(grid[0, 2 * col : 2 * col + 2]) for col in range(3)]
    bottom_axes = [fig.add_subplot(grid[1, 1:3]), fig.add_subplot(grid[1, 3:5])]

    for axis, sample in ((top_axes[0], "MA04"), (top_axes[2], "MA07")):
        composition, uncertainty = hoelbacle_samples[sample]
        add_atomic_percent_pie(
            axis,
            composition,
            color_map,
            f"EPMA {sample}, NESTOR-3 JOG\nHoël-Bacle, 2026",
            uncertainties=uncertainty,
        )
    add_atomic_percent_pie(
        top_axes[1],
        sciantix_atomic_percent,
        color_map,
        "SCIANTIX, NESTOR-3 JOG\nThis work, post-cooldown",
    )
    add_atomic_percent_pie(
        bottom_axes[0],
        cappia_composition,
        color_map,
        "EDS, FFTF JOG\nCappia et al., 2020",
    )
    add_atomic_percent_pie(
        bottom_axes[1],
        germinal_composition,
        color_map,
        "GERMINAL, NESTOR-3 JOG\nOulfarsi et al., 2024",
    )

    fig.subplots_adjust(wspace=0.15, hspace=0.25, left=0.02, right=0.98, top=0.92, bottom=0.03)
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


# Alternating neutral bands mark the pellet shell each SCIANTIX node
# represents. Same style in the Cs and O/M preprocessing figures
# (csred_lib/ and oxired_lib/examples/PHENIXpins.py).
RADIAL_POINT_BAND_COLOR = "#ececE8"

# Same phase -> same colour in every JOG figure (JOG stack plots and radial
# phase-fraction figure). Fixed, so dropping the metallic phases from a
# figure does not shift the colours of the other phases.
JOG_PHASE_COLORS = {
    "Ba3MoO6": "#736F3F",          # olive
    "BaMoO4": "#BFAE56",           # muted mustard
    "BaO": "#B29DA6",              # dusty mauve
    "Cs2MoO4": "#D9AF32",          # muted gold
    "FCC": "#A66226",              # burnt orange
    "HCP": "#733426",              # dark terracotta
    "MoO2": "#737675",             # warm grey
    "MoPd2": "#9D6953",            # muted clay
    "Perovskite": "#363726",       # dark olive
    "liquid metallic": "#785C2D",  # earthy brown
    "liquid oxide": "#789174",     # sage green
    "Sigma": "#657D8A",            # dusty blue
    "other phases": "#8B7966",     # warm taupe
}

# Radial phase-fraction figure: colour from JOG_PHASE_COLORS, marker = phase,
# open marker = variant (beta-Cs2MoO4, Cs-rich liquid, ordered HCP), so a
# phase looks the same at end of irradiation and after cooldown. Small
# horizontal offsets (R/Ro) keep phases with similar fractions at the same
# node from hiding each other (e.g. HCP and Ba3MoO6 at point 4).
RADIAL_OXIDE_DODGE = 0.018
RADIAL_METALLIC_DODGE = -0.018
# key: (legend label, colour, marker, filled, x-offset)
RADIAL_PHASE_STYLE = {
    "CS2MOO4_S1": (r"$\alpha$-Cs$_2$MoO$_4$ (S1)", JOG_PHASE_COLORS["Cs2MoO4"], "o", True, 0.0),
    "CS2MOO4_S2": (r"$\beta$-Cs$_2$MoO$_4$ (S2)", JOG_PHASE_COLORS["Cs2MoO4"], "o", False, 0.0),
    "LIQUID_BA": ("Liquid oxide, Ba-rich", JOG_PHASE_COLORS["liquid oxide"], "s", True, 0.0),
    "LIQUID_CS": ("Liquid oxide, Cs-rich", JOG_PHASE_COLORS["liquid oxide"], "s", False, 0.0),
    "BAMOO4": ("BaMoO$_4$", JOG_PHASE_COLORS["BaMoO4"], "^", True, RADIAL_OXIDE_DODGE),
    "BA3MOO6": ("Ba$_3$MoO$_6$", JOG_PHASE_COLORS["Ba3MoO6"], "v", True, RADIAL_OXIDE_DODGE),
    "BAO": ("BaO (halite)", JOG_PHASE_COLORS["BaO"], "X", True, RADIAL_OXIDE_DODGE),
    "MOO2": ("MoO$_2$", JOG_PHASE_COLORS["MoO2"], "*", True, RADIAL_OXIDE_DODGE),
    "HCP": ("HCP", JOG_PHASE_COLORS["HCP"], "D", True, RADIAL_METALLIC_DODGE),
    "HCP_ORD": ("HCP (ord.)", JOG_PHASE_COLORS["HCP"], "D", False, RADIAL_METALLIC_DODGE),
    "FCC": ("FCC", JOG_PHASE_COLORS["FCC"], "P", True, RADIAL_METALLIC_DODGE),
}
RADIAL_PHASE_KEY_BY_DB_NAME = {
    "CS2MOO4_S1": "CS2MOO4_S1", "CS2MOO4_S2": "CS2MOO4_S2", "BAMOO4": "BAMOO4",
    "BA3MOO6": "BA3MOO6", "HALITE": "BAO", "MOO2": "MOO2",
    "HCP_A3": "HCP", "HCP_ORD": "HCP_ORD", "FCC_A1": "FCC",
}
RADIAL_PROFILE_COLOR = "#363726"  # dark olive

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
    """Mark the pellet shell each SCIANTIX node represents (alternating grey
    bands, same mesh edges as the Cs/O redistribution preprocessing) and
    label it "Point N"."""
    edges = preprocessing_radial_edges_over_ro(n_cells)
    for i in range(n_cells):
        if i % 2 == 0:
            axis.axvspan(edges[i], edges[i + 1], color=RADIAL_POINT_BAND_COLOR, zorder=0, linewidth=0)
        if show_labels:
            axis.text(
                0.5 * (edges[i] + edges[i + 1]), 0.97,
                f"Point {i + 1}", ha="center", va="top", fontsize=15, color="#3a3a3a",
                transform=axis.get_xaxis_transform(), zorder=1,
            )
    axis.grid(False, axis="x")


def radial_phase_key(phase: str, instance_rows: list[dict[str, object]]) -> str:
    """Key of RADIAL_PHASE_STYLE; the oxide liquids are told apart by their
    dominant cation (CS+ vs BA+2 on the first sublattice)."""
    name = phase.strip().upper()
    if name == "LIQUID":
        cations = {
            str(row["constituent"]): float(row["site_fraction"])
            for row in instance_rows
            if int(row["sublattice"]) == 1
        }
        return "LIQUID_CS" if cations.get("CS+", 0.0) > cations.get("BA+2", 0.0) else "LIQUID_BA"
    return RADIAL_PHASE_KEY_BY_DB_NAME.get(name, name)


def radial_phase_fractions(case_dir: Path, target_time: float, side: str) -> dict[str, float]:
    """Grain-boundary phase mole fractions at one snapshot, by RADIAL_PHASE_STYLE key."""
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
    instances: dict[tuple[str, str], list[dict[str, object]]] = defaultdict(list)
    for row in snapshot_rows:
        instances[(str(row["phase"]), str(row["phase_instance"]))].append(row)
    moles: dict[str, float] = defaultdict(float)
    for (phase, _), instance_rows in instances.items():
        n = max(float(row["phase_moles"]) for row in instance_rows)
        if n > 0.0:
            moles[radial_phase_key(phase, instance_rows)] += n
    total = sum(moles.values())
    return {key: n / total for key, n in moles.items()} if total > 0.0 else {}


def plot_radial_phase_mole_fraction_pre_post(
    case_directories: list[Path],
    radii_m_array: np.ndarray,
    output_profiles: dict[str, np.ndarray],
    reference_time: np.ndarray,
    reference_fima: np.ndarray,
    saved_paths: list[Path],
) -> None:
    """Paper figure: (a) phases at end of irradiation, (b) phases after the
    final cooldown, (c) temperature at both states, (d) O/M ratio, on a
    shared R/Ro axis. Same colour + marker for a phase in (a) and (b)."""
    if TEMPERATURE_LABEL not in output_profiles or "O/M ratio (/)" not in output_profiles:
        return
    fima_end = float(np.interp(COOLDOWN_START, reference_time, reference_fima))
    snapshots = [
        (COOLDOWN_START, "before", f"(a) End of irradiation ({fima_end:.1f} % FIMA)"),
        (COOLDOWN_END, "after", "(b) After cooldown to 300 K"),
    ]
    fractions = [
        [radial_phase_fractions(case_dir, target_time, side) for case_dir in case_directories]
        for target_time, side, _ in snapshots
    ]
    if not any(point for snapshot in fractions for point in snapshot):
        return

    r_over_ro = radii_m_array / PELLET_RADIUS_M
    fig, axes = plt.subplots(
        4, 1, figsize=(10, 13.5), sharex=True,
        gridspec_kw={"height_ratios": [3.0, 3.0, 1.45, 1.2], "hspace": 0.24},
    )
    for index, axis in enumerate(axes):
        add_radial_point_regions(axis, len(r_over_ro), show_labels=index == 0)
        axis.tick_params(labelsize=16)

    present = {key for snapshot in fractions for point in snapshot for key in point}
    ordered = [key for key in RADIAL_PHASE_STYLE if key in present] + sorted(present - set(RADIAL_PHASE_STYLE))
    for axis, snapshot, (_, _, title) in zip(axes[:2], fractions, snapshots):
        for key in ordered:
            values = np.array([point.get(key, np.nan) for point in snapshot], dtype=float)
            if np.all(np.isnan(values)):
                continue
            label, color, marker, filled, dodge = RADIAL_PHASE_STYLE.get(
                key, (pretty_phase_name(key), PAPER_PALETTE[8], "h", False, 0.0)
            )
            x = r_over_ro + dodge
            axis.plot(x, values, color=color, linestyle=(0, (2.0, 2.5)), linewidth=1.2, zorder=2)
            axis.plot(
                x, values, linestyle="none", marker=marker,
                markersize=13 if marker == "*" else 10,
                markerfacecolor=color if filled else "white",
                markeredgecolor=tuple(0.7 * c for c in to_rgb(color)) if filled else color,
                markeredgewidth=1.0 if filled else 2.0,
                label=label, zorder=3,
            )
        axis.set_yscale("log")
        axis.set_ylim(1.0e-3, 4.0)
        axis.set_ylabel("Phase fraction (-)", fontsize=18)
        axis.set_title(title, loc="left", fontsize=18)
        # Legend in the empty band inward of point 1 (R/Ro < ~0.29).
        axis.legend(
            loc="center left", bbox_to_anchor=(0.0, 0.47), fontsize=14, frameon=False,
            handlelength=1.2, handletextpad=0.5, labelspacing=0.45, borderaxespad=0.3,
        )

    temperature_axis, om_axis = axes[2], axes[3]
    pre = snapshot_index(reference_time, COOLDOWN_START, "before")
    post = snapshot_index(reference_time, COOLDOWN_END, "after")
    for index, linestyle, text in ((pre, "-", "end of irradiation"), (post, (0, (5.0, 3.0)), "after cooldown")):
        temperature = output_profiles[TEMPERATURE_LABEL][:, index]
        temperature_axis.plot(
            r_over_ro, temperature, color=RADIAL_PROFILE_COLOR, linestyle=linestyle,
            linewidth=2.0, marker="o", markersize=6,
        )
        temperature_axis.annotate(
            text, (r_over_ro[0], temperature[0]), xytext=(-12, 0), textcoords="offset points",
            ha="right", va="center", fontsize=14, color="#3a3a3a",
        )
    temperature_axis.set_ylabel("T (K)", fontsize=18)
    temperature_axis.set_ylim(0.0, 2500.0)
    temperature_axis.set_yticks(np.arange(0.0, 2501.0, 500.0))
    temperature_axis.set_title("(c) Temperature", loc="left", fontsize=18)

    om_pre = output_profiles["O/M ratio (/)"][:, pre]
    om_post = output_profiles["O/M ratio (/)"][:, post]
    om_axis.plot(r_over_ro, om_pre, color=RADIAL_PROFILE_COLOR, linewidth=2.0, marker="o", markersize=6)
    if np.allclose(om_pre, om_post, atol=1.0e-5):
        om_title = "(d) O/M ratio (unchanged by the cooldown)"
    else:
        om_axis.plot(
            r_over_ro, om_post, color=RADIAL_PROFILE_COLOR, linestyle=(0, (5.0, 3.0)),
            linewidth=2.0, marker="o", markersize=6,
        )
        om_title = "(d) O/M ratio (solid: end of irradiation, dashed: after cooldown)"
    om_axis.set_ylabel("O/M (-)", fontsize=18)
    om_axis.set_ylim(1.98, 2.005)
    om_axis.set_yticks([1.98, 1.99, 2.00])
    om_axis.set_title(om_title, loc="left", fontsize=18)
    om_axis.set_xlabel("R/Ro (-)", fontsize=18)
    om_axis.set_xlim(0.0, 1.0)

    # No tight_layout (save_figure): it would override the panel spacing.
    output_path = PLOTS_DIR / "radial_phase_mole_fraction_pre_post_cooldown.png"
    output_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(output_path, bbox_inches="tight")
    plt.close(fig)
    saved_paths.append(output_path)


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

    # Only the post-cooldown pie is used in the paper (Fig. atomic_percent_pie_comparison).
    for snapshot_slug, snapshot_name, target_time, side in COOLDOWN_SNAPSHOTS:
        if snapshot_slug != "post_cooldown":
            continue
        fima_at_snapshot = float(np.interp(target_time, reference_time, reference_fima))
        snapshot_label = f"{snapshot_name} ({fima_at_snapshot:.1f} at.%)"
        plot_jog_composition_comparison_pies(
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
    # Paper figure (Fig. radial phase mole fraction): pre- and post-cooldown
    # in one figure; mid-irradiation stays in radial_phase_snapshots for the
    # text results summary below.
    plot_radial_phase_mole_fraction_pre_post(
        ordered_case_directories,
        radii_m_array,
        output_profiles,
        reference_time,
        reference_fima,
        saved_paths,
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
                    color="#eda100",
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
        jog_colors = JOG_PHASE_COLORS
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
    parser = argparse.ArgumentParser(
        description=(
            "Regenerate JOG paper figures and results_summary.txt from already-run "
            "validation/jog/test_PHENIXpins_point_* cases. Run the cases first with "
            "`./runTesting.sh --oc` (or `python3 -m testing.runner --oc`)."
        )
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

    case_directories = case_dirs()
    if not case_directories:
        raise FileNotFoundError(
            f"No test_PHENIXpins_point_* directories found in {CASES_DIR}. "
            "Run the JOG test cases first, e.g. `./runTesting.sh --oc`."
        )

    if args.runnode:
        if args.number is None:
            raise ValueError("--runnode requires --number")
        case_directories = filter_case_dirs(case_directories, args.number)

    case_directories = completed_case_dirs(case_directories)

    PLOTS_DIR.mkdir(parents=True, exist_ok=True)
    saved_paths: list[Path] = []

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
