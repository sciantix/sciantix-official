#!/usr/bin/env python3
import argparse
import csv
import math
import os
import re
import shutil
import subprocess
from collections import defaultdict
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib")

import matplotlib.pyplot as plt
import numpy as np
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
BURNUP_LABEL = "Burnup (MWd/kgUO2)"
BURNUP_UNIT = "MWd/kgMOX"
TIME_LABEL = "Time (h)"
FIMA_LABEL = "FIMA (%)"
COLUMNAR_GRAIN_R_OVER_RO_RANGE = (0.2, 0.75)
TOURASSE_OUTER_DIAMETER_UM = 5430.0
HCP_A3_COMPARISON_ELEMENTS = ("MO", "PD", "RH", "RU", "TC")

SHARED_INPUT_FILES = (
    "input_settings.txt",
    "input_initial_conditions.txt",
    "input_scaling_factors.txt",
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

SUMMARY_STACK_COLORS = PAPER_PALETTE
COLORS = PAPER_PALETTE
DISTINCT_COLOR_VALUES = PAPER_PALETTE
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


def secondary_time_axis(axis: plt.Axes, burnup: np.ndarray, time: np.ndarray, time_label: str) -> None:
    def burnup_to_time(x):
        return np.interp(x, burnup, time)

    def time_to_burnup(x):
        return np.interp(x, time, burnup)

    axis.secondary_xaxis("top", functions=(burnup_to_time, time_to_burnup)).set_xlabel(time_label)


def save_figure(fig: plt.Figure, path: Path, saved_paths: list[Path]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
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


def parse_radius_mm(case_dir: Path) -> float:
    match = re.search(r"_r_(\d+p\d+)mm$", case_dir.name)
    if not match:
        raise ValueError(f"Could not parse radius from case directory name: {case_dir.name}")
    return float(match.group(1).replace("p", "."))

def build_species_color_map(labels: list[str]) -> dict[str, object]:
    species_names = sorted({label.split(" (", 1)[0] for label in labels})
    if not species_names:
        return {}

    return {
        species: DISTINCT_COLOR_VALUES[index % len(DISTINCT_COLOR_VALUES)]
        for index, species in enumerate(species_names)
    }

def build_label_color_map(labels: list[str]) -> dict[str, object]:
    unique_labels = sorted(set(labels))
    if not unique_labels:
        return {}

    species_color_map = build_species_color_map(unique_labels)
    return {
        label: species_color_map[label.split(" (", 1)[0]]
        for label in unique_labels
    }


def assign_distinct_colors(labels: list[str], palette: list[str] = SUMMARY_STACK_COLORS) -> dict[str, object]:
    return {
        label: palette[index % len(palette)]
        for index, label in enumerate(labels)
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
        header not in {BURNUP_LABEL, TIME_LABEL}
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

    return build_label_color_map(gb_labels)


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


def hcp_a3_element_atomic_percent(case_dir: Path) -> tuple[float, dict[str, float]]:
    element_moles: dict[str, float] = defaultdict(float)
    rows = [
        row
        for row in load_phase_sublattice_rows(case_dir / PHASE_SUBLATTICE_OUTPUT_NAME)
        if row["location"] == "at grain boundary" and row["phase"] == "HCP_A3"
    ]
    if not rows:
        return 0.0, {}

    final_time = max(float(row["time"]) for row in rows)
    for row in rows:
        if not np.isclose(float(row["time"]), final_time):
            continue

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

    return final_time, normalize_atomic_percent(element_moles)


def outer_node_element_atomic_percent(case_dir: Path) -> tuple[float, dict[str, float]]:
    element_moles: dict[str, float] = defaultdict(float)
    rows = [
        row
        for row in load_phase_sublattice_rows(case_dir / PHASE_SUBLATTICE_OUTPUT_NAME)
        if row["location"] == "at grain boundary"
    ]
    if not rows:
        return 0.0, {}

    final_time = max(float(row["time"]) for row in rows)
    for row in rows:
        if not np.isclose(float(row["time"]), final_time):
            continue

        element = normalize_constituent_element(str(row["constituent"]))
        if element is None:
            continue

        constituent_moles = (
            float(row["phase_form_units"])
            * float(row["sites"])
            * float(row["site_fraction"])
        )
        if constituent_moles > 0.0:
            element_moles[element] += constituent_moles

    total_moles = sum(element_moles.values())
    if total_moles <= 0.0:
        return final_time, {}

    return final_time, {
        element: 100.0 * moles / total_moles
        for element, moles in element_moles.items()
        if moles > 0.0
    }


def plot_outer_node_atomic_percent_pie(
    case_dir: Path,
    saved_paths: list[Path],
    *,
    excluded_elements: set[str] | None = None,
    output_name: str = "outer_radial_node_atomic_percent_pie.svg",
    title_suffix: str = "",
) -> None:
    final_time, atomic_percent = outer_node_element_atomic_percent(case_dir)
    if not atomic_percent:
        return
    if excluded_elements:
        atomic_percent = {
            element: value
            for element, value in atomic_percent.items()
            if element.upper() not in excluded_elements
        }
        total = sum(atomic_percent.values())
        if total <= 0.0:
            return
        atomic_percent = {
            element: 100.0 * value / total
            for element, value in atomic_percent.items()
        }

    labels = sorted(atomic_percent, key=atomic_percent.get, reverse=True)
    values = [atomic_percent[label] for label in labels]
    colors = [
        DISTINCT_COLOR_VALUES[index % len(DISTINCT_COLOR_VALUES)]
        for index in range(len(labels))
    ]

    fig, axis = plt.subplots(figsize=(5,5), constrained_layout=True)
    outside_labels = ["" for _ in labels]
    wedges, texts = axis.pie(
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
        axis.text(
            radius * math.cos(theta),
            radius * math.sin(theta) + 0.05,
            element,
            ha="center",
            va="center",
            fontsize=14,
            fontweight="bold",
            color="#171717",
        )
        axis.text(
            radius * math.cos(theta),
            radius * math.sin(theta) - 0.05,
            f"{value:.0f}%",
            ha="center",
            va="center",
            fontsize=14,
            color="#171717",
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

    axis.set_title(
        f"Outer radial node at.%{title_suffix} at EOL",
        fontweight="bold",
        y=0.94,
        loc="center",
    )
    axis.set_aspect("equal")
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
    wedges, _ = axis.pie(
        values,
        labels=["" for _ in labels],
        colors=colors,
        startangle=20,
        counterclock=False,
        wedgeprops={"alpha": 0.75},
        radius=2.0,
    )

    for wedge, element, value in zip(wedges, labels, values):
        theta = math.radians((wedge.theta1 + wedge.theta2) / 2.0)
        radius = 0.73 if value >= 8.0 else 2.0
        axis.text(
            radius * math.cos(theta),
            radius * math.sin(theta) + (0.05 if value >= 8.0 else 0.0),
            element,
            ha="center",
            va="center",
            fontsize=12,
            fontweight="bold",
            color="#171717",
        )
        if value >= 8.0:
            axis.text(
                radius * math.cos(theta),
                radius * math.sin(theta) - 0.08,
                f"{value:.1f}%",
                ha="center",
                va="center",
                fontsize=11,
                color="#171717",
            )

    axis.set_title(title, fontsize=14, fontweight="bold", pad=12)
    axis.set_aspect("equal")


def plot_hcp_a3_eol_comparison_pies(
    case_directories: list[Path],
    radii_m_array: np.ndarray,
    saved_paths: list[Path],
) -> None:
    experimental_entries = load_experimental_hcp_a3_composition_data(
        EXP_DATA_DIR / "Samuelsson2020_HCP_A3.txt"
    )
    if not experimental_entries:
        return

    r_over_ro_array = radii_m_array / PELLET_RADIUS_M
    case_compositions: list[dict[str, float]] = []
    for case_dir in case_directories:
        _, composition = hcp_a3_element_atomic_percent(case_dir)
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
    all_positions = sorted({
        *(round(entry["r_over_ro"], 4) for entry in calc_entries),
        *(round(float(entry["r_over_ro"]), 4) for entry in experimental_entries),
    })
    n_columns = len(all_positions)
    fig, axes = plt.subplots(2, n_columns, figsize=(3.1 * n_columns,10))
    axes = np.atleast_2d(axes)

    for column, position in enumerate(all_positions):
        calc_entry = next(
            (entry for entry in calc_entries if np.isclose(entry["r_over_ro"], position)),
            None,
        )
        if calc_entry is None:
            axes[0, column].axis("off")
        else:
            add_atomic_percent_pie(
                axes[0, column],
                dict(calc_entry["composition"]),
                color_map,
                f"SCIANTIX\nR/Ro = {position:.2f}",
            )

        exp_entry = next(
            (entry for entry in experimental_entries if np.isclose(float(entry["r_over_ro"]), position)),
            None,
        )
        if exp_entry is None:
            axes[1, column].axis("off")
        else:
            add_atomic_percent_pie(
                axes[1, column],
                dict(exp_entry["composition"]),
                color_map,
                f"Experiment\nR/Ro = {position:.2f}",
            )

    fig.suptitle("White phase (HCP) composition at EOL", y=0.95, fontweight="bold")
    fig.subplots_adjust(wspace=0.02, hspace=0.02, left=0.03, right=0.99, top=0.90, bottom=0.06)
    save_figure(fig, PLOTS_DIR / "HCP_A3_EOL_comparison_pies.png", saved_paths)


def plot_phase_sublattice_composition(
    case_dir: Path,
    case_plot_dir: Path,
    burnup: np.ndarray,
    time: np.ndarray,
    saved_paths: list[Path],
) -> None:
    sublattice_file = case_dir / PHASE_SUBLATTICE_OUTPUT_NAME
    if not sublattice_file.exists():
        print(f"No {sublattice_file.relative_to(TEST_DIR)} found; skipping phase sublattice plots.")
        return

    rows = load_phase_sublattice_rows(sublattice_file)
    if not rows:
        print(f"No non-zero phase sublattice composition rows found in {sublattice_file.relative_to(TEST_DIR)}.")
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
                    axis.plot(plot_burnup, plot_fraction, label=constituent)

            sites = sublattice_rows[0]["sites"]
            axis.set_title(f"Sublattice {sublattice}, sites = {sites:g}")
            axis.set_ylabel("Site fraction (-)")
            axis.set_xlabel(BURNUP_LABEL)
            axis.set_xlim(0, max(burnup))
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

    burnup = values[:, columns[BURNUP_LABEL]]
    time = values[:, columns[TIME_LABEL]]

    thermochemistry_time = thermochemistry_values[:, thermochemistry_columns[TIME_LABEL]]
    thermochemistry_burnup = np.interp(thermochemistry_time, time, burnup)

    case_plot_dir = PLOTS_DIR / case_dir.name
    case_plot_dir.mkdir(parents=True, exist_ok=True)

    fig, axes = plt.subplots(1, 2, figsize=(10,5), sharex=True)
    axis = axes[0]
    if "O content (mol/m3)" in columns:
        axis.plot(burnup, values[:, columns["O content (mol/m3)"]], color=COLORS[0], label="Oxygen")
    axis.set_xlabel(BURNUP_LABEL)
    axis.set_xlim(0.0, max(burnup))
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

    fig, axes = plt.subplots(3, 3, figsize=(15, 15), sharex=True, sharey=True)
    axes = axes.flatten()
    for axis, species in zip(axes, fp_species):
        for label, color, suffix in [
            *fp_suffixes,
        ]:
            column_name = f"{species}{suffix}"
            if column_name in columns:
                axis.plot(burnup, values[:, columns[column_name]] / AVOGADRO_NUMBER, color=color, label=label, linewidth = 3)
        axis.set_title(species)
        axis.set_xlabel(BURNUP_LABEL)
        axis.set_ylabel("Inventory (mol m$^{-3}$)")
        axis.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))
        if fp_max > 0.0:
            axis.set_ylim(0.0, fp_max * 1.05)
        axis.set_xlim(0.0, max(burnup))
        if axis.get_legend_handles_labels()[0]:
            axis.legend(loc="upper left")
    save_figure(fig, case_plot_dir / "inventory_fission_products.png", saved_paths)

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

    gb_variables = [
        header
        for header in thermochemistry_headers
        if is_grain_boundary_amount_column(header)
        and not is_all_zero(thermochemistry_values[:, thermochemistry_columns[header]], atol=total_mass[-1]*0.001)
    ]
    gb_sorted_variables = sorted(
        gb_variables,
        key=lambda variable: thermochemistry_values[-1, thermochemistry_columns[variable]],
        reverse=True,
    )

    fig, axis = plt.subplots(1,1, figsize=(5+3,5))

    axis.plot(burnup, total_mass, color="black", label="Total mass")

    gb_stacked_data = [thermochemistry_values[:, thermochemistry_columns[variable]] for variable in gb_sorted_variables]
    gb_colors = evenly_spaced_colors(len(gb_sorted_variables))
    if gb_stacked_data:
        phase_hatch = {
            "gas": "...",
            "liquid": "///",
            "condensed": "xx",
            "unknown": "\\\\\\",
        }
        gb_labels = []
        gb_hatches = []
        for variable in gb_sorted_variables:
            species = grain_boundary_species(variable)
            phase = grain_boundary_phase(variable)
            if phase =="gas":
                gb_labels.append(f"{species} (g)")
            elif phase =="liquid":
                gb_labels.append("Liquid")
            else:
                gb_labels.append(f"{species}")
            gb_hatches.append(phase_hatch.get(phase, phase_hatch["unknown"]))

        polys = axis.stackplot(
            thermochemistry_burnup,
            gb_stacked_data,
            labels=gb_labels,
            colors=gb_colors,
        )
        for poly, hatch in zip(polys, gb_hatches):
            poly.set_hatch(hatch)
            poly.set_edgecolor((0.1, 0.1, 0.1, 0.7))
            poly.set_linewidth(0.1)

    axis.set_xlabel(BURNUP_LABEL)
    axis.set_xlim(0, max(burnup))
    axis.set_ylim(0, total_mass[-1]*1.05)
    axis.set_ylabel("Mass per fuel volume (g m$^{-3}$)")
    axis.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))
            

    handles, labels = axis.get_legend_handles_labels()
    axis.legend(handles, labels,  loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
    save_figure(fig, case_plot_dir / "thermochemistry.png", saved_paths)

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
        phase_inventory_histories.append(load_phase_sublattice_inventory(case_dir / PHASE_SUBLATTICE_OUTPUT_NAME))

    radii_mm_array = np.array(radii_mm, dtype=float)
    order = np.argsort(radii_mm_array)
    radii_mm_array = radii_mm_array[order]
    radii_m_array = radii_mm_array * 1.0e-3
    output_histories = [output_histories[index] for index in order]
    thermo_histories = [thermo_histories[index] for index in order]
    phase_inventory_histories = [phase_inventory_histories[index] for index in order]
    ordered_case_directories = [case_directories[index] for index in order]

    reference_time = output_histories[0]["Time (h)"]
    reference_burnup = output_histories[0][BURNUP_LABEL]
    reference_fima = output_histories[0]["FIMA (%)"]

    plot_outer_node_atomic_percent_pie(ordered_case_directories[-1], saved_paths)
    plot_outer_node_atomic_percent_pie(
        ordered_case_directories[-1],
        saved_paths,
        excluded_elements=METALLIC_ELEMENTS_EXCLUDED_FROM_FILTERED_PIE,
        output_name="outer_radial_node_atomic_percent_pie_without_ru_pd_rh_tc.svg",
        title_suffix=" w/o Ru, Pd, Rh, Tc",
    )
    plot_hcp_a3_eol_comparison_pies(ordered_case_directories, radii_m_array, saved_paths)

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

    snapshot_targets = np.linspace(
        float(np.nanmin(reference_burnup)),
        float(np.nanmax(reference_burnup)),
        5,
    )
    indexes: list[int] = []
    for target in snapshot_targets:
        index = int(np.argmin(np.abs(reference_burnup - target)))
        if index not in indexes:
            indexes.append(index)
    snapshot_colors = evenly_spaced_colors(len(indexes))
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
        tourasse_burnup = np.interp(tourasse_fima, reference_fima, reference_burnup)
        fayette_fmp_data = load_experimental_fmp_mo_ru_data(EXP_DATA_DIR / "Fayette2026_FMP.txt")
        fayette_fima, fayette_burnup_mo_ru = fayette_fmp_data["burnup"]
        fayette_burnup = np.interp(fayette_fima, reference_fima, reference_burnup)
        mo_hcp_over_ru_hcp = np.divide(
            mo_hcp_profile,
            ru_hcp_profile,
            out=np.zeros_like(mo_hcp_profile),
            where=ru_hcp_profile > 0.0,
        )
        if not is_all_zero(mo_hcp_over_ru_hcp):
            fig, axis = plt.subplots(1,1, figsize=(9,5))
            for color, index in zip(snapshot_colors, indexes):
                if is_all_zero(mo_hcp_over_ru_hcp[:, index]):
                    continue
                if reference_burnup[index]<1: 
                    continue
                axis.plot(
                    r_over_ro_array,
                    mo_hcp_over_ru_hcp[:, index],
                    color=color,
                    marker="o",
                    label=f"{reference_burnup[index]:.0f} {BURNUP_UNIT}",
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
            columnar_mo_ru = np.mean(mo_hcp_over_ru_hcp[columnar_region_mask, :], axis=0)
            axis.plot(
                reference_burnup,
                columnar_mo_ru,
                color=COLORS[0],
                label="This work",
            )
            axis.scatter(
                tourasse_burnup,
                tourasse_burnup_mo_ru,
                edgecolors=COLORS[7],
                facecolors="none",
                marker="D",
                label="Tourasse et al. (1992)",
                zorder=3,
                linewidths=1.6,
            )
            axis.scatter(
                fayette_burnup,
                fayette_burnup_mo_ru,
                edgecolors=COLORS[6],
                facecolors="none",
                marker="s",
                label="Fayette et al. (2025)",
                zorder=3,
                linewidths=1.6,
            )
            axis.hlines(21.9/19.8, 0, max(reference_burnup), color="black", label="Theoretical yield ratio")
            axis.set_xlabel(BURNUP_LABEL)
            axis.set_xlim(0, max(reference_burnup))
            axis.set_ylabel("Mo / Ru in FMP (-)")
            axis.set_ylim(0,1.25)
            axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
            save_figure(fig, PLOTS_DIR / "Mo_Ru_ratio_HCP_A3_burnup.png", saved_paths)

    if "Temperature (K)" in output_profiles:
        fig, axis = plt.subplots(1,1,figsize=(9,5))
        temperature_c_profiles = output_profiles["Temperature (K)"] - 273.15
        for color, index in zip(snapshot_colors, indexes):
            axis.plot(
                radii_mm_array*1e-3/PELLET_RADIUS_M,
                temperature_c_profiles[:, index],
                color=color,
                marker="o",
                label=f"{reference_burnup[index]:.0f} {BURNUP_UNIT}",
            )
        axis.set_xlabel("R/Ro")
        axis.set_ylabel("Temperature ($^\\circ$C)")
        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
        save_figure(fig, PLOTS_DIR / "Temperature.png", saved_paths)

    if "Stoichiometry deviation (/)" in output_profiles:
        fig, axis = plt.subplots(1,1,figsize=(9,5))
        for color, index in zip(snapshot_colors, indexes):
            axis.plot(
                radii_mm_array*1e-3/PELLET_RADIUS_M,
                2.0 + output_profiles["Stoichiometry deviation (/)"][:, index],
                color=color,
                marker="o",
                label=f"{reference_burnup[index]:.0f} {BURNUP_UNIT}",
            )
        axis.set_xlabel("R/Ro")
        axis.set_ylabel("Oxygen-to-Metal ratio (-)")
        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
        save_figure(fig, PLOTS_DIR / "StoichiometryRadial.png", saved_paths)

    if "Fission gas release (/)" in output_profiles:
        fig, axis = plt.subplots(1,1, figsize=(5+3,5))
        for radius_mm, case_history in zip(radii_mm_array, output_histories):
            case_burnup = case_history[BURNUP_LABEL]
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
        and not is_all_zero(thermo_profiles[header], 1)
    ]
    gb_sorted_variables = sorted(
        gb_variables,
        key=lambda variable: radial_volume_average(thermo_profiles[variable], radii_m_array)[-1],
        reverse=True,
    )
    
    if gb_sorted_variables:
        summary_entries: list[tuple[str, np.ndarray]] = []
        for variable in gb_sorted_variables:
            species = grain_boundary_species(variable)
            phase = grain_boundary_phase(variable)
            if phase == "gas": continue
            series = radial_volume_average(thermo_profiles[variable], radii_m_array)
            summary_entries.append((f"{species}", series))

        fig, axis = plt.subplots(1,1,figsize=(9,5))
        gb_labels = [label for label, _ in summary_entries]
        gb_radial_histories = [series for _, series in summary_entries]
        gb_colors = evenly_spaced_colors(len(gb_labels))

        if gb_radial_histories:
            axis.stackplot(
                reference_burnup,
                gb_radial_histories,
                colors=gb_colors,
                labels=gb_labels,
                alpha=0.9,
            )

            cumulative_histories = np.cumsum(np.vstack(gb_radial_histories), axis=0)
            for boundary in cumulative_histories:
                axis.plot(reference_burnup, boundary, color="#111827", linewidth=0.25, alpha=0.40)

        axis.set_xlabel(BURNUP_LABEL)
        axis.set_xlim(0, max(reference_burnup))
        axis.set_ylabel("Mass concentration (g m$^{-3}$)")
        handles, labels = axis.get_legend_handles_labels()
        axis.legend(handles, labels,loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
        axis.ticklabel_format(axis="y", style="sci", scilimits=(0, 0))
    
        save_figure(fig, PLOTS_DIR / "Thermochemistry_No_Gas.png", saved_paths)
    
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

        fig, axis = plt.subplots(1,1,figsize=(10,5))
        
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
        

def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--plot-only",
        action="store_true",
        help="Skip the SCIANTIX runs and regenerate plots from existing point outputs.",
    )
    args = parser.parse_args()

    case_directories = case_dirs()
    if not case_directories:
        raise FileNotFoundError(f"No point_* directories found in {TEST_DIR}")

    PLOTS_DIR.mkdir(exist_ok=True)
    saved_paths: list[Path] = []
    case_results: list[tuple[Path, int]] = []
    comparison_results: list[tuple[Path, bool, Path]] = []

    if not args.plot_only:
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

            case_saved_paths: list[Path] = []
            case_color_map = build_thermochemistry_color_map([case_dir])
            plot_case(case_dir, case_saved_paths, case_color_map)
            saved_paths.extend(case_saved_paths)
            print(
                f"Generated {len(case_saved_paths)} plots for {case_dir.name} "
                f"in {(PLOTS_DIR / case_dir.name).relative_to(TEST_DIR)}",
                flush=True,
            )
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

    gb_color_map = build_thermochemistry_color_map(case_directories)

    if args.plot_only:
        for case_dir in case_directories:
            plot_case(case_dir, saved_paths, gb_color_map)

    plot_radial_profiles(case_directories, saved_paths, gb_color_map)

    print("Generated plots:")
    for path in saved_paths:
        print(path)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
