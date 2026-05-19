#!/usr/bin/env python3
import argparse
import csv
import os
import re
import shutil
import subprocess
from collections import defaultdict
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/tmp/matplotlib")

import matplotlib.pyplot as plt
import numpy as np

TEST_DIR = Path(__file__).resolve().parent
RUN_LOG = "sciantix.log"
BUILD_EXECUTABLE = TEST_DIR.parents[1] / "build" / "sciantix.x"
RUN_SUMMARY = TEST_DIR / "run_summary.txt"
MAIN_OUTPUT_NAME = "output.txt"
THERMO_OUTPUT_NAME = "thermochemistry_output.txt"
PHASE_SUBLATTICE_OUTPUT_NAME = "phase_sublattice_composition.txt"
THERMOCHEMISTRY_MANIFEST_FILE = TEST_DIR / "input_thermochemistry.txt"
PLOTS_DIR = TEST_DIR / "plots"
GOLD_DIR = TEST_DIR / "gold"
EXP_DATA_DIR = TEST_DIR / "exp_data"
PELLET_RADIUS_M = 2.7e-3
AVOGADRO_NUMBER = 6.02214076e23
BURNUP_LABEL = "Burnup (MWd/kgUO2)"
TIME_LABEL = "Time (h)"
FIMA_LABEL = "FIMA (%)"

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
MAX_STACKPLOT_LEGEND_ITEMS = 50
JOG_PHASES = [
    ("CS2MOO4_S2", "JOG from CS2MOO4_S2 (/)", "CS2MOO4_S2 (condensed, at grain boundary) (mol/m3)"),
    ("CS2MOO4_S1", "JOG from CS2MOO4_S1 (/)", "CS2MOO4_S1 (condensed, at grain boundary) (mol/m3)"),
    ("MOO2", "JOG from MOO2 (/)", "MOO2 (condensed, at grain boundary) (mol/m3)"),
    ("CS2MO3O10", "JOG from CS2MO3O10 (/)", "CS2MO3O10 (condensed, at grain boundary) (mol/m3)"),
    ("CS2MO4O13", "JOG from CS2MO4O13 (/)", "CS2MO4O13 (condensed, at grain boundary) (mol/m3)"),
    ("BCC_A2", "JOG from BCC_A2 (/)", "BCC_A2 (condensed, at grain boundary) (mol/m3)"),
    ("FCC_A1", "JOG from FCC_A1 (/)", "FCC_A1 (condensed, at grain boundary) (mol/m3)"),
    ("HCP_A3", "JOG from HCP_A3 (/)", "HCP_A3 (condensed, at grain boundary) (mol/m3)"),
]
SUMMARY_STACK_COLORS = [
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b",
    "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#aec7e8", "#ffbb78",
    "#98df8a", "#ff9896", "#c5b0d5", "#c49c94", "#f7b6d2", "#c7c7c7",
    "#dbdb8d", "#9edae5", "#393b79", "#637939", "#8c6d31", "#843c39",
]
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

COLORS = ["#ff0000", "#ff7f00", "#00c853", "#2962ff", "#aa00ff", "#8b5cf6"]
# Muted but still well-separated palette for species-consistent plots.
DISTINCT_COLOR_VALUES = [
    "#6baed6", "#fd8d3c", "#74c476", "#9e9ac8", "#e377c2", "#8c564b",
    "#17becf", "#bcbd22", "#c7a9d9", "#fdae6b", "#a1d99b", "#bcbddc",
    "#fdd0a2", "#9ecae1", "#c994c7", "#bdbdbd", "#66c2a5", "#fc8d62",
    "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3",
]


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


def is_all_zero(series: np.ndarray) -> bool:
    return np.allclose(series, 0.0)


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


def add_capped_legend(axis: plt.Axes, max_items: int = MAX_STACKPLOT_LEGEND_ITEMS, **kwargs) -> None:
    handles, labels = axis.get_legend_handles_labels()
    if not handles:
        return

    if len(handles) > max_items:
        handles = handles[:max_items]
        labels = labels[:max_items]
        labels[-1] = labels[-1] + " ..."

    axis.legend(handles, labels, **kwargs)


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


def run_sciantix_case(case_dir: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [str(BUILD_EXECUTABLE)],
        cwd=case_dir,
        text=True,
        capture_output=True,
        check=False,
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
            "mo_liquid_moo4": 0.0,
            "mo_solid_cs2moo4": 0.0,
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

        if location == "at grain boundary" and phase == "liquid" and "MOO4" in constituent.upper():
            time_inventory["mo_liquid_moo4"] += constituent_moles

        if (
            location == "at grain boundary"
            and phase.upper().startswith("CS2MOO4")
            and constituent.upper() == "MO"
        ):
            time_inventory["mo_solid_cs2moo4"] += constituent_moles

        if location == "at grain boundary" and phase == "HCP_A3":
            normalized_constituent = "MO" if constituent.upper() == "MO" else constituent.upper()
            time_inventory["hcp_constituents"][normalized_constituent] += constituent_moles
            if normalized_constituent == "MO":
                time_inventory["mo_hcp"] += constituent_moles
            if normalized_constituent == "RU":
                time_inventory["ru_hcp"] += constituent_moles

    return dict(inventory)


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
            len(sublattices),
            1,
            figsize=(12, 20),
            sharex=True,
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
                if not is_all_zero(plot_fraction): 
                    axis.plot(plot_burnup, plot_fraction, label=constituent)

            sites = sublattice_rows[0]["sites"]
            axis.set_ylabel(f"Sublattice = {sublattice}\nSite = {sites:g}\nSite fraction (-)")
            axis.set_ylim(-0.05, 1.05)
            axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)

        axes[-1].set_xlabel(BURNUP_LABEL)
        phase_label = display_phase_label(phase, phase_instance)
        fig.suptitle(f"{phase_label} ({location})", y=1.0)
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

    fig, axes = plt.subplots(5, 2, figsize=(12, 20))
    axes = axes.flatten()

    axis = axes[0]
    if "O content (mol/m3)" in columns:
        axis.plot(burnup, values[:, columns["O content (mol/m3)"]], color=COLORS[0], label="Oxygen")
    axis.set_xlabel(BURNUP_LABEL)
    axis.set_ylabel("Concentration (mol m$^{-3}$)")
    axis.legend(loc="upper left")

    axis = axes[1]
    if "U content (mol/m3)" in columns:
        axis.plot(burnup, values[:, columns["U content (mol/m3)"]], color=COLORS[0], label="Uranium")
    if "Pu content (mol/m3)" in columns:
        axis.plot(burnup, values[:, columns["Pu content (mol/m3)"]], color=COLORS[1], label="Plutonium")
    axis.set_xlabel(BURNUP_LABEL)
    axis.set_ylabel("Concentration (mol m$^{-3}$)")
    axis.legend(loc="upper left")

    for axis, species, ylabel in [
        (axes[2], "Xe", "Xe (mol m$^{-3}$)"),
        (axes[3], "Kr", "Kr (mol m$^{-3}$)"),
        (axes[4], "Cs", "Cs (mol m$^{-3}$)"),
        (axes[5], "Mo", "Mo (mol m$^{-3}$)"),
        (axes[6], "Pd", "Pd (mol m$^{-3}$)"),
        (axes[7], "Tc", "Tc (mol m$^{-3}$)"),
        (axes[8], "Rh", "Rh (mol m$^{-3}$)"),
        (axes[9], "Ru", "Ru (mol m$^{-3}$)"),
    ]:
        for label, color, suffix in [
            ("Produced", COLORS[0], " produced (at/m3)"),
            ("In grain", COLORS[1], " in grain (at/m3)"),
            ("At grain boundary", COLORS[2], " at grain boundary (at/m3)"),
            ("Reacted", COLORS[3], " reacted (at/m3)"),
            ("Released", COLORS[4], " released (at/m3)"),
            ("In solution", COLORS[5], " in solution (at/m3)"),
        ]:
            column_name = f"{species}{suffix}"
            if column_name in columns:
                axis.plot(burnup, values[:, columns[column_name]] / AVOGADRO_NUMBER, color=color, label=label)
        axis.set_xlabel(BURNUP_LABEL)
        axis.set_ylabel(ylabel)
        if axis.get_legend_handles_labels()[0]:
            axis.legend(loc="upper left")

    save_figure(fig, case_plot_dir / "inventory.png", saved_paths)

    if "Fuel oxygen potential (KJ/mol)" in columns:
        fig, axis = plt.subplots()
        axis.plot(burnup, values[:, columns["Fuel oxygen potential (KJ/mol)"]], label="SCIANTIX", color=COLORS[0])
        if "Fuel oxygen potential - Kato (KJ/mol)" in columns:
            axis.plot(burnup, values[:, columns["Fuel oxygen potential - Kato (KJ/mol)"]], label="Kato", color=COLORS[1], linestyle="--")
        if "Fuel oxygen potential - CALPHAD (KJ/mol)" in columns:
            axis.plot(burnup, values[:, columns["Fuel oxygen potential - CALPHAD (KJ/mol)"]], label="CALPHAD", color=COLORS[3], linestyle="--")
        axis.set_xlabel(BURNUP_LABEL)
        axis.set_ylabel("Fuel oxygen potential (KJ/mol O$_2$)")
        axis.legend(loc="best")
        save_figure(fig, case_plot_dir / "oxygenpotential.png", saved_paths)

    gb_variables = [
        header
        for header in thermochemistry_headers
        if is_grain_boundary_amount_column(header)
        and not is_all_zero(thermochemistry_values[:, thermochemistry_columns[header]])
    ]
    gb_sorted_variables = sorted(
        gb_variables,
        key=lambda variable: thermochemistry_values[-1, thermochemistry_columns[variable]],
        reverse=True,
    )
    fig, axis = plt.subplots()
    gb_stacked_data = [thermochemistry_values[:, thermochemistry_columns[variable]] for variable in gb_sorted_variables]
    gb_colors = [gb_color_map[variable] for variable in gb_sorted_variables]
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
            gb_labels.append(f"{species} ({phase})")
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
    axis.set_ylabel("Concentration (mol m$^{-3}$)")
    add_capped_legend(axis, loc="upper left", fontsize=8)
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

    reference_time = output_histories[0]["Time (h)"]
    reference_burnup = output_histories[0][BURNUP_LABEL]
    reference_fima = output_histories[0]["FIMA (%)"]

    output_profiles: dict[str, np.ndarray] = {}
    for name in output_histories[0]:
        aligned_series = []
        for case_history in output_histories:
            if name in case_history:
                aligned_series.append(np.interp(reference_time, case_history["Time (h)"], case_history[name]))
            else:
                aligned_series.append(np.zeros_like(reference_time))
        output_profiles[name] = np.vstack(aligned_series)

    thermo_profiles: dict[str, np.ndarray] = {}
    for name in thermo_histories[0]:
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
    mo_liquid_moo4_profile = aligned_inventory_profile("mo_liquid_moo4")
    mo_solid_cs2moo4_profile = aligned_inventory_profile("mo_solid_cs2moo4")

    snapshot_targets = np.linspace(
        float(np.nanmin(reference_burnup)),
        float(np.nanmax(reference_burnup)),
        6,
    )
    indexes: list[int] = []
    for target in snapshot_targets:
        index = int(np.argmin(np.abs(reference_burnup - target)))
        if index not in indexes:
            indexes.append(index)
    if indexes[-1] != len(reference_time) - 1:
        indexes.append(len(reference_time) - 1)
    snapshot_colors = plt.cm.viridis(np.linspace(0, 1, len(indexes)))

    hcp_plot_constituents = [
        constituent
        for constituent in hcp_constituents
        if constituent not in {"O", "VA"}
        and not is_all_zero(radial_volume_average(hcp_profiles[constituent], radii_m_array))
    ]
    if hcp_plot_constituents:
        hcp_integrated_histories = [
            radial_volume_average(hcp_profiles[constituent], radii_m_array)
            for constituent in hcp_plot_constituents
        ]
        hcp_total = np.sum(np.vstack(hcp_integrated_histories), axis=0)
        hcp_fractions = [
            np.divide(
                history,
                hcp_total,
                out=np.zeros_like(history),
                where=hcp_total > 0.0,
            )
            for history in hcp_integrated_histories
        ]
        hcp_labels = ["Mo" if label == "MO" else label for label in hcp_plot_constituents]
        hcp_colors = assign_distinct_colors(hcp_labels)

        fig, axis = plt.subplots()
        axis.stackplot(
            reference_burnup,
            *hcp_fractions,
            labels=hcp_labels,
            colors=[hcp_colors[label] for label in hcp_labels],
            alpha=0.9,
        )
        axis.set_xlabel(BURNUP_LABEL)
        axis.set_ylabel("HCP_A3 occupied-sublattice fraction (-)")
        axis.set_ylim(0.0, 1.0)
        axis.legend(loc="center left", bbox_to_anchor=(1.02, 0.5), ncol=1)
        save_figure(fig, PLOTS_DIR / "HCP_A3_sublattice_composition.png", saved_paths)

    if "Mo produced (at/m3)" in output_profiles:
        mo_produced_profile = output_profiles["Mo produced (at/m3)"] / AVOGADRO_NUMBER
        mo_oxide_profile = mo_liquid_moo4_profile + mo_solid_cs2moo4_profile
        mo_residual_profile = mo_produced_profile - mo_hcp_profile - mo_oxide_profile
        mo_residual_fraction = np.divide(
            mo_residual_profile,
            mo_produced_profile,
            out=np.zeros_like(mo_residual_profile),
            where=mo_produced_profile > 0.0,
        )
        if np.any(np.abs(mo_residual_fraction) > 0.01):
            print("Check residual")
        quantity_panels = [
            ("Mo produced", mo_produced_profile),
            ("Mo oxide in liquid MOO4", mo_liquid_moo4_profile),
            ("Mo oxide in solid CS2MOO4", mo_solid_cs2moo4_profile),
            ("Mo metal in HCP_A3", mo_hcp_profile),
            #("Mo residual", mo_residual_profile),
        ]
        fig, axes = plt.subplots(3, 2, figsize=(12, 13), sharex=True)
        axes = axes.flatten()
        for axis, (title, profile) in zip(axes, quantity_panels):
            for color, index in zip(snapshot_colors, indexes):
                axis.plot(
                    radii_mm_array,
                    profile[:, index],
                    color=color,
                    marker="o",
                    label=f"{reference_burnup[index]:.1f} MWd/kg$_{{MOX}}$",
                )
            axis.set_title(title)
            axis.set_ylabel("Concentration (mol m$^{-3}$)")
            if title != "Mo residual":
                axis.set_ylim(bottom=0.0)
        for axis in axes[len(quantity_panels):]:
            axis.set_visible(False)
        for axis in axes[-2:]:
            axis.set_xlabel("Radius (mm)")
        axes[0].legend(loc="best")
        save_figure(fig, PLOTS_DIR / "Mo_inventory_by_radius.png", saved_paths)

        mo_oxide_over_produced = np.divide(
            mo_solid_cs2moo4_profile,
            mo_produced_profile,
            out=np.zeros_like(mo_solid_cs2moo4_profile),
            where=mo_produced_profile > 0.0,
        )
        fig, axis = plt.subplots()
        for color, index in zip(snapshot_colors, indexes):
            if is_all_zero(mo_produced_profile[:, index]):
                continue
            axis.plot(
                radii_mm_array,
                mo_oxide_over_produced[:, index],
                color=color,
                marker="o",
                label=f"{reference_burnup[index]:.1f} MWd/kg$_{{MOX}}$",
            )
        axis.set_xlabel("Radius (mm)")
        axis.set_ylabel("Mo in CS2MOO4 / Mo produced (-)")
        axis.set_ylim(bottom=0.0)
        axis.legend(loc="best")
        save_figure(fig, PLOTS_DIR / "Mo_oxide_fraction_by_radius.png", saved_paths)

        mo_metal_over_produced = np.divide(
            mo_hcp_profile,
            mo_produced_profile,
            out=np.zeros_like(mo_hcp_profile),
            where=mo_produced_profile > 0.0,
        )
        fig, axis = plt.subplots()
        for color, index in zip(snapshot_colors, indexes):
            if is_all_zero(mo_produced_profile[:, index]):
                continue
            axis.plot(
                radii_mm_array,
                mo_metal_over_produced[:, index],
                color=color,
                marker="o",
                label=f"{reference_burnup[index]:.1f} MWd/kg$_{{MOX}}$",
            )
        axis.set_xlabel("Radius (mm)")
        axis.set_ylabel("Mo in HCP_A3 / Mo produced (-)")
        axis.set_ylim(bottom=0.0)
        axis.legend(loc="best")
        save_figure(fig, PLOTS_DIR / "Mo_metal_fraction_by_radius.png", saved_paths)

        mo_hcp_over_ru_hcp = np.divide(
            mo_hcp_profile,
            ru_hcp_profile,
            out=np.zeros_like(mo_hcp_profile),
            where=ru_hcp_profile > 0.0,
        )
        if not is_all_zero(mo_hcp_over_ru_hcp):
            fig, axis = plt.subplots()
            for color, index in zip(snapshot_colors, indexes):
                if is_all_zero(mo_hcp_over_ru_hcp[:, index]):
                    continue
                axis.plot(
                    radii_mm_array,
                    mo_hcp_over_ru_hcp[:, index],
                    color=color,
                    marker="o",
                    label=f"{reference_burnup[index]:.1f} MWd/kg$_{{MOX}}$",
                )
            axis.set_xlabel("Radius (mm)")
            axis.set_ylabel("Mo / Ru in HCP_A3 (-)")
            axis.set_ylim(bottom=0.0)
            axis.legend(loc="upper left")
            save_figure(fig, PLOTS_DIR / "Mo_Ru_ratio_HCP_A3.png", saved_paths)

    if "Temperature (K)" in output_profiles:
        fig, axis = plt.subplots()
        for color, index in zip(snapshot_colors, indexes):
            axis.plot(
                radii_mm_array,
                output_profiles["Temperature (K)"][:, index],
                color=color,
                marker="o",
                label=f"{reference_burnup[index]:.1f} MWd/kg$_{{MOX}}$",
            )
        axis.set_xlabel("Radius (mm)")
        axis.set_xlim([0, 3.0])
        axis.set_ylabel("Temperature (K)")
        axis.set_ylim([800, 2000])
        axis.legend(loc="best")
        save_figure(fig, PLOTS_DIR / "Temperature.png", saved_paths)

    if "System pressure (Pa)" in output_profiles:
        fig, axis = plt.subplots()
        for radius_mm, case_history in zip(radii_mm_array, output_histories):
            case_burnup = case_history[BURNUP_LABEL]
            pressure_bar = case_history["System pressure (Pa)"] * 1.0e-5
            axis.plot(case_burnup, pressure_bar, label=f"r = {radius_mm:.3f} mm", alpha=0.9)
        axis.set_xlabel("Burnup (MWd/kg$_{MOX}$)")
        axis.set_ylim([0,100])
        axis.set_ylabel("Pressure (bar)")
        axis.legend(loc="upper left")
        save_figure(fig, PLOTS_DIR / "Pressure.png", saved_paths)

    if "Stoichiometry deviation (/)" in output_profiles:
        fig, axis = plt.subplots()
        for radius_mm, case_history in zip(radii_mm_array, output_histories):
            case_burnup = case_history[BURNUP_LABEL]
            axis.plot(
                case_burnup,
                2.0 + case_history["Stoichiometry deviation (/)"],
                label=f"r = {radius_mm:.1f} mm",
                alpha=0.9,
            )
        axis.set_xlabel("Burnup (MWd/kg$_{MOX}$)")
        axis.set_ylabel("Oxygen-to-Metal ratio (-)")
        axis.legend(loc="lower right")
        save_figure(fig, PLOTS_DIR / "Stoichiometry.png", saved_paths)

        fig, axis = plt.subplots()
        for color, index in zip(snapshot_colors, indexes):
            axis.plot(
                radii_mm_array,
                2.0 + output_profiles["Stoichiometry deviation (/)"][:, index],
                color=color,
                marker="o",
                label=f"{reference_burnup[index]:.1f} MWd/kg$_{{MOX}}$",
            )
        axis.set_xlabel("Radius (mm)")
        axis.set_xlim([0, 3.0])
        axis.set_ylabel("Oxygen-to-Metal ratio (-)")
        axis.legend(loc="lower right")
        save_figure(fig, PLOTS_DIR / "StoichiometryRadial.png", saved_paths)

    if "Fission gas release (/)" in output_profiles:
        fgr_radial_average = radial_volume_average(
            output_profiles["Fission gas release (/)"],
            radii_m_array,
        )

        fig, axis = plt.subplots()
        for radius_mm, case_history in zip(radii_mm_array, output_histories):
            case_burnup = case_history[BURNUP_LABEL]
            axis.plot(
                case_burnup,
                case_history["Fission gas release (/)"],
                label=f"r = {radius_mm:.1f} mm",
                alpha=0.9,
            )
        axis.plot(
            reference_burnup,
            fgr_radial_average,
            color="#111827",
            linewidth=2.5,
            label="Radial average",
        )
        axis.set_xlabel("Burnup (MWd/kg$_{MOX}$)")
        axis.set_ylabel("Fission Gas Release (/)")
        axis.legend(loc="upper left")
        save_figure(fig, PLOTS_DIR / "FGR.png", saved_paths)

    if "q (-)" in output_profiles:
        fig, axis = plt.subplots()
        for radius_mm, case_history in zip(radii_mm_array, output_histories):
            case_burnup = case_history[BURNUP_LABEL]
            axis.plot(
                case_burnup,
                case_history["q (-)"],
                label=f"r = {radius_mm:.1f} mm",
                alpha=0.9,
            )
        axis.set_xlabel("Burnup (MWd/kg$_{MOX}$)")
        axis.set_ylabel("Pu / (U  + Pu)")
        axis.set_ylim(0.20, 0.24)
        axis.legend(loc="upper left")
        save_figure(fig, PLOTS_DIR / "Qratio.png", saved_paths)

    if "Fuel oxygen potential (KJ/mol)" in output_profiles:
        fig, axis = plt.subplots()
        for radius_mm, case_history in zip(radii_mm_array, output_histories):
            case_burnup = case_history[BURNUP_LABEL]
            axis.plot(
                case_burnup,
                case_history["Fuel oxygen potential (KJ/mol)"],
                label=f"r = {radius_mm:.1f} mm",
                alpha=0.9,
            )
        axis.set_xlabel("Burnup (MWd/kg$_{MOX}$)")
        axis.set_ylabel("Fuel oxygen potential (KJ/mol O$_2$)")
        axis.legend(loc="upper left")
        save_figure(fig, PLOTS_DIR / "oxygenpotential.png", saved_paths)

        fig, axis = plt.subplots()
        for radius_mm, case_history in zip(radii_mm_array, output_histories):
            case_burnup = case_history[BURNUP_LABEL]
            axis.plot(
                case_burnup,
                case_history["Fuel oxygen potential (KJ/mol)"] / 2.0,
                label=f"r = {radius_mm:.1f} mm",
                alpha=0.9,
            )
        axis.set_xlabel("Burnup (MWd/kg$_{MOX}$)")
        axis.set_ylabel("Oxygen potential (KJ/mol O)")
        axis.legend(loc="upper left")
        save_figure(fig, PLOTS_DIR / "oxygenpotential_2.png", saved_paths)

    gb_variables = [
        header
        for header in thermo_profiles
        if is_grain_boundary_amount_column(header)
        and not is_all_zero(thermo_profiles[header])
    ]
    gb_sorted_variables = sorted(
        gb_variables,
        key=lambda variable: radial_volume_average(thermo_profiles[variable], radii_m_array)[-1],
        reverse=True,
    )
    species_colors: dict[str, object] = {}

    if gb_sorted_variables:
        species_colors = build_species_color_map(gb_sorted_variables)
        summary_entries: list[tuple[str, np.ndarray]] = []
        for variable in gb_sorted_variables:
            species = grain_boundary_species(variable)
            phase = grain_boundary_phase(variable)
            if phase not in {"condensed", "liquid"}:
                continue
            series = radial_volume_average(thermo_profiles[variable], radii_m_array)
            summary_entries.append((f"{species}", series))

        fig, axis = plt.subplots()
        gb_labels = [label for label, _ in summary_entries]
        gb_radial_histories = [series for _, series in summary_entries]
        summary_colors = assign_distinct_colors(gb_labels)
        gb_colors = [summary_colors[label] for label in gb_labels]

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

        axis.set_xlabel("Burnup (MWd/kg$_{MOX}$)")
        axis.set_ylabel("Concentration (mol m$^{-3}$)")
        add_capped_legend(axis, loc="upper left")
        save_figure(fig, PLOTS_DIR / "Thermochemistry_No_Gas.png", saved_paths)
    
    if "JOG (/)" in output_profiles:
        jog_total_thickness_over_time_um = radial_integral_over_radius(
            output_profiles["JOG (/)"],
            radii_m_array,
        ) * 1.0e6

        jog_liquid_thickness_over_time_um = None
        if "JOG from liquid (/)" in output_profiles:
            jog_liquid_thickness_over_time_um = radial_integral_over_radius(
                output_profiles["JOG from liquid (/)"],
                radii_m_array,
            ) * 1.0e6
        melis_fima, melis_thickness = load_experimental_jog_data(EXP_DATA_DIR / "Melis1993.txt")
        tourasse_fima, tourasse_thickness = load_experimental_jog_data(EXP_DATA_DIR / "Tourasse1992.txt")

        def fima_to_burnup(fima_values: np.ndarray) -> np.ndarray:
            return np.interp(fima_values, reference_fima, reference_burnup)

        condensed_contribution_columns = [
            ("CS2MOO4_S2", "JOG from CS2MOO4_S2 (/)"),
            ("CS2MOO4_S1", "JOG from CS2MOO4_S1 (/)"),
            ("MOO2", "JOG from MOO2 (/)"),
            ("CS2MO3O10", "JOG from CS2MO3O10 (/)"),
            ("CS2MO4O13", "JOG from CS2MO4O13 (/)"),
            ("BCC_A2", "JOG from BCC_A2 (/)"),
            ("FCC_A1", "JOG from FCC_A1 (/)"),
            ("HCP_A3", "JOG from HCP_A3 (/)"),
        ]
        condensed_entries: list[tuple[str, np.ndarray, object]] = []
        for index, (label, column_name) in enumerate(condensed_contribution_columns):
            if column_name not in output_profiles:
                continue
            series = radial_integral_over_radius(output_profiles[column_name], radii_m_array) * 1.0e6
            if is_all_zero(series):
                continue
            condensed_entries.append((label, series, species_colors.get(label, plt.cm.tab20(index / max(1, len(condensed_contribution_columns))))))

        # Put CS2MOO4_S2 at the base inside the condensed stack ordering.
        condensed_entries.sort(key=lambda item: (item[0] != "CS2MOO4_S2", item[0]))
        gb_labels = []
        gb_radial_histories = []
        gb_colors = []

        gb_labels.extend(item[0] for item in condensed_entries)
        gb_radial_histories.extend(item[1] for item in condensed_entries)
        gb_colors.extend(item[2] for item in condensed_entries)

        # Keep liquid at the base of the stack when available.
        if jog_liquid_thickness_over_time_um is not None and not is_all_zero(jog_liquid_thickness_over_time_um):
            gb_labels.append("LIQUID")
            gb_radial_histories.append(jog_liquid_thickness_over_time_um)
            gb_colors.append("#f97316")

        fig, axis = plt.subplots()
        if gb_radial_histories:
            axis.stackplot(
                reference_burnup,
                *gb_radial_histories,
                colors=gb_colors,
                labels=gb_labels,
                alpha=0.9,
            )
            cumulative_histories = np.cumsum(np.vstack(gb_radial_histories), axis=0)
            for boundary in cumulative_histories:
                axis.plot(reference_burnup, boundary, color="#111827", linewidth=0.25, alpha=0.40)

        axis.plot(reference_burnup, jog_total_thickness_over_time_um, color="#111827", label="Total")
        axis.scatter(
            fima_to_burnup(melis_fima),
            melis_thickness,
            edgecolors="black", facecolor=None,
            marker="o",
            label="Melis et al. (1993)",
            zorder=3,
        )
        axis.scatter(
            fima_to_burnup(tourasse_fima),
            tourasse_thickness,
            edgecolors="black", facecolor=None,
            marker="D",
            label="Tourasse et al. (1992)",
            zorder=3,
        )
        axis.set_xlabel("Burnup (MWd/kg$_{MOX}$)")
        axis.set_ylabel("JOG thickness (um)")
        axis.legend(loc="upper left")
        save_figure(fig, PLOTS_DIR / "JOG.png", saved_paths)

        outer_indices = [
            index for index, radius_mm in enumerate(radii_mm_array)
            if radius_mm >=  2.3
        ]
        if len(outer_indices) >= 2:
            outer_jog_total_thickness_over_time_um = radial_integral_masked_to_full_radius(
                output_profiles["JOG (/)"],
                radii_m_array,
                outer_indices,
            ) * 1.0e6

            outer_jog_liquid_thickness_over_time_um = None
            if "JOG from liquid (/)" in output_profiles:
                outer_jog_liquid_thickness_over_time_um = radial_integral_masked_to_full_radius(
                    output_profiles["JOG from liquid (/)"],
                    radii_m_array,
                    outer_indices,
                ) * 1.0e6

            outer_entries: list[tuple[str, np.ndarray, object]] = []
            for index, (label, column_name) in enumerate(condensed_contribution_columns):
                if column_name not in output_profiles:
                    continue
                series = radial_integral_masked_to_full_radius(
                    output_profiles[column_name],
                    radii_m_array,
                    outer_indices,
                ) * 1.0e6
                if is_all_zero(series):
                    continue
                outer_entries.append((
                    label,
                    series,
                    species_colors.get(label, plt.cm.tab20(index / max(1, len(condensed_contribution_columns)))),
                ))

            outer_entries.sort(key=lambda item: (item[0] != "CS2MOO4_S2", item[0]))
            outer_labels = [item[0] for item in outer_entries]
            outer_histories = [item[1] for item in outer_entries]
            outer_colors = [item[2] for item in outer_entries]

            if outer_jog_liquid_thickness_over_time_um is not None and not is_all_zero(outer_jog_liquid_thickness_over_time_um):
                outer_labels.append("LIQUID")
                outer_histories.append(outer_jog_liquid_thickness_over_time_um)
                outer_colors.append("#f97316")

            fig, axis = plt.subplots()
            if outer_histories:
                axis.stackplot(
                    reference_burnup,
                    *outer_histories,
                    colors=outer_colors,
                    labels=outer_labels,
                    alpha=0.9,
                )
                cumulative_histories = np.cumsum(np.vstack(outer_histories), axis=0)
                for boundary in cumulative_histories:
                    axis.plot(reference_burnup, boundary, color="#111827", linewidth=0.25, alpha=0.40)

            axis.plot(reference_burnup, outer_jog_total_thickness_over_time_um, color="#111827", label="Total")
            axis.scatter(
                fima_to_burnup(melis_fima),
                melis_thickness,
                edgecolors="black", facecolor=None,
                marker="o",
                label="Melis et al. (1993)",
                zorder=3,
            )
            axis.scatter(
                fima_to_burnup(tourasse_fima),
                tourasse_thickness,
                edgecolors="black", facecolor=None,
                marker="D",
                label="Tourasse et al. (1992)",
                zorder=3,
            )
            axis.set_xlabel("Burnup (MWd/kg$_{MOX}$)")
            axis.set_ylabel("JOG thickness (um)")
            axis.legend(loc="upper left")
            save_figure(fig, PLOTS_DIR / "JOG_radii_18_20.png", saved_paths)
        

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
            RUN_LOG_case.write_text(completed.stdout + completed.stderr)
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
