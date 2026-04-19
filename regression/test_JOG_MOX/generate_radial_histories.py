#!/usr/bin/env python3
from __future__ import annotations

import json
from pathlib import Path


TEST_DIR = Path(__file__).resolve().parent
NOTEBOOK_PATH = TEST_DIR / "history.ipynb"
PELLET_RADIUS_MM = 2.719
OUTER_TEMPERATURE_K = 1000.0
END_OF_LIFE_FIMA = 0.1328
THEORETICAL_DENSITY_KG_M3 = 11159.0
RELATIVE_DENSITY = 0.9472
AVOGADRO = 6.02214076e23
OXYGEN_PER_METAL = 1.975
Q_PU = 0.288
OXYGEN_MASS_G_MOL = 15.999
U_MASS_G_MOL = 238.02891
PU_ISOTOPIC_FRACTIONS = {
    238: 0.013,
    239: 0.604,
    240: 0.234,
    241: 0.104,
    242: 0.045,
}
RADIAL_FRACTIONS = [0.0, 0.25, 0.50, 0.75, 1.0]
HISTORY_POINTS = 5


def parse_history_cell(cell_source: list[str]) -> list[tuple[float, float]]:
    points: list[tuple[float, float]] = []
    for raw_line in cell_source:
        line = raw_line.strip().strip('"')
        if not line or line.startswith("#"):
            continue
        if "," not in line:
            continue
        left, right = [item.strip() for item in line.split(",", 1)]
        points.append((float(left), float(right)))
    return points


def downsample_time_history(points: list[tuple[float, float]], target_count: int) -> list[tuple[float, float]]:
    if target_count < 2 or len(points) <= target_count:
        return points

    selected_indexes = {
        int(round(i * (len(points) - 1) / (target_count - 1)))
        for i in range(target_count)
    }
    ordered_indexes = sorted(selected_indexes)
    return [points[index] for index in ordered_indexes]


def load_notebook_histories() -> tuple[list[tuple[float, float]], list[tuple[float, float]]]:
    notebook = json.loads(NOTEBOOK_PATH.read_text())
    code_cells = [cell for cell in notebook["cells"] if cell.get("cell_type") == "code"]
    centerline_temperature_c = parse_history_cell(code_cells[0]["source"])
    pressure_bar_raw = parse_history_cell(code_cells[1]["source"])

    # The pressure notebook cell contains the early-irradiation points appended at the end.
    # We sort them back in time and keep the last value for repeated timestamps.
    pressure_map = {time_days: pressure_bar for time_days, pressure_bar in pressure_bar_raw}
    pressure_bar = sorted(pressure_map.items())
    centerline_temperature_c = downsample_time_history(centerline_temperature_c, HISTORY_POINTS)
    return centerline_temperature_c, pressure_bar


def estimate_uniform_fission_rate() -> float:
    average_pu_mass = sum(isotope * fraction for isotope, fraction in PU_ISOTOPIC_FRACTIONS.items())
    average_metal_mass = (1.0 - Q_PU) * U_MASS_G_MOL + Q_PU * average_pu_mass
    oxide_molar_mass = average_metal_mass + OXYGEN_PER_METAL * OXYGEN_MASS_G_MOL
    fuel_density = THEORETICAL_DENSITY_KG_M3 * RELATIVE_DENSITY
    oxide_moles_per_m3 = fuel_density / (oxide_molar_mass / 1000.0)
    heavy_metal_atoms_per_m3 = oxide_moles_per_m3 * AVOGADRO

    irradiation_seconds = max(time_days for time_days, _ in load_notebook_histories()[0]) * 24.0 * 3600.0
    return END_OF_LIFE_FIMA * heavy_metal_atoms_per_m3 / irradiation_seconds


def interpolate_pressure_bar(time_days: float, pressure_history_bar: list[tuple[float, float]]) -> float:
    if time_days <= pressure_history_bar[0][0]:
        return pressure_history_bar[0][1]
    if time_days >= pressure_history_bar[-1][0]:
        return pressure_history_bar[-1][1]

    for (t0, p0), (t1, p1) in zip(pressure_history_bar[:-1], pressure_history_bar[1:]):
        if t0 <= time_days <= t1:
            if t1 == t0:
                return p1
            weight = (time_days - t0) / (t1 - t0)
            return p0 + weight * (p1 - p0)

    return pressure_history_bar[-1][1]


def radial_temperature_k(centerline_temperature_c: float, radial_fraction: float) -> float:
    centerline_temperature_k = centerline_temperature_c + 273.15
    return OUTER_TEMPERATURE_K + (centerline_temperature_k - OUTER_TEMPERATURE_K) * (1.0 - radial_fraction**2)


def make_case_dir_name(index: int, radius_mm: float) -> str:
    return f"point_{index:02d}_r_{radius_mm:.4f}mm".replace(".", "p")


def write_input_history(
    case_dir: Path,
    centerline_temperature_c: list[tuple[float, float]],
    pressure_history_bar: list[tuple[float, float]],
    radial_fraction: float,
    fission_rate: float,
) -> None:
    case_dir.mkdir(exist_ok=True)
    lines: list[str] = []

    for time_days, centerline_temp_c in centerline_temperature_c:
        time_hours = time_days * 24.0
        temperature_k = radial_temperature_k(centerline_temp_c, radial_fraction)
        pressure_pa = interpolate_pressure_bar(time_days, pressure_history_bar) * 1.0e5
        hydrostatic_stress_mpa = 0.0
        lines.append(
            f"{time_hours:.4f}   {temperature_k:.6e}   {fission_rate:.6e}   "
            f"{hydrostatic_stress_mpa:.6e}   {pressure_pa:.6e}"
        )

    (case_dir / "input_history.txt").write_text("\n".join(lines) + "\n")


def main() -> None:
    centerline_temperature_c, pressure_history_bar = load_notebook_histories()
    fission_rate = estimate_uniform_fission_rate()

    for index, radial_fraction in enumerate(RADIAL_FRACTIONS, start=1):
        radius_mm = radial_fraction * PELLET_RADIUS_MM
        case_dir = TEST_DIR / make_case_dir_name(index, radius_mm)
        write_input_history(
            case_dir=case_dir,
            centerline_temperature_c=centerline_temperature_c,
            pressure_history_bar=pressure_history_bar,
            radial_fraction=radial_fraction,
            fission_rate=fission_rate,
        )


if __name__ == "__main__":
    main()
