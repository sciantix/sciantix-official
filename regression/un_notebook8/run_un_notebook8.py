#!/usr/bin/env python3
"""Standalone UN notebook-8 regression runner."""

from __future__ import annotations

import argparse
import csv
import math
import subprocess
import sys
from pathlib import Path


SUITE_DIR = Path(__file__).resolve().parent
REPO_ROOT = SUITE_DIR.parents[1]
CASES_DIR = SUITE_DIR / "cases"
REFERENCE_CSV = SUITE_DIR / "reference" / "python8_reference_points.csv"
RESULTS_DIR = SUITE_DIR / "results"
FIGURES_DIR = SUITE_DIR / "figures"

FISSION_RATE = 5.77e19
FUEL_DENSITY = 10600.0
GRAIN_RADIUS = 4.5e-6
U_ATOM_DENSITY = 2.364191e28

COMPARE_COLUMNS = [
    ("FIMA (%)", ["FIMA (%)"]),
    ("Intragranular bulk gas bubble swelling", ["Intragranular bulk gas bubble swelling (/)"]),
    ("Dislocation gas bubble swelling", ["Dislocation gas bubble swelling (/)"]),
    ("Intragranular gas bubble swelling", ["Intragranular gas bubble swelling (/)"]),
    ("Dislocation bubble radius", ["Dislocation bubble radius (m)"]),
    ("Dislocation bubble concentration", ["Dislocation bubble concentration (bub/m3)"]),
    ("Dislocation bubble pressure", ["Dislocation bubble pressure (MPa)"]),
    ("Dislocation bubble equilibrium pressure", ["Dislocation bubble equilibrium pressure (MPa)"]),
    ("UN grain-face gas", ["UN grain-face gas (at/m3)"]),
    ("UN released gas", ["UN released gas (at/m3)"]),
    ("UN fission gas release", ["UN fission gas release (/)"]),
    ("Grain-face fractional coverage", ["Grain-face fractional coverage (/)"]),
    ("UN bulk nucleation rate", ["UN bulk nucleation rate (bub/m3/s)"]),
]


POINT_CASES = [
    ("test_UN_T900_FIMA1p3", 900.0, 1.3),
    ("test_UN_T1200_FIMA1p3", 1200.0, 1.3),
    ("test_UN_T1600_FIMA1p3", 1600.0, 1.3),
    ("test_UN_T1800_FIMA1p3", 1800.0, 1.3),
    ("test_UN_T2000_FIMA1p3", 2000.0, 1.3),
    ("test_UN_T1600_FIMA1p1", 1600.0, 1.1),
    ("test_UN_T1600_FIMA3p2", 1600.0, 3.2),
]


def time_hours_from_fima(fima_percent: float) -> float:
    return fima_percent * U_ATOM_DENSITY / (FISSION_RATE * 3.6e5)


def write_text(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def settings_text() -> str:
    settings = [
        (0, "iGrainGrowth"),
        (11, "iFissionGasDiffusivity"),
        (4, "iDiffusionSolver"),
        (5, "iIntraGranularBubbleBehavior"),
        (4, "iResolutionRate"),
        (2, "iTrappingRate"),
        (1, "iNucleationRate"),
        (1, "iOutput"),
        (0, "iGrainBoundaryVacancyDiffusivity"),
        (0, "iGrainBoundaryBehaviour"),
        (0, "iGrainBoundaryMicroCracking"),
        (2, "iFuelMatrix"),
        (0, "iGrainBoundaryVenting"),
        (0, "iRadioactiveFissionGas"),
        (0, "iHelium"),
        (0, "iHeDiffusivity"),
        (0, "iGrainBoundarySweeping"),
        (0, "iHighBurnupStructureFormation"),
        (0, "iHighBurnupStructurePorosity"),
        (0, "iHeliumProductionRate"),
        (0, "iStoichiometryDeviation"),
        (0, "iBubbleDiffusivity"),
        (1, "iChromiumSolubility"),
        (0, "iDensification"),
        (1, "iReleaseMode"),
        (1, "iUNDislocationDensity"),
        (2, "iUNVacancyDiffusivity"),
        (1, "iUNInterGranularBehavior"),
    ]
    return "".join(f"{value}\n# {name}\n" for value, name in settings)


def initial_conditions_text() -> str:
    return f"""\
{GRAIN_RADIUS}
# initial grain radius (m)
0.0 0.0 0.0 0.0 0.0 0.0
# initial Xe (at/m3) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released
0.0 0.0 0.0 0.0 0.0 0.0
# initial Kr (at/m3) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released
0.0 0.0 0.0 0.0 0.0 0.0
# initial He (at/m3) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released
0.0 0.0
# initial intragranular bubble concentration (bub/m3), radius (m)
0.0
# initial fuel burn-up (MWd/kgUO2)
0.0
# initial fuel effective burn-up (MWd/kgUO2)
0.0
# initial irradiation time (h)
{FUEL_DENSITY}
# initial fuel density (kg/m3)
0.0 1.5 0.0 0.0 98.5
# initial U234 U235 U236 U237 U238 (% of heavy atoms) content
0.0 0.0 0.0 0.0 0.0 0.0 0.0
# initial Xe133 (at/m3) produced, intragranular, intragranular in solution, intragranular in bubbles, decayed, grain boundary, released
0.0 0.0 0.0 0.0 0.0 0.0 0.0
# initial Kr85m (at/m3) produced, intragranular, intragranular in solution, intragranular in bubbles, decayed, grain boundary, released
0.0
# initial fuel stoichiometry deviation (\\)
0.0
# initial chromium content (at%)
"""


def scaling_factors_text() -> str:
    names = [
        "sf_resolution_rate",
        "sf_trapping_rate",
        "sf_nucleation_rate",
        "sf_diffusivity",
        "sf_diffusivity2",
        "sf_temperature",
        "sf_fission_rate",
        "sf_helium_production_rate",
        "sf_dummy",
    ]
    return "".join(f"1.0\n# {name}\n" for name in names)


def history_text(temperature: float, fima_percent: float, points: int = 2) -> str:
    if points < 2:
        points = 2
    end_h = time_hours_from_fima(fima_percent)
    lines = []
    for index in range(points):
        t_h = end_h * index / (points - 1)
        lines.append(f"{t_h:.12e} {temperature:.6g} {FISSION_RATE:.12e} 0.0")
    return "\n".join(lines) + "\n"


def generate_case(case_name: str, temperature: float, fima_percent: float, history_points: int = 2) -> Path:
    case_dir = CASES_DIR / case_name
    case_dir.mkdir(parents=True, exist_ok=True)
    write_text(case_dir / "input_settings.txt", settings_text())
    write_text(case_dir / "input_initial_conditions.txt", initial_conditions_text())
    write_text(case_dir / "input_scaling_factors.txt", scaling_factors_text())
    write_text(case_dir / "input_history.txt", history_text(temperature, fima_percent, history_points))
    return case_dir


def read_table(path: Path) -> tuple[list[str], list[dict[str, float]]]:
    with path.open(newline="", encoding="utf-8") as handle:
        sample = handle.readline()
        handle.seek(0)
        delimiter = "\t" if "\t" in sample else ","
        reader = csv.DictReader(handle, delimiter=delimiter)
        rows = []
        for row in reader:
            parsed = {}
            for key, value in row.items():
                if key is None:
                    continue
                try:
                    parsed[key.strip()] = float(value)
                except (TypeError, ValueError):
                    parsed[key.strip()] = math.nan
            rows.append(parsed)
        return list(reader.fieldnames or []), rows


def find_value(row: dict[str, float], names: list[str]) -> tuple[float | None, str | None]:
    for name in names:
        if name in row:
            return row[name], name
    return None, None


def reference_rows() -> dict[str, dict[str, float]]:
    with REFERENCE_CSV.open(newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        return {row["case"]: row for row in reader}


def reference_value(row: dict[str, str], quantity: str, aliases: list[str]) -> tuple[float | None, str | None]:
    names = [quantity] + aliases
    for name in names:
        if name in row and row[name] != "":
            return float(row[name]), name
    return None, None


def rel_diff(value: float, reference: float) -> float:
    scale = max(abs(reference), 1.0e-300)
    return abs(value - reference) / scale


def compare_case(
    case_name: str,
    temperature: float,
    fima_percent: float,
    rel_tol: float,
    abs_tol: float,
    references: dict[str, dict[str, str]],
) -> list[dict[str, str]]:
    case_dir = CASES_DIR / case_name
    output_path = case_dir / "output.txt"
    _, output_rows = read_table(output_path)
    if not output_rows:
        raise RuntimeError(f"No output rows in {output_path}")
    last = output_rows[-1]
    reference = references.get(case_name, {})
    rows = []
    for quantity, aliases in COMPARE_COLUMNS:
        value, output_name = find_value(last, [quantity] + aliases)
        ref, reference_name = reference_value(reference, quantity, aliases)
        warning = ""
        status = "PASS"
        abs_error = math.nan
        rel_error = math.nan
        if value is None:
            status = "MISSING_OUTPUT"
            warning = f"missing SCIANTIX output column: {quantity}"
        elif ref is None:
            status = "MISSING_REFERENCE"
            warning = f"missing notebook-8 reference column: {quantity}"
        else:
            abs_error = abs(value - ref)
            rel_error = rel_diff(value, ref)
            if not math.isfinite(value):
                status = "FAIL"
                warning = "non-finite SCIANTIX value"
            elif abs_error > abs_tol and rel_error > rel_tol:
                status = "FAIL"
        rows.append(
            {
                "case": case_name,
                "T_K": f"{temperature:g}",
                "target_FIMA_percent": f"{fima_percent:g}",
                "quantity": quantity,
                "sciantix_column": output_name or "",
                "reference_column": reference_name or "",
                "sciantix": "" if value is None else f"{value:.16e}",
                "reference": "" if ref is None else f"{ref:.16e}",
                "abs_diff": "" if math.isnan(abs_error) else f"{abs_error:.16e}",
                "rel_diff": "" if math.isnan(rel_error) else f"{rel_error:.16e}",
                "status": status,
                "warning": warning,
            }
        )
    return rows


def run_sciantix(executable: Path, case_dir: Path) -> None:
    if not executable.is_file():
        raise FileNotFoundError(f"sciantix executable not found: {executable}")
    subprocess.run([str(executable), str(case_dir) + "/"], cwd=REPO_ROOT, check=True)


def write_summary(rows: list[dict[str, str]]) -> Path:
    RESULTS_DIR.mkdir(parents=True, exist_ok=True)
    path = RESULTS_DIR / "un_notebook8_summary.csv"
    fields = [
        "case",
        "T_K",
        "target_FIMA_percent",
        "quantity",
        "sciantix_column",
        "reference_column",
        "sciantix",
        "reference",
        "abs_diff",
        "rel_diff",
        "status",
        "warning",
    ]
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)
    return path


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--exe", default=str(REPO_ROOT / "build" / "sciantix.x"))
    parser.add_argument("--rel-tol", type=float, default=0.35)
    parser.add_argument("--abs-tol", type=float, default=1.0e-30)
    parser.add_argument("--strict", action="store_true", help="return non-zero when any comparison fails")
    parser.add_argument("--no-plot", action="store_true", help="skip figure generation")
    args = parser.parse_args()

    executable = Path(args.exe).resolve()
    references = reference_rows()
    all_rows = []

    for case_name, temperature, fima_percent in POINT_CASES:
        case_dir = generate_case(case_name, temperature, fima_percent)
        run_sciantix(executable, case_dir)
        all_rows.extend(compare_case(case_name, temperature, fima_percent, args.rel_tol, args.abs_tol, references))

    history_dir = generate_case("test_UN_history_T1600", 1600.0, 3.2, history_points=33)
    run_sciantix(executable, history_dir)

    summary = write_summary(all_rows)
    failures = [row for row in all_rows if row["status"] == "FAIL"]
    missing = [row for row in all_rows if row["status"].startswith("MISSING")]

    if not args.no_plot:
        try:
            import plot_un_notebook8

            plot_un_notebook8.main([])
        except Exception as exc:
            print(f"WARNING: plotting failed: {exc}", file=sys.stderr)

    print(f"Wrote {summary}")
    print(f"Compared {len(all_rows)} values: {len(failures)} fail, {len(missing)} missing.")
    if failures:
        print("First failures:")
        for row in failures[:10]:
            print(
                f"  {row['case']} {row['quantity']}: "
                f"SCIANTIX={row['sciantix']} reference={row['reference']} rel={row['rel_diff']}"
            )
    if missing:
        print("Warnings:")
        for row in missing[:10]:
            print(f"  {row['case']} {row['quantity']}: {row['warning']}")
    return 1 if args.strict and failures else 0


if __name__ == "__main__":
    raise SystemExit(main())

