#!/usr/bin/env python3
"""Manual preprocessing step for the JOG PHENIXpins regression cases.

Regenerates regression/jog/test_PHENIXpins_point_*/input_history.txt (via OXIRED)
and the Cs-production entry of input_scaling_factors.txt (via CSRED), using the
preprocessing packages under utilities/preprocessing/. 
Not run automatically by the regression suite.
"""
import re
import subprocess
import sys
from pathlib import Path

import numpy as np

TEST_DIR = Path(__file__).resolve().parent
CASES_DIR = TEST_DIR.parent
REPO_ROOT = TEST_DIR.parents[2]
PREPROCESSING_DIR = REPO_ROOT / "utilities" / "preprocessing"
OXIRED_SCRIPT = PREPROCESSING_DIR / "oxired_lib" / "examples" / "PHENIXpins.py"
OXIRED_HISTORY_DIR = OXIRED_SCRIPT.parent / "PHENIXpins_history"
CSRED_LIB_DIR = PREPROCESSING_DIR / "csred_lib"

sys.path.insert(0, str(CSRED_LIB_DIR))

PELLET_RADIUS_M = 2.719e-3
# Index into the (non-comment) value lines of input_scaling_factors.txt.
# main's 11-slot layout ends with grain_boundary_energy, fabricated_porosity,
# cs_production (index 10).
CS_PRODUCTION_SCALING_FACTOR_INDEX = 10


def case_dirs() -> list[Path]:
    return sorted(path for path in CASES_DIR.glob("test_PHENIXpins_point_*") if path.is_dir())


def parse_radius_mm(case_dir: Path) -> float:
    match = re.search(r"_r_(\d+p\d+)mm$", case_dir.name)
    if not match:
        raise ValueError(f"Could not parse radius from case directory name: {case_dir.name}")
    return float(match.group(1).replace("p", "."))


def run_subprocess_or_raise(command: list[str], cwd: Path, step_name: str) -> None:
    completed = subprocess.run(command, cwd=cwd, capture_output=True, check=False)
    print(completed.stdout.decode("utf-8", errors="replace"), end="")
    if completed.returncode != 0:
        raise RuntimeError(
            f"{step_name} failed (returncode={completed.returncode}):\n"
            + completed.stderr.decode("utf-8", errors="replace")
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

    history_by_index: dict[int, Path] = {}
    for history_case_dir in history_case_dirs:
        match = re.match(r"point_(\d+)", history_case_dir.name)
        if match:
            history_by_index[int(match.group(1))] = history_case_dir

    for case_dir in case_directories:
        match = re.search(r"point_(\d+)", case_dir.name)
        history_case_dir = history_by_index.get(int(match.group(1))) if match else None
        if history_case_dir is None:
            available = ", ".join(path.name for path in history_case_dirs)
            raise FileNotFoundError(
                f"No OXIRED point_* history directory matches {case_dir.name}. "
                f"Available OXIRED directories: {available}"
            )

        source = history_case_dir / "input_history.txt"
        if not source.exists():
            raise FileNotFoundError(f"OXIRED did not produce {source}")
        (case_dir / "input_history.txt").write_text(source.read_text())


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

    r_inner = 0.8e-3  # central hole (Inspyre deliverable 7.3)

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
    """Compute per-radius Cs-production scaling factors with CSRED and apply them.

    (To also refresh CSRED's own diagnostic plot, separately run
    `python3 utilities/preprocessing/csred_lib/examples/PHENIXpins.py` -- it is
    not needed to compute the scaling factors applied here.)
    """
    print("Solving CSRED Cs-production scaling factors...", flush=True)
    radius, scaling_factor = solve_csred_cs_production_scaling_factors()
    for case_dir in case_directories:
        target_radius_m = parse_radius_mm(case_dir) * 1.0e-3
        nearest_index = int(np.argmin(np.abs(radius - target_radius_m)))
        update_scaling_factor_value(
            case_dir / "input_scaling_factors.txt",
            index=CS_PRODUCTION_SCALING_FACTOR_INDEX,
            value=float(scaling_factor[nearest_index]),
            comment="Cs production",
        )


def main() -> int:
    case_directories = case_dirs()
    if not case_directories:
        raise FileNotFoundError(f"No test_PHENIXpins_point_* directories found in {CASES_DIR}")

    generate_oxired_input_histories(case_directories)
    generate_csred_scaling_factors(case_directories)

    print(f"Updated input_history.txt and input_scaling_factors.txt for {len(case_directories)} cases.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
