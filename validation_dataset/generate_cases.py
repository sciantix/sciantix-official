from __future__ import annotations

import argparse
import shutil
import subprocess
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
DATA_DIR = ROOT / "validation_dataset" / "oxygenpotential"
TEMPLATE_DIR = ROOT / "regression" / "test_MOX_pO2_verification"
BUILD_BINARY = ROOT / "build" / "sciantix.x"

# Sources measured on irradiated or simulated high-burnup fuel (see ReadMe);
# they validate the models at burnup and live in a separate regression group.
BURNUP_SOURCES = {
    "Ewart1979a",
    "Ewart1979b",
    "Ewart1984",
    "Johnson1973",
    "Matzke1988",
    "Sato1997",
    "Tetenbaum1977",
    "Woodley1978",
}
FRESHFUEL_GROUP_DIR = ROOT / "regression" / "oxygenpotential_freshfuel"
BURNUP_GROUP_DIR = ROOT / "regression" / "oxygenpotential_burnup"


def group_dir_for(source_stem: str) -> Path:
    return BURNUP_GROUP_DIR if source_stem in BURNUP_SOURCES else FRESHFUEL_GROUP_DIR


COLUMN_MAP = {
    "Javed1972": (1, 2, 3, 6),
    "Johnson1973": (0, 1, 2, 5),
    "Ewart1979a": (0, 1, 2, 5),
    "Ewart1979b": (0, 1, 2, 5),
    "Ewart1984": (0, 1, 2, 5),
    "Hirooka2020": (0, 1, 4, 6),
    "Hirooka2022": (0, 1, -3, -1),
    "Nakamichi2011": (0, 1, 4, 6),
    "Osaka2005": (0, 1, 4, 7),
    "Sato1997": (None, 1, 2, 5),
    "Rao2006": (0, 1, 2, 5),
}


@dataclass(frozen=True)
class DataPoint:
    temperature: float
    pu_m_percent: float
    om_ratio: float
    oxygen_potential: float


def parse_number(token: str) -> float:
    return float(token.strip().replace("*", "").replace(",", "."))


def format_value(value: float) -> str:
    if abs(value - round(value)) < 1.0e-8:
        return str(int(round(value)))
    return f"{value:g}".replace(".", "p")


def data_lines(path: Path) -> list[list[str]]:
    rows = []
    for line in path.read_text(encoding="utf-8").splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("#") or stripped.startswith("Data source"):
            continue
        if not stripped[0].isdigit():
            continue
        tokens = stripped.split()
        if any(token.lower() == "invalid" for token in tokens):
            continue
        rows.append(tokens)
    return rows


def load_points(path: Path) -> list[DataPoint]:
    temperature_index, pu_index, om_index, mu_index = COLUMN_MAP.get(path.stem, (0, 1, 2, 4))
    points = []

    for tokens in data_lines(path):
        try:
            temperature = 1273.0 if temperature_index is None else parse_number(tokens[temperature_index])
            pu_m_percent = parse_number(tokens[pu_index])
            om_ratio = parse_number(tokens[om_index])
            oxygen_potential = parse_number(tokens[mu_index])
        except (IndexError, ValueError):
            continue

        points.append(DataPoint(temperature, pu_m_percent, om_ratio, oxygen_potential))

    if not points:
        raise ValueError(f"No usable data points found in {path}")
    return points


def grouped_points(points: list[DataPoint]) -> dict[tuple[float, float], list[DataPoint]]:
    groups = defaultdict(list)
    for point in points:
        groups[(point.temperature, point.pu_m_percent)].append(point)
    return dict(groups)


def write_settings(case_dir: Path) -> None:
    lines = (TEMPLATE_DIR / "input_settings.txt").read_text().splitlines()
    updated = []
    for line in lines:
        if "iStoichiometryDeviation" in line:
            updated.append("9    #    iStoichiometryDeviation (9= prescribed O/M history)")
        else:
            updated.append(line)
    (case_dir / "input_settings.txt").write_text("\n".join(updated) + "\n")


def write_initial_conditions(case_dir: Path, q: float, om_start: float) -> None:
    text = (TEMPLATE_DIR / "input_initial_conditions.txt").read_text()
    text = text.replace("__Q_VALUE__", f"{q:.8f}")
    # Overwrite the value line right above the stoichiometry-deviation comment,
    # whatever the template default is.
    lines = text.splitlines()
    for index, line in enumerate(lines):
        if "initial fuel stoichiometry deviation" in line and index > 0:
            lines[index - 1] = f"{om_start - 2.0:.8f}"
            break
    else:
        raise ValueError("Template is missing the initial stoichiometry deviation entry")
    (case_dir / "input_initial_conditions.txt").write_text("\n".join(lines) + "\n")


def write_history(case_dir: Path, temperature: float, om_start: float, om_end: float) -> None:
    text = (
        f"0\t{temperature:.8g}\t0\t0\t1e5\t{om_start:.8g}\n"
        f"1\t{temperature:.8g}\t0\t0\t1e5\t{om_end:.8g}\n"
    )
    (case_dir / "input_history.txt").write_text(text)


def write_experimental_subset(case_dir: Path, points: list[DataPoint]) -> None:
    lines = ["Temperature_K\tPu_M_percent\tO_M_ratio\tmu_O2_kJ_mol"]
    lines.extend(
        f"{point.temperature:.8g}\t{point.pu_m_percent:.8g}\t{point.om_ratio:.8g}\t{point.oxygen_potential:.8g}"
        for point in sorted(points, key=lambda item: item.om_ratio)
    )
    (case_dir / "experimental_subset.txt").write_text("\n".join(lines) + "\n")


def clean_case_dir(case_dir: Path) -> None:
    case_dir.mkdir(parents=True, exist_ok=True)
    for child in case_dir.iterdir():
        if child.name == "experimental_data.txt":
            continue
        if child.is_dir():
            shutil.rmtree(child)
        else:
            child.unlink()


def prepare_source(path: Path) -> list[Path]:
    points = load_points(path)
    source_dir = group_dir_for(path.stem) / f"test_{path.stem}"
    clean_case_dir(source_dir)
    shutil.copy2(path, source_dir / "experimental_data.txt")

    case_dirs = []
    for (temperature, pu_m_percent), subset in sorted(grouped_points(points).items()):
        q = pu_m_percent / 100.0
        om_values = [point.om_ratio for point in subset]
        om_start = min(om_values)
        om_end = max(om_values)

        case_dir = source_dir / f"T_{format_value(temperature)}K_q_{format_value(pu_m_percent)}"
        case_dir.mkdir(parents=True, exist_ok=True)

        shutil.copy2(TEMPLATE_DIR / "input_scaling_factors.txt", case_dir / "input_scaling_factors.txt")
        shutil.copy2(TEMPLATE_DIR / "input_thermochemistry.txt", case_dir / "input_thermochemistry.txt")
        shutil.copy2(TEMPLATE_DIR / "input_thermochemistry_settings.txt", case_dir / "input_thermochemistry_settings.txt")
        write_settings(case_dir)
        write_initial_conditions(case_dir, q, om_start)
        write_history(case_dir, temperature, om_start, om_end)
        write_experimental_subset(case_dir, subset)
        case_dirs.append(case_dir)

    return case_dirs


def split_files() -> list[Path]:
    return sorted(path for path in DATA_DIR.glob("*.txt") if path.name != "CompleteDataset.txt")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--write-gold", action="store_true", help="Run SCIANTIX and refresh output_gold.txt.")
    args = parser.parse_args()

    case_dirs = []
    for path in split_files():
        case_dirs.extend(prepare_source(path))

    if args.write_gold:
        if not BUILD_BINARY.exists():
            raise FileNotFoundError(f"Missing SCIANTIX binary: {BUILD_BINARY}")

        for case_dir in case_dirs:
            subprocess.run([str(BUILD_BINARY), "./"], cwd=case_dir, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=True)
            shutil.copy2(case_dir / "output.txt", case_dir / "output_gold.txt")

    print(f"Prepared {len(case_dirs)} oxygen-potential regression cases.")


if __name__ == "__main__":
    main()
