#!/usr/bin/env python3
"""COARSENING: Baker parity plots and dislocation-density option comparisons."""

from __future__ import annotations

import argparse
import math
import os
import shutil
import subprocess
import tempfile
from pathlib import Path


METRICS = {
    "density_bub_m3": {
        "expected": "ig_density.txt",
        "column": "Intragranular bubble concentration (bub/m3)",
        "factor": 1.0,
        "expected_factor": 1.0,
        "label": "Bubble concentration (bub/m3)",
        "figure": "parity_bubble_number.png",
    },
    "radius_m": {
        "expected": "ig_radius.txt",
        "column": "Intragranular bubble radius (m)",
        "factor": 1.0,
        "expected_factor": 1.0,
        "label": "Bubble radius (m)",
        "figure": "parity_radius.png",
    },
    "swelling_percent": {
        "expected": "ig_swelling.txt",
        "column": "Intragranular gas bubble swelling (/)",
        "factor": 100.0,
        "expected_factor": 1.0,
        "label": "Intragranular swelling (%)",
        "figure": "parity_swelling.png",
    },
}

DISLOCATION_OPTIONS = {
    0: ("none", "COARSENING no dislocations", "#1f77b4", "s"),
    1: ("barani_2019", "Barani 2019", "#d62728", "o"),
    2: ("zullo_2026", "Zullo 2026", "#2ca02c", "^"),
    3: ("zullo_nicodemo_2026", "Zullo - Nicodemo 2026", "#9467bd", "D"),
}


def load_expected(path: Path) -> dict[str, float]:
    # COARSENING: Baker data keys are temperatures, mapped to the corresponding case suffix.
    values: dict[str, float] = {}
    with path.open() as stream:
        for line in stream:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            temperature, value = line.split()[:2]
            values[f"test_Baker1977__{temperature}K"] = float(value)
    return values


def load_output(path: Path) -> tuple[list[str], list[float]]:
    lines = [line.strip() for line in path.read_text().splitlines() if line.strip()]
    header = [item.strip() for item in lines[0].split("\t") if item.strip()]
    values = [float(item) for item in lines[-1].split("\t") if item.strip()]
    return header, values


def run_case(exe: Path, case: Path) -> None:
    # COARSENING: remove run artifacts before executing temporary Baker variants.
    for artifact in ["output.txt", "execution.txt", "overview.txt", "input_check.txt"]:
        artifact_path = case / artifact
        if artifact_path.exists():
            artifact_path.unlink()
    subprocess.run([str(exe), str(case) + os.sep], check=True)


def set_optional_setting(path: Path, setting_name: str, value: int) -> None:
    # COARSENING: switch only dislocation density in copied cases.
    lines = path.read_text().splitlines()
    replacement = (
        f"{value}    #    {setting_name} "
        "(0= none, 1= Barani 2019, 2= Zullo 2026, 3= Zullo - Nicodemo 2026 COARSENING)"
    )
    for index, line in enumerate(lines):
        if f"#    {setting_name}" in line:
            lines[index] = replacement
            break
    else:
        lines.append(replacement)
    path.write_text("\n".join(lines) + "\n")


def set_core_coarsening_options(path: Path, enabled: bool) -> None:
    # COARSENING: Baker dislocation-density variants use the same core coarsening settings as White; only the dislocation-density selector changes.
    lines = path.read_text().splitlines()
    resolution = 4
    trapping = 2
    nucleation = 2
    for index, line in enumerate(lines):
        if "#    iResolutionRate" in line:
            lines[index] = (
                f"{resolution}    #    iResolutionRate "
                "(0= constant value, 1= Turnbull (1971), 2= Losonen (2000), "
                "3= thermal resolution, Cognini et al. (2021), 4= Setyawan size-dependent re-solution COARSENING)"
            )
        elif "#    iTrappingRate" in line:
            lines[index] = (
                f"{trapping}    #    iTrappingRate "
                "(0= constant value, 1= Ham (1958), 2= Ham bulk + Barani dislocation trapping COARSENING)"
            )
        elif "#    iNucleationRate" in line:
            lines[index] = (
                f"{nucleation}    #    iNucleationRate "
                "(0= constant value, 1= Olander, Wongsawaeng (2006), "
                "2= Olander bulk + Barani dislocation nucleation COARSENING)"
            )
    path.write_text("\n".join(lines) + "\n")


def collect_rows(root: Path,
                 legacy_root: Path,
                 expected: dict[str, dict[str, float]],
                 case_names: list[str]) -> list[dict[str, float | str]]:
    # COARSENING: collect current BakerCoarsening outputs and legacy Baker outputs for measured parity plots.
    rows: list[dict[str, float | str]] = []
    for name in case_names:
        case = root / name
        legacy_case = legacy_root / name
        if not case.is_dir() or not legacy_case.is_dir():
            continue
        header, values = load_output(case / "output.txt")
        legacy_header, legacy_values = load_output(legacy_case / "output.txt")
        row: dict[str, float | str] = {"case": name}
        for metric, config in METRICS.items():
            experimental = expected[metric][name] * float(config["expected_factor"])
            column = str(config["column"])
            row[f"{metric}_exp"] = experimental
            row[f"{metric}_coarsening"] = values[header.index(column)] * float(config["factor"])
            row[f"{metric}_legacy"] = legacy_values[legacy_header.index(column)] * float(config["factor"])
        rows.append(row)
    return rows


def positive_pairs(x_values: list[float], y_values: list[float]) -> tuple[list[float], list[float]]:
    # COARSENING: log parity plots skip zero values.
    pairs = [(x, y) for x, y in zip(x_values, y_values) if x > 0.0 and y > 0.0]
    return [x for x, _ in pairs], [y for _, y in pairs]


def metric_limits(*series: list[float]) -> tuple[float, float]:
    # COARSENING: compact log limits around measured and calculated Baker data.
    values = [value for data in series for value in data if value > 0.0]
    if not values:
        return 1.0e-12, 1.0
    lower = 10.0 ** math.floor(math.log10(min(values)))
    upper = 10.0 ** math.ceil(math.log10(max(values)))
    if lower == upper:
        upper *= 10.0
    return lower, upper


def plot_parity(root: Path, rows: list[dict[str, float | str]]) -> None:
    # COARSENING: Baker measured-vs-calculated parity for legacy SCIANTIX and model 4.
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.ticker import LogFormatterMathtext

    figures = root / "figures"
    figures.mkdir(exist_ok=True)
    for metric, config in METRICS.items():
        exp = [float(row[f"{metric}_exp"]) for row in rows]
        coarsening = [float(row[f"{metric}_coarsening"]) for row in rows]
        legacy = [float(row[f"{metric}_legacy"]) for row in rows]
        lower, upper = metric_limits(exp, coarsening, legacy)
        legacy_x, legacy_y = positive_pairs(exp, legacy)
        coarsening_x, coarsening_y = positive_pairs(exp, coarsening)

        fig, ax = plt.subplots(figsize=(5.2, 5.2), constrained_layout=True)
        ax.scatter(legacy_x, legacy_y, color="black", s=18, marker="o", label="SCIANTIX legacy")
        ax.scatter(coarsening_x, coarsening_y, color="#d62728", s=18, marker="o", label="SCIANTIX COARSENING")
        ax.set_xscale("log")
        ax.set_yscale("log")
        ax.plot([lower, upper], [lower, upper], color="0.55", linewidth=1.0, linestyle="--", label="Parity")
        ax.set_xlim(lower, upper)
        ax.set_ylim(lower, upper)
        ax.xaxis.set_major_formatter(LogFormatterMathtext())
        ax.yaxis.set_major_formatter(LogFormatterMathtext())
        ax.set_aspect("equal", adjustable="box")
        ax.set_xlabel(f"Experimental {config['label']}")
        ax.set_ylabel(f"Calculated {config['label']}")
        ax.grid(True, color="0.88", linewidth=0.8)
        ax.legend(frameon=False, loc="best")
        ax.set_title(config["label"])
        fig.savefig(figures / str(config["figure"]), dpi=300)
        plt.close(fig)


def plot_dislocation_option_comparison(root: Path,
                                       legacy_root: Path,
                                       expected: dict[str, dict[str, float]],
                                       case_names: list[str],
                                       exe: Path) -> None:
    # COARSENING: compare Baker response for no/fixed/variable dislocation densities.
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.ticker import LogFormatterMathtext

    figures = root / "figures"
    figures.mkdir(exist_ok=True)
    legacy_rows: dict[str, list[tuple[float, float]]] = {name: [] for name in METRICS}
    variant_rows: dict[int, dict[str, list[tuple[float, float]]]] = {
        option: {name: [] for name in METRICS} for option in DISLOCATION_OPTIONS
    }

    for name in case_names:
        legacy_case = legacy_root / name
        if not legacy_case.is_dir():
            continue
        legacy_header, legacy_values = load_output(legacy_case / "output.txt")
        for metric, config in METRICS.items():
            experimental = expected[metric][name] * float(config["expected_factor"])
            column = str(config["column"])
            legacy_rows[metric].append((experimental, legacy_values[legacy_header.index(column)] * float(config["factor"])))

    with tempfile.TemporaryDirectory(prefix="sciantix_baker_coarsening_") as temp_dir:
        temp_root = Path(temp_dir)
        for option in DISLOCATION_OPTIONS:
            option_root = temp_root / f"dislocation_{option}"
            option_root.mkdir()
            for name in case_names:
                source_case = root / name
                if not source_case.is_dir():
                    continue
                case = option_root / name
                shutil.copytree(source_case, case)
                set_optional_setting(case / "input_settings.txt", "iCoarseningDislocationDensity", option)
                set_core_coarsening_options(case / "input_settings.txt", False)  # COARSENING: Baker comparisons keep White-like core coarsening settings fixed.
                run_case(exe, case)
                header, values = load_output(case / "output.txt")
                for metric, config in METRICS.items():
                    experimental = expected[metric][name] * float(config["expected_factor"])
                    column = str(config["column"])
                    calculated = values[header.index(column)] * float(config["factor"])
                    variant_rows[option][metric].append((experimental, calculated))

    for metric, config in METRICS.items():
        all_y = [[y for _, y in legacy_rows[metric]]]
        all_y.extend([[y for _, y in variant_rows[option][metric]] for option in DISLOCATION_OPTIONS])
        lower, upper = metric_limits([x for x, _ in legacy_rows[metric]], *all_y)
        fig, ax = plt.subplots(figsize=(5.2, 5.2), constrained_layout=True)
        legacy_pairs = [(x, y) for x, y in legacy_rows[metric] if x > 0.0 and y > 0.0]
        ax.scatter([x for x, _ in legacy_pairs],
                   [y for _, y in legacy_pairs],
                   color="black",
                   s=14,
                   marker="o",
                   label="SCIANTIX legacy")
        for option, (_, label, color, marker) in DISLOCATION_OPTIONS.items():
            pairs = [(x, y) for x, y in variant_rows[option][metric] if x > 0.0 and y > 0.0]
            ax.scatter([x for x, _ in pairs], [y for _, y in pairs], color=color, s=18, marker=marker, label=label)
        ax.set_xscale("log")
        ax.set_yscale("log")
        ax.plot([lower, upper], [lower, upper], color="0.55", linewidth=1.0, linestyle="--", label="Parity")
        ax.set_xlim(lower, upper)
        ax.set_ylim(lower, upper)
        ax.xaxis.set_major_formatter(LogFormatterMathtext())
        ax.yaxis.set_major_formatter(LogFormatterMathtext())
        ax.set_aspect("equal", adjustable="box")
        ax.set_xlabel(f"Experimental {config['label']}")
        ax.set_ylabel(f"Calculated {config['label']}")
        ax.grid(True, color="0.88", linewidth=0.8)
        ax.legend(frameon=False, loc="best")
        ax.set_title(f"{config['label']} - dislocation options")
        fig.savefig(figures / f"dislocation_options_{metric}.png", dpi=300)
        plt.close(fig)


def rmse(pairs: list[tuple[float, float]]) -> float:
    return math.sqrt(sum((calc - exp) ** 2 for exp, calc in pairs) / len(pairs)) if pairs else 0.0


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--figures", action="store_true", help="COARSENING: write Baker parity plots.")
    parser.add_argument("--dislocation-variants",
                        action="store_true",
                        help="COARSENING: run and plot dislocation-density options 0/1/2/3 in temporary cases.")
    parser.add_argument("--exe", default=None, help="Path to sciantix.x. Defaults to ../../build/sciantix.x.")
    args = parser.parse_args()

    root = Path(__file__).resolve().parent
    repo = root.parent.parent
    legacy_root = repo / "regression" / "baker"
    exe = Path(args.exe).resolve() if args.exe else repo / "build" / "sciantix.x"
    expected = {
        metric: load_expected(root / "data" / str(config["expected"])) for metric, config in METRICS.items()
    }
    case_names = sorted(set.intersection(*(set(values) for values in expected.values())))
    rows = collect_rows(root, legacy_root, expected, case_names)

    print("COARSENING Baker intragranular metrics")
    print(f"Cases analyzed: {len(rows)}")
    for metric in METRICS:
        coarsening_pairs = [(float(row[f"{metric}_exp"]), float(row[f"{metric}_coarsening"])) for row in rows]
        legacy_pairs = [(float(row[f"{metric}_exp"]), float(row[f"{metric}_legacy"])) for row in rows]
        print(
            f"{metric}: COARSENING RMSE={rmse(coarsening_pairs):.6g} | "
            f"legacy RMSE={rmse(legacy_pairs):.6g}"
        )

    if args.figures and rows:
        plot_parity(root, rows)
        if args.dislocation_variants:
            plot_dislocation_option_comparison(root, legacy_root, expected, case_names, exe)
        print(f"Figures: {root / 'figures'}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
