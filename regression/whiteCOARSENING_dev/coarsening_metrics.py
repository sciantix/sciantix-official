#!/usr/bin/env python3
"""COARSENING: run/analyze White cases for Barani, Zullo, and Zullo-Nicodemo options."""

from __future__ import annotations

import argparse
import csv
import math
import os
import shutil
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path


# COARSENING: White/Barani figures of merit and SCIANTIX output columns.
METRICS = {
    "radius_m": {
        "expected": "ig_coarsening_radius.txt",
        "expected_factor": 1.0e-9,
        "coarsening_column": "Intragranular coarsened bubble radius (m)",
        "legacy_column": "Intragranular bubble radius (m)",
        "factor": 1.0,
        "label": "Bubble radius (m)",
        "limits": (1.0e-10, 1.0e-6),
    },
    "density_bub_m3": {
        "expected": "ig_coarsening_density.txt",
        "expected_factor": 1.0e19,
        "coarsening_column": "Intragranular coarsened bubble concentration (bub/m3)",
        "legacy_column": "Intragranular bubble concentration (bub/m3)",
        "factor": 1.0,
        "label": "Bubble concentration (bub/m3)",
        "limits": (1.0e17, 1.0e25),
    },
    "swelling_percent": {
        "expected": "ig_coarsening_swelling.txt",
        "expected_factor": 1.0,
        "coarsening_column": "Intragranular coarsened gas bubble swelling (/)",
        "legacy_column": "Intragranular gas bubble swelling (/)",
        "factor": 100.0,
        "label": "Intragranular swelling (%)",
        "limits": (1.0e-2, 1.0e2),
    },
}

LEGACY_STYLE = {"label": "SCIANTIX legacy", "color": "black", "marker": "o", "size": 18}


@dataclass(frozen=True)
class Variant:
    # COARSENING: compact variant descriptor for the dislocation-density and K_eff options.
    key: str
    label: str
    dislocation: int
    k_model: int = 0
    k0: float = 1.0e6
    tsat: float = 1500.0
    bsat: float = 20.0
    color: str = "#d62728"
    marker: str = "o"


BASE_VARIANTS = [
    Variant("barani_2019", "Barani 2019", 1, color="#d62728", marker="x"),
    Variant("zullo_2026", "Zullo 2026", 2, color="#2ca02c", marker="^"),
    Variant("zullo_nicodemo_2026", "Zullo - Nicodemo 2026", 3, color="#9467bd", marker="*"),
]

DEV_BASE_VARIANTS = [
    Variant("barani_2019", "Barani 2019", 1, color="#d62728", marker="x"),
    Variant("zullo_nicodemo_2026", "Zullo - Nicodemo 2026", 3, color="#9467bd", marker="*"),
]


def repo_root(root: Path) -> Path:
    return root.parents[1]


def executable(root: Path, requested: str | None) -> Path:
    # COARSENING: use the repository build by default, but allow explicit executable paths.
    return Path(requested).resolve() if requested else repo_root(root) / "build" / "sciantix.x"


def case_dirs(root: Path) -> list[Path]:
    return sorted(path for path in root.glob("test_*") if path.is_dir())


def load_expected(path: Path, factor: float = 1.0) -> dict[str, float]:
    values: dict[str, float] = {}
    with path.open() as stream:
        for line in stream:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            name, value = line.split()[:2]
            values[name] = float(value) * factor
    return values


def load_output(path: Path) -> tuple[list[str], list[float]]:
    lines = [line.strip() for line in path.read_text().splitlines() if line.strip()]
    if len(lines) < 2:
        raise RuntimeError(f"COARSENING: output file has no data rows: {path}")
    header = [item.strip() for item in lines[0].split("\t") if item.strip()]
    values = [float(item) for item in lines[-1].split("\t") if item.strip()]
    return header, values


def column_value(header: list[str], values: list[float], column: str) -> float:
    return values[header.index(column)]


def run_case(exe: Path, case: Path) -> None:
    # COARSENING: remove run artifacts before executing each copied White case.
    for artifact in ["output.txt", "execution.txt", "overview.txt", "input_check.txt"]:
        artifact_path = case / artifact
        if artifact_path.exists():
            artifact_path.unlink()
    subprocess.run([str(exe), str(case) + os.sep], check=True)


def set_optional_setting(path: Path, setting_name: str, value: int) -> None:
    # COARSENING: keep only the two supported optional switches in regression inputs.
    descriptions = {
        "iCoarseningDislocationDensity": (
            "(0= none, 1= Barani 2019, 2= Zullo 2026, 3= Zullo - Nicodemo 2026 COARSENING)"
        ),
        "iCoarseningKModel": "(0= Barani K0, 1= K0*fBu*fT COARSENING)",
    }
    replacement = f"{value}    #    {setting_name} {descriptions[setting_name]}"
    lines = [
        line
        for line in path.read_text().splitlines()
        if "iCoarseningDislocationDensity" not in line and "iCoarseningKModel" not in line
    ]
    if setting_name == "iCoarseningDislocationDensity":
        lines.append(replacement)
        lines.append(f"0    #    iCoarseningKModel {descriptions['iCoarseningKModel']}")
    else:
        lines.append(f"1    #    iCoarseningDislocationDensity {descriptions['iCoarseningDislocationDensity']}")
        lines.append(replacement)
    path.write_text("\n".join(lines) + "\n")


def apply_variant(case: Path, variant: Variant) -> None:
    # COARSENING: apply dislocation-density law and optional K_eff scaling factors.
    settings = case / "input_settings.txt"
    lines = [
        line
        for line in settings.read_text().splitlines()
        if "iCoarseningDislocationDensity" not in line and "iCoarseningKModel" not in line
    ]
    lines.append(
        f"{variant.dislocation}    #    iCoarseningDislocationDensity "
        "(0= none, 1= Barani 2019, 2= Zullo 2026, 3= Zullo - Nicodemo 2026 COARSENING)"
    )
    lines.append(f"{variant.k_model}    #    iCoarseningKModel (0= Barani K0, 1= K0*fBu*fT COARSENING)")
    settings.write_text("\n".join(lines) + "\n")

    scaling_lines = [
        "1.0    # sf_resolution_rate",
        "1.0    # sf_trapping_rate",
        "1.0    # sf_nucleation_rate",
        "1.0    # sf_diffusivity",
        "1.0    # sf_diffusivity2",
        "1.0    # sf_temperature",
        "1.0    # sf_fission_rate",
        "1.0    # sf_helium_production_rate",
        "1.0    # sf_dummy",
        f"{variant.k0:.12g}    # sf_coarsening_k0 COARSENING",
        f"{variant.tsat:.12g}    # sf_coarsening_tsat COARSENING",
        f"{variant.bsat:.12g}    # sf_coarsening_bsat COARSENING",
    ]
    (case / "input_scaling_factors.txt").write_text("\n".join(scaling_lines) + "\n")


def collect_expected(root: Path) -> dict[str, dict[str, float]]:
    # COARSENING: gather the experimental White/Barani data by case and metric.
    expected_by_metric = {
        metric: load_expected(root / "data" / config["expected"], config["expected_factor"])
        for metric, config in METRICS.items()
    }
    case_names = sorted(set.intersection(*(set(values) for values in expected_by_metric.values())))
    return {
        case_name: {metric: expected_by_metric[metric][case_name] for metric in METRICS}
        for case_name in case_names
    }


def collect_legacy(root: Path, expected: dict[str, dict[str, float]]) -> dict[str, dict[str, float]]:
    # COARSENING: use the unchanged White gold outputs as the legacy SCIANTIX reference.
    legacy_root = root.parent / "white"
    rows: dict[str, dict[str, float]] = {}
    for case_name in expected:
        output = legacy_root / case_name / "output_gold.txt"
        if not output.exists():
            output = legacy_root / case_name / "output.txt"
        header, values = load_output(output)
        rows[case_name] = {}
        for metric, config in METRICS.items():
            rows[case_name][metric] = column_value(header, values, config["legacy_column"]) * config["factor"]
    return rows


def run_variant(root: Path, exe: Path, variant: Variant, expected: dict[str, dict[str, float]]) -> dict[str, dict[str, float]]:
    # COARSENING: run one dislocation/K_eff option in temporary cases so the regression folder stays clean.
    rows: dict[str, dict[str, float]] = {}
    with tempfile.TemporaryDirectory(prefix=f"{variant.key}_", dir=root) as temporary:
        temp_root = Path(temporary)
        for source_case in case_dirs(root):
            if source_case.name not in expected:
                continue
            case = temp_root / source_case.name
            shutil.copytree(source_case, case)
            apply_variant(case, variant)
            run_case(exe, case)
            header, values = load_output(case / "output.txt")
            rows[source_case.name] = {
                metric: column_value(header, values, config["coarsening_column"]) * config["factor"]
                for metric, config in METRICS.items()
            }
            rows[source_case.name]["intergranular_swelling_percent"] = (
                column_value(header, values, "Intergranular gas swelling (/)") * 100.0
            )
            rows[source_case.name]["dislocation_density"] = column_value(header, values, "Dislocation density (m/m3)")
            rows[source_case.name]["temperature"] = column_value(header, values, "Temperature (K)")
            rows[source_case.name]["burnup"] = column_value(header, values, "Burnup (MWd/kgUO2)")
            rows[source_case.name]["k_eff"] = column_value(header, values, "Coarsening bubbles per dislocation (bub/m)")
    return rows


def run_default_regression(root: Path, exe: Path) -> None:
    # COARSENING: execute the folder's default settings, useful for standard regression use.
    for case in case_dirs(root):
        run_case(exe, case)


def clean_figures(root: Path) -> None:
    # COARSENING: remove stale development figures before writing the current regression set.
    figures = root / "figures"
    if figures.exists():
        for figure in figures.glob("*.png"):
            figure.unlink()
    figures.mkdir(exist_ok=True)


def write_rows(root: Path,
               expected: dict[str, dict[str, float]],
               legacy: dict[str, dict[str, float]],
               variant_rows: dict[str, dict[str, dict[str, float]]]) -> None:
    # COARSENING: tabulate all model variants used to build the figures.
    output = root / "coarsening_metrics.csv"
    fieldnames = ["case", "metric", "experimental", "legacy"] + list(variant_rows)
    with output.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fieldnames)
        writer.writeheader()
        for case_name in expected:
            for metric in METRICS:
                row = {
                    "case": case_name,
                    "metric": metric,
                    "experimental": expected[case_name][metric],
                    "legacy": legacy[case_name][metric],
                }
                for key, rows in variant_rows.items():
                    row[key] = rows[case_name][metric]
                writer.writerow(row)


def positive_pairs(x_values: list[float], y_values: list[float]) -> tuple[list[float], list[float]]:
    # COARSENING: log-scale parity plots cannot show zero-valued points.
    pairs = [(x, y) for x, y in zip(x_values, y_values) if x > 0.0 and y > 0.0]
    return [x for x, _ in pairs], [y for _, y in pairs]


def parity_axes(ax, limits: tuple[float, float], label: str) -> None:
    from matplotlib.ticker import FixedLocator, LogFormatterMathtext

    lower, upper = limits
    ax.set_xscale("log")
    ax.set_yscale("log")
    ax.plot([lower, upper], [lower, upper], color="0.55", linewidth=1.0, linestyle="--", label="Parity")
    ax.set_xlim(lower, upper)
    ax.set_ylim(lower, upper)
    ticks = []
    tick = lower
    while tick <= upper * 1.0001:
        ticks.append(tick)
        tick *= 10.0
    ax.xaxis.set_major_locator(FixedLocator(ticks))
    ax.yaxis.set_major_locator(FixedLocator(ticks))
    ax.xaxis.set_major_formatter(LogFormatterMathtext())
    ax.yaxis.set_major_formatter(LogFormatterMathtext())
    ax.set_aspect("equal", adjustable="box")
    ax.set_xlabel(f"Experimental {label}")
    ax.set_ylabel(f"Calculated {label}")
    ax.grid(True, color="0.88", linewidth=0.8)


def plot_metric_set(root: Path,
                    expected: dict[str, dict[str, float]],
                    legacy: dict[str, dict[str, float]],
                    variant_rows: dict[str, dict[str, dict[str, float]]],
                    variants: list[Variant],
                    prefix: str,
                    include_legacy: bool = True) -> None:
    # COARSENING: make Barani-only and all-law parity plot groups.
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    figures = root / "figures"
    figures.mkdir(exist_ok=True)
    for metric, config in METRICS.items():
        exp_values = [expected[case][metric] for case in expected]
        fig, ax = plt.subplots(figsize=(5.2, 5.2), constrained_layout=True)
        if include_legacy:
            x_values, y_values = positive_pairs(exp_values, [legacy[case][metric] for case in expected])
            ax.scatter(
                x_values,
                y_values,
                color=LEGACY_STYLE["color"],
                s=LEGACY_STYLE["size"],
                marker=LEGACY_STYLE["marker"],
                label=LEGACY_STYLE["label"],
            )
        for variant in variants:
            rows = variant_rows[variant.key]
            x_values, y_values = positive_pairs(exp_values, [rows[case][metric] for case in expected])
            ax.scatter(x_values, y_values, color=variant.color, s=22, marker=variant.marker, label=variant.label)
        parity_axes(ax, config["limits"], config["label"])
        ax.set_title(config["label"])
        ax.legend(frameon=False, loc="best")
        fig.savefig(figures / f"{prefix}_{metric}.png", dpi=300)
        plt.close(fig)


def plot_intergranular(root: Path,
                       barani_rows: dict[str, dict[str, float]],
                       legacy_root: Path | None = None) -> None:
    # COARSENING: parity for intergranular swelling, legacy White gold vs Barani model 4.
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    legacy_root = legacy_root or root.parent / "white"
    measured = load_expected(legacy_root / "data" / "ig_swelling.txt", 1.0)
    cases = [case for case in sorted(measured) if case in barani_rows and (legacy_root / case).exists()]
    legacy_values = []
    barani_values = []
    measured_values = []
    for case in cases:
        output = legacy_root / case / "output_gold.txt"
        if not output.exists():
            output = legacy_root / case / "output.txt"
        header, values = load_output(output)
        measured_values.append(measured[case])
        legacy_values.append(column_value(header, values, "Intergranular gas swelling (/)") * 100.0)
        barani_values.append(barani_rows[case]["intergranular_swelling_percent"])

    figures = root / "figures"
    figures.mkdir(exist_ok=True)
    fig, ax = plt.subplots(figsize=(5.2, 5.2), constrained_layout=True)
    x_values, y_values = positive_pairs(measured_values, legacy_values)
    ax.scatter(x_values, y_values, color="black", s=18, marker="o", label="SCIANTIX legacy")
    x_values, y_values = positive_pairs(measured_values, barani_values)
    ax.scatter(x_values, y_values, color="#d62728", s=22, marker="x", label="Barani 2019")
    parity_axes(ax, (1.0e-2, 1.0e2), "Intergranular swelling (%)")
    ax.set_title("Intergranular swelling (%)")
    ax.legend(frameon=False, loc="best")
    fig.savefig(figures / "parity_intergranular_swelling_legacy_barani.png", dpi=300)
    plt.close(fig)


def plot_dislocation_history(root: Path, variant_rows: dict[str, dict[str, dict[str, float]]], variants: list[Variant]) -> None:
    # COARSENING: show how the three dislocation laws respond to White final temperature and burnup.
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    figures = root / "figures"
    figures.mkdir(exist_ok=True)
    for x_key, x_label, name in [
        ("temperature", "Final temperature (K)", "dislocation_density_vs_temperature.png"),
        ("burnup", "Final burnup (MWd/kgUO2)", "dislocation_density_vs_burnup.png"),
    ]:
        fig, ax = plt.subplots(figsize=(6.2, 4.4), constrained_layout=True)
        for variant in variants:
            rows = variant_rows[variant.key]
            x_values = [rows[case][x_key] for case in sorted(rows)]
            y_values = [max(rows[case]["dislocation_density"], 1.0) for case in sorted(rows)]
            ax.scatter(x_values, y_values, color=variant.color, s=24, marker=variant.marker, label=variant.label)
        ax.set_yscale("log")
        ax.set_xlabel(x_label)
        ax.set_ylabel("Dislocation density (m/m3)")
        ax.grid(True, color="0.88", linewidth=0.8)
        ax.legend(frameon=False, loc="best")
        fig.savefig(figures / name, dpi=300)
        plt.close(fig)


def metric_mape(expected: dict[str, dict[str, float]], rows: dict[str, dict[str, float]], metric: str) -> float:
    # COARSENING: MAPE used for the compact K_eff calibration summary.
    pairs = [(expected[case][metric], rows[case][metric]) for case in expected if expected[case][metric] > 0.0]
    return 100.0 * sum(abs(calc - exp) / exp for exp, calc in pairs) / len(pairs)


def calibration_variants() -> list[Variant]:
    # COARSENING: small K0, Tsat, Bsat grid guided by White history correlations.
    variants: list[Variant] = []
    for k0 in [2.5e5, 5.0e5, 1.0e6]:
        for tsat in [1350.0, 1650.0]:
            for bsat in [8.0, 32.0]:
                key = f"keff_k{k0:.0e}_t{tsat:.0f}_b{bsat:.0f}".replace("+", "")
                label = f"K0={k0:.1e}, Tsat={tsat:.0f}, Bsat={bsat:.0f}"
                variants.append(
                    Variant(key, label, 3, k_model=1, k0=k0, tsat=tsat, bsat=bsat, color="#ff7f0e", marker="P")
                )
    return variants


def run_calibration(root: Path,
                    exe: Path,
                    expected: dict[str, dict[str, float]],
                    legacy: dict[str, dict[str, float]]) -> None:
    # COARSENING: calibrate the history-weighted K_eff = K0*fBu*fT in whiteCOARSENING_dev.
    base_rows = {variant.key: run_variant(root, exe, variant, expected) for variant in DEV_BASE_VARIANTS}
    summary = []
    grid_rows: dict[str, dict[str, dict[str, float]]] = {}
    for variant in calibration_variants():
        rows = run_variant(root, exe, variant, expected)
        grid_rows[variant.key] = rows
        radius_mape = metric_mape(expected, rows, "radius_m")
        density_mape = metric_mape(expected, rows, "density_bub_m3")
        swelling_mape = metric_mape(expected, rows, "swelling_percent")
        # COARSENING: objective weights low-swelling correction while keeping radius and density honest.
        objective = swelling_mape + 0.5 * density_mape + 0.5 * radius_mape
        summary.append((objective, swelling_mape, density_mape, radius_mape, variant))

    summary.sort(key=lambda item: item[0])
    with (root / "keff_calibration_summary.csv").open("w", newline="") as stream:
        writer = csv.writer(stream)
        writer.writerow(["objective", "swelling_mape", "density_mape", "radius_mape", "K0", "Tsat", "Bsat", "variant"])
        for objective, swelling_mape, density_mape, radius_mape, variant in summary:
            writer.writerow([objective, swelling_mape, density_mape, radius_mape, variant.k0, variant.tsat, variant.bsat, variant.key])

    best = summary[0][4]
    best_variant = Variant(
        "best_keff",
        f"Zullo - Nicodemo + K(Bu,T): K0={best.k0:.1e}, Tsat={best.tsat:.0f}, Bsat={best.bsat:.0f}",
        3,
        k_model=1,
        k0=best.k0,
        tsat=best.tsat,
        bsat=best.bsat,
        color="#ff7f0e",
        marker="P",
    )
    variant_rows = dict(base_rows)
    variant_rows[best_variant.key] = grid_rows[best.key]
    write_rows(root, expected, legacy, variant_rows)
    plot_metric_set(root, expected, legacy, variant_rows, [*DEV_BASE_VARIANTS, best_variant], "parity_dev_calibrated")
    print(
        "COARSENING best K_eff:",
        f"K0={best.k0:.3g}",
        f"Tsat={best.tsat:.0f}",
        f"Bsat={best.bsat:.0f}",
        f"objective={summary[0][0]:.3g}",
    )


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--exe", default=None, help="COARSENING: path to sciantix.x")
    parser.add_argument("--run", action="store_true", help="COARSENING: run default folder cases before plotting")
    parser.add_argument("--calibrate", action="store_true", help="COARSENING: run the K0/Tsat/Bsat grid")
    args = parser.parse_args()

    root = Path(__file__).resolve().parent
    exe = executable(root, args.exe)
    expected = collect_expected(root)
    legacy = collect_legacy(root, expected)
    clean_figures(root)

    if args.run:
        run_default_regression(root, exe)

    if root.name == "whiteCOARSENING_dev" or args.calibrate:
        run_calibration(root, exe, expected, legacy)
        return

    variant_rows = {variant.key: run_variant(root, exe, variant, expected) for variant in BASE_VARIANTS}
    write_rows(root, expected, legacy, variant_rows)
    plot_metric_set(root, expected, legacy, variant_rows, [BASE_VARIANTS[0]], "parity_legacy_barani")
    plot_metric_set(root, expected, legacy, variant_rows, BASE_VARIANTS, "parity_legacy_barani_zullo")
    plot_intergranular(root, variant_rows["barani_2019"])
    plot_dislocation_history(root, variant_rows, BASE_VARIANTS)


if __name__ == "__main__":
    main()
