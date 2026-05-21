#!/usr/bin/env python3
"""COARSENING: run/analyze White cases with the Barani two-size intragranular model."""

from __future__ import annotations

import argparse
import csv
import math
import os
import shutil
import subprocess
import tempfile
from pathlib import Path


# COARSENING: output columns introduced for the Barani et al. dislocation-bubble population and legacy columns used for parity plots.
METRICS = {
    "radius_m": {
        "expected": "ig_coarsening_radius.txt",
        "coarsening_column": "Intragranular coarsened bubble radius (m)",
        "legacy_column": "Intragranular bubble radius (m)",
        "factor": 1.0,
        "expected_factor": 1.0e-9,
        "label": "Bubble radius (m)",
        "figure": "parity_radius.png",
        "limits": (1.0e-10, 1.0e-6),
    },
    "density_bub_m3": {
        "expected": "ig_coarsening_density.txt",
        "coarsening_column": "Intragranular coarsened bubble concentration (bub/m3)",
        "legacy_column": "Intragranular bubble concentration (bub/m3)",
        "factor": 1.0,
        "expected_factor": 1.0e19,
        "label": "Bubble concentration (bub/m3)",
        "figure": "parity_bubble_number.png",
        "limits": (1.0e17, 1.0e25),
    },
    "swelling_percent": {
        "expected": "ig_coarsening_swelling.txt",
        "coarsening_column": "Intragranular coarsened gas bubble swelling (/)",
        "legacy_column": "Intragranular gas bubble swelling (/)",
        "factor": 100.0,
        "expected_factor": 1.0,
        "label": "Intragranular swelling (%)",
        "figure": "parity_swelling.png",
        "limits": (1.0e-2, 1.0e2),
    },
}

DISLOCATION_OPTIONS = {
    0: ("none", "COARSENING no dislocations", "#1f77b4", "s"),
    1: ("barani_2019", "Barani 2019", "#d62728", "o"),
    2: ("zullo_2026", "Zullo 2026", "#2ca02c", "^"),
    3: ("zullo_nicodemo_2026", "Zullo - Nicodemo 2026", "#9467bd", "D"),
}

# COARSENING: comparison set for the White development plots, adding the distributed Nicodemo 2026 population.
DISTRIBUTION_VARIANTS = {
    "barani_2019": {
        "dislocation": 1,
        "distribution": 0,
        "label": "Barani 2019",
        "color": "#d62728",
        "marker": "o",
    },
    "zullo_2026": {
        "dislocation": 2,
        "distribution": 0,
        "label": "Zullo 2026",
        "color": "#2ca02c",
        "marker": "^",
    },
    "zullo_nicodemo_2026": {
        "dislocation": 3,
        "distribution": 0,
        "label": "Zullo - Nicodemo 2026",
        "color": "#9467bd",
        "marker": "D",
    },
    "nicodemo_2026_distribution": {
        "dislocation": 3,
        "distribution": 1,
        "label": "Nicodemo 2026",
        "color": "#ff7f0e",
        "marker": "P",
    },
}

DISTRIBUTED_COLUMNS = {
    # COARSENING: distributed-population reporting columns used only by the Nicodemo 2026 variant.
    "density_bub_m3": "Intragranular distributed coarsened bubble concentration (bub/m3)",
    "radius_m": "Intragranular distributed coarsened bubble radius mean (m)",
}


def load_expected(path: Path) -> dict[str, float]:
    values: dict[str, float] = {}
    with path.open() as stream:
        for line in stream:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            name, value = line.split()[:2]
            values[name] = float(value)
    return values


def load_output(path: Path) -> tuple[list[str], list[float]]:
    lines = [line.strip() for line in path.read_text().splitlines() if line.strip()]
    if len(lines) < 2:
        raise RuntimeError(f"Output file has no data rows: {path}")
    header = [item.strip() for item in lines[0].split("\t") if item.strip()]
    values = [float(item) for item in lines[-1].split("\t") if item.strip()]
    return header, values


def run_case(exe: Path, case: Path) -> None:
    # COARSENING: remove run artifacts before executing the White case with the new intragranular model.
    for artifact in ["output.txt", "execution.txt", "overview.txt", "input_check.txt"]:
        artifact_path = case / artifact
        if artifact_path.exists():
            artifact_path.unlink()
    subprocess.run([str(exe), str(case) + os.sep], check=True)


def set_optional_setting(path: Path, setting_name: str, value: int) -> None:
    # COARSENING: modify optional model-development settings in temporary variant cases.
    lines = path.read_text().splitlines()
    descriptions = {
        "iCoarseningDislocationDensity": (
            "(0= none, 1= Barani 2019, 2= Zullo 2026, 3= Zullo - Nicodemo 2026 COARSENING)"
        ),
        "iCoarseningSizeDistribution": (
            "(0= single Barani mean, 1= four-family Nicodemo 2026 distribution COARSENING)"
        ),
    }
    replacement = f"{value}    #    {setting_name} {descriptions.get(setting_name, '(COARSENING)')}"
    for index, line in enumerate(lines):
        if f"#    {setting_name}" in line:
            lines[index] = replacement
            break
    else:
        lines.append(replacement)
    path.write_text("\n".join(lines) + "\n")


def rmse(pairs: list[tuple[float, float]]) -> float:
    return math.sqrt(sum((calc - exp) ** 2 for exp, calc in pairs) / len(pairs))


def mape(pairs: list[tuple[float, float]]) -> float:
    usable = [(exp, calc) for exp, calc in pairs if exp != 0.0]
    return 100.0 * sum(abs(calc - exp) / abs(exp) for exp, calc in usable) / len(usable)


def plot_parity(root: Path, rows: list[dict[str, float | str]]) -> None:
    # COARSENING: compare Barani model predictions against legacy SCIANTIX White outputs.
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.ticker import FixedLocator, LogFormatterMathtext

    figures = root / "figures"
    figures.mkdir(exist_ok=True)

    for metric, config in METRICS.items():
        exp = [float(row[f"{metric}_exp"]) for row in rows]
        coarsening = [float(row[f"{metric}_coarsening"]) for row in rows]
        legacy = [float(row[f"{metric}_legacy"]) for row in rows]

        lower, upper = config["limits"]

        def positive_pairs(x_values: list[float], y_values: list[float]) -> tuple[list[float], list[float]]:
            # COARSENING: log parity plots cannot show zero-valued legacy points.
            pairs = [(x, y) for x, y in zip(x_values, y_values) if x > 0.0 and y > 0.0]
            return [x for x, _ in pairs], [y for _, y in pairs]

        legacy_x, legacy_y = positive_pairs(exp, legacy)
        coarsening_x, coarsening_y = positive_pairs(exp, coarsening)

        fig, ax = plt.subplots(figsize=(5.2, 5.2), constrained_layout=True)
        ax.scatter(legacy_x, legacy_y, color="black", s=18, marker="o", label="SCIANTIX legacy")
        ax.scatter(coarsening_x, coarsening_y, color="#d62728", s=18, marker="o", label="SCIANTIX COARSENING")
        ax.set_xscale("log")
        ax.set_yscale("log")
        # COARSENING: central parity guide only.
        ax.plot([lower, upper], [lower, upper], color="0.55", linewidth=1.0, linestyle="--", label="Parity")
        ax.set_xlim(lower, upper)
        ax.set_ylim(lower, upper)
        major_ticks = []
        tick = lower
        while tick <= upper * 1.0001:
            major_ticks.append(tick)
            tick *= 10.0
        ax.xaxis.set_major_locator(FixedLocator(major_ticks))
        ax.yaxis.set_major_locator(FixedLocator(major_ticks))
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


def plot_intergranular_gold_comparison(root: Path, legacy_root: Path) -> None:
    # COARSENING: compare intergranular swelling against White experimental data for legacy gold and model 4.
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.ticker import FixedLocator, LogFormatterMathtext

    figures = root / "figures"
    figures.mkdir(exist_ok=True)
    measured = load_expected(legacy_root / "data" / "ig_swelling.txt")

    measured_percent: list[float] = []
    gold_percent: list[float] = []
    coarsening_percent: list[float] = []

    for case_name in sorted(measured):
        case = root / case_name
        legacy_gold = legacy_root / case_name / "output_gold.txt"
        if not case.exists():
            continue
        if not legacy_gold.exists():
            continue

        header, values = load_output(case / "output.txt")
        gold_header, gold_values = load_output(legacy_gold)

        column = "Intergranular gas swelling (/)"
        measured_percent.append(measured[case_name])
        coarsening_percent.append(values[header.index(column)] * 100.0)
        gold_percent.append(gold_values[gold_header.index(column)] * 100.0)

    lower, upper = 1.0e-2, 1.0e1
    fig, ax = plt.subplots(figsize=(5.2, 5.2), constrained_layout=True)
    ax.scatter(measured_percent, gold_percent, facecolors="none", edgecolors="black", s=24, marker="^", label="SCIANTIX white gold")
    ax.scatter(measured_percent, coarsening_percent, color="#d62728", s=18, marker="o", label="SCIANTIX COARSENING")
    ax.set_xscale("log")
    ax.set_yscale("log")
    ax.plot([lower, upper], [lower, upper], color="0.55", linewidth=1.0, linestyle="--", label="Parity")
    ax.set_xlim(lower, upper)
    ax.set_ylim(lower, upper)
    ticks = [1.0e-2, 1.0e-1, 1.0, 1.0e1]
    ax.xaxis.set_major_locator(FixedLocator(ticks))
    ax.yaxis.set_major_locator(FixedLocator(ticks))
    ax.xaxis.set_major_formatter(LogFormatterMathtext())
    ax.yaxis.set_major_formatter(LogFormatterMathtext())
    ax.set_aspect("equal", adjustable="box")
    ax.set_xlabel("Experimental intergranular swelling (%)")
    ax.set_ylabel("Calculated intergranular swelling (%)")
    ax.grid(True, color="0.88", linewidth=0.8)
    ax.legend(frameon=False, loc="best")
    ax.set_title("Intergranular swelling (%)")
    fig.savefig(figures / "parity_intergranular_swelling.png", dpi=300)
    plt.close(fig)

    gold_pairs = list(zip(measured_percent, gold_percent))
    coarsening_pairs = list(zip(measured_percent, coarsening_percent))
    print(
        "intergranular_swelling_percent: "
        f"white-gold RMSE={rmse(gold_pairs):.6g}, MAPE={mape(gold_pairs):.3f}% | "
        f"COARSENING RMSE={rmse(coarsening_pairs):.6g}, MAPE={mape(coarsening_pairs):.3f}%"
    )


def plot_dislocation_option_comparison(root: Path,
                                       legacy_root: Path,
                                       expected: dict[str, dict[str, float]],
                                       case_names: list[str],
                                       exe: Path) -> None:
    # COARSENING: run dislocation-density and distributed-population variants in temporary cases.
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.ticker import FixedLocator, LogFormatterMathtext

    figures = root / "figures"
    figures.mkdir(exist_ok=True)

    legacy_rows: dict[str, list[tuple[float, float]]] = {name: [] for name in METRICS}
    variant_rows: dict[str, dict[str, list[tuple[float, float, float, float]]]] = {
        key: {name: [] for name in METRICS} for key in DISTRIBUTION_VARIANTS
    }
    dislocation_state: dict[int, list[tuple[float, float, float]]] = {option: [] for option in DISLOCATION_OPTIONS}

    for name in case_names:
        legacy_case = legacy_root / name
        if not legacy_case.is_dir():
            continue
        legacy_header, legacy_values = load_output(legacy_case / "output.txt")
        for metric, config in METRICS.items():
            experimental = expected[metric][name] * float(config["expected_factor"])
            legacy_index = legacy_header.index(str(config["legacy_column"]))
            legacy_rows[metric].append((experimental, legacy_values[legacy_index] * float(config["factor"])))

    with tempfile.TemporaryDirectory(prefix="sciantix_white_coarsening_") as temp_dir:
        temp_root = Path(temp_dir)
        for key, variant in DISTRIBUTION_VARIANTS.items():
            option_root = temp_root / key
            option_root.mkdir()
            for name in case_names:
                source_case = root / name
                if not source_case.is_dir():
                    continue
                case = option_root / name
                shutil.copytree(source_case, case)
                set_optional_setting(case / "input_settings.txt",
                                     "iCoarseningDislocationDensity",
                                     int(variant["dislocation"]))
                set_optional_setting(case / "input_settings.txt",
                                     "iCoarseningSizeDistribution",
                                     int(variant["distribution"]))
                run_case(exe, case)
                header, values = load_output(case / "output.txt")
                temperature = values[header.index("Temperature (K)")]
                burnup = values[header.index("Burnup (MWd/kgUO2)")]
                dislocation_density = values[header.index("Dislocation density (m/m3)")]
                if int(variant["distribution"]) == 0:
                    dislocation_state[int(variant["dislocation"])].append((temperature, burnup, dislocation_density))
                for metric, config in METRICS.items():
                    experimental = expected[metric][name] * float(config["expected_factor"])
                    coarsening_column = str(config["coarsening_column"])
                    if int(variant["distribution"]) == 1:
                        coarsening_column = DISTRIBUTED_COLUMNS.get(metric, coarsening_column)
                    coarsening_index = header.index(coarsening_column)
                    calculated = values[coarsening_index] * float(config["factor"])
                    lower_band = calculated
                    upper_band = calculated
                    if int(variant["distribution"]) == 1 and metric == "radius_m":
                        lower_band = values[header.index("Intragranular distributed coarsened bubble radius p10 (m)")]
                        upper_band = values[header.index("Intragranular distributed coarsened bubble radius p90 (m)")]
                    variant_rows[key][metric].append((experimental, calculated, lower_band, upper_band))

    for metric, config in METRICS.items():
        lower, upper = config["limits"]
        fig, ax = plt.subplots(figsize=(5.2, 5.2), constrained_layout=True)
        legacy_pairs = [(x, y) for x, y in legacy_rows[metric] if x > 0.0 and y > 0.0]
        if legacy_pairs:
            ax.scatter([x for x, _ in legacy_pairs],
                       [y for _, y in legacy_pairs],
                       color="black",
                       s=14,
                       marker="o",
                       label="SCIANTIX legacy")
        for key, variant in DISTRIBUTION_VARIANTS.items():
            pairs = [(x, y, lo, hi) for x, y, lo, hi in variant_rows[key][metric] if x > 0.0 and y > 0.0]
            if pairs:
                x_values = [x for x, _, _, _ in pairs]
                y_values = [y for _, y, _, _ in pairs]
                if int(variant["distribution"]) == 1 and metric == "radius_m":
                    lower_errors = [max(y - lo, 0.0) for _, y, lo, _ in pairs]
                    upper_errors = [max(hi - y, 0.0) for _, y, _, hi in pairs]
                    ax.errorbar(x_values,
                                y_values,
                                yerr=[lower_errors, upper_errors],
                                fmt=str(variant["marker"]),
                                color=str(variant["color"]),
                                ecolor=str(variant["color"]),
                                elinewidth=0.8,
                                capsize=1.8,
                                markersize=3.8,
                                linestyle="none",
                                label=str(variant["label"]))
                else:
                    ax.scatter(x_values,
                               y_values,
                               color=str(variant["color"]),
                               s=14,
                               marker=str(variant["marker"]),
                               label=str(variant["label"]))
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
        ax.set_xlabel(f"Experimental {config['label']}")
        ax.set_ylabel(f"Calculated {config['label']}")
        ax.grid(True, color="0.88", linewidth=0.8)
        ax.legend(frameon=False, loc="best")
        ax.set_title(f"{config['label']} - dislocation options")
        fig.savefig(figures / f"dislocation_options_{metric}.png", dpi=300)
        plt.close(fig)

    # COARSENING: print variant scores so the distributed-population effect is visible without opening the figures.
    for metric in METRICS:
        print(f"{metric} variants:")
        for key, variant in DISTRIBUTION_VARIANTS.items():
            pairs = [(x, y) for x, y, _, _ in variant_rows[key][metric]]
            if not pairs:
                continue
            print(f"  {variant['label']}: RMSE={rmse(pairs):.6g}, MAPE={mape(pairs):.3f}%")

    plot_focused_nicodemo_comparison(root, variant_rows)
    plot_dislocation_density_state(root, dislocation_state)


def plot_focused_nicodemo_comparison(root: Path,
                                     variant_rows: dict[str, dict[str, list[tuple[float, float, float, float]]]]) -> None:
    # COARSENING: draw compact White parity plots for Barani, Zullo-Nicodemo, and distributed Nicodemo 2026 only.
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.collections import LineCollection
    from matplotlib.lines import Line2D
    from matplotlib.ticker import FixedLocator, LogFormatterMathtext

    figures = root / "figures"
    figures.mkdir(exist_ok=True)

    focused_styles = {
        "barani_2019": {"label": "Barani 2019", "color": "#d62728", "marker": "x", "size": 26},
        "zullo_nicodemo_2026": {"label": "Zullo - Nicodemo 2026", "color": "#9467bd", "marker": "*", "size": 42},
        "nicodemo_2026_distribution": {"label": "Nicodemo 2026", "color": "#ff7f0e", "marker": "o", "size": 16},
    }

    for metric, config in METRICS.items():
        lower, upper = config["limits"]
        fig, ax = plt.subplots(figsize=(5.2, 5.2), constrained_layout=True)

        for key in ("barani_2019", "zullo_nicodemo_2026"):
            style = focused_styles[key]
            pairs = [(x, y) for x, y, _, _ in variant_rows[key][metric] if x > 0.0 and y > 0.0]
            if not pairs:
                continue
            ax.scatter([x for x, _ in pairs],
                       [y for _, y in pairs],
                       color=str(style["color"]),
                       s=float(style["size"]),
                       marker=str(style["marker"]),
                       linewidths=1.1,
                       alpha=0.95,
                       zorder=4,
                       label=str(style["label"]))

        nicodemo_style = focused_styles["nicodemo_2026_distribution"]
        nicodemo_pairs = [
            (x, y, lo, hi)
            for x, y, lo, hi in variant_rows["nicodemo_2026_distribution"][metric]
            if x > 0.0 and y > 0.0
        ]
        if metric == "radius_m":
            for experimental, mean, p10, p90 in nicodemo_pairs:
                if p10 <= 0.0 or p90 <= 0.0 or p90 <= p10:
                    continue
                # COARSENING: vertical continuous-histogram surrogate, thicker near the predicted radius mode.
                y_values = [10 ** (math.log10(p10) + i * (math.log10(p90) - math.log10(p10)) / 48.0) for i in range(49)]
                sigma = max((math.log(p90) - math.log(p10)) / (2.0 * 1.2815515655446004), 1.0e-12)
                mu = math.log(mean)
                densities = [
                    math.exp(-0.5 * ((math.log(math.sqrt(y_values[i] * y_values[i + 1])) - mu) / sigma) ** 2)
                    for i in range(len(y_values) - 1)
                ]
                max_density = max(densities) if densities else 1.0
                line_segments = [
                    [(experimental, y_values[i]), (experimental, y_values[i + 1])] for i in range(len(y_values) - 1)
                ]
                linewidths = [0.35 + 5.0 * density / max_density for density in densities]
                collection = LineCollection(line_segments,
                                            colors=str(nicodemo_style["color"]),
                                            linewidths=linewidths,
                                            alpha=0.30,
                                            zorder=1)
                ax.add_collection(collection)
            ax.scatter([x for x, _, _, _ in nicodemo_pairs],
                       [y for _, y, _, _ in nicodemo_pairs],
                       color=str(nicodemo_style["color"]),
                       s=float(nicodemo_style["size"]),
                       marker=str(nicodemo_style["marker"]),
                       alpha=0.65,
                       linewidths=0.0,
                       zorder=3,
                       label=str(nicodemo_style["label"]))
        elif nicodemo_pairs:
            ax.scatter([x for x, _, _, _ in nicodemo_pairs],
                       [y for _, y, _, _ in nicodemo_pairs],
                       color=str(nicodemo_style["color"]),
                       s=float(nicodemo_style["size"]),
                       marker=str(nicodemo_style["marker"]),
                       alpha=0.65,
                       linewidths=0.0,
                       zorder=3,
                       label=str(nicodemo_style["label"]))

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
        ax.set_xlabel(f"Experimental {config['label']}")
        ax.set_ylabel(f"Calculated {config['label']}")
        ax.grid(True, color="0.88", linewidth=0.8)

        handles, labels = ax.get_legend_handles_labels()
        if metric == "radius_m":
            handles.append(Line2D([0], [0], color=str(nicodemo_style["color"]), linewidth=4.0, alpha=0.30))
            labels.append("Nicodemo 2026 distribution")
        ax.legend(handles, labels, frameon=False, loc="best", fontsize=9)
        ax.set_title(f"{config['label']} - focused comparison")
        fig.savefig(figures / f"focused_nicodemo_{metric}.png", dpi=300)
        plt.close(fig)


def plot_dislocation_density_state(root: Path, state: dict[int, list[tuple[float, float, float]]]) -> None:
    # COARSENING: show how Barani 2019, Zullo 2026, and Zullo - Nicodemo 2026 dislocation densities map onto White case state points.
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    figures = root / "figures"
    figures.mkdir(exist_ok=True)

    axes = [
        ("temperature", "Temperature (K)", "dislocation_density_vs_temperature.png", 0),
        ("burnup", "Burnup (MWd/kgUO2)", "dislocation_density_vs_burnup.png", 1),
    ]
    for _, xlabel, filename, index in axes:
        fig, ax = plt.subplots(figsize=(5.6, 4.2), constrained_layout=True)
        for option in (1, 2, 3):
            _, label, color, marker = DISLOCATION_OPTIONS[option]
            points = [(row[index], row[2]) for row in state.get(option, []) if row[2] > 0.0]
            if not points:
                continue
            ax.scatter([x for x, _ in points], [y for _, y in points], s=18, color=color, marker=marker, label=label)
        ax.set_yscale("log")
        ax.set_xlabel(xlabel)
        ax.set_ylabel("Dislocation density (m/m3)")
        ax.grid(True, color="0.88", linewidth=0.8)
        ax.legend(frameon=False, loc="best")
        ax.set_title("Dislocation density")
        fig.savefig(figures / filename, dpi=300)
        plt.close(fig)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--run", action="store_true", help="COARSENING: execute SCIANTIX before collecting metrics.")
    parser.add_argument("--figures", action="store_true", help="COARSENING: write parity plots in the figures folder.")
    parser.add_argument("--dislocation-variants",
                        action="store_true",
                        help="COARSENING: run and plot dislocation-density options 0/1/2/3 in temporary cases.")
    parser.add_argument("--exe", default=None, help="Path to sciantix.x. Defaults to ../../build/sciantix.x.")
    args = parser.parse_args()

    root = Path(__file__).resolve().parent
    repo = root.parent.parent
    legacy_root = repo / "regression" / "white"
    exe = Path(args.exe).resolve() if args.exe else repo / "build" / "sciantix.x"

    expected = {
        metric: load_expected(root / "data" / str(config["expected"])) for metric, config in METRICS.items()
    }
    case_names = sorted(set.intersection(*(set(values) for values in expected.values())))

    rows: list[dict[str, float | str]] = []
    coarsening_pairs: dict[str, list[tuple[float, float]]] = {name: [] for name in METRICS}
    legacy_pairs: dict[str, list[tuple[float, float]]] = {name: [] for name in METRICS}

    for name in case_names:
        case = root / name
        legacy_case = legacy_root / name
        if not case.is_dir():
            print(f"COARSENING: skipping missing White case {name}")
            continue
        if not legacy_case.is_dir():
            print(f"COARSENING: skipping missing legacy White case {name}")
            continue
        if args.run:
            run_case(exe, case)

        header, values = load_output(case / "output.txt")
        legacy_header, legacy_values = load_output(legacy_case / "output.txt")
        row: dict[str, float | str] = {"case": name}

        for metric, config in METRICS.items():
            factor = float(config["factor"])
            coarsening_index = header.index(str(config["coarsening_column"]))
            legacy_index = legacy_header.index(str(config["legacy_column"]))
            calculated = values[coarsening_index] * factor
            legacy_calculated = legacy_values[legacy_index] * factor
            experimental = expected[metric][name] * float(config["expected_factor"])
            row[f"{metric}_exp"] = experimental
            row[f"{metric}_coarsening"] = calculated
            row[f"{metric}_legacy"] = legacy_calculated
            row[f"{metric}_coarsening_error"] = calculated - experimental
            row[f"{metric}_legacy_error"] = legacy_calculated - experimental
            coarsening_pairs[metric].append((experimental, calculated))
            legacy_pairs[metric].append((experimental, legacy_calculated))

        rows.append(row)

    csv_path = root / "coarsening_metrics.csv"
    if rows:
        fieldnames = list(rows[0].keys())
        with csv_path.open("w", newline="") as stream:
            writer = csv.DictWriter(stream, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(rows)

    print("COARSENING White intragranular metrics")
    print(f"Cases analyzed: {len(rows)}")
    print(f"CSV: {csv_path}")
    for metric, metric_pairs in coarsening_pairs.items():
        if not metric_pairs:
            continue
        print(
            f"{metric}: COARSENING RMSE={rmse(metric_pairs):.6g}, MAPE={mape(metric_pairs):.3f}% | "
            f"legacy RMSE={rmse(legacy_pairs[metric]):.6g}, MAPE={mape(legacy_pairs[metric]):.3f}%"
        )

    if args.figures and rows:
        plot_parity(root, rows)
        plot_intergranular_gold_comparison(root, legacy_root)
        if args.dislocation_variants:
            plot_dislocation_option_comparison(root, legacy_root, expected, case_names, exe)
        print(f"Figures: {root / 'figures'}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
