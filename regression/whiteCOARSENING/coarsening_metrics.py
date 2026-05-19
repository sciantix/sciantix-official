#!/usr/bin/env python3
"""COARSENING: run/analyze White cases with the Barani two-size intragranular model."""

from __future__ import annotations

import argparse
import csv
import math
import os
import subprocess
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


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--run", action="store_true", help="COARSENING: execute SCIANTIX before collecting metrics.")
    parser.add_argument("--figures", action="store_true", help="COARSENING: write parity plots in the figures folder.")
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
        print(f"Figures: {root / 'figures'}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
