#!/usr/bin/env python3
from __future__ import annotations

import csv
import re
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np

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
    "lines.markersize": 4,
    "legend.frameon": False,
})

SCRIPT_DIR = Path(__file__).resolve().parent

def read_table(path: Path) -> list[dict[str, str]]:
    with path.open(newline="") as stream:
        return list(csv.DictReader(stream, delimiter="\t"))


def source_label(source: str) -> str:
    match = re.fullmatch(r"(.+?)(\d{4}[a-z]?)", source)
    if not match:
        return source

    name, year = match.groups()
    name = re.sub(r"(?<=[a-z])(?=[A-Z])", " ", name)
    name = name.replace(" And ", " and ")
    return f"{name}, {year}"


def read_output_curve_Kato(case_dir: Path) -> tuple[np.ndarray, np.ndarray]:
    rows = read_table(case_dir / "output.txt")
    om = np.array([float(row["O/M ratio (/)"]) for row in rows], dtype=float)
    mu = np.array([float(row["Fuel oxygen potential - Kato (KJ/mol)"]) for row in rows], dtype=float)

    order = np.argsort(om)
    om = om[order]
    mu = mu[order]

    unique_om, unique_indices = np.unique(om, return_index=True)
    return unique_om, mu[unique_indices]

def read_output_curve_OC(case_dir: Path) -> tuple[np.ndarray, np.ndarray]:
    rows = read_table(case_dir / "output.txt")
    om = np.array([float(row["O/M ratio (/)"]) for row in rows], dtype=float)
    mu = np.array([float(row["Fuel oxygen potential - CALPHAD (KJ/mol)"]) for row in rows], dtype=float)

    order = np.argsort(om)
    om = om[order]
    mu = mu[order]

    unique_om, unique_indices = np.unique(om, return_index=True)
    return unique_om, mu[unique_indices]

def interpolate_curve(x: np.ndarray, y: np.ndarray, samples: np.ndarray) -> np.ndarray:
    if len(x) == 1:
        return np.full_like(samples, y[0], dtype=float)
    return np.interp(samples, x, y)


def collect_points_Kato() -> list[dict[str, float | str]]:
    points = []
    for source_dir in sorted(SCRIPT_DIR.glob("test_*")):
        if not source_dir.is_dir():
            continue

        source = source_dir.name.removeprefix("test_")
        for case_dir in sorted(source_dir.glob("T_*_q_*")):
            if not (case_dir / "output.txt").exists():
                continue

            curve_om, curve_mu = read_output_curve_Kato(case_dir)
            for row in read_table(case_dir / "experimental_subset.txt"):
                exp_om = float(row["O_M_ratio"])
                exp_mu = float(row["mu_O2_kJ_mol"])
                sci_mu = interpolate_curve(curve_om, curve_mu, np.array([exp_om]))[0]
                points.append(
                    {
                        "source": source,
                        "case": f"{source}/{case_dir.name}",
                        "temperature": float(row["Temperature_K"]),
                        "pu_m_percent": float(row["Pu_M_percent"]),
                        "om": exp_om,
                        "exp_mu": exp_mu,
                        "sci_mu": sci_mu,
                        "residual": sci_mu - exp_mu,
                    }
                )

    return points

def collect_points_OC() -> list[dict[str, float | str]]:
    points = []
    for source_dir in sorted(SCRIPT_DIR.glob("test_*")):
        if not source_dir.is_dir():
            continue

        source = source_dir.name.removeprefix("test_")
        for case_dir in sorted(source_dir.glob("T_*_q_*")):
            if not (case_dir / "output.txt").exists():
                continue

            curve_om, curve_mu = read_output_curve_OC(case_dir)
            for row in read_table(case_dir / "experimental_subset.txt"):
                exp_om = float(row["O_M_ratio"])
                exp_mu = float(row["mu_O2_kJ_mol"])
                sci_mu = interpolate_curve(curve_om, curve_mu, np.array([exp_om]))[0]
                points.append(
                    {
                        "source": source,
                        "case": f"{source}/{case_dir.name}",
                        "temperature": float(row["Temperature_K"]),
                        "pu_m_percent": float(row["Pu_M_percent"]),
                        "om": exp_om,
                        "exp_mu": exp_mu,
                        "sci_mu": sci_mu,
                        "residual": sci_mu - exp_mu,
                    }
                )

    return points


def plot_parity(points: list[dict[str, float | str]], folder) -> None:
    exp = np.array([point["exp_mu"] for point in points], dtype=float)
    sci = np.array([point["sci_mu"] for point in points], dtype=float)
    temperatures = np.array([point["temperature"] for point in points], dtype=float)

    lower = -800.0
    upper = 0.0
    margin = 0.05 * (upper - lower)
    limits = [lower - margin, upper + margin]

    fig, ax = plt.subplots()
    scatter = ax.scatter(exp, sci, c=temperatures, cmap="turbo", s=18, alpha=0.8)
    ax.plot(limits, limits, color="0.25", linewidth=1.4, label="1:1")
    ax.set_xlim(limits)
    ax.set_ylim(limits)
    ax.set_xlabel("Experimental oxygen potential (kJ/mol)")
    ax.set_ylabel("Calculated oxygen potential (kJ/mol)")
    ax.grid(True, linestyle=":", alpha=0.6)
    ax.legend(loc="upper left")
    colorbar = fig.colorbar(scatter, ax=ax)
    colorbar.set_label("Temperature (K)")
    fig.tight_layout()
    fig.savefig(folder / "parity_oxygen_potential.png", dpi=220)
    plt.close(fig)


def plot_residuals(points: list[dict[str, float | str]], folder) -> None:
    sources = sorted({str(point["source"]) for point in points})
    residual_by_source = []
    for source in sources:
        values = [float(point["residual"]) for point in points if point["source"] == source]
        residual_by_source.append(float(np.mean(values)))

    y_positions = np.arange(len(sources))
    colors = np.where(np.array(residual_by_source) >= 0.0, "#4477aa", "#bb5566")
    labels = [source_label(source) for source in sources]

    fig, ax = plt.subplots()
    ax.barh(y_positions, residual_by_source, color=colors)
    ax.axvline(0.0, color="0.25", linewidth=1.0)
    ax.set_yticks(y_positions)
    ax.set_yticklabels(labels)
    ax.set_xlim(-200, 200)
    ax.invert_yaxis()
    ax.set_xlabel("Mean calculated - experimental oxygen potential (kJ/mol)")
    ax.grid(True, axis="x", linestyle=":", alpha=0.6)
    fig.tight_layout()
    fig.savefig(folder / "mean_residual_oxygen_potential_by_source.png", dpi=220)
    plt.close(fig)


def plot_source_curves_Kato(folder) -> None:
    source_plot_dir = folder / "sources"
    source_plot_dir.mkdir(exist_ok=True)

    for source_dir in sorted(SCRIPT_DIR.glob("test_*")):
        if not source_dir.is_dir():
            continue

        fig, ax = plt.subplots()
        plotted = False
        for case_dir in sorted(source_dir.glob("T_*_q_*")):
            if not (case_dir / "output.txt").exists():
                continue

            curve_om, curve_mu = read_output_curve_Kato(case_dir)
            exp_rows = read_table(case_dir / "experimental_subset.txt")
            exp_om = np.array([float(row["O_M_ratio"]) for row in exp_rows], dtype=float)
            exp_mu = np.array([float(row["mu_O2_kJ_mol"]) for row in exp_rows], dtype=float)

            label = case_dir.name.removeprefix("T_").replace("_q_", ", Pu/M=")
            ax.plot(curve_om, curve_mu, linewidth=1.5, label=label)
            ax.scatter(exp_om, exp_mu, s=18)
            plotted = True

        if not plotted:
            plt.close(fig)
            continue

        source = source_dir.name.removeprefix("test_")
        ax.set_title(source)
        ax.set_xlabel("O/M ratio (-)")
        ax.set_ylabel("Oxygen potential (kJ/mol)")
        ax.grid(True, linestyle=":", alpha=0.6)
        ax.legend()
        fig.tight_layout()
        fig.savefig(source_plot_dir / f"{source}.png", dpi=220)
        plt.close(fig)

def plot_source_curves_OC(folder) -> None:
    source_plot_dir = folder / "sources"
    source_plot_dir.mkdir(exist_ok=True)

    for source_dir in sorted(SCRIPT_DIR.glob("test_*")):
        if not source_dir.is_dir():
            continue

        fig, ax = plt.subplots()
        plotted = False
        for case_dir in sorted(source_dir.glob("T_*_q_*")):
            if not (case_dir / "output.txt").exists():
                continue

            curve_om, curve_mu = read_output_curve_OC(case_dir)
            exp_rows = read_table(case_dir / "experimental_subset.txt")
            exp_om = np.array([float(row["O_M_ratio"]) for row in exp_rows], dtype=float)
            exp_mu = np.array([float(row["mu_O2_kJ_mol"]) for row in exp_rows], dtype=float)

            label = case_dir.name.removeprefix("T_").replace("_q_", ", Pu/M=")
            ax.plot(curve_om, curve_mu, linewidth=1.5, label=label)
            ax.scatter(exp_om, exp_mu, s=18)
            plotted = True

        if not plotted:
            plt.close(fig)
            continue

        source = source_dir.name.removeprefix("test_")
        ax.set_title(source)
        ax.set_xlabel("O/M ratio (-)")
        ax.set_ylabel("Oxygen potential (kJ/mol)")
        ax.grid(True, linestyle=":", alpha=0.6)
        ax.legend()
        fig.tight_layout()
        fig.savefig(source_plot_dir / f"{source}.png", dpi=220)
        plt.close(fig)


def write_summary(points: list[dict[str, float | str]], folder) -> None:
    output = folder / "oxygen_potential_plot_data.tsv"
    with output.open("w", newline="") as stream:
        writer = csv.DictWriter(
            stream,
            delimiter="\t",
            fieldnames=[
                "source",
                "case",
                "temperature",
                "pu_m_percent",
                "om",
                "exp_mu",
                "sci_mu",
                "residual",
            ],
        )
        writer.writeheader()
        writer.writerows(points)


def main() -> None:
    FIGURES_DIR = SCRIPT_DIR / "figures_Kato"
    FIGURES_DIR.mkdir(exist_ok=True)
    points = collect_points_Kato()
    write_summary(points, folder=FIGURES_DIR)
    plot_parity(points, folder=FIGURES_DIR)
    plot_residuals(points, folder=FIGURES_DIR)
    # plot_source_curves_Kato(folder=FIGURES_DIR)
    print(f"Saved oxygen-potential plots in {FIGURES_DIR}")
    FIGURES_DIR = SCRIPT_DIR / "figures_OC"
    FIGURES_DIR.mkdir(exist_ok=True)
    points = collect_points_OC()
    write_summary(points, folder=FIGURES_DIR)
    plot_parity(points, folder=FIGURES_DIR)
    plot_residuals(points, folder=FIGURES_DIR)
    # plot_source_curves_OC(folder=FIGURES_DIR)
    print(f"Saved oxygen-potential plots in {FIGURES_DIR}")


if __name__ == "__main__":
    main()
