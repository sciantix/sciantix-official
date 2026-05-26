#!/usr/bin/env python3
"""COARSENING: plot analytical dislocation-bubble nucleation terms without running White cases."""

from __future__ import annotations

import math
import csv
from pathlib import Path


K0 = 8.0e5
RHO_BARANI = 4.0e13
DEFAULT_BSAT = 16.0
DEFAULT_TSAT = 1850.0
TEMPERATURE_WIDTH = 100.0


def positive(value: float) -> float:
    # COARSENING: keep analytical scans inside the same positive domain used by SCIANTIX.
    return max(value, 0.0)


def f_burnup(burnup: float, burnup_saturation: float) -> float:
    # COARSENING: same cumulative burnup activation used by iCoarseningKModel = 1 and 2.
    return 1.0 - math.exp(-positive(burnup) / max(burnup_saturation, 1.0e-12))


def df_burnup_dbu(burnup: float, burnup_saturation: float) -> float:
    # COARSENING: derivative of fBu, useful as a smooth equivalent nu_b per burnup.
    bsat = max(burnup_saturation, 1.0e-12)
    return math.exp(-positive(burnup) / bsat) / bsat


def f_temperature(maximum_temperature: float, temperature_saturation: float) -> float:
    # COARSENING: same high-temperature damping used by iCoarseningKModel = 1 and 2.
    return 1.0 / (1.0 + math.exp((maximum_temperature - temperature_saturation) / TEMPERATURE_WIDTH))


def rho_veshchunov_2009(burnup: float, temperature: float) -> float:
    # COARSENING: same Veshchunov 2009 dislocation-density correlation used by SCIANTIX.
    if burnup <= 0.0 or temperature <= 0.0:
        return 0.0
    a = 6.545e12
    n = 1.151
    a_inf = 0.608
    t_c = 1109.0
    dt = 25.8
    thermal_weight = a_inf + (1.0 - a_inf) / (1.0 + math.exp((temperature - t_c) / dt))
    return positive(a * burnup**n * thermal_weight)


def nicodemo_1_target_density(burnup: float, temperature: float, bsat: float, tsat: float) -> float:
    # COARSENING: algebraic target N_d = rho_d*K0*fBu*fT for iCoarseningKModel = 1.
    rho_d = rho_veshchunov_2009(burnup, temperature)
    return rho_d * K0 * f_burnup(burnup, bsat) * f_temperature(temperature, tsat)


def nu_b_equivalent_per_burnup(burnup: float,
                               temperature: float,
                               bsat: float,
                               tsat: float,
                               availability: float = 1.0) -> float:
    # COARSENING: smooth equivalent nu_b=dN_d/dBu. Model 2 multiplies it by available sites.
    rho_d = rho_veshchunov_2009(burnup, temperature)
    return rho_d * K0 * df_burnup_dbu(burnup, bsat) * f_temperature(temperature, tsat) * availability


def nicodemo_2_site_saturation(burnup: float, temperature: float) -> float:
    # COARSENING: model-2 available dislocation-bubble site density N_d,sat = rho_d*K0.
    return rho_veshchunov_2009(burnup, temperature) * K0


def make_range(start: float, stop: float, count: int) -> list[float]:
    step = (stop - start) / (count - 1)
    return [start + step * index for index in range(count)]


def output_last_row(path: Path) -> dict[str, float]:
    # COARSENING: read final White case state without re-running SCIANTIX.
    lines = [line.strip() for line in path.read_text().splitlines() if line.strip()]
    header = [item.strip() for item in lines[0].split("\t") if item.strip()]
    values = [float(item) for item in lines[-1].split("\t") if item.strip()]
    return dict(zip(header, values))


def maximum_history_temperature(path: Path) -> float:
    # COARSENING: fT uses the maximum temperature history, not the cooled final output temperature.
    temperatures = []
    with path.open() as stream:
        for line in stream:
            fields = line.split()
            if len(fields) >= 2:
                temperatures.append(float(fields[1]))
    return max(temperatures) if temperatures else 0.0


def load_white_case_summary(root: Path) -> list[dict[str, float | str]]:
    # COARSENING: collect final Bu/T and swelling errors from existing WhiteCOARSENING outputs.
    metrics: dict[str, dict[str, float]] = {}
    with (root / "coarsening_metrics.csv").open() as stream:
        for row in csv.DictReader(stream):
            metrics.setdefault(row["case"], {})[row["metric"]] = {
                "experimental": float(row["experimental"]),
                "barani": float(row["barani_2019"]),
            }

    rows: list[dict[str, float | str]] = []
    for case, case_metrics in metrics.items():
        output = root / case / "output.txt"
        if not output.exists():
            continue
        final = output_last_row(output)
        maximum_temperature = maximum_history_temperature(root / case / "input_history.txt")
        experimental_swelling = case_metrics["swelling_percent"]["experimental"]
        barani_swelling = case_metrics["swelling_percent"]["barani"]
        rows.append(
            {
                "case": case,
                "burnup": final["Burnup (MWd/kgUO2)"],
                "temperature": maximum_temperature,
                "experimental_swelling": experimental_swelling,
                "swelling_ratio": barani_swelling / experimental_swelling if experimental_swelling > 0.0 else math.nan,
            }
        )
    return rows


def main() -> None:
    import matplotlib

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    root = Path(__file__).resolve().parent
    figures = root / "figures"
    figures.mkdir(exist_ok=True)

    burnup_values = make_range(0.0, 80.0, 320)
    temperature_values = make_range(900.0, 2200.0, 320)
    bsat_values = [4.0, 8.0, 16.0, 32.0]
    tsat_values = [1450.0, 1650.0, 1850.0]
    broad_bsat_values = [4.0, 8.0, 16.0, 32.0, 64.0]
    broad_tsat_values = [1350.0, 1500.0, 1650.0, 1800.0, 1950.0]

    fig, axes = plt.subplots(1, 2, figsize=(10.0, 4.2), constrained_layout=True)
    # COARSENING: iCoarseningKModel=1 uses K_eff = K0*fBu*fT.
    for bsat in [8.0, DEFAULT_BSAT, 32.0]:
        axes[0].plot(burnup_values, [f_burnup(bu, bsat) for bu in burnup_values], label=f"Bsat={bsat:g}")
    for tsat in [1650.0, DEFAULT_TSAT, 2000.0]:
        axes[1].plot(
            temperature_values,
            [f_temperature(temp, tsat) for temp in temperature_values],
            label=f"Tsat={tsat:g} K",
        )
    axes[0].set_xlabel("Burnup (MWd/kgUO2)")
    axes[0].set_ylabel("F(Bu)")
    axes[0].set_title("Option 1 burnup factor")
    axes[1].set_xlabel("Maximum temperature (K)")
    axes[1].set_ylabel("f(T)")
    axes[1].set_title("Option 1 temperature factor")
    for ax in axes:
        ax.grid(True, color="0.88")
        ax.legend(frameon=False)
    fig.savefig(figures / "option1_fbu_ft.png", dpi=300)
    plt.close(fig)

    fig, axes = plt.subplots(1, 2, figsize=(10.0, 4.2), constrained_layout=True)
    # COARSENING: iCoarseningKModel=2 activates sites kinetically through nu_d.
    for temp in [1200.0, 1500.0, 1800.0]:
        axes[0].plot(
            burnup_values,
            [max(nicodemo_2_site_saturation(bu, temp), 1.0) for bu in burnup_values],
            label=f"T={temp:g} K",
        )
        axes[1].plot(
            burnup_values,
            [max(nu_b_equivalent_per_burnup(bu, temp, DEFAULT_BSAT, DEFAULT_TSAT), 1.0) for bu in burnup_values],
            label=f"T={temp:g} K",
        )
    axes[0].set_yscale("log")
    axes[1].set_yscale("log")
    axes[0].set_xlabel("Burnup (MWd/kgUO2)")
    axes[0].set_ylabel("N_d,sat = rho_d K0 (bub/m3)")
    axes[0].set_title("Option 2 site saturation")
    axes[1].set_xlabel("Burnup (MWd/kgUO2)")
    axes[1].set_ylabel("nu_d equivalent (bub/m3)/(MWd/kgUO2)")
    axes[1].set_title("Option 2 initial activation rate")
    for ax in axes:
        ax.grid(True, color="0.88")
        ax.legend(frameon=False)
    fig.savefig(figures / "option2_nd_nud.png", dpi=300)
    plt.close(fig)

    fig, axes = plt.subplots(2, 2, figsize=(10.5, 8.0), constrained_layout=True)

    for bsat in bsat_values:
        axes[0, 0].plot(burnup_values, [f_burnup(bu, bsat) for bu in burnup_values], label=f"Bsat={bsat:g}")
    axes[0, 0].set_xlabel("Burnup (MWd/kgUO2)")
    axes[0, 0].set_ylabel("fBu")
    axes[0, 0].set_title("Burnup activation")
    axes[0, 0].grid(True, color="0.88")
    axes[0, 0].legend(frameon=False)

    for tsat in tsat_values:
        axes[0, 1].plot(temperature_values, [f_temperature(temp, tsat) for temp in temperature_values], label=f"Tsat={tsat:g} K")
    axes[0, 1].set_xlabel("Maximum temperature (K)")
    axes[0, 1].set_ylabel("fT")
    axes[0, 1].set_title("Temperature damping")
    axes[0, 1].grid(True, color="0.88")
    axes[0, 1].legend(frameon=False)

    for temp in [1200.0, 1500.0, 1800.0]:
        axes[1, 0].plot(
            burnup_values,
            [max(nicodemo_1_target_density(bu, temp, DEFAULT_BSAT, DEFAULT_TSAT), 1.0) for bu in burnup_values],
            label=f"Tmax={temp:g} K",
        )
    axes[1, 0].set_yscale("log")
    axes[1, 0].set_xlabel("Burnup (MWd/kgUO2)")
    axes[1, 0].set_ylabel("Nicodemo 1 target N_d (bub/m3)")
    axes[1, 0].set_title("Algebraic target")
    axes[1, 0].grid(True, color="0.88")
    axes[1, 0].legend(frameon=False)

    for bsat in bsat_values:
        axes[1, 1].plot(
            burnup_values,
            [max(nu_b_equivalent_per_burnup(bu, 1500.0, bsat, DEFAULT_TSAT), 1.0) for bu in burnup_values],
            label=f"Bsat={bsat:g}",
        )
    axes[1, 1].set_yscale("log")
    axes[1, 1].set_xlabel("Burnup (MWd/kgUO2)")
    axes[1, 1].set_ylabel("nu_b equivalent (bub/m3)/(MWd/kgUO2)")
    axes[1, 1].set_title("Initial kinetic activation rate")
    axes[1, 1].grid(True, color="0.88")
    axes[1, 1].legend(frameon=False)

    fig.savefig(figures / "nu_b_setting_sensitivity.png", dpi=300)
    plt.close(fig)

    fig, ax = plt.subplots(figsize=(6.4, 4.8), constrained_layout=True)
    ax.plot(
        burnup_values,
        [max(nu_b_equivalent_per_burnup(bu, 1500.0, DEFAULT_BSAT, DEFAULT_TSAT, 1.0), 1.0) for bu in burnup_values],
        label="Nicodemo 1 equivalent d target/dBu",
        color="#ff7f0e",
    )
    for availability, style in [(1.0, "-"), (0.5, "--"), (0.1, ":")]:
        ax.plot(
            burnup_values,
            [
                max(nu_b_equivalent_per_burnup(bu, 1500.0, DEFAULT_BSAT, DEFAULT_TSAT, availability), 1.0)
                for bu in burnup_values
            ],
            linestyle=style,
            color="#9467bd",
            label=f"Nicodemo 2, available={availability:g}",
        )
    ax.set_yscale("log")
    ax.set_xlabel("Burnup (MWd/kgUO2)")
    ax.set_ylabel("nu_b equivalent (bub/m3)/(MWd/kgUO2)")
    ax.set_title("Nicodemo 1 vs 2 activation")
    ax.grid(True, color="0.88")
    ax.legend(frameon=False)
    fig.savefig(figures / "nu_b_nicodemo_1_vs_2.png", dpi=300)
    plt.close(fig)

    fig, axes = plt.subplots(2, 2, figsize=(10.5, 8.0), constrained_layout=True)
    for bsat in broad_bsat_values:
        axes[0, 0].plot(burnup_values, [f_burnup(bu, bsat) for bu in burnup_values], label=f"Bsat={bsat:g}")
        axes[1, 0].plot(
            burnup_values,
            [max(df_burnup_dbu(bu, bsat), 1.0e-6) for bu in burnup_values],
            label=f"Bsat={bsat:g}",
        )
    for tsat in broad_tsat_values:
        axes[0, 1].plot(
            temperature_values,
            [f_temperature(temp, tsat) for temp in temperature_values],
            label=f"Tsat={tsat:g} K",
        )
        axes[1, 1].plot(
            temperature_values,
            [K0 * f_temperature(temp, tsat) for temp in temperature_values],
            label=f"Tsat={tsat:g} K",
        )

    axes[0, 0].set_title("fBu saturation")
    axes[0, 0].set_xlabel("Burnup (MWd/kgUO2)")
    axes[0, 0].set_ylabel("fBu")
    axes[1, 0].set_title("d fBu / dBu")
    axes[1, 0].set_xlabel("Burnup (MWd/kgUO2)")
    axes[1, 0].set_ylabel("1/(MWd/kgUO2)")
    axes[1, 0].set_yscale("log")
    axes[0, 1].set_title("fT saturation")
    axes[0, 1].set_xlabel("Maximum temperature (K)")
    axes[0, 1].set_ylabel("fT")
    axes[1, 1].set_title("K0*fT")
    axes[1, 1].set_xlabel("Maximum temperature (K)")
    axes[1, 1].set_ylabel("bub/m")
    for ax in axes.flat:
        ax.grid(True, color="0.88")
        ax.legend(frameon=False)
    fig.savefig(figures / "k_function_saturation_scan.png", dpi=300)
    plt.close(fig)

    white_rows = load_white_case_summary(root)
    if white_rows:
        fig, ax = plt.subplots(figsize=(6.6, 5.0), constrained_layout=True)
        x_values = [float(row["burnup"]) for row in white_rows]
        y_values = [float(row["temperature"]) for row in white_rows]
        colors = [math.log10(max(float(row["swelling_ratio"]), 1.0e-12)) for row in white_rows]
        sizes = [35.0 if float(row["experimental_swelling"]) <= 3.0 else 22.0 for row in white_rows]
        scatter = ax.scatter(x_values, y_values, c=colors, s=sizes, cmap="coolwarm", edgecolors="black", linewidths=0.25)
        for bsat in [8.0, 16.0, 32.0]:
            bu_line = burnup_values
            weight_line = [f_burnup(bu, bsat) for bu in bu_line]
            scaled_line = [900.0 + 450.0 * value for value in weight_line]
            ax.plot(bu_line, scaled_line, linestyle="--", linewidth=1.0, label=f"scaled fBu, Bsat={bsat:g}")
        colorbar = fig.colorbar(scatter, ax=ax)
        colorbar.set_label("log10(Barani swelling / experimental swelling)")
        ax.set_xlabel("Burnup (MWd/kgUO2)")
        ax.set_ylabel("Maximum temperature (K)")
        ax.set_title("White low-swelling cases vs K weights")
        ax.grid(True, color="0.88")
        ax.legend(frameon=False, loc="best")
        fig.savefig(figures / "white_cases_swelling_error_bu_t.png", dpi=300)
        plt.close(fig)


if __name__ == "__main__":
    main()
