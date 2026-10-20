"""
sciantix testing suite
author: Elisa Cappellari

Hiernaut et al. (2008), J. Nucl. Mater. 372, 215-225 (doi:10.1016/j.jnucmat.2007.03.174):
release rate of Cs, I and Te from high-burnup UO2 (not oxidised) annealed in a Knudsen cell in vacuum at
30 K/min up to complete vaporisation, compared with SCIANTIX coupled with Thermochimica (Cs-I-Te database)
for the grain-boundary chemistry and with the matrix vaporisation model.
The measured (mass spectrometry, kg/s) and calculated release rates are normalised to their maximum.

python3 validation/volatileFPs/plot_Hiernaut2008.py
"""
#!/usr/bin/env python3
import os
import sys

import matplotlib.pyplot as plt
import numpy as np

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from testing.core.common import load_gold, load_output

ELEMENTS = {"Cs": "Cs137", "I": "I129", "Te": "Te130"}
COLORS = {"Cs": "dodgerblue", "I": "forestgreen", "Te": "crimson"}
# End of the base irradiation in input_history.txt: the annealing starts afterwards
ANNEALING_START_H = 53400.1


def column(output, name):
    header = [h.strip() for h in output.header]
    if name not in header:
        raise KeyError(f"Column '{name}' not found in output.txt")
    return output.data[:, header.index(name)]


def normalised_release_rate(output, element):
    time = column(output, "Time (h)")
    released = column(output, f"{element} released (at/m3)")
    annealing = time >= ANNEALING_START_H
    rate = np.gradient(released[annealing], time[annealing] * 3600.0)
    peak = np.max(rate) if rate.size and np.max(rate) > 0.0 else 1.0
    return column(output, "Temperature (K)")[annealing], rate / peak


def load_experimental(case_dir, isotope):
    data = np.genfromtxt(os.path.join(case_dir, "exp_data", f"{isotope}.txt"), delimiter=";", skip_header=1)
    return data[:, 0], data[:, 1] / np.max(data[:, 1])


def main():
    root = os.path.dirname(os.path.abspath(__file__))
    case_dir = os.path.join(root, "test_Hiernaut2008")
    outdir = os.path.join(root, "figures")
    os.makedirs(outdir, exist_ok=True)

    outputs = {"SCIANTIX": load_output(case_dir)}
    if os.path.isfile(os.path.join(case_dir, "output_gold.txt")):
        outputs["SCIANTIX (gold)"] = load_gold(case_dir)

    fig, axes = plt.subplots(len(ELEMENTS), 1, figsize=(7, 9), sharex=True)
    for ax, (element, isotope) in zip(axes, ELEMENTS.items()):
        temperature_exp, rate_exp = load_experimental(case_dir, isotope)
        ax.plot(temperature_exp, rate_exp, "o", markersize=3, color=COLORS[element], label=f"{isotope} - Hiernaut (2008)")
        for label, output in outputs.items():
            temperature, rate = normalised_release_rate(output, element)
            ax.plot(temperature, rate, "-" if label == "SCIANTIX" else ":", color=COLORS[element], label=f"{element} - {label}")
        ax.set_ylabel("Normalised release rate (/)")
        ax.set_ylim(0.0, 1.1)
        ax.legend(frameon=False, fontsize=8)

    axes[-1].set_xlabel("Temperature (K)")
    axes[0].set_title("Hiernaut et al. (2008) - annealing of high-burnup UO$_2$ at 30 K/min")
    fig.tight_layout()

    figure_path = os.path.join(outdir, "Hiernaut2008_release_rate.png")
    fig.savefig(figure_path, dpi=200)
    print(f"Saved {figure_path}")


if __name__ == "__main__":
    main()
