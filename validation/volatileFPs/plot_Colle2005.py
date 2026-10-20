"""
sciantix testing suite
author: Elisa Cappellari

Colle et al. (2006), J. Nucl. Mater. 348, 229-242 (doi:10.1016/j.jnucmat.2005.09.024):
fractional release of Xe, Cs, I and Te and fractional vaporisation of the matrix (UO, UO2) from high-burnup
UO2 (not oxidised) during stepwise thermal annealing up to ~2800 K, compared with SCIANTIX coupled with
Thermochimica (Cs-I-Te database) for the grain-boundary chemistry and with the matrix vaporisation model.

python3 validation/volatileFPs/plot_Colle2005.py
"""
#!/usr/bin/env python3
import os
import sys

import matplotlib.pyplot as plt
import numpy as np

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from testing.core.common import load_gold, load_output

ELEMENTS = ("Xe", "Cs", "I", "Te")
COLORS = {"Xe": "darkorange", "Cs": "dodgerblue", "I": "forestgreen", "Te": "crimson"}
# Matrix vapour species measured in Fig. 4 (marker of the experimental points)
MATRIX_SPECIES = {"UO2": "s", "UO": "^"}
MATRIX_COLOR = "dimgray"
# End of the base irradiation in input_history.txt: the annealing starts afterwards
ANNEALING_START_H = 53400.1


def column(output, name):
    header = [h.strip() for h in output.header]
    if name not in header:
        raise KeyError(f"Column '{name}' not found in output.txt")
    return output.data[:, header.index(name)]


def release_fraction(output, element):
    """Fraction of the inventory present at the start of the annealing (end-of-life conditions, as in Fig. 4)
    released during the annealing."""
    time = column(output, "Time (h)")
    produced = column(output, f"{element} produced (at/m3)")
    released = column(output, f"{element} released (at/m3)")
    start = np.nonzero(time < ANNEALING_START_H)[0][-1]
    inventory = produced[start] - released[start]
    if inventory <= 0.0:
        return np.zeros_like(released)
    return (released - released[start]) / inventory


def vaporised_fraction(output):
    """Fraction of the matrix (U) present at the start of the annealing vaporised during the annealing."""
    time = column(output, "Time (h)")
    uranium = column(output, "U content (mol/m3)")
    start = np.nonzero(time < ANNEALING_START_H)[0][-1]
    if uranium[start] <= 0.0:
        return np.zeros_like(uranium)
    return 1.0 - uranium / uranium[start]


def load_experimental(case_dir, element):
    path = os.path.join(case_dir, "exp_data", f"{element}.txt")
    data = np.genfromtxt(path, delimiter=";", skip_header=1)
    return data[:, 0], data[:, 1]


def main():
    root = os.path.dirname(os.path.abspath(__file__))
    case_dir = os.path.join(root, "test_Colle2005")
    outdir = os.path.join(root, "figures")
    os.makedirs(outdir, exist_ok=True)

    outputs = {"SCIANTIX": load_output(case_dir)}
    if os.path.isfile(os.path.join(case_dir, "output_gold.txt")):
        outputs["SCIANTIX (gold)"] = load_gold(case_dir)

    fig, ax = plt.subplots(figsize=(8, 6))
    for element in ELEMENTS:
        temperature_exp, release_exp = load_experimental(case_dir, element)
        ax.plot(temperature_exp, release_exp, "o", markersize=3, color=COLORS[element], label=f"{element} - Colle (2006)")

        for label, output in outputs.items():
            annealing = column(output, "Time (h)") >= ANNEALING_START_H
            ax.plot(
                column(output, "Temperature (K)")[annealing],
                release_fraction(output, element)[annealing],
                "-" if label == "SCIANTIX" else ":",
                color=COLORS[element],
                label=f"{element} - {label}",
            )

    for species, marker in MATRIX_SPECIES.items():
        temperature_exp, release_exp = load_experimental(case_dir, species)
        ax.plot(temperature_exp, release_exp, marker, markersize=3, color=MATRIX_COLOR, label=f"{species} - Colle (2006)")
    for label, output in outputs.items():
        annealing = column(output, "Time (h)") >= ANNEALING_START_H
        ax.plot(
            column(output, "Temperature (K)")[annealing],
            vaporised_fraction(output)[annealing],
            "-" if label == "SCIANTIX" else ":",
            color=MATRIX_COLOR,
            label=f"matrix - {label}",
        )

    ax.set_xlabel("Temperature (K)")
    ax.set_ylabel("Fractional release during annealing (/)")
    ax.set_title("Colle et al. (2006) - annealing of high-burnup UO$_2$")
    ax.set_ylim(0.0, 1.05)
    ax.legend(frameon=False, fontsize=8, ncol=2)
    fig.tight_layout()

    figure_path = os.path.join(outdir, "Colle2005_release.png")
    fig.savefig(figure_path, dpi=200)
    print(f"Saved {figure_path}")


if __name__ == "__main__":
    main()
