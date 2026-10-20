"""
sciantix testing suite
author: Elisa Cappellari

Colle et al. (2023), J. Nucl. Mater. 578, 154340 (doi:10.1016/j.jnucmat.2023.154340):
vaporisation of the heterogeneous high-burnup MOX fuel MOX2 (IFA-655, 118 MWd/kgHM) in a Knudsen cell at
10 K/min up to complete vaporisation.
- Nd is immobile and non-volatile, its release intensity follows the vaporisation of the matrix (Fig. 6). Compared
  with the matrix vaporisation rate calculated by SCIANTIX (Hertz-Knudsen effusion of UO, UO2, UO3, PuO, PuO2, O2),
  both normalised to their maximum.
- Normalised fractional release (NFR, Eq. (5) of the paper: release during the annealing normalised to the value at
  the end of the release) of Kr, Xe, I, Cs and Te of MOX2 (Figs. 9, 10, 12, 13, 14), compared with the release
  calculated by SCIANTIX during the annealing, normalised in the same way.

python3 validation/volatileFPs/plot_Colle2023.py
"""
#!/usr/bin/env python3
import os
import sys

import matplotlib.pyplot as plt
import numpy as np

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from testing.core.common import load_gold, load_output

# Start of the annealing in input_history.txt (end of the base irradiation at 28245 h)
ANNEALING_START_H = 28245.001
# End of the Nd(Ox) signal in Fig. 6: the sample is completely vaporised
END_OF_VAPORISATION_EXP_K = 2610.0
MATRIX_COLOR = "dimgray"
# Fission products with the normalised fractional release of MOX2 in exp_data/ (figure of Colle et al. (2023))
FISSION_PRODUCTS = {"Kr": "Fig. 9", "Xe": "Fig. 10", "I": "Fig. 12", "Cs": "Fig. 13", "Te": "Fig. 14"}
FP_COLORS = {"Kr": "tab:purple", "Xe": "tab:blue", "I": "tab:green", "Cs": "tab:orange", "Te": "tab:red"}


def column(output, name):
    header = [h.strip() for h in output.header]
    if name not in header:
        raise KeyError(f"Column '{name}' not found in output.txt")
    return output.data[:, header.index(name)]


def matrix_vaporisation(output):
    """Temperature, vaporised fraction and normalised vaporisation rate of the heavy metal (U + Pu) during the
    annealing."""
    time = column(output, "Time (h)")
    metal = column(output, "U content (mol/m3)") + column(output, "Pu content (mol/m3)")
    annealing = time >= ANNEALING_START_H
    start = np.nonzero(~annealing)[0][-1]
    temperature = column(output, "Temperature (K)")[annealing]
    vaporised = 1.0 - metal[annealing] / metal[start]
    time = time[annealing] * 3600.0
    # Rate over each time step; the rows at the history points are repeated (zero time step) and dropped
    keep = np.r_[True, np.diff(time) > 0.0]
    temperature, vaporised, time = temperature[keep], vaporised[keep], time[keep]
    rate = np.r_[0.0, np.diff(vaporised) / np.diff(time)]
    # The step at which the matrix is declared completely vaporised removes what is left at once: it is not an
    # effusion rate and is excluded from the rate curve
    rate[vaporised >= 1.0] = np.nan
    peak = np.nanmax(rate) if np.nanmax(rate) > 0.0 else 1.0
    return temperature, vaporised, rate / peak


def normalised_fractional_release(output, element):
    """Temperature and release of the element during the annealing, normalised to the release at the end of the
    annealing (NFR, Eq. (5) of Colle et al. (2023))."""
    time = column(output, "Time (h)")
    released = column(output, f"{element} released (at/m3)")
    annealing = time >= ANNEALING_START_H
    start = np.nonzero(~annealing)[0][-1]
    release = released[annealing] - released[start]
    total = release[-1]
    nfr = release / total if total > 0.0 else np.zeros_like(release)
    return column(output, "Temperature (K)")[annealing], nfr


def plot_fission_product_release(outputs, case_dir, outdir):
    fig, ax = plt.subplots(1, len(FISSION_PRODUCTS), figsize=(4.0 * len(FISSION_PRODUCTS), 4.2), sharey=True)
    for a, (element, figure) in zip(ax, FISSION_PRODUCTS.items()):
        data = np.genfromtxt(os.path.join(case_dir, "exp_data", f"{element}.txt"), delimiter=";", skip_header=1)
        color = FP_COLORS[element]
        a.plot(data[:, 0], data[:, 1], "o", markersize=4, color=color, label=f"MOX2 - Colle et al. (2023), {figure}")
        for label, output in outputs.items():
            T, nfr = normalised_fractional_release(output, element)
            a.plot(T, nfr, "-" if label == "SCIANTIX" else ":", color=color, label=label)
        a.set_title(element, fontsize=11)
        a.set_xlabel("Temperature (K)")
        a.set_xlim(500.0, 2800.0)
        a.set_ylim(0.0, 1.05)
        a.legend(frameon=False, fontsize=7, loc="upper left")
    ax[0].set_ylabel("Normalised fractional release during the annealing (/)")
    fig.suptitle("Colle et al. (2023) - release of the fission products of MOX2 (IFA-655) annealed at 10 K/min")
    fig.tight_layout()

    figure_path = os.path.join(outdir, "Colle2023_release.png")
    fig.savefig(figure_path, dpi=200)
    print(f"Saved {figure_path}")


def main():
    root = os.path.dirname(os.path.abspath(__file__))
    case_dir = os.path.join(root, "test_Colle2023")
    outdir = os.path.join(root, "figures")
    os.makedirs(outdir, exist_ok=True)

    outputs = {"SCIANTIX": load_output(case_dir)}
    if os.path.isfile(os.path.join(case_dir, "output_gold.txt")):
        outputs["SCIANTIX (gold)"] = load_gold(case_dir)
    nd = np.genfromtxt(os.path.join(case_dir, "exp_data", "Nd.txt"), delimiter=";", skip_header=1)

    fig, ax = plt.subplots(1, 2, figsize=(13, 5))
    ax[0].plot(nd[:, 0], nd[:, 1], "o", markersize=4, color="limegreen", label="Nd(Ox) - Colle et al. (2023), Fig. 6")
    for label, output in outputs.items():
        T, vaporised, rate = matrix_vaporisation(output)
        style = "-" if label == "SCIANTIX" else ":"
        ax[0].plot(T, rate, style, color=MATRIX_COLOR, label=f"matrix (U+Pu) - {label}")
        ax[1].plot(T, vaporised, style, color=MATRIX_COLOR, label=f"matrix (U+Pu) - {label}")
    for a in ax:
        a.axvline(END_OF_VAPORISATION_EXP_K, color="limegreen", linestyle="--",
                  label="complete vaporisation - Colle et al. (2023)")
        a.set_xlabel("Temperature (K)")
        a.set_xlim(1500.0, 2800.0)
        a.legend(frameon=False, fontsize=8, loc="upper left")
    ax[0].set_ylabel("Normalised release intensity / vaporisation rate (/)")
    ax[0].set_ylim(0.0, 1.1)
    ax[0].set_title("Release intensity of Nd and vaporisation rate of the matrix", fontsize=10)
    ax[1].set_ylabel("Fraction of the matrix vaporised during the annealing (/)")
    ax[1].set_ylim(0.0, 1.05)
    ax[1].set_title("Matrix vaporised", fontsize=10)
    fig.suptitle("Colle et al. (2023) - annealing of high-burnup MOX (MOX2, IFA-655) at 10 K/min")
    fig.tight_layout()

    figure_path = os.path.join(outdir, "Colle2023_matrix_vaporisation.png")
    fig.savefig(figure_path, dpi=200)
    print(f"Saved {figure_path}")

    plot_fission_product_release(outputs, case_dir, outdir)


if __name__ == "__main__":
    main()
