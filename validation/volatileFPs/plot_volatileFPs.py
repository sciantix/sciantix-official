"""
sciantix testing suite
author: Elisa Cappellari

Detailed plots of the volatileFPs cases (test_Colle2005, test_Hiernaut2008): irradiation and annealing
history, base irradiation, matrix vaporisation, fission-product inventories and grain-boundary chemistry
(Thermochimica, Cs-I-Te). The comparison with the experimental data is in plot_Colle2005.py and
plot_Hiernaut2008.py.

python3 validation/volatileFPs/plot_volatileFPs.py
"""
#!/usr/bin/env python3
import os
import sys

import matplotlib.pyplot as plt
import numpy as np

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from testing.core.common import load_output

CASES = ("test_Colle2005", "test_Hiernaut2008", "test_Colle2023")
# Figure titles (the folder name stays in the file names)
LABELS = {
    "Colle2005": "Colle et al. (2006)",
    "Hiernaut2008": "Hiernaut et al. (2008)",
    "Colle2023": "Colle et al. (2023)",
}
ELEMENTS = ("Xe", "Cs", "I", "Te")
COLORS = {"Xe": "darkorange", "Cs": "dodgerblue", "I": "forestgreen", "Te": "crimson"}
# Minimum share of the largest mass for a grain-boundary species to be plotted
SPECIES_THRESHOLD = 0.01


class Case:
    def __init__(self, case_dir):
        self.directory = case_dir
        self.name = os.path.basename(case_dir).replace("test_", "")
        self.label = LABELS.get(self.name, self.name)
        self.output = load_output(case_dir)
        thermochemistry_path = os.path.join(case_dir, "thermochemistry_output.txt")
        self.thermochemistry = load_output(case_dir, "thermochemistry_output.txt") if os.path.isfile(
            thermochemistry_path) else None
        self.time = self.column("Time (h)")
        # Annealing: from the start of the matrix vaporisation (input_thermochemistry_settings.txt). Base irradiation:
        # up to the first output row at the last time of input_history.txt with a fission rate (the following rows
        # of the short transition to the annealing are printed with the same rounded time)
        self.annealing_start = read_vaporisation_start(case_dir)
        self.annealing = self.time >= self.annealing_start - 1.0e-9
        history = np.loadtxt(os.path.join(case_dir, "input_history.txt"))
        irradiation_end = history[history[:, 2] > 0.0, 0].max()
        last = np.nonzero(self.time >= irradiation_end - 1.0e-6)[0][0]
        self.irradiation = np.arange(len(self.time)) <= last
        self.mox = self.has("Pu content (mol/m3)")

    def column(self, name, output=None):
        output = self.output if output is None else output
        header = [h.strip() for h in output.header]
        if name not in header:
            raise KeyError(f"Column '{name}' not found")
        return output.data[:, header.index(name)]

    def has(self, name, output=None):
        output = self.output if output is None else output
        return name in [h.strip() for h in output.header]

    def thermochemistry_columns(self, location):
        header = [h.strip() for h in self.thermochemistry.header]
        return [h for h in header if f", {location})" in h]


def read_vaporisation_start(case_dir):
    for line in open(os.path.join(case_dir, "input_thermochemistry_settings.txt")):
        key, _, value = line.split("#")[0].partition("=")
        if key.strip() == "matrix.vaporisation_start_time":
            return float(value)
    raise ValueError(f"matrix.vaporisation_start_time not found in {case_dir}")


def save(fig, outdir, case, suffix, rect=(0.0, 0.0, 1.0, 1.0)):
    fig.tight_layout(rect=rect)
    path = os.path.join(outdir, f"{case.name}_{suffix}.png")
    fig.savefig(path, dpi=200)
    plt.close(fig)
    print(f"Saved {path}")


def plot_history(case, outdir):
    t, T = case.time, case.column("Temperature (K)")
    annealing_minutes = (t - case.annealing_start) * 60.0

    fig, ax = plt.subplots(2, 2, figsize=(11, 8))
    ax[0, 0].plot(t, T, color="black")
    ax[0, 0].set_xlabel("Time (h)")
    ax[0, 0].set_ylabel("Temperature (K)")
    ax[0, 0].set_title("Base irradiation + annealing")

    ax[0, 1].plot(annealing_minutes[case.annealing], T[case.annealing], color="black")
    ax[0, 1].set_xlabel("Time from the start of the annealing (min)")
    ax[0, 1].set_ylabel("Temperature (K)")
    ax[0, 1].set_title("Annealing")

    ax[1, 0].plot(t, case.column("Burnup (MWd/kgUO2)"), color="tab:blue")
    ax[1, 0].set_xlabel("Time (h)")
    ax[1, 0].set_ylabel("Burnup (MWd/kgUO2)", color="tab:blue")
    twin = ax[1, 0].twinx()
    twin.plot(t, case.column("Fission rate (fiss / m3 s)"), color="tab:gray", linestyle="--")
    twin.set_ylabel("Fission rate (fiss/m3 s)", color="tab:gray")
    ax[1, 0].set_title("Base irradiation + annealing")

    ax[1, 1].semilogy(t, case.column("System pressure (Pa)"), color="tab:purple")
    ax[1, 1].set_xlabel("Time (h)")
    ax[1, 1].set_ylabel("System pressure (Pa)")
    ax[1, 1].set_title("Base irradiation + annealing")

    fig.suptitle(f"{case.label} - history")
    save(fig, outdir, case, "history")

def plot_grain_boundary_chemistry(case, outdir):
    if case.thermochemistry is None:
        return
    ann = case.annealing
    T = case.column("Temperature (K)")[ann]
    columns = case.thermochemistry_columns("at grain boundary")
    gas = [c for c in columns if "(gas," in c]
    condensed = [c for c in columns if "(condensed," in c or c.startswith("LIQUID (")]

    fig, ax = plt.subplots(1, 2, figsize=(12, 5))
    for a, group, title in [(ax[0], gas, "Gas species"), (ax[1], condensed, "Condensed and liquid phases")]:
        values = {c: case.column(c, case.thermochemistry)[ann] for c in group}
        peak = max((v.max() for v in values.values()), default=0.0)
        for column, value in values.items():
            if peak > 0.0 and value.max() >= SPECIES_THRESHOLD * peak:
                label = column.split(" (")[0]
                if label == "LIQUID":
                    label = "LIQUID (" + column.split("(")[1].split(",")[0] + ")"
                a.plot(T, value, label=label)
        a.set_xlabel("Temperature (K)")
        a.set_ylabel("Mass (g/m3)")
        a.set_title(title)
        a.legend(frameon=False, fontsize=8)
    fig.suptitle(f"{case.label} - Cs-I-Te chemistry at grain boundary calculated via Thermochimica during annealing")
    save(fig, outdir, case, "gb_chemistry")


def main():
    root = os.path.dirname(os.path.abspath(__file__))
    outdir = os.path.join(root, "figures")
    os.makedirs(outdir, exist_ok=True)

    for case_name in CASES:
        case_dir = os.path.join(root, case_name)
        if not os.path.isfile(os.path.join(case_dir, "output.txt")):
            print(f"Skipping {case_name}: output.txt not found (run the case first)")
            continue
        case = Case(case_dir)
        plot_history(case, outdir)
        plot_grain_boundary_chemistry(case, outdir)


if __name__ == "__main__":
    main()
