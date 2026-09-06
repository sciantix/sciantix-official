"""Compare the HBS-formation options against each other on one irradiation.

Runs `iHighBurnupStructureFormation` = 1, 2, 3 and 4 on the SAME history, initial
conditions and time stepping, with the porosity model held at 3 -- the
formation-agnostic one -- so that the only thing that varies is the formation model.
Prints the comparison table and writes `figures/formation_options.png`.

@author  E. Cappellari
@date    2026-09-05

This is what answers "does option 4 behave sensibly next to the models already in
SCIANTIX", which `compare_with_sciantix.py` cannot: that script checks the C++
against the reference Python, i.e. that the port is faithful, not that the model is
reasonable.

Usage
-----
    python3 compare_formation_options.py                     # test_UO2HBS history
    python3 compare_formation_options.py --case ../../regression/hbs/test_UO2HBS_dislocation
    python3 compare_formation_options.py --steps 5000        # finer time stepping

The cases are built in a temporary directory and thrown away; nothing under
`regression/` is touched. SCIANTIX must have been built first (`./Allmake.sh`).

Note on the porosity model
--------------------------
Option 4 refuses `iHighBurnupStructurePorosity = 2`, which reads the formation
parameter vector positionally in the KJMA layout and would otherwise run to
completion with the HBS porosity collapsed to zero. This script therefore uses
porosity option 3 for every formation option, which also makes the comparison fair.
"""

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))

DEFAULT_CASE = os.path.join(ROOT, "regression", "hbs", "test_UO2HBS")
SCIANTIX = os.path.join(ROOT, "build", "sciantix.x")

# positions of the two options in input_settings.txt, which InputReading.C reads
# positionally (0-based line indices)
LINE_FORMATION = 17
LINE_POROSITY = 18

OPTIONS = (
    (1, "1  KJMA, Barani (2020)", "tab:blue"),
    (2, "2  KJMA + bu_inc = 15", "tab:orange"),
    (3, "3  rho_d, Veshchunov (2009)", "tab:green"),
    (4, "4  Landau functional", "k"),
)

UO2_TO_U = 0.8814          # kgU/kgUO2, as in the models themselves


def build_case(base, target, formation, porosity, steps):
    """One case directory, copied from `base` with the two options overridden."""
    os.makedirs(target, exist_ok=True)
    for name in ("input_history.txt", "input_initial_conditions.txt",
                 "input_scaling_factors.txt"):
        source = os.path.join(base, name)
        if os.path.exists(source):
            shutil.copy(source, os.path.join(target, name))

    with open(os.path.join(base, "input_settings.txt"), encoding="utf-8") as handle:
        lines = handle.read().split("\n")

    def override(index, value):
        head, sep, tail = lines[index].partition("#")
        lines[index] = "%d\t%s%s" % (value, sep, tail)

    override(LINE_FORMATION, formation)
    override(LINE_POROSITY, porosity)
    if steps is not None:
        for i, line in enumerate(lines):
            if "Number_of_time_steps_per_interval" in line:
                override(i, steps)

    with open(os.path.join(target, "input_settings.txt"), "w", encoding="utf-8") as handle:
        handle.write("\n".join(lines))


def read_output(case):
    """`output.txt` as a dict of columns."""
    with open(os.path.join(case, "output.txt"), encoding="utf-8") as handle:
        header = [c for c in handle.readline().rstrip("\n").split("\t") if c.strip()]
        columns = {name: [] for name in header}
        for line in handle:
            cells = [c for c in line.rstrip("\n").split("\t") if c.strip()]
            if not cells:
                continue
            for name, cell in zip(header, cells):
                columns[name].append(float(cell))
    return columns


def crossing(burnup, values, threshold):
    """First burnup at which `values` reaches `threshold`, or None."""
    for b, v in zip(burnup, values):
        if v >= threshold:
            return b
    return None


def report(results):
    """The comparison table."""
    labels = [label for _, label, _ in OPTIONS]
    burnup = results[OPTIONS[0][0]]["bu_U"]

    print("Restructured volume fraction alpha_r")
    print()
    print("  bu [MWd/kgU] |  " + " | ".join("%-27s" % l for l in labels))
    print("  " + "-" * (15 + 30 * len(labels)))
    for target in (20, 40, 60, 80, 100, 140, 180):
        k = min(range(len(burnup)), key=lambda i: abs(burnup[i] - target))
        print("  %12.1f |  " % burnup[k]
              + " | ".join("%-27.6f" % results[o]["alpha"][k] for o, _, _ in OPTIONS))

    print()
    print("burnup [MWd/kgU] at which alpha_r first reaches")
    print()
    print("  threshold    |  " + " | ".join("%-27s" % l for l in labels))
    print("  " + "-" * (15 + 30 * len(labels)))
    for threshold in (0.01, 0.10, 0.50, 0.90, 0.99):
        cells = []
        for option, _, _ in OPTIONS:
            hit = crossing(results[option]["bu_U"], results[option]["alpha"], threshold)
            cells.append("%-27s" % ("never" if hit is None else "%.1f" % hit))
        print("  %12.2f |  " % threshold + " | ".join(cells))

    print()
    print("HBS porosity: the transient where alpha_r flattens, and the end state")
    print()
    print("  quantity     |  " + " | ".join("%-27s" % l for l in labels))
    print("  " + "-" * (15 + 30 * len(labels)))
    peaks, dips, finals = [], [], []
    for option, _, _ in OPTIONS:
        bu, por = results[option]["bu_U"], results[option]["porosity"]
        high, low = overshoot(bu, por)
        if high is None:
            peaks.append("none")
            dips.append("+0.0 %")
        else:
            peaks.append("%.6f at %.1f" % (por[high], bu[high]))
            dips.append("%+.1f %% to %.6f at %.1f"
                        % (100.0 * (por[low] - por[high]) / por[high], por[low], bu[low]))
        finals.append("%.6f" % por[-1])
    for name, cells in (("local max", peaks), ("dip after it", dips), ("final", finals)):
        print("  %-12s |  " % name + " | ".join("%-27s" % c for c in cells))

    spread = [results[o]["porosity"][-1] for o, _, _ in OPTIONS]
    print()
    print("  the four options end within %.2f %% of the same porosity"
          % (100.0 * (max(spread) - min(spread)) / min(spread)))


def overshoot(bu, por, tolerance=1.0e-4, floor_fraction=0.05):
    """(peak index, following trough index) of the porosity overshoot, or (None, None).

    Not the maximum over a window.  In every one of these runs the porosity ends
    HIGHER than the overshoot, so the maximum of any window wide enough to contain the
    corner lands on the window's own right edge and the dip measures as exactly zero -
    which is what this function is here to stop reporting.

    What is wanted is the highest point that the curve later falls below: among the
    indices whose value is undercut afterwards, the one with the largest porosity, and
    then the minimum that follows it.  `floor_fraction` ignores the numerical ripple
    of the moment solver at the start, where the porosity is still a rounding error.
    """
    ceiling = max(por)
    if ceiling <= 0.0:
        return None, None
    floor = floor_fraction * ceiling

    # suffix minimum, so the scan below stays linear
    suffix_min = [0.0] * len(por)
    running = por[-1]
    for i in range(len(por) - 1, -1, -1):
        running = min(running, por[i])
        suffix_min[i] = running

    peak = None
    for i in range(len(por) - 1):
        if por[i] <= floor:
            continue
        if suffix_min[i + 1] < por[i] * (1.0 - tolerance):
            if peak is None or por[i] > por[peak]:
                peak = i
    if peak is None:
        return None, None
    trough = min(range(peak + 1, len(por)), key=lambda i: por[i])
    return peak, trough


def plot(results, path):
    """The three-panel figure. Needs matplotlib."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    figure, axes = plt.subplots(1, 3, figsize=(14.0, 4.2))
    for option, label, colour in OPTIONS:
        width = 2.2 if option == 4 else 1.4
        axes[0].plot(results[option]["bu_U"], results[option]["alpha"],
                     color=colour, lw=width, label=label)
        axes[1].plot(results[option]["bu_U"], results[option]["porosity"],
                     color=colour, lw=width)
    axes[0].set_ylabel(r"restructured fraction  $\alpha_r$  [-]")
    axes[0].legend(fontsize=8, loc="lower right")
    axes[1].set_ylabel("HBS porosity  [-]")

    landau = results[4]
    axes[2].plot(landau["bu_U"], landau["theta"], "k-", lw=2.2)
    axes[2].set_ylabel(r"mean misorientation  $\Theta$  [deg]   (option 4 only)")
    twin = axes[2].twinx()
    twin.plot(landau["bu_U"], [r * 1e6 for r in landau["radius"]], "r--", lw=1.6)
    twin.set_ylabel(r"subgrain radius  $r_n$  [$\mu$m]", color="r")
    twin.set_ylim(0.0, 2.0)
    twin.tick_params(axis="y", colors="r")

    for axis in axes:
        axis.set_xlabel("local burnup  [MWd/kgU]")
    figure.suptitle("HBS formation options on the same irradiation, "
                    "porosity model 3 throughout")
    figure.tight_layout()
    os.makedirs(os.path.dirname(path), exist_ok=True)
    figure.savefig(path, dpi=140)
    print()
    print("written: %s" % path)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--case", default=DEFAULT_CASE,
                        help="regression case to take the history from "
                             "(default: regression/hbs/test_UO2HBS)")
    parser.add_argument("--steps", type=int, default=1500,
                        help="Number_of_time_steps_per_interval (default 1500)")
    parser.add_argument("--figure", default=os.path.join(HERE, "figures", "formation_options.png"),
                        help="where to write the figure")
    parser.add_argument("--no-plot", action="store_true", help="table only")
    arguments = parser.parse_args(argv)

    if not os.path.exists(SCIANTIX):
        raise SystemExit("%s not found -- build SCIANTIX first (./Allmake.sh)" % SCIANTIX)

    results = {}
    with tempfile.TemporaryDirectory(prefix="hbs_formation_") as workdir:
        for option, label, _ in OPTIONS:
            case = os.path.join(workdir, "formation_%d" % option)
            build_case(arguments.case, case, option, 3, arguments.steps)
            print("running option %d ..." % option, flush=True)
            outcome = subprocess.run([SCIANTIX, case + os.sep],
                                     stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
            if outcome.returncode != 0 or not os.path.exists(os.path.join(case, "output.txt")):
                raise SystemExit("option %d failed:\n%s"
                                 % (option, outcome.stderr.decode(errors="replace")))
            columns = read_output(case)
            results[option] = {
                "bu_U": [b / UO2_TO_U for b in columns["Burnup (MWd/kgUO2)"]],
                "alpha": columns["Restructured volume fraction (/)"],
                "porosity": columns["HBS porosity (/)"],
                "theta": columns.get("Mean misorientation (deg)"),
                "radius": columns.get("Subgrain radius (m)"),
            }

    print()
    print("history from %s, %d time steps per interval, iHighBurnupStructurePorosity = 3"
          % (os.path.relpath(arguments.case, ROOT), arguments.steps))
    print()
    report(results)
    if not arguments.no_plot:
        try:
            plot(results, arguments.figure)
        except ImportError:
            print("\n(matplotlib not available, figure skipped)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
