"""
Cross-group parity plots, one figure per physical phenomenon rather than per
experimental campaign: how well the model holds on a given scale across
independent datasets.

Two shapes of experimental reference are supported:

  Scalar -- one measured value per case, listed in <group>/data/<file> and keyed
            either by case name (white, kashibe) or by temperature (baker,
            cornell); both resolve by substring match on the case directory.
  Curve  -- a measured X-Y curve per case, sitting inside the case directory.
            The calculated curve is linearly interpolated at each experimental
            abscissa, so one case contributes as many parity points as it has
            measurements.

Not covered here: oxygen potential, which has its own dedicated parity plot
over 323 cases (validation/oxygenpotential/combined_parity_plot.py), and JOG,
which has no scalar experimental reference to compare against.

author: Giovanni Zullo
"""
import os
import glob
import sys
import numpy as np
import matplotlib.pyplot as plt

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))
from testing.core.common import load_output

ROOT = os.path.dirname(os.path.abspath(__file__))

# Riglet-Martial data are given in wt%; this converts them to at/m3.
CR_CONVERSION = 52 * 100 / 6.022e23 / 1.07998e7
# The CONTACT experimental burnup is per kg of uranium, the code reports per kg of UO2.
UO2_TO_U = 1 / 0.8814
# Upper bound on markers drawn per source; statistics always use every point.
MAX_MARKERS = 400


def scalar(group, data_file):
    return dict(kind="scalar", group=group, data=data_file)


def curve(group, exp_file, xcol, exp_cols=(0, 1), exp_factor=1.0, xfactor=1.0, skip=0, only=""):
    """`only` restricts the source to cases whose name contains it: several groups
    keep a copy of the same experimental file in every case directory."""
    return dict(kind="curve", group=group, exp=exp_file, xcol=xcol, exp_cols=exp_cols,
                exp_factor=exp_factor, xfactor=xfactor, skip=skip, only=only)


# quantity -> (output column(s), factor on the calculated value, axis label)
QUANTITY = {
    "radius":        ("Intragranular bubble radius (m)", 1.0, "intragranular bubble radius (m)"),
    "density":       ("Intragranular bubble concentration (bub/m3)", 1.0, "intragranular bubble density (bub/m$^3$)"),
    "intra sw":      ("Intragranular gas bubble swelling (/)", 100.0, "intragranular gaseous swelling (%)"),
    "inter sw":      ("Intergranular gas swelling (/)", 100.0, "intergranular gaseous swelling (%)"),
    "fgr":           ("Fission gas release (/)", 100.0, "fission gas release (%)"),
    "Xe133 R/B":     ("Xe133 R/B (/)", 1.0, "$^{133}$Xe R/B (/)"),
    "Kr85m R/B":     ("Kr85m R/B (/)", 1.0, "$^{85m}$Kr R/B (/)"),
    "He release":    ("He fractional release (/)", 1.0, "helium fractional release (/)"),
    "HBS porosity":  ("HBS porosity (/)", 1.0, "HBS porosity (/)"),
    "HBS radius":    ("HBS pore radius (m)", 1.0, "HBS pore radius (m)"),
    "HBS density":   ("HBS pore density (pores/m3)", 1.0, "HBS pore density (pores/m$^3$)"),
    "Cr content":    (("Chromium solution (at/m3)", "Chromia solution (at/m3)"), 1.0,
                      "Cr content in the lattice (at/m$^3$)"),
}

TOPICS = {
    "intragranular": {
        "radius":   [scalar("baker", "ig_radius.txt"), scalar("cornell", "ig_radius.txt"),
                     scalar("kashibe", "intragranular_radius.txt")],
        "density":  [scalar("baker", "ig_density.txt"), scalar("cornell", "ig_density.txt"),
                     scalar("kashibe", "intragranular_density.txt")],
        "intra sw": [scalar("baker", "ig_swelling.txt")],
    },
    "intergranular": {
        "inter sw": [scalar("white", "ig_swelling.txt"), scalar("kashibe", "intergranular_swelling.txt")],
    },
    "fission_gas_release": {
        "fgr": [scalar("kashibe", "fgr.txt"),
                curve("chromium", "Killeen_exp.txt", "FIMA (%)", exp_cols=(1, 0), only="Killeen"),
                curve("contact", "experimental_fgr.txt", "Burnup (MWd/kgUO2)",
                      exp_factor=100.0, xfactor=UO2_TO_U, skip=1)],
    },
    "short_lived_gas": {
        "Xe133 R/B": [curve("contact", "experimental_RB_Xe133.txt", "Burnup (MWd/kgUO2)",
                            xfactor=UO2_TO_U, skip=1)],
        "Kr85m R/B": [curve("contact", "experimental_RB_Kr85m.txt", "Burnup (MWd/kgUO2)",
                            xfactor=UO2_TO_U, skip=1)],
    },
    "helium": {
        "He release": [curve("talip", "Talip2014_release_data.txt", "Time (h)")],
    },
    "hbs": {
        "HBS porosity": [curve("hbs", "exp_porosity.txt", "Burnup (MWd/kgUO2)")],
        "HBS radius":   [curve("hbs", "exp_pore_radius.txt", "Burnup (MWd/kgUO2)")],
        "HBS density":  [curve("hbs", "exp_pore_density.txt", "Burnup (MWd/kgUO2)")],
    },
    "chromium": {
        "Cr content": [curve("chromium", "Riglet-Martial_data_exp1.txt", "Temperature (K)",
                             exp_cols=(1, 0), exp_factor=1 / CR_CONVERSION, only="solubility1"),
                       curve("chromium", "Riglet-Martial_data_exp2.txt", "Temperature (K)",
                             exp_cols=(1, 0), exp_factor=1 / CR_CONVERSION, only="solubility2")],
    },
}

COLOR = {"baker": "tab:blue", "cornell": "tab:orange", "kashibe": "tab:green", "white": "tab:red",
         "chromium": "tab:purple", "contact": "tab:brown", "talip": "tab:cyan", "hbs": "tab:olive"}


def column(output, spec, factor):
    """Return a calculated column, summing the parts when spec is a tuple."""
    header = [h.strip() for h in output.header]
    names = (spec,) if isinstance(spec, str) else spec
    if any(n not in header for n in names):
        return None
    return sum(output.data[:, header.index(n)] for n in names) * factor


def from_scalar(source, spec, factor):
    """One parity point per case, from a group-level table of measured values."""
    cases = sorted(glob.glob(os.path.join(ROOT, source["group"], "test_*")))
    exp, calc = [], []
    with open(os.path.join(ROOT, source["group"], "data", source["data"])) as f:
        for line in f:
            if not line.strip() or line.lstrip().startswith("#"):
                continue
            key, value = line.split()[:2]
            match = [c for c in cases if key in os.path.basename(c)]
            if len(match) != 1:
                print(f"  [skip] {source['group']}/{key}: {len(match)} matching cases")
                continue
            y = column(load_output(match[0]), spec, factor)
            if y is None:
                continue
            exp.append(float(value))
            calc.append(y[-1])
    return np.array(exp), np.array(calc)


def from_curve(source, spec, factor):
    """One parity point per measurement, interpolating the calculated curve."""
    exp, calc = [], []
    for case in sorted(glob.glob(os.path.join(ROOT, source["group"], "test_*"))):
        if source["only"] not in os.path.basename(case):
            continue
        path = os.path.join(case, source["exp"])
        if not os.path.isfile(path):
            continue
        data = np.genfromtxt(path, skip_header=source["skip"])
        if data.ndim != 2 or not data.size:
            continue
        xi, yi = source["exp_cols"]
        exp_x, exp_y = data[:, xi], data[:, yi] * source["exp_factor"]

        output = load_output(case)
        sim_y = column(output, spec, factor)
        sim_x = column(output, source["xcol"], source["xfactor"])
        if sim_y is None or sim_x is None:
            continue

        order = np.argsort(sim_x)
        sim_x, sim_y = sim_x[order], sim_y[order]
        # Only compare where the simulation actually spans the measurement.
        inside = (exp_x >= sim_x[0]) & (exp_x <= sim_x[-1])
        exp.extend(exp_y[inside])
        calc.extend(np.interp(exp_x[inside], sim_x, sim_y))
    return np.array(exp), np.array(calc)


def panel(ax, quantity, sources):
    spec, factor, label = QUANTITY[quantity]
    lo, hi = np.inf, -np.inf
    seen = {}

    for source in sources:
        exp, calc = (from_scalar if source["kind"] == "scalar" else from_curve)(source, spec, factor)
        keep = np.isfinite(exp) & np.isfinite(calc) & (exp > 0) & (calc > 0)
        exp, calc = exp[keep], calc[keep]
        if not exp.size:
            print(f"  [empty] {source['group']} / {quantity}")
            continue
        group = source["group"]
        # A group contributing twice (two data files) is drawn once per file but labelled once.
        e, c = seen.get(group, (np.array([]), np.array([])))
        seen[group] = (np.concatenate([e, exp]), np.concatenate([c, calc]))
        lo, hi = min(lo, exp.min(), calc.min()), max(hi, exp.max(), calc.max())

    for group, (exp, calc) in seen.items():
        mad = 100 * np.median(np.abs(calc / exp - 1.0))
        series = f"{group} ({exp.size}), MAD {mad:.0f}%"
        # A densely sampled curve (talip is ~20k points) would draw as a blob:
        # thin it for display only, the statistic above uses every point.
        if exp.size > MAX_MARKERS:
            step = exp.size // MAX_MARKERS
            exp, calc = exp[::step], calc[::step]
        ax.plot(exp, calc, "o", ms=4, alpha=0.7, color=COLOR[group], label=series)

    if not np.isfinite(lo):
        ax.text(0.5, 0.5, f"no data\n{quantity}", ha="center", va="center", transform=ax.transAxes)
        return

    lo, hi = 0.5 * lo, 2.0 * hi
    line = np.array([lo, hi])
    ax.plot(line, line, "-", color="#777777", lw=0.9)
    ax.plot(line, 2 * line, "--", color="#777777", lw=0.7)
    ax.plot(line, 0.5 * line, "--", color="#777777", lw=0.7)
    ax.set(xscale="log", yscale="log", xlim=(lo, hi), ylim=(lo, hi),
           xlabel=f"experimental {label}", ylabel=f"calculated {label}")
    ax.set_aspect("equal")
    ax.grid(True, which="both", ls=":", alpha=0.4)
    ax.legend(fontsize=8, loc="upper left")


def main():
    outdir = os.path.join(ROOT, "figures")
    os.makedirs(outdir, exist_ok=True)

    for topic, quantities in TOPICS.items():
        print(f"== {topic}")
        fig, axes = plt.subplots(1, len(quantities), figsize=(5.2 * len(quantities), 5.2), squeeze=False)
        for ax, quantity in zip(axes[0], quantities):
            panel(ax, quantity, quantities[quantity])
        fig.tight_layout()
        path = os.path.join(outdir, f"parity_{topic}.png")
        fig.savefig(path, dpi=200)
        plt.close(fig)
        print(f"Saved: {path}")


if __name__ == "__main__":
    main()
