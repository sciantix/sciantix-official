"""
sciantix regression suite -- HBS fragmentation figures

Compares test_UO2HBS_frag_NFIR with the NFIR annealing data in ``data_frag``
(see data_frag/ReadMe), into ``figures/<option>/``:

    plot_frag_NFIR_temperature.png   annealing temperature history, input vs Fig. 9
    plot_frag_NFIR_release.png       cumulative and instantaneous release vs Fig. 9
    plot_frag_NFIR_pores.png         HBS pore size before/after vs Fig. 12
    plot_frag_NFIR_readme.png        porosity, Xe in grain, FGR, pore pressure vs ReadMe data
    plot_frag_NFIR_xe.png            Xe repartition among the reservoirs vs burnup
    figures/summary.png              (all three options) cumulative release vs Fig. 9, and pore state at the
                                     end of the base irradiation vs the ReadMe data

Run the case first (build/sciantix.x in the case folder), then this script.
The release is the fraction of the gas produced released during the anneal only
(the release of the base irradiation is subtracted). The model tracks the mean
and variance of the Xe atoms per pore, from which a Gaussian diameter distribution is drawn in Fig. 12.
"""

import os

import matplotlib.pyplot as plt
import numpy as np

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
DATA_DIR = os.path.join(SCRIPT_DIR, "data")


CASES = ["test_UO2HBS_frag_jernkvist", "test_UO2HBS_frag_kulacsy", "test_UO2HBS_frag_lefm", "test_UO2HBS_frag_j19damage"]

# Start of the annealing in the input history (h): the 20 C point after the base irradiation.
T_ANNEAL_START_H = 38000.01
BIN_UM = 0.2
XE_EQUIVALENT = 4.88897e26  # Xe atoms per m3 for 1 wt% retention (as in plot.py)
UO2_TO_HM = 0.8814  # burnup is written in MWd/kgUO2, plotted in MWd/kgHM


def read_xy(name):
    return np.loadtxt(os.path.join(DATA_DIR, name), delimiter=";", comments="#", skiprows=2, encoding="utf-8")


def read_bars(name):
    with open(os.path.join(DATA_DIR, name), encoding="utf-8") as f:
        return np.array([float(l.split(";")[1]) for l in f if l.startswith("Bar")])


def main(FIG_DIR, CASE):
    os.makedirs(FIG_DIR, exist_ok=True)
    d = np.genfromtxt(os.path.join(CASE, "output.txt"), delimiter="\t", names=True, dtype=float, encoding="utf-8", invalid_raise=False)
    names = d.dtype.names
    col = lambda key: d[[n for n in names if n.startswith(key)][0]]

    t = col("Time")
    T = col("Temperature") - 273.15
    fgr = col("Fission_gas_release") * 100.0
    sel = t >= T_ANNEAL_START_H
    tm = (t[sel] - T_ANNEAL_START_H) * 60.0
    rel = fgr[sel] - fgr[sel][0]
    keep = np.concatenate(([True], np.diff(tm) > 1e-9))
    tm_u, rel_u = tm[keep], rel[keep]
    inst = np.gradient(rel_u, tm_u)

    # --- temperature
    tt = read_xy("TemperatureAnnealing.txt")
    fig, ax = plt.subplots(figsize=(6, 4))
    ax.plot(tt[:, 0], tt[:, 1], "o", ms=3, mfc="none", label="Data (Fig. 9)")
    ax.plot(tm, T[sel], "-", label="SCIANTIX input")
    ax.set_xlabel("Time (min)"); ax.set_ylabel("Temperature (°C)"); ax.legend(frameon=False)
    fig.tight_layout(); fig.savefig(os.path.join(FIG_DIR, "plot_frag_NFIR_temperature.png"), dpi=200); plt.close(fig)

    # --- release
    cum = read_xy("CumulativeRelease.txt")
    ins = read_xy("InstantaneousRelease.txt")
    fig, (a1, a2) = plt.subplots(1, 2, figsize=(11, 4))
    a1.plot(cum[:, 0], cum[:, 1], "o", ms=3, mfc="none", label="Kr-85 (Fig. 9)")
    a1.plot(tm, rel, "-", label="SCIANTIX")
    a1.set_xlabel("Time (min)"); a1.set_ylabel("Cumulative release (% of produced)"); a1.legend(frameon=False)
    a2.plot(ins[:, 0], ins[:, 1], "o", ms=3, mfc="none", label="Kr-85 (Fig. 9)")
    a2.plot(tm_u, inst, "-", label="SCIANTIX")
    a2.set_xlabel("Time (min)"); a2.set_ylabel("Instantaneous release (%/min)"); a2.legend(frameon=False)
    fig.tight_layout(); fig.savefig(os.path.join(FIG_DIR, "plot_frag_NFIR_release.png"), dpi=200); plt.close(fig)

    # --- pore size distribution. The model gives mean and variance of the Xe atoms per pore;
    # the diameter distribution is taken Gaussian with sigma_R = R * CV / 3 (as in plot.py),
    # and scaled so that its bin sum is the model HBS porosity, as the bars sum to the porosity.
    before, after = read_bars("AsIrradiated.txt"), read_bars("PostAnnealing.txt")
    edges = np.arange(len(before) + 1) * BIN_UM
    centers = 0.5 * (edges[:-1] + edges[1:])
    i0, i1 = np.where(sel)[0][0], -1
    fig, ax = plt.subplots(figsize=(6.5, 4))
    w = 0.4 * BIN_UM
    ax.bar(centers - w / 2, before, width=w, label="As irradiated (Fig. 12)", color="C0", alpha=0.6)
    ax.bar(centers + w / 2, after, width=w, label="After anneal (Fig. 12)", color="C1", alpha=0.6)
    xf = np.linspace(0, edges[-1], 400)
    for i, c, lab in ((i0, "C0", "SCIANTIX, start of anneal"), (i1, "C1", "SCIANTIX, end of anneal")):
        R = col("HBS_pore_radius")[i]
        CV = np.sqrt(max(col("Xe_atoms_per_HBS_pore__variance")[i], 0.0)) / col("Xe_atoms_per_HBS_pore_a")[i]
        mu, sig = 2e6 * R, 2e6 * R * CV / 3.0
        phi = 100.0 * col("HBS_porosity")[i]
        ax.plot(xf, phi * BIN_UM * np.exp(-0.5 * ((xf - mu) / sig) ** 2) / (sig * np.sqrt(2 * np.pi)), "--", color=c, label=lab)
        print("%s: mean diameter %.3f um, sigma %.3f um, porosity %.1f %%" % (lab, mu, sig, phi))
    ax.set_xlabel("Pore diameter (µm)"); ax.set_ylabel("Surface per 0.2 µm bin (%)")
    ax.legend(frameon=False, fontsize=8)
    fig.tight_layout(); fig.savefig(os.path.join(FIG_DIR, "plot_frag_NFIR_pores.png"), dpi=200); plt.close(fig)

    # --- other data from the ReadMe, at the end of the base irradiation (103.5 MWd/kgHM)
    base = t <= T_ANNEAL_START_H
    bu = col("Burnup")[base] / UO2_TO_HM
    fig, axs = plt.subplots(2, 2, figsize=(10, 7))
    (a1, a2), (a3, a4) = axs
    a1.plot(bu, 100 * col("HBS_porosity")[base], label="SCIANTIX, HBS porosity")
    a1.plot(103.5, 11, "k*", ms=9, label="SEM, 11 %")
    a1.set_ylabel("Porosity (%)")
    a2.plot(bu, (col("Xe_in_grain_atm3")[base] + col("Xe_in_grain_HBS_atm3")[base]) / XE_EQUIVALENT, label="SCIANTIX, Xe in grain")
    a2.plot(103.5, 0.2, "k*", ms=9, label="Measured in the grains, 0.2 wt%")
    a2.set_ylabel("Xe in grain (wt%)")
    a3.plot(bu, 100 * col("Fission_gas_release")[base], label="SCIANTIX")
    a3.plot(103.5, 2.9, "k*", ms=9, label="Puncturing, 2.9 %")
    a3.set_ylabel("Fission gas release (%)")
    hi = bu > 60  # below this the pores are too few and small for the pressure to mean anything
    a4.plot(bu[hi], col("HBS_pore_pressure")[base][hi] / 1e6, label="SCIANTIX (630 °C)")
    a4.plot(103.5, 133, "k*", ms=9, label="EoS from data, 133 MPa at 700 °C")
    a4.set_ylabel("HBS pore pressure (MPa)")
    for a in axs.flat:
        a.set_xlabel("Burnup (MWd/kgHM)"); a.legend(frameon=False, fontsize=8)
    fig.tight_layout(); fig.savefig(os.path.join(FIG_DIR, "plot_frag_NFIR_readme.png"), dpi=200); plt.close(fig)

    # --- Xe repartition over the base irradiation, as fractions of the Xe produced. The production is
    # split in two variables (outside / inside the restructured volume): the total is their sum, the
    # base of the model's own fission gas release. The HBS pores are a reservoir of their own.
    prod = (d[[n for n in names if n.startswith("Xe_produced_atm3")][0]]
            + d[[n for n in names if n.startswith("Xe_produced_in_HBS")][0]])[base]
    get = lambda key: d[[n for n in names if n.startswith(key)][0]][base] / np.where(prod > 0, prod, np.nan)
    reservoirs = [
        ("Grain, non-HBS", get("Xe_in_grain_atm3")),
        ("Grain, HBS", get("Xe_in_grain_HBS")),
        ("Grain boundary, non-HBS", get("Xe_at_grain_boundary_atm3")),
        ("Grain boundary, HBS", get("Xe_at_grain_boundary_HBS")),
        ("HBS pores", get("Xe_in_HBS_pores")),
        ("Released", get("Xe_released")),
    ]
    fig, ax = plt.subplots(figsize=(6, 4))
    ax.stackplot(bu, *[np.nan_to_num(v) for _, v in reservoirs], labels=[n for n, _ in reservoirs])
    retained = 1 - reservoirs[-1][1][-1]
    ax.plot(103.5, 0.88 * retained, "k*", ms=9, label="Data: 88% of retained Xe in HBS bubbles")
    ax.set_xlabel("Burnup (MWd/kgHM)"); ax.set_ylabel("Fraction of Xe produced (/)")
    ax.set_xlim(bu[bu > 0].min() if (bu > 0).any() else 0, bu.max()); ax.set_ylim(0, 1.05)
    ax.legend(frameon=False, fontsize=8, loc="center left", bbox_to_anchor=(1.0, 0.5))
    fig.tight_layout(); fig.savefig(os.path.join(FIG_DIR, "plot_frag_NFIR_xe.png"), dpi=200); plt.close(fig)
    print("Xe fraction sum (reservoirs, pores included) at end of base irradiation: %.3f" % sum(np.nan_to_num(v)[-1] for _, v in reservoirs))


def history(case, name, t_start_h, unit, unit_label, window_h=None):
    """Input history of a case (identical for the three options): the whole history, and the transient from t_start_h (in unit_label)."""
    h = np.loadtxt(os.path.join(SCRIPT_DIR, case, "input_history.txt"))
    t, T, F, P = h[:, 0], h[:, 1], h[:, 2], -h[:, 3]
    fig, axs = plt.subplots(2, 3, figsize=(13, 6.5))
    for row, (x, lab, sel) in enumerate(((t, "Time (h)", np.ones_like(t, dtype=bool)),
                                          ((t - t_start_h) * unit, "Time (%s)" % unit_label, t >= (t_start_h if window_h is None else t_start_h - window_h)))):
        for a, (y, ylab) in zip(axs[row], ((T, "Temperature (K)"), (F, "Fission rate (fiss/m3 s)"), (P, "Confining pressure (MPa)"))):
            a.plot(x[sel], y[sel], "-o" if sel.sum() < 60 else "-", ms=3)
            a.set_xlabel(lab); a.set_ylabel(ylab)
    axs[0, 1].set_title("%s: whole history" % name); axs[1, 1].set_title("transient")
    fig.tight_layout(); fig.savefig(os.path.join(SCRIPT_DIR, "figures", "plot_history_%s.png" % name), dpi=200); plt.close(fig)


def summary(FIG_DIR):
    """The three options against the annealing data and the ReadMe data, on one page."""
    os.makedirs(FIG_DIR, exist_ok=True)
    cum = read_xy("CumulativeRelease.txt")
    fig, (a1, a2, a3) = plt.subplots(1, 3, figsize=(15, 4.2))
    a1.plot(cum[:, 0], cum[:, 1], "ko", ms=3, mfc="none", label="Kr-85 (Fig. 9)")
    for case in CASES:
        d = np.genfromtxt(os.path.join(SCRIPT_DIR, case, "output.txt"), delimiter="\t", names=True, dtype=float,
                          encoding="utf-8", invalid_raise=False)
        names = d.dtype.names
        col = lambda key: d[[n for n in names if n.startswith(key)][0]]
        t, fgr = col("Time"), col("Fission_gas_release") * 100.0
        sel = t >= T_ANNEAL_START_H
        a1.plot((t[sel] - T_ANNEAL_START_H) * 60.0, fgr[sel] - fgr[sel][0], label=case.replace("test_UO2HBS_frag_", ""))
        base = t <= T_ANNEAL_START_H
        bu = col("Burnup")[base] / UO2_TO_HM
        a2.plot(bu, 100 * col("HBS_porosity")[base], label=case.replace("test_UO2HBS_frag_", ""))
        hi = bu > 60
        a3.plot(bu[hi], col("HBS_pore_pressure")[base][hi] / 1e6)
    a1.set_xlabel("Time (min)"); a1.set_ylabel("Cumulative release (% of produced)")
    a2.plot(103.5, 11, "k*", ms=9, label="SEM, 11 %"); a2.set_xlabel("Burnup (MWd/kgHM)"); a2.set_ylabel("HBS porosity (%)")
    a3.plot(103.5, 133, "k*", ms=9, label="EoS from data, 133 MPa at 700 °C")
    a3.set_xlabel("Burnup (MWd/kgHM)"); a3.set_ylabel("HBS pore pressure (MPa, at the irradiation T)")
    a3.legend(frameon=False, fontsize=8)
    a1.legend(frameon=False, fontsize=8); a2.legend(frameon=False, fontsize=8)
    fig.tight_layout(); fig.savefig(os.path.join(FIG_DIR, "summary.png"), dpi=200); plt.close(fig)


if __name__ == "__main__":

    for case in CASES:
        FIG_DIR = os.path.join(SCRIPT_DIR, "figures", case.replace("test_UO2HBS_frag_", ""))
        CASE = os.path.join(SCRIPT_DIR, case)
        main(FIG_DIR, CASE)
    summary(os.path.join(SCRIPT_DIR, "figures"))
    history("test_UO2HBS_frag_kulacsy", "NFIR", T_ANNEAL_START_H, 60.0, "min")
