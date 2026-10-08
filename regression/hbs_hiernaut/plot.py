"""
sciantix regression suite -- HBS fragmentation against the Hiernaut (2008) anneal used by Kulacsy (2015, Sec. 4.2)

Cases test_UO2HBS_frag_hiernaut_{outer,inner}_<option>, figures into figures/<option>/ and figures/summary.png:

    plot_hiernaut_release.png   Xe released during the anneal vs temperature: outer part, inner part and their
                                Xe-weighted sum, against the measured release of the whole sample (Kulacsy Fig. 9)
    plot_hiernaut_state.png     HBS porosity over the base irradiation, fragmented fraction D and burst release G vs T

The release is the fraction of the gas produced released after the end of the base irradiation (the release of the
base irradiation is subtracted, as in hbs_nfir/plot.py); the curve starts before the confining stress of the base irradiation (40 / 60 MPa [P])
is released to 0.1 MPa, so a burst on unloading at 600 K appears as a step at the first point. The sample is 1/3 outer (160 MWd/kgU, 2.7 wt% Xe) and
2/3 inner (105 MWd/kgU, 1.5 wt% Xe) by volume [P, Kulacsy Sec. 4.2]; the sum weights each part by its Xe inventory [R].
"""

import os

import matplotlib.pyplot as plt
import numpy as np

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
DATA_DIR = os.path.join(SCRIPT_DIR, "data")
OPTIONS = ["jernkvist", "kulacsy", "lefm"]
UO2_TO_HM = 0.8814
W_OUTER = (1.0 / 3.0 * 2.7) / (1.0 / 3.0 * 2.7 + 2.0 / 3.0 * 1.5)


def read_xy(name):
    return np.loadtxt(os.path.join(DATA_DIR, name), delimiter=";", comments="#", skiprows=2, encoding="utf-8")


def load(case):
    d = np.genfromtxt(os.path.join(SCRIPT_DIR, case, "output.txt"), delimiter="\t", names=True, dtype=float, encoding="utf-8", invalid_raise=False)
    names = d.dtype.names
    col = lambda key: d[[n for n in names if n.startswith(key)][0]]
    t, T = col("Time"), col("Temperature")
    t_base = np.loadtxt(os.path.join(SCRIPT_DIR, case, "input_history.txt"))[1, 0]  # end of the base irradiation
    i0 = np.searchsorted(t, t_base)  # before the confining stress is removed
    j = i0 + int(np.argmax(T[i0:] <= 600.0 + 1e-6))  # start of the ramp: the unloading burst, if any, is the value here
    out = dict(T=T[j:], base=slice(0, i0), col=col)
    out["rel"] = 100.0 * (col("Fission_gas_release")[j:] - col("Fission_gas_release")[i0])
    out["base_fgr"] = 100.0 * col("Fission_gas_release")[i0]
    out["D"], out["G"] = col("HBS_fragmented_fraction")[j:], col("HBS_burst_release_fraction")[j:]
    out["bu"] = col("Burnup")[:i0] / UO2_TO_HM
    out["por"] = 100.0 * col("HBS_porosity")[:i0]
    return out


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


def main():
    meas = read_xy("MeasuredRelease.txt")
    grid = np.linspace(600.0, 2300.0, 600)
    summary = {}
    for opt in OPTIONS:
        fig_dir = os.path.join(SCRIPT_DIR, "figures", opt)
        os.makedirs(fig_dir, exist_ok=True)
        r = {p: load("test_UO2HBS_frag_hiernaut_%s_%s" % (p, opt)) for p in ("outer", "inner")}
        curve = {p: np.interp(grid, r[p]["T"], r[p]["rel"]) for p in r}
        tot = W_OUTER * curve["outer"] + (1 - W_OUTER) * curve["inner"]
        summary[opt] = (curve, tot)

        fig, ax = plt.subplots(figsize=(6.5, 4.2))
        ax.plot(meas[:, 0], meas[:, 1], "ks", ms=4, label="Measured, whole sample (Fig. 9)")
        ax.plot(grid, curve["outer"], "-", label="SCIANTIX outer (base FGR %.1f %%)" % r["outer"]["base_fgr"])
        ax.plot(grid, curve["inner"], "-", label="SCIANTIX inner (base FGR %.0f %%)" % r["inner"]["base_fgr"])
        ax.plot(grid, tot, "k--", label="SCIANTIX sum (outer weight %.2f)" % W_OUTER)
        ax.set_xlim(600, 2000); ax.set_ylim(0, 100)
        ax.set_xlabel("Temperature (K)"); ax.set_ylabel("Release during the anneal (% of produced)"); ax.legend(frameon=False, fontsize=8)
        fig.tight_layout(); fig.savefig(os.path.join(fig_dir, "plot_hiernaut_release.png"), dpi=200); plt.close(fig)

        fig, axs = plt.subplots(1, 3, figsize=(13, 4))
        for p, ls in (("outer", "-"), ("inner", "--")):
            axs[0].plot(r[p]["bu"], r[p]["por"], ls, label=p)
            axs[1].plot(r[p]["T"], r[p]["D"], ls, label=p)
            axs[2].plot(r[p]["T"], r[p]["G"], ls, label=p)
        axs[0].plot([160, 105], [17, 10], "k*", ms=9, label="Kulacsy input (17, 10 %)")
        axs[0].set_xlabel("Burnup (MWd/kgHM)"); axs[0].set_ylabel("HBS porosity (%)")
        axs[1].set_xlabel("Temperature (K)"); axs[1].set_ylabel("Fragmented fraction D")
        axs[2].set_xlabel("Temperature (K)"); axs[2].set_ylabel("Burst release fraction G")
        for a in axs: a.legend(frameon=False, fontsize=8)
        fig.tight_layout(); fig.savefig(os.path.join(fig_dir, "plot_hiernaut_state.png"), dpi=200); plt.close(fig)
        print("%-10s base FGR outer %.1f %% inner %.1f %%; sum at 900/1100/1500/2000 K: %s" % (
            opt, r["outer"]["base_fgr"], r["inner"]["base_fgr"], " ".join("%.0f" % np.interp(k, grid, tot) for k in (900, 1100, 1500, 2000))))

    fig, axs = plt.subplots(1, 3, figsize=(14, 4), sharey=True)
    for a, (title, key) in zip(axs, (("Outer part", 0), ("Inner part", 1), ("Whole sample", 2))):
        for opt in OPTIONS:
            curve, tot = summary[opt]
            a.plot(grid, [curve["outer"], curve["inner"], tot][key], label=opt)
        if key == 2: a.plot(meas[:, 0], meas[:, 1], "ks", ms=4, label="Measured")
        a.set_title(title); a.set_xlim(600, 2000); a.set_xlabel("Temperature (K)")
    axs[0].set_ylabel("Release during the anneal (% of produced)"); axs[2].legend(frameon=False, fontsize=8)
    fig.tight_layout(); fig.savefig(os.path.join(SCRIPT_DIR, "figures", "summary.png"), dpi=200); plt.close(fig)

    for part in ("outer", "inner"):
        case = "test_UO2HBS_frag_hiernaut_%s_kulacsy" % part
        history(case, "hiernaut_" + part, np.loadtxt(os.path.join(SCRIPT_DIR, case, "input_history.txt"))[1, 0], 60.0, "min")


if __name__ == "__main__":
    main()
