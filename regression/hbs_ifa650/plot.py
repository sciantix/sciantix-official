"""
sciantix regression suite -- HBS fragmentation against the Halden LOCA tests IFA-650.9 and IFA-650.10 (Jernkvist 2019, Sec. 3.2.2)

Cases test_UO2HBS_frag_{ifa6509,ifa65010}_<option>: one rim point per rod, base irradiation at the rim local burnup with the
LHGR history of Jernkvist Fig. 8, 8 h of preconditioning, then the typical IFA-650 transient of FFRD Fig. 2.1-2 (cladding temperature
and rod pressure, burst at 298 s) for both rods.
Figures into figures/<option>/ and figures/summary.png:

    plot_ifa650_<rod>.png   rim temperature and hydrostatic pressure, fragmented fraction D, rim-weighted rodlet pulverisation,
                            local rim release, vs time after the start of the blowdown
    figures/summary.png     D vs time for both rods and the three options, against the rodlet outcome

The rodlet-average pulverisation is D times the rim area fraction [?]: 0.44 for 650.9 (HBS from r/Rp = 0.75, Jernkvist Fig. 9) and
0.024 for 650.10 (50 um rim, 4.1 mm pellet radius). Jernkvist's calculated 41 % (650.9) and 10 % (650.10) are model results [E]; the 10 % of
650.10 comes from non-restructured material and is outside the HBS model.
"""

import os

import matplotlib.pyplot as plt
import numpy as np

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
OPTIONS = ["jernkvist", "kulacsy", "lefm", "j19damage"]
RODS = {"ifa6509": dict(label="IFA-650.9", rim=0.44, ref=41.0, burst=298.0), "ifa65010": dict(label="IFA-650.10", rim=0.024, ref=10.0, burst=298.0)}


def load(case):
    d = np.genfromtxt(os.path.join(SCRIPT_DIR, case, "output.txt"), delimiter="\t", names=True, dtype=float, encoding="utf-8", invalid_raise=False)
    names = d.dtype.names
    col = lambda key: d[[n for n in names if n.startswith(key)][0]]
    t = col("Time")
    hist = np.loadtxt(os.path.join(SCRIPT_DIR, case, "input_history.txt"))
    t_test = hist[-1, 0] - 1000.0 / 3600.0  # the history ends 1000 s after the start of the blowdown (FFRD Fig. 2.1-2)
    i_test = np.searchsorted(t, t_test - 300.0 / 3600.0)  # the transient starts 300 s before the blowdown
    s = slice(i_test, None)
    return dict(t=(t[s] - t_test) * 3600.0, T=col("Temperature")[s], P=-col("Hydrostatic_stress")[s], D=col("HBS_fragmented_fraction")[s],
                G=col("HBS_burst_release_fraction")[s], rel=100.0 * (col("Fission_gas_release")[s] - col("Fission_gas_release")[i_test]),
                pre=100.0 * col("Fission_gas_release")[i_test], por=100.0 * col("HBS_porosity")[i_test], D_pre=col("HBS_fragmented_fraction")[i_test])


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
    data = {}
    for rod, info in RODS.items():
        for opt in OPTIONS:
            fig_dir = os.path.join(SCRIPT_DIR, "figures", opt)
            os.makedirs(fig_dir, exist_ok=True)
            r = data[(rod, opt)] = load("test_UO2HBS_frag_%s_%s" % (rod, opt))
            fig, axs = plt.subplots(1, 3, figsize=(14, 4))
            axs[0].plot(r["t"], r["T"], "C3-"); axs[0].set_ylabel("Rim temperature (K)", color="C3")
            ax2 = axs[0].twinx(); ax2.plot(r["t"], r["P"], "C0--"); ax2.set_ylabel("Confining pressure (MPa)", color="C0")
            axs[1].plot(r["t"], r["D"], label="D (rim)"); axs[1].plot(r["t"], info["rim"] * r["D"], label="rodlet-average pulverisation (rim-weighted)")
            axs[1].axhline(info["ref"] / 100.0, color="k", ls=":", label="Jernkvist, calculated (%.0f %%)" % info["ref"])
            axs[1].set_ylabel("Fraction"); axs[1].legend(frameon=False, fontsize=7)
            axs[2].plot(r["t"], r["rel"], label="local rim release"); axs[2].plot(r["t"], 100.0 * r["G"], "--", label="G x 100")
            axs[2].set_ylabel("Release (% of rim gas produced)"); axs[2].legend(frameon=False, fontsize=8)
            for a in (axs[0], axs[1], axs[2]):
                a.set_xlabel("Time after blowdown start (s)"); a.axvline(info["burst"], color="gray", ls=":")
            fig.suptitle("%s, %s (rim porosity before the test %.1f %%, rim FGR before the test %.1f %%)" % (info["label"], opt, r["por"], r["pre"]), fontsize=9)
            fig.tight_layout(); fig.savefig(os.path.join(fig_dir, "plot_ifa650_%s.png" % rod), dpi=200); plt.close(fig)
            tb = np.searchsorted(r["t"], info["burst"] - 1.0)
            print("%-9s %-10s pre: porosity %.1f %% D %.2f FGR %.1f %% | D at start of burst-1s %.2f, end %.2f; release end %.1f %%" % (
                info["label"], opt, r["por"], r["D_pre"], r["pre"], r["D"][tb], r["D"][-1], r["rel"][-1]))

    fig, axs = plt.subplots(1, 2, figsize=(12, 4))
    for a, (rod, info) in zip(axs, RODS.items()):
        for opt in OPTIONS:
            a.plot(data[(rod, opt)]["t"], data[(rod, opt)]["D"], label=opt)
        a.axvline(info["burst"], color="gray", ls=":", label="cladding burst")
        a.set_title(info["label"]); a.set_xlabel("Time after blowdown start (s)"); a.set_ylabel("Rim fragmented fraction D")
    axs[0].legend(frameon=False, fontsize=8)
    fig.tight_layout(); fig.savefig(os.path.join(SCRIPT_DIR, "figures", "summary.png"), dpi=200); plt.close(fig)

    for rod in RODS:
        case = "test_UO2HBS_frag_%s_kulacsy" % rod
        t_blow = np.loadtxt(os.path.join(SCRIPT_DIR, case, "input_history.txt"))[-1, 0] - 1000.0 / 3600.0
        history(case, rod, t_blow, 3600.0, "s after the blowdown", window_h=300.0 / 3600.0)


if __name__ == "__main__":
    main()
