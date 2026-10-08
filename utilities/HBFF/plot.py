"""
Local analysis/plotting script for the HBS fragmentation options 1-4 (regression/hbs_nfir and regression/hbs_ifa650, test_UO2HBS_frag_*).

NOT a reference implementation of the model: it only reads output.txt columns and re-evaluates the closed-form
equations already documented in the header comment of src/models/HighBurnupStructureFragmentation.C, purely to
draw the figures below. This file is intentionally left untracked (see .gitignore or just do not `git add` it).

Regenerates, into utilities/HBFF/figures/:
    criteria_comparison.png              critical pressure of the four options against the pore radius, and the pressure shapes
    frag_ifa650.png                      D, G of the four options in the IFA-650.9 and .10 transients
    frag_cases.png                       D, G, retention, fragment size of the three options, against T (transient)
    pore_state_burnup.png                N_p, p_g, P_cr (matching option) against burnup (base irradiation)
    pore_state_temperature.png           N_p, p_g, P_cr (matching option) against T (transient)
    pore_distribution_burnup.png         pore radius/pressure bands + size PDF snapshots, against burnup
    pore_distribution_temperature.png    pore radius/pressure bands + size PDF snapshots, against T
    nfirv.png                            the three criteria against the NFIR-V annealing test (prescribed state)
    release_budget.png                   Xe produced/retained in the HBS pores and total/HBS release, against time
    pressure_time.png                    p_g, P_cr, P_h against real time: full transient + zoom on the depressurization
    kulacsy_class_breakdown.png          option 2's pore-class gas content and broken/intact state, a few snapshot T

usage: python3 utilities/HBFF/plot.py [--case NAME] [--figure NAME]
    (no arguments: regenerate everything, for every case where relevant)
"""

import argparse
import os
import sys

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", ".."))
FIG_DIR = os.path.join(ROOT, "utilities", "HBFF", "figures")
CASES = ["jernkvist", "kulacsy", "lefm", "j19damage"]
T_ANNEAL_START_H = 38000.005  # (h) end of the base irradiation in the NFIR history: the anneal starts at 38000.01

# validated categorical + sequential palette (dataviz skill, references/palette.md), light mode
BLUE, ORANGE, YELLOW = "#2a78d6", "#eb6834", "#eda100"
SEQ_BLUE = ["#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281"]
INK, INK2, MUTED, GRID = "#0b0b0b", "#52514e", "#898781", "#e1e0d9"
SURFACE = "#fcfcfb"
COLORS = {"jernkvist": BLUE, "kulacsy": ORANGE, "lefm": YELLOW, "j19damage": "#2f9e6e"}
LABELS = {"jernkvist": "1 Jernkvist (2019)", "kulacsy": "2 Kulacsy (2015)", "lefm": "3 LEFM", "j19damage": "4 J19 + damage"}

GAMMA_HBS, POISSON = 1.1, 0.32  # UO2HBS matrix, SetMatrix.C (fuel_.getSurfaceTension/getPoissonRatio)
SIGMA_HBS_CR = 21.0e6  # (Pa) option 1, J19 Table 4
G_HBS, Y_FACTOR = 0.055, 1.0  # option 3, G_hbs fitted on the NFIR Kr-85 release
C_DP = 55.0  # (N/m) option 4, J19 Table 4


def case_path(case, name=None):
    if name is None:
        return os.path.join(ROOT, "regression", "hbs_nfir", f"test_UO2HBS_frag_{case}", "output.txt")
    return os.path.join(ROOT, "regression", "hbs_ifa650", f"test_UO2HBS_frag_{name}_{case}", "output.txt")


def read(path):
    with open(path) as fh:
        names = [n for n in fh.readline().rstrip("\n").split("\t") if n.strip()]
    d = np.genfromtxt(path, skip_header=1)
    col = {n.split(" (")[0]: i for i, n in enumerate(names)}
    return {k: d[:, i] for k, i in col.items()}


def irradiation_segment(d):
    return 0, int(np.argmax(d["Time"] >= T_ANNEAL_START_H))


def transient_segment(d):
    i0 = int(np.argmax(d["Time"] >= T_ANNEAL_START_H))
    return i0, len(d["Time"])


def style(ax):
    ax.set_facecolor(SURFACE)
    for spine in ("top", "right"):
        ax.spines[spine].set_visible(False)
    for spine in ("left", "bottom"):
        ax.spines[spine].set_color(MUTED)
    ax.tick_params(colors=INK2, labelsize=9)
    ax.grid(True, color=GRID, linewidth=0.7)
    ax.set_axisbelow(True)


def kulacsy_sigma_f(T, xi):
    return 170.0e6 * np.exp(-191.34 / np.minimum(T, 1000.0)) * np.sqrt(np.maximum(1.0 - 2.62 * xi, 0.0))


def elastic_modulus(T, xi, Bu):
    return 2.237e11 * (1.0 - 2.6 * xi) * (1.0 - 1.394e-4 * (T - 293.0)) * (1.0 - 0.1506 * (1.0 - np.exp(-0.035 * Bu)))


def critical_pressure(option, s):
    T, Ph, xi, R, Bu = s["T"], s["Ph"], s["xi"], s["R"], s["Bu"]
    Ps = 2.0 * GAMMA_HBS / R
    if option in ("jernkvist", "j19damage"):
        return Ps + (SIGMA_HBS_CR * (1.0 - xi) + Ph) / xi
    if option == "lefm":
        E = elastic_modulus(T, xi, Bu)
        return Ph + Ps + (1.0 / (2.0 * Y_FACTOR)) * np.sqrt(np.pi * E * G_HBS / ((1.0 - POISSON**2) * R))
    if option == "kulacsy":
        # median-class threshold [K15 Eq. 7, 15], for display only: not a population probability like the others
        r_med = np.exp(-0.5) * 1.0e-6
        gam = 0.41 * (0.85 - 1.4e-4 * T) * (1.0 - xi) ** 4.025
        return kulacsy_sigma_f(T, xi) + 2.0 * gam / r_med + 1.5 * Ph
    raise ValueError(option)


def state(d):
    return {"T": d["Temperature"], "Ph": -d["Hydrostatic stress"] * 1.0e6, "xi": d["HBS porosity"],
            "R": d["HBS pore radius"], "Bu": d["Effective burnup"] / 0.8814}


def cv_n(d):
    n, varn = d["Xe atoms per HBS pore"], d["Xe atoms per HBS pore - variance"]
    return np.where(n > 0, np.sqrt(np.maximum(varn, 0.0)) / np.where(n > 0, n, 1.0), 0.0)


def sigma_lnR(d):
    cv_r = cv_n(d) / 3.0
    return np.sqrt(np.log(1.0 + cv_r**2))


def savefig(fig, name):
    os.makedirs(FIG_DIR, exist_ok=True)
    path = os.path.join(FIG_DIR, name)
    fig.tight_layout()
    fig.savefig(path, dpi=150, facecolor=SURFACE, bbox_inches="tight")
    plt.close(fig)
    print("wrote", path)


# ------------------------------------------------------------------------------------------------------------
def plot_frag_cases():
    fig, ax = plt.subplots(1, 4, figsize=(17, 3.8))
    for case in CASES:
        d = read(case_path(case))
        i0, i1 = transient_segment(d)
        T = d["Temperature"][i0:i1]
        ax[0].plot(T, d["HBS fragmented fraction"][i0:i1], color=COLORS[case], label=LABELS[case])
        ax[1].plot(T, d["HBS burst release fraction"][i0:i1], color=COLORS[case])
        ax[2].plot(T, d["HBS gas retention fraction"][i0:i1], color=COLORS[case])
        dsz = d["HBS fragment size"][i0:i1] * 1e6
        ax[3].plot(T[dsz > 0], dsz[dsz > 0], color=COLORS[case])
    titles = ("D (broken pores)", "G (released gas)", "S (gas retention)", "fragment size (um)")
    for a, t in zip(ax, titles):
        style(a)
        a.set_xlabel("T (K)", color=INK2)
        a.set_title(t, color=INK, fontsize=11, loc="left")
    ax[3].set_yscale("log")
    ax[0].legend(frameon=False, fontsize=8)
    savefig(fig, "frag_cases.png")


def plot_release_budget():
    fig, axes = plt.subplots(2, 4, figsize=(17, 7.2))
    for j, case in enumerate(CASES):
        d = read(case_path(case))
        i0, i1 = transient_segment(d)
        t = d["Time"][i0:i1] - d["Time"][i0]

        # "Xe in HBS pores" and "Xe released" are on different bases (pore-only vs. whole-pellet,
        # all release mechanisms) and are not in a ceiling/subset relation with each other or with
        # "Xe produced in HBS" (pore gas also comes from material swept into the HBS on
        # restructuring, not only from post-restructuring production): plotted together as two
        # independent absolute inventories, not as a budget that must sum to one total.
        ax0 = axes[0, j]
        style(ax0)
        # solid lines for both: some options vent in a single step, and a dashed pattern can render
        # with no visible dash across a jump that sharp
        ax0.plot(t, d["Xe in HBS pores"][i0:i1], color=COLORS[case], linewidth=1.8, label="in HBS pores (retained)")
        ax0.plot(t, d["Xe released"][i0:i1], color=INK2, linewidth=1.3, label="released (all mechanisms)")
        ax0.set_title(LABELS[case], color=INK, fontsize=10, loc="left")
        if j == 0:
            ax0.set_ylabel("Xe (at/m$^3$)", color=INK2, fontsize=9)
            ax0.legend(frameon=False, fontsize=7, loc="center left")

        ax1 = axes[1, j]
        style(ax1)
        ax1.plot(t, 100 * d["Fission gas release"][i0:i1], color=INK2, linewidth=1.3,
                 label="total FGR (all mechanisms)")
        ax1.plot(t, 100 * d["HBS burst release fraction"][i0:i1], color=COLORS[case], linewidth=1.8,
                 label="G (HBS pore gas vented)")
        ax1.set_xlabel("time since transient start (h)", color=INK2, fontsize=9)
        if j == 0:
            ax1.set_ylabel("release (%)", color=INK2, fontsize=9)
            ax1.legend(frameon=False, fontsize=7, loc="upper left")
    fig.suptitle("Xe budget over the NFIR anneal: where the HBS-produced gas sits, and how much is released",
                 color=INK, fontsize=10, y=1.0)
    savefig(fig, "release_budget.png")


def plot_pressure_time():
    zoom_hi = 0.05  # (h) covers the 0.01 h P_h depressurization with margin
    fig, axes = plt.subplots(2, 4, figsize=(17, 7.2))
    for j, case in enumerate(CASES):
        d = read(case_path(case))
        i0, i1 = transient_segment(d)
        t = d["Time"][i0:i1] - d["Time"][i0]
        s = state(d)
        sc = {k: v[i0:i1] for k, v in s.items()}
        ok = (sc["R"] >= 0.1e-6) & (sc["xi"] > 0.0) & (sc["xi"] < 0.35)
        pg = np.where(ok, d["HBS pore pressure"][i0:i1] / 1.0e6, np.nan)
        pcr = np.where(ok, critical_pressure(case, sc) / 1.0e6, np.nan)
        ph = sc["Ph"] / 1.0e6

        for row, hi in enumerate((t.max(), zoom_hi)):
            ax = axes[row, j]
            style(ax)
            mask = t <= hi
            ax.plot(t[mask], pg[mask], color=COLORS[case], linewidth=1.8, label="$p_g$")
            ax.plot(t[mask], pcr[mask], color=COLORS[case], linewidth=1.2, linestyle="--", label="$P_{cr}$")
            ax.plot(t[mask], ph[mask], color=MUTED, linewidth=1.2, linestyle=":", label="$P_h$")
            if row == 0:
                ax.set_title(LABELS[case], color=INK, fontsize=10, loc="left")
            else:
                ax.set_xlabel("time since transient start (h)", color=INK2, fontsize=9)
            if j == 0:
                ax.set_ylabel("pressure (MPa)" + (" — zoom on depressurization" if row == 1 else ""),
                             color=INK2, fontsize=8)
            if row == 0 and j == 0:
                ax.legend(frameon=False, fontsize=7)
    savefig(fig, "pressure_time.png")


def plot_pore_state(x_key, x_label, segment, out_name, log_scale):
    fig, axes = plt.subplots(3, 1, figsize=(7.2, 9.0))
    for case in CASES:
        d = read(case_path(case))
        i0, i1 = segment(d)
        sl = slice(i0, i1)
        s = state(d)
        x = s[x_key][sl]
        sc = {k: v[sl] for k, v in s.items()}
        ok = (sc["R"] >= 0.1e-6) & (sc["xi"] > 0.0) & (sc["xi"] < 0.35)
        pcr = critical_pressure(case, sc) / 1.0e6
        pg = d["HBS pore pressure"][sl] / 1.0e6
        axes[0].plot(x, d["HBS pore density"][sl], color=COLORS[case], label=LABELS[case])
        axes[1].plot(x, np.where(ok, pg, np.nan), color=COLORS[case])
        axes[2].plot(x, np.where(ok, pcr, np.nan), color=COLORS[case])
    titles = ["pore number density $N_p$", "pore pressure $p_g$", "critical pressure $P_{cr}$ (matching option)"]
    ylabels = ["$N_p$ (pores/m$^3$)", "$p_g$ (MPa)", "$P_{cr}$ (MPa)"]
    for ax, title, yl in zip(axes, titles, ylabels):
        style(ax)
        ax.set_title(title, color=INK, fontsize=11, loc="left")
        ax.set_ylabel(yl, color=INK2, fontsize=9)
        ax.set_xlabel(x_label, color=INK2, fontsize=9)
    if log_scale:
        axes[1].set_yscale("log")
        axes[2].set_yscale("log")
    handles, labels = axes[0].get_legend_handles_labels()
    leg = fig.legend(handles, labels, loc="lower center", ncol=2, frameon=False, fontsize=9, bbox_to_anchor=(0.5, 0.98))
    fig.tight_layout(rect=(0, 0, 1, 0.93))
    fig.savefig(os.path.join(FIG_DIR, out_name), dpi=150, facecolor=SURFACE, bbox_extra_artists=(leg,),
                bbox_inches="tight")
    plt.close(fig)
    print("wrote", os.path.join(FIG_DIR, out_name))


def plot_pore_distribution(x_key, x_label, segment, snapshot_x, out_name, case, log_r):
    d = read(case_path(case))
    i0, i1 = segment(d)
    valid = (d["HBS pore radius"] >= 0.1e-6) & (d["HBS porosity"] > 0.0) & (d["HBS porosity"] < 0.35)
    sl = slice(i0, i1)
    x = (d["Effective burnup"] / 0.8814 if x_key == "Bu" else d["Temperature"])[sl]
    ok = valid[sl]
    R = d["HBS pore radius"][sl] * 1.0e6
    Np = d["HBS pore density"][sl]
    pg = d["HBS pore pressure"][sl] / 1.0e6
    sig = sigma_lnR(d)[sl]
    cvn = cv_n(d)[sl]

    fig, axes = plt.subplots(3, 1, figsize=(7.2, 9.6))

    ax = axes[0]
    style(ax)
    Rlo, Rhi = np.where(ok, R * np.exp(-sig), np.nan), np.where(ok, R * np.exp(sig), np.nan)
    ax.fill_between(x, Rlo, Rhi, color=BLUE, alpha=0.18, linewidth=0)
    ax.plot(x, np.where(ok, R, np.nan), color=BLUE, linewidth=1.8)
    ax.set_title("pore radius: mean $\\pm\\,1$ log-$\\sigma$ band", color=INK, fontsize=11, loc="left")
    ax.set_ylabel("$R$ (um)", color=INK2, fontsize=9)
    ax.set_xlabel(x_label, color=INK2, fontsize=9)

    ax = axes[1]
    style(ax)
    plo = np.clip(np.where(ok, pg * (1.0 - cvn), np.nan), 0.0, None)
    phi = np.where(ok, pg * (1.0 + cvn), np.nan)
    ax.fill_between(x, plo, phi, color=BLUE, alpha=0.18, linewidth=0)
    ax.plot(x, np.where(ok, pg, np.nan), color=BLUE, linewidth=1.8)
    ax.set_yscale("log")
    ax.set_title("pore pressure: mean $\\pm\\,1\\,\\mathrm{CV}_n$ band", color=INK, fontsize=11, loc="left")
    ax.set_ylabel("$p_g$ (MPa)", color=INK2, fontsize=9)
    ax.set_xlabel(x_label, color=INK2, fontsize=9)

    ax = axes[2]
    style(ax)
    r_lo, r_hi = R[ok].min(), R[ok].max()
    r_grid = (np.geomspace(max(0.05, 0.2 * r_lo), 3.0 * r_hi, 400) if log_r
              else np.linspace(max(0.0, 0.5 * r_lo), 1.5 * r_hi, 400))
    for j, xv in enumerate(snapshot_x):
        k = int(np.argmin(np.abs(x - xv)))
        if not ok[k] or sig[k] <= 0.0:
            continue
        pdf = (Np[k] / (r_grid * sig[k] * np.sqrt(2 * np.pi))) * np.exp(
            -((np.log(r_grid) - np.log(R[k])) ** 2) / (2 * sig[k] ** 2))
        unit = " MWd/kgUO$_2$" if x_key == "Bu" else " K"
        ax.plot(r_grid, pdf, color=SEQ_BLUE[j], linewidth=1.8, label=f"{xv:.0f}{unit}  ($N_p$={Np[k]:.1e} m$^{{-3}}$)")
    if log_r:
        ax.set_xscale("log")
    ax.set_title("pore population: number density over radius", color=INK, fontsize=11, loc="left")
    ax.set_xlabel("$R$ (um)", color=INK2, fontsize=9)
    ax.set_ylabel("dN$_p$/dR (pores m$^{-3}$ um$^{-1}$)", color=INK2, fontsize=9)
    ax.legend(frameon=False, fontsize=8, loc="upper right")

    fig.suptitle(f"HBS pore state ({case} case)", color=INK, fontsize=11, y=0.995)
    fig.tight_layout(rect=(0, 0, 1, 0.97))
    fig.savefig(os.path.join(FIG_DIR, out_name), dpi=150, facecolor=SURFACE, bbox_inches="tight")
    plt.close(fig)
    print("wrote", os.path.join(FIG_DIR, out_name))


def lognormal_classes(median, sigma, n_nodes=81, span=4.0):
    x = np.linspace(-span, span, n_nodes)
    w = np.exp(-0.5 * x**2)
    return median * np.exp(sigma * x), w / w.sum()


def vdw_atoms_from_pressure(p, V, T):
    a_w, b_w = 1.17e-48, 8.49e-29
    lo, hi = 0.0, 0.99 * V / b_w
    for _ in range(100):
        mid = 0.5 * (lo + hi)
        pmid = mid * 1.380651e-23 * T / (V - mid * b_w) - a_w * mid * mid / V**2 if mid > 0 else 0.0
        if pmid < p:
            lo = mid
        else:
            hi = mid
    return 0.5 * (lo + hi)


def kulacsy_classes(T, xi, Ph, T_ss, Ph_ss):
    """Per-class radius, weight, gas content and broken/intact state [P] K15, same equations as
    HighBurnupStructureFragmentation.C case 2. Returns None if no class is stable."""
    radii, w = lognormal_classes(np.exp(-0.5) * 1e-6, 0.356)
    gamma_ss = 0.41 * (0.85 - 1.4e-4 * T_ss)
    e_ss = 2.334e11 * (1.0 - 1.0915e-4 * T_ss)
    g_ss = e_ss / (2.0 * (1.0 + POISSON))
    r_min = g_ss * 0.39e-9 / (kulacsy_sigma_f(T_ss, xi) + 0.5 * Ph_ss)
    keep = radii >= r_min
    if not keep.any():
        return None
    radii, w = radii[keep], w[keep] / w[keep].sum()
    V = 4.0 / 3.0 * np.pi * radii**3
    p0 = Ph_ss + 2.0 * gamma_ss / radii + g_ss * 0.39e-9 / radii
    n = np.array([vdw_atoms_from_pressure(p0[i], V[i], T_ss) for i in range(len(radii))])
    gam = 0.41 * (0.85 - 1.4e-4 * T) * (1.0 - xi) ** 4.025
    # forward van der Waals EoS at the transient T (n was found above by inverting it at T_ss)
    p = n * 1.380651e-23 * T / (V - n * 8.49e-29) - 1.17e-48 * n * n / V**2
    sigma_t = p - 2.0 * gam / radii - 1.5 * Ph
    broken = sigma_t > kulacsy_sigma_f(T, xi)
    return radii, w, n, broken


def kulacsy_release_population(T, xi, Ph, T_ss, Ph_ss):
    """Full log-normal population [P] K15: gas-weighted released fraction."""
    out = kulacsy_classes(T, xi, Ph, T_ss, Ph_ss)
    if out is None:
        return 0.0
    _, w, n, broken = out
    gas = w * n
    return float((gas * broken).sum() / gas.sum())


def plot_kulacsy_classes():
    # only option 2 resolves pressure per pore-size class (limitation L1: the others assume a
    # uniform pressure); this is the one place a genuine pressure/gas *distribution* over the pore
    # population exists in the model, at the state of the kulacsy regression case's own transient:
    # T_ss = 723 K, Ph_ss = 0 (the transient starts right after the P_h relief, at constant T).
    T_SS, PH_SS, PH = 903.15, 70.0e6, 0.0
    snapshots = [800.0, 1000.0, 1200.0, 1500.0, 1800.0]

    d = read(case_path("kulacsy"))
    i0, i1 = transient_segment(d)
    T_arr = d["Temperature"][i0:i1]
    xi_arr = d["HBS porosity"][i0:i1]

    fig, axes = plt.subplots(1, len(snapshots), figsize=(3.4 * len(snapshots), 3.8), sharey=True)
    for ax, T_snap in zip(axes, snapshots):
        style(ax)
        k = int(np.argmin(np.abs(T_arr - T_snap)))
        out = kulacsy_classes(T_arr[k], xi_arr[k], PH, T_SS, PH_SS)
        if out is None:
            ax.set_title(f"T = {T_snap:.0f} K\n(no stable pore class)", fontsize=9, color=INK, loc="left")
            continue
        radii, w, n, broken = out
        r_um = radii * 1.0e6
        gas = w * n
        colors = np.where(broken, ORANGE, BLUE)
        ax.bar(r_um, gas, width=0.05 * r_um, color=colors, alpha=0.85, linewidth=0)
        released = 100 * gas[broken].sum() / gas.sum()
        ax.set_title(f"T = {T_snap:.0f} K, released {released:.0f}%", fontsize=9, color=INK, loc="left")
        ax.set_xlabel("$R$ (um)", color=INK2, fontsize=9)
    axes[0].set_ylabel("gas per class, $w_i\\,n_i$ (at)", color=INK2, fontsize=9)
    fig.suptitle("Kulacsy (2015): pore-class gas content over the NFIR anneal (orange = broken class)",
                 color=INK, fontsize=10, y=1.02)
    savefig(fig, "kulacsy_class_breakdown.png")


def plot_nfirv():
    from math import erfc, log, sqrt

    XI, R_MEAN, CV_D, ATOM_VOLUME, BURNUP = 0.11, 0.86e-6, 0.32, 1.518e-28, 91.2
    T_SS, PH_SS = 750.0, 0.0  # [?] irradiation state of the NFIR-V disc: not reported, see README B5
    T_C = np.array([870, 900, 930, 960, 990, 1020, 1050, 1090, 1130, 1160, 1190], dtype=float)
    FGR = np.array([0.1, 0.4, 0.8, 1.4, 2.4, 3.8, 5.7, 7.6, 11.8, 16.7, 22.8])

    def hs_pressure(n, V, T):
        d_xe = 4.45e-10 * (0.8542 - 0.03996 * np.log(T / 231.2))
        v_xe = np.pi / 6.0 * d_xe**3
        eta = min(n * v_xe / V, 0.65)
        Z = (1 + eta + eta**2 - eta**3) / (1 - eta) ** 3
        return n * 1.380651e-23 * T * Z / V

    def state_at(T):
        V = 4.0 / 3.0 * np.pi * R_MEAN**3
        n = V / ATOM_VOLUME
        cv_n_ = 3.0 * sqrt(np.exp(log(1.0 + CV_D**2)) - 1.0)
        return {"T": T, "Ph": 0.0, "xi": XI, "R": R_MEAN, "Bu": BURNUP, "n": n, "varn": (cv_n_ * n) ** 2,
                "pg": hs_pressure(n, V, T)}

    def release(option, T_kelvin):
        out = []
        for T in T_kelvin:
            s = state_at(T)
            Ps = 2.0 * GAMMA_HBS / s["R"]
            if option == "jernkvist":
                pcr = Ps + (SIGMA_HBS_CR * (1.0 - s["xi"]) + s["Ph"]) / s["xi"]
                out.append(1.0 if s["pg"] >= pcr else 0.0)
            elif option == "kulacsy":
                out.append(kulacsy_release_population(T, s["xi"], s["Ph"], T_SS, PH_SS))
            elif option == "lefm":
                E = elastic_modulus(T, s["xi"], s["Bu"])
                c = 0.5 * sqrt(np.pi * E * G_HBS / (1 - POISSON**2))
                target = s["pg"] - s["Ph"]
                lo, hi = 1e-9, 1e-3
                for _ in range(80):
                    mid = sqrt(lo * hi)
                    if c / sqrt(mid) + 2 * GAMMA_HBS / mid >= target:
                        lo = mid
                    else:
                        hi = mid
                r_c = sqrt(lo * hi)
                cv_r = sqrt(s["varn"]) / s["n"] / 3.0
                sig = sqrt(log(1 + cv_r**2))
                z = (log(r_c) - log(s["R"])) / sig
                out.append(0.5 * erfc((z - 3 * sig) / sqrt(2.0)))
        return np.array(out)

    T_K = T_C + 273.15
    fig, ax = plt.subplots(figsize=(6.0, 4.0))
    T_fine = np.linspace(850.0, 1250.0, 200) + 273.15
    for case in CASES[:3]:
        ax.plot(T_fine - 273.15, 100 * release(case, T_fine), color=COLORS[case], label=LABELS[case])
    ax.plot(T_C, FGR, "k*", label="NFIR-V (OHP Fig. 6)")
    style(ax)
    ax.set_xlabel("temperature (C)", color=INK2)
    ax.set_ylabel("release (% of the pore gas)", color=INK2)
    ax.legend(frameon=False, fontsize=8)
    savefig(fig, "nfirv.png")


def plot_criteria_comparison():
    """Critical pressure against the pore radius at a fixed state, and the pressure shapes of the options."""
    xi, T, Bu = 0.105, 1200.0, 100.0
    R = np.geomspace(0.1e-6, 2.0e-6, 300)
    fig, axes = plt.subplots(1, 3, figsize=(16, 4.2))
    for ax, Ph in zip(axes[:2], (0.0, 70.0e6)):
        style(ax)
        Ps = 2.0 * GAMMA_HBS / R
        E = elastic_modulus(T, xi, Bu)
        ax.plot(R * 1e6, (Ps + (SIGMA_HBS_CR * (1 - xi) + Ph) / xi) / 1e6, color=COLORS["jernkvist"], label=LABELS["jernkvist"] + " / " + LABELS["j19damage"])
        gam = 0.41 * (0.85 - 1.4e-4 * T) * (1 - xi) ** 4.025
        ax.plot(R * 1e6, (kulacsy_sigma_f(T, xi) + 2 * gam / R + 1.5 * Ph) / 1e6, color=COLORS["kulacsy"], label=LABELS["kulacsy"])
        ax.plot(R * 1e6, (Ph + Ps + 0.5 * np.sqrt(np.pi * E * G_HBS / ((1 - POISSON**2) * R)) / Y_FACTOR) / 1e6, color=COLORS["lefm"], label=LABELS["lefm"])
        ax.set_yscale("log")
        ax.set_xscale("log")
        ax.set_xlabel("pore radius R (um)", color=INK2)
        ax.set_ylabel("critical pressure (MPa)", color=INK2)
        ax.set_title(f"$P_{{cr}}$, xi = {xi}, T = {T:.0f} K, $P_h$ = {Ph / 1e6:.0f} MPa", color=INK, fontsize=10, loc="left")
    axes[0].legend(frameon=False, fontsize=8)
    ax = axes[2]
    style(ax)
    G_SHEAR, B = 70.0e9, 0.39e-9
    ax.plot(R * 1e6, G_SHEAR * B / R / 1e6, color=COLORS["kulacsy"], label="K15: $Gb/R$ (kappa = 1)")
    ax.plot(R * 1e6, C_DP / R / 1e6, color=COLORS["j19damage"], label="option 4 / J19: $c_{dp}/R$ (kappa = 1)")
    ax.plot(R * 1e6, np.full_like(R, 100.0), color=COLORS["lefm"], label="option 3: uniform overpressure (example)")
    ax.set_xscale("log")
    ax.set_yscale("log")
    ax.set_xlabel("pore radius R (um)", color=INK2)
    ax.set_ylabel("overpressure above $P_h + 2\\gamma/R$ (MPa)", color=INK2)
    ax.set_title("pressure shape over the pore classes", color=INK, fontsize=10, loc="left")
    ax.legend(frameon=False, fontsize=8)
    savefig(fig, "criteria_comparison.png")


def plot_frag_ifa650():
    fig, axes = plt.subplots(2, 2, figsize=(12, 7))
    for row, name in enumerate(("ifa6509", "ifa65010")):
        for case in CASES:
            d = read(case_path(case, name))
            hot = d["Fission rate"] < 0.5 * d["Fission rate"].max()
            i0 = int(np.argmax(hot & (d["Time"] > d["Time"][-1] - 1.0)))
            i1 = i0 + int(np.argmax(d["Temperature"][i0:]))  # heat-up only
            t = (d["Time"][i0:i1] - d["Time"][i0]) * 3600.0
            axes[row, 0].plot(t, d["HBS fragmented fraction"][i0:i1], color=COLORS[case], label=LABELS[case])
            axes[row, 1].plot(d["Temperature"][i0:i1], d["HBS burst release fraction"][i0:i1], color=COLORS[case])
        axes[row, 0].set_ylabel(f"{name}: D", color=INK2)
        axes[row, 1].set_ylabel("G", color=INK2)
        axes[row, 0].set_xlabel("time since the start of the transient (s)", color=INK2)
        axes[row, 1].set_xlabel("T (K)", color=INK2)
        for a in axes[row]:
            style(a)
    axes[0, 0].legend(frameon=False, fontsize=8)
    savefig(fig, "frag_ifa650.png")


FIGURES = {
    "criteria_comparison": plot_criteria_comparison,
    "frag_ifa650": plot_frag_ifa650,
    "frag_cases": plot_frag_cases,
    "pore_state_burnup": lambda: plot_pore_state("Bu", "burnup (MWd/kgUO$_2$)", irradiation_segment,
                                                  "pore_state_burnup.png", True),
    "pore_state_temperature": lambda: plot_pore_state("T", "temperature (K)", transient_segment,
                                                        "pore_state_temperature.png", True),
    "pore_distribution_burnup": lambda: plot_pore_distribution("Bu", "burnup (MWd/kgUO$_2$)", irradiation_segment,
                                                                [30, 50, 80, 100],
                                                                "pore_distribution_burnup.png", "jernkvist", True),
    "pore_distribution_temperature": lambda: plot_pore_distribution("T", "temperature (K)", transient_segment,
                                                                     [750, 1000, 1300],
                                                                     "pore_distribution_temperature.png", "kulacsy",
                                                                     False),
    "nfirv": plot_nfirv,
    "release_budget": plot_release_budget,
    "pressure_time": plot_pressure_time,
    "kulacsy_class_breakdown": plot_kulacsy_classes,
}


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--figure", choices=sorted(FIGURES), default=None, help="regenerate only this one")
    args = ap.parse_args()
    for name, fn in FIGURES.items():
        if args.figure and name != args.figure:
            continue
        fn()
    return 0


if __name__ == "__main__":
    sys.exit(main())
