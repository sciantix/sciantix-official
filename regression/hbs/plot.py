"""
sciantix regression suite -- HBS figures
author: Giovanni Zullo

Produces the figures of the HBS manuscript (Zullo et al.) from the regression
cases, into ``regression/hbs/figures``:

    plot_pore_density.png   HBS pore number density vs burnup
    plot_porosity.png       HBS porosity vs burnup, with the Fokker-Planck band
    plot_pore_radius.png    HBS mean pore radius vs burnup, with the band
    plot_xe_depletion.png   Xe retention in grains vs Walker 1999
    plot_fuel_swelling.png  matrix swelling breakdown vs Spino 2006
    plot_pore_variance.png  second central moment B (diagnostic)
    plot_CV.png             coefficient of variation of the pore distribution
    plot_xe_inventory.png   xenon mass balance across the reservoirs

Two configurations are overlaid on the first five figures:

    test_UO2HBS      mechanistic cluster dynamics + KJMA with bu_inc = 15
    test_UO2HBS_0    semi-empirical porosity + KJMA without incubation

Both are read live from their case folder, so the reference curve always matches
what the twin case currently produces. Run the cases first (python3 -m
regression.runner --hbs), then this script. The last three figures rest on the
cluster-dynamics moments, which the semi-empirical case does not populate, and
are drawn from test_UO2HBS alone.

Experimental data and literature model curves live in ``regression/hbs/data``.
"""

import os
import sys

import matplotlib.pyplot as plt
import numpy as np
from scipy.signal import savgol_filter

# Add the project root to path so we can import regression.core
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from regression.core.common import SciantixOutput

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
DATA_DIR = os.path.join(SCRIPT_DIR, "data")
FIG_DIR = os.path.join(SCRIPT_DIR, "figures")
LIVE_CASE = os.path.join(SCRIPT_DIR, "test_UO2HBS")
REFERENCE_CASE = os.path.join(SCRIPT_DIR, "test_UO2HBS_0")

# Burnup is written in MWd/kgUO2 and plotted in MWd/kgHM.
UO2_TO_HM = 0.8814
# Xe atoms per m3 corresponding to 1 wt% retention.
XE_EQUIVALENT = 4.88897e26
# Solid fission product swelling prefactor (Olander), quoted as such in the
# manuscript. Single source of truth: the breakdown curves and the total must
# use the same value, otherwise the dashed curves do not add up to the solid one.
SOLID_FP_COEFF = 0.00303

LABEL_LIVE = "SCIANTIX, this work"
LABEL_REFERENCE = "SCIANTIX, semi-empirical"

# =============================================================================
# Plot style
# =============================================================================

FIG_SIZE = (8.0, 5.5)
DPI = 150

COLOR_CURRENT = "#2ca02c"    # this work      - test_UO2HBS
COLOR_REFERENCE = "#1f77b4"  # semi-empirical - test_UO2HBS_0
COLOR_BARANI = "#8b4513"     # Barani et al. 2022, JNM 563
COLOR_CAPPIA = "#555555"
COLOR_SPINO = "#c44545"
COLOR_SPINO_LIGHT = "#d98080"
COLOR_SPINO_DARK = "#7a2828"
COLOR_NOIROT = "#d62728"
COLOR_LASSMANN = "#ff7f0e"
COLOR_UNE_LOW = "#8e44ad"
COLOR_UNE_STRONG = "#e377c2"
COLOR_WALKER = "#1a3c6e"
COLOR_ALPHA = "#f39c12"
COLOR_LASSMANN_FIT = "#6a0dad"

MARKER_SIZE = 5
LINEWIDTH_MODEL = 2.0
LINEWIDTH_REF = 1.5

plt.rcParams.update({
    "figure.figsize": FIG_SIZE,
    "axes.labelsize": 11,
    "axes.titlesize": 12,
    "xtick.labelsize": 10,
    "ytick.labelsize": 10,
    "legend.fontsize": 9,
    "legend.frameon": True,
    "axes.grid": True,
    "grid.linestyle": "--",
    "grid.linewidth": 0.5,
    "grid.alpha": 0.6,
})

X_LABEL_BU = r"Burnup (MWd kgHM$^{-1}$)"


# =============================================================================
# Helpers
# =============================================================================

def new_axes(xlabel, ylabel):
    fig, ax = plt.subplots()
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    return fig, ax


def save(fig, filename):
    os.makedirs(FIG_DIR, exist_ok=True)
    out = os.path.join(FIG_DIR, filename)
    fig.savefig(out, dpi=DPI, bbox_inches="tight")
    plt.close(fig)
    print("Saved:", out)


def load_xy(filename, skip_header=0):
    """Two-column data file from regression/hbs/data. Empty array when absent."""
    path = os.path.join(DATA_DIR, filename)
    if not os.path.isfile(path):
        print(f"Warning: data file not found, series skipped: {path}")
        return np.empty((0, 2))
    return np.genfromtxt(path, skip_header=skip_header)


def lassmann_fit(bu, threshold=60.0):
    """
    K. Lassmann et al., JNM 226 (1995) 1-8. Xe retention (wt%) vs local burnup
    (MWd/kgHM): linear build-up up to ``threshold``, then exponential decay.
    """
    return np.where(
        bu < threshold,
        1.46e-2 * bu,
        1.46e-2 * (1.0 / 0.0584 + (60.0 - 1.0 / 0.0584) * np.exp(-0.0584 * (bu - 60.0))),
    )


def load_case(case_dir):
    """
    Read a case output into the series the figures need. Columns that the case
    does not produce come back as None: the semi-empirical reference has no
    cluster-dynamics moments and no HBS reservoir breakdown.
    """
    path = os.path.join(case_dir, "output.txt")
    if not os.path.isfile(path):
        print(f"Output not found, case skipped: {path}")
        return None

    out = SciantixOutput(path)
    headers = [h.strip() for h in out.header]

    def col(name):
        if name not in headers:
            return None
        return out.data[:, headers.index(name)].astype(float)

    case = {"name": os.path.basename(case_dir)}
    burnup = col("Burnup (MWd/kgUO2)")
    if burnup is None:
        print(f"No burnup column in {path}, case skipped")
        return None

    case["burnup"] = burnup / UO2_TO_HM
    case["poreDensity"] = col("HBS pore density (pores/m3)")
    case["porosity"] = col("HBS porosity (/)")
    case["poreRadius"] = col("HBS pore radius (m)")
    case["alpha"] = col("Restructured volume fraction (/)")
    case["xe_ig"] = col("Xe in grain (at/m3)")
    case["xe_igHBS"] = col("Xe in grain HBS (at/m3)")
    case["swe_igs"] = col("Intragranular gas solution swelling (/)")
    case["swe_igb"] = col("Intragranular gas bubble swelling (/)")
    case["fima"] = col("FIMA (%)")

    # Cluster-dynamics moments of the pore size distribution
    M2 = col("Xe atoms per HBS pore - variance (at^2/pore)")
    n_xe = col("Xe atoms per HBS pore (at/pore)")
    B = col("Xe in HBS pores - variance (at^2/m3)")
    if M2 is not None and n_xe is not None and B is not None:
        safe = (case["poreDensity"] > 0) & (n_xe > 0) & (case["poreRadius"] > 0)
        sigma_n = np.where(safe, np.sqrt(np.maximum(M2, 0.0)), 0.0)
        CV = np.zeros_like(sigma_n)
        np.divide(sigma_n, n_xe, out=CV, where=safe)
        case["B_raw"] = B
        case["CV"] = CV
        case["sigma_R"] = case["poreRadius"] * CV / 3.0
        case["sigma_xi"] = case["porosity"] * CV
    else:
        case["B_raw"] = case["CV"] = case["sigma_R"] = case["sigma_xi"] = None

    # Xenon inventory across the reservoirs
    reservoir_names = [
        "Xe produced (at/m3)", "Xe produced in HBS (at/m3)",
        "Xe in grain (at/m3)", "Xe in grain HBS (at/m3)",
        "Xe at grain boundary (at/m3)", "Xe at grain boundary HBS (at/m3)",
        "Xe in HBS pores (at/m3)", "Xe released (at/m3)",
    ]
    reservoirs = {name: col(name) for name in reservoir_names}
    case["xe_inventory"] = reservoirs if all(v is not None for v in reservoirs.values()) else None

    return case


# =============================================================================
# Figures
# =============================================================================

def plot_pore_density(live, reference):
    exp_cappia = load_xy("exp_pore_density.txt")
    exp_spino_67 = load_xy("exp_pore_density_2.txt")
    exp_spino_80 = load_xy("exp_pore_density_3.txt")
    exp_spino_98 = load_xy("exp_pore_density_4.txt")
    barani = load_xy("Barani_pore_density.txt")

    fig, ax = new_axes(X_LABEL_BU, r"Pore number density (pores m$^{-3}$)")
    ax.plot(exp_cappia[:, 0], exp_cappia[:, 1], "o", color=COLOR_CAPPIA,
            markersize=MARKER_SIZE, label="Cappia et al. 2016")
    ax.plot(exp_spino_67[:, 0], exp_spino_67[:, 1], "s", color=COLOR_SPINO_LIGHT,
            markersize=MARKER_SIZE, label=r"Spino et al. 2006, Bu$_\mathrm{AV}=67$ MWd/kgHM")
    ax.plot(exp_spino_80[:, 0], exp_spino_80[:, 1], "s", color=COLOR_SPINO,
            markersize=MARKER_SIZE, label=r"Spino et al. 2006, Bu$_\mathrm{AV}=80$ MWd/kgHM")
    ax.plot(exp_spino_98[:, 0], exp_spino_98[:, 1], "s", color=COLOR_SPINO_DARK,
            markersize=MARKER_SIZE, label=r"Spino et al. 2006, Bu$_\mathrm{AV}=98$ MWd/kgHM")
    ax.plot(barani[:, 0], barani[:, 1], "-", color=COLOR_BARANI,
            linewidth=LINEWIDTH_MODEL, label="Barani et al. 2022")
    if reference is not None:
        ax.plot(reference["burnup"], reference["poreDensity"], "-", color=COLOR_REFERENCE,
                linewidth=LINEWIDTH_MODEL, label=LABEL_REFERENCE)
    ax.plot(live["burnup"], live["poreDensity"], "-", color=COLOR_CURRENT,
            linewidth=LINEWIDTH_MODEL, label=LABEL_LIVE)
    ax.set_xlim(0, 210)
    ax.legend(loc="upper right")
    save(fig, "plot_pore_density.png")


def plot_porosity(live, reference):
    exp_cappia = load_xy("exp_porosity.txt")
    exp_spino = load_xy("exp_porosity_2.txt")
    exp_noirot = load_xy("exp_porosity_3.txt")
    exp_lassmann = load_xy("exp_porosity_4.txt")
    exp_une_low = load_xy("exp_porosity_5.txt")
    exp_une_strong = load_xy("exp_porosity_6.txt")
    barani = load_xy("Barani_porosity.txt")

    fig, ax = new_axes(X_LABEL_BU, "HBS porosity (/)")
    ax.plot(exp_cappia[:, 0], exp_cappia[:, 1], "o", color=COLOR_CAPPIA,
            markersize=MARKER_SIZE, label="Cappia et al. 2016")
    ax.plot(exp_spino[:, 0], exp_spino[:, 1], "s", color=COLOR_REFERENCE,
            markersize=MARKER_SIZE, label="Spino et al. 2006")
    ax.plot(exp_noirot[:, 0], exp_noirot[:, 1], "^", color=COLOR_NOIROT,
            markersize=MARKER_SIZE, label="Noirot et al. 2008")
    ax.plot(exp_lassmann[:, 0], exp_lassmann[:, 1], "D", color=COLOR_LASSMANN,
            markersize=MARKER_SIZE, label="Lassmann et al. 1995")
    ax.plot(exp_une_low[:, 0], exp_une_low[:, 1], "v", color=COLOR_UNE_LOW,
            markersize=MARKER_SIZE, label="Une et al. 2001, low PCMI")
    ax.plot(exp_une_strong[:, 0], exp_une_strong[:, 1], "v", color=COLOR_UNE_STRONG,
            markersize=MARKER_SIZE, label="Une et al. 2001, strong PCMI")
    ax.plot(barani[:, 0], barani[:, 1], "-", color=COLOR_BARANI,
            linewidth=LINEWIDTH_MODEL, label="Barani et al. 2022")
    if reference is not None:
        ax.plot(reference["burnup"], reference["porosity"], "-", color=COLOR_REFERENCE,
                linewidth=LINEWIDTH_MODEL, label=LABEL_REFERENCE)
    ax.plot(live["burnup"], live["porosity"], "-", color=COLOR_CURRENT,
            linewidth=LINEWIDTH_MODEL, label=LABEL_LIVE)
    if live["sigma_xi"] is not None:
        ax.fill_between(live["burnup"],
                        np.maximum(live["porosity"] - live["sigma_xi"], 0.0),
                        live["porosity"] + live["sigma_xi"],
                        alpha=0.20, color=COLOR_CURRENT, linewidth=0,
                        label=r"$\pm\sigma_\xi$ (Fokker-Planck)")
    ax.set_xlim(0, 210)
    ax.set_ylim(0, 0.27)
    ax.legend(loc="upper left", ncol=1)
    save(fig, "plot_porosity.png")


def plot_pore_radius(live, reference):
    exp_cappia = load_xy("exp_pore_radius.txt")
    exp_spino = load_xy("exp_pore_radius_2.txt")
    barani = load_xy("Barani_pore_radius.txt")

    fig, ax = new_axes(X_LABEL_BU, "Pore radius (m)")
    ax.plot(exp_cappia[:, 0], exp_cappia[:, 1], "o", color=COLOR_CAPPIA,
            markersize=MARKER_SIZE, label="Cappia et al. 2016")
    ax.plot(exp_spino[:, 0], exp_spino[:, 1], "s", color=COLOR_SPINO,
            markersize=MARKER_SIZE, label="Spino et al. 2006")
    ax.plot(barani[:, 0], barani[:, 1], "-", color=COLOR_BARANI,
            linewidth=LINEWIDTH_MODEL, label="Barani et al. 2022")
    if reference is not None:
        ax.plot(reference["burnup"], reference["poreRadius"], "-", color=COLOR_REFERENCE,
                linewidth=LINEWIDTH_MODEL, label=LABEL_REFERENCE)
    ax.plot(live["burnup"], live["poreRadius"], "-", color=COLOR_CURRENT,
            linewidth=LINEWIDTH_MODEL, label=LABEL_LIVE)
    if live["sigma_R"] is not None:
        ax.fill_between(live["burnup"],
                        np.maximum(live["poreRadius"] - live["sigma_R"], 0.0),
                        live["poreRadius"] + live["sigma_R"],
                        alpha=0.20, color=COLOR_CURRENT, linewidth=0,
                        label=r"$\pm\sigma_R$ (Fokker-Planck)")
    ax.set_xlim(0, 210)
    ax.legend(loc="upper left")
    save(fig, "plot_pore_radius.png")


def plot_xe_depletion(live, reference):
    walker = load_xy("walker_data_1999.txt", skip_header=1)

    bu_range = np.linspace(0.0, 200.0, 1000)

    fig, ax1 = plt.subplots()
    ax1.plot(bu_range, lassmann_fit(bu_range), "--", color=COLOR_LASSMANN_FIT,
             linewidth=LINEWIDTH_REF, label=r"Lassmann fit (Bu$_0=60$ MWd/kgHM)")
    if len(walker):
        ax1.scatter(walker[:, 0], walker[:, 1], color=COLOR_WALKER, edgecolors="black",
                    marker="o", s=22, label="Walker 1999")

    if reference is not None:
        total_ref = (reference["xe_ig"] + reference["xe_igHBS"]) / XE_EQUIVALENT
        ax1.plot(reference["burnup"], total_ref, "-", color=COLOR_REFERENCE,
                 linewidth=LINEWIDTH_MODEL, label=LABEL_REFERENCE + " (total)")

    ax1.plot(live["burnup"], live["xe_ig"] / XE_EQUIVALENT, "-.", color=COLOR_CURRENT,
             linewidth=LINEWIDTH_REF, label=LABEL_LIVE + ", NR")
    ax1.plot(live["burnup"], live["xe_igHBS"] / XE_EQUIVALENT, ":", color=COLOR_SPINO,
             linewidth=LINEWIDTH_REF, label=LABEL_LIVE + ", HBS")
    ax1.plot(live["burnup"], (live["xe_ig"] + live["xe_igHBS"]) / XE_EQUIVALENT, "-",
             color="#2c2c2c", linewidth=LINEWIDTH_MODEL, label=LABEL_LIVE + " (total)")
    ax1.set_xlabel("Burnup (MWd/kgHM)")
    ax1.set_ylabel("Xe in grains (wt%)")
    ax1.set_xlim(0, 200)
    ax1.set_ylim(0, 1.75)

    ax2 = ax1.twinx()
    ax2.plot(live["burnup"], live["alpha"], "-", color=COLOR_ALPHA,
             linewidth=LINEWIDTH_MODEL, label=r"Restructured fraction ($\alpha_r$, this work)")
    ax2.set_ylabel("Restructured volume fraction (/)")
    ax2.set_ylim(0.0, 1.05)
    ax2.grid(False)

    lines1, labels1 = ax1.get_legend_handles_labels()
    lines2, labels2 = ax2.get_legend_handles_labels()
    ax1.legend(lines1 + lines2, labels1 + labels2, loc="upper right", bbox_to_anchor=(1.0, 0.92))
    save(fig, "plot_xe_depletion.png")


def plot_fuel_swelling(live, reference):
    spino = load_xy("spino_swelling_data.txt", skip_header=1)

    fig, ax1 = plt.subplots()

    if reference is not None:
        total_ref = (reference["swe_igs"] + reference["swe_igb"]
                     + SOLID_FP_COEFF * reference["fima"])
        ax1.plot(reference["burnup"], total_ref, "-", color=COLOR_REFERENCE,
                 linewidth=LINEWIDTH_MODEL, label=LABEL_REFERENCE + " (total)")

    ax1.plot(live["burnup"], SOLID_FP_COEFF * live["fima"], "-.", color=COLOR_SPINO,
             linewidth=LINEWIDTH_REF, label="Solid fission products (Olander)")
    ax1.plot(live["burnup"], live["swe_igs"], "-.", color=COLOR_UNE_LOW,
             linewidth=LINEWIDTH_REF, label="Intra-granular gas in solution")
    ax1.plot(live["burnup"], live["swe_igb"], "-.", color=COLOR_CURRENT,
             linewidth=LINEWIDTH_REF, label="Intra-granular gas in bubbles")
    total_live = live["swe_igs"] + live["swe_igb"] + SOLID_FP_COEFF * live["fima"]
    ax1.plot(live["burnup"], total_live, "-", color="#1a365d",
             linewidth=LINEWIDTH_MODEL, label=LABEL_LIVE + " (total)")

    if len(spino):
        ax1.scatter(spino[:, 0], spino[:, 1], color=COLOR_WALKER, edgecolors="black",
                    marker="o", s=22, label="Spino et al. 2006")
    ax1.set_xlabel("Burnup (MWd/kgHM)")
    ax1.set_ylabel("Fuel matrix swelling (/)")
    ax1.set_xlim(0, 145)

    ax2 = ax1.twinx()
    ax2.plot(live["burnup"], live["alpha"], "--", color=COLOR_ALPHA,
             linewidth=LINEWIDTH_MODEL, label=r"Restructured fraction ($\alpha_r$, this work)")
    ax2.set_ylabel("Restructured volume fraction (/)")
    ax2.set_ylim(0.0, 1.05)
    ax2.grid(False)

    lines1, labels1 = ax1.get_legend_handles_labels()
    lines2, labels2 = ax2.get_legend_handles_labels()
    ax1.legend(lines1 + lines2, labels1 + labels2, loc="upper left")
    save(fig, "plot_fuel_swelling.png")


def plot_pore_variance(live):
    fig, ax = new_axes(X_LABEL_BU, r"Pore variance $B$ (at$^2$ m$^{-3}$)")
    ax.plot(live["burnup"], live["B_raw"], "-", color=COLOR_CURRENT,
            linewidth=LINEWIDTH_MODEL, label=r"$B = \sum c_n\,(n - \bar{n})^2$")
    ax.set_xlim(0, 210)
    ax.legend(loc="upper right")
    save(fig, "plot_pore_variance.png")


def plot_cv(live):
    fig, ax = new_axes(X_LABEL_BU, r"Coefficient of variation $\sqrt{B/N_p}\,/\,\bar{n}$ (/)")

    # The implicit-Euler moment solver produces a period-2 alternation in CV near
    # nucleation onset (consecutive timesteps follow two distinct envelopes). A short
    # median is insufficient because it locks onto one envelope; a Savitzky-Golay
    # filter averages across the oscillation and preserves the U-shape of the curve.
    cv_raw = np.asarray(live["CV"], dtype=float)
    # The first timesteps after onset hold a handful of pores per m3, all at the seed
    # value n=2: the moments are not yet statistically meaningful and produce isolated
    # outliers that the filter window would smear backward as a spurious tick.
    valid = np.asarray(live["poreDensity"], dtype=float) > 1.0e10
    cv_clean = np.where(valid, cv_raw, 0.0)
    n = len(cv_clean)
    win = min(51, n if n % 2 == 1 else n - 1)
    cv_smoothed = savgol_filter(cv_clean, window_length=win, polyorder=3, mode="nearest") \
        if win >= 5 else cv_clean
    cv_smoothed = np.clip(np.where(valid, cv_smoothed, 0.0), 0.0, None)

    ax.plot(live["burnup"], cv_smoothed, "-", color=COLOR_CURRENT, linewidth=LINEWIDTH_MODEL)
    ax.set_xlim(0, 210)
    ax.set_ylim(0, None)
    save(fig, "plot_CV.png")


def plot_xe_inventory(live):
    xe = live["xe_inventory"]
    scale = 1.0e26

    reservoirs = [
        (xe["Xe in grain (at/m3)"] / scale, "Xe in NR grain", "#1a3c6e"),
        (xe["Xe at grain boundary (at/m3)"] / scale, "Xe at NR grain boundary", "#4a7bb5"),
        (xe["Xe in grain HBS (at/m3)"] / scale, "Xe in HBS grain", "#f4a65a"),
        (xe["Xe at grain boundary HBS (at/m3)"] / scale, "Xe at HBS grain boundary", "#e6703f"),
        (xe["Xe in HBS pores (at/m3)"] / scale, "Xe in HBS pores", "#b23838"),
        (xe["Xe released (at/m3)"] / scale, "Xe released", "#4a4a4a"),
    ]
    xe_total = (xe["Xe produced (at/m3)"] + xe["Xe produced in HBS (at/m3)"]) / scale

    labels = [label for _, label, _ in reservoirs]
    colors = [color for _, _, color in reservoirs]
    series = np.vstack([y for y, _, _ in reservoirs])
    fractions = 100.0 * series / np.where(xe_total > 0.0, xe_total, 1.0)

    fig, (ax_top, ax_bot) = plt.subplots(
        2, 1, figsize=(8.0, 8.0), sharex=True,
        gridspec_kw={"height_ratios": [1.1, 1.0], "hspace": 0.08},
    )

    ax_top.stackplot(live["burnup"], series, labels=labels, colors=colors,
                     alpha=0.92, linewidth=0)
    ax_top.plot(live["burnup"], xe_total, "--", color="black",
                linewidth=LINEWIDTH_REF, label="Xe produced (total)")
    ax_top.set_ylabel(r"Xe inventory ($10^{26}$ at m$^{-3}$)")
    ax_top.set_xlim(0, 210)
    ax_top.set_ylim(bottom=0.0)
    ax_top.legend(loc="upper left", ncol=1, fontsize=8.5)

    ax_bot.stackplot(live["burnup"], fractions, labels=labels, colors=colors,
                     alpha=0.92, linewidth=0)
    ax_bot.set_xlabel(X_LABEL_BU)
    ax_bot.set_ylabel("Share of produced Xe (%)")
    ax_bot.set_xlim(0, 210)
    ax_bot.set_ylim(0.0, 100.0)

    ax_alpha = ax_bot.twinx()
    ax_alpha.plot(live["burnup"], 100.0 * live["alpha"], "-", color=COLOR_ALPHA,
                  linewidth=LINEWIDTH_MODEL, label=r"Restructured fraction $\alpha_r$")
    ax_alpha.set_ylabel(r"$\alpha_r$ (%)", color=COLOR_ALPHA)
    ax_alpha.tick_params(axis="y", colors=COLOR_ALPHA)
    ax_alpha.set_ylim(0.0, 100.0)
    ax_alpha.grid(False)

    lines_bot, labels_bot = ax_bot.get_legend_handles_labels()
    lines_alpha, labels_alpha = ax_alpha.get_legend_handles_labels()
    ax_bot.legend(lines_alpha + lines_bot, labels_alpha + labels_bot,
                  loc="upper left", ncol=1, fontsize=8.5)

    save(fig, "plot_xe_inventory.png")


def main():
    live = load_case(LIVE_CASE)
    if live is None:
        print("Run the HBS cases first: python3 -m regression.runner --hbs")
        return

    reference = load_case(REFERENCE_CASE)
    if reference is None:
        print(f"No reference case in {REFERENCE_CASE}, plotting the live run alone")

    plot_pore_density(live, reference)
    plot_porosity(live, reference)
    plot_pore_radius(live, reference)
    plot_xe_depletion(live, reference)
    plot_fuel_swelling(live, reference)

    if live["CV"] is not None:
        plot_pore_variance(live)
        plot_cv(live)
    else:
        print("No cluster-dynamics moments in the live case: variance and CV skipped")

    if live["xe_inventory"] is not None:
        plot_xe_inventory(live)
    else:
        print("No xenon inventory columns in the live case: inventory plot skipped")


if __name__ == "__main__":
    main()
