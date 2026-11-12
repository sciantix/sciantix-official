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
    case["time_s"] = col("Time (h)") * 3600.0
    case["T"] = col("Temperature (K)")
    case["fission_rate"] = col("Fission rate (fiss / m3 s)")
    case["sigma_h"] = col("Hydrostatic stress (MPa)")
    case["n_xe"] = col("Xe atoms per HBS pore (at/pore)")
    case["n_vac"] = col("Vacancies per HBS pore (vac/pore)")
    case["poreVolume"] = col("HBS pore volume (m3)")

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

    fig, (ax, ax2) = plt.subplots(1, 2, figsize=(14.0, 5.5))
    ax.set_xlabel(X_LABEL_BU)
    ax.set_ylabel(r"Pore number density (pores m$^{-3}$)")
    ax.set_title("(a)", loc="left")
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
    ax.set_ylim(0, 1.0e18)  # headroom so the legend clears the peak and the Cappia points
    ax.legend(loc="upper right", ncol=2, fontsize="small")
    draw_np_budget(ax2, live)
    ax2.set_title("(b)", loc="left")
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
    ax.set_ylim(0, 0.30)
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


CV_WINDOW_BU = 0.5        # (MWd/kgHM) running-mean window for the plotted CV
CV_MIN_PORE_DENSITY = 1.0e10  # (pores/m3) first nucleation steps excluded


def cv_running_mean(live):
    """
    CV averaged over a running burnup window of CV_WINDOW_BU. Between ~20 and
    ~33 MWd/kgHM (N_p < 1e14 m-3, pores below 10 nm, capillary pressure of the
    order of GPa) the vacancy content of the nanometric clusters switches between
    zero and a positive value from one time step to the next, and the CV follows.
    The window (about 17 steps) removes this step-to-step oscillation and leaves
    the curve unchanged where it is smooth (features on 5-10 MWd/kgHM).
    The first steps after nucleation (N_p < 1e10 m-3) are excluded: the variance
    source uses the mean size of the previous step, which is zero before the first
    nucleus, and gives CV = 1 for one step.
    """
    bu = np.asarray(live["burnup"], dtype=float)
    cv = np.asarray(live["CV"], dtype=float)
    valid = np.asarray(live["poreDensity"], dtype=float) > CV_MIN_PORE_DENSITY
    out = np.full_like(cv, np.nan)
    idx = np.where(valid)[0]
    if len(idx) == 0:
        return out
    step = np.median(np.diff(bu[idx]))
    w = max(1, int(round(CV_WINDOW_BU / step)) | 1)  # odd number of steps
    seg = cv[idx]
    k = np.ones(w)
    out[idx] = np.convolve(seg, k, "same") / np.convolve(np.ones_like(seg), k, "same")
    return out


DISTRIBUTION_BURNUPS = (40, 65, 110, 150, 200)
# distinct hues, none green (the CV curve is green)
DISTRIBUTION_COLORS = ("#1f4e9c", "#e67e22", "#c0392b", "#8e44ad", "#222222")


def plot_cv(live):
    """
    (a) Coefficient of variation vs burnup, with the burnups of panel (b) marked.
    (b) Pore radius distributions reconstructed from the first three moments,
    assuming a log-normal shape (Barani et al. 2022, Sec. 2.1). With mean n_bar
    and CV = sqrt(B/N_p)/n_bar, sigma_ln^2 = ln(1 + CV^2) in n. Radius follows
    R = R_p (n/n_bar)^(1/3), the same mapping used for the sigma_R band, so the
    distribution is log-normal in R with sigma_ln/3.
    """
    bu = live["burnup"]
    cv = cv_running_mean(live)
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14.0, 5.5))

    ax1.plot(bu, cv, "-", color=COLOR_CURRENT, linewidth=LINEWIDTH_MODEL)
    r_nm = np.linspace(1.0, 1000.0, 2000)
    for b, c in zip(DISTRIBUTION_BURNUPS, DISTRIBUTION_COLORS):
        i = int(np.argmin(np.abs(bu - b)))
        CV, Np, Rm = cv[i], live["poreDensity"][i], live["poreRadius"][i] * 1e9
        if not CV > 0 or Np <= 0:
            continue
        ax1.plot(bu[i], CV, "o", color=c, markersize=8, zorder=3)
        s = np.sqrt(np.log(1.0 + CV**2)) / 3.0
        mu = np.log(Rm) - 0.5 * np.log(1.0 + CV**2) / 3.0
        g = Np * np.exp(-(np.log(r_nm) - mu)**2 / (2 * s**2)) / (r_nm * s * np.sqrt(2 * np.pi))
        ax2.plot(r_nm, g, "-", color=c, linewidth=LINEWIDTH_MODEL,
                 label=rf"{b} MWd kgHM$^{{-1}}$: CV = {CV:.2f}, $R_p$ = {Rm:.0f} nm")

    ax1.set_xlabel(X_LABEL_BU)
    ax1.set_ylabel(r"Coefficient of variation $\sqrt{B/N_p}\,/\,\bar{n}$ (/)")
    ax1.set_xlim(0, 210)
    ax1.set_ylim(0, None)
    ax1.set_title("(a)", loc="left")
    ax2.set_xlabel("Pore radius (nm)")
    ax2.set_ylabel(r"$dN_p/dR$ (pores m$^{-3}$ nm$^{-1}$)")
    ax2.set_xlim(0, 1000)
    ax2.set_ylim(0, None)
    ax2.legend(loc="upper right")
    ax2.set_title("(b)", loc="left")
    save(fig, "plot_CV.png")


# Constants of the UO2HBS matrix used by the porosity model (SetMatrix.C).
SURFACE_TENSION = 1.1      # (N/m)
SCHOTTKY_VOLUME = 4.09e-29  # (m3)
BOLTZMANN = 1.380649e-23    # (J/K)
NUCLEATION_PREFACTOR = 8.8e17  # pores m-3 per unit alpha_r (Eq. nu_P)


def pore_pressure(live):
    """
    Carnahan-Starling pressure in the mean pore, as computed in
    HighBurnupStructurePorosity.C: hard-sphere Xe diameter of Brearley-MacInnes,
    packing fraction n_Xe*v_Xe/V_p (capped at 0.65 in the code).
    Returns (p_gas, p_eq, v_xe) in Pa, Pa, m3.
    """
    T, n, V, R = live["T"], live["n_xe"], live["poreVolume"], live["poreRadius"]
    d = 4.45e-10 * (0.8542 - 0.03996 * np.log(T / 231.2))
    v_xe = np.pi / 6.0 * d**3
    ok = V > 0
    y = np.zeros_like(V)
    np.divide(n * v_xe, V, out=y, where=ok)
    y = np.minimum(y, 0.65)
    Z = (1 + y + y**2 - y**3) / (1 - y)**3
    p_gas = np.zeros_like(V)
    np.divide(Z * n * BOLTZMANN * T, V, out=p_gas, where=ok)
    p_eq = np.zeros_like(V)
    np.divide(2.0 * SURFACE_TENSION, R, out=p_eq, where=R > 0)
    p_eq = p_eq - live["sigma_h"] * 1e6
    return p_gas, p_eq, v_xe


def plot_pore_pressure(live):
    """
    (a) Mean-pore gas pressure against the equilibrium pressure and the
    estimates of Noirot et al. 2008 (Table 2, 650 K) and Jernkvist 2019;
    (b) volume per gas atom in the pore, the EoS-independent quantity measured
    by Noirot; (c) split of the mean pore volume between gas and vacancies.
    """
    p_gas, p_eq, v_xe = pore_pressure(live)
    bu = live["burnup"]
    on = live["poreDensity"] > 1.0e15  # skip the first few pores after onset
    noirot = load_xy("noirot2008_pressure.txt")
    rim, agg = noirot[noirot[:, 3] == 0], noirot[noirot[:, 3] == 1]

    fig, (ax1, ax2, ax3) = plt.subplots(3, 1, figsize=(8.0, 14.0), sharex=True)
    ax1.axhspan(50, 150, color="#bbbbbb", alpha=0.35, label="Jernkvist 2019, 50-150 MPa")
    ax1.plot(bu[on], p_gas[on] / 1e6, "-", color=COLOR_CURRENT, linewidth=LINEWIDTH_MODEL,
             label=r"$p_P$, Carnahan-Starling, 723 K")
    ax1.plot(bu[on], p_eq[on] / 1e6, "--", color="k", linewidth=LINEWIDTH_REF,
             label=r"$p_P^{eq} = 2\gamma_p/R_p - \sigma_h$")
    ax2.plot(bu[on], live["poreVolume"][on] / live["n_xe"][on] * 1e30, "-", color=COLOR_CURRENT,
             linewidth=LINEWIDTH_MODEL, label=r"$V_p/n_{Xe}$")
    for ax, col in ((ax1, 2), (ax2, 1)):
        ax.plot(rim[:, 0], rim[:, col], "o", color=COLOR_NOIROT, markersize=MARKER_SIZE + 2,
                label="Noirot et al. 2008, UO$_2$ rim, 650 K")
        ax.plot(agg[:, 0], agg[:, col], "s", color=COLOR_NOIROT, markerfacecolor="none",
                markersize=MARKER_SIZE + 2, label="Noirot et al. 2008, peripheral agglomerates, 650 K")
    ax1.set_ylabel("Pressure (MPa)")
    ax1.set_ylim(0, None)
    ax2.set_ylabel(r"Volume per gas atom ($\mathrm{\AA}^3$ at$^{-1}$)")
    ax2.set_ylim(0, 600)

    ax3.plot(bu[on], live["n_xe"][on] * v_xe[on] * 1e18, "-", color=COLOR_SPINO, linewidth=LINEWIDTH_MODEL,
             label=r"gas, $n_{Xe}\,\Omega_{Xe}$")
    ax3.plot(bu[on], live["n_vac"][on] * SCHOTTKY_VOLUME * 1e18, "-", color=COLOR_REFERENCE,
             linewidth=LINEWIDTH_MODEL, label=r"vacancies, $n_{vp}\,\Omega_{vac}$")
    ax3.plot(bu[on], live["poreVolume"][on] * 1e18, "-", color=COLOR_CURRENT, linewidth=LINEWIDTH_MODEL,
             label=r"total, $V_p$")
    ax3.set_ylabel(r"Mean pore volume ($\mu$m$^3$)")
    ax3.set_ylim(0, None)

    ax3.set_xlabel(X_LABEL_BU)
    for ax, tag in zip((ax1, ax2, ax3), "abc"):
        ax.set_xlim(0, 210)  # same range as the other figures; Noirot MOX points at 220 and 270 fall outside
        ax.legend(loc="upper right" if ax is ax2 else "upper left")
        ax.set_title(f"({tag})", loc="left")
    save(fig, "plot_pore_pressure.png")


def np_budget(live):
    """
    Cumulative contributions to N_p, reconstructed from the output:
    nucleation nu_P dt = prefactor * d(alpha_r) (Eq. nu_P), re-solution
    alpha_n(R_p) N_p dt, coalescence as the remainder of the N_p balance.
    """
    t, Np, R, F = live["time_s"], live["poreDensity"], live["poreRadius"], live["fission_rate"]
    alpha = live["alpha"]
    dt = np.diff(t, prepend=t[0])
    d_alpha = np.maximum(np.diff(alpha, prepend=alpha[0]), 0.0)
    nucleated = np.cumsum(np.where(live["burnup"] > 15.0, NUCLEATION_PREFACTOR * d_alpha, 0.0))
    a_n = 2e-23 * F * (3e-9 / (3e-9 + R)) * (1e-9 / (1e-9 + R))
    resolved = np.cumsum(a_n * Np * dt)
    coalesced = nucleated - resolved - Np
    return nucleated, resolved, coalesced


def draw_np_budget(ax, live):
    nuc, res, coal = np_budget(live)
    bu = live["burnup"]
    ax.plot(bu, nuc, "-", color=COLOR_REFERENCE, linewidth=LINEWIDTH_MODEL, label="nucleated")
    ax.plot(bu, res, "-", color=COLOR_SPINO, linewidth=LINEWIDTH_MODEL, label="destroyed by re-solution")
    ax.plot(bu, coal, "-", color=COLOR_BARANI, linewidth=LINEWIDTH_MODEL, label="lost by coalescence")
    ax.plot(bu, live["poreDensity"], "-", color=COLOR_CURRENT, linewidth=LINEWIDTH_MODEL + 0.5,
            label=r"$N_p$")
    ax.set_xlabel(X_LABEL_BU)
    ax.set_ylabel(r"Cumulative pore number density (pores m$^{-3}$)")
    ax.set_xlim(0, 210)
    ax.set_ylim(0, None)
    ax.legend(loc="upper left")



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
        plot_pore_pressure(live)
    else:
        print("No cluster-dynamics moments in the live case: variance and CV skipped")

    if live["xe_inventory"] is not None:
        plot_xe_inventory(live)
    else:
        print("No xenon inventory columns in the live case: inventory plot skipped")


if __name__ == "__main__":
    main()
