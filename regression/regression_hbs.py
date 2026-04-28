"""
HBS regression driver and plotting.

Runs SCIANTIX on every ``test_UO2HBS*`` folder found in the working path
(excluding any ``*dislocation*`` variant and ``test_UO2HBS_0``, which are
out of scope as live tests for the current manuscript), checks the output
against gold data, and -- when mode_plot is enabled -- produces a fixed
set of validation plots.

Reference curves for the manuscript comparison come from a frozen snapshot
of the semi-empirical / Barani-original run, stored as ``output_0.txt``
inside ``test_UO2HBS``. The two configurations compared in the plots are:

    - frozen ``output_0.txt`` (copied from ``test_UO2HBS_0``):
        semi-empirical porosity + KJMA without incubation
        (iHighBurnupStructureFormation = 1, iHighBurnupStructurePorosity = 1)
    - live ``test_UO2HBS``:
        mechanistic cluster dynamics + KJMA with bu_inc = 15
        (iHighBurnupStructureFormation = 2, iHighBurnupStructurePorosity = 2)

All plots are written to ``SAVE_DIR`` (the manuscript Images folder). If
the reference snapshot is missing the plots degrade gracefully to the
single primary curve.

Plots produced:
    1. plot_pore_density.png   - HBS pore number density vs burnup
    2. plot_porosity.png       - HBS porosity vs burnup
    3. plot_pore_radius.png    - HBS mean pore radius vs burnup
    4. plot_xe_depletion.png   - Xe retention in grains vs Walker 1999
    5. plot_fuel_swelling.png  - Matrix swelling breakdown vs Spino 2006
    6. plot_pore_variance.png  - Second central moment B (diagnostic)
    7. plot_CV.png             - Coefficient of variation of the Fokker-Planck
    8. plot_xe_inventory.png   - Xenon mass balance: where does the gas go?

Plots 1-5 overlay the two tests; plots 6-8 are diagnostic and show only
the production test (``test_UO2HBS``).

@author G. Zullo
"""

import os

import matplotlib.pyplot as plt
import numpy as np

from regression_functions import *

SAVE_DIR = '/home/giovanni/research-manuscripts/Zullo_et_al__HBS/Images'

# Folder recognized by this driver as the production test for the current
# manuscript. The semi-empirical / Barani-original reference curves used in
# the comparison plots are loaded from a frozen snapshot sitting inside
# PRIMARY_FOLDER (REFERENCE_OUTPUT), not from a live regression folder.
PRIMARY_FOLDER   = "test_UO2HBS"
REFERENCE_OUTPUT = "output_0.txt"

# Folder names whose (case-insensitive) substring matches this marker are
# skipped. Keeps the dislocation-density branch out of the manuscript plots
# without having to delete the regression folder.
DISLOCATION_MARKER = "dislocation"

# Folders excluded from the regression loop entirely (kept on disk but not
# treated as live tests). test_UO2HBS_0 is the source of output_0.txt above
# and is retained as a runnable configuration, but the plotting phase reads
# the frozen snapshot instead.
EXCLUDED_FOLDERS = ("test_UO2HBS_0",)

# =============================================================================
# Plot style
# =============================================================================

FIG_SIZE = (8.0, 5.5)
DPI = 150

# Consistent color palette across plots.
COLOR_CURRENT = "#2ca02c"       # this work    - test_UO2HBS  (mechanistic + bu_inc=15)
COLOR_REFERENCE = "#1f77b4"     # reference    - test_UO2HBS_0 (semi-empirical + Barani original)
COLOR_BARANI = "#8b4513"        # Barani et al. 2022 JNM 563 - literature reference
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


# =============================================================================
# Helpers
# =============================================================================

def _new_axes(xlabel, ylabel, title=None):
    """Create a single-axes figure with standardised labels."""
    fig, ax = plt.subplots()
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    if title is not None:
        ax.set_title(title)
    return fig, ax


def _save(fig, filename):
    """Save into SAVE_DIR (absolute path) and close the figure."""
    os.makedirs(SAVE_DIR, exist_ok=True)
    out = os.path.join(SAVE_DIR, filename)
    fig.savefig(out, dpi=DPI, bbox_inches="tight")
    plt.close(fig)


def _lassmann_fit(bu, threshold=60.0):
    """
    K. Lassmann et al., JNM 226 (1995) 1-8. Xe retention (wt%) vs local
    burnup (MWd/kgHM). Linear build-up up to ``threshold`` then exponential
    decay.
    """
    result = np.empty_like(bu, dtype=float)
    for i, value in enumerate(bu):
        if value < threshold:
            result[i] = 1.46e-2 * value
        else:
            result[i] = 1.46e-2 * (
                1.0 / 0.0584
                + (60.0 - 1.0 / 0.0584) * np.exp(-0.0584 * (value - 60.0))
            )
    return result


def _extract_data(data):
    """
    Extract all scalar time-series needed by the plotting code from a raw
    SCIANTIX ``output.txt`` table. Returns a dict keyed by semantic name.
    Guards against missing optional columns (e.g. in the semi-empirical
    reference test, which does not populate cluster-dynamics moments).
    """
    def pos(name):
        try:
            return findSciantixVariablePosition(data, name)
        except Exception:
            return None

    def col(name):
        p = pos(name)
        if p is None:
            return None
        return data[1:, p].astype(float)

    out = {}
    out["burnup"] = col("Burnup (MWd/kgUO2)") / 0.8814
    out["effectiveBurnup"] = col("Effective burnup (MWd/kgUO2)") / 0.8814
    out["poreDensity"] = col("HBS pore density (pores/m3)")
    out["porosity"] = col("HBS porosity (/)")
    out["poreRadius"] = col("HBS pore radius (m)")

    # Cluster-dynamics moments (may be absent in the semi-empirical test).
    M2 = col("Xe atoms per HBS pore - variance (at^2/pore)")
    nXe = col("Xe atoms per HBS pore (at/pore)")
    B = col("Xe in HBS pores - variance (at^2/m3)")

    if M2 is not None and nXe is not None and B is not None:
        safe = (out["poreDensity"] > 0) & (nXe > 0) & (out["poreRadius"] > 0)
        sigma_n = np.where(safe, np.sqrt(np.maximum(M2, 0.0)), 0.0)
        CV = np.zeros_like(sigma_n)
        np.divide(sigma_n, nXe, out=CV, where=safe)
        out["B_raw"] = B
        out["CV"] = CV
        out["sigma_R"] = out["poreRadius"] * CV / 3.0
        out["sigma_xi"] = out["porosity"] * CV
        out["has_moments"] = True
    else:
        out["B_raw"] = None
        out["CV"] = None
        out["sigma_R"] = None
        out["sigma_xi"] = None
        out["has_moments"] = False

    # Xenon inventory columns (may be absent in the semi-empirical test).
    xe_names = [
        "Xe produced (at/m3)", "Xe produced in HBS (at/m3)",
        "Xe in grain (at/m3)", "Xe in grain HBS (at/m3)",
        "Xe at grain boundary (at/m3)", "Xe at grain boundary HBS (at/m3)",
        "Xe in HBS pores (at/m3)", "Xe released (at/m3)",
    ]
    xe_cols = {name: col(name) for name in xe_names}
    out["xe_inventory"] = xe_cols if all(v is not None for v in xe_cols.values()) else None

    return out


# =============================================================================
# Main regression driver
# =============================================================================

def regression_hbs(wpath, mode_HBS, mode_gold, mode_plot,
                   folderList, number_of_tests, number_of_tests_failed):

    if mode_HBS == 0:
        return folderList, number_of_tests, number_of_tests_failed

    # ------------------------------------------------------------------
    # PHASE 1: for each HBS folder run SCIANTIX / gold check and extract
    # the time series into a dict. No plots are produced in this phase.
    # ------------------------------------------------------------------
    results = {}        # folder_name -> dict of extracted arrays
    sd_by_folder = {}   # folder_name -> output of sciantix_dictionary("output.txt")

    for entry in sorted(os.listdir(wpath)):
        if "HBS" not in entry or not os.path.isdir(entry):
            continue
        if DISLOCATION_MARKER in entry.lower():
            # Dislocation variant is intentionally skipped for the current paper.
            continue
        if entry in EXCLUDED_FOLDERS:
            # Frozen reference: its snapshot lives as output_0.txt inside
            # PRIMARY_FOLDER and is loaded directly during the plotting phase.
            continue

        folderList.append(entry)
        os.chdir(entry)
        print(f"Now in folder {entry}...")
        number_of_tests += 1

        if mode_gold == 0:
            do_sciantix_only()
            data, data_gold = check_output(entry)
            number_of_tests_failed = check_result(number_of_tests_failed)
        elif mode_gold == 1:
            do_sciantix_only()
            data, data_gold = check_output(entry)
            print("...golding results.")
            do_gold()
        elif mode_gold == 2:
            data, data_gold = check_output(entry)
            number_of_tests_failed = check_result(number_of_tests_failed)
        elif mode_gold == 3:
            data, data_gold = check_output(entry)
            print("...golding existing results.")
            do_gold()

        if mode_plot == 1:
            results[entry] = _extract_data(data)
            sd_by_folder[entry] = sciantix_dictionary("output.txt")

        os.chdir("..")

    if mode_plot != 1:
        return folderList, number_of_tests, number_of_tests_failed

    # ------------------------------------------------------------------
    # PHASE 2: build comparison plots from the collected data.
    # ------------------------------------------------------------------
    if PRIMARY_FOLDER not in results:
        print(f"[regression_hbs] {PRIMARY_FOLDER} not available -- skipping plots.")
        return folderList, number_of_tests, number_of_tests_failed

    primary = results[PRIMARY_FOLDER]

    # Experimental datasets and literature reference live next to the
    # primary test folder. Load them from there.
    os.chdir(PRIMARY_FOLDER)

    # Reference curves: frozen snapshot of the semi-empirical / Barani-original
    # run, copied in once as output_0.txt. Extract it the same way as a live
    # test so the plotting code below doesn't need to special-case anything.
    reference = None
    sd_reference = None
    if os.path.exists(REFERENCE_OUTPUT):
        try:
            reference = _extract_data(import_data(REFERENCE_OUTPUT))
            sd_reference = sciantix_dictionary(REFERENCE_OUTPUT)
        except Exception as exc:
            print(f"[regression_hbs] {REFERENCE_OUTPUT} unreadable ({exc}); "
                  "reference curves skipped.")
            reference = None
            sd_reference = None
    else:
        print(f"[regression_hbs] {REFERENCE_OUTPUT} not found in {PRIMARY_FOLDER}; "
              "reference curves skipped.")

    exp_np_cappia    = import_data("exp_pore_density.txt").astype(float)
    exp_np_spino_67  = import_data("exp_pore_density_2.txt").astype(float)
    exp_np_spino_80  = import_data("exp_pore_density_3.txt").astype(float)
    exp_np_spino_98  = import_data("exp_pore_density_4.txt").astype(float)

    exp_p_cappia     = import_data("exp_porosity.txt").astype(float)
    exp_p_spino      = import_data("exp_porosity_2.txt").astype(float)
    exp_p_noirot     = import_data("exp_porosity_3.txt").astype(float)
    exp_p_lassmann   = import_data("exp_porosity_4.txt").astype(float)
    exp_p_une_low    = import_data("exp_porosity_5.txt").astype(float)
    exp_p_une_strong = import_data("exp_porosity_6.txt").astype(float)

    exp_r_cappia     = import_data("exp_pore_radius.txt").astype(float)
    exp_r_spino      = import_data("exp_pore_radius_2.txt").astype(float)

    barani_density  = np.genfromtxt("Barani_pore_density.txt")
    barani_porosity = np.genfromtxt("Barani_porosity.txt")
    barani_radius   = np.genfromtxt("Barani_pore_radius.txt")

    walker = np.genfromtxt("walker_data_1999.txt")
    walker_bu = walker[1:, 0]
    walker_xe = walker[1:, 1]

    spino_swe = np.genfromtxt("spino_swelling_data.txt")
    spino_bu = spino_swe[1:, 0]
    spino_sw = spino_swe[1:, 1]

    os.chdir("..")

    x_label_bu = r"Burnup (MWd kgHM$^{-1}$)"

    # Labels for the two-curve overlay. The reference test is labelled by its
    # model content rather than by its folder name so the reader doesn't need
    # to know the regression test layout.
    label_primary   = "SCIANTIX, this work"
    label_reference = "SCIANTIX, semi-empirical"

    # ------------------------------------------------------------------
    # Plot 1: HBS pore number density
    # ------------------------------------------------------------------
    fig, ax = _new_axes(
        x_label_bu,
        r"Pore number density (pores m$^{-3}$)",
    )
    ax.plot(exp_np_cappia[:, 0], exp_np_cappia[:, 1],
            "o", color=COLOR_CAPPIA, markersize=MARKER_SIZE,
            label="Cappia et al. 2016")
    ax.plot(exp_np_spino_67[:, 0], exp_np_spino_67[:, 1],
            "s", color=COLOR_SPINO_LIGHT, markersize=MARKER_SIZE,
            label=r"Spino et al. 2006, Bu$_\mathrm{AV}=67$ MWd/kgHM")
    ax.plot(exp_np_spino_80[:, 0], exp_np_spino_80[:, 1],
            "s", color=COLOR_SPINO, markersize=MARKER_SIZE,
            label=r"Spino et al. 2006, Bu$_\mathrm{AV}=80$ MWd/kgHM")
    ax.plot(exp_np_spino_98[:, 0], exp_np_spino_98[:, 1],
            "s", color=COLOR_SPINO_DARK, markersize=MARKER_SIZE,
            label=r"Spino et al. 2006, Bu$_\mathrm{AV}=98$ MWd/kgHM")
    ax.plot(barani_density[:, 0], barani_density[:, 1],
            "-", color=COLOR_BARANI, linewidth=LINEWIDTH_MODEL,
            label="Barani et al. 2022")
    if reference is not None:
        ax.plot(reference["burnup"], reference["poreDensity"],
                "-", color=COLOR_REFERENCE, linewidth=LINEWIDTH_MODEL,
                label=label_reference)
    ax.plot(primary["burnup"], primary["poreDensity"],
            "-", color=COLOR_CURRENT, linewidth=LINEWIDTH_MODEL,
            label=label_primary)
    ax.set_xlim(0, 210)
    ax.legend(loc="upper right")
    _save(fig, "plot_pore_density.png")

    # ------------------------------------------------------------------
    # Plot 2: HBS porosity
    # ------------------------------------------------------------------
    fig, ax = _new_axes(
        x_label_bu,
        "HBS porosity (/)",
    )
    ax.plot(exp_p_cappia[:, 0], exp_p_cappia[:, 1],
            "o", color=COLOR_CAPPIA, markersize=MARKER_SIZE,
            label="Cappia et al. 2016")
    ax.plot(exp_p_spino[:, 0], exp_p_spino[:, 1],
            "s", color=COLOR_REFERENCE, markersize=MARKER_SIZE,
            label="Spino et al. 2006")
    ax.plot(exp_p_noirot[:, 0], exp_p_noirot[:, 1],
            "^", color=COLOR_NOIROT, markersize=MARKER_SIZE,
            label="Noirot et al. 2008")
    ax.plot(exp_p_lassmann[:, 0], exp_p_lassmann[:, 1],
            "D", color=COLOR_LASSMANN, markersize=MARKER_SIZE,
            label="Lassmann et al. 1995")
    ax.plot(exp_p_une_low[:, 0], exp_p_une_low[:, 1],
            "v", color=COLOR_UNE_LOW, markersize=MARKER_SIZE,
            label="Une et al. 2001, low PCMI")
    ax.plot(exp_p_une_strong[:, 0], exp_p_une_strong[:, 1],
            "v", color=COLOR_UNE_STRONG, markersize=MARKER_SIZE,
            label="Une et al. 2001, strong PCMI")
    ax.plot(barani_porosity[:, 0], barani_porosity[:, 1],
            "-", color=COLOR_BARANI, linewidth=LINEWIDTH_MODEL,
            label="Barani et al. 2022")
    if reference is not None:
        ax.plot(reference["burnup"], reference["porosity"],
                "-", color=COLOR_REFERENCE, linewidth=LINEWIDTH_MODEL,
                label=label_reference)
    ax.plot(primary["burnup"], primary["porosity"],
            "-", color=COLOR_CURRENT, linewidth=LINEWIDTH_MODEL,
            label=label_primary)
    if primary["sigma_xi"] is not None:
        ax.fill_between(primary["burnup"],
                        np.maximum(primary["porosity"] - primary["sigma_xi"], 0.0),
                        primary["porosity"] + primary["sigma_xi"],
                        alpha=0.20, color=COLOR_CURRENT, linewidth=0,
                        label=r"$\pm\sigma_\xi$ (Fokker-Planck)")
    ax.set_xlim(0, 210)
    ax.set_ylim(0, 0.25)
    ax.legend(loc="upper left", ncol=1)
    _save(fig, "plot_porosity.png")

    # ------------------------------------------------------------------
    # Plot 3: HBS pore mean radius
    # ------------------------------------------------------------------
    fig, ax = _new_axes(
        x_label_bu,
        "Pore radius (m)",
    )
    ax.plot(exp_r_cappia[:, 0], exp_r_cappia[:, 1],
            "o", color=COLOR_CAPPIA, markersize=MARKER_SIZE,
            label="Cappia et al. 2016")
    ax.plot(exp_r_spino[:, 0], exp_r_spino[:, 1],
            "s", color=COLOR_SPINO, markersize=MARKER_SIZE,
            label="Spino et al. 2006")
    ax.plot(barani_radius[:, 0], barani_radius[:, 1],
            "-", color=COLOR_BARANI, linewidth=LINEWIDTH_MODEL,
            label="Barani et al. 2022")
    if reference is not None:
        ax.plot(reference["burnup"], reference["poreRadius"],
                "-", color=COLOR_REFERENCE, linewidth=LINEWIDTH_MODEL,
                label=label_reference)
    ax.plot(primary["burnup"], primary["poreRadius"],
            "-", color=COLOR_CURRENT, linewidth=LINEWIDTH_MODEL,
            label=label_primary)
    if primary["sigma_R"] is not None:
        ax.fill_between(primary["burnup"],
                        np.maximum(primary["poreRadius"] - primary["sigma_R"], 0.0),
                        primary["poreRadius"] + primary["sigma_R"],
                        alpha=0.20, color=COLOR_CURRENT, linewidth=0,
                        label=r"$\pm\sigma_R$ (Fokker-Planck)")
    ax.set_xlim(0, 210)
    ax.legend(loc="upper left")
    _save(fig, "plot_pore_radius.png")

    # ------------------------------------------------------------------
    # Plot 4: Xenon depletion in grains (vs Walker 1999 + Lassmann fit)
    # ------------------------------------------------------------------
    bu_range = np.linspace(0.0, 200.0, 1000)
    Xe_eq = 4.88897e26
    sd_primary = sd_by_folder[PRIMARY_FOLDER]

    fig, ax1 = plt.subplots()
    ax1.plot(bu_range, _lassmann_fit(bu_range),
             "--", color=COLOR_LASSMANN_FIT, linewidth=LINEWIDTH_REF,
             label=r"Lassmann fit (Bu$_0=60$ MWd/kgHM)")
    ax1.scatter(walker_bu, walker_xe,
                color=COLOR_WALKER, edgecolors="black",
                marker="o", s=22, label="Walker 1999")

    # Reference test: total Xe in grains as a single line.
    if sd_reference is not None:
        total_ref = (sd_reference["xe_ig"] + sd_reference["xe_igHBS"]) / Xe_eq
        ax1.plot(sd_reference["bu"] / 0.8814, total_ref,
                 "-", color=COLOR_REFERENCE, linewidth=LINEWIDTH_MODEL,
                 label=label_reference + " (total)")

    # Primary test: split into NR, HBS and total to preserve the diagnostic
    # detail that was already in the original single-test plot.
    ax1.plot(sd_primary["bu"] / 0.8814, sd_primary["xe_ig"] / Xe_eq,
             "-.", color=COLOR_CURRENT, linewidth=LINEWIDTH_REF,
             label=label_primary + ", NR")
    ax1.plot(sd_primary["bu"] / 0.8814, sd_primary["xe_igHBS"] / Xe_eq,
             ":", color=COLOR_SPINO, linewidth=LINEWIDTH_REF,
             label=label_primary + ", HBS")
    total_primary = (sd_primary["xe_igHBS"] + sd_primary["xe_ig"]) / Xe_eq
    ax1.plot(sd_primary["bu"] / 0.8814, total_primary,
             "-", color="#2c2c2c", linewidth=LINEWIDTH_MODEL,
             label=label_primary + " (total)")
    ax1.set_xlabel("Burnup (MWd/kgHM)")
    ax1.set_ylabel("Xe in grains (wt%)")
    ax1.set_xlim(0, 200)
    ax1.set_ylim(0, 1.75)

    ax2 = ax1.twinx()
    ax2.plot(sd_primary["bu"] / 0.8814, sd_primary["alpha"],
             "-", color=COLOR_ALPHA, linewidth=LINEWIDTH_MODEL,
             label=r"Restructured fraction ($\alpha_r$, this work)")
    ax2.set_ylabel("Restructured volume fraction (/)")
    ax2.set_ylim(0.0, 1.05)
    ax2.grid(False)

    lines1, labels1 = ax1.get_legend_handles_labels()
    lines2, labels2 = ax2.get_legend_handles_labels()
    ax1.legend(lines1 + lines2, labels1 + labels2,
               loc="upper right", bbox_to_anchor=(1.0, 0.92))
    _save(fig, "plot_xe_depletion.png")

    # ------------------------------------------------------------------
    # Plot 5: Fuel matrix swelling (vs Spino 2006)
    # ------------------------------------------------------------------
    fig, ax1 = plt.subplots()

    # Reference test: single total-swelling curve.
    if sd_reference is not None:
        total_ref = (sd_reference["swe_igs"] + sd_reference["swe_igb"]
                     + 0.0032 * sd_reference["fima"])
        ax1.plot(sd_reference["bu"] / 0.8814, total_ref,
                 "-", color=COLOR_REFERENCE, linewidth=LINEWIDTH_MODEL,
                 label=label_reference + " (total)")

    # Primary test: keep the full breakdown.
    ax1.plot(sd_primary["bu"] / 0.8814, 0.00303 * sd_primary["fima"],
             "-.", color=COLOR_SPINO, linewidth=LINEWIDTH_REF,
             label="Solid fission products (Olander)")
    ax1.plot(sd_primary["bu"] / 0.8814, sd_primary["swe_igs"],
             "-.", color=COLOR_UNE_LOW, linewidth=LINEWIDTH_REF,
             label="Intra-granular gas in solution")
    ax1.plot(sd_primary["bu"] / 0.8814, sd_primary["swe_igb"],
             "-.", color=COLOR_CURRENT, linewidth=LINEWIDTH_REF,
             label="Intra-granular gas in bubbles")
    total_primary = (sd_primary["swe_igs"] + sd_primary["swe_igb"]
                     + 0.0032 * sd_primary["fima"])
    ax1.plot(sd_primary["bu"] / 0.8814, total_primary,
             "-", color="#1a365d", linewidth=LINEWIDTH_MODEL,
             label=label_primary + " (total)")

    ax1.scatter(spino_bu, spino_sw,
                color=COLOR_WALKER, edgecolors="black",
                marker="o", s=22, label="Spino et al. 2006")
    ax1.set_xlabel("Burnup (MWd/kgHM)")
    ax1.set_ylabel("Fuel matrix swelling (/)")
    ax1.set_xlim(0, 145)

    ax2 = ax1.twinx()
    ax2.plot(sd_primary["bu"] / 0.8814, sd_primary["alpha"],
             "--", color=COLOR_ALPHA, linewidth=LINEWIDTH_MODEL,
             label=r"Restructured fraction ($\alpha_r$, this work)")
    ax2.set_ylabel("Restructured volume fraction (/)")
    ax2.set_ylim(0.0, 1.05)
    ax2.grid(False)

    lines1, labels1 = ax1.get_legend_handles_labels()
    lines2, labels2 = ax2.get_legend_handles_labels()
    ax1.legend(lines1 + lines2, labels1 + labels2, loc="upper left")
    _save(fig, "plot_fuel_swelling.png")

    # ------------------------------------------------------------------
    # Plot 6: Pore variance B (diagnostic, primary test only)
    #
    # Plot 7: Coefficient of variation CV (diagnostic, primary test only)
    #
    # Plot 8: Xenon inventory stacked area (primary test only)
    #
    # These three plots are specific to the cluster-dynamics machinery of the
    # primary test. The semi-empirical reference does not populate the
    # underlying Fokker-Planck moments or the full xenon-reservoir breakdown,
    # so overlaying it is either meaningless (all zeros) or misleading
    # (totals only, obscuring the per-reservoir story). They are drawn here
    # using primary data exclusively.
    # ------------------------------------------------------------------
    if primary["has_moments"]:
        fig, ax = _new_axes(
            x_label_bu,
            r"Pore variance $B$ (at$^2$ m$^{-3}$)",
        )
        ax.plot(primary["burnup"], primary["B_raw"],
                "-", color=COLOR_CURRENT, linewidth=LINEWIDTH_MODEL,
                label=r"$B = \sum c_n\,(n - \bar{n})^2$")
        ax.set_xlim(0, 210)
        ax.legend(loc="upper right")
        _save(fig, "plot_pore_variance.png")

        fig, ax = _new_axes(
            x_label_bu,
            r"Coefficient of variation $\sigma_n / \bar{n}$ (/)",
        )
        ax.plot(primary["burnup"], primary["CV"],
                "-", color=COLOR_CURRENT, linewidth=LINEWIDTH_MODEL,
                label=r"CV $= \sqrt{M_2}\,/\,\bar{n}$")
        ax.set_xlim(0, 210)
        ax.set_ylim(0, None)
        ax.legend(loc="upper right")
        _save(fig, "plot_CV.png")
    else:
        print(f"[regression_hbs] {PRIMARY_FOLDER} has no cluster-dynamics moments; "
              "plot_pore_variance and plot_CV skipped.")

    if primary["xe_inventory"] is not None:
        xe = primary["xe_inventory"]
        scale = 1.0e26

        xe_ig    = xe["Xe in grain (at/m3)"]               / scale
        xe_gb    = xe["Xe at grain boundary (at/m3)"]      / scale
        xe_igHBS = xe["Xe in grain HBS (at/m3)"]           / scale
        xe_gbHBS = xe["Xe at grain boundary HBS (at/m3)"]  / scale
        xe_pore  = xe["Xe in HBS pores (at/m3)"]           / scale
        xe_rel   = xe["Xe released (at/m3)"]               / scale
        xe_total = (xe["Xe produced (at/m3)"]
                    + xe["Xe produced in HBS (at/m3)"]) / scale

        reservoirs = [
            (xe_ig,    "Xe in NR grain",           "#1a3c6e"),
            (xe_gb,    "Xe at NR grain boundary",  "#4a7bb5"),
            (xe_igHBS, "Xe in HBS grain",          "#f4a65a"),
            (xe_gbHBS, "Xe at HBS grain boundary", "#e6703f"),
            (xe_pore,  "Xe in HBS pores",          "#b23838"),
            (xe_rel,   "Xe released",              "#4a4a4a"),
        ]

        labels = [label for _, label, _ in reservoirs]
        colors = [color for _, _, color in reservoirs]
        series = np.vstack([y for y, _, _ in reservoirs])

        total_nz = np.where(xe_total > 0.0, xe_total, 1.0)
        fractions = 100.0 * series / total_nz

        fig, (ax_top, ax_bot) = plt.subplots(
            2, 1, figsize=(8.0, 8.0), sharex=True,
            gridspec_kw={"height_ratios": [1.1, 1.0], "hspace": 0.08},
        )

        ax_top.stackplot(primary["burnup"], series,
                         labels=labels, colors=colors,
                         alpha=0.92, linewidth=0)
        ax_top.plot(primary["burnup"], xe_total,
                    "--", color="black", linewidth=LINEWIDTH_REF,
                    label="Xe produced (total)")
        ax_top.set_ylabel(r"Xe inventory ($10^{26}$ at m$^{-3}$)")
        ax_top.set_xlim(0, 210)
        ax_top.set_ylim(bottom=0.0)
        ax_top.legend(loc="upper left", ncol=1, fontsize=8.5)

        ax_bot.stackplot(primary["burnup"], fractions,
                         labels=labels, colors=colors,
                         alpha=0.92, linewidth=0)
        ax_bot.set_xlabel(x_label_bu)
        ax_bot.set_ylabel("Share of produced Xe (%)")
        ax_bot.set_xlim(0, 210)
        ax_bot.set_ylim(0.0, 100.0)

        ax_alpha = ax_bot.twinx()
        ax_alpha.plot(sd_primary["bu"] / 0.8814, 100.0 * sd_primary["alpha"],
                      "-", color=COLOR_ALPHA, linewidth=LINEWIDTH_MODEL,
                      label=r"Restructured fraction $\alpha_r$")
        ax_alpha.set_ylabel(r"$\alpha_r$ (%)", color=COLOR_ALPHA)
        ax_alpha.tick_params(axis="y", colors=COLOR_ALPHA)
        ax_alpha.set_ylim(0.0, 100.0)
        ax_alpha.grid(False)

        lines_bot, labels_bot = ax_bot.get_legend_handles_labels()
        lines_alpha, labels_alpha = ax_alpha.get_legend_handles_labels()
        ax_bot.legend(lines_alpha + lines_bot, labels_alpha + labels_bot,
                      loc="upper left", ncol=1, fontsize=8.5)

        _save(fig, "plot_xe_inventory.png")
    else:
        print(f"[regression_hbs] {PRIMARY_FOLDER} has no xenon-inventory columns; "
              "plot_xe_inventory skipped.")

    return folderList, number_of_tests, number_of_tests_failed