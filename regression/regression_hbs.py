"""
HBS regression driver and plotting.

Runs SCIANTIX on every ``test_UO2HBS*`` folder found in the working path,
compares the output against gold data, and (optionally) produces a fixed
set of validation plots:

    1. plot_pore_density.png   - HBS pore number density vs effective burnup
    2. plot_porosity.png       - HBS porosity vs effective burnup
    3. plot_pore_radius.png    - HBS mean pore radius vs effective burnup
    4. plot_xe_depletion.png   - Xe retention in grains vs Walker 1999
    5. plot_fuel_swelling.png  - Matrix swelling breakdown vs Spino 2005

Earlier versions of this script also emitted "simple" duplicates and D_gb^v
sensitivity plots; both were removed in the cleanup.

@author G. Zullo
"""

import os

import matplotlib.pyplot as plt
import numpy as np

from regression_functions import *

# =============================================================================
# Plot style
# =============================================================================

FIG_SIZE = (8.0, 5.5)
DPI = 150

# Consistent color palette across plots
COLOR_CURRENT = "#2ca02c"       # SCIANTIX 2.2.1 - current branch
COLOR_LEGACY = "#1f77b4"        # SCIANTIX 2.0   - reference
COLOR_BARANI = "#8b4513"        # Barani (2022) JNM 563 - reference
COLOR_CAPPIA = "#555555"
COLOR_SPINO = "#c44545"
COLOR_SPINO_LIGHT = "#d98080"
COLOR_SPINO_DARK = "#7a2828"
COLOR_NOIROT = "#1f6aa3"
COLOR_LASSMANN = "#ff7f0e"
COLOR_UNE_LOW = "#8e44ad"
COLOR_UNE_STRONG = "#e377c2"
COLOR_WALKER = "#1a3c6e"
COLOR_ALPHA = "#f39c12"         # restructured volume fraction (twin axis)
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


def _new_axes(xlabel, ylabel, title=None):
    """Create a single-axes figure with standardised labels."""
    fig, ax = plt.subplots()
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    if title is not None:
        ax.set_title(title)
    return fig, ax


def _save(fig, filename):
    """Save and close a figure in a consistent way."""
    fig.tight_layout()
    fig.savefig(filename, dpi=DPI, bbox_inches="tight")
    plt.close(fig)


def _lassmann_fit(bu, threshold=60.0):
    """
    K. Lassmann et al., JNM 226 (1995) 1-8. Xe retention (wt%) vs local
    burnup (GWd/tU). Linear build-up up to ``threshold`` then exponential
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


# =============================================================================
# Main regression driver
# =============================================================================

def regression_hbs(wpath, mode_HBS, mode_gold, mode_plot,
                   folderList, number_of_tests, number_of_tests_failed):

    if mode_HBS == 0:
        return folderList, number_of_tests, number_of_tests_failed

    for entry in sorted(os.listdir(wpath)):
        if "HBS" not in entry or not os.path.isdir(entry):
            continue

        folderList.append(entry)
        os.chdir(entry)
        print(f"Now in folder {entry}...")
        number_of_tests += 1

        # ---------------------------------------------------------------------
        # Run SCIANTIX / check against gold according to mode_gold
        # ---------------------------------------------------------------------
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

        # ---------------------------------------------------------------------
        # Extract SCIANTIX variables from the current run
        # ---------------------------------------------------------------------
        burnup_pos = findSciantixVariablePosition(data, "Burnup (MWd/kgUO2)")
        eff_bu_pos = findSciantixVariablePosition(data, "Effective burnup (MWd/kgUO2)")
        pore_den_pos = findSciantixVariablePosition(data, "HBS pore density (pores/m3)")
        porosity_pos = findSciantixVariablePosition(data, "HBS porosity (/)")
        pore_rad_pos = findSciantixVariablePosition(data, "HBS pore radius (m)")

        burnup = data[1:, burnup_pos].astype(float) / 0.8814
        effectiveBurnup = data[1:, eff_bu_pos].astype(float) / 0.8814
        poreDensity = data[1:, pore_den_pos].astype(float)
        porosity = data[1:, porosity_pos].astype(float)
        poreRadius = data[1:, pore_rad_pos].astype(float)

        # Variance of the pore-size distribution (Fokker-Planck second moment)
        M2_pos = findSciantixVariablePosition(data, "Xe atoms per HBS pore - variance (at^2/pore)")
        nXe_pos = findSciantixVariablePosition(data, "Xe atoms per HBS pore (at/pore)")
        B_pos = findSciantixVariablePosition(data, "Xe in HBS pores - variance (at^2/m3)")

        M2 = data[1:, M2_pos].astype(float)             # at^2/pore
        nXe_per_pore = data[1:, nXe_pos].astype(float)  # at/pore
        B_raw = data[1:, B_pos].astype(float)            # at^2/m^3

        # Coefficient of variation CV = sigma_n / n_mean (guard: Np, n > 0)
        safe = (poreDensity > 0) & (nXe_per_pore > 0) & (poreRadius > 0)
        sigma_n = np.where(safe, np.sqrt(np.maximum(M2, 0.0)), 0.0)
        CV = np.where(safe, sigma_n / nXe_per_pore, 0.0)

        # Propagated standard deviations (first-order Taylor, R ~ n^{1/3})
        sigma_R = poreRadius * CV / 3.0          # std dev of pore radius
        sigma_xi = porosity * CV                  # std dev of porosity

        sd = sciantix_dictionary("output.txt")

        if mode_plot != 1:
            os.chdir("..")
            continue

        # ---------------------------------------------------------------------
        # Load experimental datasets and legacy reference curves
        # ---------------------------------------------------------------------
        exp_np_cappia = import_data("exp_pore_density.txt").astype(float)
        exp_np_spino_67 = import_data("exp_pore_density_2.txt").astype(float)
        exp_np_spino_80 = import_data("exp_pore_density_3.txt").astype(float)
        exp_np_spino_98 = import_data("exp_pore_density_4.txt").astype(float)

        exp_p_cappia = import_data("exp_porosity.txt").astype(float)
        exp_p_spino = import_data("exp_porosity_2.txt").astype(float)
        exp_p_noirot = import_data("exp_porosity_3.txt").astype(float)
        exp_p_lassmann = import_data("exp_porosity_4.txt").astype(float)
        exp_p_une_low = import_data("exp_porosity_5.txt").astype(float)
        exp_p_une_strong = import_data("exp_porosity_6.txt").astype(float)

        exp_r_cappia = import_data("exp_pore_radius.txt").astype(float)
        exp_r_spino = import_data("exp_pore_radius_2.txt").astype(float)

        old_density = np.genfromtxt("SCIANTIX_2.0_density.txt")
        old_porosity = np.genfromtxt("SCIANTIX_2.0_porosity.txt")
        old_radius = np.genfromtxt("SCIANTIX_2.0_radius.txt")

        barani_density = np.genfromtxt("Barani_pore_density.txt")
        barani_porosity = np.genfromtxt("Barani_porosity.txt")
        barani_radius = np.genfromtxt("Barani_pore_radius.txt")

        x_label_bu_eff = r"Effective burnup (MWd kgHM$^{-1}$)"

        # ---------------------------------------------------------------------
        # Plot 1: HBS pore number density
        # ---------------------------------------------------------------------
        fig, ax = _new_axes(
            x_label_bu_eff,
            r"Pore number density (pores m$^{-3}$)",
        )
        ax.plot(exp_np_cappia[:, 0], exp_np_cappia[:, 1],
                "o", color=COLOR_CAPPIA, markersize=MARKER_SIZE,
                label="Cappia et al. (2016)")
        ax.plot(exp_np_spino_67[:, 0], exp_np_spino_67[:, 1],
                "s", color=COLOR_SPINO_LIGHT, markersize=MARKER_SIZE,
                label=r"Spino et al. (2006), Bu$_\mathrm{AV}=67$ MWd/kgU")
        ax.plot(exp_np_spino_80[:, 0], exp_np_spino_80[:, 1],
                "s", color=COLOR_SPINO, markersize=MARKER_SIZE,
                label=r"Spino et al. (2006), Bu$_\mathrm{AV}=80$ MWd/kgU")
        ax.plot(exp_np_spino_98[:, 0], exp_np_spino_98[:, 1],
                "s", color=COLOR_SPINO_DARK, markersize=MARKER_SIZE,
                label=r"Spino et al. (2006), Bu$_\mathrm{AV}=98$ MWd/kgU")
        ax.plot(barani_density[:, 0], barani_density[:, 1],
                "-", color=COLOR_BARANI, linewidth=LINEWIDTH_MODEL,
                label="Barani (2022)")
        ax.plot(old_density[:, 0], old_density[:, 1],
                "-", color=COLOR_LEGACY, linewidth=LINEWIDTH_MODEL,
                label="SCIANTIX 2.0 (2024)")
        ax.plot(effectiveBurnup, poreDensity,
                "-", color=COLOR_CURRENT, linewidth=LINEWIDTH_MODEL,
                label="SCIANTIX 2.2.1 (2026)")
        ax.set_xlim(0, 210)
        ax.legend(loc="upper right")
        _save(fig, "plot_pore_density.png")

        # ---------------------------------------------------------------------
        # Plot 2: HBS porosity
        # ---------------------------------------------------------------------
        fig, ax = _new_axes(
            x_label_bu_eff,
            "HBS porosity (/)",
        )
        ax.plot(exp_p_cappia[:, 0], exp_p_cappia[:, 1],
                "o", color=COLOR_CAPPIA, markersize=MARKER_SIZE,
                label="Cappia et al. (2016)")
        ax.plot(exp_p_spino[:, 0], exp_p_spino[:, 1],
                "s", color=COLOR_SPINO, markersize=MARKER_SIZE,
                label="Spino et al. (2006)")
        ax.plot(exp_p_noirot[:, 0], exp_p_noirot[:, 1],
                "^", color=COLOR_NOIROT, markersize=MARKER_SIZE,
                label="Noirot et al. (2008)")
        ax.plot(exp_p_lassmann[:, 0], exp_p_lassmann[:, 1],
                "D", color=COLOR_LASSMANN, markersize=MARKER_SIZE,
                label="Lassmann et al. (2003)")
        ax.plot(exp_p_une_low[:, 0], exp_p_une_low[:, 1],
                "v", color=COLOR_UNE_LOW, markersize=MARKER_SIZE,
                label="Une et al. (2001), low PCMI")
        ax.plot(exp_p_une_strong[:, 0], exp_p_une_strong[:, 1],
                "v", color=COLOR_UNE_STRONG, markersize=MARKER_SIZE,
                label="Une et al. (2001), strong PCMI")
        ax.plot(barani_porosity[:, 0], barani_porosity[:, 1],
                "-", color=COLOR_BARANI, linewidth=LINEWIDTH_MODEL,
                label="Barani (2022)")
        ax.plot(old_porosity[:, 0], old_porosity[:, 1],
                "-", color=COLOR_LEGACY, linewidth=LINEWIDTH_MODEL,
                label="SCIANTIX 2.0 (2023)")
        ax.plot(effectiveBurnup, porosity,
                "-", color=COLOR_CURRENT, linewidth=LINEWIDTH_MODEL,
                label="SCIANTIX 2.2.1 (2026)")
        ax.fill_between(effectiveBurnup,
                        np.maximum(porosity - sigma_xi, 0.0),
                        porosity + sigma_xi,
                        alpha=0.20, color=COLOR_CURRENT, linewidth=0,
                        label=r"$\pm\sigma_\xi$ (Fokker-Planck)")
        ax.set_xlim(0, 210)
        ax.set_ylim(0, 0.25)
        ax.legend(loc="upper left", ncol=1)
        _save(fig, "plot_porosity.png")

        # ---------------------------------------------------------------------
        # Plot 3: HBS pore mean radius
        # ---------------------------------------------------------------------
        fig, ax = _new_axes(
            x_label_bu_eff,
            "Pore radius (m)",
        )
        ax.plot(exp_r_cappia[:, 0], exp_r_cappia[:, 1],
                "o", color=COLOR_CAPPIA, markersize=MARKER_SIZE,
                label="Cappia et al. (2016)")
        ax.plot(exp_r_spino[:, 0], exp_r_spino[:, 1],
                "s", color=COLOR_SPINO, markersize=MARKER_SIZE,
                label="Spino et al. (2006)")
        ax.plot(barani_radius[:, 0], barani_radius[:, 1],
                "-", color=COLOR_BARANI, linewidth=LINEWIDTH_MODEL,
                label="Barani (2022)")
        ax.plot(old_radius[:, 0], old_radius[:, 1],
                "-", color=COLOR_LEGACY, linewidth=LINEWIDTH_MODEL,
                label="SCIANTIX 2.0 (2023)")
        ax.plot(effectiveBurnup, poreRadius,
                "-", color=COLOR_CURRENT, linewidth=LINEWIDTH_MODEL,
                label="SCIANTIX 2.2.1 (2026)")
        ax.fill_between(effectiveBurnup,
                        np.maximum(poreRadius - sigma_R, 0.0),
                        poreRadius + sigma_R,
                        alpha=0.20, color=COLOR_CURRENT, linewidth=0,
                        label=r"$\pm\sigma_R$ (Fokker-Planck)")
        ax.set_xlim(0, 210)
        ax.legend(loc="upper left")
        _save(fig, "plot_pore_radius.png")

        # ---------------------------------------------------------------------
        # Plot 4: Xenon depletion in grains (vs Walker 1999 + Lassmann fit)
        # ---------------------------------------------------------------------
        walker = np.genfromtxt("walker_data_1999.txt")
        walker_bu = walker[1:, 0]
        walker_xe = walker[1:, 1]

        bu_range = np.linspace(0.0, 200.0, 1000)
        Xe_eq = 4.88897e26  # normalisation for wt%

        fig, ax1 = plt.subplots()
        ax1.plot(bu_range, _lassmann_fit(bu_range),
                 "--", color=COLOR_LASSMANN_FIT, linewidth=LINEWIDTH_REF,
                 label=r"Lassmann fit (Bu$_0=60$ GWd/tU)")
        ax1.scatter(walker_bu, walker_xe,
                    color=COLOR_WALKER, edgecolors="black",
                    marker="o", s=22, label="Walker (1999)")
        ax1.plot(sd["bu"] / 0.8814, sd["xe_ig"] / Xe_eq,
                 "-.", color=COLOR_CURRENT, linewidth=LINEWIDTH_REF,
                 label="Xe in grains (non-HBS)")
        ax1.plot(sd["bu"] / 0.8814, sd["xe_igHBS"] / Xe_eq,
                 ":", color=COLOR_SPINO, linewidth=LINEWIDTH_REF,
                 label="Xe in grains (HBS)")
        ax1.plot(sd["bu"] / 0.8814,
                 (sd["xe_igHBS"] + sd["xe_ig"]) / Xe_eq,
                 "-", color="#2c2c2c", linewidth=LINEWIDTH_MODEL,
                 label="Xe in grains (total)")
        ax1.set_xlabel("Burnup (GWd/tU)")
        ax1.set_ylabel("Xe in grains (wt%)")
        ax1.set_xlim(0, 200)
        ax1.set_ylim(0, 1.75)

        ax2 = ax1.twinx()
        ax2.plot(sd["bu"] / 0.8814, sd["alpha"],
                 "-", color=COLOR_ALPHA, linewidth=LINEWIDTH_MODEL,
                 label="Restructured fraction")
        ax2.set_ylabel("Restructured volume fraction (/)")
        ax2.set_ylim(0.0, 1.05)
        ax2.grid(False)

        lines1, labels1 = ax1.get_legend_handles_labels()
        lines2, labels2 = ax2.get_legend_handles_labels()
        ax1.legend(lines1 + lines2, labels1 + labels2, loc="upper right")
        _save(fig, "plot_xe_depletion.png")

        # ---------------------------------------------------------------------
        # Plot 5: Fuel matrix swelling (vs Spino 2005)
        # ---------------------------------------------------------------------
        spino_swe = np.genfromtxt("spino_swelling_data.txt")
        spino_bu = spino_swe[1:, 0]
        spino_sw = spino_swe[1:, 1]

        fig, ax1 = plt.subplots()
        ax1.plot(sd["bu"] / 0.8814, 0.00303 * sd["fima"],
                 "-.", color=COLOR_SPINO, linewidth=LINEWIDTH_REF,
                 label="Solid fission products (Olander)")
        ax1.plot(sd["bu"] / 0.8814, sd["swe_igs"],
                 "-.", color=COLOR_UNE_LOW, linewidth=LINEWIDTH_REF,
                 label="Intra-granular gas in solution")
        ax1.plot(sd["bu"] / 0.8814, sd["swe_igb"],
                 "-.", color=COLOR_CURRENT, linewidth=LINEWIDTH_REF,
                 label="Intra-granular gas in bubbles")
        ax1.plot(sd["bu"] / 0.8814,
                 sd["swe_igs"] + sd["swe_igb"] + 0.0032 * sd["fima"],
                 "-", color="#1a365d", linewidth=LINEWIDTH_MODEL,
                 label="Total matrix swelling")
        ax1.scatter(spino_bu, spino_sw,
                    color=COLOR_WALKER, edgecolors="black",
                    marker="o", s=22, label="Spino et al. (2005)")
        ax1.set_xlabel("Burnup (GWd/tU)")
        ax1.set_ylabel("Fuel matrix swelling (/)")
        ax1.set_xlim(0, 145)

        ax2 = ax1.twinx()
        ax2.plot(sd["bu"] / 0.8814, sd["alpha"],
                 "--", color=COLOR_ALPHA, linewidth=LINEWIDTH_MODEL,
                 label="Restructured fraction")
        ax2.set_ylabel("Restructured volume fraction (/)")
        ax2.set_ylim(0.0, 1.05)
        ax2.grid(False)

        lines1, labels1 = ax1.get_legend_handles_labels()
        lines2, labels2 = ax2.get_legend_handles_labels()
        ax1.legend(lines1 + lines2, labels1 + labels2, loc="upper left")
        _save(fig, "plot_fuel_swelling.png")

        # ---------------------------------------------------------------------
        # Plot 6: Pore variance B (diagnostic, not for paper)
        # ---------------------------------------------------------------------
        fig, ax = _new_axes(
            x_label_bu_eff,
            r"Pore variance $B$ (at$^2$ m$^{-3}$)",
        )
        ax.plot(effectiveBurnup, B_raw,
                "-", color=COLOR_CURRENT, linewidth=LINEWIDTH_MODEL,
                label=r"$B = \sum c_n\,(n - \bar{n})^2$")
        ax.set_xlim(0, 210)
        ax.legend(loc="upper right")
        _save(fig, "plot_pore_variance.png")

        # ---------------------------------------------------------------------
        # Plot 7: Coefficient of variation CV (diagnostic)
        # ---------------------------------------------------------------------
        # CV = sigma_n / n_mean should show a "U" shape: high at onset (few
        # heterogeneous pores), minimum near the N_p peak (many similar-sized
        # pores), rising again at high burnup (nucleation off, old pores
        # coarsen while distribution broadens). This is a qualitative test
        # of the Fokker-Planck cluster-dynamics formulation.
        fig, ax = _new_axes(
            x_label_bu_eff,
            r"Coefficient of variation $\sigma_n / \bar{n}$ (/)",
        )
        ax.plot(effectiveBurnup, CV,
                "-", color=COLOR_CURRENT, linewidth=LINEWIDTH_MODEL,
                label=r"CV $= \sqrt{M_2}\,/\,\bar{n}$")
        ax.set_xlim(0, 210)
        ax.set_ylim(0, None)
        ax.legend(loc="upper right")
        _save(fig, "plot_CV.png")

        os.chdir("..")

    return folderList, number_of_tests, number_of_tests_failed
