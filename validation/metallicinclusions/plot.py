"""
sciantix testing suite
author: Vittoria Bellani, Elisa Cappellari

Five metal particles (FMP: Mo, Tc, Ru, Rh, Pd) in UO2 irradiated in LWR conditions.

    python3 validation/metallicinclusions/plot.py

Run the groups first (python3 -m testing.runner --metallicinclusions [--metallicinclusions-oc --oc]);
when a case has no output.txt its output_gold.txt is used. Figures are written in figures/:

- metallicinclusions_UO2_LWR.png : calibration cases vs TEM data at four radial positions, 54.1 MWd/kgUO2
- buck2015_ATM109.png            : independent validation, Buck et al. (2015), ATM-109 BWR fuel, 70.8 GWd/tHM
- fmp_partition.png              : single inventory, produced metal split into dissolved metal (grain
                                   interior and grain-boundary zone), intragranular and grain-boundary
                                   particles, vs burnup
- fmp_elements.png               : the five elements, produced inventory and end-of-life partition
- fmp_thermochemistry.png        : OpenCalphad coupling and oxygen balance (oxygen potential, O/M, Mo
                                   oxidation, oxygen taken by the Mo oxides, grain-boundary phases and radius)

Observables of the calibration: mean radius R from the SCIANTIX output (R = (3 n Omega / 4 pi)^(1/3), n atoms
per particle, Omega = 1.44123e-29 m3), projected area fraction through a TEM foil of thickness t,
A = 1 - exp(-n pi R^2 t), with n the number density in the zone of the population: N_intra / phi_1 in the
grain interior and N_GB / phi_2 in the grain-boundary zone (layer of 100 nm on each side of the boundary, the
TEM grain-boundary regions). J = mean |ln(model/exp)| over A_intra, R_intra, R_GB at the four positions, as in
the calibration; J4 adds A_GB, which is now computed on the same region as the TEM grain-boundary images.

EC November 2026: translated to English, FMP naming, radius read from the new output columns, new figures
(Buck et al. 2015 validation, inventory partition, elements, thermochemistry), two-zone model (area fractions
on the zone volumes) and oxygen balance.
"""

import os
import sys
import math
import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.ticker

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..")))

from testing.core.common import load_named

GROUP_DIR = os.path.dirname(os.path.abspath(__file__))
DATA_DIR = os.path.join(GROUP_DIR, "data")
OC_DIR = os.path.join(GROUP_DIR, "thermochemistry")
FIGURES_DIR = os.path.join(GROUP_DIR, "figures")

BURNUP_EXP = 54.10116  # MWd/kgUO2, TEM data of the calibration
ELEMENTS = ["Mo", "Tc", "Ru", "Rh", "Pd"]

# Categorical palette (fixed order, validated for colour-vision deficiency); every series also has its own
# marker or line style and a legend entry.
C = ["#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4"]
INK = "#333333"
MARKERS = ["o", "s", "^", "D", "v"]
LINESTYLES = ["-", "--", "-.", ":", (0, (5, 1, 1, 1))]

# name, r/R, TEM foil thickness (m)
POSITIONS = [
    ("r000", 0.00, 50e-9),  # nominal thickness
    ("r030", 0.30, 50e-9),  # nominal thickness
    ("r056", 0.56, 70e-9),
    ("r075", 0.75, 40e-9),
]

# Buck et al. (2015): four equal-volume rings, name and r/R at the middle of the ring
BUCK_RINGS = [("r035", 0.354), ("r061", 0.612), ("r079", 0.791), ("r094", 0.935)]

plt.rcParams.update({
    "font.size": 9, "axes.titlesize": 9.5, "axes.labelsize": 9, "legend.fontsize": 7.5,
    "axes.edgecolor": "#888888", "axes.linewidth": 0.8, "axes.grid": True, "grid.color": "#e6e6e6",
    "grid.linewidth": 0.6, "lines.linewidth": 1.8, "lines.markersize": 6, "legend.frameon": False,
    "axes.spines.top": False, "axes.spines.right": False,
})


def load_case(case_dir, stem="output"):
    filename = f"{stem}.txt" if os.path.isfile(os.path.join(case_dir, f"{stem}.txt")) else f"{stem}_gold.txt"
    if not os.path.isfile(os.path.join(case_dir, filename)):
        return None
    return load_named(case_dir, filename)


def produced_total(out):
    return sum(out.get_all(f"{e} produced (at/m3)") for e in ELEMENTS)


def read_exp(path):
    """One header line + one data row (files have CR line endings)."""
    with open(path, newline="") as f:
        lines = [line for line in f.read().splitlines() if line.strip()]
    header, values = lines[0].split(), [float(v) for v in lines[1].split()]
    row = dict(zip(header, values))
    return {
        "area_fraction": row["Inclusions"],
        "radius": row["Average_inclusion_radius"] * 1e-6,  # um -> m
        "std_radius": row["Standard_Deviation_of_inclusion_radius"] * 1e-6,
    }


def model_at_burnup(out, burnup, tem_thickness):
    i = int(np.nanargmin(np.abs(out.get_all("Burnup (MWd/kgUO2)") - burnup)))
    at = lambda var: out.get_all(var)[i]
    r_intra, r_gb = at("Intragranular FMP radius (m)"), at("Intergranular FMP radius (m)")
    # local number densities: grain interior (zone 1) and grain-boundary zone (zone 2)
    phi_gb = at("FMP grain-boundary zone volume fraction (/)")
    n_intra = max(at("Intragranular FMP concentration (FMP/m3)"), 0.0) / (1.0 - phi_gb)
    n_gb = max(at("Intergranular FMP concentration (FMP/m3)"), 0.0) / phi_gb
    total = produced_total(out)[i]
    return {
        "radius_intra": r_intra,
        "radius_gb": r_gb,
        "area_intra": -math.expm1(-n_intra * math.pi * r_intra**2 * tem_thickness),
        "area_gb": -math.expm1(-n_gb * math.pi * r_gb**2 * tem_thickness),
        "matrix_fraction": at("FMP in matrix (at/m3)") / total if total > 0 else float("nan"),
        "gb_fraction": at("FMP at grain boundary (at/m3)") / total if total > 0 else float("nan"),
    }


def log_error(model, exp):
    return abs(math.log(model / exp)) if model > 0.0 else float("inf")


def save(fig, name):
    os.makedirs(FIGURES_DIR, exist_ok=True)
    path = os.path.join(FIGURES_DIR, name)
    fig.savefig(path, dpi=170, bbox_inches="tight", facecolor="white")
    plt.close(fig)
    print(f"Figure: {path}")


# --------------------------------------------------------------------------------------------------------------
# 1. Calibration cases vs TEM
# --------------------------------------------------------------------------------------------------------------
def plot_calibration():
    rows = []
    for name, r_over_R, thickness in POSITIONS:
        out = load_case(os.path.join(GROUP_DIR, f"test_FMP_UO2_{name}"))
        rows.append({
            "name": name, "r_over_R": r_over_R, "model": model_at_burnup(out, BURNUP_EXP, thickness),
            "intra": read_exp(os.path.join(DATA_DIR, "intra", f"exp_intra_{name}.txt")),
            "inter": read_exp(os.path.join(DATA_DIR, "inter", f"exp_inter_{name}.txt")),
        })

    errors, errors_area_gb = [], []
    print("Calibration cases (TEM, 54.1 MWd/kgUO2)")
    print(f"{'case':<6}{'r/R':>6}  {'R_intra exp/mod (nm)':>22}  {'R_GB exp/mod (nm)':>20}  "
          f"{'A_intra exp/mod (%)':>21}  {'A_GB exp/mod (%)':>20}  {'in matrix':>9}  {'in GB':>6}")
    for row in rows:
        m, ei, eg = row["model"], row["intra"], row["inter"]
        errors += [log_error(m["area_intra"], ei["area_fraction"]), log_error(m["radius_intra"], ei["radius"]),
                   log_error(m["radius_gb"], eg["radius"])]
        errors_area_gb.append(log_error(m["area_gb"], eg["area_fraction"]))
        print(f"{row['name']:<6}{row['r_over_R']:>6.2f}  "
              f"{ei['radius']*1e9:>10.2f} /{m['radius_intra']*1e9:>9.2f}  "
              f"{eg['radius']*1e9:>9.2f} /{m['radius_gb']*1e9:>8.2f}  "
              f"{ei['area_fraction']*100:>10.3f} /{m['area_intra']*100:>8.3f}  "
              f"{eg['area_fraction']*100:>9.3f} /{m['area_gb']*100:>8.3f}  "
              f"{m['matrix_fraction']*100:>8.1f}%  {m['gb_fraction']*100:>5.2f}%")
    j3, j4 = np.mean(errors), np.mean(errors + errors_area_gb)
    print(f"J = mean |ln(model/exp)| over {len(errors)} observables = {j3:.3f}; "
          f"with A_GB ({len(errors) + len(errors_area_gb)} observables) J4 = {j4:.3f}\n")

    x = np.array([row["r_over_R"] for row in rows])
    fig, (ax_r, ax_a) = plt.subplots(1, 2, figsize=(10, 3.9))
    ax_r.errorbar(x, [r["intra"]["radius"] * 1e9 for r in rows], yerr=[r["intra"]["std_radius"] * 1e9 for r in rows],
                  fmt="o", color=C[0], capsize=3, label="TEM, intragranular")
    ax_r.errorbar(x + 0.01, [r["inter"]["radius"] * 1e9 for r in rows],
                  yerr=[r["inter"]["std_radius"] * 1e9 for r in rows], fmt="s", color=C[1], capsize=3,
                  label="TEM, grain boundary")
    ax_r.plot(x, [r["model"]["radius_intra"] * 1e9 for r in rows], "-", color=C[0], label="SCIANTIX, intragranular")
    ax_r.plot(x, [r["model"]["radius_gb"] * 1e9 for r in rows], "--", color=C[1], label="SCIANTIX, grain boundary")
    ax_r.set(xlabel="r/R (-)", ylabel="Mean particle radius (nm)", title="FMP radius at 54.1 MWd/kgUO2 (calibration)")
    ax_r.set_ylim(bottom=0)
    ax_r.legend()

    ax_a.semilogy(x, [r["intra"]["area_fraction"] * 100 for r in rows], "o", color=C[0], label="TEM, intragranular")
    ax_a.semilogy(x + 0.01, [r["inter"]["area_fraction"] * 100 for r in rows], "s", color=C[1],
                  label="TEM, grain boundary")
    ax_a.semilogy(x, [r["model"]["area_intra"] * 100 for r in rows], "-", color=C[0], label="SCIANTIX, intragranular")
    ax_a.semilogy(x, [r["model"]["area_gb"] * 100 for r in rows], "--", color=C[1],
                  label="SCIANTIX, grain-boundary zone")
    ax_a.set(xlabel="r/R (-)", ylabel="Projected area fraction (%)",
             title="FMP projected area fraction (TEM foil, zone volumes)")
    ax_a.set_ylim(0.1, 100)
    ax_a.yaxis.set_major_formatter(matplotlib.ticker.FuncFormatter(lambda v, _: f"{v:g}"))
    ax_a.legend(loc="upper right", ncol=2, fontsize=8)
    fig.suptitle(f"Calibration cases, two-zone model: J = {j3:.3f} (A_intra, R_intra, R_GB), "
                 f"J4 = {j4:.3f} (with A_GB)", fontsize=10)
    fig.tight_layout()
    save(fig, "metallicinclusions_UO2_LWR.png")


# --------------------------------------------------------------------------------------------------------------
# 2. Buck et al. (2015), ATM-109: independent validation
# --------------------------------------------------------------------------------------------------------------
def sphere_volume_nm3(radius_m):
    return 4.0 / 3.0 * math.pi * (radius_m * 1e9) ** 3


def diameter_nm(volume_nm3):
    return (6.0 * volume_nm3 / math.pi) ** (1.0 / 3.0)


def read_buck_summary():
    values = {}
    with open(os.path.join(DATA_DIR, "buck2015", "summary.txt")) as f:
        for line in f:
            if line.strip() and not line.startswith("#"):
                key, value = line.split()[:2]
                values[key] = float(value)
    return values


def plot_buck():
    rings = []
    for name, r_over_R in BUCK_RINGS:
        out = load_case(os.path.join(GROUP_DIR, f"test_FMP_UO2_Buck2015_{name}"))
        if out is None:
            print(f"Buck et al. (2015): no output for {name}, figure skipped")
            return
        rings.append((name, r_over_R, out))

    exp = read_buck_summary()
    lnv_exp = {s: np.loadtxt(os.path.join(DATA_DIR, "buck2015", f"fig6_{s}.txt")) for s in ("carbonate", "nitric")}

    # Pellet-averaged population: equal-volume rings, intragranular and grain-boundary particles
    populations = []  # (ln V, number density, label)
    for name, r_over_R, out in rings:
        for kind, rad, conc in (("intra", "Intragranular FMP radius (m)", "Intragranular FMP concentration (FMP/m3)"),
                                ("GB", "Intergranular FMP radius (m)", "Intergranular FMP concentration (FMP/m3)")):
            radius, number = out.get_all(rad)[-1], max(out.get_all(conc)[-1], 0.0)
            if radius > 0.0 and number > 0.0:
                populations.append((math.log(sphere_volume_nm3(radius)), number, f"{name} {kind}"))
    lnv = np.array([p[0] for p in populations])
    weight = np.array([p[1] for p in populations])
    order = np.argsort(lnv)
    lnv, weight = lnv[order], weight[order] / weight.sum()
    d_pop = np.array([diameter_nm(math.exp(v)) for v in lnv])
    d_number_mean = float(np.sum(weight * d_pop))
    d_median = diameter_nm(math.exp(lnv[np.searchsorted(np.cumsum(weight), 0.5)]))

    print("Buck et al. (2015), ATM-109, pellet-averaged population (four equal-volume rings)")
    for name, r_over_R, out in rings:
        total = produced_total(out)[-1]
        print(f"  {name} r/R={r_over_R:.3f} T={out.get_all('Temperature (K)')[0]:.0f} K  "
              f"d_intra={2e9*out.get_all('Intragranular FMP radius (m)')[-1]:.1f} nm  "
              f"d_GB={2e9*out.get_all('Intergranular FMP radius (m)')[-1]:.1f} nm  "
              f"matrix={out.get_all('FMP in matrix (at/m3)')[-1]/total*100:.0f}%  "
              f"GB={out.get_all('FMP at grain boundary (at/m3)')[-1]/total*100:.2f}%")
    for s, label in (("carbonate", "carbonate-peroxide"), ("nitric", "nitric")):
        mu, sig = exp[f"lnV_mu_{'carbonate_peroxide' if s == 'carbonate' else s}"], \
            exp[f"lnV_sigma_{'carbonate_peroxide' if s == 'carbonate' else s}"]
        print(f"  exp {label}: median d = {diameter_nm(math.exp(mu)):.1f} nm, number-mean d = "
              f"{diameter_nm(math.exp(mu)) * math.exp((sig / 3.0) ** 2 / 2.0):.1f} nm (log-normal of Fig. 6)")
    print(f"  model: mean ln V = {np.sum(weight * lnv):.2f}, median d = {d_median:.1f} nm, "
          f"number-mean d = {d_number_mean:.1f} nm\n")

    fig, axes = plt.subplots(1, 3, figsize=(14.5, 4.1))

    # (a) cumulative distribution of ln V, as Fig. 6 of the paper
    ax = axes[0]
    xs = np.linspace(-2, 18, 300)
    from math import erf
    for k, (s, label, key) in enumerate((("carbonate", "carbonate-peroxide", "carbonate_peroxide"),
                                         ("nitric", "nitric acid", "nitric"))):
        data = lnv_exp[s]
        ax.plot(data[:, 0], data[:, 1], MARKERS[k + 1], color=C[k], ms=4.5, mfc="white", mew=1.2,
                label=f"Buck et al. (2015), {label}")
        mu, sig = exp[f"lnV_mu_{key}"], exp[f"lnV_sigma_{key}"]
        ax.plot(xs, [0.5 * (1 + erf((x - mu) / (sig * math.sqrt(2)))) for x in xs], ":", color=C[k], lw=1.2,
                label=f"log-normal fit, {label}")
    ax.step(np.r_[-2, lnv, 18], np.r_[0, np.cumsum(weight), 1], where="post", color=INK, lw=2,
            label="SCIANTIX, pellet average")
    ax.set(xlim=(-2, 18), ylim=(0, 1.02), xlabel="ln particle volume (nm$^3$)", ylabel="Cumulative number fraction (-)",
           title="(a) Particle volume distribution (Fig. 6)")
    top = ax.secondary_xaxis("top", functions=(lambda v: v, lambda v: v))
    ticks_d = [2, 5, 10, 20, 50, 100, 300]
    top.set_xticks([math.log(math.pi / 6 * d**3) for d in ticks_d], [str(d) for d in ticks_d])
    top.set_xlabel("Equivalent diameter (nm)", fontsize=8)
    ax.legend(loc="upper left", fontsize=7)

    # (b) diameter per ring vs the reported sizes
    ax = axes[1]
    ax.axhspan(exp["particle_diameter_min"], exp["particle_diameter_max"], color="#eeeeee", zorder=0,
               label="observed range, 10-300 nm")
    ax.axhspan(exp["mean_particle_size_carbonate_peroxide"], exp["mean_particle_size_nitric"], color="#c9c9c9",
               zorder=0, label="reported mean size, 43-58 nm")
    x = [r for _, r, _ in rings]
    ax.plot(x, [2e9 * out.get_all("Intragranular FMP radius (m)")[-1] for *_, out in rings], "o-", color=C[0],
            label="SCIANTIX, intragranular")
    ax.plot(x, [2e9 * out.get_all("Intergranular FMP radius (m)")[-1] for *_, out in rings], "s", color=C[1],
            mfc="none", ms=9, mew=1.4, label="SCIANTIX, grain boundary")
    ax.axhline(d_number_mean, color=INK, lw=1.4, ls="--", label=f"SCIANTIX, pellet number-mean ({d_number_mean:.0f} nm)")
    ax.set_yscale("log")
    ax.set(xlim=(0, 1), ylim=(5, 400), xlabel="r/R (-)", ylabel="Particle diameter (nm)",
           title="(b) End of irradiation, 70.8 GWd/tHM")
    ax.legend(loc="upper right", fontsize=7)

    # (c) evolution in time
    ax = axes[2]
    for k, (name, r_over_R, out) in enumerate(rings):
        t = out.get_all("Time (h)") / 8766.0 + 1979.083
        ax.plot(t, 2e9 * out.get_all("Intragranular FMP radius (m)"), ls=LINESTYLES[k], color=C[k],
                label=f"r/R = {r_over_R:.2f}, {out.get_all('Temperature (K)')[0]:.0f} K")
    fission = rings[0][2].get_all("Fission rate (fiss / m3 s)")
    t = rings[0][2].get_all("Time (h)") / 8766.0 + 1979.083
    ax.fill_between(t, 0, 1, where=fission <= 0.0, color="#dddddd", transform=ax.get_xaxis_transform(), zorder=0,
                    label="out of the reactor")
    ax.set(xlabel="Year", ylabel="Intragranular particle diameter (nm)", title="(c) Growth during irradiation")
    ax.set_ylim(bottom=0)
    ax.legend(fontsize=7)

    fig.suptitle("Independent validation: ATM-109, Quad Cities I BWR (Buck et al. 2015). "
                 "Power and temperature history assumed (see README)", fontsize=10)
    fig.tight_layout()
    save(fig, "buck2015_ATM109.png")


# --------------------------------------------------------------------------------------------------------------
# 3. Single inventory: matrix / intragranular particles / grain-boundary particles
# --------------------------------------------------------------------------------------------------------------
def stacked_partition(ax, out, title):
    bu = out.get_all("Burnup (MWd/kgUO2)")
    total = produced_total(out)
    ok = total > 0
    dissolved_gb = out.get_all("FMP in grain-boundary zone matrix (at/m3)")
    parts = [out.get_all("FMP in matrix (at/m3)") - dissolved_gb, dissolved_gb,
             out.get_all("FMP in intragranular particles (at/m3)"), out.get_all("FMP at grain boundary (at/m3)")]
    fractions = [np.clip(p[ok] / total[ok], 0, 1) * 100 for p in parts]
    ax.stackplot(bu[ok], *fractions, colors=[C[0], C[4], C[2], C[1]], edgecolor="white", linewidth=0.6,
                 labels=["dissolved, grain interior", "dissolved, grain-boundary zone", "intragranular particles",
                         "grain-boundary particles"])
    ax.set(xlim=(0, bu[-1]), ylim=(0, 100), title=title)
    ax.text(0.97, 0.06, f"GB particles {fractions[3][-1]:.2f}%", transform=ax.transAxes, ha="right", fontsize=7.5,
            color=INK)


def plot_partition():
    buck = [load_case(os.path.join(GROUP_DIR, f"test_FMP_UO2_Buck2015_{n}")) for n, _ in BUCK_RINGS]
    nrows = 2 if all(o is not None for o in buck) else 1
    fig, axes = plt.subplots(nrows, 4, figsize=(13, 3.2 * nrows), sharey=True, squeeze=False)
    for k, (name, r_over_R, _) in enumerate(POSITIONS):
        out = load_case(os.path.join(GROUP_DIR, f"test_FMP_UO2_{name}"))
        stacked_partition(axes[0, k], out, f"Calibration r/R = {r_over_R:.2f} ({out.get_all('Temperature (K)')[0]:.0f} K)")
    if nrows == 2:
        for k, ((name, r_over_R), out) in enumerate(zip(BUCK_RINGS, buck)):
            stacked_partition(axes[1, k], out, f"ATM-109 r/R = {r_over_R:.2f} ({out.get_all('Temperature (K)')[0]:.0f} K)")
    for ax in axes[:, 0]:
        ax.set_ylabel("Share of produced Mo+Tc+Ru+Rh+Pd (%)")
    for ax in axes[-1]:
        ax.set_xlabel("Burnup (MWd/kgUO2)")
    handles, labels = axes[0, 0].get_legend_handles_labels()
    fig.legend(handles, labels, loc="lower center", ncol=4, bbox_to_anchor=(0.5, -0.03))
    fig.suptitle("Single inventory: sum of 'X produced' = dissolved (two zones) + intragranular + grain-boundary "
                 "particles", fontsize=10)
    fig.tight_layout(rect=(0, 0.04, 1, 1))
    save(fig, "fmp_partition.png")


# --------------------------------------------------------------------------------------------------------------
# 4. The five elements
# --------------------------------------------------------------------------------------------------------------
def plot_elements():
    oc = load_case(os.path.join(OC_DIR, "test_FMP_UO2_OC_r000"))
    out = oc if oc is not None else load_case(os.path.join(GROUP_DIR, "test_FMP_UO2_r000"))
    bu = out.get_all("Burnup (MWd/kgUO2)")
    total = produced_total(out)

    fig, (ax_p, ax_b) = plt.subplots(1, 2, figsize=(11.5, 3.9), gridspec_kw={"width_ratios": [1, 1.35]})
    for k, e in enumerate(ELEMENTS):
        ax_p.plot(bu, out.get_all(f"{e} produced (at/m3)") / 1e26, ls=LINESTYLES[k], color=C[k],
                  label=f"{e} ({out.get_all(f'{e} produced (at/m3)')[-1] / total[-1] * 100:.1f}%)")
    ax_p.set(xlabel="Burnup (MWd/kgUO2)", ylabel="Produced (10$^{26}$ at/m$^3$)",
             title="(a) Production, LWR element yields (share of the five)", xlim=(0, bu[-1]))
    ax_p.set_ylim(bottom=0)
    ax_p.legend()

    # (b) Particle composition (at.%). The FMP inventories are lumped, so the particles have the composition of
    # the produced five elements; with OpenCalphad the oxidised Mo is removed from the metallic part of all the
    # particles (same oxygen potential in the whole grain).
    f_ox = float(np.clip(out.get_all("Mo in oxide fraction (/)")[-1], 0, 1)) if oc is not None else 0.0
    share = np.array([out.get_all(f"{e} produced (at/m3)")[-1] / total[-1] for e in ELEMENTS])
    metallic = share.copy()
    metallic[0] *= 1.0 - f_ox
    metallic /= metallic.sum()
    # Undissolved residue of LWR fuel (XRD), 20Mo:0.5-5Tc:50-60Ru:10Rh:10Pd, Adachi et al. (1990) as quoted
    # by Buck et al. (2015); mid-range values, normalised.
    residue = np.array([20.0, 2.75, 55.0, 10.0, 10.0])
    residue /= residue.sum()
    bars = [("SCIANTIX, particles without OC", share),
            ("SCIANTIX, particles (metallic)" + (f" with OC, {f_ox*100:.0f}% of Mo oxidised" if oc is not None else ""),
             metallic),
            ("Dissolution residue, Adachi et al.\n(as quoted by Buck et al. 2015)", residue)]
    y = np.arange(len(bars))[::-1]
    left = np.zeros(len(bars))
    for k, e in enumerate(ELEMENTS):
        width = np.array([b[1][k] for b in bars]) * 100
        ax_b.barh(y, width, left=left, color=C[k], edgecolor="white", linewidth=2, height=0.6, label=e)
        for yi, li, wi in zip(y, left, width):
            if wi >= 6:
                ax_b.text(li + wi / 2, yi, f"{wi:.0f}", ha="center", va="center", fontsize=7.5, color="white")
        left += width
    ax_b.set_yticks(y, [b[0] for b in bars])
    ax_b.set(xlim=(0, 100), xlabel="Composition (at.%)",
             title=f"(b) Particle composition at {bu[-1]:.1f} MWd/kgUO2, r/R = 0")
    ax_b.grid(False)
    ax_b.legend(loc="upper center", bbox_to_anchor=(0.5, -0.2), ncol=5)
    fig.tight_layout()
    save(fig, "fmp_elements.png")


# --------------------------------------------------------------------------------------------------------------
# 5. OpenCalphad coupling at the grain boundary
# --------------------------------------------------------------------------------------------------------------
AVOGADRO = 6.02214076e23


def blackburn_potential(x, temperature):
    """Oxygen potential (kJ/mol O2) of UO(2+x), Blackburn (1973)."""
    x = np.asarray(x, dtype=float)
    out = np.full_like(x, np.nan)
    ok = (x > 0) & (x < 1)
    ln_p = 2 * np.log(x[ok] * (2 + x[ok]) / (1 - x[ok])) + 108 * x[ok] ** 2 - 32700.0 / temperature[ok] + 9.92
    out[ok] = 8.314e-3 * temperature[ok] * ln_p
    return out


def plot_thermochemistry():
    cases = []
    for k, (name, r_over_R, _) in enumerate(POSITIONS):
        oc = load_case(os.path.join(OC_DIR, f"test_FMP_UO2_OC_{name}"))
        if oc is None:
            print("Thermochemistry cases without output, figure skipped")
            return
        cases.append((k, name, r_over_R, oc, load_case(os.path.join(GROUP_DIR, f"test_FMP_UO2_{name}"))))

    fig, axes = plt.subplots(2, 3, figsize=(14, 7.2))
    (ax_mu, ax_x, ax_f), (ax_ph, ax_rgb, ax_o) = axes
    for k, name, r_over_R, oc, ref in cases:
        bu = oc.get_all("Burnup (MWd/kgUO2)")
        temperature = oc.get_all("Temperature (K)")
        style = dict(color=C[k], ls=LINESTYLES[k], label=f"r/R = {r_over_R:.2f} ({temperature[0]:.0f} K)")
        thin = dict(color=C[k], lw=0.9, alpha=0.6)

        # Oxygen balance: excess oxygen E = x M + O in the Mo oxides (Kleykamp); without the balance the fuel
        # would keep all of it, x_K = E / M
        x = oc.get_all("Stoichiometry deviation (/)")
        heavy_metal = oc.get_all("U content (mol/m3)") * AVOGADRO
        oxygen_oxides = oc.get_all("FMP oxygen in Mo oxides (at/m3)")
        x_kleykamp = x + np.divide(oxygen_oxides, heavy_metal, out=np.zeros_like(x), where=heavy_metal > 0)

        mu = oc.get_all("Fuel oxygen potential (KJ/mol)").astype(float).copy()
        mu[mu >= 0.0] = np.nan  # first step, before the oxygen potential is evaluated
        ax_mu.plot(bu, mu, **style)
        ax_mu.plot(bu, blackburn_potential(x_kleykamp, temperature), **thin)
        ax_x.semilogy(bu, np.where(x > 0, x, np.nan), **style)
        ax_x.semilogy(bu, np.where(x_kleykamp > 0, x_kleykamp, np.nan), **thin)
        ax_f.plot(bu, oc.get_all("Mo in oxide fraction (/)"), **style)
        ax_o.plot(bu, 100 * np.divide(oxygen_oxides, x_kleykamp * heavy_metal, out=np.zeros_like(x),
                                      where=x_kleykamp * heavy_metal > 0), **style)
        ax_rgb.plot(bu, oc.get_all("Intergranular FMP radius (m)") * 1e9, **style)
        ax_rgb.plot(ref.get_all("Burnup (MWd/kgUO2)"), ref.get_all("Intergranular FMP radius (m)") * 1e9, **thin)
        tem = read_exp(os.path.join(DATA_DIR, "inter", f"exp_inter_{name}.txt"))
        ax_rgb.errorbar(BURNUP_EXP + 0.8 * k, tem["radius"] * 1e9, yerr=tem["std_radius"] * 1e9, fmt=MARKERS[k], color=C[k],
                        capsize=3, mfc="white", mew=1.3)

    ax_mu.set(xlabel="Burnup (MWd/kgUO2)", ylabel="Oxygen potential (kJ/mol O$_2$)", ylim=(-440, -230),
              title="(a) Oxygen potential: O balance (thick), Kleykamp (thin)")
    ax_x.set(xlabel="Burnup (MWd/kgUO2)", ylabel="x in UO$_{2+x}$ (-)", ylim=(1e-8, 3e-2),
             title="(b) x: O balance (thick), Kleykamp (thin)")
    ax_f.set(xlabel="Burnup (MWd/kgUO2)", ylabel="Mo in oxide phases (-)", ylim=(-0.02, 1.02),
             title="(c) Mo oxidised, all particles (OpenCalphad)")
    ax_o.set(xlabel="Burnup (MWd/kgUO2)", ylabel="Excess oxygen in Mo oxides (%)", ylim=(-2, 102),
             title="(f) Excess oxygen taken by the Mo of the particles")
    ax_rgb.set(xlabel="Burnup (MWd/kgUO2)", ylabel="Grain-boundary particle radius (nm)",
               title="(e) GB radius: OC (thick), no OC (thin), TEM (markers)")
    ax_rgb.set_ylim(bottom=0)
    for ax in (ax_mu, ax_x, ax_f, ax_o):
        ax.legend(fontsize=7)

    # (d) phases at the grain boundary, centre position
    th = load_case(os.path.join(OC_DIR, "test_FMP_UO2_OC_r000"), "thermochemistry_output")
    oc0 = cases[0][3]
    phases = [("MOO2", "MoO$_2$"), ("MO4O11", "Mo$_4$O$_{11}$"), ("HCP_A3", "HCP_A3 (metal)"), ("FCC_A1", "FCC_A1 (metal)"),
              ("BCC_A2", "BCC_A2 (metal)")]
    bu_th = np.interp(th.get_all("Time (h)"), oc0.get_all("Time (h)"), oc0.get_all("Burnup (MWd/kgUO2)"))
    columns = {key: th.get_all(f"{key} (condensed, at grain boundary) (g/m3)") for key, _ in phases
               if f"{key} (condensed, at grain boundary) (g/m3)" in th.colmap}
    solved = sum(columns.values()) > 0.0  # time steps with an OpenCalphad result at the grain boundary
    for k, (key, label) in enumerate(phases):
        if key in columns and np.nanmax(columns[key]) > 0:
            values = np.where(solved, columns[key], np.nan)
            ax_ph.plot(bu_th[solved], values[solved], MARKERS[k], color=C[k], ms=1.6, label=label)
    ax_ph.set_yscale("log")
    ax_ph.set_ylim(bottom=1e-1)
    ax_ph.set(xlabel="Burnup (MWd/kgUO2)", ylabel="Phase amount (g/m$^3$ of fuel)",
              title="(d) Grain-boundary phases, r/R = 0 (converged steps)")
    ax_ph.legend(fontsize=7, markerscale=4)
    fig.suptitle("OpenCalphad with the FMP inventory and oxygen balance fuel/Mo oxides "
                 "(UO2, LWR, iStoichiometryDeviation = 7)", fontsize=10)
    fig.tight_layout()
    save(fig, "fmp_thermochemistry.png")

    print("Thermochemistry (end of irradiation)")
    for k, name, r_over_R, oc, ref in cases:
        print(f"  {name}: mu_O2 = {oc.get_all('Fuel oxygen potential (KJ/mol)')[-1]:.0f} kJ/mol, "
              f"x = {oc.get_all('Stoichiometry deviation (/)')[-1]:.2e}, f_ox(Mo) = {oc.get_all('Mo in oxide fraction (/)')[-1]:.2f}, "
              f"R_GB = {oc.get_all('Intergranular FMP radius (m)')[-1]*1e9:.2f} nm (no OC "
              f"{ref.get_all('Intergranular FMP radius (m)')[-1]*1e9:.2f} nm)")


def main():
    plot_calibration()
    plot_buck()
    plot_partition()
    plot_elements()
    plot_thermochemistry()


if __name__ == "__main__":
    main()
