"""
sciantix testing suite
author: Vittoria Bellani, Elisa Cappellari

Five metal particles (FMP: Mo, Tc, Ru, Rh, Pd) in UO2 irradiated in LWR conditions.

    python3 validation/metallicinclusions/plot.py

Run the groups first (python3 -m testing.runner --metallicinclusions [--metallicinclusions-oc --oc]);
when a case has no output.txt its output_gold.txt is used. Figures are written in figures/:

- metallicinclusions_UO2_LWR.png : validation against the TEM data at four radial positions, 54.1 MWd/kgUO2
- buck2015_ATM109.png            : validation against Buck et al. (2015), ATM-109 BWR fuel, 70.8 GWd/tHM
- fmp_partition.png              : single inventory, produced metal split into dissolved metal (intra1, intra2)
                                   and particles (intra1, intra2, gb), vs burnup
- fmp_elements.png               : the five elements, produced inventory and end-of-life partition
- fmp_thermochemistry.png        : OpenCalphad coupling and oxygen balance (oxygen potential, O/M, Mo
                                   oxidation, oxygen taken by the Mo oxides, grain-boundary phases and radius)

Observables: mean radius R from the SCIANTIX output (R = (3 n Omega / 4 pi)^(1/3), n atoms per particle,
Omega = 1.44123e-29 m3), projected area fraction through a TEM foil of thickness t, A = 1 - exp(-pi t sum n R^2).
TEM intragranular images: intra1 particles, n = N_1 / phi_1. TEM grain-boundary images (strips of 100 nm on each
side of the boundary): intra2 and gb particles together on the strip volume phi_2, number-weighted mean radius.
J = mean |ln(model/exp)| over R and A of the two images at the four positions (16 observables). Nothing is fitted
on these data: J is a validation metric.

EC November 2026: translated to English, FMP naming, radius read from the new output columns, new figures
(Buck et al. 2015 validation, inventory partition, elements, thermochemistry), three-zone model and oxygen
balance, TEM data used for validation only, one colour per zone and clearer labels.
"""

import os
import sys
import math
import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.lines
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
GRAY = "#777777"  # measurements (Buck) and line-style keys
MARKERS = ["o", "s", "^", "D", "v"]
LINESTYLES = ["-", "--", "-.", ":", (0, (5, 1, 1, 1))]
# One colour per zone in every figure; the TEM grain-boundary strip (intra2 + gb pooled) has its own colour and
# the dissolved metal a light tint of its zone
ZONE = {"intra1": C[0], "intra2": C[2], "gb": C[3], "strip": C[4]}
TINT = {"intra1": "#b7d3f6", "intra2": "#a4dfca"}
NAME = {"intra1": "grain interior", "intra2": "near-boundary zone", "gb": "grain boundary",
        "strip": "near-boundary zone + grain boundary"}  # zone names in the figures
BU_LABEL = "Burnup (MWd/kgUO$_2$)"
RR_LABEL = "Radial position r/R (-)"

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
    "axes.spines.top": False, "axes.spines.right": False, "axes.axisbelow": True,
})
log_ticks = matplotlib.ticker.FuncFormatter(lambda v, _: f"{v:g}")


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


POPULATIONS = ("Intragranular", "Near-boundary", "Intergranular")  # intra1, intra2, gb


def strip_radius(out):
    """Number-weighted mean radius of intra2 + gb, the particles of the TEM grain-boundary strips."""
    n2, ngb = (np.maximum(out.get_all(f"{p} FMP concentration (FMP/m3)"), 0.0) for p in POPULATIONS[1:])
    r2, rgb = (out.get_all(f"{p} FMP radius (m)") for p in POPULATIONS[1:])
    return np.divide(n2 * r2 + ngb * rgb, n2 + ngb, out=np.zeros_like(r2), where=n2 + ngb > 0)


def model_at_burnup(out, burnup, tem_thickness):
    i = int(np.nanargmin(np.abs(out.get_all("Burnup (MWd/kgUO2)") - burnup)))
    at = lambda var: out.get_all(var)[i]
    n1, n2, ngb = (max(at(f"{p} FMP concentration (FMP/m3)"), 0.0) for p in POPULATIONS)
    r1, r2, rgb = (at(f"{p} FMP radius (m)") for p in POPULATIONS)
    phi_2 = at("FMP near-boundary zone volume fraction (/)")
    total = produced_total(out)[i]
    return {
        "radius_intra": r1,
        "radius_near": r2,
        "radius_gb": rgb,
        "radius_strip": strip_radius(out)[i],
        "area_intra": -math.expm1(-n1 / (1.0 - phi_2) * math.pi * r1**2 * tem_thickness),
        "area_strip": -math.expm1(-math.pi * tem_thickness * (n2 * r2**2 + ngb * rgb**2) / phi_2),
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
# 1. Validation against the TEM data (V. Bellani, thesis, images of Angleraud 2024)
# --------------------------------------------------------------------------------------------------------------
def plot_validation():
    rows = []
    for name, r_over_R, thickness in POSITIONS:
        out = load_case(os.path.join(GROUP_DIR, f"test_FMP_UO2_{name}"))
        rows.append({
            "name": name, "r_over_R": r_over_R, "model": model_at_burnup(out, BURNUP_EXP, thickness),
            "intra": read_exp(os.path.join(DATA_DIR, "intra", f"exp_intra_{name}.txt")),
            "inter": read_exp(os.path.join(DATA_DIR, "inter", f"exp_inter_{name}.txt")),
        })

    errors = []
    print("Validation, TEM data at 54.1 MWd/kgUO2 (intra = intra1, strip = intra2 + gb within 100 nm of the boundary)")
    print(f"{'case':<6}{'r/R':>6}  {'R_intra exp/mod (nm)':>22}  {'R_strip exp/mod (nm)':>22}  "
          f"{'A_intra exp/mod (%)':>21}  {'A_strip exp/mod (%)':>21}  {'R_intra2, R_gb (nm)':>20}  {'dissolved':>9}")
    for row in rows:
        m, ei, eg = row["model"], row["intra"], row["inter"]
        errors += [log_error(m["radius_intra"], ei["radius"]), log_error(m["area_intra"], ei["area_fraction"]),
                   log_error(m["radius_strip"], eg["radius"]), log_error(m["area_strip"], eg["area_fraction"])]
        print(f"{row['name']:<6}{row['r_over_R']:>6.2f}  "
              f"{ei['radius']*1e9:>10.2f} /{m['radius_intra']*1e9:>9.2f}  "
              f"{eg['radius']*1e9:>10.2f} /{m['radius_strip']*1e9:>9.2f}  "
              f"{ei['area_fraction']*100:>10.3f} /{m['area_intra']*100:>8.3f}  "
              f"{eg['area_fraction']*100:>10.3f} /{m['area_strip']*100:>8.3f}  "
              f"{m['radius_near']*1e9:>9.2f}, {m['radius_gb']*1e9:>6.2f}  {m['matrix_fraction']*100:>8.1f}%")
    j = np.mean(errors)
    print(f"J = mean |ln(model/exp)| over {len(errors)} observables = {j:.3f} (validation metric, nothing fitted)\n")

    # TEM: open markers (error bars = standard deviation of the measured radii); SCIANTIX: lines
    x = np.array([row["r_over_R"] for row in rows])
    tem = lambda kind, key, scale: np.array([r[kind][key] for r in rows]) * scale
    mod = lambda key, scale: np.array([r["model"][key] for r in rows]) * scale
    tem_style = dict(ls="none", capsize=3, ms=7, mfc="white", mew=1.5, zorder=3)
    fig, (ax_r, ax_a) = plt.subplots(1, 2, figsize=(10, 4.4))
    ax_r.errorbar(x - 0.008, tem("intra", "radius", 1e9), yerr=tem("intra", "std_radius", 1e9), marker="o",
                  color=ZONE["intra1"], label="TEM, grain interior", **tem_style)
    ax_r.errorbar(x + 0.008, tem("inter", "radius", 1e9), yerr=tem("inter", "std_radius", 1e9), marker="s",
                  color=ZONE["strip"], label="TEM, within 100 nm of the grain boundary", **tem_style)
    ax_a.errorbar(x - 0.008, tem("intra", "area_fraction", 100), marker="o", color=ZONE["intra1"], **tem_style)
    ax_a.errorbar(x + 0.008, tem("inter", "area_fraction", 100), marker="s", color=ZONE["strip"], **tem_style)
    for ax, key, scale in ((ax_r, "radius", 1e9), (ax_a, "area", 100)):
        ax.plot(x, mod(f"{key}_intra", scale), "o-", ms=4, color=ZONE["intra1"], label=f"SCIANTIX, {NAME['intra1']}")
        ax.plot(x, mod(f"{key}_strip", scale), "s--", ms=4, color=ZONE["strip"], label=f"SCIANTIX, {NAME['strip']}")
    ax_r.plot(x, mod("radius_near", 1e9), "^:", ms=4, lw=1.2, color=ZONE["intra2"],
              label=f"SCIANTIX, {NAME['intra2']} alone")
    ax_r.plot(x, mod("radius_gb", 1e9), "v:", ms=4, lw=1.2, color=ZONE["gb"], label=f"SCIANTIX, {NAME['gb']} alone")
    ax_r.text(0.98, 0.97, "error bars: ±1 standard deviation of the measured radii", transform=ax_r.transAxes,
              ha="right", va="top", fontsize=7, color=GRAY)
    ax_r.set(ylabel="Mean particle radius (nm)", title="(a) Mean particle radius", ylim=(0, None))
    ax_a.set(yscale="log", ylim=(0.1, 100), ylabel="Projected area fraction (%)",
             title="(b) Projected area fraction in the TEM foil")
    ax_a.yaxis.set_major_formatter(log_ticks)
    for ax in (ax_r, ax_a):
        ax.set(xlabel=RR_LABEL, xticks=x, xlim=(-0.05, 0.8))
    handles, labels = ax_r.get_legend_handles_labels()
    fig.legend(handles[-2:] + handles[:-2], labels[-2:] + labels[:-2], loc="lower center", ncol=3)
    fig.suptitle(f"Validation on the TEM data at {BURNUP_EXP:.1f} MWd/kgUO$_2$, nothing fitted: "
                 f"J = mean |ln(model / TEM)| = {j:.2f} over {len(errors)} observables", fontsize=10)
    fig.tight_layout(rect=(0, 0.11, 1, 1))
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

    # Pellet-averaged population: equal-volume rings, intra1, intra2 and gb particles
    populations = []  # (ln V, number density, label)
    for name, r_over_R, out in rings:
        for kind in POPULATIONS:
            radius = out.get_all(f"{kind} FMP radius (m)")[-1]
            number = max(out.get_all(f"{kind} FMP concentration (FMP/m3)")[-1], 0.0)
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
              f"d_intra1={2e9*out.get_all('Intragranular FMP radius (m)')[-1]:.1f} nm  "
              f"d_intra2={2e9*out.get_all('Near-boundary FMP radius (m)')[-1]:.1f} nm  "
              f"d_gb={2e9*out.get_all('Intergranular FMP radius (m)')[-1]:.1f} nm  "
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

    # Measurements in gray, SCIANTIX pellet average in black, SCIANTIX zones in their colours
    # (a) cumulative distribution of ln V, as Fig. 6 of the paper
    ax = axes[0]
    xs = np.linspace(-2, 18, 300)
    from math import erf
    for k, (s, label, key) in enumerate((("carbonate", "carbonate-peroxide", "carbonate_peroxide"),
                                         ("nitric", "nitric acid", "nitric"))):
        data = lnv_exp[s]
        ax.plot(data[:, 0], data[:, 1], MARKERS[k + 1], color=GRAY, ms=4.5, mfc="white", mew=1.2,
                label=f"Buck et al. (2015), {label}")
        mu, sig = exp[f"lnV_mu_{key}"], exp[f"lnV_sigma_{key}"]
        ax.plot(xs, [0.5 * (1 + erf((x - mu) / (sig * math.sqrt(2)))) for x in xs], (":", "--")[k], color=GRAY,
                lw=1.2, label=f"log-normal fit, {label}")
    ax.step(np.r_[-2, lnv, 18], np.r_[0, np.cumsum(weight), 1], where="post", color=INK, lw=2,
            label="SCIANTIX, pellet average (all zones)")
    ax.set(xlim=(-2, 18), ylim=(0, 1.02), xlabel="ln(V / nm$^3$) (-)", ylabel="Cumulative number fraction (-)",
           title="(a) Particle volume distribution (cf. Buck, Fig. 6)")
    top = ax.secondary_xaxis("top", functions=(lambda v: v, lambda v: v))
    ticks_d = [2, 5, 10, 20, 50, 100, 300]
    top.set_xticks([math.log(math.pi / 6 * d**3) for d in ticks_d], [str(d) for d in ticks_d])
    top.set_xlabel("Equivalent diameter (nm)", fontsize=8)
    ax.legend(loc="lower right", fontsize=7)

    # (b) diameter per ring vs the reported sizes
    ax = axes[1]
    ax.axhspan(exp["particle_diameter_min"], exp["particle_diameter_max"], color="#eeeeee", zorder=0,
               label="Buck, observed range (10-300 nm)")
    ax.axhspan(exp["mean_particle_size_carbonate_peroxide"], exp["mean_particle_size_nitric"], color="#c9c9c9",
               zorder=0, label="Buck, reported mean size (43-58 nm)")
    x = [r for _, r, _ in rings]
    for k, (kind, zone) in enumerate(zip(POPULATIONS, ("intra1", "intra2", "gb"))):
        ax.plot(x, [2e9 * out.get_all(f"{kind} FMP radius (m)")[-1] for *_, out in rings], MARKERS[k] + "-",
                color=ZONE[zone], ms=7, label=f"SCIANTIX, {NAME[zone]}")
    ax.axhline(d_number_mean, color=INK, lw=1.4, ls="--",
               label=f"SCIANTIX, pellet number mean ({d_number_mean:.1f} nm)")
    ax.set(yscale="log", xlim=(0, 1), ylim=(3, 400), xlabel=RR_LABEL, ylabel="Particle diameter (nm)",
           title="(b) Diameter at the end of irradiation (70.8 GWd/tHM)")
    ax.yaxis.set_major_formatter(log_ticks)
    ax.legend(loc="upper right", fontsize=7, frameon=True, facecolor="white", edgecolor="none")

    # (c) evolution in time, grain interior only: one blue (its colour) per ring, darker = hotter
    ax = axes[2]
    for k, (name, r_over_R, out) in enumerate(rings):
        t = out.get_all("Time (h)") / 8766.0 + 1979.083
        ax.plot(t, 2e9 * out.get_all("Intragranular FMP radius (m)"), ls=LINESTYLES[k],
                color=("#0d366b", "#1c5cab", "#3987e5", "#86b6ef")[k],
                label=f"r/R = {r_over_R:.2f} ({out.get_all('Temperature (K)')[0]:.0f} K)")
    fission = rings[0][2].get_all("Fission rate (fiss / m3 s)")
    t = rings[0][2].get_all("Time (h)") / 8766.0 + 1979.083
    ax.fill_between(t, 0, 1, where=fission <= 0.0, color="#e6e6e6", transform=ax.get_xaxis_transform(), zorder=0)
    ax.text(np.median(t[fission <= 0.0]), 0.5, "out of the reactor", rotation=90, ha="center", va="center",
            fontsize=7, color=GRAY, transform=ax.get_xaxis_transform())
    ax.set(xlabel="Year", ylabel="Particle diameter (nm)", title="(c) Grain-interior particles during irradiation")
    ax.set_ylim(bottom=0)
    ax.legend(loc="lower right", fontsize=7)

    fig.suptitle("Independent validation: ATM-109 fuel, Quad Cities I BWR (Buck et al. 2015); "
                 "power and temperature history assumed (see README)", fontsize=10)
    fig.tight_layout()
    save(fig, "buck2015_ATM109.png")


# --------------------------------------------------------------------------------------------------------------
# 3. Single inventory: dissolved metal and particles of the three zones
# --------------------------------------------------------------------------------------------------------------
def stacked_partition(ax, out, title):
    bu = out.get_all("Burnup (MWd/kgUO2)")
    total = produced_total(out)
    ok = total > 0
    dissolved_2 = out.get_all("FMP in near-boundary zone matrix (at/m3)")
    parts = [out.get_all("FMP in matrix (at/m3)") - dissolved_2, out.get_all("FMP in intragranular particles (at/m3)"),
             dissolved_2, out.get_all("FMP in near-boundary particles (at/m3)"),
             out.get_all("FMP at grain boundary (at/m3)")]
    fractions = [np.clip(p[ok] / total[ok], 0, 1) * 100 for p in parts]
    ax.stackplot(bu[ok], *fractions, edgecolor="white", linewidth=0.6,
                 colors=[TINT["intra1"], ZONE["intra1"], TINT["intra2"], ZONE["intra2"], ZONE["gb"]],
                 labels=[f"{NAME['intra1']}, dissolved", f"{NAME['intra1']}, particles",
                         f"{NAME['intra2']}, dissolved", f"{NAME['intra2']}, particles", NAME["gb"]])
    ax.set(xlim=(0, bu[-1]), ylim=(0, 100), title=title)
    ax.grid(False)
    ax.text(0.96, 0.05, f"at the end:\ngrain-interior particles {fractions[1][-1]:.0f}%\n"
            f"near-boundary particles {fractions[3][-1]:.0f}%\ngrain boundary {fractions[4][-1]:.1f}%",
            transform=ax.transAxes,
            ha="right", va="bottom", fontsize=7, color=INK,
            bbox=dict(boxstyle="round,pad=0.3", fc="white", ec="none", alpha=0.85))


def plot_partition():
    buck = [load_case(os.path.join(GROUP_DIR, f"test_FMP_UO2_Buck2015_{n}")) for n, _ in BUCK_RINGS]
    nrows = 2 if all(o is not None for o in buck) else 1
    fig, axes = plt.subplots(nrows, 4, figsize=(13, 3.2 * nrows), sharey=True, squeeze=False)
    for k, (name, r_over_R, _) in enumerate(POSITIONS):
        out = load_case(os.path.join(GROUP_DIR, f"test_FMP_UO2_{name}"))
        stacked_partition(axes[0, k], out, f"TEM case, r/R = {r_over_R:.2f} ({out.get_all('Temperature (K)')[0]:.0f} K)")
    if nrows == 2:
        for k, ((name, r_over_R), out) in enumerate(zip(BUCK_RINGS, buck)):
            stacked_partition(axes[1, k], out, f"ATM-109, r/R = {r_over_R:.2f} ({out.get_all('Temperature (K)')[0]:.0f} K)")
    for ax in axes[:, 0]:
        ax.set_ylabel("Share of the produced FMP (%)")
    for ax in axes[-1]:
        ax.set_xlabel(BU_LABEL)
    handles, labels = axes[0, 0].get_legend_handles_labels()
    fig.legend(handles, labels, loc="lower center", ncol=5, bbox_to_anchor=(0.5, -0.03))
    fig.suptitle("Where the produced FMP (Mo + Tc + Ru + Rh + Pd) is: dissolved in the matrix or in particles, "
                 "per zone (the five parts sum to 100%)", fontsize=10)
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

    fig, (ax_p, ax_b) = plt.subplots(1, 2, figsize=(11.5, 3.9), gridspec_kw={"width_ratios": [1, 1.15]})
    for k, e in enumerate(ELEMENTS):
        ax_p.plot(bu, out.get_all(f"{e} produced (at/m3)") / 1e26, ls=LINESTYLES[k], color=C[k],
                  label=f"{e} ({out.get_all(f'{e} produced (at/m3)')[-1] / total[-1] * 100:.1f}%)")
    ax_p.set(xlabel=BU_LABEL, ylabel="Produced (10$^{26}$ at/m$^3$)",
             title="(a) Produced, LWR yields (share of the five)", xlim=(0, bu[-1]))
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
    bars = [("SCIANTIX, no OC", share),
            ("SCIANTIX with OC, metallic part\n" + (f"({f_ox*100:.0f}% of Mo oxidised)" if oc is not None else "(no OC output)"),
             metallic),
            ("Undissolved residue, Adachi et al.\n(quoted by Buck et al. 2015)", residue)]
    y = np.arange(len(bars))[::-1]
    left = np.zeros(len(bars))
    for k, e in enumerate(ELEMENTS):
        width = np.array([b[1][k] for b in bars]) * 100
        ax_b.barh(y, width, left=left, color=C[k], edgecolor="white", linewidth=2, height=0.6, label=e)
        for yi, li, wi in zip(y, left, width):
            if wi >= 6:  # white text only on the dark blue, dark text on the lighter fills
                ax_b.text(li + wi / 2, yi, f"{wi:.0f}", ha="center", va="center", fontsize=7.5,
                          color="white" if k == 0 else INK)
        left += width
    ax_b.set_yticks(y, [b[0] for b in bars])
    ax_b.set(xlim=(0, 100), xlabel="Composition (at.%)",
             title=f"(b) Particle composition at {bu[-1]:.1f} MWd/kgUO$_2$, r/R = 0")
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
        ax_rgb.plot(bu, strip_radius(oc) * 1e9, **style)
        ax_rgb.plot(ref.get_all("Burnup (MWd/kgUO2)"), strip_radius(ref) * 1e9, **thin)
        tem = read_exp(os.path.join(DATA_DIR, "inter", f"exp_inter_{name}.txt"))
        ax_rgb.errorbar(BURNUP_EXP + 0.8 * k, tem["radius"] * 1e9, yerr=tem["std_radius"] * 1e9, fmt=MARKERS[k], color=C[k],
                        capsize=3, mfc="white", mew=1.3)

    ax_mu.set(ylabel="Oxygen potential (kJ/mol O$_2$)", ylim=(-440, -230), title="(a) Fuel oxygen potential")
    ax_x.set(ylabel="x in UO$_{2+x}$ (-)", ylim=(1e-8, 3e-2), title="(b) Stoichiometry deviation")
    ax_f.set(ylabel="Fraction of Mo in oxides (-)", ylim=(-0.02, 1.02),
             title="(c) Mo in oxides, all particles (OpenCalphad)")
    ax_o.set(ylabel="Share of the excess O (%)", ylim=(-2, 102), title="(f) Excess oxygen bound in the Mo oxides")
    ax_rgb.set(ylabel="Mean particle radius (nm)", ylim=(0, None),
               title="(e) Near-boundary zone + grain boundary vs TEM")
    for ax in axes.flat:
        ax.set_xlabel(BU_LABEL)
    # Thick / thin line key (gray proxies); the colours are the radial positions, legend below the figure
    key = lambda thick, thin: [matplotlib.lines.Line2D([], [], color=GRAY, lw=1.8, label=thick),
                               matplotlib.lines.Line2D([], [], color=GRAY, lw=0.9, label=thin)]
    ax_mu.legend(handles=key("with O balance (Mo oxides take O)", "without (all excess O stays in the fuel)"),
                 loc="center right", fontsize=7)
    ax_x.legend(handles=key("with O balance", "without O balance"), loc="lower left", fontsize=7)
    ax_rgb.legend(handles=key("with OpenCalphad", "without OpenCalphad") + [matplotlib.lines.Line2D(
        [], [], color=GRAY, marker="o", mfc="white", mew=1.3, ls="none", label="TEM, ±1 std of the radii")],
        loc="upper left", fontsize=7)
    fig.legend(*ax_f.get_legend_handles_labels(), loc="lower center", ncol=4)

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
    ax_ph.set(ylabel="Phase amount (g/m$^3$ of fuel)", title="(d) Grain-boundary phases at r/R = 0 (converged steps)")
    ax_ph.legend(fontsize=7, markerscale=4, title="Phase", title_fontsize=7)
    fig.suptitle("OpenCalphad coupling: FMP inventory at the grain boundary, oxygen balance between fuel and Mo oxides "
                 "(UO$_2$, LWR, iStoichiometryDeviation = 7)", fontsize=10)
    fig.tight_layout(rect=(0, 0.04, 1, 1))
    save(fig, "fmp_thermochemistry.png")

    print("Thermochemistry (end of irradiation)")
    for k, name, r_over_R, oc, ref in cases:
        print(f"  {name}: mu_O2 = {oc.get_all('Fuel oxygen potential (KJ/mol)')[-1]:.0f} kJ/mol, "
              f"x = {oc.get_all('Stoichiometry deviation (/)')[-1]:.2e}, f_ox(Mo) = {oc.get_all('Mo in oxide fraction (/)')[-1]:.2f}, "
              f"R_intra1 = {oc.get_all('Intragranular FMP radius (m)')[-1]*1e9:.2f} nm (no OC "
              f"{ref.get_all('Intragranular FMP radius (m)')[-1]*1e9:.2f} nm), R_strip = {strip_radius(oc)[-1]*1e9:.2f} nm "
              f"(no OC {strip_radius(ref)[-1]*1e9:.2f} nm)")


def main():
    plot_validation()
    plot_buck()
    plot_partition()
    plot_elements()
    plot_thermochemistry()


if __name__ == "__main__":
    main()
