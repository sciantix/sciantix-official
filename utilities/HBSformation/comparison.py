"""Comparison of the HBS-formation options with each other and with the experimental data.

This is NOT a validation: option 3 is a fit on part of these points and option 4 is
calibrated on most of them, so the tables below are a comparison of four models on a
common set of targets, not an out-of-sample test.  The leave-one-paper-out columns of
`calibrate.py --study` are the out-of-sample check.

Every point of the four curated JSON datasets is used, none is dropped:

  ZAC2022  Zacharie-Aubrun et al. (2022), standard UO2 and Cr-doped, EBSD.
  ONO2025  Onofri et al. (2025), EBSD, misorientation only (no restructuring).
  GER2018  Gerczak et al. (2018), HBS area fraction (via Barani 2020) and rim grain size.
  NOI2015  Noirot et al. (2015), Xe-depleted area fraction on Halden discs.

The points differ enormously in quality and in relevance to a PWR standard-UO2 rod, so
they do not enter the metrics with the same weight.  Each carries a weight

    w = rank factor x relevance / 3,   rank factor  A, B -> 1 ; U* -> 0.5 ; U -> 0.25

where the rank is the Rose quality rank of the value (M.A. Rose, ANL/CFCT-22/26, adapted
to post-irradiation microscopy in `data/*.json`, key `vocabulary.rose_quality_ranking`)
and the relevance 1-3 is this project's own judgement of how close the sample is to the
target application (PWR, standard undoped UO2, ~10 um grains).

Since assessment v2.1 a value digitized by the curator from a figure of the same paper has
verifiability M, like a value stated in a table or in the text, so it reaches rank B.
Verifiability I -- and so rank U -- is kept for values quoted SECOND-HAND, which is why the
five Gerczak fraction points stay at w = 0.25: they come from Barani et al. (2020) Table 2,
not from Gerczak's own text.

This is the same weight `calibrate.py` uses in its data set C, via
`hbs_dataset.study_weight`.

Both the figure and the metrics honour it: RMSE and R2 are weighted, and the marker area
in the figure is proportional to w, so a point that is barely trusted is visibly small.
The unweighted numbers are printed beside the weighted ones so the effect is readable.

@author  E. Cappellari
@date    2026-09-16

Usage
-----
    python3 comparison.py                  # metrics + figures/comparison.png

Writes `figures/comparison.png`.

The closed forms of options 1, 2 and 3 are transcribed here from
`src/models/HighBurnupStructureFormation.C` so that the four options can be put on
one axis without running SCIANTIX; the constants are quoted with their case number.
`compare_formation_options.py` is the complementary check that runs the real code.
"""

from __future__ import annotations

import math
import os
import sys

from hbs_dataset import load_points
from hbs_formation_landau import (DATA_FILE, FABRICATION_POROSITY, GRAIN_RADIUS,
                                  hbs_state, measured_radius, theta_measured)

HERE = os.path.dirname(os.path.abspath(__file__))
FIGURES = os.path.join(HERE, "figures")

# Temperature at which the illustrative curves of the figure are drawn.  It is NOT used
# in any metric: every point is predicted at its own local temperature, porosity and
# fabrication grain size.  900 K is the rim temperature of the PWR rods of ZAC2022/GER2018.
CURVE_TEMPERATURE = 900.0                                                # K


# ---------------------------------------------------------------------------
# The formation options, as closed forms
# ---------------------------------------------------------------------------

def option1_kjma(burnup, temperature=None, porosity=None, grain_radius=None):
    """Case 1: KJMA, Barani et al. (2020), no incubation burnup.

    `burnup` is the EFFECTIVE burnup.
    """
    return 1.0 - math.exp(-2.77e-7 * math.pow(max(burnup, 0.0), 3.54))


def option2_kjma_incubation(burnup, temperature=None, porosity=None, grain_radius=None):
    """Case 2: the same KJMA shifted by bu_inc = 15 MWd/kgU, Biswas & Aagesen (2025).

    `burnup` is the EFFECTIVE burnup.
    """
    excess = burnup - 15.0
    if excess <= 0.0:
        return 0.0
    return 1.0 - math.exp(-2.77e-7 * math.pow(excess, 3.54))


def option3_dislocation(burnup, temperature, porosity=None, grain_radius=None):
    """Case 3: KJMA driven by the dislocation density of Veshchunov & Shestak (2009).

    rho_d(bu,T) = A bu^n [A_inf + (1 - A_inf)/(1 + exp((T - Tc)/dT))], then
    alpha = 1 - exp(-K (max(rho_d - rho_crit, 0)/rho_scale)^gamma).

    `burnup` is the LOCAL burnup.
    """
    if burnup <= 0.0:
        return 0.0
    temperature_factor = 0.608 + (1.0 - 0.608) / (1.0 + math.exp((temperature - 1109.0) / 25.8))
    rho_d = 6.545e12 * math.pow(burnup, 1.151) * temperature_factor
    xi = max((rho_d - 6.0e14) / 1.0e15, 0.0)
    return min(1.0 - 1.0e-9, 1.0 - math.exp(-2.597 * math.pow(xi, 1.104)))


def option4_landau(burnup, temperature, porosity=FABRICATION_POROSITY,
                   grain_radius=GRAIN_RADIUS):
    """Case 4: the Landau functional of this directory.

    `burnup` is the LOCAL burnup.
    """
    return hbs_state(burnup, temperature, porosity=porosity,
                     grain_radius_m=grain_radius).restructured_fraction


# (number, label, callable, colour, linestyle, driven by the EFFECTIVE burnup).
OPTIONS = (
    (1, "KJMA, Barani (2020)", option1_kjma, "tab:blue", ":", True),
    (2, "KJMA + bu_inc = 15", option2_kjma_incubation, "tab:orange", "-", True),
    (3, "rho_d, Veshchunov (2009)", option3_dislocation, "tab:green", "-.", False),
    (4, "Landau functional", option4_landau, "k", "-", False),
)

# How each option relates to the fraction data it is scored on here.
FRACTION_STATUS = {
    1: "constants from literature",
    2: "constants from literature",
    3: "fitted on GER/NOI fractions",
    4: "calibrated on all of them",
}

# The four papers, as they are drawn.  `group` is what hbs_dataset assigns:
# ZAC = Zacharie standard UO2, Cr = Zacharie Cr-doped, ONO/GER/NOI = the other papers.
# (group, marker, label for the console, label for the figure).
GROUPS = (
    ("ZAC", "s", "Zacharie (2022), std UO2", r"Zacharie (2022), std UO$_2$"),
    ("Cr",  "D", "Zacharie (2022), Cr-doped", "Zacharie (2022), Cr-doped"),
    ("ONO", "^", "Onofri (2025)", "Onofri (2025)"),
    ("GER", "o", "Gerczak (2018)", "Gerczak (2018)"),
    ("NOI", "P", "Noirot (2015)", "Noirot (2015)"),
)

OBSERVABLES = (
    ("theta", "mean misorientation", "deg", 1.0),
    ("fraction", "restructured fraction", "-", 1.0),
    ("radius", "subgrain radius", "um", 1e6),
)


# ---------------------------------------------------------------------------
# weighted metrics
# ---------------------------------------------------------------------------
#
# With w_i = 1 these reduce exactly to the usual RMSE and R2, so the weighted and the
# unweighted column of every table below are produced by the same two functions.

def rmse(observed, predicted, weights=None):
    """sqrt( sum w (o - p)^2 / sum w )."""
    if weights is None:
        weights = [1.0] * len(observed)
    total = sum(weights)
    return math.sqrt(sum(w * (o - p) ** 2 for o, p, w in zip(observed, predicted, weights)) / total)


def r_squared(observed, predicted, weights=None):
    """1 - sum w (o - p)^2 / sum w (o - mean_w)^2, with mean_w the weighted mean.

    The reference against which the model is scored is the weighted mean, so that R2 is
    asked the same question as the weighted RMSE: how much of the variance THAT MATTERS
    is explained.
    """
    if weights is None:
        weights = [1.0] * len(observed)
    total = sum(weights)
    mean = sum(w * o for o, w in zip(observed, weights)) / total
    residual = sum(w * (o - p) ** 2 for o, p, w in zip(observed, predicted, weights))
    spread = sum(w * (o - mean) ** 2 for o, w in zip(observed, weights))
    return 1.0 - residual / spread if spread > 0.0 else float("nan")


# ---------------------------------------------------------------------------
# the data
# ---------------------------------------------------------------------------

def targets(path=DATA_FILE):
    """Every (point, observable) target of the four papers, with its weight.

    No point is dropped: the Cr-doped samples, the Halden discs and the values digitized
    from figures are all in, carrying the low weight their rank and relevance give them.
    """
    folder = path if os.path.isdir(path) else None
    points, notes = load_points(folder, fabrication_porosity=FABRICATION_POROSITY,
                                grain_radius=GRAIN_RADIUS, theta_measured=theta_measured,
                                measured_radius=measured_radius)
    points = [p for p in points if p["bu"] > 0.0 and not math.isnan(p["w_study"])]
    for point in points:
        # No effective-burnup history exists in the JSON datasets, so options 1 and 2 are
        # fed the local burnup.  Below the 1273.15 K cut-off of EffectiveBurnup.C the two
        # coincide; above it this over-feeds them, which flatters options 1 and 2.
        point["bu_effective"] = point["bu"]
    return points, notes


def predict(option, point):
    """What `option` predicts for the fraction at this point."""
    _, _, model, _, _, effective = option
    burnup = point["bu_effective"] if effective else point["bu"]
    return model(burnup, point["T"], point["porosity"], point["grain_radius"])


def landau_value(observable, point):
    """What the Landau functional predicts for theta / fraction / radius at this point."""
    state = hbs_state(point["bu"], point["T"], porosity=point["porosity"],
                      grain_radius_m=point["grain_radius"])
    return {"theta": state.theta_deg,
            "fraction": state.restructured_fraction,
            "radius": state.subgrain_radius_m}[observable]


def of(points, observable):
    return [p for p in points if p["obs"] == observable]


# ---------------------------------------------------------------------------
# report
# ---------------------------------------------------------------------------

def scored(rows, predicted):
    """(weighted RMSE, weighted R2, plain RMSE, plain R2) of one observable."""
    observed = [r["y"] for r in rows]
    weights = [r["w_study"] for r in rows]
    return (rmse(observed, predicted, weights), r_squared(observed, predicted, weights),
            rmse(observed, predicted), r_squared(observed, predicted))


def composition(rows):
    """One line per paper: how many points it brings and how much weight it carries."""
    total = sum(r["w_study"] for r in rows)
    lines = []
    for name, _, label, _ in GROUPS:
        group = [r for r in rows if r["group"] == name]
        if not group:
            continue
        share = sum(r["w_study"] for r in group)
        ranks = sorted({r["rank"] for r in group})
        lines.append("    %-28s N = %2d   weight %5.1f%% of the total   rank %s"
                     % (label, len(group), 100.0 * share / total, ",".join(ranks)))
    return lines


def report(points):
    print("=" * 84)
    print("HBS formation -- four options against all %d targets of the four JSON datasets"
          % len(points))
    print("=" * 84)
    print()
    print("  Weights: w = rank factor x relevance / 3, rank A,B -> 1  U* -> 0.5  U -> 0.25.")
    print("  'weighted' uses them; 'plain' gives every point w = 1, for comparison.")
    print()

    # ---- the restructured fraction: the only observable the four options share --------
    fraction = of(points, "fraction")
    print("-" * 84)
    print("restructured fraction X, %d targets" % len(fraction))
    print("-" * 84)
    for line in composition(fraction):
        print(line)
    print()
    print("    option                        weighted RMSE    R2  |  plain RMSE    R2  |  relation")
    print("    " + "-" * 78)
    for option in OPTIONS:
        number, label = option[0], option[1]
        predicted = [predict(option, p) for p in fraction]
        w_rmse, w_r2, p_rmse, p_r2 = scored(fraction, predicted)
        print("    %d  %-27s %.4f  %+.4f  |  %.4f  %+.4f  |  %s"
              % (number, label, w_rmse, w_r2, p_rmse, p_r2, FRACTION_STATUS[number]))
    print()

    # What the weighting actually changed: the ranking of the options.
    order_w = sorted(OPTIONS, key=lambda o: rmse([p["y"] for p in fraction],
                                                 [predict(o, p) for p in fraction],
                                                 [p["w_study"] for p in fraction]))
    order_p = sorted(OPTIONS, key=lambda o: rmse([p["y"] for p in fraction],
                                                 [predict(o, p) for p in fraction]))
    print("    ranking weighted: %s" % " < ".join(str(o[0]) for o in order_w))
    print("    ranking plain   : %s%s"
          % (" < ".join(str(o[0]) for o in order_p),
             "   (unchanged)" if order_w == order_p else "   <- the weighting reorders them"))
    print()

    # Per paper, for option 4: where the error sits once the weights are applied.
    print("    option 4, per paper:")
    print("      paper                          N    w share   weighted RMSE   plain RMSE")
    print("      " + "-" * 72)
    total_weight = sum(p["w_study"] for p in fraction)
    for name, _, label, _ in GROUPS:
        group = [p for p in fraction if p["group"] == name]
        if not group:
            continue
        predicted = [predict(OPTIONS[3], p) for p in group]
        share = 100.0 * sum(p["w_study"] for p in group) / total_weight
        print("      %-28s %3d   %5.1f%%      %.4f          %.4f"
              % (label, len(group), share,
                 rmse([p["y"] for p in group], predicted, [p["w_study"] for p in group]),
                 rmse([p["y"] for p in group], predicted)))
    print()

    # ---- theta and the radius: option 4 only ----------------------------------------
    for observable, name, unit, scale in OBSERVABLES:
        if observable == "fraction":
            continue
        rows = of(points, observable)
        print("-" * 84)
        print("%s, %d targets -- option 4 only (options 1-3 do not produce it)"
              % (name, len(rows)))
        print("-" * 84)
        for line in composition(rows):
            print(line)
        predicted = [landau_value(observable, r) for r in rows]
        w_rmse, w_r2, p_rmse, p_r2 = scored(rows, predicted)
        print()
        print("    4  %-27s weighted RMSE %.4f %s  R2 %+.4f"
              % ("Landau functional", w_rmse * scale, unit, w_r2))
        print("       %-27s plain    RMSE %.4f %s  R2 %+.4f"
              % ("", p_rmse * scale, unit, p_r2))
        print()


# ---------------------------------------------------------------------------
# figure
# ---------------------------------------------------------------------------

def marker_area(weight):
    """Marker area proportional to the weight, floored so the lightest point is visible."""
    return 16.0 + 130.0 * weight


def plot(points, notes):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib.lines import Line2D

    plt.style.use("seaborn-v0_8-whitegrid")
    plt.rcParams.update({
        "font.family": "serif",
        "font.serif": ["Times New Roman", "Times", "Nimbus Roman", "DejaVu Serif"],
        "mathtext.fontset": "dejavuserif",
        "font.size": 20,
        "axes.labelsize": 20,
        "axes.titlesize": 20,
        "xtick.labelsize": 20,
        "ytick.labelsize": 20,
        "legend.fontsize": 16,
        "figure.dpi": 300,
        "axes.grid": True,
        "grid.alpha": 0.5,
        "grid.linestyle": "--",
        "lines.linewidth": 3,
        "legend.frameon": True,
    })

    os.makedirs(FIGURES, exist_ok=True)
    burnups = [1.0 + 0.25 * i for i in range(660)]      # 1 -> 165 MWd/kgU

    figure, axes = plt.subplots(1, 3, figsize=(18, 6.6), layout="constrained")

    # one temperature scale for the three panels, over every point drawn
    temperatures = [p["T"] for p in points]
    low, high = min(temperatures), max(temperatures)

    def draw(axis, rows, scale=1.0):
        """The points of one panel, shaped by paper and sized by weight."""
        handle = None
        for name, marker, _, _ in GROUPS:
            group = [r for r in rows if r["group"] == name]
            if not group:
                continue
            handle = axis.scatter([r["bu"] for r in group], [r["y"] * scale for r in group],
                                  c=[r["T"] for r in group], cmap="coolwarm",
                                  vmin=low, vmax=high, marker=marker,
                                  s=[marker_area(r["w_study"]) for r in group],
                                  edgecolors="0.25", linewidths=0.5, zorder=4)
        return handle

    # ---- panel 1: restructured fraction, the four options ------------------
    for number, label, model, colour, style, _ in OPTIONS:
        width = 2.4 if number == 4 else 1.5
        axes[0].plot(burnups, [model(b, CURVE_TEMPERATURE) for b in burnups],
                     style, color=colour, lw=width, label="%d  %s" % (number, label))
    scatter = draw(axes[0], of(points, "fraction"))
    axes[0].set_ylabel(r"restructured fraction  $X$  (-)")

    # ---- panel 2: mean misorientation --------------------------------------
    axes[1].plot(burnups, [hbs_state(b, CURVE_TEMPERATURE).theta_deg for b in burnups],
                 "k-", lw=2.4)
    draw(axes[1], of(points, "theta"))
    axes[1].axhline(10.0, ls=":", lw=0.8, color="0.5")
    axes[1].set_ylabel(r"mean misorientation  $\Theta$  (deg)")

    # ---- panel 3: subgrain radius ------------------------------------------
    radii = [hbs_state(b, CURVE_TEMPERATURE).subgrain_radius_m for b in burnups]
    axes[2].plot([b for b, r in zip(burnups, radii) if not math.isnan(r)],
                 [r * 1e6 for r in radii if not math.isnan(r)], "k-", lw=2.4)
    draw(axes[2], of(points, "radius"), scale=1e6)
    axes[2].set_ylabel(r"subgrain radius  $r_n$  ($\mu$m)")
    axes[2].set_ylim(0.0, 1.2)

    for axis in axes:
        axis.set_xlabel("Burnup (MWd/kgU)")
        axis.set_xlim(0, 165)

    # ---- legends -----------------------------------------------------------
    # identity is never colour alone: the colour carries the local temperature, the
    # SHAPE carries the paper and the AREA carries the calibration weight.
    lines, labels = axes[0].get_legend_handles_labels()
    papers = [Line2D([], [], ls="none", marker=marker, ms=9, color="0.45",
                     markeredgecolor="0.25", label=label)
              for _, marker, _, label in GROUPS]
    sizes = [Line2D([], [], ls="none", marker="s", color="0.45", markeredgecolor="0.25",
                    ms=math.sqrt(marker_area(w)), label=text)
             for w, text in ((1.0, "$w = 1$  (rank B, PWR std UO$_2$)"),
                             (2.0 / 3.0, "$w = 0.67$  (Cr-doped, Halden std disc)"),
                             (1.0 / 3.0, "$w = 0.33$  (Halden large-grain disc)"),
                             (0.25, "$w = 0.25$  (rank U: second-hand or flagged)"))]

    first = figure.legend(lines + papers, labels + [h.get_label() for h in papers],
                          loc="outside lower left", ncol=3, frameon=False,
                          handlelength=2.0, columnspacing=1.2, handletextpad=0.5)
    figure.add_artist(first)
    figure.legend(sizes, [h.get_label() for h in sizes], loc="outside lower right",
                  ncol=1, frameon=False, handletextpad=0.6, labelspacing=0.7,
                  title="marker area $\\propto$ weight", fontsize=13, title_fontsize=13)

    figure.suptitle("HBS formation: four options against every point of the four datasets\n"
                    "colour = local temperature, shape = paper, area = calibration weight; "
                    "curves drawn at %d K" % CURVE_TEMPERATURE, fontsize=17)
    bar = figure.colorbar(scatter, ax=axes, fraction=0.022, pad=0.015)
    bar.set_label("local temperature  (K)", fontsize=16)
    bar.ax.tick_params(labelsize=14)

    path = os.path.join(FIGURES, "comparison.png")
    figure.savefig(path, dpi=140)
    print("written: %s" % path)
    for note in notes:
        print("  note: %s" % note)


def main(argv=None):
    points, notes = targets()
    report(points)
    try:
        plot(points, notes)
    except ImportError:
        print("(matplotlib not available, figure skipped)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
