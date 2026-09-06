"""Validation of the HBS-formation options against experimental data.

Two independent datasets, and the distinction between them is the whole point:

  PIE, Gerczak (2018) / Noirot (2015) -- 8 rim positions, restructured fraction
      against local burnup at T ~ 900 K.  This is the data formation options 2 and
      3 were CALIBRATED on, so for them it is a fit, not a validation.  Option 4
      has never seen it: it is calibrated on the EBSD dataset below.  So on these
      8 points option 4 is out-of-sample and the others are in-sample, and any
      comparison has to be read that way.

  EBSD, Zacharie-Aubrun et al. (2022) / Onofri et al. (2025) -- 41 radial points
      with a mean misorientation, of which 27 also carry a restructured fraction
      and 14 a subgrain size.  This is what option 4 is calibrated on, and it is
      the only dataset that constrains the two quantities options 1-3 do not
      produce at all: the mean misorientation and the subgrain radius.

@author  E. Cappellari
@date    2026-09-05

Usage
-----
    python3 validation.py                  # metrics + figures/validation.png
    python3 validation.py --no-plot        # metrics only

Writes `figures/validation.png` and `figures/validation_parity.png`.

The closed forms of options 1, 2 and 3 are transcribed here from
`src/models/HighBurnupStructureFormation.C` so that the four options can be put on
one axis without running SCIANTIX; the constants are quoted with their case number.
`compare_formation_options.py` is the complementary check that runs the real code.
"""

from __future__ import annotations

import argparse
import math
import os
import sys

from hbs_formation_landau import (DATA_FILE, FABRICATION_POROSITY, GRAIN_RADIUS,
                                  REFERENCE_POROSITY, THETA_HAGB, _bisect, hbs_state,
                                  load_ebsd, measured_radius, theta_measured)

HERE = os.path.dirname(os.path.abspath(__file__))
FIGURES = os.path.join(HERE, "figures")

# ---------------------------------------------------------------------------
# PIE data, Gerczak (2018) / Noirot (2015)
#
# The 8 rim positions that formation options 2 and 3 were calibrated against.
# Copied verbatim from `context/kjma_fit_comparison.py` (G. Zullo), where they
# appear as `bu_exp` / `alpha_exp`.  Burnup in MWd/kgU; the rim temperature is
# not resolved per point and is taken as 900 K for all of them, as in that fit.
# ---------------------------------------------------------------------------
PIE_BURNUP = (64.32, 70.91, 72.27, 77.05, 83.86, 88.41, 90.68, 129.77)   # MWd/kgU
PIE_FRACTION = (0.2687, 0.5438, 0.5534, 0.5979, 0.6211, 0.6869, 0.7566, 1.0002)
PIE_TEMPERATURE = 900.0                                                  # K


# ---------------------------------------------------------------------------
# The other formation options, as closed forms
# ---------------------------------------------------------------------------

def option1_kjma(burnup, temperature=None, porosity=None, grain_radius=None):
    """Case 1: KJMA, Barani et al. (2020), no incubation burnup.

    `burnup` here is the EFFECTIVE burnup: SCIANTIX drives this option from
    sciantix_variable["Effective burnup"] (HighBurnupStructureFormation.C, the
    `option == 1 || option == 2` branch), not from the local burnup.
    """
    return 1.0 - math.exp(-2.77e-7 * math.pow(max(burnup, 0.0), 3.54))


def option2_kjma_incubation(burnup, temperature=None, porosity=None, grain_radius=None):
    """Case 2: the same KJMA shifted by bu_inc = 15 MWd/kgU, Biswas & Aagesen (2025).

    `burnup` is the EFFECTIVE burnup, as for option 1.
    """
    excess = burnup - 15.0
    if excess <= 0.0:
        return 0.0
    return 1.0 - math.exp(-2.77e-7 * math.pow(excess, 3.54))


def option3_dislocation(burnup, temperature, porosity=None, grain_radius=None):
    """Case 3: KJMA driven by the dislocation density of Veshchunov & Shestak (2009).

    rho_d(bu,T) = A bu^n [A_inf + (1 - A_inf)/(1 + exp((T - Tc)/dT))], then
    alpha = 1 - exp(-K (max(rho_d - rho_crit, 0)/rho_scale)^gamma).

    `burnup` is the LOCAL burnup: this option carries its own thermal suppression in
    the sigmoid, so SCIANTIX deliberately does not also feed it the effective burnup.
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

    `burnup` is the LOCAL burnup, for the same reason as option 3: the temperature
    dependence is already carried by G(T) in Eq. (2).  Unlike options 1-3 this one
    also reads the state of the fuel -- the porosity enters Eq. (2) and the grain
    radius is the ceiling of Eq. (9).  The other three accept and ignore them, so
    the four share one signature.
    """
    return hbs_state(burnup, temperature, porosity=porosity,
                     grain_radius_m=grain_radius).restructured_fraction


# (number, label, callable, colour, linestyle, driven by the EFFECTIVE burnup).
#
# The last field is not cosmetic.  SCIANTIX drives options 1 and 2 from
# sciantix_variable["Effective burnup"] and options 3 and 4 from ["Burnup"], so
# scoring all four at the same number would not be scoring what SCIANTIX computes.
# On this dataset the two differ on 10 of the 42 rows, by up to 39.2 GWd/tU at the
# hot centre positions of Std-73.
OPTIONS = (
    (1, "KJMA, Barani (2020)", option1_kjma, "tab:blue", ":", True),
    (2, "KJMA + bu_inc = 15", option2_kjma_incubation, "tab:orange", "-", True),
    (3, "rho_d, Veshchunov (2009)", option3_dislocation, "tab:green", "-.", False),
    (4, "Landau functional", option4_landau, "k", "-", False),
)


def burnup_for(row, uses_effective_burnup):
    """The burnup SCIANTIX would feed this option at this EBSD point."""
    return row["burnup_effective"] if uses_effective_burnup else row["burnup"]

# How each option relates to the 8 PIE points -- needed to read the table honestly.
#
#   1  constants from Barani et al. (2020/2022); not fitted here.
#   2  the same constants with bu_inc = 15 MWd/kgHM taken from Biswas & Aagesen
#      (2025), Eq. 45; not fitted here either, but selected against these points
#      (HighBurnupStructureFormation.C case 2, "parameter selection Zullo (2026)").
#   3  K_rho and gamma_rho ARE a direct fit to these 8 points
#      (case 3, "fit Zullo (2026)"; the fit lives in context/kjma_fit_comparison.py).
#   4  calibrated on the EBSD dataset only; has never seen these points.
PIE_STATUS = {
    1: "constants from literature",
    2: "literature constants, selected here",
    3: "FITTED on these 8 points",
    4: "never seen them (out-of-sample)",
}


# ---------------------------------------------------------------------------
# metrics
# ---------------------------------------------------------------------------

def rmse(observed, predicted):
    return math.sqrt(sum((o - p) ** 2 for o, p in zip(observed, predicted)) / len(observed))


def r_squared(observed, predicted):
    mean = sum(observed) / len(observed)
    residual = sum((o - p) ** 2 for o, p in zip(observed, predicted))
    total = sum((o - mean) ** 2 for o in observed)
    return 1.0 - residual / total


def ebsd_points(path=DATA_FILE):
    """The EBSD rows, with the three observables and the local conditions."""
    rows = [r for r in load_ebsd(path) if r["burnup"] > 0.0]
    for row in rows:
        row["theta_obs"] = theta_measured(row)
        row["radius_obs"] = measured_radius(row)
    return rows


def report(rows):
    """The two validation tables."""
    print("=" * 78)
    print("PIE data -- Gerczak (2018) / Noirot (2015), 8 rim positions, T = %.0f K"
          % PIE_TEMPERATURE)
    print("=" * 78)
    print()
    print("  option                          RMSE      R2        relation to this data")
    print("  " + "-" * 74)
    # PIE_BURNUP is a local burnup and no effective one exists for these 8 points.
    # At the assumed T = 900 K, below the 1273.15 K cut-off of EffectiveBurnup.C, the
    # two coincide, so options 1 and 2 are fed the right number here by construction.
    for number, label, model, _, _, _ in OPTIONS:
        predicted = [model(b, PIE_TEMPERATURE) for b in PIE_BURNUP]
        print("  %d  %-27s %.4f    %+.4f   %s"
              % (number, label, rmse(PIE_FRACTION, predicted),
                 r_squared(PIE_FRACTION, predicted), PIE_STATUS[number]))
    print()
    print("  Only option 3 was fitted on these points, and it is the one that wins:")
    print("  that is a fit being scored on its own training data.  Option 4 has never")
    print("  seen them, so its number is an honest out-of-sample check -- and it is")
    print("  the worst of the four in aggregate.")
    print()
    print("  Where option 4's error actually is:")
    print()
    print("     bu      measured |   opt1     opt2     opt3     opt4   |  opt4 - meas.")
    print("    " + "-" * 68)
    for burnup, observed in zip(PIE_BURNUP, PIE_FRACTION):
        predicted = [model(burnup, PIE_TEMPERATURE) for _, _, model, _, _, _ in OPTIONS]
        print("    %6.2f   %.4f   |  %.4f   %.4f   %.4f   %.4f  |  %+.4f"
              % (burnup, observed, predicted[0], predicted[1], predicted[2],
                 predicted[3], predicted[3] - observed))
    print()

    # Where the error sits, measured rather than asserted, so that this paragraph
    # cannot go stale the next time calibrate.py moves the parameters.
    residuals = [option4_landau(bu, PIE_TEMPERATURE) - obs
                 for bu, obs in zip(PIE_BURNUP, PIE_FRACTION)]
    over = [(bu, r) for bu, r in zip(PIE_BURNUP, residuals) if r > 0.1]
    saturation = _bisect(
        lambda b: hbs_state(b, PIE_TEMPERATURE).theta_deg - THETA_HAGB * (1.0 - 1e-12),
        1.0, 400.0)
    if over:
        print("  It is not spread out.  %d of the 8 points carry a residual above +0.10,"
              % len(over))
        print("  at %s MWd/kgU, and all on the same side: there the lever"
              % ", ".join("%.1f" % bu for bu, _ in over))
        print("  rule has already saturated -- Theta reaches theta_HAGB at %.1f MWd/kgU"
              % saturation)
        print("  at this temperature and P = %g -- while the data is still at %.2f-%.2f."
              % (REFERENCE_POROSITY,
                 min(PIE_FRACTION[PIE_BURNUP.index(bu)] for bu, _ in over),
                 max(PIE_FRACTION[PIE_BURNUP.index(bu)] for bu, _ in over)))
    else:
        print("  No single point dominates: every residual is below +0.10.")
    print()
    print("  Read with care: these 8 points carry NO resolved temperature -- 900 K is")
    print("  assumed for all of them.  The EBSD data shows that in exactly this burnup")
    print("  band the temperature is a first-order variable: at 76-84 MWd/kgU the")
    print("  measured fraction runs from 0.20 (T ~ 860 K) to 1.00 (T ~ 665 K).  A model")
    print("  with a real temperature dependence is therefore being scored against")
    print("  points whose temperature is a guess.")

    print()
    print("=" * 78)
    print("EBSD data -- Zacharie-Aubrun et al. (2022) / Onofri et al. (2025)")
    print("=" * 78)
    print()
    print("  Only option 4 produces the mean misorientation and the subgrain radius;")
    print("  options 1-3 produce the restructured fraction alone.")
    print()

    fraction_rows = [r for r in rows if not math.isnan(r["f10"])]
    print("  restructured fraction X, %d points" % len(fraction_rows))
    print("  " + "-" * 74)
    for number, label, model, _, _, effective in OPTIONS:
        observed = [r["f10"] / 100.0 for r in fraction_rows]
        predicted = [model(burnup_for(r, effective), r["temperature"],
                           r["porosity"], r["grain_radius"])
                     for r in fraction_rows]
        status = "CALIBRATED on it" if number == 4 else ""
        print("  %d  %-27s RMSE %.4f   R2 %+.4f   %-16s %s"
              % (number, label, rmse(observed, predicted), r_squared(observed, predicted),
                 status, "at bu_eff" if effective else "at bu_local"))
    changed = sum(1 for r in fraction_rows
                  if abs(r["burnup_effective"] - r["burnup"]) > 1e-9)
    print()
    print("  Options 1 and 2 are scored at the EFFECTIVE burnup, which is what SCIANTIX")
    print("  feeds them; options 3 and 4 at the local burnup, which is what SCIANTIX")
    print("  feeds those.  The two differ on %d of these %d points, at the hot centre"
          % (changed, len(fraction_rows)))
    print("  positions where TRANSURANUS suppresses the burnup accumulation.")

    print()
    theta_obs = [r["theta_obs"] for r in rows]
    theta_mod = [hbs_state(r["burnup"], r["temperature"], porosity=r["porosity"],
                           grain_radius_m=r["grain_radius"]).theta_deg for r in rows]
    print("  mean misorientation Theta, %d points" % len(rows))
    print("  " + "-" * 74)
    print("  4  %-27s RMSE %.4f deg   R2 %+.4f   CALIBRATED on it"
          % ("Landau functional", rmse(theta_obs, theta_mod), r_squared(theta_obs, theta_mod)))

    size_rows = [r for r in rows if not math.isnan(r["radius_obs"])]
    radius_obs = [r["radius_obs"] for r in size_rows]
    radius_mod = [hbs_state(r["burnup"], r["temperature"], porosity=r["porosity"],
                            grain_radius_m=r["grain_radius"]).subgrain_radius_m
                  for r in size_rows]
    print()
    print("  subgrain radius r_n, %d points" % len(size_rows))
    print("  " + "-" * 74)
    print("  4  %-27s RMSE %.4f um    R2 %+.4f   CALIBRATED on it"
          % ("Landau functional", rmse(radius_obs, radius_mod) * 1e6,
             r_squared(radius_obs, radius_mod)))
    print()


# ---------------------------------------------------------------------------
# figures
# ---------------------------------------------------------------------------

def plot(rows):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    os.makedirs(FIGURES, exist_ok=True)
    burnups = [1.0 + 0.25 * i for i in range(660)]      # 1 -> 165 MWd/kgU

    # ---- figure 1: the three observables against burnup --------------------
    figure, axes = plt.subplots(1, 3, figsize=(15.4, 4.5), layout="constrained")

    for number, label, model, colour, style, _ in OPTIONS:
        width = 2.4 if number == 4 else 1.5
        axes[0].plot(burnups, [model(b, PIE_TEMPERATURE) for b in burnups],
                     style, color=colour, lw=width, label="%d  %s" % (number, label))
    axes[0].plot(PIE_BURNUP, PIE_FRACTION, "o", ms=8, color="white",
                 markeredgecolor="k", markeredgewidth=1.4, zorder=5,
                 label="PIE, Gerczak (2018) / Noirot (2015)")
    # the EBSD points are coloured by their local temperature: the scatter at fixed
    # burnup is not noise, it is the temperature dependence
    temperatures = [r["temperature"] for r in rows]
    low, high = min(temperatures), max(temperatures)
    fraction_rows = [r for r in rows if not math.isnan(r["f10"])]
    scatter = axes[0].scatter([r["burnup"] for r in fraction_rows],
                              [r["f10"] / 100.0 for r in fraction_rows],
                              c=[r["temperature"] for r in fraction_rows],
                              cmap="coolwarm", vmin=low, vmax=high, s=34, marker="s",
                              edgecolors="0.3", linewidths=0.4, zorder=4,
                              label="EBSD, Zacharie (2022) / Onofri (2025)")
    axes[0].set_ylabel(r"restructured fraction  $X$  [-]")
    axes[0].legend(fontsize=7.5, loc="lower right")
    axes[0].set_title("all four options, curves at T = 900 K", fontsize=10)

    axes[1].plot(burnups, [hbs_state(b, PIE_TEMPERATURE).theta_deg for b in burnups],
                 "k-", lw=2.4)
    axes[1].scatter([r["burnup"] for r in rows], [r["theta_obs"] for r in rows],
                    c=temperatures, cmap="coolwarm", vmin=low, vmax=high, s=34,
                    marker="s", edgecolors="0.3", linewidths=0.4)
    axes[1].axhline(10.0, ls=":", lw=0.8, color="0.5")
    axes[1].set_ylabel(r"mean misorientation  $\Theta$  [deg]")
    axes[1].set_title(r"option 4 only; $\theta_{HAGB}$ dotted", fontsize=10)

    radii = [hbs_state(b, PIE_TEMPERATURE).subgrain_radius_m for b in burnups]
    axes[2].plot([b for b, r in zip(burnups, radii) if not math.isnan(r)],
                 [r * 1e6 for r in radii if not math.isnan(r)], "k-", lw=2.4)
    size_rows = [r for r in rows if not math.isnan(r["radius_obs"])]
    axes[2].scatter([r["burnup"] for r in size_rows],
                    [r["radius_obs"] * 1e6 for r in size_rows],
                    c=[r["temperature"] for r in size_rows], cmap="coolwarm",
                    vmin=low, vmax=high, s=34, marker="s",
                    edgecolors="0.3", linewidths=0.4)
    axes[2].set_ylabel(r"subgrain radius  $r_n$  [$\mu$m]")
    axes[2].set_ylim(0.0, 1.2)
    axes[2].set_title("option 4 only; EBSD ECD50%/2", fontsize=10)

    for axis in axes:
        axis.set_xlabel("local burnup  [MWd/kgU]")
        axis.set_xlim(0, 165)
    figure.suptitle("HBS formation, validation against experimental data. "
                    "EBSD squares are coloured by their own local temperature; "
                    "the curves are at 900 K")
    bar = figure.colorbar(scatter, ax=axes, fraction=0.022, pad=0.015)
    bar.set_label("EBSD local temperature  [K]")
    first = os.path.join(FIGURES, "validation.png")
    figure.savefig(first, dpi=140)

    # ---- figure 2: parity, each point at its own local temperature ---------
    figure, axes = plt.subplots(1, 3, figsize=(13.5, 4.6), layout="constrained")

    for number, label, model, colour, _, _ in OPTIONS:
        predicted = [model(b, PIE_TEMPERATURE) for b in PIE_BURNUP]
        marker = "o" if number == 4 else "^"
        size = 9 if number == 4 else 6
        axes[0].plot(PIE_FRACTION, predicted, marker, ms=size, color=colour, alpha=0.85,
                     label="%d  %s%s" % (number, label,
                                         "  (fitted here)" if number == 3 else ""))
    axes[0].set_xlabel("measured  $X$"); axes[0].set_ylabel("predicted  $X$")
    axes[0].set_title("PIE, Gerczak / Noirot  (8 points)", fontsize=10)
    axes[0].legend(fontsize=7.5, loc="lower right")

    predicted = [option4_landau(r["burnup"], r["temperature"], r["porosity"], r["grain_radius"])
                 for r in fraction_rows]
    axes[1].plot([r["f10"] / 100.0 for r in fraction_rows], predicted, "o", ms=5, color="k")
    axes[1].set_xlabel("measured  $X$"); axes[1].set_ylabel("predicted  $X$")
    axes[1].set_title("EBSD, restructured fraction  (%d points)" % len(fraction_rows),
                      fontsize=10)

    axes[2].plot([r["theta_obs"] for r in rows],
                 [hbs_state(r["burnup"], r["temperature"], porosity=r["porosity"],
                            grain_radius_m=r["grain_radius"]).theta_deg for r in rows],
                 "o", ms=5, color="k")
    axes[2].set_xlabel(r"measured  $\Theta$  [deg]")
    axes[2].set_ylabel(r"predicted  $\Theta$  [deg]")
    axes[2].set_title(r"EBSD, mean misorientation  (%d points)" % len(rows), fontsize=10)

    for axis, top in zip(axes, (1.05, 1.05, 10.5)):
        axis.plot([0, top], [0, top], "-", lw=0.9, color="0.6", zorder=0)
        axis.set_xlim(0, top); axis.set_ylim(0, top)
        axis.set_aspect("equal", adjustable="box")
    figure.suptitle("Parity. Every point is evaluated at its own local burnup and "
                    "temperature; the PIE points at 900 K")
    second = os.path.join(FIGURES, "validation_parity.png")
    figure.savefig(second, dpi=140)

    print("written: %s" % first)
    print("written: %s" % second)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--no-plot", action="store_true", help="metrics only")
    arguments = parser.parse_args(argv)

    rows = ebsd_points()
    report(rows)
    if not arguments.no_plot:
        try:
            plot(rows)
        except ImportError:
            print("(matplotlib not available, figures skipped)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
