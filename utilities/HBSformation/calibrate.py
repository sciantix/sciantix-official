"""Joint calibration of the Landau HBS-formation model on Theta AND on the sizes.

@author  E. Cappellari
@date    2026-09-06
---------------------------------------------------------------------------------
Fits `beta`, `k` and `rho_c` of `hbs_formation_landau.py` against the EBSD dataset
in `data/`, and prints the result ready to paste back into that file and into the
`case 4` parameter push of `src/models/HighBurnupStructureFormation.C`.

The objective is dimensionless and symmetric in the two observables:

    J(k, beta, rho_c) = <(Theta_pred - Theta_obs)^2>/var(Theta)
                      + w <(r_pred - r_obs)^2>/var(r)


This script calls `hbs_state()` itself rather than re-deriving the model, so it
cannot drift away from the implementation it is calibrating.

USAGE
---------------------------------------------------------------------------------
    python3 calibrate.py                        # the shipped calibration
    python3 calibrate.py --weight 0.2           # weigh the sizes more
    python3 calibrate.py --weight 0             # Theta alone, see the plateau
    python3 calibrate.py --fix-rho-c 4.55e-6    # rho_c anchored to a grain radius
    python3 calibrate.py --front                # scan w, write the trade-off figure
    python3 calibrate.py --reference-table      # regenerate the frozen self-test table

Needs numpy and scipy.  `hbs_formation_landau.py` itself stays stdlib-only.
"""

from __future__ import annotations

import argparse
import math
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from hbs_formation_landau import (  # noqa: E402
    ALPHA_MAX,
    N_FAMILIES,
    REFERENCE_POROSITY,
    REFERENCE_TEMPERATURE,
    ModelParameters,
    hbs_state,
    load_ebsd,
    measured_radius,
    regime_boundaries,
    theta_measured,
    validate,
)

try:
    import numpy as np
    from scipy.optimize import differential_evolution
except ImportError as error:  # pragma: no cover
    sys.exit("calibrate.py needs numpy and scipy (%s).\n"
             "Install them, or use hbs_formation_landau.py alone, which does not." % error)

# Search box: (k, beta, log10 rho_c).  Deliberately wide -- with the narrow bounds
# of the earlier single-observable fits the joint fit ran into the lower edge of
# rho_c and stopped there.
BOUNDS_K = (0.001, 50.0)
BOUNDS_BETA = (2.0, 300.0)
BOUNDS_LOG10_RHO_C = (8.0, 20.0)

# Weight of the size term in the objective.  0.05 keeps RMSE(Theta) within a few
# per cent of its unconstrained minimum while removing the plateau entirely.
WEIGHT_DEFAULT = 0.05

# Several seeds because at w > 0 there is a second basin, near beta = 14 with
# rho_c against the upper bound, whose RMSE(Theta) is clearly worse: a single seed
# can land in it and look converged.
SEEDS_DEFAULT = 6


# ---------------------------------------------------------------------------
# TARGETS
# ---------------------------------------------------------------------------

def load_targets(path=None):
    """(theta targets, size targets) as dicts of arrays.

    Theta uses every row with burnup > 0; the sizes only the subset that carries
    an ECD50%.  Both keep the porosity and the grain radius of their specimen.
    """
    rows = [r for r in (load_ebsd(path) if path else load_ebsd()) if r["burnup"] > 0.0]

    def pack(subset, values):
        return {
            "burnup": np.array([r["burnup"] for r in subset]),
            "temperature": np.array([r["temperature"] for r in subset]),
            "porosity": np.array([r["porosity"] for r in subset]),
            "grain_radius": np.array([r["grain_radius"] for r in subset]),
            "label": [r["label"] for r in subset],
            "y": np.array(values),
        }

    theta = pack(rows, [theta_measured(r) for r in rows])
    with_size = [r for r in rows if not math.isnan(measured_radius(r))]
    size = pack(with_size, [measured_radius(r) for r in with_size])
    with_fraction = [r for r in rows if not math.isnan(r["f10"])]
    fraction = pack(with_fraction, [r["f10"] / 100.0 for r in with_fraction])
    return theta, size, fraction


def predict(parameters, targets):
    """(Theta [deg], r_n [m], X [-]) of the model on a target set."""
    theta, radius, fraction = [], [], []
    for burnup, temperature, porosity, grain_radius in zip(
            targets["burnup"], targets["temperature"],
            targets["porosity"], targets["grain_radius"]):
        state = hbs_state(float(burnup), float(temperature), porosity=float(porosity),
                          grain_radius_m=float(grain_radius), parameters=parameters)
        theta.append(state.theta_deg)
        radius.append(state.subgrain_radius_m)
        fraction.append(state.restructured_fraction)
    return np.array(theta), np.array(radius), np.array(fraction)


# ---------------------------------------------------------------------------
# THE FIT
# ---------------------------------------------------------------------------

def build_parameters(vector, n_families, fixed_rho_c=None):
    """A `ModelParameters` from the optimizer's vector.

    Everything is cast to a plain `float`: `differential_evolution` hands back numpy
    scalars, whose `repr` is `np.float64(...)` and would be pasted verbatim into the
    Python constants and the C++ parameter push by `print_paste_block`.
    """
    if fixed_rho_c is None:
        k_sweep, beta, rho_c = vector[0], vector[1], 10.0 ** vector[2]
    else:
        k_sweep, beta, rho_c = vector[0], vector[1], fixed_rho_c
    return ModelParameters(n_families=float(n_families), beta=float(beta),
                           k_sweep=float(k_sweep), rho_c=float(rho_c))


def objective(vector, theta, size, fraction, weight, fraction_weight,
              variance_theta, variance_radius, variance_fraction, n_families, fixed_rho_c):
    parameters = build_parameters(vector, n_families, fixed_rho_c)
    theta_model, _, _ = predict(parameters, theta)
    _, radius_model, _ = predict(parameters, size)
    if not (np.all(np.isfinite(theta_model)) and np.all(np.isfinite(radius_model))):
        return 1.0e6
    cost = float(np.mean((theta_model - theta["y"]) ** 2) / variance_theta)
    if weight > 0.0:
        cost += weight * float(np.mean((radius_model - size["y"]) ** 2) / variance_radius)
    if fraction_weight > 0.0:
        _, _, fraction_model = predict(parameters, fraction)
        cost += fraction_weight * float(
            np.mean((fraction_model - fraction["y"]) ** 2) / variance_fraction)
    return cost


def fit(theta, size, fraction, weight=WEIGHT_DEFAULT, fraction_weight=0.0,
        seeds=SEEDS_DEFAULT, n_families=N_FAMILIES, fixed_rho_c=None,
        maxiter=800, popsize=25, verbose=True):
    """Repeated global search. Returns (best ModelParameters, best cost, all runs)."""
    variance_theta = float(theta["y"].var())
    variance_radius = float(size["y"].var())
    variance_fraction = float(fraction["y"].var())
    bounds = [BOUNDS_K, BOUNDS_BETA]
    if fixed_rho_c is None:
        bounds.append(BOUNDS_LOG10_RHO_C)

    runs = []
    for seed in range(seeds):
        result = differential_evolution(
            objective, bounds,
            args=(theta, size, fraction, weight, fraction_weight, variance_theta,
                  variance_radius, variance_fraction, n_families, fixed_rho_c),
            seed=seed, tol=1e-12, maxiter=maxiter, popsize=popsize)
        parameters = build_parameters(result.x, n_families, fixed_rho_c)
        runs.append((float(result.fun), parameters))
        if verbose:
            print("    seed %d   J = %.6f   k = %8.5f   beta = %7.3f   rho_c = %.4e"
                  % (seed, result.fun, parameters.k_sweep, parameters.beta, parameters.rho_c))
    runs.sort(key=lambda item: item[0])
    return runs[0][1], runs[0][0], runs


def scores(parameters, theta, size, fraction=None):
    """RMSE and R2 of the fitted observables, and of the fraction when given."""
    theta_model, _, _ = predict(parameters, theta)
    _, radius_model, _ = predict(parameters, size)

    def metrics(observed, model):
        residual = observed - model
        return (float(np.sqrt(np.mean(residual ** 2))),
                float(1.0 - np.sum(residual ** 2) / np.sum((observed - observed.mean()) ** 2)))

    rmse_theta, r2_theta = metrics(theta["y"], theta_model)
    rmse_radius, r2_radius = metrics(size["y"], radius_model)
    result = dict(rmse_theta=rmse_theta, r2_theta=r2_theta,
                  rmse_radius=rmse_radius, r2_radius=r2_radius)
    if fraction is not None:
        _, _, fraction_model = predict(parameters, fraction)
        result["rmse_fraction"], result["r2_fraction"] = metrics(fraction["y"], fraction_model)
    return result


# ---------------------------------------------------------------------------
# REPORTING
# ---------------------------------------------------------------------------

def print_paste_block(parameters, weight, seeds):
    """The fitted parameters, in the exact form the two implementations need."""
    print()
    print("  Paste into utilities/HBSformation/hbs_formation_landau.py:")
    print()
    # Full precision on both sides: the Python and the C++ must carry the SAME
    # literal, or compare_with_sciantix.py stops being a test of the port.
    print("    N_FAMILIES = %-26s # -" % repr(parameters.n_families))
    print("    BETA       = %-26s # -" % repr(parameters.beta))
    print("    K_SWEEP    = %-26s # -" % repr(parameters.k_sweep))
    print("    RHO_C      = %-26s # m^-2" % repr(parameters.rho_c))
    print()
    print("  Paste into the case 4 parameter push of")
    print("  src/models/HighBurnupStructureFormation.C (offsets 0-3):")
    print()
    print("    parameter.push_back(%s);  // n, dislocation families in a wall"
          % repr(parameters.n_families))
    print("    parameter.push_back(%s);  // beta, wall geometry" % repr(parameters.beta))
    print("    parameter.push_back(%s);  // k, sweeping" % repr(parameters.k_sweep))
    print("    parameter.push_back(%s);  // rho_c, strain-field cut-off (m^-2)"
          % repr(parameters.rho_c))
    print()
    print("  Joint calibration, weight w = %g on the size term, %d seeds." % (weight, seeds))
    print("  rho_c^(-1/2) = %.4f um." % (parameters.rho_c ** -0.5 * 1e6))


def print_reference_table(parameters, temperature=REFERENCE_TEMPERATURE,
                          porosity=REFERENCE_POROSITY):
    """The frozen self-test table of `hbs_formation_landau.py`, regenerated."""
    threshold, onset, saturation = regime_boundaries(temperature, porosity, parameters)
    # threshold + 0.05 is in the band where Eq. (9) would diverge and the grain
    # ceiling binds: the frozen table has to pin that branch too.
    burnups = [10.0, 20.0, 30.0, 40.0,
               round(threshold, 4), round(threshold + 0.05, 4), round(threshold + 0.5, 4),
               50.0, round(onset, 4), 60.0, 70.0, 80.0,
               round(saturation, 4), 100.0, 150.0]
    burnups = sorted(set(burnups))

    print("REFERENCE_TABLE = [")
    for burnup in burnups:
        state = hbs_state(burnup, temperature, porosity=porosity, parameters=parameters)
        radius = ("math.nan" if math.isnan(state.subgrain_radius_m)
                  else repr(state.subgrain_radius_m))
        fraction = ("ALPHA_MAX" if state.restructured_fraction == ALPHA_MAX
                    else repr(state.restructured_fraction))
        print("    (%s, %s, %s, %s, %s),"
              % (repr(burnup), repr(state.rho_tot), repr(state.theta_deg), radius, fraction))
    print("]")
    print()
    print("BU_THRESHOLD = %.4f      # GWd/tU   C2 = 0: Theta leaves zero" % threshold)
    print("BU_ONSET = %.4f          # GWd/tU   Theta = theta_u: X leaves zero" % onset)
    print("BU_SATURATION = %.4f     # GWd/tU   Theta = theta_HAGB: X reaches its cap" % saturation)


def plot_front(theta, size, fraction, weights, seeds, n_families, maxiter, popsize,
               path=None):
    """RMSE(Theta) against RMSE(r_n) as the size weight is scanned."""
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    if path is None:
        path = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "figures", "calibration_front.png")

    points = []
    for weight in weights:
        print("  w = %g" % weight)
        parameters, _, _ = fit(theta, size, fraction, weight, 0.0, seeds, n_families,
                               maxiter=maxiter, popsize=popsize, verbose=False)
        score = scores(parameters, theta, size)
        points.append((weight, parameters, score))
        print("    k = %8.5f  beta = %7.3f  rho_c = %.4e   "
              "RMSE_Theta = %.4f deg   RMSE_r = %.4f um"
              % (parameters.k_sweep, parameters.beta, parameters.rho_c,
                 score["rmse_theta"], score["rmse_radius"] * 1e6))

    figure, axis = plt.subplots(figsize=(7.0, 5.0))
    axis.plot([p[2]["rmse_radius"] * 1e6 for p in points],
              [p[2]["rmse_theta"] for p in points], "o-", color="k")
    for weight, _, score in points:
        axis.annotate(" w = %g" % weight,
                      (score["rmse_radius"] * 1e6, score["rmse_theta"]), fontsize=8)
    axis.set_xlabel(r"RMSE on the subgrain radius $r_n$  [$\mu$m]")
    axis.set_ylabel(r"RMSE on the mean misorientation $\Theta$  [deg]")
    axis.set_title("Joint calibration: what the size term costs on $\\Theta$")
    axis.grid(alpha=0.3)
    figure.tight_layout()
    os.makedirs(os.path.dirname(path), exist_ok=True)
    figure.savefig(path, dpi=150)
    plt.close(figure)
    print("  written: %s" % path)
    return points


def main(argv=None):
    parser = argparse.ArgumentParser(
        description="Joint calibration of beta, k and rho_c of the Landau HBS-formation model.",
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--weight", type=float, default=WEIGHT_DEFAULT, metavar="W",
                        help="weight of the size term in the objective (default %g; "
                             "0 fits Theta alone)" % WEIGHT_DEFAULT)
    parser.add_argument("--fraction-weight", dest="fraction_weight", type=float,
                        default=0.0, metavar="W",
                        help="weight of the restructured fraction in the objective "
                             "(default 0: the fraction is a PREDICTION of the calibrated "
                             "model, not a target). Raising it buys very little -- see the "
                             "'Where the fraction error comes from' section of README.md")
    parser.add_argument("--seeds", type=int, default=SEEDS_DEFAULT, metavar="N",
                        help="independent global searches (default %d)" % SEEDS_DEFAULT)
    parser.add_argument("--n", type=float, default=N_FAMILIES, metavar="N",
                        help="dislocation families in a wall, held fixed (default %g)"
                             % N_FAMILIES)
    parser.add_argument("--fix-rho-c", type=float, default=None, metavar="R",
                        help="fix rho_c to R^-2 with R a length in metres, e.g. 4.55e-6 to "
                             "anchor it to an as-fabricated grain radius")
    parser.add_argument("--maxiter", type=int, default=800)
    parser.add_argument("--popsize", type=int, default=25)
    parser.add_argument("--front", action="store_true",
                        help="scan the size weight and write figures/calibration_front.png")
    parser.add_argument("--reference-table", dest="reference_table", action="store_true",
                        help="print the frozen self-test table for the SHIPPED parameters "
                             "and exit, without fitting")
    arguments = parser.parse_args(argv)

    shipped = ModelParameters()

    if arguments.reference_table:
        print_reference_table(shipped)
        return 0

    theta, size, fraction = load_targets()
    print("Joint calibration of the Landau HBS-formation model")
    print("  Theta  N = %2d   sizes  N = %2d   fractions  N = %2d"
          % (len(theta["y"]), len(size["y"]), len(fraction["y"])))
    print("  w = %g (sizes)   w_f = %g (fraction)   seeds = %d   n = %g"
          % (arguments.weight, arguments.fraction_weight, arguments.seeds, arguments.n))
    fixed_rho_c = None
    if arguments.fix_rho_c is not None:
        fixed_rho_c = arguments.fix_rho_c ** -2.0
        print("  rho_c fixed at %.4e m^-2  (%.3f um)"
              % (fixed_rho_c, arguments.fix_rho_c * 1e6))
    print()

    if arguments.front:
        plot_front(theta, size, fraction, [0.0, 0.01, 0.05, 0.2, 1.0], arguments.seeds,
                   arguments.n, arguments.maxiter, arguments.popsize)
        return 0

    parameters, cost, _ = fit(theta, size, fraction, arguments.weight,
                              arguments.fraction_weight, arguments.seeds, arguments.n,
                              fixed_rho_c, arguments.maxiter, arguments.popsize)
    score = scores(parameters, theta, size, fraction)

    print()
    print("  best   J = %.6f" % cost)
    print("    n     = %.6g" % parameters.n_families)
    print("    beta  = %.6g" % parameters.beta)
    print("    k     = %.6g" % parameters.k_sweep)
    print("    rho_c = %.6g m^-2   (rho_c^(-1/2) = %.4f um)"
          % (parameters.rho_c, parameters.rho_c ** -0.5 * 1e6))
    print()
    print("    Theta   N = %2d   RMSE = %.4f deg   R2 = %.4f"
          % (len(theta["y"]), score["rmse_theta"], score["r2_theta"]))
    print("    r_n     N = %2d   RMSE = %.4f um    R2 = %.4f"
          % (len(size["y"]), score["rmse_radius"] * 1e6, score["r2_radius"]))

    print("    X       N = %2d   RMSE = %.4f       R2 = %.4f   (in the objective at w_f = %g)"
          % (len(fraction["y"]), score["rmse_fraction"], score["r2_fraction"],
             arguments.fraction_weight))

    shipped_score = scores(shipped, theta, size)
    print()
    print("    shipped parameters for comparison:")
    print("      beta = %.6g, k = %.6g, rho_c = %.6g"
          % (shipped.beta, shipped.k_sweep, shipped.rho_c))
    print("      Theta RMSE = %.4f deg (R2 = %.4f),   r_n RMSE = %.4f um (R2 = %.4f)"
          % (shipped_score["rmse_theta"], shipped_score["r2_theta"],
             shipped_score["rmse_radius"] * 1e6, shipped_score["r2_radius"]))

    print()
    print("  All three observables with the fitted parameters:")
    print()
    validate(parameters=parameters)

    print_paste_block(parameters, arguments.weight, arguments.seeds)
    return 0


if __name__ == "__main__":
    sys.exit(main())
