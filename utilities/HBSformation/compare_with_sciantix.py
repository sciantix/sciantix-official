"""Check that the SCIANTIX C++ model reproduces the reference Python implementation.

`src/models/HighBurnupStructureFormation.C` case 4 is a statement-by-statement
transcription of `hbs_formation_landau.py :: hbs_state()`.  This script is what keeps
it one: it replays the model, in Python, on the (burnup, temperature) pairs SCIANTIX
actually stepped through, and compares the three outputs column by column.

@author  E. Cappellari
@date    2026-09-05

Usage
-----
    python3 compare_with_sciantix.py ../../regression/hbs/test_UO2HBS_landau/output.txt

It replays the SCIANTIX-specific steps too, so the comparison covers the whole
algorithm and not only the closed-form part:

  * the burnup conversion, MWd/kgUO2 -> MWd/kgU (= GWd/tU);
  * the other three inputs the model reads from SCIANTIX rather than from its own
    constants - the porosity and the deviation from stoichiometry, which enter the
    shear modulus of Eq. (2), and the grain radius, which is the ceiling of Eq. (9);
  * the monotonic lock on the mean misorientation, which makes the restructuring
    irreversible and from which the radius and the fraction are then derived;
  * the two conventions the C++ needs and the reference script does not: the radius
    is 0.0 rather than NaN where there is no substructure, and the restructured
    fraction is capped at ALPHA_MAX because SCIANTIX divides by (1 - alpha).

Every printed value stands for an interval of doubles that would have printed the 
same way; the model is evaluated at the corners of the box spanned by all four 
printed inputs, and since every output is monotone in each of them, the corner 
values bracket every value the C++ could legitimately have produced. 
A column passes when the SCIANTIX number lies inside its bracket, up to a small slack 
for the rounding of the OUTPUT column itself.  
The residual reported is how far outside the bracket the value falls.

Exit status is 0 when every column agrees, 1 otherwise.
"""

from __future__ import annotations

import argparse
import math
import sys

import itertools

from hbs_formation_landau import (ALPHA_MAX, FABRICATION_POROSITY, THETA_HAGB, THETA_MAX,
                                  THETA_U, UO2_TO_U, hbs_state, wall_geometry)

# Column headers written by src/file_manager/Output.C.
COL_BURNUP = "Burnup (MWd/kgUO2)"
COL_TEMPERATURE = "Temperature (K)"
COL_POROSITY = "Porosity (/)"
COL_STOICHIOMETRY = "Stoichiometry deviation (/)"
COL_GRAIN_RADIUS = "Grain radius (m)"
COL_THETA = "Mean misorientation (deg)"
COL_RADIUS = "Subgrain radius (m)"
COL_FRACTION = "Restructured volume fraction (/)"
COL_RHO = "Dislocation density (1/m2)"

# The inputs case 4 reads from SCIANTIX, in the order `outputs()` takes them.
INPUTS = (COL_BURNUP, COL_TEMPERATURE, COL_POROSITY, COL_STOICHIOMETRY, COL_GRAIN_RADIUS)

# Of those, the ones the output file must carry for the comparison to mean anything.
# The porosity and the deviation from stoichiometry are NOT among them: they enter
# only through G and nu, which cancel exactly in eta^2 = -C2/(2 C4) since the
# grain-boundary surface term was dropped, so none of the four compared quantities
# depends on them.  `Output.C` gates those two columns on iGrainBoundaryVenting and
# iStoichiometryDeviation, which the HBS cases leave at 0, so they are usually
# absent.  When they ARE present the script uses them rather than the defaults, so
# that this check keeps working if the functional ever regains a term that breaks
# the cancellation -- and `--selftest` is what asserts the cancellation still holds.
REQUIRED_INPUTS = (COL_BURNUP, COL_TEMPERATURE, COL_GRAIN_RADIUS)
OPTIONAL_INPUT_DEFAULTS = {COL_POROSITY: FABRICATION_POROSITY, COL_STOICHIOMETRY: 0.0}

COMPARED = (
    (COL_THETA, "theta", "deg"),
    (COL_RADIUS, "radius", "m"),
    (COL_FRACTION, "fraction", "-"),
    (COL_RHO, "rho_tot", "1/m2"),
)

# src/file_manager/Output.C, the branch used for iOutput = 1.
OUTPUT_SIGNIFICANT_DIGITS = 7


def _split(line):
    """One tab-separated line as a list of cells, without the trailing empty one.

    `Output.C` terminates every field with a tab, the header row included, so a plain
    split leaves an empty cell at the end of every line.
    """
    cells = line.rstrip("\n").split("\t")
    while cells and not cells[-1].strip():
        cells.pop()
    return cells


def read_output(path):
    """`output.txt` as (header list, list of row lists of floats)."""
    with open(path, encoding="utf-8") as handle:
        header = _split(handle.readline())
        rows = []
        for line in handle:
            cells = _split(line)
            if cells:
                rows.append([float(cell) for cell in cells])
    return header, rows


def printed_interval(value, digits=OUTPUT_SIGNIFICANT_DIGITS):
    """The interval of doubles that print as `value` at `digits` significant digits."""
    if value == 0.0:
        return 0.0, 0.0
    quantum = 10.0 ** (math.floor(math.log10(abs(value))) - (digits - 1))
    return value - 0.5 * quantum, value + 0.5 * quantum


def outputs(burnup_uo2, temperature, porosity, stoichiometry_deviation, grain_radius,
            theta_previous):
    """The four compared quantities at one set of inputs, lock included."""
    state = hbs_state(burnup_uo2 / UO2_TO_U, temperature, porosity=porosity,
                      stoichiometry_deviation=stoichiometry_deviation,
                      grain_radius_m=grain_radius)

    # monotonic lock: HBS restructuring is irreversible
    theta = min(THETA_HAGB, max(theta_previous, state.theta_deg))

    if theta > state.theta_deg:
        # re-derive the radius and the fraction from the locked misorientation, on the
        # geometry of the current burnup - exactly what the C++ does
        radius, fraction = at_imposed_theta(state, theta, grain_radius)
    else:
        radius, fraction = state.subgrain_radius_m, state.restructured_fraction

    radius = 0.0 if math.isnan(radius) else radius     # the C++ writes 0.0, not NaN
    return {"theta": theta, "radius": radius, "fraction": fraction, "rho_tot": state.rho_tot}


def at_imposed_theta(state, theta, grain_radius):
    """(radius, fraction) at an imposed misorientation, Eqs. (9) and (10).

    Needed only when the monotonic lock binds; it reuses the geometry of the current
    burnup, which is what the C++ does, and keeps the same grain ceiling.
    """
    _, s_over_v_max, dr_over_r_max = wall_geometry(state.rho_tot)
    eta = math.radians(theta) / THETA_MAX
    s_over_v = s_over_v_max * eta
    dr_over_r = dr_over_r_max * eta * eta
    radius = min(1.5 / s_over_v * (1.0 + dr_over_r), grain_radius) if s_over_v > 0.0 else math.nan
    fraction = min(ALPHA_MAX, max(0.0, (theta - THETA_U) / (THETA_HAGB - THETA_U)))
    return radius, fraction


def bracket(printed_inputs, theta_previous):
    """Per-quantity (low, high) over the box of inputs consistent with what was printed.

    Every output is monotone in each of the five inputs, so evaluating the corners of
    the box brackets every value the C++ could have produced.  Inputs printed as an
    exact 0.0 collapse to a single point, so the box is usually smaller than 2^5.
    """
    boxes = [sorted(set(printed_interval(value))) for value in printed_inputs]
    corners = [outputs(*combination, theta_previous=theta_previous)
               for combination in itertools.product(*boxes)]
    return {key: (min(c[key] for c in corners), max(c[key] for c in corners))
            for key in corners[0]}


def compare(path, verbose=True):
    header, rows = read_output(path)
    missing = [name for name in REQUIRED_INPUTS + tuple(c for c, _, _ in COMPARED)
               if name not in header]
    if missing:
        raise SystemExit(
            "%s does not carry %s.\nThe case must run with "
            "iHighBurnupStructureFormation = 4, which is what turns those columns on."
            % (path, ", ".join('"%s"' % name for name in missing)))

    substituted = [name for name in OPTIONAL_INPUT_DEFAULTS if name not in header]
    if substituted and verbose:
        print("  not in this output file, module defaults substituted: %s"
              % ", ".join('%s = %g' % (name, OPTIONAL_INPUT_DEFAULTS[name])
                          for name in substituted))
        print("  they cancel out of all four compared quantities, see the header of "
              "this script.")
        print()

    index = {name: header.index(name) for name in header}
    worst = {key: (0.0, 0.0, None) for _, key, _ in COMPARED}   # residual, relative, row
    theta_previous = 0.0
    lock_bound = 0

    for number, row in enumerate(rows):
        printed_inputs = tuple(row[index[name]] if name in index
                               else OPTIONAL_INPUT_DEFAULTS[name] for name in INPUTS)

        nominal = outputs(*printed_inputs, theta_previous=theta_previous)
        bounds = bracket(printed_inputs, theta_previous)
        instant = hbs_state(printed_inputs[0] / UO2_TO_U, printed_inputs[1],
                            porosity=printed_inputs[2],
                            stoichiometry_deviation=printed_inputs[3],
                            grain_radius_m=printed_inputs[4])
        lock_bound += nominal["theta"] > instant.theta_deg
        theta_previous = nominal["theta"]

        for column, key, _ in COMPARED:
            got = row[index[column]]
            low, high = bounds[key]
            # the printed column is itself rounded, so widen the bracket by its own quantum
            slack = 0.5 * (printed_interval(got)[1] - printed_interval(got)[0])
            residual = max(low - slack - got, got - high - slack, 0.0)
            relative = residual / abs(got) if got != 0.0 else residual
            if residual > worst[key][0]:
                worst[key] = (residual, relative, number)

    ok = all(worst[key][0] == 0.0 for _, key, _ in COMPARED)

    if verbose:
        print("SCIANTIX vs utilities/HBSformation/hbs_formation_landau.py")
        print("  %s" % path)
        print("  %d timesteps; the monotonic lock binds on %d of them" % (len(rows), lock_bound))
        print("  each value is bracketed over the inputs consistent with the %d printed"
              % OUTPUT_SIGNIFICANT_DIGITS)
        print("  significant digits of Output.C -- see the module docstring")
        print()
        print("  column                             outside bracket   relative   at row")
        print("  " + "-" * 70)
        for column, key, unit in COMPARED:
            residual, relative, number = worst[key]
            print("  %-32s   %.3e       %.3e   %s   %s"
                  % ("%s [%s]" % (column.rsplit(" (", 1)[0], unit), residual, relative,
                     "     -" if number is None else "%6d" % number,
                     "ok" if residual == 0.0 else "FAIL"))
        print()
        print("  ->  %s" % ("AGREE: every value lies inside its bracket" if ok else "DISAGREE"))
    return ok


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("output", help="the output.txt of a case run with "
                                       "iHighBurnupStructureFormation = 4")
    arguments = parser.parse_args(argv)
    return 0 if compare(arguments.output) else 1


if __name__ == "__main__":
    sys.exit(main())
