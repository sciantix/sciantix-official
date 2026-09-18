"""UO2 grain boundary energy gamma(Delta theta), target for the phase field.

    python3 uo2_gb_energy.py [--refresh] [--samples N]

References
==========
[Z21]  J. Zhang et al., "Grain boundary energy of UO2 and CeO2 from atomistic
       simulations and a 5-DOF interpolation", J. Am. Ceram. Soc. (2021).
       The Bulatov-Reed-Kumar 5-DOF function refitted for UO2 (Basak
       potential); eRGB = 1.545 J/m^2.  Translated to Python in Zhang/.
[BRK]  V.V. Bulatov, B.W. Reed, M. Kumar, "Grain boundary energy function for
       fcc metals", Acta Mater. 65 (2014) 161.
[T26]  I.T. Tandogan, M. Budnitzki, S. Sandfeld, JMPS 206 (2026) 106325.
       Fig. 2 calibrates the phase field on Cu <100> tilt boundaries.

What this module produces
=========================
The phase field of phasefield_tandogan_1d.py needs ONE curve gamma(Delta theta):
its lattice orientation theta is a single scalar and the interfaces are
isotropic (a(n_gb, theta) = 1, Sec. 2.2.1 of [T26]).  The 5-DOF function of
[Z21] gives gamma(P, Q) for a full misorientation + boundary plane, so it has
to be reduced to one degree of freedom.  Three reductions are tabulated:

  gamma_random   average of GB5DOF over random misorientation AXES and random
                 boundary PLANES at fixed misorientation angle.  This is the
                 reduction consistent with a(n_gb, theta) = 1 and the one the
                 phase field is fitted to.
  gamma_st100    symmetric tilt about <100>, the literal analogue of the Cu
                 choice of Fig. 2 of [T26].
  gamma_st110    symmetric tilt about <110>, the wall built from the a/2<110>
                 Burgers vector of UO2.

The three agree to a few per cent below 10 deg and differ by up to ~10% at
30 deg; only the first is used by the phase field, the other two are kept for
the comparison plot and to document the spread.

Note on the data: the MD set of [Z21] (Zhang/supplementarydata) has no
symmetric-tilt point below 12.7 deg, so the low-angle Read-Shockley branch of
all three curves comes from the [BRK] interpolation, not from MD.
"""

import contextlib
import csv
import io
import math
import os
import sys

import numpy as np
from scipy.spatial.transform import Rotation

_HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(_HERE, "Zhang"))

from GB_functions import GB5DOF, makeparvec, set100, set110, set111

MATERIAL = "UO2"
TABLE = os.path.join(_HERE, "data", "uo2_gb_energy.csv")

# angles of the table: dense where the phase field is fitted (0-30 deg),
# coarse above, up to the largest misorientation of a cubic crystal (62.8 deg)
ANGLES_DEG = (1.0, 2.5, 5.0, 7.5, 10.0, 12.5, 15.0, 17.5, 20.0, 22.5, 25.0,
              27.5, 30.0, 35.0, 40.0, 45.0, 50.0, 60.0)

FIT_ANGLES_DEG = (2.5, 5.0, 7.5, 10.0, 15.0, 20.0, 25.0, 30.0)


def _axis_rotation(axis, angle):
    """Rotation matrix of `angle` [rad] about `axis` (Rodrigues)."""
    n = np.asarray(axis, dtype=float)
    n = n / np.linalg.norm(n)
    k = np.array([[0.0, -n[2], n[1]], [n[2], 0.0, -n[0]], [-n[1], n[0], 0.0]])
    return np.eye(3) + math.sin(angle) * k + (1.0 - math.cos(angle)) * k @ k


def eRGB():
    """Energy of a 'random' boundary [J/m^2], 1.545 for UO2 in [Z21]."""
    return makeparvec(MATERIAL)[2]


# ---------------------------------------------------------------------------
# The three reductions
# ---------------------------------------------------------------------------

def random_gamma(angles_deg=ANGLES_DEG, samples=2000, seed=0):
    """gamma(psi) averaged over random misorientation axes and GB planes.

    GB5DOF fixes the boundary normal at [1, 0, 0] in the sample frame and
    takes the two grain orientations P and Q.  Drawing Q uniformly over SO(3)
    therefore samples the boundary plane uniformly, and drawing the
    misorientation axis uniformly gives P = R(n, psi) Q.

    Returns (mean, standard deviation) over the samples, both (n_angles,).
    """
    rng = np.random.default_rng(seed)
    mean = np.zeros(len(angles_deg))
    sd = np.zeros(len(angles_deg))
    for i, degrees in enumerate(angles_deg):
        psi = math.radians(degrees)
        energies = np.empty(samples)
        with contextlib.redirect_stdout(io.StringIO()):
            for k in range(samples):
                q = Rotation.random(random_state=int(rng.integers(2 ** 31))).as_matrix()
                p = _axis_rotation(rng.normal(size=3), psi) @ q
                energies[k] = GB5DOF(p, q, MATERIAL)
        mean[i], sd[i] = energies.mean(), energies.std()
        print(f"  psi = {degrees:5.1f} deg: gamma = {mean[i]:.3f} +- {sd[i]:.3f} J/m^2")
    return mean, sd


def tilt_gamma(angles_deg=ANGLES_DEG, axis="100"):
    """gamma(psi) of the SYMMETRIC TILT boundaries about <axis>.

    Cross section of the 5-DOF function at eta = 0 (symmetric) and
    phi = pi/2 (tilt), as in Zhang/GB_plot.py.
    """
    parameters, _, energy_rgb = makeparvec(MATERIAL)
    ksi = np.radians(np.asarray(angles_deg, dtype=float))
    geometry = np.vstack([np.zeros(ksi.size), ksi, np.zeros(ksi.size),
                          np.full(ksi.size, 0.5 * math.pi)])
    dimensionless = {"100": set100, "110": set110, "111": set111}[axis]
    return energy_rgb * dimensionless(geometry, parameters)


# ---------------------------------------------------------------------------
# Table
# ---------------------------------------------------------------------------

COLUMNS = ("angle_deg", "gamma_random", "sd_random", "gamma_st100", "gamma_st110")


def build_table(angles_deg=ANGLES_DEG, samples=2000, seed=0):
    """Compute the three curves; returns a dict of arrays keyed by COLUMNS."""
    print(f"UO2 gamma(Delta theta), eRGB = {eRGB():.3f} J/m^2, {samples} samples")
    mean, sd = random_gamma(angles_deg, samples, seed)
    return {"angle_deg": np.asarray(angles_deg, dtype=float),
            "gamma_random": mean, "sd_random": sd,
            "gamma_st100": tilt_gamma(angles_deg, "100"),
            "gamma_st110": tilt_gamma(angles_deg, "110")}


def write_table(table, path=TABLE):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", newline="") as stream:
        writer = csv.writer(stream)
        writer.writerow(COLUMNS)
        for row in zip(*(table[key] for key in COLUMNS)):
            writer.writerow([f"{value:.6g}" for value in row])
    print(f"wrote {path}")


def read_table(path=TABLE):
    with open(path) as stream:
        rows = list(csv.DictReader(stream))
    return {key: np.array([float(row[key]) for row in rows]) for key in COLUMNS}


def load_table(path=TABLE, refresh=False, samples=2000, seed=0):
    """The cached table, computed on first use (the sampling takes minutes)."""
    if refresh or not os.path.exists(path):
        table = build_table(ANGLES_DEG, samples, seed)
        write_table(table, path)
        return table
    return read_table(path)


def target(angles_deg=FIT_ANGLES_DEG, which="random", path=TABLE):
    """gamma [J/m^2] of the chosen reduction at `angles_deg`, interpolated.

    which: "random" (used by the phase field), "st100" or "st110".
    """
    table = load_table(path)
    key = {"random": "gamma_random", "st100": "gamma_st100",
           "st110": "gamma_st110"}[which]
    return np.interp(np.asarray(angles_deg, dtype=float),
                     table["angle_deg"], table[key])


def main(argv):
    refresh = "--refresh" in argv
    samples = 2000
    if "--samples" in argv:
        samples = int(argv[argv.index("--samples") + 1])
    table = load_table(refresh=refresh or not os.path.exists(TABLE), samples=samples)
    print()
    print(f"{'deg':>6} {'random':>8} {'sd':>7} {'ST<100>':>8} {'ST<110>':>8}")
    for row in zip(*(table[key] for key in COLUMNS)):
        print(f"{row[0]:6.1f} {row[1]:8.3f} {row[2]:7.3f} {row[3]:8.3f} {row[4]:8.3f}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
