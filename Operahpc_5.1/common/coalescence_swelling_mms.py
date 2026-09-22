"""
MMS kernel for grain-boundary bubble coalescence (Solver::BinaryInteraction,
src/classes/Solver.C) and the downstream algebra that turns bubble
concentration + volume into the intergranular gaseous swelling OFFBEAT
consumes as an eigenstrain (InterGranularBubbleBehavior.C:145-296).

Solver::BinaryInteraction(y_old, k, increment) returns

    y_new = y_old / (1 + k*y_old*increment)

which is the *exact* (not just first-order) solution of the separable ODE
dy/dx = -k*y^2 for a constant k over one step of size `increment` in x --
here x is bubble area A, not time, since InterGranularBubbleBehavior.C:168
calls it as `BinaryInteraction(N_initial, 2.0, dbubble_area)` with
`dbubble_area` the step's increment in "Intergranular bubble area"
(coalescence rate proportional to N^2 per unit area swept, coefficient
k=2.0 fixed in the source). Because the recursion is the exact solution of
its own governing ODE (not a Taylor/backward-Euler approximation of it),
verifying it against a manufactured N_M(A) should show error at the level
of floating-point round-off *regardless of step size* -- a different,
notable result next to every other solver in this suite (cases 01, 03, 04,
10, 11), which all show genuine O(dt) or O(dt^2) truncation error.

This module also reproduces the exact downstream algebra
(InterGranularBubbleBehavior.C:151-165,294-296) that turns bubble
concentration N and volume V into radius, area and swelling:

    V = n_Xe*VdW_Xe + n_vacancies*V_Schottky
    R = 0.620350491 * (V / lenticular_shape_factor)^(1/3)
    A = pi * (R * sin(semidihedral_angle))^2
    swelling = (3 / a_grain) * N * V
"""

import numpy as np

VDW_XE = 8.48e-29                 # m^3 (src/operations/SetGas.C:33)
SCHOTTKY_VOLUME = 4.09e-29        # m^3 (src/operations/SetMatrix.C:55)
LENTICULAR_SHAPE_FACTOR = 0.168610764  # (SetMatrix.C:59)
SEMIDIHEDRAL_ANGLE = 0.872664626  # rad (SetMatrix.C:57)
INTERACTION_COEFFICIENT = 2.0     # fixed in InterGranularBubbleBehavior.C:169


def binary_interaction_step(y_old, k, increment):
    """Solver::BinaryInteraction, exact closed form."""
    return y_old / (1.0 + k * y_old * increment)


def volume(n_Xe, n_vacancies):
    return n_Xe * VDW_XE + n_vacancies * SCHOTTKY_VOLUME


def radius(V):
    return 0.620350491 * (V / LENTICULAR_SHAPE_FACTOR) ** (1.0 / 3.0)


def area(R):
    return np.pi * (R * np.sin(SEMIDIHEDRAL_ANGLE)) ** 2


def swelling(N, V, a_grain):
    return 3.0 / a_grain * N * V


def saturating(t, x0, dx, tau):
    """Representative saturating growth curve, shared shape for the
    manufactured vacancies-per-bubble and Xe-atoms-per-bubble inputs."""
    return x0 + dx * (1.0 - np.exp(-t / tau))


def run(t, y_M, n_Xe_M, N0, a_grain, k=INTERACTION_COEFFICIENT, coalescence_step=binary_interaction_step):
    """
    Manufacture the full chain from prescribed y_M(t) [vacancies/bubble] and
    n_Xe_M(t) [Xe atoms/bubble]: volume/radius/area (exact algebra) and
    concentration N_M(t) (the ODE solution driven by the resulting area
    trajectory), then re-derive concentration numerically step by step with
    `coalescence_step` (default: the real Solver::BinaryInteraction) using
    only the per-step area increment, and recompute swelling both ways.

    Returns a dict of all manufactured and numerical trajectories.
    """
    V_M = volume(n_Xe_M, y_M)
    R_M = radius(V_M)
    A_M = area(R_M)

    N_M = 1.0 / (1.0 / N0 + k * (A_M - A_M[0]))
    swelling_M = swelling(N_M, V_M, a_grain)

    N_N = np.zeros_like(t)
    N_N[0] = N_M[0]
    for j in range(1, len(t)):
        dA = A_M[j] - A_M[j - 1]
        N_N[j] = coalescence_step(N_N[j - 1], k, dA)

    V_N = V_M  # inputs y_M, n_Xe_M treated as exact (validated elsewhere, e.g. case 10)
    swelling_N = swelling(N_N, V_N, a_grain)

    return {
        "V_M": V_M, "R_M": R_M, "A_M": A_M, "N_M": N_M, "swelling_M": swelling_M,
        "N_N": N_N, "swelling_N": swelling_N,
    }


def error_norms(y_M, y_N):
    err = np.abs(np.asarray(y_M) - np.asarray(y_N))
    return {
        "L1": float(np.mean(err)),
        "L2": float(np.sqrt(np.mean(err**2))),
        "Linf": float(np.max(err)),
        "final": float(err[-1]),
    }
