"""
MMS kernel for SCIANTIX's generic quartic-equation Newton solver
(Solver::QuarticEquation, src/classes/Solver.C:346-376), used by grain
growth (Ainscough model, src/models/GrainGrowth.C:59-70,138-139) to advance
"Grain radius" via the quadratic special case

    y^2 - R_old*y - K*dt = 0        (a=0, b=0, c=1, d=-R_old, e=-K*dt)

where K is the (temperature- and burnup-dependent) grain-boundary mobility
rate constant (src/classes/Matrix.C:30-33, Ainscough et al., 1973) and dt is
the physics time step in seconds.

`Solver::QuarticEquation` is a fixed-point Newton iteration:

    while iter < max_iter (=5):
        function  = a*y^4 + b*y^3 + c*y^2 + d*y + e      # residual at y0
        derivative = 4a*y^3 + 3b*y^2 + 2c*y + d
        y1 = y0 - function/derivative
        y0 = y1
        if function < tol (=1e-3):   # <-- checked BEFORE this step, SIGNED
            return y1
    return y1

`quartic_step_shipped` reproduces this exactly, including the signed,
un-normalised tolerance check. `quartic_step_fixed` is the same iteration
with the check replaced by `abs(function) < tol`, kept for comparison only
-- it is not what src/classes/Solver.C actually does.

For the grain-growth equation, `function`'s natural magnitude is a
length-squared quantity (~1e-9 to 1e-16 m^2 for realistic grain radii and
mobility*dt products), always far below the fixed tol=1e-3 the C++ carries.
Since the check is evaluated on the residual *before* the just-taken step
rather than after it, and that pre-step residual is essentially always
below 1e-3 regardless of how large the step itself was, both the signed and
the abs() form of the check trigger after exactly one Newton step for this
equation -- `max_iter=5` is never actually used. One Newton step from
y0=R_old is an excellent approximation when K*dt is small (the common case
for short time steps), but degrades badly once K*dt grows -- exactly what a
coarser macro time step (as OFFBEAT uses; see case 08/09) or a hotter,
faster-growing grain would produce. This module lets the case quantify that.
"""

import numpy as np


def quartic_step(y0, a, b, c, d, e, tol=1e-3, max_iter=5, use_abs=False):
    """
    One call to (a faithful reproduction of) Solver::QuarticEquation.
    Returns (root, iterations_taken).
    """
    y = y0
    function = None
    for it in range(max_iter):
        function = a * y**4 + b * y**3 + c * y**2 + d * y + e
        derivative = 4.0 * a * y**3 + 3.0 * b * y**2 + 2.0 * c * y + d
        y1 = y - function / derivative
        y = y1
        converged = abs(function) < tol if use_abs else (function < tol)
        if converged:
            return y1, it + 1
    return y, max_iter


def newton_to_full_convergence(y0, a, b, c, d, e, n_iter=50):
    """Reference: run the same Newton recursion for many more iterations,
    with no early exit -- the value it settles on is the 'true' root this
    equation's own Newton map converges to."""
    y = y0
    for _ in range(n_iter):
        f = a * y**4 + b * y**3 + c * y**2 + d * y + e
        fp = 4.0 * a * y**3 + 3.0 * b * y**2 + 2.0 * c * y + d
        y = y - f / fp
    return y


def mobility_ainscough(T, A=1.455e-8, Q_over_R=32114.5):
    """Grain-boundary mobility, Ainscough et al. 1973 (Matrix.C:32)."""
    return A * np.exp(-Q_over_R / T)


def R_M(t, R0, dR, tau):
    """Manufactured, saturating grain-growth trajectory (Ainscough-like)."""
    return R0 + dR * (1.0 - np.exp(-t / tau))


def dRM_dt(t, dR, tau):
    return dR / tau * np.exp(-t / tau)


def run_grain_growth(R0, dR, tau, K, dt, t_end, use_abs=False, max_iter=5, tol=1e-3):
    """
    Advance the shipped (or abs-corrected) quartic-equation solver, step by
    step, using the exact source K*dt needed at each step to drive the
    discrete quadratic update toward the manufactured trajectory R_M(t).

    Returns t, R_M_traj, R_N_traj (numerical, propagated from its own
    previous output as SCIANTIX does), iters_per_step.
    """
    t = np.arange(0.0, t_end + dt, dt)
    n_t = len(t)
    R_M_traj = R_M(t, R0, dR, tau)

    R_N = np.zeros(n_t)
    R_N[0] = R_M_traj[0]
    iters = np.zeros(n_t - 1, dtype=int)

    for j in range(1, n_t):
        R_old = R_N[j - 1]
        target = R_M_traj[j]
        Kdt = target**2 - R_M_traj[j - 1] * target  # exact source for THIS quadratic form
        root, it = quartic_step(R_old, 0.0, 0.0, 1.0, -R_old, -Kdt, tol=tol, max_iter=max_iter, use_abs=use_abs)
        R_N[j] = root
        iters[j - 1] = it

    return t, R_M_traj, R_N, iters


def error_norms(y_M, y_N):
    err = np.abs(np.asarray(y_M) - np.asarray(y_N))
    return {
        "L1": float(np.mean(err)),
        "L2": float(np.sqrt(np.mean(err**2))),
        "Linf": float(np.max(err)),
        "final": float(err[-1]),
    }
