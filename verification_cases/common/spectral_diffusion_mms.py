"""
Reusable MMS kernel for SCIANTIX's intra-granular spectral diffusion solver
(PolyPole-2, cf. Solver::SpectralDiffusion in src/classes/Solver.C).

This module implements the method of manufactured solutions (MMS) described in
OperaHPC WP5-D5.1, Section 4.3 "Focus on OFFBEAT-SCIANTIX scheme":

    dc/dt = D(T,Fdot) * (1/r^2) d/dr( r^2 dc/dr )  +  beta        (Eq. 10)

for the family of manufactured solutions

    c_M(r,t) = (a^2 - r^2) * f(t) ,      D_M(t) = D(t)            (generalises Eq. 11-12)

which satisfies the homogeneous Dirichlet condition c_M(a,t) = 0 imposed at the
grain boundary. For this family the manufactured source term is

    beta_M(r,t) = (a^2 - r^2) f'(t)  +  6 D(t) f(t)                (generalises Eq. 13)

Projecting beta_M onto the solver's spherical eigenbasis

    psi_k(r) = sin(k*pi*r/a) / ( r * sqrt(2*pi*a) )

(as used in ../MMS_Solver/source_projection.py, and reproduced here so every
case shares one audited implementation) gives the modal source

    S_k(t) = 2*sqrt(2*pi/a) * [ -6*(-1)^k*a^4*f'(t)/(pi^3*k^3) - 6*(-1)^k*a^2*D(t)*f(t)/(pi*k) ]

which generalises the projection reported in Eq. (14). Each mode obeys the ODE
dX_k/dt = -lambda_k(t) X_k + S_k(t), lambda_k(t) = D(t) pi^2 k^2 / a^2, advanced
with the same first-order backward-Euler scheme as the C++ solver.

This kernel is shared by case_01 (report's representative sinusoidal case),
case_02 (the exponential case wired into Solver.C/GasDiffusion.C) and case_03
(the convergence study), so the projection/eigenbasis math is verified once
and reused, instead of being re-derived per case.
"""

import numpy as np


def modal_source(k, t, f, fprime, D, a):
    """Projected source S_k(t) for c_M = (a^2 - r^2) f(t), D_M = D(t)."""
    sign = (-1.0) ** k
    term_time = -6.0 * sign * a**4 * fprime(t) / (np.pi**3 * k**3)
    term_diff = -6.0 * sign * a**2 * D(t) * f(t) / (np.pi * k)
    return 2.0 * np.sqrt(2.0 * np.pi / a) * (term_time + term_diff)


def psi(r, k, a):
    """Eigenfunction of the spherical Laplacian used by the spectral solver."""
    r = np.asarray(r, dtype=float)
    out = np.empty_like(r)
    small = r < 1e-12 * a
    out[~small] = np.sin(k * np.pi * r[~small] / a) / (r[~small] * np.sqrt(2.0 * np.pi * a))
    out[small] = k * np.pi / (a * np.sqrt(2.0 * np.pi * a))  # r -> 0 limit
    return out


def run(f, fprime, D, a, n_modes, dt, t_end, c0=0.0):
    """
    Backward-Euler / spectral MMS solve for c_M(r,t) = (a^2 - r^2) f(t), D_M = D(t).

    Returns
    -------
    t        : (n_t,) time array
    C_M      : (n_t,) manufactured volume-averaged concentration, 0.4*a^2*f(t)
    C_N      : (n_t,) numerical volume-averaged concentration (sum over modes)
    X_N      : (n_modes, n_t) modal coefficients (for radial-profile reconstruction)
    k        : (n_modes,) mode indices
    """
    t = np.arange(0.0, t_end + dt, dt)
    n_t = len(t)
    k = np.arange(1, n_modes + 1)

    X_N = np.zeros((n_modes, n_t))
    C_N_modes = np.zeros((n_modes, n_t))

    C_M = 0.4 * a**2 * f(t) + c0

    for j in range(1, n_t):
        S_k = modal_source(k, t[j], f, fprime, D, a)
        lamda = D(t[j]) * np.pi**2 * k**2 / a**2
        X_N[:, j] = (X_N[:, j - 1] + dt * S_k) / (1.0 + dt * lamda)
        C_N_modes[:, j] = (
            X_N[:, j] * (-np.sqrt(8.0 / np.pi) * (-1.0) ** k / k) / (np.pi * 4.0 / 3.0 * a**1.5)
        )

    C_N = np.sum(C_N_modes, axis=0) + c0
    return t, C_M, C_N, X_N, k


def reconstruct_profile(X_N, k, a, r, j):
    """Radial concentration profile c_N(r) at time index j from modal coefficients."""
    r = np.atleast_1d(np.asarray(r, dtype=float))
    profile = np.zeros_like(r)
    for i, ki in enumerate(k):
        profile += X_N[i, j] * psi(r, ki, a)
    return profile


def single_step(X_prev, k, t_next, dt, f, fprime, D, a, c0=0.0):
    """
    One backward-Euler modal update, X_prev (at t_next-dt) -> (X_new, C_new)
    at t_next -- the same per-step math as the loop body in `run(...)`,
    exposed standalone so a caller can drive it with a *different* D estimate
    on each call while starting from the *same* X_prev. This mirrors how
    OFFBEAT's `fgrSCIANTIX::correct()` re-solves one time step's SCIANTIX
    advance from the same previous-time-step state at every outer iteration,
    each time with an updated temperature (hence D) estimate
    (gitlab.com/AlessandroScolaro/offbeat-official,
    offbeatLib/fissionGasRelease/fgrSCIANTIX.C:606-658).
    """
    S_k = modal_source(k, t_next, f, fprime, D, a)
    lamda = D(t_next) * np.pi**2 * k**2 / a**2
    X_new = (X_prev + dt * S_k) / (1.0 + dt * lamda)
    C_new = np.sum(X_new * (-np.sqrt(8.0 / np.pi) * (-1.0) ** k / k) / (np.pi * 4.0 / 3.0 * a**1.5)) + c0
    return X_new, C_new


def truncate_precision(x, digits):
    """
    Round every element of `x` to `digits` significant decimal figures.

    Models the ASCII state hand-off SCIANTIX actually uses when driven by an
    external code (OFFBEAT): sciantix_variable fields are written to
    `output.txt` with `std::setprecision(7)`, history_variable fields with
    `std::setprecision(10)` (src/file_manager/Output.C:60-97), and read back
    in from `input_initial_conditions.txt` at the next call
    (src/file_manager/InputReading.C:115-149). `digits=None` (handled by the
    caller) skips this and keeps full double precision.
    """
    x = np.asarray(x, dtype=float)
    out = np.array(x, copy=True)
    nz = x != 0.0
    exponent = np.floor(np.log10(np.abs(x[nz])))
    factor = 10.0 ** (digits - 1 - exponent)
    out[nz] = np.round(x[nz] * factor) / factor
    return out


def run_with_restart(f, fprime, D, a, n_modes, dt, steps_per_call, t_end, digits=None, c0=0.0):
    """
    Same backward-Euler / spectral update as `run(...)`, but the modal state
    X_k is only held in memory for `steps_per_call` consecutive micro steps
    of size `dt` at a time ("one driver call"); at every call boundary the
    state is round-tripped through `truncate_precision(..., digits)` before
    continuing, reproducing SCIANTIX being re-invoked by an external driver
    (OFFBEAT) rather than integrated once, continuously, in-process as in
    `run(...)`. `digits=None` disables truncation, isolating the cost of
    *restarting at all* (which should reproduce `run(...)` exactly, since the
    update is Markovian in X_k) from the cost of *finite-precision* restart
    I/O.

    Returns the same tuple as `run(...)`.
    """
    t = np.arange(0.0, t_end + dt, dt)
    n_t = len(t)
    k = np.arange(1, n_modes + 1)

    X_N = np.zeros((n_modes, n_t))
    C_N_modes = np.zeros((n_modes, n_t))
    C_M = 0.4 * a**2 * f(t) + c0

    for j in range(1, n_t):
        S_k = modal_source(k, t[j], f, fprime, D, a)
        lamda = D(t[j]) * np.pi**2 * k**2 / a**2
        X_N[:, j] = (X_N[:, j - 1] + dt * S_k) / (1.0 + dt * lamda)

        if digits is not None and j % steps_per_call == 0:
            X_N[:, j] = truncate_precision(X_N[:, j], digits)

        C_N_modes[:, j] = (
            X_N[:, j] * (-np.sqrt(8.0 / np.pi) * (-1.0) ** k / k) / (np.pi * 4.0 / 3.0 * a**1.5)
        )

    C_N = np.sum(C_N_modes, axis=0) + c0
    return t, C_M, C_N, X_N, k


def error_norms(C_M, C_N):
    """L1 (average), L2 (RMSE), Linf (max) and final-value absolute error."""
    err = np.abs(C_M - C_N)
    return {
        "L1": float(np.mean(err)),
        "L2": float(np.sqrt(np.mean(err**2))),
        "Linf": float(np.max(err)),
        "final": float(err[-1]),
    }


def order_of_convergence(error_coarse, error_fine, refinement_ratio=2.0):
    """Observed order of accuracy between two refinements of a step size h."""
    return np.log(error_coarse / error_fine) / np.log(refinement_ratio)
