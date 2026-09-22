"""
MMS kernel for SCIANTIX's UO2 stoichiometry-deviation solvers: the
equilibrium root-find (Solver::NewtonBlackburn, src/models/
UO2Thermochemistry.C, Blackburn 1973) and the time evolution toward that
equilibrium (Solver::NewtonLangmuirBasedModel, src/models/
StoichiometryDeviation.C case 6, Massih 2018). Both are genuinely
OFFBEAT-exchanged: `Sciantix_variables[85]` <-> `oxygenMetalRatio_[cellI]`
(offbeat-official, offbeatLib/fissionGasRelease/fgrSCIANTIX.C:485,531).

1) NewtonBlackburn solves, for given temperature T and oxygen partial
   pressure p_O2, the equilibrium stoichiometry deviation x_eq from the
   Blackburn correlation:

       2*ln(x(x+2)/(1-x)) + 108*x^2 - 32700/T + 9.92 = ln(p_O2)

   an algebraic (not time-dependent) root-find. `blackburn_forward(x, T)`
   is the same correlation run forward (StoichiometryDeviation.C's
   `BlackburnThermochemicalModel`), used here to manufacture the p_O2 that
   makes a chosen x_eq exact.

2) NewtonLangmuirBasedModel advances "Stoichiometry deviation" x(t) via the
   backward-Euler solve of dx/dt = K*(1 - beta*exp(alpha*x))
   (StoichiometryDeviation.C:242, Massih 2018). Substituting y = beta*
   exp(alpha*x) turns this into the logistic equation dy/dt = alpha*K*y*(1-y),
   which has an exact closed form -- so, unlike most solvers in this suite,
   here the manufactured solution can be the *exact* solution of the same
   continuous ODE the ODE solver discretises (same idea as verifying
   backward Euler against dy/dt=-y), rather than one derived by adding an
   artificial source term.
"""

import numpy as np


def newton_blackburn(a, b, c_param, tol=1e-3, max_iter=50):
    """Faithful reproduction of Solver::NewtonBlackburn."""
    c = np.log(c_param)
    if a == 0.0:
        a = 1.0e-7
    x1 = a
    for it in range(max_iter):
        fun = 2.0 * np.log(a * (a + 2.0) / (1.0 - a)) + 108.0 * a**2 - 32700.0 / b + 9.92 - c
        deriv = 216.0 * a + 2.0 * (a**2 - 2.0 * a - 2.0) / ((a - 1.0) * a * (2.0 + a))
        x1 = a - fun / deriv
        a = x1
        if abs(fun) < tol:
            return x1, it + 1
    return x1, max_iter


def blackburn_forward(x, T):
    """StoichiometryDeviation.C's BlackburnThermochemicalModel: x,T -> p_O2 [atm]."""
    ln_p = 2.0 * np.log(x * (2.0 + x) / (1.0 - x)) + 108.0 * x**2 - 32700.0 / T + 9.92
    return np.exp(ln_p)


def newton_langmuir_step(x0, K, beta, alpha, dt, tol=1e-3, max_iter=50):
    """Faithful reproduction of Solver::NewtonLangmuirBasedModel (one step)."""
    x00 = x0
    x = x0
    x1 = x
    for it in range(max_iter):
        fun = x - x00 - K * dt + K * beta * np.exp(alpha * x) * dt
        deriv = 1.0 + K * beta * alpha * np.exp(alpha * x) * dt
        x1 = x - fun / deriv
        x = x1
        if abs(fun) < tol:
            return x1, it + 1
    return x1, max_iter


def x_exact_logistic(t, x0, K, beta, alpha):
    """Exact solution of dx/dt = K*(1-beta*exp(alpha*x)) via y=beta*exp(alpha*x)."""
    y0 = beta * np.exp(alpha * x0)
    r = alpha * K
    y = 1.0 / (1.0 + (1.0 / y0 - 1.0) * np.exp(-r * t))
    return np.log(y / beta) / alpha


def run_langmuir(x0, K, beta, alpha, dt, t_end):
    t = np.arange(0.0, t_end + dt, dt)
    x_N = np.zeros(len(t))
    x_N[0] = x0
    iters = np.zeros(len(t) - 1, dtype=int)
    for j in range(1, len(t)):
        x_N[j], iters[j - 1] = newton_langmuir_step(x_N[j - 1], K, beta, alpha, dt)
    x_M = x_exact_logistic(t, x0, K, beta, alpha)
    return t, x_M, x_N, iters


def error_norms(y_M, y_N):
    err = np.abs(np.asarray(y_M) - np.asarray(y_N))
    return {
        "L1": float(np.mean(err)),
        "L2": float(np.sqrt(np.mean(err**2))),
        "Linf": float(np.max(err)),
        "final": float(err[-1]),
    }
