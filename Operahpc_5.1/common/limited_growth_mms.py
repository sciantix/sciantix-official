"""
MMS kernel for SCIANTIX's grain-boundary bubble growth solver
(Solver::LimitedGrowth, src/classes/Solver.C:22-25), specifically its use in
the intergranular bubble behaviour model (Pastore et al., 2013) where the
manufactured driver is the hydrostatic stress OFFBEAT passes in.

Solver::LimitedGrowth closes the implicit (backward-Euler) update of

    dy/dt = a/y + b(t)                                              (*)

in closed form via the quadratic formula:

    y_new = 0.5 * [ (y_old + b*dt) + sqrt( (y_old + b*dt)^2 + 4*a*dt ) ]

This is exactly the model used for "Intergranular vacancies per bubble"
in src/models/InterGranularBubbleBehavior.C:136-143, with parameters

    a = growth_rate       = volume_flow_rate * n_atoms_per_bubble / V_Schottky
    b(t) = equilibrium_term = -volume_flow_rate * equilibrium_pressure(t)
                               / (k_B * T)
    equilibrium_pressure(t) = 2*surface_tension/r_bubble - sigma_H(t)*1e6

(InterGranularBubbleBehavior.C:103-114), where sigma_H(t) [MPa] is the
hydrostatic stress OFFBEAT passes to SCIANTIX each call
(offbeat-official, offbeatLib/fissionGasRelease/fgrSCIANTIX.C:614-619,
652-653: `Hydrostaticstress_input`). Both `equilibrium_pressure`/
`equilibrium_term` are evaluated from the *previous* time step's bubble
radius (`getInitialValue()`), so within one step b(t) is a function of time
only (through the prescribed/old r, T and the new sigma_H) -- exactly the
same structure already verified for D(t) in `spectral_diffusion_mms.py`.

For the manufactured family y_M(t) = y0 * (1 + eps*sin(omega*t)) (kept
positive by construction, eps<1), the needed source is derived from (*):

    b_M(t) = dy_M/dt - a / y_M(t)

which is then inverted back through the equilibrium-pressure/term algebra
to the manufactured hydrostatic stress sigma_H_M(t) that would produce it
-- i.e. this case manufactures the *coupling variable itself* (stress),
not an abstract source term.
"""

import numpy as np


def y_M(t, y0, eps, omega):
    return y0 * (1.0 + eps * np.sin(omega * t))


def dyM_dt(t, y0, eps, omega):
    return y0 * eps * omega * np.cos(omega * t)


def manufactured_source(t, a, y0, eps, omega):
    """b_M(t) such that dy_M/dt = a/y_M(t) + b_M(t) holds exactly."""
    return dyM_dt(t, y0, eps, omega) - a / y_M(t, y0, eps, omega)


def equilibrium_term_to_stress(b, surface_tension, r_bubble, kB, T, volume_flow_rate):
    """
    Invert b = equilibrium_term = -volume_flow_rate*(2*gamma/r - sigma_H*1e6)/(kB*T)
    for sigma_H [MPa] (InterGranularBubbleBehavior.C:112-114).
    """
    equilibrium_pressure = -b * kB * T / volume_flow_rate
    return (2.0 * surface_tension / r_bubble - equilibrium_pressure) / 1e6


def stress_to_equilibrium_term(sigma_H, surface_tension, r_bubble, kB, T, volume_flow_rate):
    """Forward direction: sigma_H [MPa] -> b (equilibrium_term), as SCIANTIX computes it."""
    equilibrium_pressure = 2.0 * surface_tension / r_bubble - sigma_H * 1e6
    return -volume_flow_rate * equilibrium_pressure / (kB * T)


def limited_growth_step(y_old, a, b, dt):
    """Solver::LimitedGrowth's closed-form backward-Euler update."""
    rhs = y_old + b * dt
    return 0.5 * (rhs + np.sqrt(rhs**2 + 4.0 * a * dt))


def run(a, y0, eps, omega, dt, t_end, surface_tension, r_bubble, kB, T, volume_flow_rate, y_init=None):
    """
    Full MMS run: manufacture sigma_H_M(t), round-trip it through SCIANTIX's
    own equilibrium_term formula, and advance y with `limited_growth_step`.

    Returns t, Y_M, Y_N, sigma_H_M.
    """
    t = np.arange(0.0, t_end + dt, dt)
    n_t = len(t)

    b_M = manufactured_source(t, a, y0, eps, omega)
    sigma_H_M = equilibrium_term_to_stress(b_M, surface_tension, r_bubble, kB, T, volume_flow_rate)

    Y_N = np.zeros(n_t)
    Y_N[0] = y_init if y_init is not None else y_M(0.0, y0, eps, omega)
    for j in range(1, n_t):
        b_j = stress_to_equilibrium_term(sigma_H_M[j], surface_tension, r_bubble, kB, T, volume_flow_rate)
        Y_N[j] = limited_growth_step(Y_N[j - 1], a, b_j, dt)

    Y_M = y_M(t, y0, eps, omega)
    return t, Y_M, Y_N, sigma_H_M


def error_norms(Y_M, Y_N):
    err = np.abs(Y_M - Y_N)
    return {
        "L1": float(np.mean(err)),
        "L2": float(np.sqrt(np.mean(err**2))),
        "Linf": float(np.max(err)),
        "final": float(err[-1]),
    }
