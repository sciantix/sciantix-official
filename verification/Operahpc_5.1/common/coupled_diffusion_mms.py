"""
Two-species spectral diffusion MMS kernel: intra-granular diffusion with a
directional production/decay coupling between species, verifying
Solver::SpectralDiffusion2equations-style coupling.

This is the multi-isotope situation SCIANTIX actually solves inside an
OFFBEAT coupling: a short-lived radioactive precursor (species 2, e.g.
133-Xe or 85m-Kr) diffuses and decays into a stable daughter (species 1,
e.g. Xe or Kr), which itself diffuses:

    dc1/dt = D1 div grad c1 + lambda*c2 + beta1
    dc2/dt = D2 div grad c2 - lambda*c2 + beta2

with lambda the precursor's decay constant. Manufactured solutions of the
same family used elsewhere in this suite, c_i,M(r,t) = (a^2-r^2) f_i(t),
give manufactured sources of the form (a^2-r^2)*G_i(t) + 6*H_i(t):

    beta1_M = (a^2-r^2)[f1'(t) - lambda*f2(t)]        + 6 D1(t) f1(t)
    beta2_M = (a^2-r^2)[f2'(t) + lambda*f2(t)]         + 6 D2(t) f2(t)

which reuses the single-species projection operator `modal_source` from
spectral_diffusion_mms.py unchanged (it only needs G_i(t) and H_i(t)).

Because the coupling is one-directional (2 -> 1 only), the per-mode implicit
backward-Euler system is lower-triangular and solves sequentially rather
than needing a 2x2 matrix solve per mode:

    X2_k[j] = (X2_k[j-1] + dt*S2_k) / (1 + dt*(lambda2_k + lambda))
    X1_k[j] = (X1_k[j-1] + dt*(S1_k + lambda*X2_k[j])) / (1 + dt*lambda1_k)
"""

import numpy as np

from spectral_diffusion_mms import modal_source


def run(f1, f1prime, D1, f2, f2prime, D2, lam, a, n_modes, dt, t_end):
    """
    Backward-Euler / spectral MMS solve for the coupled 2-species system.

    Returns t, C1_M, C1_N, C2_M, C2_N, X1_N, X2_N, k
    """
    t = np.arange(0.0, t_end + dt, dt)
    n_t = len(t)
    k = np.arange(1, n_modes + 1)

    X1_N = np.zeros((n_modes, n_t))
    X2_N = np.zeros((n_modes, n_t))
    C1_modes = np.zeros((n_modes, n_t))
    C2_modes = np.zeros((n_modes, n_t))

    one = lambda t: 1.0
    G1 = lambda t: f1prime(t) - lam * f2(t)
    H1 = lambda t: D1(t) * f1(t)
    G2 = lambda t: f2prime(t) + lam * f2(t)
    H2 = lambda t: D2(t) * f2(t)

    C1_M = 0.4 * a**2 * f1(t)
    C2_M = 0.4 * a**2 * f2(t)

    recon_coeff = (-np.sqrt(8.0 / np.pi) * (-1.0) ** k / k) / (np.pi * 4.0 / 3.0 * a**1.5)

    for j in range(1, n_t):
        tj = t[j]
        S1_k = modal_source(k, tj, one, G1, lambda tt: H1(tt), a)
        S2_k = modal_source(k, tj, one, G2, lambda tt: H2(tt), a)

        lambda1_k = D1(tj) * np.pi**2 * k**2 / a**2
        lambda2_k = D2(tj) * np.pi**2 * k**2 / a**2

        X2_N[:, j] = (X2_N[:, j - 1] + dt * S2_k) / (1.0 + dt * (lambda2_k + lam))
        X1_N[:, j] = (X1_N[:, j - 1] + dt * (S1_k + lam * X2_N[:, j])) / (1.0 + dt * lambda1_k)

        C1_modes[:, j] = X1_N[:, j] * recon_coeff
        C2_modes[:, j] = X2_N[:, j] * recon_coeff

    C1_N = np.sum(C1_modes, axis=0)
    C2_N = np.sum(C2_modes, axis=0)

    return t, C1_M, C1_N, C2_M, C2_N, X1_N, X2_N, k
