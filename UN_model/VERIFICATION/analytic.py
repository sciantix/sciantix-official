"""Independent constant-coefficient spherical spectral reference via SciPy expm."""
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parent / 'dependencies'))
import numpy as np
from scipy.linalg import expm

VOLUME = 4 * np.pi / 3


def projections(n):
    k = np.arange(1, n + 1)
    return np.sqrt(8 / np.pi) * (-1.0) ** (k + 1) / k


def matrices(case, n):
    k = np.arange(1, n + 1)
    diffusion = case['D'] * (np.pi * k / case['R']) ** 2
    j = np.zeros((n, 3, 3))
    j[:, 0, 0] = -diffusion - case['gb'] - case['gd']
    j[:, 0, 1], j[:, 0, 2] = case['bb'], case['bd']
    j[:, 1, 0], j[:, 1, 1] = case['gb'], -case['bb']
    j[:, 2, 0], j[:, 2, 2] = case['gd'], -case['bd']
    return j


def solution(case, n, times, formulation='augmented'):
    # Scale concentrations to prevent a huge source column from dominating
    # expm's matrix norm. An augmented system avoids inverses of singular J.
    scale = max(max(np.abs(case['initial'])), max(np.abs(case['source'])) * max(times), 1.0)
    block = np.zeros((n, 4, 4))
    block[:, :3, :3] = matrices(case, n)
    block[:, :3, 3] = np.asarray(case['source']) / scale
    y0 = np.r_[np.asarray(case['initial']) / scale, 1.0]
    q = projections(n)
    modes = []
    for t in times:
        if formulation == 'equilibrium_shift':
            assert case['D'] > 0 and case['bb'] > 0 and case['bd'] > 0
            # Independent stationary balance plus expm(J*t) for the transient.
            # This avoids magnifying roundoff in an augmented zero eigenvalue
            # when t spans many decades. It is algebraically the same IVP.
            loss = case['D'] * (np.pi * np.arange(1, n + 1) / case['R']) ** 2
            eq = np.zeros((n, 3))
            eq[:, 0] = sum(case['source']) / scale / loss
            eq[:, 1] = (case['gb'] * eq[:, 0] + case['source'][1] / scale) / case['bb']
            eq[:, 2] = (case['gd'] * eq[:, 0] + case['source'][2] / scale) / case['bd']
            state = eq + np.einsum('nij,nj->ni', expm(block[:, :3, :3] * t), y0[:3] - eq)
            y = np.column_stack([state, np.ones(n)])
        else:
            y = np.broadcast_to(y0, (n, 4)) if t == 0 else (expm(block * t) @ y0)
        modes.append((y[:, :3] * (scale * q[:, None])).T)
    modes = np.asarray(modes)
    means = np.einsum('tpn,n->tp', modes, q / VOLUME)
    return means, modes


def zero_diffusion_continuum(case, times):
    assert case['D'] == 0
    finite, _ = solution(case, 1, times)
    return finite / (projections(1)[0] ** 2 / VOLUME)
