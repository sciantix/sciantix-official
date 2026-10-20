#!/usr/bin/env python3
"""Independent quadrature checks of spherical basis, projection and averages."""
from pathlib import Path
import json
import numpy as np
from analytic import projections, VOLUME


def main():
    from scipy.special import roots_legendre
    x, w = roots_legendre(1024)
    x, w = (x + 1) / 2, w / 2
    indices = np.array([1, 2, 3, 10, 40, 80, 160])
    phi = indices[:, None] * np.pi / np.sqrt(2 * np.pi) * np.sinc(indices[:, None] * x)
    weights = 4 * np.pi * x**2 * w
    gram = (phi * weights) @ phi.T
    q = projections(160)[indices - 1]
    integral = phi @ weights
    orth = float(abs(gram - np.eye(len(indices))).max())
    projection = float(abs(integral - q).max())
    assert orth < 1e-11 and projection < 1e-11
    boundary = np.sin(indices * np.pi) / np.sqrt(2 * np.pi)
    assert abs(boundary).max() < 1e-12
    rows = []
    for n in [10, 20, 40, 80, 160]:
        fraction = float(np.sum(projections(n)**2) / VOLUME)
        assert 0 < fraction < 1
        # Twenty corrections in production modeInitialization sum a geometric
        # series; the projection used by the analytic benchmark is unadjusted.
        rows.append({'n': n, 'uniform_mean_fraction': fraction,
                     'uniform_initialization_rescaling': (1 - (1 - fraction)**20) / fraction})
    result = {'status': 'pass', 'orthogonality_max_abs_error': orth, 'projection_max_abs_error': projection,
              'boundary_max_abs_error': float(abs(boundary).max()),
              'basis': 'phi_n(x)=sin(n*pi*x)/(sqrt(2*pi)*x), phi_n(0)=n*pi/sqrt(2*pi)',
              'measure': '4*pi*x^2 dx, x=r/R; <phi_n,phi_m>=delta_nm',
              'source_projection': 'q_n=sqrt(8/pi)*(-1)^(n+1)/n',
              'average_weights': 'q_n/(4*pi/3)',
              'center': 'phi_n is even and analytic near x=0, so derivative at center is zero',
              'nonmobile_boundary': 'No separate diffusive boundary condition for mb/md; same L2 basis is used.',
              'projection_fractions': rows}
    out = Path(__file__).resolve().parent / 'basis_checks.json'
    out.write_text(json.dumps(result, indent=2) + '\n')
    print('PASS spherical normalization/projection/boundary:', orth, projection)


if __name__ == '__main__':
    main()
