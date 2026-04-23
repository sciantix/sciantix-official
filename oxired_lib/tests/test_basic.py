from __future__ import annotations

import numpy as np

from oxired import CylinderGeometry, OxiRedCylinder, PolynomialProfile


def test_hypo_steady_state_conserves_average() -> None:
    geom = CylinderGeometry(r_outer=3e-3)
    profile = PolynomialProfile(0.0, geom.r_outer, 2600.0, 1240.0)
    solver = OxiRedCylinder(geom, profile, pu_fraction=0.2, n_cells=120)
    result = solver.solve_steady_state(average_om=1.965)
    assert abs(result.average_om - 1.965) < 5e-6
    assert result.om[0] < result.om[-1]  # oxygen migrates outward in hypo fuel


def test_hyper_steady_state_conserves_average() -> None:
    geom = CylinderGeometry(r_outer=4e-3)
    profile = PolynomialProfile(0.0, geom.r_outer, 2200.0, 1200.0)
    solver = OxiRedCylinder(geom, profile, pu_fraction=0.1, n_cells=120)
    result = solver.solve_steady_state(average_om=2.01)
    assert abs(result.average_om - 2.01) < 5e-6
    assert result.om[0] > result.om[-1]  # oxygen migrates inward in hyper fuel


def test_transient_moves_toward_steady_state() -> None:
    geom = CylinderGeometry(r_outer=4e-3)
    profile = PolynomialProfile(0.0, geom.r_outer, 2600.0, 1240.0)
    solver = OxiRedCylinder(geom, profile, pu_fraction=0.2, n_cells=120)
    steady = solver.solve_steady_state(average_om=1.965)
    transient = solver.solve_transient(initial_om_profile=1.965, target_average_om=1.965, dt=1200.0)
    assert transient.om[0] <= 1.965 + 1e-12
    assert transient.om[-1] >= 1.965 - 1e-12
    assert transient.tau_seconds is not None and transient.tau_seconds > 0
    assert np.any(np.abs(transient.om - steady.om) > 0)
