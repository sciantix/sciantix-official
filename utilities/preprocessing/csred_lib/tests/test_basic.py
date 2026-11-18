from __future__ import annotations

import numpy as np
import pytest

from csred import (
    CsRedCylinder,
    CylinderGeometry,
    PolynomialProfile,
    area_average,
)


def test_flat_production_conserves_inventory_and_enriches_cold_rim() -> None:
    geom = CylinderGeometry(r_outer=3e-3)
    profile = PolynomialProfile(0.0, geom.r_outer, 2200.0, 900.0)
    solver = CsRedCylinder(
        geom,
        profile,
        n_cells=8,
    )
    edges, _ = solver.mesh()

    time = np.linspace(0.0, 200.0 * 3600.0, 5)
    produced = np.tile(np.linspace(0.0, 1.0, len(time))[:, np.newaxis], (1, solver.n_cells))

    result = solver.solve_history(time, produced)

    assert area_average(edges, result.redistributed) == pytest.approx(area_average(edges, produced[-1]))
    assert area_average(edges, result.scaling_factor * produced[-1]) == pytest.approx(
        area_average(edges, produced[-1])
    )
    assert result.scaling_factor[-1] > result.scaling_factor[0]


def test_zero_heat_of_transport_keeps_flat_production_flat() -> None:
    geom = CylinderGeometry(r_outer=3e-3)
    profile = PolynomialProfile(0.0, geom.r_outer, 2200.0, 900.0)
    solver = CsRedCylinder(
        geom,
        profile,
        n_cells=8,
        heat_of_transport=0.0,
    )

    time = np.linspace(0.0, 200.0 * 3600.0, 5)
    produced = np.tile(np.linspace(0.0, 1.0, len(time))[:, np.newaxis], (1, solver.n_cells))

    result = solver.solve_history(time, produced)

    assert np.allclose(result.scaling_factor, 1.0)


def test_zero_production_returns_neutral_scaling() -> None:
    geom = CylinderGeometry(r_outer=3e-3)
    profile = PolynomialProfile(0.0, geom.r_outer, 2200.0, 900.0)
    solver = CsRedCylinder(geom, profile, n_cells=5)

    time = np.array([0.0, 10.0])
    produced = np.zeros((2, solver.n_cells))

    result = solver.solve_history(time, produced)

    assert np.all(result.scaling_factor == 1.0)


def test_equilibrium_scaling_conserves_area_average() -> None:
    geom = CylinderGeometry(r_outer=3e-3)
    profile = PolynomialProfile(0.0, geom.r_outer, 2200.0, 900.0)
    solver = CsRedCylinder(geom, profile, n_cells=8)
    edges, _ = solver.mesh()

    produced = np.ones(solver.n_cells)
    scaling = solver.equilibrium_scaling_factor(produced)

    assert area_average(edges, scaling) == pytest.approx(1.0)
    assert scaling[-1] > scaling[0]


def test_invalid_inputs_are_rejected() -> None:
    with pytest.raises(ValueError, match="r_outer"):
        CylinderGeometry(r_outer=1e-3, r_inner=2e-3)
    with pytest.raises(ValueError, match="temperatures"):
        PolynomialProfile(0.0, 1.0, 0.0, 1000.0)

    geom = CylinderGeometry(r_outer=3e-3)
    profile = PolynomialProfile(0.0, geom.r_outer, 2200.0, 900.0)
    solver = CsRedCylinder(geom, profile, n_cells=5)
    with pytest.raises(ValueError, match="shape"):
        solver.solve_history(np.array([0.0, 1.0]), np.ones((2, 4)))
