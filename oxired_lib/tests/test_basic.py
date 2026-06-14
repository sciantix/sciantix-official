from __future__ import annotations

import pytest

from oxired import CylinderGeometry, OxiRedCylinder, PolynomialProfile, fission_yield_for_element


def test_hypo_steady_state_conserves_average() -> None:
    geom = CylinderGeometry(r_outer=3e-3)
    profile = PolynomialProfile(0.0, geom.r_outer, 2600.0, 1240.0)
    solver = OxiRedCylinder(geom, profile, pu_fraction=0.2, n_cells=120)

    result = solver.solve_steady_state(average_om=1.965)

    assert abs(result.average_om - 1.965) < 5e-6
    assert result.om[0] < result.om[-1]


def test_invalid_geometry_is_rejected() -> None:
    with pytest.raises(ValueError, match="r_outer"):
        CylinderGeometry(r_outer=1e-3, r_inner=2e-3)
    with pytest.raises(ValueError, match="r_inner"):
        CylinderGeometry(r_outer=1e-3, r_inner=-1e-3)


def test_invalid_polynomial_profile_is_rejected() -> None:
    with pytest.raises(ValueError, match="r_outer"):
        PolynomialProfile(1.0, 1.0, 2000.0, 1000.0)
    with pytest.raises(ValueError, match="temperatures"):
        PolynomialProfile(0.0, 1.0, 0.0, 1000.0)
    with pytest.raises(ValueError, match="power"):
        PolynomialProfile(0.0, 1.0, 2000.0, 1000.0, power=0.0)


def test_only_hypostoichiometric_mode_is_supported() -> None:
    geom = CylinderGeometry(r_outer=3e-3)
    profile = PolynomialProfile(0.0, geom.r_outer, 2600.0, 1240.0)
    solver = OxiRedCylinder(geom, profile, pu_fraction=0.2, n_cells=40)

    with pytest.raises(ValueError, match="only hypostoichiometric"):
        solver.solve_steady_state(average_om=1.965, mode="hyper")  # type: ignore[arg-type]
    with pytest.raises(ValueError, match="average_om <= 2"):
        solver.solve_steady_state(average_om=2.01)


def test_fission_yield_lookup_supports_model_sinks() -> None:
    ba = fission_yield_for_element("Ba")
    mo = fission_yield_for_element("Mo")
    sr = fission_yield_for_element("Sr")
    y = fission_yield_for_element("Y")
    am_cm = fission_yield_for_element("Am+Cm")
    np = fission_yield_for_element("Np")

    assert ba.yield_percent_fp_per_fission == 9.9
    assert mo.yield_percent_fp_per_fission == 21.9
    assert sr.valence == 2
    assert y.valence == 3
    assert am_cm.yield_percent_fp_per_fission == 5.5
    assert am_cm.valence == 4
    assert np.yield_percent_fp_per_fission == 0.4
    assert np.valence == 4
