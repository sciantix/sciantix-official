from __future__ import annotations

import numpy as np
import pytest

from oxired import (
    CylinderGeometry,
    OxiRedCylinder,
    OxygenBalanceModel,
    OxygenBalanceModel_PHENIX,
    PolynomialProfile,
)


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


def test_invalid_modes_are_rejected() -> None:
    geom = CylinderGeometry(r_outer=3e-3)
    profile = PolynomialProfile(0.0, geom.r_outer, 2600.0, 1240.0)
    solver = OxiRedCylinder(geom, profile, pu_fraction=0.2, n_cells=40)

    with pytest.raises(ValueError, match="mode"):
        solver.solve_steady_state(average_om=1.965, mode="bad")  # type: ignore[arg-type]
    with pytest.raises(ValueError, match="hyper"):
        solver.solve_steady_state(average_om=1.965, mode="hyper")
    with pytest.raises(ValueError, match="hypo"):
        solver.solve_steady_state(average_om=2.01, mode="hypo")


def test_oxygen_balance_quantities_scale_with_burnup() -> None:
    balance = OxygenBalanceModel()
    result = balance.target_average_om(
        initial_average_om=1.975,
        burnup_at_percent=1.0,
        mo_oxidation_fraction=0.0,
    )

    assert abs(result.oxygen_released_per_10_fissions - 2.0) < 1e-12
    assert abs(result.oxygen_fixed_sinks_per_10_fissions - 1.566) < 1e-12
    assert abs(result.oxygen_surplus_per_10_fissions - 0.434) < 1e-12
    assert abs(result.target_average_om - (1.975 + 0.434 / 95.48)) < 1e-12


def test_oxygen_balance_rejects_nonphysical_burnup() -> None:
    with pytest.raises(ValueError, match=r"\[0, 100\]"):
        OxygenBalanceModel().target_average_om(
            initial_average_om=1.975,
            burnup_at_percent=101.0,
        )

    with pytest.raises(ValueError, match=r"\[0, 100\]"):
        OxygenBalanceModel_PHENIX().matrix_atom_inventory(burnup_at_percent=101.0)


def test_phenix_oxygen_balance_uses_imported_yields() -> None:
    balance = OxygenBalanceModel_PHENIX()
    result = balance.target_average_om(
        initial_average_om=2.0,
        burnup_at_percent=10.0,
        mo_oxidation_fraction=0.0,
    )

    assert abs(result.oxygen_released_per_10_fissions - 20.0) < 1e-12
    assert abs(result.oxygen_fixed_sinks_per_10_fissions - 14.25) < 1e-12
    assert abs(result.oxygen_mo_sink_per_10_fissions) < 1e-12
    assert abs(result.oxygen_surplus_per_10_fissions - 5.75) < 1e-12
    assert abs(result.target_average_om - (2.0 + 5.75 / 95.0032)) < 1e-12


def test_phenix_oxygen_balance_alias_and_cladding_sink() -> None:
    balance = OxygenBalanceModel_PHENIX()
    result = balance.target_average_om(
        initial_average_om=2.0,
        burnup_at_percent=10.0,
        mo_oxidation_fraction=0.6,
        cladding_sink_fraction=0.5,
    )

    assert abs(result.oxygen_mo_sink_per_10_fissions - 2.556) < 1e-12
    assert abs(result.oxygen_cladding_sink_per_10_fissions - 1.597) < 1e-12
    assert abs(result.oxygen_surplus_per_10_fissions - 1.597) < 1e-12


def test_phenix_oxygen_balance_reports_single_contributions() -> None:
    balance = OxygenBalanceModel_PHENIX()
    contributions = balance.oxygen_sink_contributions(
        burnup_at_percent=10.0,
        mo_oxidation_fraction=0.6,
    )

    by_element = {contribution.element: contribution for contribution in contributions}
    assert by_element["Zr"].reacted_fraction == 1.0
    assert by_element["Sr"].reacted_fraction == 1.0
    assert abs(by_element["Zr"].oxygen_atoms_per_100_initial_metal - 3.84) < 1e-12
    assert abs(by_element["Sr"].oxygen_atoms_per_100_initial_metal - 0.72) < 1e-12
    assert abs(by_element["Ba"].oxygen_atoms_per_100_initial_metal - 0.68) < 1e-12
    assert abs(by_element["Nb"].oxygen_atoms_per_100_initial_metal - 0.05) < 1e-12
    assert abs(by_element["Mo"].oxygen_atoms_per_100_initial_metal - 2.556) < 1e-12

    result = balance.target_average_om(
        initial_average_om=2.0,
        burnup_at_percent=10.0,
        mo_oxidation_fraction=0.6,
    )
    fixed_without_mo = sum(
        contribution.oxygen_atoms_per_100_initial_metal
        for contribution in contributions
        if contribution.element != "Mo"
    )
    assert abs(fixed_without_mo - result.oxygen_fixed_sinks_per_10_fissions) < 1e-12


def test_phenix_mo_oxidation_fraction_applies_from_start() -> None:
    balance = OxygenBalanceModel_PHENIX()
    contributions = balance.oxygen_sink_contributions(
        burnup_at_percent=1.0,
        mo_oxidation_fraction=0.6,
    )

    by_element = {contribution.element: contribution for contribution in contributions}
    assert by_element["Ba"].reacted_fraction == 1.0
    assert by_element["Mo"].reacted_fraction == 0.6
    assert abs(by_element["Mo"].oxygen_atoms_per_100_initial_metal - 0.2556) < 1e-12


def test_phenix_fixed_sink_summary_reports_zr_sr_nb_y_re() -> None:
    balance = OxygenBalanceModel_PHENIX()
    summary = balance.fixed_sink_summary(burnup_at_percent=10.0)

    assert abs(summary["Zr"] - 3.84) < 1e-12
    assert abs(summary["Sr"] - 0.72) < 1e-12
    assert abs(summary["Nb"] - 0.05) < 1e-12
    assert abs(summary["Y+RE"] - 8.96) < 1e-12
    assert abs(summary["Zr+Sr+Y+RE"] - 13.52) < 1e-12
    assert abs(summary["Zr+Sr+Nb+Y+RE"] - 13.57) < 1e-12


def test_phenix_matrix_atom_inventory_is_reported() -> None:
    balance = OxygenBalanceModel_PHENIX()
    inventory = balance.matrix_atom_inventory(burnup_at_percent=10.0)

    assert abs(inventory.initial_metal_atoms - 100.0) < 1e-12
    assert abs(inventory.burned_metal_atoms - 10.0) < 1e-12
    assert abs(inventory.remaining_initial_metal_atoms - 90.0) < 1e-12
    assert abs(inventory.zr_atoms_in_matrix - 0.48) < 1e-12
    assert abs(inventory.sr_atoms_in_matrix - 0.0432) < 1e-12
    assert abs(inventory.y_re_atoms_in_matrix - 4.48) < 1e-12
    assert abs(inventory.fission_product_atoms_in_matrix - 5.0032) < 1e-12
    assert abs(inventory.matrix_atoms - 95.0032) < 1e-12
