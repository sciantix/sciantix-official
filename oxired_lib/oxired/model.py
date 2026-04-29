"""
Core oxygen redistribution solver for cylindrical fuel.
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, Literal, Optional

import numpy as np

from .constants import (
    R_GAS,
    diffusion_coefficient,
    heat_of_transport_interstitial,
    heat_of_transport_vacancy,
)
from .fission_yields import PHENIX_FISSION_YIELDS, SOLID_SOLUTION, fission_yield_for_element

Mode = Literal["auto", "hypo", "hyper"]


@dataclass
class CylinderGeometry:
    r_outer: float
    r_inner: float = 0.0

    def __post_init__(self) -> None:
        if not np.isfinite(self.r_outer) or not np.isfinite(self.r_inner):
            raise ValueError("geometry radii must be finite")
        if self.r_inner < 0.0:
            raise ValueError("r_inner must be non-negative")
        if self.r_outer <= self.r_inner:
            raise ValueError("r_outer must be greater than r_inner")

    @property
    def equivalent_radius(self) -> float:
        return float(np.sqrt(self.r_outer**2 - self.r_inner**2))


@dataclass
class OxiRedResult:
    radius: np.ndarray
    temperature: np.ndarray
    om: np.ndarray
    defect_fraction: np.ndarray
    mode: Literal["hypo", "hyper"]
    average_om: float
    tau_seconds: Optional[float] = None

    def as_dict(self) -> dict:
        return {
            "radius": self.radius,
            "temperature": self.temperature,
            "om": self.om,
            "defect_fraction": self.defect_fraction,
            "mode": self.mode,
            "average_om": self.average_om,
            "tau_seconds": self.tau_seconds,
        }


class OxiRedCylinder:
    """Solve OXIRED oxygen redistribution in a cylindrical pellet.

    Parameters
    ----------
    geometry:
        Pellet geometry in meters.
    temperature_profile:
        Callable T(r) returning temperature in kelvin.
    pu_fraction:
        Plutonium fraction q in (U1-q, Puq)O2±x.
        For pure UO2+x, use q=0.
    n_cells:
        Number of finite-volume rings.
    """

    def __init__(
        self,
        geometry: CylinderGeometry,
        temperature_profile: Callable[[np.ndarray], np.ndarray],
        pu_fraction: float,
        n_cells: int = 200,
    ) -> None:
        self.geometry = geometry
        self.temperature_profile = temperature_profile
        self.pu_fraction = pu_fraction
        self.n_cells = int(n_cells)
        if self.n_cells < 3:
            raise ValueError("n_cells must be at least 3")
        if not (0.0 <= pu_fraction < 1.0):
            raise ValueError("pu_fraction must satisfy 0 <= q < 1")

    def mesh(self) -> tuple[np.ndarray, np.ndarray]:
        edges = np.linspace(self.geometry.r_inner, self.geometry.r_outer, self.n_cells + 1)
        centers = 0.5 * (edges[:-1] + edges[1:])
        return edges, centers

    def solve_steady_state(
        self,
        average_om: float,
        mode: Mode = "auto",
        max_iter: int = 200,
        tol: float = 1e-10,
        relaxation: float = 1.0,
    ) -> OxiRedResult:
        """
        Solve the steady-state radial O/M profile.
        """
        if not np.isfinite(average_om):
            raise ValueError("average_om must be finite")
        if not (0.0 < relaxation <= 1.0):
            raise ValueError("relaxation must be in (0, 1]")
        actual_mode = self._resolve_mode(average_om, mode)
        edges, r = self.mesh()
        T = np.asarray(self.temperature_profile(r), dtype=float)
        if T.shape != r.shape:
            raise ValueError("temperature profile must return one value per radial cell")
        if np.any(T <= 0.0):
            raise ValueError("temperature profile must be strictly positive")

        c_avg = self._om_to_defect_fraction(average_om, actual_mode)
        c = np.full_like(r, c_avg)
        ring_areas = edges[1:] ** 2 - edges[:-1] ** 2

        for _ in range(max_iter):
            q_local = self._heat_of_transport_from_c(c=c, mode=actual_mode)
            # cell-wise profile factor, based on integrated d ln c = (Q/R) d(1/T)
            invT = 1.0 / T
            d_invT = invT - invT[0]
            f = np.exp((q_local / R_GAS) * d_invT)
            # normalize to conserve area-averaged defect inventory
            scale = c_avg * np.sum(ring_areas) / np.sum(ring_areas * f)
            c_new = scale * f
            c_relaxed = relaxation * c_new + (1.0 - relaxation) * c
            err = np.max(np.abs(c_relaxed - c))
            c = c_relaxed
            if err < tol:
                break
        else:
            raise RuntimeError("steady-state iteration did not converge")

        om = self._defect_fraction_to_om(c, actual_mode)
        avg_om = self.area_average(edges, om)
        return OxiRedResult(
            radius=r,
            temperature=T,
            om=om,
            defect_fraction=c,
            mode=actual_mode,
            average_om=float(avg_om),
            tau_seconds=None,
        )

    def solve_transient(
        self,
        initial_om_profile: np.ndarray | float,
        target_average_om: float,
        dt: float,
        mode: Mode = "auto",
    ) -> OxiRedResult:
        """
        Apply Lassmann's transient approximation over one time step.
        c(r,t+dt) = c_inf(r) + (c(r,t) - c_inf(r)) exp(-dt/tau)
        with tau based on the average fuel temperature.
        """
        if dt < 0.0:
            raise ValueError("dt must be non-negative")
        steady = self.solve_steady_state(average_om=target_average_om, mode=mode)
        edges, r = self.mesh()
        T = steady.temperature
        actual_mode = steady.mode

        if np.isscalar(initial_om_profile):
            om0 = np.full_like(r, float(initial_om_profile))
        else:
            om0 = np.asarray(initial_om_profile, dtype=float)
            if om0.shape != r.shape:
                raise ValueError("initial_om_profile has incompatible shape")

        c0 = self._om_to_defect_fraction(om0, actual_mode)
        c_inf = steady.defect_fraction
        tau = self.redistribution_time_constant(T)
        factor = float(np.exp(-dt / tau))
        c_t = c_inf + (c0 - c_inf) * factor
        om_t = self._defect_fraction_to_om(c_t, actual_mode)

        return OxiRedResult(
            radius=r,
            temperature=T,
            om=om_t,
            defect_fraction=c_t,
            mode=actual_mode,
            average_om=float(self.area_average(edges, om_t)),
            tau_seconds=float(tau),
        )

    def redistribution_time_constant(self, temperature: np.ndarray) -> float:
        """Average redistribution time constant tau [s]."""
        t_av = 0.5 * (float(temperature[0]) + float(temperature[-1]))
        d_av = diffusion_coefficient(t_av)
        return self.geometry.equivalent_radius**2 / (17.2 * d_av)

    def burnup_shifted_average_om(
        self,
        initial_om: float,
        burnup_at_percent: float,
        fuel_type: Literal["fbr_mox", "lwr_uo2"] = "fbr_mox",
    ) -> float:
        """Apply the simple OXIRED burnup stoichiometry-shift model."""
        if fuel_type == "fbr_mox":
            return min(2.0, initial_om + 0.005 * burnup_at_percent)
        if fuel_type == "lwr_uo2":
            return initial_om + 0.0013 * burnup_at_percent
        raise ValueError("unsupported fuel_type")

    def area_average(self, edges: np.ndarray, values: np.ndarray) -> float:
        ring_areas = edges[1:] ** 2 - edges[:-1] ** 2
        return float(np.sum(values * ring_areas) / np.sum(ring_areas))

    def _resolve_mode(self, average_om: float, mode: Mode) -> Literal["hypo", "hyper"]:
        if mode == "auto":
            return "hyper" if average_om >= 2.0 else "hypo"
        if mode not in ("hypo", "hyper"):
            raise ValueError("mode must be 'auto', 'hypo', or 'hyper'")
        if mode == "hypo" and average_om > 2.0:
            raise ValueError("hypo mode requires average_om <= 2")
        if mode == "hyper" and average_om < 2.0:
            raise ValueError("hyper mode requires average_om >= 2")
        return mode

    @staticmethod
    def _om_to_defect_fraction(om: np.ndarray | float, mode: Literal["hypo", "hyper"]) -> np.ndarray:
        om = np.asarray(om, dtype=float)
        if mode == "hyper":
            return om - 2.0
        return 0.5 * (2.0 - om)

    @staticmethod
    def _defect_fraction_to_om(c: np.ndarray, mode: Literal["hypo", "hyper"]) -> np.ndarray:
        if mode == "hyper":
            return 2.0 + c
        return 2.0 - 2.0 * c

    def _heat_of_transport_from_c(self, c: np.ndarray, mode: Literal["hypo", "hyper"]) -> np.ndarray:
        om = self._defect_fraction_to_om(c, mode)
        if mode == "hyper":
            return np.vectorize(lambda x: heat_of_transport_interstitial(x, self.pu_fraction))(om)
        if self.pu_fraction <= 0.0:
            raise ValueError("hypostoichiometric mixed-oxide mode requires pu_fraction > 0")
        return np.vectorize(lambda x: heat_of_transport_vacancy(x, self.pu_fraction))(om)


@dataclass(frozen=True)
class OxygenBalanceResult:
    initial_average_om: float
    burnup_at_percent: float
    oxygen_released_per_10_fissions: float
    oxygen_fixed_sinks_per_10_fissions: float
    oxygen_mo_sink_per_10_fissions: float
    oxygen_cladding_sink_per_10_fissions: float
    oxygen_surplus_per_10_fissions: float
    delta_om: float
    target_average_om: float


@dataclass(frozen=True)
class OxygenBalanceContribution:
    name: str
    element: str
    group: str
    fp_atoms_per_100_initial_metal: float
    reacted_fraction: float
    oxygen_per_fp_atom: float
    oxygen_atoms_per_100_initial_metal: float


@dataclass(frozen=True)
class MatrixAtomInventory:
    initial_metal_atoms: float
    burned_metal_atoms: float
    remaining_initial_metal_atoms: float
    zr_atoms_in_matrix: float
    sr_atoms_in_matrix: float
    y_re_atoms_in_matrix: float
    fission_product_atoms_in_matrix: float
    matrix_atoms: float


@dataclass(frozen=True)
class OxygenBalanceModel_PHENIX:
    """Phenix-specific oxygen balance using the imported fission-yield table."""
    zr_matrix_fraction: float = 0.25
    sr_matrix_fraction: float = 0.12

    def target_average_om(
        self,
        initial_average_om: float,
        burnup_at_percent: float,
        mo_oxidation_fraction: float = 0.6,
        cladding_sink_fraction: float = 0.0,
    ) -> OxygenBalanceResult:
        """
        Compute target bulk O/M after burnup with partial Mo oxidation and
        optional cladding oxygen sink.

        Parameters
        ----------
        initial_average_om:
            Initial average oxygen-to-metal ratio.
        burnup_at_percent:
            Burnup in at.%, i.e. fissions per initial metal atom in percent.
            NB.  10 fissions over 100 initial metal atoms 
                    = 0.1 FIMA fraction
                    = 10 at.% FIMA
        mo_oxidation_fraction:
            Fraction of the Phenix Mo yield oxidized as MoO2 from the start.
        cladding_sink_fraction:
            Fraction of positive oxygen surplus absorbed by the cladding.

        Returns
        -------
        OxygenBalanceResult
        """
        if not (0.0 <= burnup_at_percent <= 100.0):
            raise ValueError("burnup_at_percent must be in [0, 100]")
        if not (0.0 <= mo_oxidation_fraction <= 1.0):
            raise ValueError("mo_oxidation_fraction must be in [0, 1]")
        if not (0.0 <= cladding_sink_fraction <= 1.0):
            raise ValueError("cladding_sink_fraction must be in [0, 1]")

        # Quantities are expressed as atoms per 100 initial metal atoms.
        # At 10 at.% burnup this is numerically "per 10 fissions".
        released = initial_average_om * burnup_at_percent

        # Simple sink model: Zr, Sr, Y+RE, Ba, Nb consume oxygen; Mo consumes a
        # prescribed fraction of its full MoO2 oxygen demand.
        contributions = self.oxygen_sink_contributions(
            burnup_at_percent=burnup_at_percent,
            mo_oxidation_fraction=mo_oxidation_fraction,
        )
        fixed = sum(
            contribution.oxygen_atoms_per_100_initial_metal
            for contribution in contributions
            if contribution.element != "Mo"
        )
        mo_sink = sum(
            contribution.oxygen_atoms_per_100_initial_metal
            for contribution in contributions
            if contribution.element == "Mo"
        )
        gross_surplus = released - fixed - mo_sink
        cladding_sink = max(gross_surplus, 0.0) * cladding_sink_fraction
        net_surplus = gross_surplus - cladding_sink

        matrix_atoms = self.matrix_atom_inventory(burnup_at_percent).matrix_atoms
        delta_om = net_surplus / matrix_atoms
        target = initial_average_om + delta_om

        return OxygenBalanceResult(
            initial_average_om=initial_average_om,
            burnup_at_percent=burnup_at_percent,
            oxygen_released_per_10_fissions=released,
            oxygen_fixed_sinks_per_10_fissions=fixed,
            oxygen_mo_sink_per_10_fissions=mo_sink,
            oxygen_cladding_sink_per_10_fissions=cladding_sink,
            oxygen_surplus_per_10_fissions=net_surplus,
            delta_om=delta_om,
            target_average_om=target,
        )

    def oxygen_sink_contributions(
        self,
        burnup_at_percent: float,
        mo_oxidation_fraction: float = 0.6,
    ) -> tuple[OxygenBalanceContribution, ...]:
        if not (0.0 <= burnup_at_percent <= 100.0):
            raise ValueError("burnup_at_percent must be in [0, 100]")
        if not (0.0 <= mo_oxidation_fraction <= 1.0):
            raise ValueError("mo_oxidation_fraction must be in [0, 1]")

        contributions = [
            # Zr and Sr oxidize fully; their matrix fractions only affect the
            # matrix-atom inventory used in the O/M denominator.
            self._oxygen_contribution("Zr", burnup_at_percent),
            self._oxygen_contribution("Sr", burnup_at_percent),
        ]
        contributions.extend(
            # Y + rare earths in solid solution are assumed fully oxidized.
            self._oxygen_contribution(entry.element, burnup_at_percent)
            for entry in self._solid_solution_entries()
        )
        contributions.extend(
            [
                self._oxygen_contribution("Ba", burnup_at_percent),
                self._oxygen_contribution("Nb", burnup_at_percent),
                self._oxygen_contribution("Mo", burnup_at_percent, mo_oxidation_fraction),
            ]
        )
        return tuple(contributions)

    def fixed_sink_summary(
        self,
        burnup_at_percent: float,
    ) -> dict[str, float]:
        contributions = self.oxygen_sink_contributions(
            burnup_at_percent=burnup_at_percent,
            mo_oxidation_fraction=0.0,
        )
        by_element = {contribution.element: contribution for contribution in contributions}
        y_re_sink = sum(
            contribution.oxygen_atoms_per_100_initial_metal
            for contribution in contributions
            if contribution.group == SOLID_SOLUTION
        )
        return {
            "Zr": by_element["Zr"].oxygen_atoms_per_100_initial_metal,
            "Sr": by_element["Sr"].oxygen_atoms_per_100_initial_metal,
            "Nb": by_element["Nb"].oxygen_atoms_per_100_initial_metal,
            "Y+RE": y_re_sink,
            "Zr+Sr+Y+RE": by_element["Zr"].oxygen_atoms_per_100_initial_metal
            + by_element["Sr"].oxygen_atoms_per_100_initial_metal
            + y_re_sink,
            "Zr+Sr+Nb+Y+RE": by_element["Zr"].oxygen_atoms_per_100_initial_metal
            + by_element["Sr"].oxygen_atoms_per_100_initial_metal
            + by_element["Nb"].oxygen_atoms_per_100_initial_metal
            + y_re_sink,
        }

    def matrix_atom_inventory(self, burnup_at_percent: float) -> MatrixAtomInventory:
        if not (0.0 <= burnup_at_percent <= 100.0):
            raise ValueError("burnup_at_percent must be in [0, 100]")

        initial_metal_atoms = 100.0
        burned_metal_atoms = burnup_at_percent
        remaining_initial_metal_atoms = initial_metal_atoms - burned_metal_atoms

        # Matrix atoms are the un-fissioned initial metal atoms plus the FP
        # atoms assumed to stay dissolved in the fuel matrix.
        zr_atoms = self.zr_matrix_fraction * self._fp_atoms_per_100_initial_metal("Zr", burnup_at_percent)
        sr_atoms = self.sr_matrix_fraction * self._fp_atoms_per_100_initial_metal("Sr", burnup_at_percent)
        y_re_atoms = sum(
            self._fp_atoms_per_100_initial_metal(entry.element, burnup_at_percent)
            for entry in self._solid_solution_entries()
        )
        fp_atoms = zr_atoms + sr_atoms + y_re_atoms
        matrix_atoms = remaining_initial_metal_atoms + fp_atoms

        return MatrixAtomInventory(
            initial_metal_atoms=initial_metal_atoms,
            burned_metal_atoms=burned_metal_atoms,
            remaining_initial_metal_atoms=remaining_initial_metal_atoms,
            zr_atoms_in_matrix=zr_atoms,
            sr_atoms_in_matrix=sr_atoms,
            y_re_atoms_in_matrix=y_re_atoms,
            fission_product_atoms_in_matrix=fp_atoms,
            matrix_atoms=matrix_atoms,
        )

    def _oxygen_contribution(
        self,
        element: str,
        burnup_at_percent: float,
        reacted_fraction: float = 1.0,
    ) -> OxygenBalanceContribution:
        yield_entry = fission_yield_for_element(element)
        oxygen_per_fp_atom = yield_entry.valence / 2.0
        fp_atoms = self._fp_atoms_per_100_initial_metal(element, burnup_at_percent)
        oxygen_atoms = reacted_fraction * fp_atoms * oxygen_per_fp_atom
        return OxygenBalanceContribution(
            name=f"{element} oxygen sink",
            element=element,
            group=yield_entry.group,
            fp_atoms_per_100_initial_metal=fp_atoms,
            reacted_fraction=reacted_fraction,
            oxygen_per_fp_atom=oxygen_per_fp_atom,
            oxygen_atoms_per_100_initial_metal=oxygen_atoms,
        )

    @staticmethod
    def _fp_atoms_per_100_initial_metal(element: str, burnup_at_percent: float) -> float:
        return fission_yield_for_element(element).yield_percent_fp_per_fission * burnup_at_percent / 100.0

    @staticmethod
    def _solid_solution_entries():
        return (entry for entry in PHENIX_FISSION_YIELDS if entry.group == SOLID_SOLUTION)


@dataclass(frozen=True)
class OxygenBalanceModel:
    """
    Oxygen balance model by J. Spino, P. Peerani / Journal of Nuclear Materials 375 (2008) 8-25 .

    For every 10 fissions (in LWR conditions, Table 3):
      - about 20 oxygen atoms are released
      - about 15.6 are consumed by Zr + Y/RE + Sr/Ba
      - oxidation of elements according to the increasing oxygen potential, fully oxidation can stand:
        Mo (4.58 atoms of oxygen), Sn (0.08), Cs (0.565) + Rb (0.105), Sb (0.0075), Cd (0.157) and Tc (0.86)
      - the remaining surplus raises fuel O/M unless absorbed by cladding

    For each 10 fissions:
    O/M final = O/M initial + N_o_surplus / (90 + 0.25Nzr + 0.12Nsr + Ny/re ) = O/M initial + N_o_surplus / 95.48
    In the above expressions it is implicit that 25% of Zr and 12% of Sr are dissolved in the fuel matrix. 
    This leads to 15.66 atoms per every 10 fissions consumed after oxisation of Zr, Y + RE + Sr +Ba.

    In the examples considered in Table 3, it is shown that such oxidation stagnation at fractions of 50-60% of 
    the available amount of Mo would enable fina O/M ratios of the fuel of approximately 2.01-2.013 to be achieved 
    """
    oxygen_released_per_10_fissions: float = 20.0
    fixed_sink_oxygen_per_10_fissions: float = 15.66
    mo_full_oxygen_per_10_fissions: float = 4.58
    fuel_matrix_atoms_per_10_fissions: float = 95.48

    def target_average_om(
        self,
        initial_average_om: float,
        burnup_at_percent: float,
        mo_oxidation_fraction: float = 0.6,
        cladding_sink_fraction: float = 0.0,
    ) -> OxygenBalanceResult:
        """
        Compute target bulk O/M after burnup with partial Mo oxidation and
        optional cladding oxygen sink.

        Parameters
        ----------
        initial_average_om:
            Initial average oxygen-to-metal ratio.
        burnup_at_percent:
            Burnup in at.%.
        mo_oxidation_fraction:
            Fraction of the full Mo oxygen demand consumed by Mo oxidation.
        cladding_sink_fraction:
            Fraction of positive oxygen surplus absorbed by the cladding.

        Returns
        -------
        OxygenBalanceResult
        """
        if not (0.0 <= burnup_at_percent <= 100.0):
            raise ValueError("burnup_at_percent must be in [0, 100]")
        if not (0.0 <= mo_oxidation_fraction <= 1.0):
            raise ValueError("mo_oxidation_fraction must be in [0, 1]")
        if not (0.0 <= cladding_sink_fraction <= 1.0):
            raise ValueError("cladding_sink_fraction must be in [0, 1]")

        FIMA_scaled = burnup_at_percent / 10.0 #every ten fissions

        released = self.oxygen_released_per_10_fissions * FIMA_scaled
        fixed = self.fixed_sink_oxygen_per_10_fissions * FIMA_scaled
        mo_sink = self.mo_full_oxygen_per_10_fissions * mo_oxidation_fraction * FIMA_scaled
        gross_surplus = released - fixed - mo_sink
        cladding_sink = max(gross_surplus, 0.0) * cladding_sink_fraction
        net_surplus = gross_surplus - cladding_sink

        matrix_atoms = self.fuel_matrix_atoms_per_10_fissions
        delta_om = net_surplus / matrix_atoms
        target = initial_average_om + delta_om

        return OxygenBalanceResult(
            initial_average_om=initial_average_om,
            burnup_at_percent=burnup_at_percent,
            oxygen_released_per_10_fissions=released,
            oxygen_fixed_sinks_per_10_fissions=fixed,
            oxygen_mo_sink_per_10_fissions=mo_sink,
            oxygen_cladding_sink_per_10_fissions=cladding_sink,
            oxygen_surplus_per_10_fissions=net_surplus,
            delta_om=delta_om,
            target_average_om=target,
        )
