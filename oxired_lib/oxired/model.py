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
class OxygenBalanceModel:
    """
    Oxygen balance model by J. Spino, P. Peerani / Journal of Nuclear Materials 375 (2008) 8-25 .

    For every 10 fissions (in LWR conditions, Table 3):
      - about 20 oxygen atoms are released
      - about 15.6 are consumed by Zr + Y/RE + Sr/Ba
      - oxidation of elements according to the increasing oxygen potential, fully oxidation can stand:
        Mo (4.58 atoms of oxygen), Sn (0.08), Cs (0.565) + Rb (0.105), Sb (0.0075), Cd (0.157) and Tc (0.86)
      - the remaining surplus raises fuel O/M unless absorbed by cladding

    In this work we consider up to Zr + Y/RE + Sr/Ba.
    The Mo sink is indeed considered in SCIANTIX thanks to Cs-Mo-O database.

    For each 10 fissions:
    O/M final = O/M initial + N_o_surplus / (90 + 0.25Nzr + 0.12Nsr + Ny/re ) = O/M initial + N_o_surplus / 95.48
    In the above expressions it is implicit that 25% of Zr and 12% of Sr are dissolved in the fuel matrix. 
    This leads to 15.66 atoms per every 10 fissions consumed after oxisation of Zr, Y + RE + Sr +Ba.
    """
    oxygen_released_per_10_fissions: float = 20.0
    fixed_sink_oxygen_per_10_fissions: float = 15.66
    mo_full_oxygen_per_10_fissions: float = 4.58
    fuel_matrix_atoms_per_10_fissions: float = 95.48

    def target_average_om(
        self,
        initial_average_om: float,
        burnup_at_percent: float,
        mo_oxidation_fraction: float = 0.0,
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
            Not considered.
        cladding_sink_fraction:
            Not considered.

        Returns
        -------
        OxygenBalanceResult
        """
        if burnup_at_percent < 0.0:
            raise ValueError("burnup_at_percent must be non-negative")
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
