"""Hypostoichiometric OXIRED solver used by example_usage_model.py."""
from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, Literal

import numpy as np

from .constants import R_GAS, heat_of_transport_vacancy

Mode = Literal["hypo"]


@dataclass(frozen=True)
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


@dataclass(frozen=True)
class OxiRedResult:
    radius: np.ndarray
    temperature: np.ndarray
    om: np.ndarray
    defect_fraction: np.ndarray
    average_om: float


class OxiRedCylinder:
    """Steady-state radial O/M redistribution in hypostoichiometric MOX."""

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
        if not (0.0 < pu_fraction < 1.0):
            raise ValueError("hypostoichiometric MOX requires 0 < pu_fraction < 1")

    def mesh(self) -> tuple[np.ndarray, np.ndarray]:
        edges = np.linspace(self.geometry.r_inner, self.geometry.r_outer, self.n_cells + 1)
        centers = 0.5 * (edges[:-1] + edges[1:])
        return edges, centers

    def solve_steady_state(
        self,
        average_om: float,
        mode: Mode = "hypo",
        max_iter: int = 200,
        tol: float = 1e-10,
        relaxation: float = 1.0,
    ) -> OxiRedResult:
        """Solve the hypostoichiometric steady-state radial O/M profile."""
        if mode != "hypo":
            raise ValueError("only hypostoichiometric mode is supported")
        if not np.isfinite(average_om):
            raise ValueError("average_om must be finite")
        if average_om > 2.0:
            raise ValueError("hypostoichiometric mode requires average_om <= 2")
        if not (0.0 < relaxation <= 1.0):
            raise ValueError("relaxation must be in (0, 1]")

        edges, radius = self.mesh()
        temperature = np.asarray(self.temperature_profile(radius), dtype=float)
        if temperature.shape != radius.shape:
            raise ValueError("temperature profile must return one value per radial cell")
        if np.any(temperature <= 0.0):
            raise ValueError("temperature profile must be strictly positive")

        c_avg = self._om_to_vacancy_fraction(average_om)
        c = np.full_like(radius, c_avg)
        ring_areas = edges[1:] ** 2 - edges[:-1] ** 2

        for _ in range(max_iter):
            q_local = self._heat_of_transport_from_c(c)
            inv_t = 1.0 / temperature
            d_inv_t = inv_t - inv_t[0]
            shape = np.exp((q_local / R_GAS) * d_inv_t)

            scale = c_avg * np.sum(ring_areas) / np.sum(ring_areas * shape)
            c_new = scale * shape
            c_relaxed = relaxation * c_new + (1.0 - relaxation) * c
            err = np.max(np.abs(c_relaxed - c))
            c = c_relaxed
            if err < tol:
                break
        else:
            raise RuntimeError("steady-state iteration did not converge")

        om = self._vacancy_fraction_to_om(c)
        average = self.area_average(edges, om)
        return OxiRedResult(
            radius=radius,
            temperature=temperature,
            om=om,
            defect_fraction=c,
            average_om=float(average),
        )

    @staticmethod
    def area_average(edges: np.ndarray, values: np.ndarray) -> float:
        ring_areas = edges[1:] ** 2 - edges[:-1] ** 2
        return float(np.sum(values * ring_areas) / np.sum(ring_areas))

    @staticmethod
    def _om_to_vacancy_fraction(om: np.ndarray | float) -> np.ndarray:
        return 0.5 * (2.0 - np.asarray(om, dtype=float))

    @staticmethod
    def _vacancy_fraction_to_om(c: np.ndarray) -> np.ndarray:
        return 2.0 - 2.0 * c

    def _heat_of_transport_from_c(self, c: np.ndarray) -> np.ndarray:
        om = self._vacancy_fraction_to_om(c)
        return np.vectorize(
            lambda value: heat_of_transport_vacancy(value, self.pu_fraction)
        )(om)
