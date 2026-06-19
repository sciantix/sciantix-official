"""Radial cesium redistribution model used to generate SCIANTIX scale factors."""
from __future__ import annotations

from dataclasses import dataclass
from typing import Callable

import numpy as np

from .constants import (
    CS_HEAT_OF_TRANSPORT,
    R_GAS,
)


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
class CsRedResult:
    radius: np.ndarray
    temperature: np.ndarray
    produced: np.ndarray
    redistributed: np.ndarray
    scaling_factor: np.ndarray
    average_scaling_factor: float
    equilibrium_scaling_factor: np.ndarray
    thermal_shape: np.ndarray
    inventory_history: np.ndarray
    scaling_history: np.ndarray


def area_average(edges: np.ndarray, values: np.ndarray) -> float:
    """Finite-volume radial average."""
    edges = np.asarray(edges, dtype=float)
    values = np.asarray(values, dtype=float)
    ring_areas = edges[1:] ** 2 - edges[:-1] ** 2
    return float(np.sum(values * ring_areas) / np.sum(ring_areas))


class CsRedCylinder:
    """Radial Cs redistribution toward an Arrhenius thermal concentration.

    The transported quantity is a local concentration-like inventory, for
    example Cs atoms per initial heavy-metal atom. Source values may be in any
    consistent unit because the returned scaling factors are ratios.
    """

    def __init__(
        self,
        geometry: CylinderGeometry,
        temperature_profile: Callable[[np.ndarray], np.ndarray],
        n_cells: int = 200,
        heat_of_transport: float = CS_HEAT_OF_TRANSPORT,
    ) -> None:
        self.geometry = geometry
        self.temperature_profile = temperature_profile
        self.n_cells = int(n_cells)
        self.heat_of_transport = float(heat_of_transport)

        if self.n_cells < 2:
            raise ValueError("n_cells must be at least 2")
        if not np.isfinite(self.heat_of_transport):
            raise ValueError("heat_of_transport must be finite")

    def mesh(self) -> tuple[np.ndarray, np.ndarray]:
        edges = np.linspace(self.geometry.r_inner, self.geometry.r_outer, self.n_cells + 1)
        centers = 0.5 * (edges[:-1] + edges[1:])
        return edges, centers

    def solve_history(
        self,
        time_seconds: np.ndarray,
        produced_history: np.ndarray,
        initial_inventory: np.ndarray | None = None,
    ) -> CsRedResult:
        """Redistribute a cumulative Cs production history.

        `produced_history[j, i]` is the cumulative Cs produced in radial cell
        `i` at time index `j`. Each row is mapped directly onto the normalized
        thermal shape N(r) = A exp(-Q*/RT(r)).
        """
        time_seconds = np.asarray(time_seconds, dtype=float)
        produced_history = np.asarray(produced_history, dtype=float)
        if time_seconds.ndim != 1:
            raise ValueError("time_seconds must be one-dimensional")
        if produced_history.shape != (len(time_seconds), self.n_cells):
            raise ValueError("produced_history shape must be (n_times, n_cells)")
        if len(time_seconds) == 0:
            raise ValueError("time_seconds must not be empty")
        if np.any(np.diff(time_seconds) < 0.0):
            raise ValueError("time_seconds must be monotonic")
        if np.any(produced_history < 0.0):
            raise ValueError("produced_history must be non-negative")

        edges, radius = self.mesh()
        temperature = self._temperature(radius)
        weights = self._ring_weights(edges)
        thermal_shape = self._thermal_shape(temperature)
        if initial_inventory is not None:
            initial_inventory = np.asarray(initial_inventory, dtype=float)
            if initial_inventory.shape != (self.n_cells,):
                raise ValueError("initial_inventory must have one value per radial cell")
            if np.any(initial_inventory < 0.0):
                raise ValueError("initial_inventory must be non-negative")

        inventory_history = []
        scaling_history = []
        for produced in produced_history:
            inventory = produced.copy()
            if initial_inventory is not None:
                inventory = inventory + initial_inventory
            redistributed = self._thermal_target(inventory, weights, thermal_shape)
            inventory_history.append(redistributed)
            scaling_history.append(self._scaling_factor(redistributed, produced, weights))

        inventory_history_array = np.asarray(inventory_history)
        scaling_history_array = np.asarray(scaling_history)
        final_produced = produced_history[-1]
        final_inventory = inventory_history_array[-1]
        final_scaling = scaling_history_array[-1]
        equilibrium_scaling = self.equilibrium_scaling_factor(final_produced)

        return CsRedResult(
            radius=radius,
            temperature=temperature,
            produced=final_produced.copy(),
            redistributed=final_inventory.copy(),
            scaling_factor=final_scaling.copy(),
            average_scaling_factor=area_average(edges, final_scaling),
            equilibrium_scaling_factor=equilibrium_scaling.copy(),
            thermal_shape=thermal_shape.copy(),
            inventory_history=inventory_history_array,
            scaling_history=scaling_history_array,
        )

    def equilibrium_scaling_factor(self, produced: np.ndarray) -> np.ndarray:
        """Return the steady Arrhenius radial scaling factor."""
        produced = np.asarray(produced, dtype=float)
        if produced.shape != (self.n_cells,):
            raise ValueError("produced must have one value per radial cell")
        if np.any(produced < 0.0):
            raise ValueError("produced must be non-negative")

        edges, radius = self.mesh()
        temperature = self._temperature(radius)
        weights = self._ring_weights(edges)
        thermal_shape = self._thermal_shape(temperature)
        redistributed = self._thermal_target(produced, weights, thermal_shape)
        return self._scaling_factor(redistributed, produced, weights)

    def _temperature(self, radius: np.ndarray) -> np.ndarray:
        temperature = np.asarray(self.temperature_profile(radius), dtype=float)
        if temperature.shape != radius.shape:
            raise ValueError("temperature profile must return one value per radial cell")
        if np.any(temperature <= 0.0):
            raise ValueError("temperature profile must be strictly positive")
        return temperature

    @staticmethod
    def _ring_weights(edges: np.ndarray) -> np.ndarray:
        ring_areas = edges[1:] ** 2 - edges[:-1] ** 2
        return ring_areas / np.sum(ring_areas)

    def _thermal_shape(self, temperature: np.ndarray) -> np.ndarray:
        exponent = -self.heat_of_transport / (R_GAS * temperature)
        exponent -= np.max(exponent)
        shape = np.exp(exponent)
        return shape

    @staticmethod
    def _thermal_target(
        inventory: np.ndarray,
        weights: np.ndarray,
        thermal_shape: np.ndarray,
    ) -> np.ndarray:
        total_amount = float(np.sum(inventory * weights))
        if total_amount <= 0.0:
            return np.zeros_like(inventory)
        return total_amount * thermal_shape / np.sum(weights * thermal_shape)

    @staticmethod
    def _scaling_factor(inventory: np.ndarray, produced: np.ndarray, weights: np.ndarray) -> np.ndarray:
        total_inventory = float(np.sum(inventory * weights))
        total_produced = float(np.sum(produced * weights))
        if total_produced <= 0.0 or total_inventory <= 0.0:
            return np.ones_like(inventory)

        reference = np.divide(
            produced,
            total_produced,
            out=np.zeros_like(produced),
            where=produced > 0.0,
        )
        redistributed = inventory / total_inventory
        return np.divide(
            redistributed,
            reference,
            out=np.zeros_like(inventory),
            where=reference > 0.0,
        )
