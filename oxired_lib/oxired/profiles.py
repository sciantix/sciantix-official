"""
Temperature profiles for cylindrical pellets.
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Callable

import numpy as np

TemperatureProfile = Callable[[np.ndarray], np.ndarray]

@dataclass(frozen=True)
class PolynomialProfile:
    """Simple cylindrical temperature profile.

    T(r) = T_surface + (T_center - T_surface) * (1 - xi**power)
    with xi = (r - r_inner) / (r_outer - r_inner).
    """

    r_inner: float
    r_outer: float
    t_center: float
    t_surface: float
    power: float = 2.0

    def __call__(self, r: np.ndarray) -> np.ndarray:
        r = np.asarray(r, dtype=float)
        xi = (r - self.r_inner) / (self.r_outer - self.r_inner)
        xi = np.clip(xi, 0.0, 1.0)
        return self.t_surface + (self.t_center - self.t_surface) * (1.0 - xi ** self.power)
