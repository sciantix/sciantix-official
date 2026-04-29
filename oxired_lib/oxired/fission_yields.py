"""
Elemental yields of fission products in a Phénix fuel pin at 10 at%
From Table 2, "Fuel Performance of Fast Spectrum Oxide Fuel", 
    Michel Pelletier and Yannick Guérin

Valence from J. Spino, P. Peerani / Journal of Nuclear Materials 375 (2008) 8-25. 
    0 is set if unknown / forming metallic precipitates / gas
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable


@dataclass(frozen=True)
class FissionYield:
    group: str
    element: str
    yield_percent_fp_per_fission: float
    valence: float
    note: str = ""

    @property
    def atoms_per_10_fissions(self) -> float:
        return self.yield_percent_fp_per_fission / 10.0


SOLID_SOLUTION = "FPs in solid solution"
OXIDE_PRECIPITATES = "FPs forming oxide precipitates"
METALLIC_PRECIPITATES = "FPs forming metallic precipitates"
GASES_AND_VOLATILE_FPS = "Gases and volatile FPs"

PHENIX_FISSION_YIELDS: tuple[FissionYield, ...] = (
    FissionYield(SOLID_SOLUTION, "Y", 1.9, 4),
    FissionYield(SOLID_SOLUTION, "La", 5.6, 4),
    FissionYield(SOLID_SOLUTION, "Ce", 11.4, 4),
    FissionYield(SOLID_SOLUTION, "Pr", 4.8, 4),
    FissionYield(SOLID_SOLUTION, "Nd", 15.5, 4),
    FissionYield(SOLID_SOLUTION, "Pm", 1.2, 4),
    FissionYield(SOLID_SOLUTION, "Sm", 3.4, 4),
    FissionYield(SOLID_SOLUTION, "Eu", 0.6, 4),
    FissionYield(SOLID_SOLUTION, "Gd", 0.4, 4),
    FissionYield(OXIDE_PRECIPITATES, "Rb", 1.5, 1),
    FissionYield(OXIDE_PRECIPITATES, "Cs", 20.1, 1),
    FissionYield(OXIDE_PRECIPITATES, "Sr", 3.6, 4),
    FissionYield(OXIDE_PRECIPITATES, "Ba", 6.8, 2),
    FissionYield(OXIDE_PRECIPITATES, "Zr", 19.2, 4),
    FissionYield(OXIDE_PRECIPITATES, "Nb", 0.2, 4),
    FissionYield(METALLIC_PRECIPITATES, "Mo", 21.3, 4),
    FissionYield(METALLIC_PRECIPITATES, "Tc", 5.5, 4),
    FissionYield(METALLIC_PRECIPITATES, "Ru", 22.0, 0),
    FissionYield(METALLIC_PRECIPITATES, "Rh", 5.8, 0),
    FissionYield(METALLIC_PRECIPITATES, "Pd", 13.8, 0),
    FissionYield(METALLIC_PRECIPITATES, "Ag", 1.3, 0),
    FissionYield(METALLIC_PRECIPITATES, "Cd", 1.0, 2),
    FissionYield(METALLIC_PRECIPITATES, "In", 0.1, 0),
    FissionYield(METALLIC_PRECIPITATES, "Sn", 0.5, 4),
    FissionYield(METALLIC_PRECIPITATES, "Sb", 0.2, 3),
    FissionYield(METALLIC_PRECIPITATES, "Te", 3.5, 0),
    FissionYield(
        GASES_AND_VOLATILE_FPS,
        "He",
        0.8,
        0,
        "Produced by ternary fissions, alpha decay of some actinides, and high-energy neutron reactions with oxygen.",
    ),
    FissionYield(GASES_AND_VOLATILE_FPS, "Kr", 1.8, 0),
    FissionYield(GASES_AND_VOLATILE_FPS, "Xe", 23.6, 0),
    FissionYield(GASES_AND_VOLATILE_FPS, "Br", 0.1, 0),
    FissionYield(GASES_AND_VOLATILE_FPS, "I", 1.7, 0),
)

PHENIX_FISSION_YIELD_SOURCE = (
    "Elemental yields of fission products in a Phenix fuel pin at 10 at% "
    "from Pelletier and Guerin, Fuel Performance of Fast Spectrum Oxide Fuel, Table 2."
)

def fission_yield_for_element(element: str) -> FissionYield:
    normalized = element.strip().casefold()
    for entry in PHENIX_FISSION_YIELDS:
        if entry.element.casefold() == normalized:
            return entry
    raise KeyError(f"unknown fission-product element: {element}")


def sum_yields(entries: Iterable[FissionYield]) -> float:
    return sum(entry.yield_percent_fp_per_fission for entry in entries)
