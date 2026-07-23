"""
Elemental yields of fission products in a Phénix fuel pin at 13.4 at%
From Table A1, Samuelsson, K., Dumas, J. C., Sundman, B., Lamontagne, J., & Guéneau, C. (2020). 
Simulation of the chemical state of high burnup (U,Pu)O2 fuel in fast reactors based on thermodynamic calculations. Journal of Nuclear Materials, 532(1), 151969. https://doi.org/10.1016/j.jnucmat.2019.151969
(normalized by dividing the value by 13.4%)
Valence from J. Spino, P. Peerani / Journal of Nuclear Materials 375 (2008) 8-25. 
    0 is set if unknown / forming metallic precipitates / gas
"""
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class FissionYield:
    group: str
    element: str
    yield_percent_fp_per_fission: float
    valence: float
    note: str = ""


SOLID_SOLUTION = "FPs in solid solution"
OXIDE_PRECIPITATES = "FPs forming oxide precipitates"
METALLIC_PRECIPITATES = "FPs forming metallic precipitates"
GASES_AND_VOLATILE_FPS = "Gases and volatile FPs"
ACTINIDES = "Actinides"

PHENIX_FISSION_YIELDS: tuple[FissionYield, ...] = (
    FissionYield(SOLID_SOLUTION, "Y", 1.9, 3, "Y2O3"),
    FissionYield(SOLID_SOLUTION, "La", 5.7, 3, "La2O3"),
    FissionYield(SOLID_SOLUTION, "Ce", 10.4, 3, "Ce2O3/CeO2"),
    FissionYield(SOLID_SOLUTION, "Pr", 5.0, 3, "Pr2O3"),
    FissionYield(SOLID_SOLUTION, "Nd", 16.9, 3, "Nd2O3"),
    FissionYield(SOLID_SOLUTION, "Pm", 0.0, 3, "Negligible yield"),
    FissionYield(SOLID_SOLUTION, "Sm", 5.0, 3, "Assumed"),
    FissionYield(SOLID_SOLUTION, "Eu", 0.4, 3, "Assumed"),
    FissionYield(SOLID_SOLUTION, "Gd", 0.7, 3, "Assumed"),
    FissionYield(OXIDE_PRECIPITATES, "Rb", 1.1, 1),
    FissionYield(OXIDE_PRECIPITATES, "Cs", 17.0, 1),
    FissionYield(OXIDE_PRECIPITATES, "Sr", 2.7, 2, "SrO"),
    FissionYield(OXIDE_PRECIPITATES, "Ba", 9.9, 2),
    FissionYield(OXIDE_PRECIPITATES, "Zr", 20.3, 4),
    FissionYield(OXIDE_PRECIPITATES, "Nb", 0.0, 4), # contained in Zr yield
    FissionYield(METALLIC_PRECIPITATES, "Mo", 21.9, 4),
    FissionYield(METALLIC_PRECIPITATES, "Tc", 5.3, 4),
    FissionYield(METALLIC_PRECIPITATES, "Ru", 19.8, 0),
    FissionYield(METALLIC_PRECIPITATES, "Rh", 5.9, 0),
    FissionYield(METALLIC_PRECIPITATES, "Pd", 18.9, 0), # Pd + Ag + Cd + In + Sn + Sb
    FissionYield(METALLIC_PRECIPITATES, "Te", 3.7, 0),
    FissionYield(
        GASES_AND_VOLATILE_FPS,
        "He",
        0.8,
        0,
        "Produced by ternary fissions, alpha decay of some actinides, and high-energy neutron reactions with oxygen.",
    ),
    FissionYield(GASES_AND_VOLATILE_FPS, "Kr", 1.7, 0),
    FissionYield(GASES_AND_VOLATILE_FPS, "Xe", 23.4, 0),
    FissionYield(GASES_AND_VOLATILE_FPS, "Br", 0.1, 0),
    FissionYield(GASES_AND_VOLATILE_FPS, "I", 1.6, 0),
    FissionYield(ACTINIDES, "Am+Cm", 5.5, 4, "AmO2, CmO2"),
    FissionYield(ACTINIDES, "Np", 0.4, 4, "NpO2"),
)


def fission_yield_for_element(element: str) -> FissionYield:
    normalized = element.strip().casefold()
    for entry in PHENIX_FISSION_YIELDS:
        if entry.element.casefold() == normalized:
            return entry
    raise KeyError(f"unknown fission-product element: {element}")
