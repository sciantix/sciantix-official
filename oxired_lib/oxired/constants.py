"""
Constants and semi-empirical correlations for the OXIRED model.
These correlations are taken from Lassmann (1987) and Sari & Schumacher (1976).
"""
from __future__ import annotations
from math import exp

R_GAS = 8.31446261815324  # J/mol/K

# Matze, reported in Lassman 
DIFFUSION_PREEXP = 1.39e-6  # m^2/s
DIFFUSION_Q = 75.9e3  # J/mol


def diffusion_coefficient(T: float) -> float:
    return DIFFUSION_PREEXP * exp(-DIFFUSION_Q / (R_GAS * T))


def uranium_valence_hyper(om: float, pu_fraction: float) -> float:
    """Average uranium valence in hyperstoichiometric (U1-q,Puq)O2+x."""
    if pu_fraction >= 1.0:
        raise ValueError("uranium valence is undefined for pu_fraction >= 1")
    return 4.0 + 2.0 * (om - 2.0) / (1.0 - pu_fraction)


def plutonium_valence_hypo(om: float, pu_fraction: float) -> float:
    """Average plutonium valence in hypostoichiometric (U1-q,Puq)O2-x."""
    if pu_fraction <= 0.0:
        raise ValueError("plutonium valence is undefined for pu_fraction <= 0")
    return 4.0 + 2.0 * (om - 2.0) / pu_fraction


def heat_of_transport_interstitial(om: float, pu_fraction: float) -> float:
    """Qi [J/mol] for hyperstoichiometric oxide.

    Lassmann adopts the Sari/Schumacher fit:
        Qi = -3.5e34 * exp(-17 * Vu)
    where Vu is the average uranium valence.
    """
    vu = uranium_valence_hyper(om=om, pu_fraction=pu_fraction)
    if vu < 4: 
        raise ValueError("not valid heat of transport of oxygen interstitial atoms")
        
    return -3.5e34 * exp(-17.0 * vu)


def heat_of_transport_vacancy(om: float, pu_fraction: float) -> float:
    """Qv [J/mol] for hypostoichiometric oxide.

    Lassmann uses the smooth replacement:
        Qv = -8.12e-4 * exp(4.85 * Vpu)
    where Vpu is the average plutonium valence.
    """
    vpu = plutonium_valence_hypo(om=om, pu_fraction=pu_fraction)
    if vpu > 4:
        raise ValueError("not valid heat of transport of oxygen vacancies")

    return -8.12e-4 * exp(4.85 * vpu)
