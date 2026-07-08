"""Theoretical (crystallographic) densities for the Ba-molybdate JOG phases.

rho = Z * M / (N_A * V_cell), Z = formula units per cell.

Unit cells (ref. Smith, J. Eur. Ceram. Soc. 2021,
https://doi.org/10.1016/j.jeurceramsoc.2021.01.010):
    BaMoO4:  I41/a,  a = 0.5571 nm,               c = 1.2783 nm, Z = 4
    Ba2MoO5: Pnma,   a = 0.7412 nm, b = 0.5769 nm, c = 1.1380 nm, Z = 4
    Ba3MoO6: Fm-3m,  a = 0.8600 nm,                               Z = 4

Values here feed the 8th (density, g/cm3) column of
regression/JOG/PHENIXpins/input_thermochemistry.txt, read by
ThermochemistryManifest.C / used in JOGFormation.C.
"""

AVOGADRO_NUMBER = 6.02214076e23

M_BA = 137.327
M_MO = 95.95
M_O = 15.999


def crystal_density_g_per_cm3(molar_mass, z, a_nm, b_nm, c_nm):
    v_cm3 = (a_nm * b_nm * c_nm) * 1.0e-21
    return z * molar_mass / (AVOGADRO_NUMBER * v_cm3)


PHASES = {
    "BAMOO4": dict(
        molar_mass=M_BA + M_MO + 4 * M_O,
        z=4, a_nm=0.5571, b_nm=0.5571, c_nm=1.2783,
    ),
    "BA2MOO5": dict(
        molar_mass=2 * M_BA + M_MO + 5 * M_O,
        z=4, a_nm=0.7412, b_nm=0.5769, c_nm=1.1380,
    ),
    "BA3MOO6": dict(
        molar_mass=3 * M_BA + M_MO + 6 * M_O,
        z=4, a_nm=0.8600, b_nm=0.8600, c_nm=0.8600,
    ),
}


if __name__ == "__main__":
    for name, cell in PHASES.items():
        rho = crystal_density_g_per_cm3(**cell)
        print(f"{name}: {rho:.4f} g/cm3")
