"""
Small inspection script for OxygenBalanceModel_PHENIX.

Run from this directory:

    python3 examples/phenix_oxygen_balance.py
"""
from __future__ import annotations

from dataclasses import asdict

import numpy as np

from oxired import OxygenBalanceModel_PHENIX


def print_block(title: str, rows: dict[str, float]) -> None:
    print(f"\n{title}")
    print("-" * len(title))
    for name, value in rows.items():
        print(f"{name:36s} {value:12.6f}")


def main() -> None:
    initial_om = 1.975
    burnup = 10.0
    mo_fraction = 1.0

    balance = OxygenBalanceModel_PHENIX()

    result = balance.target_average_om(
        initial_average_om=initial_om,
        burnup_at_percent=burnup,
        mo_oxidation_fraction=mo_fraction,
    )

    print("Phenix oxygen balance")
    print("=====================")
    print(f"initial O/M:                 {initial_om:.6f}")
    print(f"burnup:                      {burnup:.6f} at.%")
    print(f"Mo oxidation fraction requested: {mo_fraction:.6f}")
    print(f"Mo oxidation fraction actual:    {result.oxygen_mo_oxidation_fraction:.6f}")

    print_block("Aggregate balance", asdict(result))
    print_block("Matrix atom inventory", asdict(balance.matrix_atom_inventory(burnup)))
    print_block("Oxygen fixed by Zr, Sr, Nb, Y+RE", balance.fixed_sink_summary(burnup))

    print("\nMo oxidation fraction vs burnup")
    print("-------------------------------")
    print(f"{'burnup':>10s} {'actual Mo fraction':>18s} {'target O/M':>12s}")
    for bu in np.linspace(0.0, burnup, 11):
        bu_result = balance.target_average_om(
            initial_average_om=initial_om,
            burnup_at_percent=float(bu),
            mo_oxidation_fraction=mo_fraction,
        )
        print(
            f"{bu:10.3f} "
            f"{bu_result.oxygen_mo_oxidation_fraction:18.6f} "
            f"{bu_result.target_average_om:12.6f}"
        )

    print("\nElement contributions")
    print("---------------------")
    print(f"{'el':>3s} {'FP atoms':>10s} {'reacted':>9s} {'O/FP':>8s} {'O taken':>10s}")
    for c in balance.oxygen_sink_contributions(burnup, mo_fraction):
        # O taken = FP atoms produced * reacted fraction * valence/2
        print(
            f"{c.element:>3s} "
            f"{c.fp_atoms_per_100_initial_metal:10.6f} "
            f"{c.reacted_fraction:9.6f} "
            f"{c.oxygen_per_fp_atom:8.6f} "
            f"{c.oxygen_atoms_per_100_initial_metal:10.6f}"
        )


if __name__ == "__main__":
    main()
