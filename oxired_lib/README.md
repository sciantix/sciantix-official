# oxired

Small Python library for radial oxygen redistribution in a cylindrical oxide fuel pellet.

## Scope

This package implements the cylindrical, dilute-solution form of the OXIRED model described by Lassmann.

Implemented pieces:
- steady-state treatment based on Sari and Schumacher
- transient approximation from Lassmann toward the steady state
- hypostoichiometric mixed oxide `(U,Pu)O_{2-x}` and hyperstoichiometric `UO_{2+x}`
- simple burnup-dependent average O/M shift

## Install

```bash
pip install -e .
```