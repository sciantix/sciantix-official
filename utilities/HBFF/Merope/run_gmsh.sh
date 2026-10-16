#!/bin/bash
# author: E.Cappellari, POLIMI-CEA 2026

# =============================================================================
# 3D mesh of every mesh_*.geo written by Mérope in mmm_inputs (merope_pore_pressure.py) and
# test_rve (test_rve.py) of the current directory: 
# - mesh_<name>.msh and 
# - its log mesh_<name>.log next to each .geo.
#
# Usage, from the directory where the Python scripts were run
#   ./run_gmsh.sh
# =============================================================================

for geo in $(find mmm_inputs test_rve -name "mesh_*.geo" 2>/dev/null | sort); do
    echo "$geo"
    gmsh "$geo" -3 -o "${geo%.geo}.msh" > "${geo%.geo}.log" 2>&1
done
