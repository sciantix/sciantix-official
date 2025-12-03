#!/bin/bash

# go to repo root (directory containing this script)
cd "$(dirname "$0")"

echo "=== Running Baker parity plot ==="
python3 regression/baker/parity_plot.py

echo "=== Running White parity plot ==="
python3 regression/white/parity_plot.py

echo "=== Done ==="
