#!/bin/bash

# go to repo root (directory containing this script)
cd "$(dirname "$0")"

echo "=== Running Baker parity plot ==="
python3 validation/baker/parity_plot.py

echo ""
echo "=== Running White parity plot ==="
python3 validation/white/parity_plot.py

echo ""
echo "=== Running GPR parity plot ==="
python3 verification/test_gpr/parity_plot.py

echo ""
echo "=== Running Chromium plot ==="
python3 validation/chromium/plot.py

echo ""
echo "=== Running Contact plot ==="
python3 validation/contact/plot.py

echo ""
echo "=== Running Cornell plot ==="
python3 validation/cornell/parity_plot.py

echo ""
echo "=== Running HBS plot ==="
python3 validation/hbs/plot.py

echo ""
echo "=== Running Kashibe plot ==="
python3 validation/kashibe/plot.py

echo ""
echo "=== Running Oxidation plot ==="
python3 verification/test_oxidation/plot.py

echo ""
echo "=== Running Talip plot ==="
python3 validation/talip/plot.py

echo ""
echo "=== Running JOG plot ==="
python3 validation/jog/PHENIXpins/plot_JOG.py

echo ""
echo "=== Running oxygen-potential plot ==="
python3 validation/oxygenpotential/freshfuel/plot.py
python3 validation/oxygenpotential/burnup/plot.py

echo ""
echo "=== Running MOX pO2 verification sweep + plots ==="
python3 verification/test_MOX_po2/run_temperature_sweep.py \
    --temperatures 1400,1800,2200 --q-values 0.10,0.20,0.30

echo ""
echo "=== Done ==="
