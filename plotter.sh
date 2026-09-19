#!/bin/bash

# go to repo root (directory containing this script)
cd "$(dirname "$0")"

echo "=== Running Baker plot ==="
python3 regression/baker/plot.py

echo ""
echo "=== Running White plot ==="
python3 regression/white/plot.py

echo ""
echo "=== Running GPR plot ==="
python3 regression/gpr/plot.py

echo ""
echo "=== Running Chromium plot ==="
python3 regression/chromium/plot.py

echo ""
echo "=== Running Contact plot ==="
python3 regression/contact/plot.py

echo ""
echo "=== Running Cornell plot ==="
python3 regression/cornell/plot.py

echo ""
echo "=== Running HBS plot ==="
python3 regression/hbs/plot.py

echo ""
echo "=== Running Kashibe plot ==="
python3 regression/kashibe/plot.py

echo ""
echo "=== Running Oxidation plot ==="
python3 regression/oxidation/plot.py

echo ""
echo "=== Running Talip plot ==="
python3 regression/talip/plot.py

echo ""
echo "=== Running NUS plot ==="
python3 regression/nus/plot.py

echo ""
echo "=== Done ==="
