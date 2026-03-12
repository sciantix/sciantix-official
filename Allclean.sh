#!/bin/bash
set -e

# Move to script directory
cd "${0%/*}" || exit 1

echo "== CLEANING BUILD AND PYTHON CACHE =="
rm -rf build
rm -rf obj

echo "== CLEANING __pycache__ =="
find . -type d -name "__pycache__" -exec rm -rf {} +

echo "== CLEANING sciantix.x binaries =="
find . -type f -name "sciantix.x" -exec rm -f {} +

echo "== CLEAN DONE =="
