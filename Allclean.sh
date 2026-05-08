#!/bin/bash
set -e

# Move to script directory
cd "${0%/*}" || exit 1

echo "== CLEANING BUILD AND PYTHON CACHE =="

# Cleaning all Zone.Identifier files
find . -type f -name '*:Zone.Identifier' -exec rm -f {} \;

rm -rf build
rm -rf obj

echo "== CLEANING __pycache__ =="
find . -type d -name "__pycache__" -exec rm -rf {} +

echo "== CLEANING sciantix.x binaries =="
find . -type f -name "sciantix.x" -exec rm -f {} +

echo "== CLEAN DONE =="
