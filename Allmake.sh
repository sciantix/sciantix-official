#!/bin/bash
set -e

# Move to script directory
cd "${0%/*}" || exit 1

echo "== CONFIGURING BUILD DIRECTORY =="
mkdir -p build
cd build

echo "== RUNNING CMAKE =="
cmake ..

echo "== COMPILING SCIANTIX =="
make -j

echo "== BUILD DONE =="
