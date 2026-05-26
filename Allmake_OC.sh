#!/bin/bash
set -e

SCI_ROOT="$(cd "$(dirname "$0")" && pwd)"
OC_ROOT="${SCI_ROOT}/../opencalphad"

echo "== BUILDING OPENCALPHAD OCASI =="
cd "$OC_ROOT"
make -B OCASI FCOPT="-O2 -fPIC"

echo "== GENERATING GRAIN-BOUNDARY OPENCALPHAD LIBRARY =="
nm -g --defined-only liboctq-isoc.a \
  | awk 'NF >= 3 { print $3 " gb_" $3 }' \
  > /tmp/oc_gb_symbols.map

objcopy --redefine-syms=/tmp/oc_gb_symbols.map \
  liboctq-isoc.a liboctq-isoc-gb.a

echo "== CONFIGURING SCIANTIX =="
cd "$SCI_ROOT"
cmake -S . -B build

echo "== BUILDING SCIANTIX =="
cmake --build build --target sciantix -j

echo "== BUILD DONE =="