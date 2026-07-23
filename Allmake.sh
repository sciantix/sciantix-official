#!/bin/bash
set -e

# Move to script directory
SCI_ROOT="$(cd "${0%/*}" && pwd)"
cd "$SCI_ROOT" || exit 1

OC=0
for arg in "$@"; do
  case "$arg" in
    --oc) OC=1 ;;
    *) echo "Unknown argument: $arg" >&2; exit 1 ;;
  esac
done

if [ "$OC" -eq 1 ]; then
  OC_ROOT="${SCI_ROOT}/../opencalphad-for-sciantix"

  echo "== BUILDING OPENCALPHAD OCASI =="
  ( cd "$OC_ROOT" && make -B OCASI FCOPT="-O2 -fPIC" )

  echo "== GENERATING OPENCALPHAD LIBRARY =="
  nm -g --defined-only "$OC_ROOT/build/liboctq-isoc.a" \
    | awk 'NF >= 3 { print $3 " gb_" $3 }' \
    > /tmp/oc_gb_symbols.map

  objcopy --redefine-syms=/tmp/oc_gb_symbols.map \
    "$OC_ROOT/build/liboctq-isoc.a" "$OC_ROOT/build/liboctq-isoc-gb.a"
fi

echo "== CONFIGURING BUILD DIRECTORY =="
mkdir -p build
cd build

echo "== RUNNING CMAKE =="
cmake ..

echo "== COMPILING SCIANTIX =="
make -j

echo "== BUILD DONE =="
