#!/bin/bash
set -e

# Move to script directory
SCI_ROOT="$(cd "${0%/*}" && pwd)"
cd "$SCI_ROOT" || exit 1

if [ -t 1 ]; then
  C_RESET=$'\033[0m'
  C_HEADER=$'\033[1;36m'
  C_BUILD=$'\033[0;32m'
  C_LINK=$'\033[1;32m'
  C_OK=$'\033[1;32m'
  C_FAIL=$'\033[1;31m'
else
  C_RESET=''; C_HEADER=''; C_BUILD=''; C_LINK=''; C_OK=''; C_FAIL=''
fi

header() { printf '%s== %s ==%s\n' "$C_HEADER" "$1" "$C_RESET"; }

OC=0
FORCE=0
for arg in "$@"; do
  case "$arg" in
    --oc) OC=1 ;;
    --force) FORCE=1 ;;
    *) echo "Unknown argument: $arg" >&2; exit 1 ;;
  esac
done

oc_make_flags=()
sci_make_flags=()
if [ "$FORCE" -eq 1 ]; then
  oc_make_flags+=(-B)
  sci_make_flags+=(-B)
fi

if [ "$OC" -eq 1 ]; then
  OC_ROOT="$(cd "${SCI_ROOT}/../opencalphad-for-sciantix" && pwd)"
  OC_FCOPT="-O2 -fPIC"

  header "BUILDING OPENCALPHAD OCASI"

  oc_eval=$'__oc_print_targets:\n\t@echo $(OBJS) $(LIBS)'
  oc_targets=$(make -C "$OC_ROOT" --no-print-directory --eval="$oc_eval" __oc_print_targets 2>/dev/null)

  oc_total=$(make -C "$OC_ROOT" --no-print-directory -n -B $oc_targets FCOPT="$OC_FCOPT" 2>/dev/null \
    | grep -Ec '^(gfortran|gcc) -c ')
  [ "$oc_total" -gt 0 ] || oc_total=1

  oc_log="$(mktemp)"
  set +e
  make -C "$OC_ROOT" --no-print-directory "${oc_make_flags[@]}" OCASI FCOPT="$OC_FCOPT" 2>&1 | tee "$oc_log" | awk \
    -v total="$oc_total" -v c_build="$C_BUILD" -v c_link="$C_LINK" -v c_reset="$C_RESET" '
    /^(gfortran|gcc) -c / {
      step++
      pct = int(step * 100 / total)
      if (pct > 100) pct = 100
      lang = ($1 == "gfortran") ? "Fortran" : "C"
      out = ""
      for (i = 1; i <= NF; i++) if ($i == "-o") out = $(i + 1)
      printf("%s[%3d%%] Building %s object %s%s\n", c_build, pct, lang, out, c_reset)
      next
    }
    /^ar sq / { printf("%s[100%%] Linking static library %s%s\n", c_link, $3, c_reset); next }
    { next }
  '
  oc_status=${PIPESTATUS[0]}
  set -e

  if [ "$oc_status" -ne 0 ]; then
    printf '%s== OPENCALPHAD BUILD FAILED ==%s\n' "$C_FAIL" "$C_RESET"
    tail -n 60 "$oc_log"
    rm -f "$oc_log"
    exit "$oc_status"
  fi
  rm -f "$oc_log"
  header "OPENCALPHAD BUILD DONE"

  header "GENERATING OPENCALPHAD LIBRARY"
  nm -g --defined-only "$OC_ROOT/build/liboctq-isoc.a" \
    | awk 'NF >= 3 { print $3 " gb_" $3 }' \
    > /tmp/oc_gb_symbols.map

  objcopy --redefine-syms=/tmp/oc_gb_symbols.map \
    "$OC_ROOT/build/liboctq-isoc.a" "$OC_ROOT/build/liboctq-isoc-gb.a"

  printf '  -> %s symbols renamed -> %s\n' \
    "$(wc -l < /tmp/oc_gb_symbols.map)" "$OC_ROOT/build/liboctq-isoc-gb.a"
fi

mkdir -p build
cd build

header "RUNNING CMAKE"
cmake ..

header "COMPILING SCIANTIX"
make -j "${sci_make_flags[@]}"

printf '%s== BUILD DONE ==%s\n' "$C_OK" "$C_RESET"
