#!/bin/bash
set -e  # stop on error

# Move to script location (project root)
cd "$(dirname "$0")"

echo "===== COMPILING SCIANTIX ====="
./Allclean.sh || true
allmake_flags=()
if [[ " $* " == *" --oc "* ]]; then
  allmake_flags+=(--oc)
fi
if [[ " $* " == *" --tc "* ]]; then
  allmake_flags+=(--tc)
fi
./Allmake.sh "${allmake_flags[@]}"

# Determine number of jobs (nproc if available, else 4)
JOBS=$(nproc 2>/dev/null || echo 4)

echo ""
echo "===== RUNNING TESTS (using $JOBS threads) ====="
python3 -m testing.runner -j "$JOBS" "$@"

echo ""
echo "===== DONE ====="
