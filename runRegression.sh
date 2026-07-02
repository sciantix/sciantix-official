#!/bin/bash
set -e  # stop on error

# Move to script location (project root)
cd "$(dirname "$0")"

echo "===== COMPILING SCIANTIX ====="
./Allclean.sh || true
./Allmake.sh

# Determine number of jobs (nproc if available, else 4)
JOBS=$(nproc 2>/dev/null || echo 4)

echo ""
echo "===== RUNNING REGRESSION TESTS (using $JOBS threads) ====="
python3 -m regression.runner -j "$JOBS" "$@"

echo ""
echo "===== DONE ====="
