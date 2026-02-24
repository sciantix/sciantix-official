#!/bin/bash
set -e  # stop on error

# Move to script location (project root)
cd "$(dirname "$0")"

echo "===== COMPILING SCIANTIX ====="
./Allclean.sh || true
./Allmake.sh

echo ""
echo "===== RUNNING REGRESSION TESTS ====="
python3 -m regression.runner "$@"

echo ""
echo "===== DONE ====="
