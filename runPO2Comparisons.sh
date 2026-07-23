#!/bin/bash
set -e

cd "$(dirname "$0")"

echo "===== COMPILING SCIANTIX ====="
./Allclean.sh || true
./Allmake.sh --oc

echo "===== MOX pO2 VERIFICATION ====="

python3 regression/test_MOX_pO2_verification/run_temperature_sweep.py > out.log
python3 regression/test_MOX_pO2_verification/sciantix_verification/compare_sciantix_with_kato.py
python3 regression/test_MOX_pO2_verification/sciantix_verification/compare_sciantix_with_oc_csv.py

echo "===== DONE ====="

