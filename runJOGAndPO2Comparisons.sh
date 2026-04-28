#!/bin/bash
set -e

cd "$(dirname "$0")"

echo "===== COMPILING SCIANTIX ====="
./Allclean.sh || true
./Allmake.sh

echo "===== SIMPLE TEST ====="
python3 regression/test_OC/run_test.py > regression/test_OC/out.log

echo ""
echo "===== UO2 pO2 VERIFICATION ====="

python3 regression/test_UO2_pO2_verification/run_temperature_sweep.py > regression/test_UO2_pO2_verification/out.log
python3 regression/test_UO2_pO2_verification/sciantix_verification/compare_sciantix_with_oc_csv.py

echo ""
echo "===== MOX pO2 VERIFICATION ====="

python3 regression/test_MOX_pO2_verification/run_temperature_sweep.py > regression/test_MOX_pO2_verification/out.log
python3 regression/test_MOX_pO2_verification/sciantix_verification/compare_sciantix_with_kato.py

echo "===== DONE ====="