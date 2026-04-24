#!/bin/bash
set -e

cd "$(dirname "$0")"

echo "===== COMPILING SCIANTIX ====="
./Allclean.sh || true
./Allmake.sh

echo ""
echo "===== UO2 pO2 VERIFICATION ====="

python3 regression/test_UO2_pO2_verification/run_temperature_sweep.py > out.log
python3 regression/test_UO2_pO2_verification/sciantix_verification/compare_sciantix_with_oc_csv.py

echo ""
echo "===== MOX pO2 VERIFICATION ====="

python3 regression/test_MOX_pO2_verification/run_temperature_sweep.py > out.log
python3 regression/test_MOX_pO2_verification/sciantix_verification/compare_sciantix_with_kato.py
python3 regression/test_MOX_pO2_verification/sciantix_verification/compare_sciantix_with_oc_csv.py

echo ""
echo "===== JOG COMPARISON UO2 ====="
python3 regression/test_JOG/run_and_plot_test.py

echo "===== JOG COMPARISON MOX ====="
python3 regression/test_JOG_MOX/run_and_plot_test.py
echo "===== DONE ====="


