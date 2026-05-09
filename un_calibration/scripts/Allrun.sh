#!/usr/bin/env bash
# Allrun.sh — execute every UN-calibration script in sequence and regenerate
# the contents of un_calibration/reports/. Run from anywhere; the script
# resolves its own location.
#
# Usage:
#   ./Allrun.sh                 # run all scripts
#   ./Allrun.sh --quick         # skip the long scans (sensitivity_scan,
#                                  rho_d_laws_comparison, calibrate_f_n)
#   PYTHON=python3.12 ./Allrun.sh    # override Python interpreter
#
# Output: each script writes PNG + CSV to
#         un_calibration/reports/<script_name>/

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PYTHON="${PYTHON:-python3}"

QUICK=0
if [[ "${1:-}" == "--quick" ]]; then
    QUICK=1
fi

# Order:
#   1. smoke test (validate model)
#   2. single-config figures (fast, ~10s each)
#   3. multi-config studies (slow, minutes)
SCRIPTS_FAST=(
    smoke_test
    fig3_swelling_vs_T
    fig4_diffusivity_vs_T
    fig78_NdRd_vs_T
    fig9_gas_partition
    fig11_FGR_vs_burnup
    rho_d_diagnostic
    flag_ablation
)
SCRIPTS_SLOW=(
    rho_d_laws_comparison
    sensitivity_scan
    calibrate_f_n
)

run_one() {
    local name="$1"
    local path="$SCRIPT_DIR/$name.py"
    if [[ ! -f "$path" ]]; then
        echo "  [skip] $name.py not found"
        return
    fi
    echo
    echo "============================================================"
    echo "  $name.py"
    echo "============================================================"
    "$PYTHON" "$path"
}

start_t=$SECONDS

for s in "${SCRIPTS_FAST[@]}"; do
    run_one "$s"
done

if (( QUICK == 0 )); then
    for s in "${SCRIPTS_SLOW[@]}"; do
        run_one "$s"
    done
else
    echo
    echo "  --quick mode: skipped $(echo "${SCRIPTS_SLOW[@]}" | tr ' ' ',')"
fi

elapsed=$(( SECONDS - start_t ))
echo
echo "============================================================"
printf "  All done in %02d:%02d.\n" $((elapsed/60)) $((elapsed%60))
echo "  Reports in: un_calibration/reports/"
echo "============================================================"
