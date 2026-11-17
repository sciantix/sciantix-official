#!/bin/bash
# author: E.Cappellari, POLIMI-CEA 2026
set -e  # stop on error

# =============================================================================
# The whole loop of the HBS fragmentation option 4 (rupture per pore class) in one command.
#
#   1. build SCIANTIX
#   2. run the NFIR cases of option 4 and compare them with their gold files, through the regression runner:
#      test_UO2HBS_frag_j19damage (mean field of J19), test_UO2HBS_frag_mmm_unit and _mmm_pk (s_hat of MMM,
#      input_hbs_shat.txt, sigma_R calibrated by calibrate_sigma_r.py)
#   3. draw the figures of the model (pore EoS, classes, rupture, release; used by the paper draft), write the inputs
#      for MMM, and draw the figures of the cases in regression/hbs_nfir/figures/<case>
#
# Options 1, 2, 3 are not run.
#
# Usage
#   ./utilities/HBFF/runHBFF.sh 
# =============================================================================

# Choices
BUILD=1   # 1 build SCIANTIX, 0 reuse the existing build/sciantix.x
RUN=1     # 1 run the case, 0 reuse its existing output.txt
GOLD=0    # 1 re-bless the gold file of the case (only after checking that the change is meant)

cd "$(dirname "$0")/../.."
ROOT="$PWD"
CASE="regression/hbs_nfir/test_UO2HBS_frag_j19damage"

if [ "$BUILD" -eq 1 ]; then
    echo "===== COMPILING SCIANTIX ====="
    ./Allmake.sh
else
    echo "===== BUILD SKIPPED (BUILD=0) ====="
fi
if [ ! -x "$ROOT/build/sciantix.x" ]; then
    echo "$ROOT/build/sciantix.x is not there -- set BUILD=1" >&2
    exit 1
fi

if [ "$RUN" -eq 1 ]; then
    MODE_GOLD=$GOLD
else
    MODE_GOLD=$((2 + GOLD))
fi

echo ""
echo "===== NFIR CASES OF OPTION 4 AND THEIR GOLD (regression runner, --mode-gold $MODE_GOLD) ====="
GOLD_STATUS=0
python3 -m regression.runner --hbs_nfir.j19damage --hbs_nfir.mmm_unit --hbs_nfir.mmm_pk --mode-gold "$MODE_GOLD" || GOLD_STATUS=$?
if [ ! -f "$CASE/output.txt" ]; then
    echo "$CASE/output.txt is not there -- set RUN=1" >&2
    exit 1
fi

echo ""
echo "===== PYTHON: FIGURES, INPUTS FOR MMM, STATES OF pore_distribution.py AGAINST output.txt ====="
CHECK_STATUS=0
python3 utilities/HBFF/pore_distribution.py || CHECK_STATUS=$?

echo ""
echo "===== FIGURES OF THE CASES AGAINST THE DATA (regression/hbs_nfir/figures/<case>) ====="
python3 regression/hbs_nfir/plot.py j19damage mmm_unit mmm_pk

echo ""
if [ "$GOLD_STATUS" -ne 0 ] || [ "$CHECK_STATUS" -ne 0 ]; then
    echo "===== DONE, WITH FAILURES ====="
    [ "$GOLD_STATUS" -ne 0 ] && echo "a case disagrees with its gold (or has none): regression/report.html;" \
        "re-bless with GOLD=1 only after checking that the change is meant"
    [ "$CHECK_STATUS" -ne 0 ] && echo "pore_distribution.py: STATES is stale against output.txt, see its lines above"
    echo "figures in $ROOT/utilities/HBFF/figures and $ROOT/regression/hbs_nfir/figures/{j19damage,mmm_unit,mmm_pk}"
    exit 1
fi
echo "===== DONE ====="
echo "figures in $ROOT/utilities/HBFF/figures and $ROOT/regression/hbs_nfir/figures/{j19damage,mmm_unit,mmm_pk}"
