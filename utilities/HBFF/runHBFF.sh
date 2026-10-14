#!/bin/bash
# author: E.Cappellari, POLIMI-CEA 2026
set -e  # stop on error

# =============================================================================
# The whole loop of the HBS fragmentation option 4 (rupture per pore class) in one command.
#
#   1. build SCIANTIX
#   2. run the NFIR case of option 4 (regression/hbs_nfir/test_UO2HBS_frag_j19damage) and compare it
#      with its gold file, through the regression runner
#   3. check the C++ against its Python transcription (pore_distribution.py)
#   4. draw the figures of README_HBFFmodel.md (pore EoS, classes, rupture, release), write the inputs
#      for MMM, and draw the figures of the case in regression/hbs_nfir/figures/j19damage
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
echo "===== NFIR CASE OF OPTION 4 AND ITS GOLD (regression runner, --mode-gold $MODE_GOLD) ====="
GOLD_STATUS=0
python3 -m regression.runner --hbs_nfir.j19damage --mode-gold "$MODE_GOLD" || GOLD_STATUS=$?
if [ ! -f "$CASE/output.txt" ]; then
    echo "$CASE/output.txt is not there -- set RUN=1" >&2
    exit 1
fi

echo ""
echo "===== PYTHON TRANSCRIPTION: FIGURES, INPUTS FOR MMM, CHECK OF THE C++ OF OPTION 4 ====="
CHECK_STATUS=0
python3 utilities/HBFF/pore_distribution.py || CHECK_STATUS=$?

echo ""
echo "===== FIGURES OF THE CASE AGAINST THE DATA (regression/hbs_nfir/figures/j19damage) ====="
python3 regression/hbs_nfir/plot.py j19damage

echo ""
if [ "$GOLD_STATUS" -ne 0 ] || [ "$CHECK_STATUS" -ne 0 ]; then
    echo "===== DONE, WITH FAILURES ====="
    [ "$GOLD_STATUS" -ne 0 ] && echo "the case disagrees with its gold (or has none): regression/report.html;" \
        "re-bless with GOLD=1 only after checking that the change is meant"
    [ "$CHECK_STATUS" -ne 0 ] && echo "pore_distribution.py: the replay disagrees with output.txt, see its table above"
    echo "figures in $ROOT/utilities/HBFF/figures and $ROOT/regression/hbs_nfir/figures/j19damage"
    exit 1
fi
echo "===== DONE ====="
echo "figures in $ROOT/utilities/HBFF/figures and $ROOT/regression/hbs_nfir/figures/j19damage"
