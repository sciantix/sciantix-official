#!/bin/bash
# author: E.Cappellari, POLIMI-CEA 2026
set -e  # stop on error

# =============================================================================
# The whole loop of the HBS fragmentation option 4 (rupture per pore class) in one command.
#
#   1. build SCIANTIX
#   2. run the NFIR case of option 4 (regression/hbs_nfir/test_UO2HBS_frag_j19damage) and compare it
#      with its gold file, through the regression runner
#   3. check the C++ against its Python transcription (pore_distribution.py --check)
#   4. draw the figures of README_HBFFmodel.md (pore EoS, classes, rupture, release), write the inputs
#      for MMM, and draw the figures of the case in regression/hbs_nfir/figures/j19damage
#
# Options 1, 2, 3 are not run.
#
# Usage
#   ./utilities/HBFF/runHBFF.sh              # the whole thing
#   ./utilities/HBFF/runHBFF.sh --no-build   # reuse the existing build/sciantix.x
#   ./utilities/HBFF/runHBFF.sh --no-run     # reuse the existing output.txt of the case
#   ./utilities/HBFF/runHBFF.sh --gold       # re-bless the gold file of the case
# =============================================================================

# Move to the project root, whatever directory this was called from.
cd "$(dirname "$0")/../.."
ROOT="$PWD"
CASE="regression/hbs_nfir/test_UO2HBS_frag_j19damage"

BUILD=1
RUN=1
GOLD=0

for arg in "$@"; do
    case "$arg" in
        --no-build) BUILD=0 ;;
        --no-run)   RUN=0 ;;
        --gold)     GOLD=1 ;;
        *)          echo "unknown option: $arg" >&2; exit 1 ;;
    esac
done

if [ "$BUILD" -eq 1 ]; then
    echo "===== COMPILING SCIANTIX ====="
    ./Allmake.sh
else
    echo "===== BUILD SKIPPED (--no-build) ====="
fi
if [ ! -x "$ROOT/build/sciantix.x" ]; then
    echo "$ROOT/build/sciantix.x is not there -- drop --no-build" >&2
    exit 1
fi

# Modes of the regression runner: 0 run and compare with the gold, 1 run and write the gold,
# 2 compare the existing output, 3 write the gold from the existing output.
if [ "$RUN" -eq 1 ]; then
    MODE_GOLD=$GOLD
else
    MODE_GOLD=$((2 + GOLD))
fi

# A disagreement with the gold, or between the C++ and its transcription, must not stop the figures:
# the curves are how one finds out what moved. The two statuses are carried to the end instead.
echo ""
echo "===== NFIR CASE OF OPTION 4 AND ITS GOLD (regression runner, --mode-gold $MODE_GOLD) ====="
GOLD_STATUS=0
python3 -m regression.runner --hbs_nfir.j19damage --mode-gold "$MODE_GOLD" || GOLD_STATUS=$?
if [ ! -f "$CASE/output.txt" ]; then
    echo "$CASE/output.txt is not there -- drop --no-run" >&2
    exit 1
fi

# Option 4 is transcribed in pore_distribution.py: the replay of the recursion on the history of the
# case must give back D, G, S, S_2 and the broken fraction of every class of output.txt, and the states
# that README_HBFFmodel.md quotes must be those of the output.
echo ""
echo "===== CHECKING THE C++ OF OPTION 4 AGAINST ITS PYTHON TRANSCRIPTION ====="
CHECK_STATUS=0
python3 utilities/HBFF/pore_distribution.py --check "$CASE" || CHECK_STATUS=$?

echo ""
echo "===== FIGURES: PORE EoS, CLASSES, RUPTURE AND RELEASE (utilities/HBFF/figures) ====="
python3 utilities/HBFF/pore_distribution.py

echo ""
echo "===== INPUTS FOR MMM (utilities/HBFF/mmm_inputs) ====="
python3 utilities/HBFF/pore_distribution.py --mmm

echo ""
echo "===== FIGURES OF THE CASE AGAINST THE DATA (regression/hbs_nfir/figures/j19damage) ====="
python3 regression/hbs_nfir/plot.py j19damage

echo ""
if [ "$GOLD_STATUS" -ne 0 ] || [ "$CHECK_STATUS" -ne 0 ]; then
    echo "===== DONE, WITH FAILURES ====="
    [ "$GOLD_STATUS" -ne 0 ] && echo "the case disagrees with its gold (or has none): regression/report.html;" \
        "re-bless with --gold only after checking that the change is meant"
    [ "$CHECK_STATUS" -ne 0 ] && echo "pore_distribution.py --check failed: see its table above"
    echo "figures in $ROOT/utilities/HBFF/figures and $ROOT/regression/hbs_nfir/figures/j19damage"
    exit 1
fi
echo "===== DONE ====="
echo "figures in $ROOT/utilities/HBFF/figures and $ROOT/regression/hbs_nfir/figures/j19damage"
