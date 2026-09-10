#!/bin/bash
set -e  # stop on error

# =============================================================================
# The whole HBS-formation loop in one command.
#
#   1. build SCIANTIX
#   2. run ONLY the four regression/hbs cases, one per iHighBurnupStructureFormation
#   3. draw the comparative figures into regression/hbs/figures
#   4. check the C++ of option 4 against its reference Python implementation
#
# Usage
#   ./utilities/runHBS.sh                # the whole thing
#   ./utilities/runHBS.sh --no-build     # reuse the existing build/sciantix.x
#   ./utilities/runHBS.sh --no-figures   # build and run, stop before the figures
#   ./utilities/runHBS.sh --gold         # re-bless the gold files (--mode-gold 1)
#
# Anything else is handed to the regression runner untouched, e.g.
#   ./utilities/runHBS.sh --no-build --hbs.landau
#
# author: E. Cappellari
# =============================================================================

# Move to the project root, whatever directory this was called from.
cd "$(dirname "$0")/.."
ROOT="$PWD"

BUILD=1
FIGURES=1
MODE_GOLD=0
RUNNER_ARGS=()

for arg in "$@"; do
    case "$arg" in
        --no-build)   BUILD=0 ;;
        --no-figures) FIGURES=0 ;;
        --gold)       MODE_GOLD=1 ;;
        *)            RUNNER_ARGS+=("$arg") ;;
    esac
done

JOBS=$(nproc 2>/dev/null || echo 4)

if [ "$BUILD" -eq 1 ]; then
    echo "===== COMPILING SCIANTIX ====="
    ./Allmake.sh
else
    echo "===== BUILD SKIPPED (--no-build) ====="
    if [ ! -x "$ROOT/build/sciantix.x" ]; then
        echo "but $ROOT/build/sciantix.x is not there -- drop --no-build" >&2
        exit 1
    fi
fi

echo ""
echo "===== RUNNING THE HBS CASES (using $JOBS threads) ====="
# A case that disagrees with its gold must not stop the figures: seeing the curves
# is usually how you find out whether the disagreement is a regression or a gold
# that predates a deliberate change. The status is carried to the end instead.
REGRESSION_STATUS=0
python3 -m regression.runner --hbs -j "$JOBS" --mode-gold "$MODE_GOLD" "${RUNNER_ARGS[@]}" \
    || REGRESSION_STATUS=$?

if [ "$FIGURES" -eq 0 ]; then
    echo ""
    echo "===== FIGURES SKIPPED (--no-figures) ====="
    exit "$REGRESSION_STATUS"
fi

# The four cases as they stand: each formation option with the porosity model it
# was built around (1, 2, 3, 3). This is what a user of each case actually gets.
echo ""
echo "===== FIGURES: THE FOUR CASES OVERLAID ====="
python3 regression/hbs/plot.py --landau

# The controlled comparison: same history, same time stepping, porosity held at 3,
# only iHighBurnupStructureFormation varying. Built in a temporary directory, so
# nothing under regression/ is touched.
echo ""
echo "===== FIGURES: THE FOUR FORMATION OPTIONS, ALL ELSE EQUAL ====="
python3 utilities/HBSformation/compare_formation_options.py \
    --figure regression/hbs/figures/formation_options.png
# The README of utilities/HBSformation embeds its own copy; keep the two in step so
# the document and the figure never disagree.
cp regression/hbs/figures/formation_options.png \
   utilities/HBSformation/figures/formation_options.png

# Option 4 is a statement-by-statement transcription of the reference Python.
# This is what keeps it one; it exits non-zero when the two drift apart.
echo ""
echo "===== CHECKING THE C++ OF OPTION 4 AGAINST ITS REFERENCE ====="
python3 utilities/HBSformation/compare_with_sciantix.py \
    regression/hbs/test_UO2HBS_landau/output.txt

echo ""
if [ "$REGRESSION_STATUS" -ne 0 ]; then
    echo "===== DONE, WITH FAILING CASES ====="
    echo "figures in $ROOT/regression/hbs/figures"
    echo "the regression runner exited $REGRESSION_STATUS -- see the table above and"
    echo "regression/report.html. Re-bless with ./utilities/runHBS.sh --gold only after"
    echo "checking the disagreement is a change you meant to make."
    exit "$REGRESSION_STATUS"
fi

echo "===== DONE ====="
echo "figures in $ROOT/regression/hbs/figures"
