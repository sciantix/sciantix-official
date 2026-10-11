#!/bin/bash
# author: E.Cappellari, POLIMI-CEA 2026
set -e  # stop on error

# =============================================================================
# The whole loop of the HBS fragmentation option 4 (rupture per pore class) in one command.
#
#   1. build SCIANTIX
#   2. run the NFIR case of option 4 (regression/hbs_nfir/test_UO2HBS_frag_j19damage)
#   3. check the C++ against its Python transcription (pore_distribution.py --check)
#   4. draw the figures of option 4: those of README_HBFFmodel.md (classes, rupture, release, pore EoS),
#      the tables for MMM, and those of the case in regression/hbs_nfir/figures/j19damage
#   5. re-execute pore_distribution.ipynb
#
# SCIANTIX is run once, on the case of option 4. Options 1, 2, 3 are never run; with --all-options the
# figures that compare the four options (README.md, regression/hbs_nfir/figures/summary.png) are drawn
# too, from the outputs of options 1, 2, 3 already on disk. The IFA-650 cases are not run and their
# figure is not drawn.
#
# Usage
#   ./utilities/HBFF/runHBFF.sh                # the whole thing
#   ./utilities/HBFF/runHBFF.sh --no-build     # reuse the existing build/sciantix.x
#   ./utilities/HBFF/runHBFF.sh --no-run       # reuse the existing output.txt of the case
#   ./utilities/HBFF/runHBFF.sh --no-notebook  # leave pore_distribution.ipynb as it is
#   ./utilities/HBFF/runHBFF.sh --all-options  # also the figures that compare the four options
# =============================================================================

# Move to the project root, whatever directory this was called from.
cd "$(dirname "$0")/../.."
ROOT="$PWD"
CASE="regression/hbs_nfir/test_UO2HBS_frag_j19damage"

BUILD=1
RUN=1
NOTEBOOK=1
ALL_OPTIONS=0

for arg in "$@"; do
    case "$arg" in
        --no-build)    BUILD=0 ;;
        --no-run)      RUN=0 ;;
        --no-notebook) NOTEBOOK=0 ;;
        --all-options) ALL_OPTIONS=1 ;;
        *)             echo "unknown option: $arg" >&2; exit 1 ;;
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

echo ""
if [ "$RUN" -eq 1 ]; then
    echo "===== RUNNING THE NFIR CASE OF OPTION 4 ====="
    time ./build/sciantix.x "$CASE/"
else
    echo "===== RUN SKIPPED (--no-run) ====="
fi
if [ ! -f "$CASE/output.txt" ]; then
    echo "$CASE/output.txt is not there -- drop --no-run" >&2
    exit 1
fi

# Option 4 is transcribed in pore_distribution.py: the replay of the recursion on the history of the
# case must give back D, G, S, S_2 and the broken fraction of every class of output.txt.
echo ""
echo "===== CHECKING THE C++ OF OPTION 4 AGAINST ITS PYTHON TRANSCRIPTION ====="
python3 utilities/HBFF/pore_distribution.py --check "$CASE"

echo ""
echo "===== FIGURES: CLASSES, RUPTURE AND RELEASE (utilities/HBFF/figures/classes_*.png) ====="
python3 utilities/HBFF/pore_distribution.py

echo ""
echo "===== TABLES FOR MMM (utilities/HBFF/mmm_cases.md, mmm_cases.csv) ====="
python3 utilities/HBFF/pore_distribution.py --mmm

echo ""
echo "===== FIGURES OF THE CASE (regression/hbs_nfir/figures/j19damage) AND PORE EoS ====="
python3 regression/hbs_nfir/plot.py j19damage
python3 utilities/HBFF/plot.py --figure pore_eos
python3 utilities/HBFF/plot.py --figure pore_eos_volume

# The figures that compare the four options read the outputs of options 1, 2, 3: they are not run here.
if [ "$ALL_OPTIONS" -eq 1 ]; then
    MISSING=0
    for option in jernkvist kulacsy lefm; do
        [ -f "regression/hbs_nfir/test_UO2HBS_frag_$option/output.txt" ] || MISSING=1
    done
    echo ""
    if [ "$MISSING" -eq 0 ]; then
        echo "===== FIGURES: THE FOUR OPTIONS ON NFIR (utilities/HBFF/plot.py, regression/hbs_nfir/plot.py) ====="
        python3 utilities/HBFF/plot.py --no-ifa
        python3 regression/hbs_nfir/plot.py
    else
        echo "===== FIGURES OF THE FOUR OPTIONS SKIPPED ====="
        echo "the outputs of options 1, 2, 3 are missing: python3 -m regression.runner --hbs_nfir -j 4 --mode-gold 0"
    fi
fi

echo ""
if [ "$NOTEBOOK" -eq 0 ]; then
    echo "===== NOTEBOOK SKIPPED (--no-notebook) ====="
elif command -v jupyter > /dev/null; then
    echo "===== RE-EXECUTING pore_distribution.ipynb ====="
    jupyter nbconvert --to notebook --execute --inplace --ExecutePreprocessor.timeout=900 \
        utilities/HBFF/pore_distribution.ipynb
else
    echo "===== NOTEBOOK SKIPPED (jupyter not found) ====="
fi

echo ""
echo "===== DONE ====="
echo "figures in $ROOT/utilities/HBFF/figures and $ROOT/regression/hbs_nfir/figures"
