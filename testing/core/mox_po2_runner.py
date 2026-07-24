"""
sciantix testing suite
author: Elisa Cappellari

Dispatches verification/test_MOX_po2, which is script-driven (it generates (T,q) case directories via run_temperature_sweep.py, runs them,
and deletes them again) rather than folder-scanned like every other
registered group, so it can't go through generic_runner.run_group().

Evaluates the two acceptance criteria documented in
testing/README_oxygenpotential_VV.md:
  - Kato path (never needs OpenCalphad): max abs log10(pO2/p_ref) error < 1e-3
    over the whole domain.
  - CALPHAD path (needs OpenCalphad + upuo-v21.TDB): mean abs log10(pO2/p_ref)
    error < 0.05 and mean abs oxygen-potential error < 2 kJ/mol, per (q, T)
    group for T >= 1000 K. Skipped (not failed) when OC is unavailable.
"""

import csv
import os
import subprocess
import sys

MOX_PO2_DIR = os.path.abspath(os.path.join(
    os.path.dirname(__file__), "..", "..", "verification", "test_MOX_po2"
))
SWEEP_SCRIPT = os.path.join(MOX_PO2_DIR, "run_temperature_sweep.py")
VERIFICATION_DIR = os.path.join(MOX_PO2_DIR, "sciantix_verification")
KATO_RESIDUALS_TSV = os.path.join(VERIFICATION_DIR, "sciantix_vs_kato_residuals.tsv")
OC_SUMMARY_TSV = os.path.join(VERIFICATION_DIR, "sciantix_vs_oc_csv_summary.tsv")

KATO_MAX_ABS_LOG_ERROR = 1e-3
OC_MEAN_ABS_LOG_ERROR = 0.05
OC_MEAN_ABS_POTENTIAL_ERROR_KJ_MOL = 2.0
OC_MIN_TEMPERATURE_K = 1000.0

REQUIRED_DATABASES = ["upuo-v21.TDB"]

TEST_ID = "mox-po2/test_MOX_po2"

TEMPERATURES_K = [1400, 1800, 2200]
Q_VALUES = [0.10, 0.20, 0.30]


def _read_tsv(path):
    with open(path, newline="") as f:
        return list(csv.DictReader(f, delimiter="\t"))


def _check_kato():
    """Kato-path acceptance: max abs log10(pO2/p_ref) error < 1e-3 over the whole domain."""
    rows = _read_tsv(KATO_RESIDUALS_TSV)
    max_error = max(float(row["Max abs delta log10(p/reference)"]) for row in rows)
    ok = max_error < KATO_MAX_ABS_LOG_ERROR
    return ok, (f"Kato max abs log10(p/reference) error = {max_error:.3e} "
                f"(threshold {KATO_MAX_ABS_LOG_ERROR:.0e})")


def _check_calphad():
    """
    CALPHAD-path acceptance, per (q, T) group for T >= 1000 K: mean abs
    log10(pO2/p_ref) error < 0.05 and mean abs potential error < 2 kJ/mol.
    """
    rows = _read_tsv(OC_SUMMARY_TSV)
    relevant = [row for row in rows if float(row["Temperature key (K)"]) >= OC_MIN_TEMPERATURE_K]
    failures = [
        row for row in relevant
        if float(row["mean_abs_log_error"]) >= OC_MEAN_ABS_LOG_ERROR
        or float(row["mean_abs_potential_error"]) >= OC_MEAN_ABS_POTENTIAL_ERROR_KJ_MOL
    ]
    if not failures:
        return True, (f"CALPHAD mean abs log error < {OC_MEAN_ABS_LOG_ERROR} and mean abs "
                       f"potential error < {OC_MEAN_ABS_POTENTIAL_ERROR_KJ_MOL} kJ/mol for all "
                       f"{len(relevant)} (q,T>=1000K) groups")

    worst = max(failures, key=lambda row: float(row["mean_abs_log_error"]))
    return False, (f"CALPHAD acceptance failed for {len(failures)}/{len(relevant)} (q,T) groups "
                    f"(worst: q={worst['q key (-)']}, T={worst['Temperature key (K)']}K, "
                    f"mean_abs_log_error={float(worst['mean_abs_log_error']):.3e})")


def run(mode_gold: int, oc_status, suite="verification"):
    """
    Run the MOX pO2 verification sweep and evaluate both acceptance criteria.

    Returns a single-element list [(test_id, ok, msg, suite)], ok in
    {True, False, None} (None = skipped entirely, only for unsupported
    mode_gold values -- there is no committed gold file for this group to
    rewrite; the reference is the Kato equation and independent Thermo-Calc
    tables, not output_gold.txt).
    """
    if not os.path.isfile(SWEEP_SCRIPT):
        return [(TEST_ID, None, f"{SWEEP_SCRIPT} not found -- group excluded from this run", suite)]

    if mode_gold != 0:
        return [(TEST_ID, None,
                  "mode-gold only supports 0 (run+compare) for this script-driven group "
                  "-- no output_gold.txt to rewrite/compare against", suite)]

    sweep_args = [
        sys.executable, SWEEP_SCRIPT,
        "--temperatures", ",".join(str(t) for t in TEMPERATURES_K),
        "--q-values", ",".join(f"{q:.2f}" for q in Q_VALUES),
    ]
    try:
        subprocess.run(sweep_args, cwd=MOX_PO2_DIR, check=True)
    except subprocess.CalledProcessError as e:
        return [(TEST_ID, False, f"run_temperature_sweep.py failed: {e}", suite)]

    try:
        kato_ok, kato_msg = _check_kato()

        if oc_status.available_for(REQUIRED_DATABASES):
            calphad_ok, calphad_msg = _check_calphad()
        else:
            calphad_ok = True
            calphad_msg = f"CALPHAD check skipped: {oc_status.reason_for(REQUIRED_DATABASES)}"
    except (FileNotFoundError, KeyError, ValueError) as e:
        return [(TEST_ID, False, f"Could not evaluate verification metrics: {e}", suite)]

    ok = kato_ok and calphad_ok
    if ok:
        msg = f"{kato_msg}; {calphad_msg}"
    else:
        msg = "; ".join(m for m, passed in ((kato_msg, kato_ok), (calphad_msg, calphad_ok)) if not passed)

    return [(TEST_ID, ok, msg, suite)]
