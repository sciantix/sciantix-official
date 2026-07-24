"""
sciantix testing suite
author: Elisa Cappellari

Evaluates the MOX pO2 accuracy check documented in
testing/README_oxygenpotential_VV.md, on top of the ordinary gold-diff that
generic_runner.run_group() already performs for the 9 persistent
verification/test_MOX_po2/T_<T>K_q_<Pu>/ cases (registered like any other
verification group -- see runner.py's REGISTRY):
  - Kato path (never needs OpenCalphad): max abs log10(pO2/p_ref) error < 1e-3
    over the whole domain.
  - CALPHAD path (needs OpenCalphad + upuo-v21.TDB): mean abs log10(pO2/p_ref)
    error < 0.05 and mean abs oxygen-potential error < 2 kJ/mol, per (q, T)
    group for T >= 1000 K. Skipped (not failed) when OC is unavailable.

This module does not run sciantix.x itself -- it reads the output.txt that
run_group() already produced for each case, concatenates them into the
temperature_sweep_summary.tsv the comparison scripts expect, and invokes
those scripts. If the group wasn't actually run (e.g. cases not selected, or
mode-gold 3), it skips gracefully.
"""

import csv
import glob
import os
import subprocess
import sys

import pandas as pd

MOX_PO2_DIR = os.path.abspath(os.path.join(
    os.path.dirname(__file__), "..", "..", "verification", "test_MOX_po2"
))
VERIFICATION_DIR = os.path.join(MOX_PO2_DIR, "sciantix_verification")
SUMMARY_TSV = os.path.join(MOX_PO2_DIR, "temperature_sweep_summary.tsv")
COMPARE_KATO_SCRIPT = os.path.join(VERIFICATION_DIR, "compare_sciantix_with_kato.py")
COMPARE_OC_SCRIPT = os.path.join(VERIFICATION_DIR, "compare_sciantix_with_oc_csv.py")
KATO_RESIDUALS_TSV = os.path.join(VERIFICATION_DIR, "sciantix_vs_kato_residuals.tsv")
OC_SUMMARY_TSV = os.path.join(VERIFICATION_DIR, "sciantix_vs_oc_csv_summary.tsv")

KATO_MAX_ABS_LOG_ERROR = 1e-3
OC_MEAN_ABS_LOG_ERROR = 0.05
OC_MEAN_ABS_POTENTIAL_ERROR_KJ_MOL = 2.0
OC_MIN_TEMPERATURE_K = 1000.0

REQUIRED_DATABASES = ["upuo-v21.TDB"]

TEST_ID = "mox-po2/accuracy-check"


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


def _build_summary():
    """Concatenate every persistent case's output.txt into temperature_sweep_summary.tsv."""
    case_outputs = sorted(glob.glob(os.path.join(MOX_PO2_DIR, "T_*_q_*", "output.txt")))
    if not case_outputs:
        return None

    frames = [pd.read_csv(path, sep="\t") for path in case_outputs]
    summary = pd.concat(frames, ignore_index=True)
    summary.to_csv(SUMMARY_TSV, sep="\t", index=False)
    return len(case_outputs)


def check_accuracy(oc_status, suite="verification"):
    """
    Evaluate the Kato/CALPHAD accuracy thresholds against the persistent
    T_<T>K_q_<Pu>/ cases' output.txt (already produced by run_group()'s
    gold-diff dispatch -- this function never runs sciantix.x itself).

    Returns a single-element list [(test_id, ok, msg, suite)], ok in
    {True, False, None} (None = skipped, e.g. cases weren't actually run).
    """
    try:
        n_cases = _build_summary()
    except Exception as e:
        return [(TEST_ID, False, f"Could not build temperature_sweep_summary.tsv: {e}", suite)]

    if not n_cases:
        return [(TEST_ID, None, "no case output.txt found -- group excluded from this run", suite)]

    try:
        subprocess.run([sys.executable, COMPARE_KATO_SCRIPT], cwd=MOX_PO2_DIR, check=True)
    except subprocess.CalledProcessError as e:
        return [(TEST_ID, False, f"compare_sciantix_with_kato.py failed: {e}", suite)]

    try:
        kato_ok, kato_msg = _check_kato()

        if oc_status.available_for(REQUIRED_DATABASES):
            subprocess.run([sys.executable, COMPARE_OC_SCRIPT], cwd=MOX_PO2_DIR, check=True)
            calphad_ok, calphad_msg = _check_calphad()
        else:
            calphad_ok = True
            calphad_msg = f"CALPHAD check skipped: {oc_status.reason_for(REQUIRED_DATABASES)}"
    except (FileNotFoundError, KeyError, ValueError, subprocess.CalledProcessError) as e:
        return [(TEST_ID, False, f"Could not evaluate accuracy metrics: {e}", suite)]

    ok = kato_ok and calphad_ok
    if ok:
        msg = f"{kato_msg}; {calphad_msg}"
    else:
        msg = "; ".join(m for m, passed in ((kato_msg, kato_ok), (calphad_msg, calphad_ok)) if not passed)

    return [(TEST_ID, ok, msg, suite)]
