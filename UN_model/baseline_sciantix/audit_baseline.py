#!/usr/bin/env python3
"""Supplement historical discovery and independently check named output columns."""
import argparse
import collections
import csv
import hashlib
import importlib.metadata
import json
from pathlib import Path
import shutil
import subprocess
import sys
import time

import numpy as np


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("baseline", type=Path)
    parser.add_argument("--reuse-executions", action="store_true", help="Recheck saved outputs without rerunning cases")
    args = parser.parse_args()
    out = args.baseline.resolve()
    snap = out / "snapshot"
    sys.path.insert(0, str(snap))
    from regression.core.generic_runner import run_single_case
    from regression.core.parser import SciantixOutput

    results = json.loads((out / "case_results.json").read_text())
    seen = {r[0] for r in results}
    all_cases = {str(p.relative_to(snap / "regression")): p
                 for p in (snap / "regression").glob("*/test_*") if p.is_dir()}
    gold_before = {str(p.relative_to(snap)): sha(p) for p in (snap / "regression").rglob("output_gold.txt")}
    previous = json.loads((out / "audit.json").read_text()) if args.reuse_executions else None
    if previous:
        extra = json.loads((out / "supplemental_case_results.json").read_text())
        extra_seconds = previous["supplemental_wall_seconds"]
    else:
        extra = []
        start = time.perf_counter()
        for key in sorted(all_cases.keys() - seen):
            group, name = key.split("/")
            extra.append(run_single_case((group, name, str(all_cases[key]), 0)))
        extra_seconds = time.perf_counter() - start
        (out / "supplemental_case_results.json").write_text(json.dumps(extra, indent=2) + "\n")
    summaries = []
    for name, passed, message in results + extra:
        case = all_cases[name]
        row = {"case": name, "historical_pass": passed, "message": message,
               "selected_by_all": name in seen}
        try:
            actual, expected = SciantixOutput(case / "output.txt"), SciantixOutput(case / "output_gold.txt")
            columns = [i for i, label in enumerate(actual.header) if label.strip()]
            a, b = actual.data[:, columns], expected.data[:, columns]
            row["matching_headers"] = bool(np.array_equal(actual.header, expected.header))
            row["shape"] = list(a.shape)
            row["nonfinite_named_output_values"] = int(np.count_nonzero(~np.isfinite(a)))
            row["nonfinite_named_gold_values"] = int(np.count_nonzero(~np.isfinite(b)))
            diff = np.abs(a - b)
            row["max_absolute_difference"] = float(np.nanmax(diff))
            row["all_named_values_bitwise_equal"] = bool(np.array_equal(a, b, equal_nan=True))
            row["strict_named_check_pass"] = bool(row["matching_headers"] and np.isfinite(a).all()
                and np.isfinite(b).all() and not np.any((diff > 1e-8) & (diff / np.maximum(1e-8, np.abs(b)) > 1e-6)))
        except Exception as exc:
            row["audit_error"] = repr(exc)
            row["strict_named_check_pass"] = False
        summaries.append(row)
    # Preserve the legacy UN regression as a separate result, never use it as L3 evidence.
    command = [sys.executable, str(snap / "regression/un_notebook8/run_un_notebook8.py"),
               "--exe", str(snap / "build/sciantix.x"), "--strict", "--no-plot"]
    if previous:
        legacy_code = previous["legacy_un"]["returncode"]
        legacy_seconds = previous["legacy_un"]["wall_seconds"]
    else:
        start = time.perf_counter()
        with open(out / "legacy_un_notebook8.log", "x") as log:
            legacy = subprocess.run(command, cwd=snap, stdout=log, stderr=subprocess.STDOUT)
        legacy_code = legacy.returncode
        legacy_seconds = time.perf_counter() - start
    legacy_rows = []
    legacy_csv = snap / "regression/un_notebook8/results/un_notebook8_summary.csv"
    if legacy_csv.exists():
        shutil.copy2(legacy_csv, out / "legacy_un_notebook8_summary.csv")
        with open(legacy_csv) as handle:
            legacy_rows = list(csv.DictReader(handle))
    gold_after = {str(p.relative_to(snap)): sha(p) for p in (snap / "regression").rglob("output_gold.txt")}
    assert gold_before == gold_after
    audit = {"standard_all_counts": {"total": len(results), "passed": sum(r[1] for r in results),
                                      "failed": sum(not r[1] for r in results)},
             "supplemental_counts": {"total": len(extra), "passed": sum(r[1] for r in extra),
                                      "failed": sum(not r[1] for r in extra)},
             "supplemental_wall_seconds": extra_seconds,
             "named_column_audit": summaries,
             "strict_named_checks_passed": sum(r["strict_named_check_pass"] for r in summaries),
             "legacy_un": {"argv": command, "returncode": legacy_code, "wall_seconds": legacy_seconds,
                           "counts": dict(collections.Counter(r["status"] for r in legacy_rows)),
                           "scope": "notebook-8 legacy; 35% tolerance; not L3 regression",
                           "reference_generator_path_exists": (snap / "UN_model/notebooks/8test_UN_intergranular.ipynb").exists()},
             "gold_unchanged": gold_before == gold_after,
             "numpy_version": importlib.metadata.version("numpy"),
             "input_and_regression_code_sha256": {str(p.relative_to(snap)): sha(p)
                 for p in sorted((snap / "regression").rglob("*"))
                 if p.is_file() and (p.name.startswith("input_") or p.suffix == ".py")}}
    (out / "audit.json").write_text(json.dumps(audit, indent=2) + "\n")
    print(json.dumps({k: v for k, v in audit.items() if k not in ("named_column_audit", "input_and_regression_code_sha256")}, indent=2))
    return int(any(not r[1] for r in extra) or legacy_code != 0
               or any(not r["strict_named_check_pass"] for r in summaries))


if __name__ == "__main__":
    raise SystemExit(main())
