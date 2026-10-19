#!/usr/bin/env python3
"""Verify saved references, independent runs, modal bookkeeping and invariants."""
import argparse
import csv
import hashlib
import json
from pathlib import Path
import sys

import numpy as np

from run_reference import APPROVED_SHA256, CASES, load_notebook, write_json


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--results", type=Path, default=Path(__file__).parent / "results")
    args = parser.parse_args()
    root = args.results.resolve()
    ns, _, _ = load_notebook(None, plain=True)
    checks, summary = {}, []
    for group in ("full", "diagnostic_with_function_trace", "reproducibility_full_plain",
                  "reproducibility_full_observed", "diagnostic", "reproducibility_diagnostic_plain"):
        suite = root / group
        assert hashlib.sha256((suite / "reference_notebook.ipynb").read_bytes()).hexdigest() == APPROVED_SHA256
        for folder in suite.iterdir():
            if not folder.is_dir():
                continue
            manifest = json.loads((folder / "manifest.json").read_text())
            assert manifest["status"] == "ok"
            for name, value in manifest["output_sha256"].items():
                assert hashlib.sha256((folder / name).read_bytes()).hexdigest() == value, (folder, name)

    comparisons = [("diagnostic", "reproducibility_diagnostic_plain", list(CASES)),
                   ("diagnostic_with_function_trace", "reproducibility_diagnostic_plain", list(CASES)),
                   ("full", "reproducibility_full_plain", list(CASES)),
                   ("full", "reproducibility_full_observed", ["AP3p4_1800K"])]
    for left, right, cases in comparisons:
        for key in cases:
            with np.load(root / left / key / "history.npz") as a, np.load(root / right / key / "history.npz") as b:
                assert a.files == b.files
                exact = all(np.array_equal(a[k], b[k], equal_nan=True) for k in a.files)
                assert exact, (left, right, key)
                checks[f"{left}:{right}:{key}"] = {"exact_float64_history": exact, "variables": len(a.files)}
    for name in ("trace.jsonl.gz", "modes.npz", "history.csv", "last_rates.json"):
        first = (root / "full/AP3p4_1800K" / name).read_bytes()
        second = (root / "reproducibility_full_observed/AP3p4_1800K" / name).read_bytes()
        assert first == second, name
        checks["independent_observed:" + name] = {"identical_bytes": True}

    for key in CASES:
        folder = root / "full" / key
        manifest = json.loads((folder / "manifest.json").read_text())
        p = manifest["parameters"]
        with np.load(folder / "history.npz") as h, np.load(folder / "modes.npz") as modes:
            n = manifest["steps"]
            assert h["time"][0] == 0.0 and h["time"][-1] == p["final_time"]
            assert len(h["time"]) == n + 1
            assert np.all(np.isfinite(np.concatenate([h[k] for k in h.files])))
            assert max(np.abs(h["mass_balance_residual"])) / h["generated"][-1] < 1e-12
            for q in ("c", "mb", "md", "Nb", "Nd", "nvb", "nvd", "q_gb", "q_gf", "q_rel", "Rb", "Rd"):
                assert np.min(h[q]) >= 0, (key, q)
            assert np.allclose(h["q_gb"], h["q_gf"] + h["q_rel"], rtol=2e-15, atol=0)
            sat = h["gf_saturated"] > 0
            if sat.any():
                assert np.max(np.abs(h["Fc_gf"][sat] - 0.5)) < 5e-10
            assert manifest["observer_flags"] == {"negative_average_resets": 0, "bulk_seeds": 1, "dislocation_seeds": 1}
            assert modes["step_end"].shape == (n, 3, 160)
            assert np.array_equal(modes["step_end_time_s"], h["time"][1:])
            for j, field in enumerate(("c", "mb", "md")):
                reconstructed = np.asarray([ns["reconstruct_average"](list(row[j])) for row in modes["step_end"]])
                assert np.array_equal(reconstructed, h[field][1:]), (key, field)
            before = modes["after_exchange"][:, 1, :] + modes["after_exchange"][:, 2, :]
            after = modes["after_capture"][:, 1, :] + modes["after_capture"][:, 2, :]
            cap_error = np.max(np.abs(after - before) / np.maximum(np.max(np.abs(before), axis=1)[:, None], 1.0))
            assert cap_error < 1e-14
            assert np.array_equal(modes["after_exchange"][:, 0, :], modes["after_capture"][:, 0, :])
            crossing = int(np.flatnonzero(sat)[0]) if sat.any() else None
            row = {"case": key, "steps": n, "final_time_h": h["time"][-1] / 3600,
                   "final_fima_percent": h["burnup_percent_fima"][-1],
                   "solver_wall_seconds_observed": manifest["solver_wall_seconds"],
                   "last_dt_seconds": manifest["last_dt_s"],
                   "max_balance_residual_over_final_production": manifest["max_mass_balance_residual_over_final_production"],
                   "max_capture_modal_relative_roundoff": float(cap_error),
                   "saturation_first_history_row": crossing,
                   "saturation_first_time_h": float(h["time"][crossing] / 3600) if crossing is not None else None,
                   "final_swelling_bulk_percent": float(h["swelling_b"][-1] * 100),
                   "final_swelling_dislocation_percent": float(h["swelling_d"][-1] * 100),
                   "final_swelling_grainface_percent": float(h["swelling_gf"][-1] * 100),
                   "final_swelling_total_percent": float(h["swelling_total_gas"][-1] * 100),
                   "final_release_percent": float(h["release_gas_percent"][-1]),
                   "final_q_gb": float(h["q_gb"][-1]), "final_q_gf": float(h["q_gf"][-1]),
                   "final_q_rel": float(h["q_rel"][-1]), "final_Fc": float(h["Fc_gf"][-1])}
            summary.append(row)
    write_json(root / "verification.json", {"status": "pass", "independent_process_comparisons": checks,
              "cases": summary, "scope": "Python reproducibility and diagnostics only; no C++ equivalence claim"})
    with open(root / "full_case_summary.csv", "w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(summary[0]))
        writer.writeheader()
        writer.writerows(summary)
    print(f"PASS: {len(checks)} exact independent comparisons; all four full histories and modal invariants checked")


if __name__ == "__main__":
    main()
