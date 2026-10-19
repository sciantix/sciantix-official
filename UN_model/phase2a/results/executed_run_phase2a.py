#!/usr/bin/env python3
"""Fixture-only validation of the production SCIANTIX 3x3 exchange solver.

Reads Phase 1 artifacts; never imports/executes Python model equations and never
calculates physical coefficients. All work/build/output goes to a new directory.
"""
import argparse
import csv
import gzip
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time

import numpy as np

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
REFERENCE = ROOT / "UN_model/regression_reference/results/full"
CASES = ["AP3p2_900K", "AP3p4_1600K", "AP3p4_1800K", "ANP6_1600K"]
COEFFICIENT_FIELDS = ["dt", "Dg", None, "source_c", "g_b", "g_d", "b_b_gas", "b_d_gas", "source_mb", "source_md"]
RTOL = 1e-11
ABS_EPS_FACTOR = 64.0


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def save(path, value):
    Path(path).write_text(json.dumps(value, indent=2, allow_nan=False) + "\n")


def extract_case(case, destination):
    source = REFERENCE / case
    manifest = json.loads((source / "manifest.json").read_text())
    assert manifest["status"] == "ok" and manifest["parameters"]["n_modes"] == 160
    assert manifest["notebook_sha256"] == "e29d798d3685edb827a43933b3dc4400087f1aff488ee4080650c1430e1c59ff"
    for name, expected in manifest["output_sha256"].items():
        assert sha(source / name) == expected, (case, name)
    exchange = []
    with gzip.open(source / "trace.jsonl.gz", "rt") as handle:
        for line in handle:
            event = json.loads(line)
            if event["stage"] == "after_exchange":
                assert event["step"] == len(exchange)
                exchange.append(event["scalars"])
    with np.load(source / "modes.npz") as data:
        expected_modes = data["after_exchange"].copy()
        inputs = np.concatenate((data["initial"], data["step_end"][:-1]), axis=0)
        times = data["after_exchange_time_s"].copy()
        # Only recorded changes are replayed. This is not a C++ capture model.
        delta = np.zeros_like(inputs)
        delta[1:] = inputs[1:] - expected_modes[:-1]
    n_steps, _, n_modes = expected_modes.shape
    assert n_steps == len(exchange) == manifest["steps"]
    coefficients = np.asarray([[manifest["parameters"]["grain_radius"] if name is None else e[name]
                                for name in COEFFICIENT_FIELDS] for e in exchange], dtype="<f8")
    means = np.asarray([[e[k] for k in ("c_new", "mb_new", "md_new")] for e in exchange])
    assert np.array_equal(times, np.asarray([e["t"] + e["dt"] for e in exchange]))
    changed = np.any(delta[1:] != 0.0, axis=(1, 2))
    # Last prefix step may be followed by capture, but no intervening correction
    # is needed before that step's exchange. At least two consecutive calls.
    prefix = int(np.flatnonzero(changed)[0] + 1) if changed.any() else n_steps
    assert prefix >= 2 and not np.any(delta[:prefix])
    destination.mkdir(parents=True)
    (destination / "config.txt").write_text(f"{n_modes} {n_steps} {prefix}\n")
    for name, value in [("coefficients", coefficients), ("input_modes", inputs), ("interventions", delta)]:
        value.astype("<f8").tofile(destination / (name + ".bin"))
    np.savez_compressed(destination / "expected.npz", modes=expected_modes, averages=means, time_s=times)
    metadata = {"case": case, "steps": n_steps, "n_modes": n_modes, "uninterrupted_prefix_steps": prefix,
                "reference_files_sha256": {str(p.relative_to(ROOT)): sha(p) for p in
                    [source / "manifest.json", source / "trace.jsonl.gz", source / "modes.npz"]},
                "coefficient_fields": [name or "grain_radius_from_case_manifest" for name in COEFFICIENT_FIELDS],
                "input_convention": "initial modes at step 0, preceding saved step_end otherwise",
                "intervention_convention": "before step i>0: saved_input[i] - saved_after_exchange[i-1]; add, never rebase",
                "binary_format": "little-endian float64, C order, config dimensions, no binary header",
                "derived_files_sha256": {p.name: sha(p) for p in destination.iterdir() if p.is_file()}}
    save(destination / "manifest.json", metadata)
    return expected_modes, means, times, prefix


def compare(actual, expected, scales, case, scenario, engine, kind, times):
    diff = np.abs(actual - expected)
    atol = ABS_EPS_FACTOR * np.finfo(np.float64).eps * scales
    allowed = atol + RTOL * np.abs(expected)
    bad = ~np.isfinite(actual) | (diff > allowed)
    normalized = diff / scales
    metrics = []
    for pop, label in enumerate(("c", "mb", "md")):
        d, e, a, z, b = (v[:, pop] for v in (diff, expected, actual, normalized, bad))
        index = np.unravel_index(np.argmax(z), z.shape)
        nonzero = e != 0
        row = {"case": case, "scenario": scenario, "engine": engine, "quantity": kind + ":" + label,
               "values": int(d.size), "failures": int(b.sum()), "max_absolute_error": float(d.max()),
               "max_relative_error_nonzero_reference": float(np.max(d[nonzero] / np.abs(e[nonzero]))) if nonzero.any() else 0.0,
               "max_normalized_error": float(z[index]), "max_error_step_zero_based": int(index[0]),
               "time_s": float(times[index[0]]), "max_error_mode_zero_based": int(index[1]) if len(index) == 2 else None,
               "cpp_at_max_normalized_error": float(a[index]), "python_at_max_normalized_error": float(e[index])}
        if b.any():
            first = tuple(np.argwhere(b)[0])
            row["first_failure_step_zero_based"] = int(first[0])
            row["first_failure_mode_zero_based"] = int(first[1]) if len(first) == 2 else None
        else:
            row["first_failure_step_zero_based"] = None
            row["first_failure_mode_zero_based"] = None
        metrics.append(row)
    return metrics


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--cmake", default="/tmp/sciantix-un-l3-phase1-tools/cmake/data/bin/cmake")
    args = parser.parse_args()
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    snapshot = out / "snapshot"
    snapshot.mkdir()
    for name in ("src", "include"):
        shutil.copytree(ROOT / name, snapshot / name)
    shutil.copy2(ROOT / "CMakeLists.txt", snapshot)
    tests = snapshot / "UN_model/phase2a"
    tests.mkdir(parents=True)
    for name in ("solver_fixture.cpp", "state_test.cpp"):
        shutil.copy2(HERE / name, tests / name)
    commands = []

    def run(name, argv):
        start = time.perf_counter()
        with open(out / (name + ".log"), "w") as log:
            result = subprocess.run(list(map(str, argv)), cwd=snapshot, stdout=log, stderr=subprocess.STDOUT)
        commands.append({"name": name, "argv": list(map(str, argv)), "returncode": result.returncode,
                         "wall_seconds": time.perf_counter() - start})
        save(out / "commands.json", commands)
        if result.returncode:
            raise RuntimeError(f"{name} failed; see {out / (name + '.log')}")

    start = time.perf_counter()
    run("configure", [args.cmake, "-S", snapshot, "-B", snapshot / "build", "-DCMAKE_BUILD_TYPE=Release",
                      "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON", "-DSCIANTIX_UN_L3_TESTS=ON"])
    run("build", [args.cmake, "--build", snapshot / "build", "-j", "2"])
    run("state_ctest", [str(Path(args.cmake).with_name("ctest")), "--test-dir", snapshot / "build", "--output-on-failure"])
    shutil.copy2(snapshot / "build/compile_commands.json", out / "compile_commands.json")
    shutil.copy2(snapshot / "build/CMakeCache.txt", out / "CMakeCache.txt")
    driver = snapshot / "build/un_l3_solver_fixture.x"
    if not driver.exists():
        driver = snapshot / "build/un_l3_solver_fixture"
    rows, equality = [], []
    for case in CASES:
        fixture = out / "fixtures" / case
        expected_modes, expected_means, times, prefix = extract_case(case, fixture)
        outputs = out / "outputs" / case
        outputs.mkdir(parents=True)
        for scenario in ("single", "prefix", "replay"):
            count = prefix if scenario == "prefix" else len(times)
            modes, means = expected_modes[:count], expected_means[:count]
            # Per-timestep/population scale, so late large values cannot conceal
            # early small-population errors. The floor is one atom/m^3.
            scale = np.maximum(np.max(np.abs(modes), axis=2), 1.0)
            for engine in ("direct", "state"):
                path = outputs / (scenario + "_" + engine + ".bin")
                run(case + "_" + scenario + "_" + engine, [driver, fixture, scenario, engine, path])
                data = np.fromfile(path, dtype="<f8").reshape(count, 3 + 3 * modes.shape[2])
                actual_means, actual_modes = data[:, :3], data[:, 3:].reshape(modes.shape)
                rows += compare(actual_modes, modes, scale[:, :, None], case, scenario, engine, "modes", times[:count])
                rows += compare(actual_means, means, scale, case, scenario, engine, "averages", times[:count])
            identical = (outputs / (scenario + "_direct.bin")).read_bytes() == (outputs / (scenario + "_state.bin")).read_bytes()
            equality.append({"case": case, "scenario": scenario, "direct_and_state_identical_bytes": identical})
            assert identical, (case, scenario, "adapter mapping")
        print(f"{case}: {len(times)} isolated steps, {prefix} uninterrupted prefix, {len(times)} frozen-driven replay", flush=True)
    with open(out / "errors.csv", "w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)
    result = {"phase": "2A: numerical exchange and separate state contract; no normal SCIANTIX integration",
              "status": "pass" if not sum(r["failures"] for r in rows) else "fail",
              "rtol": RTOL, "atol": "64*float64_epsilon*max(per-step/per-population max(abs(expected_mode)),1 atom/m^3)",
              "independent_categories": {"solver": "single, uninterrupted prefix, externally driven replay",
                                         "state": "CTest ownership/flags/interleaving; adapter binary equality",
                                         "normal_Simulation_cycle": "deferred; not tested or implemented"},
              "max_normalized_error": max(r["max_normalized_error"] for r in rows),
              "failures": sum(r["failures"] for r in rows), "values_compared": sum(r["values"] for r in rows),
              "direct_state_equality": equality, "metrics": rows, "commands": commands,
              "wall_seconds": time.perf_counter() - start, "python": sys.version,
              "numpy": np.__version__, "runner_sha256": sha(__file__),
              "solver_source_sha256": sha(ROOT / "src/classes/Solver.C"),
              "binary_sha256": sha(driver),
              "git_head": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip()}
    save(out / "results.json", result)
    print(f"{result['status'].upper()}: {result['values_compared']} values, max normalized error {result['max_normalized_error']:.6g}")
    return int(result["status"] != "pass")


if __name__ == "__main__":
    raise SystemExit(main())
