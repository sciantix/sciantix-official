#!/usr/bin/env python3
"""Detection controls and memory checks on the same frozen fixture data."""
import argparse
import json
import os
from pathlib import Path
import shutil
import subprocess
import time

import numpy as np

from run_phase2a import compare, save, sha


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--release", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--cmake", default="/tmp/sciantix-un-l3-phase1-tools/cmake/data/bin/cmake")
    parser.add_argument("--disable-leak-detection", action="store_true",
                        help="Only for ptrace environments where LeakSanitizer cannot run; ASan/UBSan stay enabled")
    args = parser.parse_args()
    release, out = args.release.resolve(), args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    commands, controls = [], []
    environment = dict(os.environ)
    if args.disable_leak_detection:
        environment["ASAN_OPTIONS"] = "detect_leaks=0"

    def run(name, command):
        start = time.perf_counter()
        with open(out / (name + ".log"), "w") as handle:
            result = subprocess.run(list(map(str, command)), stdout=handle, stderr=subprocess.STDOUT, env=environment)
        commands.append({"name": name, "argv": list(map(str, command)), "returncode": result.returncode,
                         "wall_seconds": time.perf_counter() - start})
        save(out / "commands.json", commands)
        if result.returncode:
            raise RuntimeError(f"{name} failed: {out / (name + '.log')}")

    driver = release / "snapshot/build/un_l3_solver_fixture.x"
    if not driver.exists():
        driver = driver.with_suffix("")
    source = release / "fixtures/AP3p4_1800K"
    n, steps, _ = map(int, (source / "config.txt").read_text().split())
    index = steps // 2
    coefficients = np.fromfile(source / "coefficients.bin", dtype="<f8").reshape(steps, 10)[index:index+1]
    modes_in = np.fromfile(source / "input_modes.bin", dtype="<f8").reshape(steps, 3, n)[index:index+1]
    with np.load(source / "expected.npz") as reference:
        expected = reference["modes"][index:index+1]
        means = reference["averages"][index:index+1]
        times = reference["time_s"][index:index+1]
    for control in ("swapped_trapping_bindings", "swapped_source_bindings", "lost_dislocation_modes"):
        folder = out / control
        folder.mkdir()
        c, initial = coefficients.copy(), modes_in.copy()
        if control == "swapped_trapping_bindings":
            c[:, [4, 5]] = c[:, [5, 4]]
        elif control == "swapped_source_bindings":
            c[:, [3, 8]] = c[:, [8, 3]]
        else:
            # Reproduce an accidental reset to the saved initial zero md.
            first = np.fromfile(source / "input_modes.bin", dtype="<f8", count=3*n).reshape(3, n)
            initial[:, 2] = first[2]
        (folder / "config.txt").write_text(f"{n} 1 1\n")
        c.tofile(folder / "coefficients.bin")
        initial.tofile(folder / "input_modes.bin")
        np.zeros_like(initial).tofile(folder / "interventions.bin")
        path = folder / "output.bin"
        run(control, [driver, folder, "single", "direct", path])
        actual = np.fromfile(path, dtype="<f8").reshape(1, 3+3*n)
        scale = np.maximum(np.max(np.abs(expected), axis=2), 1.0)
        metrics = compare(actual[:, 3:].reshape(expected.shape), expected, scale[:, :, None],
                          "AP3p4_1800K", control, "direct", "modes", times)
        metrics += compare(actual[:, :3], means, scale, "AP3p4_1800K", control, "direct", "averages", times)
        failures = sum(r["failures"] for r in metrics)
        assert failures > 0, "Comparator failed to detect deliberate fixture corruption"
        controls.append({"control": control, "expected_mismatch_detected": True,
                         "failed_values": failures, "saved_python_step_zero_based": index, "metrics": metrics})

    source_tree = release / "snapshot"
    build = out / "sanitized_build"
    run("sanitize_configure", [args.cmake, "-S", source_tree, "-B", build, "-DSCIANTIX_UN_L3_TESTS=ON",
                               "-DCMAKE_BUILD_TYPE=Debug", "-DCMAKE_CXX_FLAGS=-fsanitize=address,undefined -fno-omit-frame-pointer"])
    run("sanitize_build", [args.cmake, "--build", build, "--target", "un_l3_state_test", "un_l3_solver_fixture", "-j", "2"])
    run("sanitize_state", [str(Path(args.cmake).with_name("ctest")), "--test-dir", build, "--output-on-failure"])
    sanitized_driver = build / "un_l3_solver_fixture.x"
    if not sanitized_driver.exists():
        sanitized_driver = sanitized_driver.with_suffix("")
    sanitizer_results = []
    for case, scenario in [("AP3p2_900K", "prefix"), ("AP3p4_1600K", "prefix"),
                           ("AP3p4_1800K", "prefix"), ("ANP6_1600K", "replay")]:
        fixture = release / "fixtures" / case
        output = out / (case + "_sanitized.bin")
        run(case + "_sanitized", [sanitized_driver, fixture, scenario, "state", output])
        release_output = release / "outputs" / case / (scenario + "_state.bin")
        # Debug/sanitized arithmetic can differ by rounding, use the same
        # quantitative comparison rather than require identical build bytes.
        n, steps, prefix = map(int, (fixture / "config.txt").read_text().split())
        count = prefix if scenario == "prefix" else steps
        actual = np.fromfile(output, dtype="<f8").reshape(count, 3+3*n)
        with np.load(fixture / "expected.npz") as reference:
            expected, means, times = (reference[k][:count] for k in ("modes", "averages", "time_s"))
        scale = np.maximum(np.max(np.abs(expected), axis=2), 1.0)
        metrics = compare(actual[:, 3:].reshape(expected.shape), expected, scale[:, :, None],
                          case, scenario, "sanitized_state", "modes", times)
        metrics += compare(actual[:, :3], means, scale, case, scenario, "sanitized_state", "averages", times)
        assert not sum(r["failures"] for r in metrics)
        sanitizer_results.append({"case": case, "scenario": scenario, "metrics": metrics,
                                   "identical_to_release": sha(output) == sha(release_output)})
    save(out / "results.json", {"status": "pass", "expected_failure_controls": controls,
                                "sanitizer_results": sanitizer_results, "commands": commands,
                                "ASAN_OPTIONS_override": environment.get("ASAN_OPTIONS"),
                                "leak_detection_disabled": args.disable_leak_detection})
    print("PASS: three detection controls; ASan/UBSan state test and saved-fixture replays")


if __name__ == "__main__":
    main()
