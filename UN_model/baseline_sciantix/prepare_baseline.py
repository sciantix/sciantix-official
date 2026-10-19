#!/usr/bin/env python3
"""Build the unmodified current sources and run the historical suite in a copy."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def hashes(root, paths):
    return {str(p.relative_to(root)): sha(p) for p in sorted(paths) if p.is_file()}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--cmake", default="cmake")
    parser.add_argument("--jobs", type=int, default=2)
    args = parser.parse_args()
    out = args.output.resolve()
    out.mkdir(parents=True, exist_ok=False)
    snap = out / "snapshot"
    snap.mkdir()
    for name in ("src", "include", "regression"):
        shutil.copytree(ROOT / name, snap / name, ignore=shutil.ignore_patterns("__pycache__", "*.pyc"))
    shutil.copy2(ROOT / "CMakeLists.txt", snap)
    source_files = [snap / "CMakeLists.txt"] + list((snap / "src").rglob("*")) + list((snap / "include").rglob("*"))
    source_hashes = hashes(snap, source_files)
    gold_before = hashes(snap, (snap / "regression").rglob("output_gold.txt"))
    manifest = {"phase": "pre-port baseline; unmodified physical implementation",
                "git_head": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip(),
                "git_branch": subprocess.check_output(["git", "branch", "--show-current"], cwd=ROOT, text=True).strip(),
                "git_status": subprocess.check_output(["git", "status", "--short"], cwd=ROOT, text=True),
                "source_sha256": source_hashes, "gold_before_sha256": gold_before,
                "python": sys.version, "python_executable": sys.executable,
                "compiler": subprocess.check_output(["g++", "--version"], text=True),
                "cmake_version": subprocess.check_output([args.cmake, "--version"], text=True),
                "commands": [], "mode_gold": 0}

    def save():
        (out / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")

    def run(name, command, cwd=snap):
        start = time.perf_counter()
        with open(out / (name + ".log"), "x") as log:
            result = subprocess.run(command, cwd=cwd, stdout=log, stderr=subprocess.STDOUT,
                                    env={**os.environ, "MPLBACKEND": "Agg", "MPLCONFIGDIR": str(out / "mplconfig")})
        record = {"name": name, "argv": list(map(str, command)), "cwd": str(cwd),
                  "returncode": result.returncode, "wall_seconds": time.perf_counter() - start}
        manifest["commands"].append(record)
        save()
        print(f"{name}: exit {result.returncode}, {record['wall_seconds']:.3f} s; {out / (name + '.log')}", flush=True)
        return result.returncode

    save()
    if run("configure", [args.cmake, "-S", str(snap), "-B", str(snap / "build"),
                          "-DCMAKE_BUILD_TYPE=Release", "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON", "-DCOUPLING_TU=OFF"]):
        return 1
    if run("build", [args.cmake, "--build", str(snap / "build"), "-j", str(args.jobs)]):
        return 1
    manifest["binary_sha256"] = sha(snap / "build/sciantix.x")
    shutil.copy2(snap / "build/CMakeCache.txt", out / "CMakeCache.txt")
    shutil.copy2(snap / "build/compile_commands.json", out / "compile_commands.json")
    run("ctest_inventory", [str(Path(args.cmake).with_name("ctest")) if "/" in args.cmake else "ctest",
                            "--test-dir", str(snap / "build"), "--show-only=json-v1"])
    # Capture structured results at the reporting boundary; comparison and case execution untouched.
    capture = out / "capture_regression.py"
    capture.write_text('''import json, pathlib, sys
sys.path.insert(0, sys.argv.pop(1))
import regression.runner as runner
original_report = runner.generate_html_report
def capture(results, output_dir):
    pathlib.Path(output_dir).parent.joinpath("baseline_case_results.json").write_text(json.dumps(results, indent=2)+"\\n")
    return original_report(results, output_dir)
runner.generate_html_report = capture
runner.main()
''')
    run("regression", [sys.executable, str(capture), str(snap), "--all", "--mode-gold", "0", "-j", str(args.jobs)])
    result_file = snap / "baseline_case_results.json"
    if result_file.exists():
        shutil.copy2(result_file, out / "case_results.json")
        results = json.loads(result_file.read_text())
        manifest["regression_counts"] = {"total": len(results), "passed": sum(bool(r[1]) for r in results),
                                          "failed": sum(not r[1] for r in results)}
        shutil.copy2(snap / "regression/report.html", out / "report.html")
    manifest["gold_after_sha256"] = hashes(snap, (snap / "regression").rglob("output_gold.txt"))
    manifest["gold_unchanged"] = manifest["gold_after_sha256"] == gold_before
    manifest["source_unchanged"] = hashes(snap, source_files) == source_hashes
    save()
    assert manifest["gold_unchanged"] and manifest["source_unchanged"]
    return 0 if manifest.get("regression_counts", {}).get("failed") == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())
