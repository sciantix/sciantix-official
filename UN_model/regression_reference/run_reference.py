#!/usr/bin/env python3
"""Execute approved notebook cells in a fresh module; no rewritten physics.

Observation inserts only calls receiving locals() into solve_UN. Removing those
calls must recover the original AST exactly. --plain executes the untouched AST.
"""
from __future__ import annotations

import argparse
import ast
import contextlib
import copy
import csv
import dataclasses
import gzip
import hashlib
import importlib.metadata
import io
import json
import math
from pathlib import Path
import platform
import subprocess
import sys
import time
import traceback
import types
import warnings

import numpy as np

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
NOTEBOOK = ROOT / "UN_model/notebooks/finalRizkUN_L3_solver_unificato.ipynb"
APPROVED_SHA256 = "e29d798d3685edb827a43933b3dc4400087f1aff488ee4080650c1430e1c59ff"
CELLS = (1, 2, 6, 8, 10)
CASES = {"AP3p2_900K": ("AP3.2", 900.0),
         "AP3p4_1600K": ("AP3.4", 1600.0),
         "AP3p4_1800K": ("AP3.4", 1800.0),
         "ANP6_1600K": ("ANP6", 1600.0)}


def digest(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def safe_json(value):
    """Explicit nonfinite encoding; never silently replace a diagnostic by zero."""
    if isinstance(value, np.ndarray):
        return safe_json(value.tolist())
    if isinstance(value, np.generic):
        return safe_json(value.item())
    if isinstance(value, Path):
        return str(value)
    if isinstance(value, float) and not math.isfinite(value):
        return {"nonfinite": repr(value)}
    if isinstance(value, dict):
        return {str(k): safe_json(v) for k, v in value.items()}
    if isinstance(value, (list, tuple)):
        return [safe_json(v) for v in value]
    return value


def write_json(path, value):
    Path(path).write_text(json.dumps(safe_json(value), indent=2, allow_nan=False) + "\n")


def observe_call(stage, location):
    node = ast.Expr(value=ast.Call(func=ast.Name(id="__un_observe__", ctx=ast.Load()),
                    args=[ast.Constant(stage), ast.Call(func=ast.Name(id="locals", ctx=ast.Load()),
                                                       args=[], keywords=[])], keywords=[]))
    return ast.copy_location(node, location)


def instrument(tree):
    original = copy.deepcopy(tree)
    solver = next(n for n in tree.body if isinstance(n, ast.FunctionDef) and n.name == "solve_UN")
    loop = next(n for n in solver.body if isinstance(n, ast.For))
    # Exact, hash-pinned statement anchors. These do not change arithmetic or branches.
    anchors = {
        "Dg, D_parts = xe_diffusivity_UN(p)": "old_state",
        "if c_new < 0.0 or mb_new < 0.0 or md_new < 0.0:": "after_exchange",
        "dmb_dt = (mb_new - mb_old) / dt": "after_nonnegative_branch",
        "if p.update_bulk_vacancies and (not bulk_first_gas_seed_applied):": "after_seed",
        "if N_b > 0.0:": "after_vacancies",
        "lambda_d = coalescence_lambda(Vd_old, Nd_old)": "after_growth_predictor",
        "rho_next = rho_d_for_time_burnup(p, burnup_new)": "after_coalescence",
        "delta_Rd_cap = max(R_d - Rd_old, 0.0)": "after_geometry",
        "generated += beta * dt": "after_capture",
        "t += dt": "after_grainface",
    }
    inserted = []
    body = []
    for node in loop.body:
        first_line = ast.unparse(node).splitlines()[0]
        if first_line in anchors:
            stage = anchors[first_line]
            body.append(observe_call(stage, node))
            inserted.append(stage)
        body.append(node)
    assert set(inserted) == set(anchors.values()), (inserted, anchors)
    body.append(observe_call("step_end", body[-1]))
    loop.body = body
    position = solver.body.index(loop)
    solver.body.insert(position, observe_call("initial", loop))

    class ReturnObserver(ast.NodeTransformer):
        def __init__(self, name):
            self.name = name

        def visit_FunctionDef(self, node):
            # Do not instrument nested bisection helpers or change their closures.
            return node

        def visit_Return(self, node):
            return [observe_call("function_return:" + self.name, node), node]

    for function in tree.body:
        if isinstance(function, ast.FunctionDef) and function.name in {
            "vacancy_concentration_implicit_step", "seed_vacancies_for_pressure_factor",
            "_limited_growth_grainface", "_solve_grainface_3x3", "update_grainface_state"
        }:
            visitor = ReturnObserver(function.name)
            # Visit statements, not the function definition itself.
            observed_body = []
            for node in function.body:
                result = visitor.visit(node)
                observed_body.extend(result if isinstance(result, list) else [result])
            function.body = observed_body
            stages_name = "function_return:" + function.name
            inserted.append(stages_name)

    stripped = copy.deepcopy(tree)

    class StripObserver(ast.NodeTransformer):
        def visit_Expr(self, node):
            if isinstance(node.value, ast.Call) and isinstance(node.value.func, ast.Name):
                if node.value.func.id == "__un_observe__":
                    return None
            return self.generic_visit(node)

    stripped = StripObserver().visit(stripped)
    assert ast.dump(stripped, include_attributes=False) == ast.dump(original, include_attributes=False)
    ast.fix_missing_locations(tree)
    return tree, inserted + ["initial", "step_end"]


def load_notebook(observer, plain=False):
    assert digest(NOTEBOOK) == APPROVED_SHA256, "Reference notebook hash changed"
    book = json.loads(NOTEBOOK.read_text())
    module = types.ModuleType("un_l3_frozen_reference")
    sys.modules[module.__name__] = module
    ns = module.__dict__
    ns["__un_observe__"] = observer
    hashes, stages = {}, []
    for index in CELLS:
        source = "".join(book["cells"][index]["source"])
        hashes[str(index)] = hashlib.sha256(source.encode()).hexdigest()
        tree = ast.parse(source)
        if index == 8 and not plain:
            tree, stages = instrument(tree)
        exec(compile(tree, f"{NOTEBOOK}:cell_{index}", "exec"), ns)
    expected = {"DT_H": 3.0, "N_MODES": 160, "GRAIN_RADIUS": 6e-6,
                "XE_DIFFUSIVITY_MODE": "rizk2025_refit_plot",
                "VU_DIFFUSIVITY_MODE": "rizk2025_refit_full", "DV_GB_MODE": "rizk_legacy_1e6_Dv1",
                "RHO_MODE": "constant", "USE_NUCLEATION_MASS_COUPLING": True,
                "USE_BULK_DISLOCATION_CAPTURE": True, "USE_INTERGRANULAR_MODEL": True,
                "USE_PHI_GAS_RESOLUTION": False, "USE_FIRST_GAS_PRESSURE_SEED": True,
                "USE_EQUILIBRIUM_DIMER_SEED": False, "USE_DYNAMIC_RHO_D_NUCLEATION": False,
                "INTERGRANULAR_POST_SAT_MODE": "pastore_3x3_exact_release"}
    for key, value in expected.items():
        assert ns[key] == value, (key, ns[key], value)
    assert ns["MANUAL_PARAMS"]["f_n"] == 4.2e-4
    assert ns["MANUAL_PARAMS"]["K_d"] == 4.1e5
    assert ns["MANUAL_PARAMS"]["rho_d"] == 3.5e13
    return ns, hashes, stages


class Observer:
    def __init__(self, path):
        self.raw = open(path, "xb")
        self.gz = gzip.GzipFile(filename="", mode="wb", fileobj=self.raw, mtime=0)
        self.records = 0
        self.current_step = -1
        self.modes = {stage: [] for stage in ("initial", "after_exchange", "after_capture", "step_end")}
        self.times = {stage: [] for stage in self.modes}
        self.flags = {"negative_average_resets": 0, "bulk_seeds": 0, "dislocation_seeds": 0}

    def __call__(self, stage, state):
        # Read only, copy all observed values immediately; never retain mutable locals.
        if stage == "old_state":
            self.current_step = int(state["_"])
        scalars = {k: v for k, v in state.items() if isinstance(v, (float, int, bool, str))}
        nested = {k: dict(state[k]) for k in ("D_parts", "Dv_parts", "trapping_parts", "gf") if k in state}
        if stage == "step_end":
            nested["last_rates"] = dict(state["last_rates"])
            self.flags["bulk_seeds"] += int(state["bulk_first_gas_seed_applied"])
            self.flags["dislocation_seeds"] += int(state["dislocation_first_gas_seed_applied"])
        if stage == "after_exchange":
            self.flags["negative_average_resets"] += int(any(state[k] < 0 for k in ("c_new", "mb_new", "md_new")))
        if stage in self.modes:
            self.modes[stage].append([list(state[k]) for k in ("modes_c", "modes_mb", "modes_md")])
            self.times[stage].append(float(state["t"]) + (float(state["dt"]) if stage.startswith("after_") else 0.0))
        row = {"stage": stage, "step": self.current_step, "scalars": scalars, **nested}
        self.gz.write((json.dumps(safe_json(row), allow_nan=False, separators=(",", ":")) + "\n").encode())
        self.records += 1

    def close(self, folder):
        self.gz.close()
        self.raw.close()
        arrays = {}
        for stage in self.modes:
            arrays[stage] = np.asarray(self.modes[stage], dtype=np.float64)
            arrays[stage + "_time_s"] = np.asarray(self.times[stage], dtype=np.float64)
        np.savez_compressed(folder / "modes.npz", **arrays)


def csv_history(path, history):
    with open(path, "x", newline="") as handle:
        writer = csv.writer(handle)
        writer.writerow(history)
        writer.writerows(zip(*(history[k] for k in history)))  # Python repr preserves float64 round trips.


def run_case(ns, case_key, suite, diagnostic_steps, folder, plain):
    folder.mkdir(parents=True, exist_ok=False)
    observer = None if plain else Observer(folder / "trace.jsonl.gz")
    ns["__un_observe__"] = observer
    original_solver = ns["solve_UN"]
    captured = {}

    def execute(p, keep_history=True):
        full_final_time = p.final_time
        if suite == "diagnostic":
            p.final_time = min(full_final_time, diagnostic_steps * p.dt)
        captured["parameters"] = dataclasses.asdict(p)
        captured["full_case_final_time_s"] = full_final_time
        assert p.vacancy_absorption_only is False
        assert p.n_modes == 160 and p.dt == 10800.0
        assert p.Dg_dislocation_scale == 17.0 and p.Dv_dislocation_scale == 10.0
        start = time.perf_counter()
        hist, rates = original_solver(p, keep_history=True)
        captured["solver_wall_seconds"] = time.perf_counter() - start
        captured["history"], captured["rates"] = hist, rates
        return hist, rates

    ns["solve_UN"] = execute
    start = time.perf_counter()
    stream = io.StringIO()
    error = None
    warning_records = []
    try:
        with warnings.catch_warnings(record=True) as caught, contextlib.redirect_stdout(stream), contextlib.redirect_stderr(stream):
            warnings.simplefilter("always")
            case_id, temperature = CASES[case_key]
            ns["run_model_point_case"](temperature, case_id, ns["make_candidate"](),
                                       dt_h=3.0, n_modes=160, keep_history=True)
            warning_records = [{"category": w.category.__name__, "message": str(w.message),
                                "filename": w.filename, "line": w.lineno} for w in caught]
    except Exception:
        error = traceback.format_exc()
    finally:
        ns["solve_UN"] = original_solver
        if observer:
            observer.close(folder)
    wall = time.perf_counter() - start
    (folder / "stdout_stderr.txt").write_text(stream.getvalue())
    write_json(folder / "warnings.json", warning_records)
    if error:
        (folder / "error.txt").write_text(error)
        write_json(folder / "manifest.json", {"status": "error", "wall_seconds": wall,
                   "case": case_key, "suite": suite, **{k: v for k, v in captured.items() if k not in ("history", "rates")}})
        raise RuntimeError(f"{case_key} failed; see {folder / 'error.txt'}")
    hist, rates = captured.pop("history"), captured.pop("rates")
    initial_gas = sum(captured["parameters"][k] for k in ("c0", "mb0", "md0"))
    hist["mass_balance_residual"] = [initial_gas + g - (c + b + d + gf + rel)
        for g, c, b, d, gf, rel in zip(*(hist[k] for k in ("generated", "c", "mb", "md", "q_gf", "q_rel")))]
    for pop in ("b", "d"):
        hist[f"pressure_ratio_{pop}"] = [a/b if b != 0 else math.nan
            for a, b in zip(hist[f"p_{pop}"], hist[f"p_{pop}_eq"])]
        hist[f"vacancies_per_bubble_{pop}"] = [v/n if n > 0 else 0.0
            for v, n in zip(hist[f"nv{pop}"], hist[f"N{pop}"])]
    csv_history(folder / "history.csv", hist)
    np.savez_compressed(folder / "history.npz", **{k: np.asarray(v) for k, v in hist.items()})
    write_json(folder / "initial_state.json", {k: v[0] for k, v in hist.items()})
    write_json(folder / "final_state.json", {k: v[-1] for k, v in hist.items()})
    write_json(folder / "last_rates.json", rates)
    nonfinite = {k: [i for i, v in enumerate(vs) if not math.isfinite(v)]
                 for k, vs in hist.items() if any(not math.isfinite(v) for v in vs)}
    manifest = {"status": "ok", "case": case_key, "suite": suite, "plain_original": plain,
                "notebook_sha256": APPROVED_SHA256, "wall_seconds_including_output": wall,
                "steps": len(hist["time"]) - 1, "final_time_s": hist["time"][-1],
                "last_dt_s": hist["time"][-1] - hist["time"][-2],
                "final_fima_percent": hist["burnup_percent_fima"][-1],
                "max_abs_mass_balance_residual": max(map(abs, hist["mass_balance_residual"])),
                "max_mass_balance_residual_over_final_production": max(map(abs, hist["mass_balance_residual"])) / hist["generated"][-1],
                "nonfinite_history_indices": nonfinite, "warning_count": len(warning_records),
                "observer_flags": observer.flags if observer else None,
                "trace_records": observer.records if observer else 0, **captured}
    manifest["output_sha256"] = {p.name: digest(p) for p in sorted(folder.iterdir()) if p.is_file()}
    write_json(folder / "manifest.json", manifest)
    print(f"{suite}/{case_key}: {manifest['steps']} steps, {captured['solver_wall_seconds']:.3f} s solver, status=ok", flush=True)
    return manifest


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True, help="New directory; refuses overwrite")
    parser.add_argument("--suite", choices=("diagnostic", "full"), required=True)
    parser.add_argument("--case", choices=CASES, action="append")
    parser.add_argument("--diagnostic-steps", type=int, default=3)
    parser.add_argument("--plain", action="store_true", help="Untouched original solver, without observation")
    args = parser.parse_args()
    if args.diagnostic_steps < 1:
        parser.error("diagnostic-steps must be positive")
    args.output = args.output.resolve()
    args.output.mkdir(parents=True, exist_ok=False)
    start = time.perf_counter()
    ns, hashes, stages = load_notebook(None, args.plain)
    (args.output / "reference_notebook.ipynb").write_bytes(NOTEBOOK.read_bytes())
    versions = {}
    for pkg in ("numpy", "pandas", "matplotlib", "scipy", "cmake"):
        try:
            versions[pkg] = importlib.metadata.version(pkg)
        except importlib.metadata.PackageNotFoundError:
            versions[pkg] = None
    # All public scalar/container settings, including tables, plus dataclass values per case.
    settings = {k: v for k, v in ns.items() if k.isupper() and isinstance(v, (bool, int, float, str, dict, list, tuple))}
    write_json(args.output / "notebook_settings.json", settings)
    results = []
    for key in args.case or CASES:
        results.append(run_case(ns, key, args.suite, args.diagnostic_steps, args.output / key, args.plain))
    manifest = {"schema": 1, "phase": "Python reference only; no C++ port",
                "notebook_sha256": APPROVED_SHA256, "cell_indices_zero_based": CELLS,
                "cell_source_sha256": hashes, "observer_ast_roundtrip_exact": not args.plain,
                "observer_stages": stages, "command": sys.argv,
                "runner_sha256": digest(__file__), "python": sys.version,
                "executable": sys.executable, "platform": platform.platform(), "packages": versions,
                "git_head": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip(),
                "git_status": subprocess.check_output(["git", "status", "--short"], cwd=ROOT, text=True),
                "suite": args.suite, "diagnostic_steps": args.diagnostic_steps if args.suite == "diagnostic" else None,
                "total_wall_seconds": time.perf_counter() - start,
                "results": results}
    write_json(args.output / "manifest.json", manifest)


if __name__ == "__main__":
    main()
