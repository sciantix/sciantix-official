#!/usr/bin/env python3
"""Calibration of the two-zone FMP model on the four UO2 LWR TEM cases (EC November 2026).

Free parameters, the grain-boundary ones no longer equal to the intragranular ones:
    k_intra_0, B_intra   growth prefactor and CNT barrier coefficient of the intragranular particles
    k_gb_0,    B_gb      the same for the grain-boundary particles
fixed: dG_intra = dG_gb = 1 eV, nu0_intra = nu0_gb = 1 1/s (as in the calibration of V. Bellani).

Objective J4 = mean |ln(model/exp)| over R_intra, R_GB, A_intra, A_GB at r/R = 0, 0.30, 0.56, 0.75
(16 observables, as plot.py); J3 (without A_GB) is reported for comparison with the previous calibration.

Steps:
1. copy of src/, include/, tests/ and CMakeLists.txt of this checkout in build/fmp_calibration/code/ (ignored
   by git, rebuilt when the executable is missing), with the four parameters read from the environment in
   src/models/FiveMetalParticles.C of the copy (the sources of the repository are not modified), built without
   OpenCalphad; the runs are in build/fmp_calibration/work/;
2. scrambled Sobol sampling (seed 42) of the four parameters, plus the nominal point;
3. Nelder-Mead refinement from the best Sobol points (multi-start, run at the same time).

Usage:
    python3 calibrate.py [--n-sobol 4096] [--jobs 4] [--maxfev 400] [--starts 5] [--reuse-sobol]
Results in results/: sobol.csv (all the points), best.txt (nominal, best Sobol, refined).
"""

import argparse
import csv
import math
import os
import re
import shutil
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor

import numpy as np
from scipy.optimize import minimize
from scipy.stats import qmc

HERE = os.path.dirname(os.path.abspath(__file__))
GROUP_DIR = os.path.dirname(HERE)
REPO = os.path.abspath(os.path.join(GROUP_DIR, "..", ".."))
BUILD_DIR = os.path.join(REPO, "build", "fmp_calibration")  # build/ is ignored by git
CODE_DIR = os.path.join(BUILD_DIR, "code")
EXE = os.path.join(CODE_DIR, "build", "sciantix.x")
WORK_DIR = os.path.join(BUILD_DIR, "work")
RESULTS_DIR = os.path.join(HERE, "results")

sys.path.insert(0, GROUP_DIR)
from plot import BURNUP_EXP, DATA_DIR, POSITIONS, read_exp  # noqa: E402

# name, environment variable, nominal value, range of the factor on the nominal value, log sampling
PARAMETERS = [
    ("k_intra_0", "FMP_K_INTRA_0", 4.980333299845e-15, (1.0e-2, 1.0e2), True),
    ("B_intra", "FMP_B_INTRA", 2.665764962509e8, (0.5, 1.5), False),
    ("k_gb_0", "FMP_K_GB_0", 4.980333299845e-15, (1.0e-1, 1.0e3), True),
    ("B_gb", "FMP_B_GB", 2.665764962509e8, (0.5, 1.5), False),
]

EXPERIMENT = {
    name: (read_exp(os.path.join(DATA_DIR, "intra", f"exp_intra_{name}.txt")),
           read_exp(os.path.join(DATA_DIR, "inter", f"exp_inter_{name}.txt")))
    for name, _, _ in POSITIONS
}

# Lines of FiveMetalParticles.C with the four parameters; the values in the source are the defaults of the copy
PARAMETER_LINES = [
    (re.compile(r"^(\s*)const double k_intra_0 = ([^,]+), k_gb_0 = ([^;]+);.*$", re.M),
     "{0}// CALIBRATION COPY (validation/metallicinclusions/calibration_two_zones): parameters from the environment\n"
     "{0}auto env = [](const char* name, double value) {{ const char* text = std::getenv(name); "
     "return text ? std::atof(text) : value; }};\n"
     '{0}const double k_intra_0 = env("FMP_K_INTRA_0", {1}), k_gb_0 = env("FMP_K_GB_0", {2});'),
    (re.compile(r"^(\s*)const double B_intra = ([^,]+), B_gb = ([^;]+);.*$", re.M),
     '{0}const double B_intra = env("FMP_B_INTRA", {1}), B_gb = env("FMP_B_GB", {2});'),
]


def prepare_code():
    """Copy of the code with the parameter hook, built without OpenCalphad."""
    if os.path.isfile(EXE):
        return
    shutil.rmtree(CODE_DIR, ignore_errors=True)
    os.makedirs(CODE_DIR)
    for item in ("src", "include", "tests"):
        shutil.copytree(os.path.join(REPO, item), os.path.join(CODE_DIR, item))
    shutil.copy(os.path.join(REPO, "CMakeLists.txt"), CODE_DIR)

    path = os.path.join(CODE_DIR, "src", "models", "FiveMetalParticles.C")
    with open(path) as f:
        source = f.read()
    for pattern, template in PARAMETER_LINES:
        source, count = pattern.subn(lambda m: template.format(*m.groups()), source, count=1)
        if count != 1:
            sys.exit(f"FiveMetalParticles.C: line not found ({pattern.pattern}), update PARAMETER_LINES")
    source = source.replace('#include "Simulation.h"', '#include "Simulation.h"\n#include <cstdlib>', 1)
    with open(path, "w") as f:
        f.write(source)

    build = os.path.join(CODE_DIR, "build")
    subprocess.run(["cmake", "-S", CODE_DIR, "-B", build, "-DCOUPLING_OC=OFF", "-DBUILD_TESTING=OFF",
                    "-DCMAKE_BUILD_TYPE=Release"], check=True, stdout=subprocess.DEVNULL)
    subprocess.run(["cmake", "--build", build, "-j", str(os.cpu_count())], check=True, stdout=subprocess.DEVNULL)


def values_of(unit_point):
    """Parameter values from a point of the unit hypercube."""
    values = []
    for (_, _, nominal, (low, high), log), u in zip(PARAMETERS, unit_point):
        factor = 10 ** (math.log10(low) + u * (math.log10(high) - math.log10(low))) if log else low + u * (high - low)
        values.append(nominal * factor)
    return values


def unit_of(values):
    point = []
    for (_, _, nominal, (low, high), log), value in zip(PARAMETERS, values):
        factor = value / nominal
        point.append((math.log10(factor) - math.log10(low)) / (math.log10(high) - math.log10(low)) if log
                     else (factor - low) / (high - low))
    return point


def read_state(output_path):
    """Row of output.txt closest to the burnup of the TEM data."""
    with open(output_path) as f:
        header = f.readline().rstrip("\n").split("\t")
        rows = [line.rstrip("\n").split("\t") for line in f if line.strip()]
    column = {name.strip(): i for i, name in enumerate(header)}
    burnup = [float(row[column["Burnup (MWd/kgUO2)"]]) for row in rows]
    row = rows[int(np.argmin(np.abs(np.array(burnup) - BURNUP_EXP)))]
    return lambda name: float(row[column[name]])


def evaluate(values, slot):
    """Runs the four cases at the same time and returns J3, J4 and the observables."""
    env = dict(os.environ)
    for (_, variable, _, _, _), value in zip(PARAMETERS, values):
        env[variable] = f"{float(value):.17g}"

    processes = []
    for name, _, _ in POSITIONS:
        case = os.path.join(WORK_DIR, f"slot{slot}", name)
        if not os.path.isdir(case):
            source = os.path.join(GROUP_DIR, f"test_FMP_UO2_{name}")
            os.makedirs(case)
            for item in os.listdir(source):
                if item.startswith("input_"):
                    shutil.copy(os.path.join(source, item), case)
        processes.append(subprocess.Popen([EXE, case + "/"], env=env, stdout=subprocess.DEVNULL,
                                          stderr=subprocess.DEVNULL))
    for process in processes:
        process.wait()

    errors3, errors4, observables = [], [], []
    for name, _, thickness in POSITIONS:
        try:
            at = read_state(os.path.join(WORK_DIR, f"slot{slot}", name, "output.txt"))
            phi_gb = at("FMP grain-boundary zone volume fraction (/)")
            r_intra, r_gb = at("Intragranular FMP radius (m)"), at("Intergranular FMP radius (m)")
            n_intra = max(at("Intragranular FMP concentration (FMP/m3)"), 0.0) / (1.0 - phi_gb)
            n_gb = max(at("Intergranular FMP concentration (FMP/m3)"), 0.0) / phi_gb
            model = [r_intra, r_gb, -math.expm1(-n_intra * math.pi * r_intra**2 * thickness),
                     -math.expm1(-n_gb * math.pi * r_gb**2 * thickness)]
        except (OSError, KeyError, ValueError, IndexError, ZeroDivisionError):
            model = [0.0, 0.0, 0.0, 0.0]
        intra, inter = EXPERIMENT[name]
        experiment = [intra["radius"], inter["radius"], intra["area_fraction"], inter["area_fraction"]]
        error = [abs(math.log(m / e)) if m > 0.0 else math.inf for m, e in zip(model, experiment)]
        errors3 += error[:3]
        errors4 += error
        observables += model
    return float(np.mean(errors3)), float(np.mean(errors4)), observables


def observable_names():
    return [f"{quantity}_{name}" for name, _, _ in POSITIONS for quantity in ("R_intra", "R_GB", "A_intra", "A_GB")]


def describe(label, values, j3, j4, observables):
    lines = [f"{label}: J4 = {j4:.4f}, J3 = {j3:.4f}"]
    lines += [f"    {p[0]:<10} = {v:.6e}  (x {v / p[2]:.4g} of the nominal value)" for p, v in zip(PARAMETERS, values)]
    for k, (name, _, _) in enumerate(POSITIONS):
        r_intra, r_gb, a_intra, a_gb = observables[4 * k:4 * k + 4]
        intra, inter = EXPERIMENT[name]
        lines.append(f"    {name}: R_intra {r_intra * 1e9:6.2f} ({intra['radius'] * 1e9:5.2f}) nm, "
                     f"R_GB {r_gb * 1e9:6.2f} ({inter['radius'] * 1e9:5.2f}) nm, "
                     f"A_intra {a_intra * 100:6.3f} ({intra['area_fraction'] * 100:5.3f}) %, "
                     f"A_GB {a_gb * 100:6.3f} ({inter['area_fraction'] * 100:5.3f}) %")
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--n-sobol", type=int, default=4096)
    parser.add_argument("--jobs", type=int, default=4, help="parameter sets run at the same time (4 cases each)")
    parser.add_argument("--maxfev", type=int, default=400, help="evaluations of each Nelder-Mead refinement")
    parser.add_argument("--starts", type=int, default=5, help="Nelder-Mead refinements, from the best Sobol points")
    parser.add_argument("--reuse-sobol", action="store_true", help="read results/sobol.csv instead of sampling")
    args = parser.parse_args()

    prepare_code()
    os.makedirs(RESULTS_DIR, exist_ok=True)

    nominal = [p[2] for p in PARAMETERS]
    sobol_path = os.path.join(RESULTS_DIR, "sobol.csv")
    if args.reuse_sobol:
        with open(sobol_path) as f:
            rows = list(csv.DictReader(f))
        sets = [[float(row[p[0]]) for p in PARAMETERS] for row in rows]
        results = [(float(row["J3"]), float(row["J4"]), [float(row[n]) for n in observable_names()]) for row in rows]
    else:
        sampler = qmc.Sobol(d=len(PARAMETERS), scramble=True, seed=42)
        sets = [nominal] + [values_of(u) for u in sampler.random(args.n_sobol)]

        def run(index):
            return evaluate(sets[index], index % args.jobs)

        results = []
        with ThreadPoolExecutor(max_workers=args.jobs) as pool:
            # each slot (work directory) is used by one set at a time: sets are submitted in groups of args.jobs
            for start in range(0, len(sets), args.jobs):
                batch = range(start, min(start + args.jobs, len(sets)))
                results += list(pool.map(run, batch))
                if start % (args.jobs * 128) == 0:
                    print(f"Sobol {len(results)}/{len(sets)}, best J4 = {min(r[1] for r in results):.4f}", flush=True)

        with open(sobol_path, "w", newline="") as f:
            writer = csv.writer(f)
            writer.writerow([p[0] for p in PARAMETERS] + ["J3", "J4"] + observable_names())
            for values, (j3, j4, observables) in zip(sets, results):
                writer.writerow([f"{v:.6e}" for v in values] + [f"{j3:.6f}", f"{j4:.6f}"] +
                                [f"{o:.6e}" for o in observables])

    order = [int(i) for i in np.argsort([r[1] for r in results])]
    best_index = order[0]

    # Nelder-Mead in the unit hypercube (log scale for the prefactors), from the best Sobol points
    def refine(start):
        slot = 100 + start
        history = []

        def objective(unit_point):
            values = values_of(np.clip(unit_point, 0.0, 1.0))
            j3, j4, observables = evaluate(values, slot)
            history.append((values, j3, j4, observables))
            return j4

        result = minimize(objective, unit_of(sets[order[start]]), method="Nelder-Mead", bounds=[(0.0, 1.0)] * 4,
                          options={"maxfev": args.maxfev, "xatol": 1.0e-4, "fatol": 1.0e-5})
        return min(history, key=lambda h: h[2]), result.nfev

    with ThreadPoolExecutor(max_workers=args.starts) as pool:
        refinements = list(pool.map(refine, range(args.starts)))
    refined, _ = min(refinements, key=lambda r: r[0][2])

    starts = ["Nelder-Mead from the best Sobol points (start J4 -> refined J4, evaluations, parameters as factors):"]
    for k, ((values, j3, j4, _), nfev) in enumerate(refinements):
        factors = ", ".join(f"{p[0]} x{v / p[2]:.3g}" for p, v in zip(PARAMETERS, values))
        starts.append(f"    #{order[k]}: {results[order[k]][1]:.4f} -> {j4:.4f} ({nfev}), {factors}")

    report = [
        f"Two-zone FMP calibration, {len(sets) - 1} Sobol points (seed 42) + {args.starts} Nelder-Mead refinements",
        "",
        *starts,
        "",
        describe("Nominal (V. Bellani, common parameters)", nominal, *results[0]),
        "",
        describe(f"Best Sobol point (#{best_index})", sets[best_index], *results[best_index]),
        "",
        describe("Refined (best of the refinements)", refined[0], *refined[1:]),
    ]
    with open(os.path.join(RESULTS_DIR, "best.txt"), "w") as f:
        f.write("\n".join(report) + "\n")
    print("\n".join(report))


if __name__ == "__main__":
    main()
