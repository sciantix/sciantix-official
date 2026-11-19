"""
Test of the closure a = 3 (one pressure in every pore class) with the unit s_hat of MMM: copy of calibrate_sigma_r.py.
@ author: E.Cappellari, POLIMI-CEA 2026
@ date:   November 2026

With a = 3 every class has the same gas density, hence the same pressure: the load of the unit runs of MMM, so the
unit surrogate is the exact s_hat (P_h a rescaling up to 2 gamma / R) and no new MMM run is needed. a also sets the
width s that SCIANTIX derives from Var(n) (CV_n on the nodes depends on a s only: s(a = 3) = 2/3 s(a = 2)), the
pressure and the gas of the classes.

As calibrate_sigma_r.py, for the unit load only, with "1.5 # sf_hbs_closure_exponent" (a = 2 x 1.5 = 3) appended to
input_scaling_factors.txt of the temporary case:
1.  scan of sigma_R (SCAN) and the J19 case as the reference;
2.  the calibrated sigma_R (least rms distance of the release curve from Kr-85), and the sigma_R of the measured final
    release;
3.  the share of the rows of the heat-up whose state (xi, s with a = 3) lies outside the range of the runs.
The regression cases mmm_unit and mmm_pk (a = 2, their own sigma_R) are run too, for the comparison.

The calibrated sigma_R and the scaling factor are written into regression/hbs_nfir/test_UO2HBS_frag_mmm_unit_a3 (the
inputs of the J19 case). Its output_gold.txt is not touched: after a check, re-bless it with
    python3 -m regression.runner --hbs_nfir.mmm_unit_a3 --mode-gold 1
Writes mmm_outputs/calibration_nfir_a3.txt/png; the files of calibrate_sigma_r.py and its cases are not touched.

usage
    python3 utilities/HBFF/calibrate_sigma_r_a3.py
"""
# %% ------------------------------------------------------------------------------------------------------------------
# Libraries and paths
import os
import shutil
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
from scipy.optimize import brentq, minimize_scalar

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.normpath(os.path.join(HERE, "..", ".."))
sys.path.insert(0, HERE)
from pore_distribution import build, radius_of, read_output, state_of_row, transient_start, xe_volume  # noqa: E402

SCIANTIX = os.path.join(ROOT, "build", "sciantix.x")
NFIR = os.path.join(ROOT, "regression", "hbs_nfir")
J19_CASE = os.path.join(NFIR, "test_UO2HBS_frag_j19damage")  # its inputs are those of every run
OUT_DIR = os.path.join(HERE, "mmm_outputs")
SF_CLOSURE = 1.5  # sf_hbs_closure_exponent: a = 2 x 1.5 = 3
A_TEST = 2.0 * SF_CLOSURE
SCAN = tuple(v * 1e6 for v in (100, 125, 150, 175, 200, 225, 250, 300))  # (Pa)
TOL = 0.5e6  # (Pa) of the searches
WORKERS = 4  # SCIANTIX runs at the same time
SURROGATE = "input_hbs_shat.txt"  # the file option 4 reads in the folder of a case
REFERENCE = ("unit", "pk")  # regression cases test_UO2HBS_frag_mmm_<load>: a = 2, their calibrated sigma_R
BLUE, ORANGE, GREEN, GREY, GRID = "#2a78d6", "#eb6834", "#2f9e6e", "#898781", "#e1e0d9"  # as pore_distribution.py
COLOR = {"unit": BLUE, "pk": GREEN, "a3": "#8a3ffc"}

# %% ------------------------------------------------------------------------------------------------------------------
# Functions

def with_sigma(text, sigma_r):
    """The text of a surrogate file with sigma_R (Pa) in place of its own, and the comment above it."""
    lines = text.splitlines()
    k = next(j for j, line in enumerate(lines) if line.startswith("sigma_R "))
    if lines[k - 1].startswith("# sigma_R"):
        lines[k - 1] = ("# sigma_R (Pa) written by calibrate_sigma_r_a3.py: with a = 3 (sf_hbs_closure_exponent 1.5), the least "
                        "rms distance from the NFIR release (mmm_outputs/calibration_nfir_a3.txt)")
    lines[k] = f"sigma_R {sigma_r:.17g}"
    return "\n".join(lines) + "\n"


def sigma_of(text):
    """sigma_R (Pa) of a surrogate file."""
    return float(next(line.split()[1] for line in text.splitlines() if line.startswith("sigma_R ")))


def run(surrogate, sigma_r=None, sf=1.0):
    """SCIANTIX on the NFIR case with the surrogate file text (None: the J19 case), sigma_R (Pa) and the scaling
    factor of the closure exponent: its output."""
    with tempfile.TemporaryDirectory(prefix="nfir_") as case:
        for name in os.listdir(J19_CASE):
            if name.startswith("input_"):
                shutil.copy(os.path.join(J19_CASE, name), case)
        if sf != 1.0:
            with open(os.path.join(case, "input_scaling_factors.txt"), "a") as fh:
                fh.write(f"\n{sf:g} # sf_hbs_closure_exponent (a = 2 x sf)\n")
        if surrogate is not None:
            with open(os.path.join(case, SURROGATE), "w") as fh:
                fh.write(with_sigma(surrogate, sigma_r))
        done = subprocess.run([SCIANTIX, case + "/"], capture_output=True, text=True)
        if done.returncode != 0 or not os.path.exists(os.path.join(case, "output.txt")):
            raise SystemExit(f"SCIANTIX failed (sigma_R = {sigma_r}):\n{done.stdout[-2000:]}\n{done.stderr[-2000:]}")
        if sf != 1.0:
            with open(os.path.join(case, "input_check.txt")) as fh:
                if f"sf_hbs_closure_exponent = {sf:g}" not in fh.read():
                    raise SystemExit("SCIANTIX did not read sf_hbs_closure_exponent (input_check.txt)")
        return read_output(os.path.join(case, "output.txt"))


def release(o):
    """Minutes from the start of the transient and release of the transient (% of the gas produced)."""
    k0 = transient_start(o)
    return (o["Time"][k0:] - o["Time"][k0]) * 60.0, 100.0 * (o["Fission gas release"][k0:] - o["Fission gas release"][k0])


def distance(o, measured):
    """rms distance (points of release) of the release curve from the measured one, at the measured times."""
    minutes, rel = release(o)
    return float(np.sqrt(np.mean((np.interp(measured[:, 0], minutes, rel) - measured[:, 1]) ** 2)))


def heatup(o):
    """Rows of the heat-up of the anneal: from the start of the transient to the peak temperature."""
    k0 = transient_start(o)
    return slice(k0, k0 + int(np.argmax(o["Temperature"][k0:])) + 1)


def onset(o):
    """T (K) of the heat-up at which 10 % and 50 % of the final release is reached (nan if after the heat-up)."""
    rel, T = release(o)[1], o["Temperature"][heatup(o)]
    final, rel = rel[-1], rel[: len(T)]
    return [float(np.interp(q * final, rel, T)) if final > 0.0 and rel[-1] >= q * final else np.nan for q in (0.1, 0.5)]

# %% ------------------------------------------------------------------------------------------------------------------
# Main

def main():
    if not os.access(SCIANTIX, os.X_OK):
        raise SystemExit(f"{SCIANTIX} not found: build SCIANTIX (./Allmake.sh, or BUILD=1 in runHBFF.sh)")
    measured = np.loadtxt(os.path.join(NFIR, "data", "CumulativeRelease.txt"), delimiter=";", comments="#", skiprows=2,
                          encoding="utf-8")
    path = os.path.join(OUT_DIR, "surrogate_unit_sciantix.txt")
    if not os.path.exists(path):
        raise SystemExit(f"{path} not found: python3 mmm_surrogate.py --fit <tree of the hypercube> <run_NFIR>")
    surrogate = open(path).read()
    references = {load: open(os.path.join(NFIR, f"test_UO2HBS_frag_mmm_{load}", SURROGATE)).read() for load in REFERENCE}

    # %% 1. the J19 case, the regression cases (a = 2) and the scan of sigma_R with a = 3 ----------------------------
    outputs = {}  # sigma_R -> output, a = 3

    def get(sigma_r):
        key = round(sigma_r)
        if key not in outputs:
            outputs[key] = run(surrogate, sigma_r, SF_CLOSURE)
        return outputs[key]

    with ThreadPoolExecutor(WORKERS) as pool:
        j19 = pool.submit(run, None)
        ref = {load: pool.submit(run, text, sigma_of(text)) for load, text in references.items()}
        list(pool.map(get, SCAN))
        j19, ref = j19.result(), {load: f.result() for load, f in ref.items()}

    # %% 2. calibrated sigma_R: the rms of the curve, and the final release for comparison ------------------------
    rms = [distance(get(s), measured) for s in SCAN]
    j = int(np.argmin(rms))
    bounds = (SCAN[max(j - 1, 0)], SCAN[min(j + 1, len(SCAN) - 1)])
    best = float(minimize_scalar(lambda s: distance(get(s), measured), bounds=bounds, method="bounded",
                                 options={"xatol": TOL}).x)
    # the final release falls with sigma_R: the root is looked for from the largest sigma_R of the scan down
    final = lambda s: release(get(s))[1][-1] - measured[-1, 1]
    values = [final(s) for s in SCAN]
    root = np.nan
    for k in range(len(SCAN) - 1, 0, -1):
        if values[k - 1] > 0.0 >= values[k]:
            root = float(brentq(final, SCAN[k - 1], SCAN[k], xtol=TOL))
            break

    # %% 3. at the calibrated sigma_R: the state against the range of the runs of the surrogate ----------------------
    o = get(best)
    words = {line.split()[0]: line.split()[1:] for line in surrogate.splitlines()
             if line.split() and line.split()[0] in ("inputs", "lower", "upper")}
    names = words["inputs"][1:]
    lower, upper = dict(zip(names, map(float, words["lower"]))), dict(zip(names, map(float, words["upper"])))
    rows = [k for k in range(heatup(o).start, heatup(o).stop) if o["HBS gas retention fraction"][k] >= 0.01]
    out, s_range = 0, []
    for k in rows:
        st = state_of_row(o, k)
        state = {"xi": st["xi"], "s": build(radius_of(st["V"]), st["n"], st["var"], st["T"], a=A_TEST)["sigma"],
                 "eta_bar": st["n"] * xe_volume(st["T"]) / st["V"]}
        s_range.append(state["s"])
        out += any(not lower[q] <= state[q] <= upper[q] for q in names if q in state)
    clipped = 100.0 * out / max(len(rows), 1)

    # %% text --------------------------------------------------------------------------------------------------------
    def row(name, o, sigma_r):
        T10, T50 = onset(o)
        return (f"  {name:<14s}{sigma_r / 1e6:10.1f}{release(o)[1][-1]:11.2f}{distance(o, measured):8.2f}"
                f"{o['HBS fragmented fraction'][-1]:8.4f}{o['HBS burst release fraction'][-1]:8.4f}{T10:8.0f}{T50:8.0f}")
    head = f"# {'case':<14s}{'sigma_R':>10s}{'release_%':>11s}{'rms':>8s}{'D':>8s}{'G':>8s}{'T_10_K':>8s}{'T_50_K':>8s}"
    lines = ["# Closure a = 3 with the unit s_hat of MMM, NFIR anneal in closed loop (python3 calibrate_sigma_r_a3.py)",
             f"# SCIANTIX option 4 on the inputs of {os.path.relpath(J19_CASE, ROOT)}, with {SURROGATE} = "
             f"mmm_outputs/surrogate_unit_sciantix.txt and {SF_CLOSURE:g} # sf_hbs_closure_exponent (a = {A_TEST:g})",
             f"# release of the transient in % of the gas produced; measured Kr-85 at the end of the anneal: "
             f"{measured[-1, 1]:.2f} %",
             "# rms: distance of the release curve from the measured one at the measured times (points of release); D, G: "
             "broken pores (number) and their gas at the end; T_10, T_50: T of the heat-up at 10 % and 50 % of the final "
             "release",
             "#", "# == 1. Scan, a = 3 ==", head, row("J19", j19, 21.0e6)]
    lines += [row("unit a=3", get(s), s) for s in SCAN]
    lines += ["#", "# == 2. Calibrated, a = 3: sigma_R of the least rms of the curve, and the sigma_R of the measured "
              "final release; the regression cases (a = 2) at their sigma_R ==", head]
    lines.append(row("unit a=3 rms", get(best), best))
    lines.append(row("unit a=3 final", get(root), root) if np.isfinite(root) else "  unit a=3 final: no root in the scan")
    lines += [row(f"{load} a=2", ref[load], sigma_of(references[load])) for load in REFERENCE]
    lines += ["#", "# == 3. Rows of the heat-up (gas retained >= 1 %) with the state outside the range of the runs of the "
              "surrogate, at the calibrated sigma_R, a = 3: SCIANTIX clips them ==",
              f"  unit a=3      {clipped:6.1f} %   (s with a = 3 on these rows: {min(s_range):.3f}-{max(s_range):.3f}; "
              f"range of the runs {lower['s']:.3f}-{upper['s']:.3f})"]
    os.makedirs(OUT_DIR, exist_ok=True)
    with open(os.path.join(OUT_DIR, "calibration_nfir_a3.txt"), "w") as fh:
        fh.write("\n".join(lines) + "\n")
    print("\n".join(lines))

    # %% the regression case -----------------------------------------------------------------------------------------
    case = os.path.join(NFIR, "test_UO2HBS_frag_mmm_unit_a3")
    os.makedirs(case, exist_ok=True)
    for name in os.listdir(J19_CASE):
        if name.startswith("input_"):
            shutil.copy(os.path.join(J19_CASE, name), case)
    with open(os.path.join(case, "input_scaling_factors.txt"), "a") as fh:
        fh.write(f"\n{SF_CLOSURE:g} # sf_hbs_closure_exponent (a = 2 x sf)\n")
    with open(os.path.join(case, SURROGATE), "w") as fh:
        fh.write(with_sigma(surrogate, round(best / 1e5) * 1e5))  # 0.1 MPa
    print(f"wrote {os.path.relpath(case, ROOT)} (sigma_R = {best / 1e6:.1f} MPa, a = {A_TEST:g}); "
          f"its gold: python3 -m regression.runner --hbs_nfir.mmm_unit_a3 --mode-gold 1")

    # %% figure ------------------------------------------------------------------------------------------------------
    fig, ax = plt.subplots(2, 2, figsize=(12.5, 9.0))
    a = ax[0, 0]
    a.plot(*release(j19), color=ORANGE, lw=1.6, label="J19 mean field, 21 MPa")
    for load in REFERENCE:
        a.plot(*release(ref[load]), color=COLOR[load], lw=1.6, ls="--",
               label=f"MMM {load}, a = 2, $\\sigma_R$ = {sigma_of(references[load]) / 1e6:.0f} MPa")
    a.plot(*release(get(best)), color=COLOR["a3"], lw=2.0, label=f"MMM unit, a = 3, $\\sigma_R$ = {best / 1e6:.0f} MPa")
    a.plot(measured[:, 0], measured[:, 1], "o", ms=3, mfc="none", color="k", label="NFIR, Noirot et al., 2014 (Kr-85)")
    a.set(xlabel="time from the start of the transient (min)", ylabel="release (% of the gas produced)",
          title="Release of the anneal, closed loop (least rms)")
    a = ax[0, 1]
    for name, o, color, ls in [("unit, a = 2", ref["unit"], COLOR["unit"], "--"), ("pk, a = 2", ref["pk"], COLOR["pk"], "--"),
                               ("unit, a = 3", get(best), COLOR["a3"], "-")]:
        T = o["Temperature"][heatup(o)]
        a.plot(T, o["HBS fragmented fraction"][heatup(o)], color=color, lw=1.7, ls=ls, label=f"D, {name}")
        a.plot(T, o["HBS burst release fraction"][heatup(o)], color=color, lw=1.0, ls=ls, label=f"G, {name}")
    a.set(xlabel="T (K)", ylabel="fraction", ylim=(0, 1), title=r"Broken pores (D, thick) and their gas (G, thin)")
    s = np.array(sorted(outputs))
    for a, what, label in ((ax[1, 0], "rms", "rms distance from the measured release (points)"),
                           (ax[1, 1], "final", "release at the end of the anneal (%)")):
        y = [distance(outputs[v], measured) for v in s] if what == "rms" else [release(outputs[v])[1][-1] for v in s]
        a.plot(s / 1e6, y, marker="o", color=COLOR["a3"], ms=5, lw=1.4, label="MMM unit, a = 3")
        for load in REFERENCE:
            v = distance(ref[load], measured) if what == "rms" else release(ref[load])[1][-1]
            a.plot(sigma_of(references[load]) / 1e6, v, marker="s", color=COLOR[load], ms=7, ls="none",
                   label=f"MMM {load}, a = 2 (regression case)")
        if what == "final":
            a.axhline(measured[-1, 1], color="black", lw=1.0, ls="--", label="measured (Kr-85)")
        a.set(xlabel=r"$\sigma_R$ (MPa)", ylabel=label, ylim=(0, None),
              title="Distance of the release curve" if what == "rms" else "Final release")
        a.legend(frameon=False, fontsize=8)
    for a in ax.ravel():
        a.grid(True, color=GRID)
    ax[0, 0].legend(frameon=False, fontsize=8)
    ax[0, 1].legend(frameon=False, fontsize=7.5)
    fig.tight_layout()
    fig.savefig(os.path.join(OUT_DIR, "calibration_nfir_a3.png"), dpi=150)
    plt.close(fig)
    print("wrote mmm_outputs/calibration_nfir_a3.txt and mmm_outputs/calibration_nfir_a3.png")
    return 0


if __name__ == "__main__":
    sys.exit(main())
