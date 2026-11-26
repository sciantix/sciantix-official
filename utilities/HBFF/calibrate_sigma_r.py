"""
sigma_R of the s_hat of MMM, calibrated on the NFIR anneal in closed loop: SCIANTIX (option 4 with the surrogate of
MMM in input_hbs_shat.txt) run on the NFIR case for several sigma_R, against the Kr-85 release measured in the anneal
(Noirot 2014, regression/hbs_nfir/data/CumulativeRelease.txt).
@ author: E.Cappellari, POLIMI-CEA 2026
@ date:   November 2026

For every load (unit, pk) of mmm_outputs/surrogate_<load>_sciantix.txt (written by mmm_surrogate.py --fit):
1.  scan of sigma_R (SCAN), one SCIANTIX run each, in a temporary copy of the inputs of the J19 case of NFIR;
2.  the calibrated sigma_R: the one that minimises the rms distance of the release curve from the measured one, at the
    measured times (bounded search around the best point of the scan); for comparison, the sigma_R that gives the
    measured release at the end of the anneal;
3.  at the calibrated sigma_R: D, G, T at 10 % and 50 % of the final release, and the share of the rows of the heat-up
    whose state (xi, s, eta_bar) lies outside the range of the runs of the surrogate (SCIANTIX clips it).
The J19 case (mean field, 21 MPa) is run too, as the reference.

The calibrated sigma_R is written into regression/hbs_nfir/test_UO2HBS_frag_mmm_<load>/input_hbs_shat.txt (the case is
made with the inputs of the J19 case). Its output_gold.txt is not touched: after a check, re-bless it with
    python3 -m regression.runner --hbs_nfir.mmm_<load> --mode-gold 1
Writes mmm_outputs/calibration_nfir.txt/png. About 10 minutes (one SCIANTIX run is 12 s).

usage
    python3 utilities/HBFF/calibrate_sigma_r.py
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
from pore_distribution import classes_of_row, read_output, state_of_row, transient_start, xe_volume  # noqa: E402

SCIANTIX = os.path.join(ROOT, "build", "sciantix.x")
NFIR = os.path.join(ROOT, "regression", "hbs_nfir")
J19_CASE = os.path.join(NFIR, "test_UO2HBS_frag_j19damage")  # its inputs are those of every run
OUT_DIR = os.path.join(HERE, "mmm_outputs")
LOADS = ("unit", "pk")
SCAN = tuple(v * 1e6 for v in (150, 175, 200, 225, 250, 275, 300, 350))  # (Pa)
TOL = 0.5e6  # (Pa) of the searches
WORKERS = 4  # SCIANTIX runs at the same time
SURROGATE = "input_hbs_shat.txt"  # the file option 4 reads in the folder of a case
BLUE, ORANGE, GREEN, GREY, GRID = "#2a78d6", "#eb6834", "#2f9e6e", "#898781", "#e1e0d9"  # as pore_distribution.py
COLOR = {"unit": BLUE, "pk": GREEN}
MARKER = {"unit": "o", "pk": "s"}

# %% ------------------------------------------------------------------------------------------------------------------
# Functions

def with_sigma(text, sigma_r):
    """The text of a surrogate file with sigma_R (Pa) in place of its own, and the comment above it."""
    lines = text.splitlines()
    k = next(j for j, line in enumerate(lines) if line.startswith("sigma_R "))
    if lines[k - 1].startswith("# sigma_R"):
        lines[k - 1] = ("# sigma_R (Pa) written by calibrate_sigma_r.py: in the regression cases, the least rms distance "
                        "from the NFIR release (mmm_outputs/calibration_nfir.txt)")
    lines[k] = f"sigma_R {sigma_r:.17g}"
    return "\n".join(lines) + "\n"


def run(surrogate, sigma_r=None):
    """SCIANTIX on the NFIR case with the surrogate file text (None: the J19 case) and sigma_R (Pa): its output."""
    with tempfile.TemporaryDirectory(prefix="nfir_") as case:
        for name in os.listdir(J19_CASE):
            if name.startswith("input_"):
                shutil.copy(os.path.join(J19_CASE, name), case)
        if surrogate is not None:
            with open(os.path.join(case, SURROGATE), "w") as fh:
                fh.write(with_sigma(surrogate, sigma_r))
        done = subprocess.run([SCIANTIX, case + "/"], capture_output=True, text=True)
        if done.returncode != 0 or not os.path.exists(os.path.join(case, "output.txt")):
            raise SystemExit(f"SCIANTIX failed (sigma_R = {sigma_r}):\n{done.stdout[-2000:]}\n{done.stderr[-2000:]}")
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
    surrogates = {}
    for load in LOADS:
        path = os.path.join(OUT_DIR, f"surrogate_{load}_sciantix.txt")
        if not os.path.exists(path):
            raise SystemExit(f"{path} not found: python3 mmm_surrogate.py --fit <tree of the hypercube> <run_NFIR>")
        surrogates[load] = open(path).read()

    # %% 1. the J19 case and the scan of sigma_R ---------------------------------------------------------------------
    outputs = {}  # (load, sigma_R) -> output

    def get(load, sigma_r):
        key = (load, round(sigma_r))
        if key not in outputs:
            outputs[key] = run(surrogates[load], sigma_r)
        return outputs[key]

    with ThreadPoolExecutor(WORKERS) as pool:
        j19 = pool.submit(run, None)
        list(pool.map(lambda args: get(*args), [(load, s) for load in LOADS for s in SCAN]))
        j19 = j19.result()

    # %% 2. calibrated sigma_R: the rms of the curve, and the final release for comparison ------------------------
    def calibrate(load):
        rms = [distance(get(load, s), measured) for s in SCAN]
        j = int(np.argmin(rms))
        bounds = (SCAN[max(j - 1, 0)], SCAN[min(j + 1, len(SCAN) - 1)])
        best = minimize_scalar(lambda s: distance(get(load, s), measured), bounds=bounds, method="bounded",
                               options={"xatol": TOL}).x
        # the final release falls with sigma_R: the root is looked for from the largest sigma_R of the scan down
        final = lambda s: release(get(load, s))[1][-1] - measured[-1, 1]
        values = [final(s) for s in SCAN]
        root = np.nan
        for k in range(len(SCAN) - 1, 0, -1):
            if values[k - 1] > 0.0 >= values[k]:
                root = brentq(final, SCAN[k - 1], SCAN[k], xtol=TOL)
                break
        return float(best), float(root)

    with ThreadPoolExecutor(len(LOADS)) as pool:
        fitted = dict(zip(LOADS, pool.map(calibrate, LOADS)))

    # %% 3. at the calibrated sigma_R: the state against the range of the runs of the surrogate ----------------------
    clipped = {}
    for load in LOADS:
        o = get(load, fitted[load][0])
        words = {line.split()[0]: line.split()[1:] for line in surrogates[load].splitlines()
                 if line.split() and line.split()[0] in ("inputs", "lower", "upper")}
        names = words["inputs"][1:]
        lower, upper = dict(zip(names, map(float, words["lower"]))), dict(zip(names, map(float, words["upper"])))
        rows = [k for k in range(heatup(o).start, heatup(o).stop) if o["HBS gas retention fraction"][k] >= 0.01]
        out = 0
        for k in rows:
            st = state_of_row(o, k)
            state = {"xi": st["xi"], "s": classes_of_row(o, k)["sigma"],
                     "eta_bar": st["n"] * xe_volume(st["T"]) / st["V"]}
            out += any(not lower[q] <= state[q] <= upper[q] for q in names if q in state)
        clipped[load] = 100.0 * out / max(len(rows), 1)

    # %% text --------------------------------------------------------------------------------------------------------
    def row(name, o, sigma_r):
        T10, T50 = onset(o)
        return (f"  {name:<12s}{sigma_r / 1e6:10.1f}{release(o)[1][-1]:11.2f}{distance(o, measured):8.2f}"
                f"{o['HBS fragmented fraction'][-1]:8.4f}{o['HBS burst release fraction'][-1]:8.4f}{T10:8.0f}{T50:8.0f}")
    head = f"# {'case':<12s}{'sigma_R':>10s}{'release_%':>11s}{'rms':>8s}{'D':>8s}{'G':>8s}{'T_10_K':>8s}{'T_50_K':>8s}"
    lines = ["# sigma_R of the s_hat of MMM calibrated on the NFIR anneal in closed loop (python3 calibrate_sigma_r.py)",
             f"# SCIANTIX option 4 on the inputs of {os.path.relpath(J19_CASE, ROOT)}, with {SURROGATE} = "
             "mmm_outputs/surrogate_<load>_sciantix.txt and the sigma_R of each row",
             f"# release of the transient in % of the gas produced; measured Kr-85 at the end of the anneal: "
             f"{measured[-1, 1]:.2f} %",
             "# rms: distance of the release curve from the measured one at the measured times (points of release); D, G: "
             "broken pores (number) and their gas at the end; T_10, T_50: T of the heat-up at 10 % and 50 % of the final "
             "release",
             "#", "# == 1. Scan ==", head, row("J19", j19, 21.0e6)]
    for load in LOADS:
        lines += [row(load, get(load, s), s) for s in SCAN]
    lines += ["#", "# == 2. Calibrated: sigma_R of the least rms of the curve (written into the regression case), and the "
              "sigma_R of the measured final release ==", head]
    for load in LOADS:
        best, root = fitted[load]
        lines.append(row(f"{load} rms", get(load, best), best))
        lines.append(row(f"{load} final", get(load, root), root) if np.isfinite(root) else f"  {load} final: no root in the scan")
    lines += ["#", "# == 3. Rows of the heat-up (gas retained >= 1 %) with the state outside the range of the runs of the "
              "surrogate, at the calibrated sigma_R: SCIANTIX clips them =="]
    lines += [f"  {load:<12s}{clipped[load]:6.1f} %" for load in LOADS]
    os.makedirs(OUT_DIR, exist_ok=True)
    with open(os.path.join(OUT_DIR, "calibration_nfir.txt"), "w") as fh:
        fh.write("\n".join(lines) + "\n")
    print("\n".join(lines))

    # %% the regression cases ----------------------------------------------------------------------------------------
    for load in LOADS:
        case = os.path.join(NFIR, f"test_UO2HBS_frag_mmm_{load}")
        os.makedirs(case, exist_ok=True)
        for name in os.listdir(J19_CASE):
            if name.startswith("input_"):
                shutil.copy(os.path.join(J19_CASE, name), case)
        with open(os.path.join(case, SURROGATE), "w") as fh:
            fh.write(with_sigma(surrogates[load], round(fitted[load][0] / 1e5) * 1e5))  # 0.1 MPa
        print(f"wrote {os.path.relpath(case, ROOT)}/{SURROGATE} (sigma_R = {fitted[load][0] / 1e6:.1f} MPa); "
              f"its gold: python3 -m regression.runner --hbs_nfir.mmm_{load} --mode-gold 1")

    # %% figure ------------------------------------------------------------------------------------------------------
    fig, ax = plt.subplots(2, 2, figsize=(12.5, 9.0))
    a = ax[0, 0]
    a.plot(*release(j19), color=ORANGE, lw=1.6, label="J19 mean field, 21 MPa")
    for load in LOADS:
        a.plot(*release(get(load, fitted[load][0])), color=COLOR[load], lw=1.8,
               label=f"MMM {load}, $\\sigma_R$ = {fitted[load][0] / 1e6:.0f} MPa (least rms)")
    a.plot(measured[:, 0], measured[:, 1], "o", ms=3, mfc="none", color="k", label="NFIR, Noirot et al., 2014 (Kr-85)")
    a.set(xlabel="time from the start of the transient (min)", ylabel="release (% of the gas produced)",
          title="Release of the anneal, closed loop")
    a = ax[0, 1]
    for load in LOADS:
        o = get(load, fitted[load][0])
        T = o["Temperature"][heatup(o)]
        a.plot(T, o["HBS fragmented fraction"][heatup(o)], color=COLOR[load], lw=1.7, label=f"D, MMM {load}")
        a.plot(T, o["HBS burst release fraction"][heatup(o)], color=COLOR[load], lw=1.7, ls="--", label=f"G, MMM {load}")
    a.plot(j19["Temperature"][heatup(j19)], j19["HBS fragmented fraction"][heatup(j19)], color=ORANGE, lw=1.4, label="D, J19")
    a.plot(j19["Temperature"][heatup(j19)], j19["HBS burst release fraction"][heatup(j19)], color=ORANGE, lw=1.4,
           ls="--", label="G, J19")
    a.set(xlabel="T (K)", ylabel="fraction", ylim=(0, 1), title=r"Broken pores (D) and their gas (G), calibrated $\sigma_R$")
    for a, what, label in ((ax[1, 0], "rms", "rms distance from the measured release (points)"),
                           (ax[1, 1], "final", "release at the end of the anneal (%)")):
        for load in LOADS:
            s = np.array(sorted({key[1] for key in outputs if key[0] == load}))
            o = [outputs[(load, v)] for v in s]
            y = [distance(x, measured) for x in o] if what == "rms" else [release(x)[1][-1] for x in o]
            a.plot(s / 1e6, y, marker=MARKER[load], color=COLOR[load], ms=5, lw=1.4, label=f"MMM {load}")
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
    fig.savefig(os.path.join(OUT_DIR, "calibration_nfir.png"), dpi=150)
    plt.close(fig)
    print("wrote mmm_outputs/calibration_nfir.txt and mmm_outputs/calibration_nfir.png")
    return 0


if __name__ == "__main__":
    sys.exit(main())
