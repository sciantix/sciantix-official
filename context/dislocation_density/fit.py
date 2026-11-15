"""Reproduce the selected Fig.5 and White fits, figures, and C++ parity check.

python3 context/dislocation_density/fit.py [--cpp-grid build/coarsening_tests.x]
#COARSENING: reproduce the frozen density fit; no simulation is calibrated here.
Requires numpy, scipy, matplotlib. No external workspace files are required.
"""

from pathlib import Path
import argparse
import csv
import hashlib
import io
import json
import os
import subprocess
import numpy as np
from scipy.special import expit
from scipy.optimize import least_squares
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

ROOT = Path(__file__).resolve().parent
CONFIG = json.loads((ROOT / "selected_parameters.json").read_text())
P = CONFIG["shared_burnup_parameters"]
PARAMETERS = np.array(list(P.values()))
N = CONFIG["nicodemo_2026"]


def exponent(T, p=PARAMETERS):
    return p[1] + (p[2] - p[1]) * expit((np.asarray(T) - p[4]) / p[5])


def veshchunov(B, T, p=PARAMETERS):
    s = expit((np.asarray(T) - p[4]) / p[5])
    return 10 ** p[0] * (np.asarray(B) / 20) ** exponent(T, p) * (1 - (1 - p[3]) * s)


def nicodemo(B, T, fit=None):
    if fit is None:
        fit = [N["log_tau_K"], N["power"]]
    T = np.asarray(T)
    dt = np.maximum(T - 900.0, 0.0)
    u = np.hypot(dt, 20.0) - 20.0
    floor100 = 1e13 * 5 ** P["n_hot"]
    R = floor100 + (veshchunov(100, 900) - floor100) * np.exp(
        -((u / np.exp(fit[0])) ** fit[1])
    )
    R = np.where(T < 900, veshchunov(100, T), R)
    return (np.asarray(B) / 100) ** exponent(T) * R


def density(B, T, model):
    if model == 0:
        return np.full(np.broadcast(np.asarray(B), np.asarray(T)).shape, 4e13)
    if model == 1:
        return nicodemo(B, T)
    raise ValueError(model)


def save(fig, name):
    for ext in ("png", "svg"):
        fig.savefig(ROOT / "figures" / f"{name}.{ext}", dpi=300, bbox_inches="tight")
    plt.close(fig)


def main():
    (ROOT / "figures").mkdir(exist_ok=True)
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--cpp-grid", type=Path)
    args = parser.parse_args()
    grid = np.genfromtxt(
        ROOT / "data/veshchunov_fig5_fit_samples.csv", delimiter=",", names=True
    )
    B, T, y = grid["burnup_GWd_tU"], grid["temperature_K"], grid["log10_rho"]
    fit = least_squares(
        lambda p: np.log10(veshchunov(B, T, p)) - y,
        [14.5, 0.65, 1.3, 0.03, 1250, 180],
        bounds=([11, 0.1, 0.1, 1e-5, 700, 5], [17, 2.5, 2.5, 0.99, 2000, 600]),
        max_nfev=5000,
    )
    # Optimizer/platform roundoff may slightly move coefficients; compare curves.
    fit_curve_error = float(np.max(abs(veshchunov(B, T, fit.x) / veshchunov(B, T) - 1)))
    assert fit_curve_error < 1e-5
    white = list(csv.DictReader((ROOT / "data/white_fig36_redigitized.csv").open()))
    b = np.array([float(r["burnup_nominal_GWd_tU"]) for r in white])
    t = np.array([float(r["T_peak_or_base_K"]) for r in white])
    observed = np.array([float(r["rho_m2"]) for r in white])
    rods = np.array([r["rod"] for r in white])
    weights = np.array([1 / sum(rods == r) for r in rods])
    weights /= weights.sum()
    trials = [
        least_squares(
            lambda p: np.sqrt(weights) * np.log10(nicodemo(b, t, p) / observed),
            [np.log(tau), power],
            bounds=([np.log(0.1), 0.55], [np.log(2000), 3]),
            max_nfev=4000,
        )
        for tau, power in [(40, 0.7), (80, 1), (200, 0.7), (400, 1.4)]
    ]
    wf = min(trials, key=lambda f: sum(f.fun**2))
    white_curve_error = float(np.max(abs(nicodemo(b, t, wf.x) / nicodemo(b, t) - 1)))
    assert white_curve_error < 1e-5
    report = dict(
        fig5_points=len(grid),
        white_points=len(white),
        white_rods=len(set(rods)),
        fig5_refitted_parameters=dict(zip(P, fit.x.tolist())),
        white_refitted_parameters=wf.x.tolist(),
        max_relative_curve_difference_from_selected=dict(
            fig5=fit_curve_error, white=white_curve_error
        ),
        fig5_RMSE_log10=float(np.sqrt(np.mean(fit.fun**2))),
        white_rod_balanced_RMSE_log10=float(np.sqrt(sum(wf.fun**2))),
        floor20_m2=1e13,
        power_at_lower_fit_bound=bool(wf.x[1] < 0.55001),
    )
    if args.cpp_grid:
        env = {k: v for k, v in os.environ.items() if not k.startswith("SCIANTIX_")}
        output = subprocess.check_output(
            [str(args.cpp_grid.resolve()), "density-grid"], text=True, env=env
        )
        rows = list(csv.DictReader(io.StringIO(output)))
        errors = [
            abs(
                float(r["rho"])
                / float(density(float(r["B"]), float(r["T"]), int(r["model"])))
                - 1
            )
            for r in rows
            if float(r["rho"]) > 0
        ]
        report["cpp_python_max_relative_error"] = max(errors)
        assert max(errors) < 1e-13
    (ROOT / "refit_verification.json").write_text(json.dumps(report, indent=2) + "\n")
    plt.rcParams.update({"font.size": 11, "svg.fonttype": "none"})
    temps = np.arange(900, 1501, 100)
    colors = plt.colormaps["viridis"](np.linspace(0.05, 0.95, len(temps)))
    bb = np.linspace(1, 100, 300)
    fig, axes = plt.subplots(1, 2, figsize=(12, 5), sharey=True, layout="constrained")
    for temp, color in zip(temps, colors):
        mask = T == temp
        axes[0].scatter(B[mask], 10 ** y[mask], s=12, color=color, alpha=0.6)
        for ax, fn in zip(axes, [veshchunov, nicodemo]):
            ax.plot(bb, fn(bb, temp), color=color, label=f"{temp} K")
    for ax, title in zip(
        axes, ["Veshchunov 2009: Fig.5 refit", "Nicodemo 2026: adapted from 900 K"]
    ):
        ax.axhline(4e13, color="#b55c25", ls="--", label="Barani 2020: fixed")
        ax.set(
            yscale="log",
            xlabel="Burnup (GWd/tU)",
            title=title,
            ylim=(1e12, 2e15),
            xlim=(0, 100),
        )
        ax.grid(alpha=0.25, which="both")
    axes[0].set_ylabel(r"Dislocation density (m$^{-2}$)")
    axes[1].legend(fontsize=8, ncol=2)
    save(fig, "fig5_fits")
    fig, axes = plt.subplots(1, 2, figsize=(12, 4.8), layout="constrained")
    tt = np.linspace(900, 2700, 500)
    for rod in sorted(set(rods)):
        mask = rods == rod
        axes[0].scatter(
            t[mask],
            observed[mask] * (20 / b[mask]) ** exponent(t[mask]),
            label=rod,
            marker=(
                "o" if white[np.flatnonzero(mask)[0]]["condition"] == "control" else "^"
            ),
        )
    for name, fn, style in [
        ("Veshchunov 2009", veshchunov, "--"),
        ("Nicodemo 2026", nicodemo, "-"),
    ]:
        axes[0].plot(tt, fn(20, tt), style, label=name)
        axes[1].scatter(observed, fn(b, t), s=24, label=name)
    axes[0].axhline(4e13, color="#b55c25", ls=":", label="Barani 2020")
    axes[0].axhline(1e13, color=".5", lw=0.8, label="Imposed asymptote at B20")
    axes[0].set(
        yscale="log",
        xlabel="Base / peak temperature (K)",
        ylabel=r"$\rho$ normalized to 20 GWd/tU (m$^{-2}$)",
    )
    axes[0].legend(fontsize=8, ncol=2)
    xx = np.geomspace(5e12, 1e15)
    axes[1].plot(xx, xx, ":", color=".4")
    axes[1].set(
        xscale="log",
        yscale="log",
        xlabel=r"White observed $\rho$ (m$^{-2}$)",
        ylabel=r"Calculated $\rho$ (m$^{-2}$)",
    )
    axes[1].legend(fontsize=9)
    for ax in axes:
        ax.grid(alpha=0.2, which="both")
    save(fig, "white_temperature_fit")
    inputs = [
        Path(__file__),
        ROOT / "selected_parameters.json",
        *sorted((ROOT / "data").glob("*.csv")),
    ]
    (ROOT / "source_manifest.json").write_text(
        json.dumps(
            {
                str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest()
                for p in inputs
            },
            indent=2,
        )
        + "\n"
    )
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
