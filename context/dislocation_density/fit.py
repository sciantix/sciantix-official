"""Reproduce the public Fig.5 fit, model curves, and C++ parity check.

python3 context/dislocation_density/fit.py [--cpp-grid build/coarsening_tests.x]
#COARSENING: check the frozen correlation; no simulation is calibrated here.
Requires numpy, scipy, matplotlib. No external workspace files are required.
The density correlation was refitted to White 2006 data that cannot be shown.
Those observations and their refit diagnostics are not distributed here.
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
    for ext in ("png",):
        fig.savefig(ROOT / "figures" / f"{name}.{ext}", dpi=300, bbox_inches="tight")
    plt.close(fig)


def model_figure(fn):
    """Curve-only two-panel layout; no restricted observations or fit ranges."""
    plt.rcParams.update(
        {
            "font.size": 10,
            "axes.labelsize": 10,
            "axes.titlesize": 10,
            "xtick.labelsize": 9,
            "ytick.labelsize": 9,
            "legend.fontsize": 9,
            "text.color": "black",
            "axes.labelcolor": "black",
            "xtick.color": "black",
            "ytick.color": "black",
        }
    )
    fig, axes = plt.subplots(1, 2, figsize=(7.1, 3.7), layout="constrained")
    colors = ("#2468a2", "#d88b24", "#bb403b", "#4d8b60")
    styles = ("-", "--", "-.", ":")
    burnup = np.linspace(0.1, 120, 600)
    temperature = np.linspace(600, 2600, 650)
    for T, color, style in zip((900, 1100, 1200, 1400), colors, styles):
        axes[0].plot(
            burnup, fn(burnup, T), color=color, ls=style, lw=1.5, label=f"{T} K"
        )
    for B, color, style in zip((5, 20, 50, 90), colors, styles):
        axes[1].plot(
            temperature,
            fn(B, temperature) / 1e15,
            color=color,
            ls=style,
            lw=1.5,
            label=rf"$B={B}$ GWd tHM$^{{-1}}$",
        )
    axes[0].set(
        xlim=(0, 120),
        ylim=(1e12, 3e15),
        yscale="log",
        xlabel=r"Burnup (GWd tHM$^{-1}$)",
        ylabel=r"$\rho_d$ (m$^{-2}$)",
    )
    axes[1].set(
        xlim=(600, 2600),
        ylim=(0, None),
        xlabel="Temperature (K)",
        ylabel=r"$\rho_d$ ($10^{15}$ m$^{-2}$)",
    )
    for ax, title in zip(axes, ("a)", "b)")):
        ax.set_title(title, loc="left", pad=10)
        ax.grid(which="both", alpha=0.16)
        ax.legend(
            loc="upper center", bbox_to_anchor=(0.5, -0.22), ncol=2, frameon=False
        )
    return fig, axes


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
    report = dict(
        fig5_points=len(grid),
        fig5_refitted_parameters=dict(zip(P, fit.x.tolist())),
        max_relative_curve_difference_from_selected=dict(fig5=fit_curve_error),
        fig5_RMSE_log10=float(np.sqrt(np.mean(fit.fun**2))),
        floor20_m2=1e13,
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
    fig, axes = model_figure(veshchunov)
    low, high = float(T.min()), float(T.max())
    axes[1].axvspan(low, high, color="#86b69b", alpha=0.2, zorder=-1)
    axes[1].text(
        (low + high) / 2,
        0.97,
        "Fit range",
        transform=axes[1].get_xaxis_transform(),
        ha="center",
        va="top",
        fontsize=9,
    )
    save(fig, "fig5_fits")
    inputs = [
        Path(__file__),
        ROOT / "selected_parameters.json",
        ROOT / "data/veshchunov_fig5_fit_samples.csv",
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
