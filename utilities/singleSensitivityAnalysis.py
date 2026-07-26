"""
Single-parameter sensitivity analysis for SCIANTIX scaling factors.

One scaling factor is perturbed at a time, sampled at random around unity, and the
response of a chosen output variable is measured against a reference run with every
factor at 1. The reported sensitivity coefficient is the normalised finite difference

    k = (1 / y_ref) * (y_ref - y) / (1 - sf)

so it answers "how strongly does this parameter move the result", not "which value fits
the data" — no experimental comparison is involved.

The companions:
  utilities/globalSensitivityAnalysis.py  perturbs every factor at once and ranks them
  regression/white/bias.py                sweeps a grid and scores it against experiment

    python3 utilities/singleSensitivityAnalysis.py

Runs happen on copies under `build/SSA/runs/`, never in the regression case itself.
Figures and the report land in `build/SSA/results/`.
"""

import os
import random
import shutil
import subprocess
import sys

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, ".."))
EXECUTABLE = os.path.join(ROOT, "build", "sciantix.x")

OUTPUT_ROOT = os.path.join(ROOT, "build", "SSA")
RUNS_ROOT = os.path.join(OUTPUT_ROOT, "runs")
RESULTS_ROOT = os.path.join(OUTPUT_ROOT, "results")

# --- what to analyse -------------------------------------------------------------
GROUP = "baker"
CASE = "test_Baker1977__1473K"
FACTOR = "resolution rate"
SCIANTIX_VARIABLE = "Intragranular gas bubble swelling (/)"
SAMPLINGS = 40
DEVIATION = 0.5  # sampled uniformly in [1 - DEVIATION, 1 + DEVIATION]
SEED = 20260726  # fixed so a rerun reproduces the same sampling

# input_scaling_factors.txt is optional — only 52 of the 113 cases ship one — so this is
# the fallback. The order matters and the comments do not: InputReading.C reads the
# values positionally and skips the comment line.
DEFAULT_SCALING_FACTORS = [
    "resolution rate",
    "trapping rate",
    "nucleation rate",
    "diffusivity",
    "temperature",
    "fission rate",
    "diffusion-based release",
    "helium production rate",
    "dummy",
]


def read_scaling_factors(path):
    """Parse input_scaling_factors.txt into an ordered {name: value} dict."""
    factors = {}
    with open(path) as f:
        lines = [line.rstrip("\n") for line in f if line.strip()]
    for i in range(0, len(lines) - 1, 2):
        name = lines[i + 1].strip()
        prefix = "# scaling factor - "
        if not name.startswith(prefix):
            continue
        factors[name[len(prefix) :]] = float(lines[i].strip())
    return factors


def write_scaling_factors(path, factors):
    with open(path, "w") as f:
        for name, value in factors.items():
            f.write(f"{value}\n")
            f.write(f"# scaling factor - {name}\n")


def column_index(header, variable_name):
    """Column of a variable in an output.txt header row, or None when absent."""
    matches = np.flatnonzero(header == variable_name)
    return int(matches[0]) if matches.size else None


class SingleSensitivityAnalysis:
    def __init__(self, group, case, factor, variable_name, samplings, deviation):
        self.case_dir = os.path.join(ROOT, "regression", group, case)
        self.case = case
        self.factor = factor
        self.variable_name = variable_name
        self.samplings = samplings
        self.deviation = deviation

        if not os.path.isdir(self.case_dir):
            raise FileNotFoundError(f"case not found: {self.case_dir}")

        shipped = os.path.join(self.case_dir, "input_scaling_factors.txt")
        if os.path.isfile(shipped):
            self.scaling_factors = read_scaling_factors(shipped)
        else:
            print(f"{case} ships no input_scaling_factors.txt, using the default list")
            self.scaling_factors = {name: 1.0 for name in DEFAULT_SCALING_FACTORS}

        if factor not in self.scaling_factors:
            raise KeyError(
                f"{factor!r} is not a scaling factor. Available: {list(self.scaling_factors)}"
            )

        self.sampled_factor = np.zeros(samplings)
        self.variable_value = np.zeros(samplings)
        self.sensitivity = np.full(samplings, np.nan)
        self.reference_value = np.nan

    def _prepare(self, name):
        """Copy the case into the scratch tree; the regression case is never written to."""
        target = os.path.join(RUNS_ROOT, name)
        if os.path.isdir(target):
            shutil.rmtree(target)
        os.makedirs(target)
        for filename in os.listdir(self.case_dir):
            if filename.startswith("input_"):
                shutil.copy(os.path.join(self.case_dir, filename), target)
        return target

    def _run(self, run_dir):
        result = subprocess.run([EXECUTABLE, run_dir + os.sep], capture_output=True, text=True)
        if result.returncode != 0:
            raise RuntimeError(f"SCIANTIX failed in {run_dir} (exit {result.returncode})")

        data = np.genfromtxt(os.path.join(run_dir, "output.txt"), dtype="str", delimiter="\t")
        index = column_index(data[0], self.variable_name)
        if index is None:
            raise KeyError(f"{self.variable_name!r} is not an output column of {run_dir}")
        return float(data[-1, index])

    def execute(self):
        print(f"\nCase:     {self.case}")
        print(f"Factor:   {self.factor} sampled in [{1 - self.deviation:g}, {1 + self.deviation:g}]")
        print(f"Variable: {self.variable_name}")
        print(f"Samples:  {self.samplings}   seed: {SEED}\n")

        random.seed(SEED)

        # Reference: every factor at unity.
        reference_dir = self._prepare("reference")
        factors = {name: 1.0 for name in self.scaling_factors}
        write_scaling_factors(os.path.join(reference_dir, "input_scaling_factors.txt"), factors)
        self.reference_value = self._run(reference_dir)
        print(f"  reference {self.variable_name} = {self.reference_value:.6e}")

        if self.reference_value == 0.0:
            raise ValueError(
                f"the reference value of {self.variable_name!r} is zero, so the normalised "
                f"sensitivity coefficient is undefined for this case"
            )

        for i in range(self.samplings):
            bias = random.uniform(1 - self.deviation, 1 + self.deviation)
            factors[self.factor] = bias

            run_dir = self._prepare(f"sample_{i + 1:03d}")
            write_scaling_factors(os.path.join(run_dir, "input_scaling_factors.txt"), factors)

            self.sampled_factor[i] = bias
            self.variable_value[i] = self._run(run_dir)

            # k = (1/y_ref) (y_ref - y) / (1 - sf); a sample landing exactly on unity has
            # no finite difference to take, so it is left out rather than dividing by zero.
            if bias != 1.0:
                self.sensitivity[i] = (
                    (self.reference_value - self.variable_value[i])
                    / (1.0 - bias)
                    / self.reference_value
                )

        spread = self.variable_value
        print(f"  spread over samples: [{spread.min():.6e}, {spread.max():.6e}]")

    def report(self):
        finite = self.sensitivity[np.isfinite(self.sensitivity)]
        mean = finite.mean() if finite.size else float("nan")

        print(f"\n  mean sensitivity coefficient: {mean:+.6f}")
        print(f"  spread of the coefficient:    [{finite.min():+.6f}, {finite.max():+.6f}]")

        os.makedirs(RESULTS_ROOT, exist_ok=True)
        path = os.path.join(RESULTS_ROOT, "report.txt")
        with open(path, "w") as f:
            f.write(f"Single-parameter sensitivity analysis — {self.case}\n")
            f.write(f"Factor:   {self.factor}\n")
            f.write(f"Variable: {self.variable_name}\n")
            f.write(f"Samples:  {self.samplings}   deviation: {self.deviation}   seed: {SEED}\n\n")
            f.write(f"reference value            {self.reference_value:.6e}\n")
            f.write(f"mean sensitivity           {mean:+.6f}\n")
            f.write(f"sensitivity range          [{finite.min():+.6f}, {finite.max():+.6f}]\n")
            f.write(f"output range               [{self.variable_value.min():.6e}, "
                    f"{self.variable_value.max():.6e}]\n\n")
            f.write(f"  {'scaling factor':>16}{'output':>16}{'sensitivity':>16}\n")
            for sf, value, k in zip(self.sampled_factor, self.variable_value, self.sensitivity):
                shown = f"{k:+.6f}" if np.isfinite(k) else "n/a"
                f.write(f"  {sf:>16.6f}{value:>16.6e}{shown:>16}\n")
        print(f"Report written to {path}")

    def plot(self):
        os.makedirs(RESULTS_ROOT, exist_ok=True)
        fig, axes = plt.subplots(1, 2, figsize=(10, 4.5))

        axes[0].scatter(self.sampled_factor, self.variable_value, c="#98E18D",
                        edgecolors="#999AA2", marker="o", s=25)
        axes[0].axhline(self.reference_value, color="black", ls="--", linewidth=1.0,
                        label="reference")
        axes[0].axvline(1.0, color="grey", ls=":", linewidth=1.0)
        axes[0].set_xlabel(self.factor)
        axes[0].set_ylabel(self.variable_name, fontsize=9)
        axes[0].legend(fontsize=8)
        axes[0].grid(True, ls=":")

        finite = np.isfinite(self.sensitivity)
        axes[1].scatter(self.sampled_factor[finite], self.sensitivity[finite], c="#98E18D",
                        edgecolors="#999AA2", marker="o", s=25)
        axes[1].axvline(1.0, color="grey", ls=":", linewidth=1.0)
        axes[1].set_xlabel(self.factor)
        axes[1].set_ylabel("Sensitivity coefficient")
        axes[1].grid(True, ls=":")

        fig.suptitle(f"{self.case} — {self.factor}", fontsize=11)
        fig.tight_layout(rect=(0, 0, 1, 0.96))

        outpath = os.path.join(RESULTS_ROOT, "sensitivity.png")
        fig.savefig(outpath, dpi=160)
        plt.close(fig)
        print("Saved:", outpath)


def main():
    if not os.path.isfile(EXECUTABLE):
        print(f"Error: {EXECUTABLE} not found. Build it first:")
        print(f"    cmake -S {ROOT} -B {ROOT}/build && cmake --build {ROOT}/build -j")
        return 1

    analysis = SingleSensitivityAnalysis(
        GROUP, CASE, FACTOR, SCIANTIX_VARIABLE, SAMPLINGS, DEVIATION
    )
    analysis.execute()
    analysis.report()
    analysis.plot()
    return 0


if __name__ == "__main__":
    sys.exit(main())
