"""
Global sensitivity analysis for SCIANTIX scaling factors.

All scaling factors are perturbed at once, sampled at random within a per-factor range,
and the resulting spread of a chosen output variable is ranked by Spearman (and Pearson)
correlation against each factor. That ranking is what makes this *global*: it says which
parameters matter when everything moves together.

The one-factor-at-a-time counterpart is `utilities/singleSensitivityAnalysis.py`, and
`regression/white/bias.py` is a deterministic grid sweep over a chosen pair.

    python3 utilities/globalSensitivityAnalysis.py

Runs are carried out on copies under `utilities/GSA_runs/`, never in the regression case
directories themselves — the analysis rewrites `input_scaling_factors.txt` on every
sample, and doing that in place would leave the validation database modified.

Results (.npy) are cached under `utilities/GSA_output_files/`; set RUN_SENSITIVITY to
False to re-plot from the cache without re-running SCIANTIX.
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
from scipy.stats import spearmanr

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, ".."))
EXECUTABLE = os.path.join(ROOT, "build", "sciantix.x")

RUNS_ROOT = os.path.join(HERE, "GSA_runs")
CACHE_ROOT = os.path.join(HERE, "GSA_output_files")

# --- what to analyse -------------------------------------------------------------
# GROUP is the regression group directory, CASE_PREFIX selects cases inside it.
GROUP = "baker"
CASE_PREFIX = "test_Baker1977__"
SCIANTIX_VARIABLE = "Intragranular gas bubble swelling (/)"
SAMPLINGS = 50
RUN_SENSITIVITY = True
SEED = 20260726  # fixed so a rerun reproduces the same sampling

# Per-factor biasing. The order is irrelevant now — factors are matched by name against
# input_scaling_factors.txt rather than by position, which is what the original required.
#   above  : [1, 1+d]        below : [1-d, 1]        around : [1-d, 1+d]
#   log10  : [1e-d, 1e+d]    scaled: [1/d, d]
BIASING = [
    {"name": "resolution rate", "deviation": 2, "feature": "scaled"},
    {"name": "trapping rate", "deviation": 5, "feature": "scaled"},
    {"name": "nucleation rate", "deviation": 0.8, "feature": "below"},
    {"name": "diffusivity", "deviation": 1, "feature": "log10"},
    {"name": "temperature", "deviation": 0, "feature": "around"},
    {"name": "fission rate", "deviation": 0, "feature": "around"},
]


# input_scaling_factors.txt is optional -- only 52 of the 113 cases ship one, and the
# Baker group does not -- so the tool falls back to this list. The order matters and the
# comments do not: InputReading.C reads the values positionally and skips the comment
# line, so the names below are for the reader.
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


def sample_bias(feature, deviation):
    if feature == "above":
        return random.uniform(1, 1 + deviation)
    if feature == "around":
        return random.uniform(1 - deviation, 1 + deviation)
    if feature == "below":
        return random.uniform(1 - deviation, 1)
    if feature == "log10":
        return random.uniform(10 ** (-deviation), 10 ** (deviation))
    if feature == "scaled":
        return random.uniform(1 / deviation, deviation)
    raise ValueError(f"Unsupported feature: {feature}")


def describe_bias(entry):
    feature, deviation = entry["feature"], entry["deviation"]
    ranges = {
        "above": (1, 1 + deviation),
        "around": (1 - deviation, 1 + deviation),
        "below": (1 - deviation, 1),
        "log10": (10 ** (-deviation), 10 ** (deviation)),
        "scaled": (1 / deviation if deviation else float("nan"), deviation),
    }
    low, high = ranges[feature]
    return f"{entry['name']:<18} {feature:<8} [{low:g}, {high:g}]"


class GlobalSensitivityAnalysis:
    def __init__(self, group, case_prefix, variable_name, biasing, samplings):
        self.group_dir = os.path.join(ROOT, "regression", group)
        self.variable_name = variable_name
        self.biasing = biasing
        self.samplings = samplings

        if not os.path.isdir(self.group_dir):
            raise FileNotFoundError(f"regression group not found: {self.group_dir}")

        self.case_names = sorted(
            name
            for name in os.listdir(self.group_dir)
            if name.startswith(case_prefix) and os.path.isdir(os.path.join(self.group_dir, name))
        )
        if not self.case_names:
            raise FileNotFoundError(f"no case matching {case_prefix!r} under {self.group_dir}")

        # Take the factor layout from whichever case ships one, else the canonical list.
        shipped = next(
            (
                path
                for path in (
                    os.path.join(self.group_dir, name, "input_scaling_factors.txt")
                    for name in self.case_names
                )
                if os.path.isfile(path)
            ),
            None,
        )
        if shipped:
            self.scaling_factors = read_scaling_factors(shipped)
        else:
            print(f"No input_scaling_factors.txt under {group}/, using the default factor list")
            self.scaling_factors = {name: 1.0 for name in DEFAULT_SCALING_FACTORS}

        unknown = [b["name"] for b in biasing if b["name"] not in self.scaling_factors]
        if unknown:
            raise KeyError(
                f"biasing refers to factors absent from input_scaling_factors.txt: {unknown}\n"
                f"available: {list(self.scaling_factors)}"
            )

        n_cases, n_factors = len(self.case_names), len(biasing)
        self.reference_value = np.zeros(n_cases)
        self.scaling_factor_map = np.zeros((n_cases, samplings, n_factors))
        self.variable_value_map = np.zeros((n_cases, samplings))
        self.pearson_corr = np.zeros((n_cases, n_factors))
        self.spearman_corr = np.zeros((n_cases, n_factors))

    # -- running ------------------------------------------------------------------
    def _prepare(self, case_name):
        """Copy a case into the scratch tree so the validation database is never touched."""
        source = os.path.join(self.group_dir, case_name)
        target = os.path.join(RUNS_ROOT, case_name)
        if os.path.isdir(target):
            shutil.rmtree(target)
        os.makedirs(target)
        for filename in os.listdir(source):
            if filename.startswith("input_"):
                shutil.copy(os.path.join(source, filename), target)
        return target

    def _run(self, case_dir):
        """Run SCIANTIX and return the final value of the variable of interest."""
        result = subprocess.run([EXECUTABLE, case_dir + os.sep], capture_output=True, text=True)
        if result.returncode != 0:
            raise RuntimeError(f"SCIANTIX failed in {case_dir} (exit {result.returncode})")

        data = np.genfromtxt(os.path.join(case_dir, "output.txt"), dtype="str", delimiter="\t")
        index = column_index(data[0], self.variable_name)
        if index is None:
            raise KeyError(f"{self.variable_name!r} is not an output column of {case_dir}")
        return float(data[-1, index])

    def uncertainty_analysis(self):
        print(f"\nSampling {self.samplings} times per case over {len(self.case_names)} cases")
        print(f"Variable: {self.variable_name}\n")
        for entry in self.biasing:
            print("  " + describe_bias(entry))

        random.seed(SEED)

        for i, case_name in enumerate(self.case_names):
            case_dir = self._prepare(case_name)
            print(f"\n[{i + 1}/{len(self.case_names)}] {case_name}")

            # Reference: every factor at unity.
            factors = dict(self.scaling_factors)
            for key in factors:
                factors[key] = 1.0
            write_scaling_factors(os.path.join(case_dir, "input_scaling_factors.txt"), factors)
            self.reference_value[i] = self._run(case_dir)
            print(f"  reference {self.variable_name} = {self.reference_value[i]:.6e}")

            for j in range(self.samplings):
                biases = [sample_bias(b["feature"], b["deviation"]) for b in self.biasing]
                for entry, bias in zip(self.biasing, biases):
                    factors[entry["name"]] = bias
                write_scaling_factors(os.path.join(case_dir, "input_scaling_factors.txt"), factors)

                self.scaling_factor_map[i, j, :] = biases
                self.variable_value_map[i, j] = self._run(case_dir)

            for k in range(len(self.biasing)):
                sampled = self.scaling_factor_map[i, :, k]
                observed = self.variable_value_map[i, :]
                # A factor held at a single value (deviation 0) has no variance, so a
                # correlation against it is undefined rather than zero.
                if np.ptp(sampled) == 0.0:
                    self.pearson_corr[i, k] = np.nan
                    self.spearman_corr[i, k] = np.nan
                else:
                    self.pearson_corr[i, k] = np.corrcoef(sampled, observed)[0, 1]
                    self.spearman_corr[i, k] = spearmanr(sampled, observed)[0]

            spread = self.variable_value_map[i, :]
            print(f"  spread over samples: [{spread.min():.6e}, {spread.max():.6e}]")

    # -- persistence --------------------------------------------------------------
    def _cache_path(self, name):
        return os.path.join(CACHE_ROOT, f"{os.path.basename(self.group_dir)}_{name}.npy")

    def save(self):
        os.makedirs(CACHE_ROOT, exist_ok=True)
        for name in (
            "reference_value",
            "scaling_factor_map",
            "variable_value_map",
            "pearson_corr",
            "spearman_corr",
        ):
            np.save(self._cache_path(name), getattr(self, name))
        print(f"\nCached to {CACHE_ROOT}")

    def load(self):
        for name in (
            "reference_value",
            "scaling_factor_map",
            "variable_value_map",
            "pearson_corr",
            "spearman_corr",
        ):
            setattr(self, name, np.load(self._cache_path(name)))
        print(f"\nLoaded from {CACHE_ROOT}")

    # -- reporting ----------------------------------------------------------------
    def ranking(self):
        """Mean |Spearman| across cases, which is the factor ranking this tool exists for."""
        with np.errstate(invalid="ignore"):
            score = np.nanmean(np.abs(self.spearman_corr), axis=0)
        order = np.argsort(-np.nan_to_num(score))

        print("\nFactor ranking (mean |Spearman| across cases):")
        for position, k in enumerate(order, start=1):
            name = self.biasing[k]["name"]
            value = score[k]
            note = "" if np.isfinite(value) else "   (held fixed, no variance)"
            shown = f"{value:.4f}" if np.isfinite(value) else "  n/a "
            print(f"  {position}. {name:<18} {shown}{note}")
        return score

    def write_report(self, score):
        path = os.path.join(HERE, "GSA_report.txt")
        with open(path, "w") as f:
            f.write(f"Global sensitivity analysis — {os.path.basename(self.group_dir)}\n")
            f.write(f"Variable: {self.variable_name}\n")
            f.write(f"Samples per case: {self.samplings}   seed: {SEED}\n\n")

            f.write("Factor ranking (mean |Spearman| across cases)\n")
            for k, entry in enumerate(self.biasing):
                f.write(f"  {entry['name']:<20}{score[k]:.6f}\n")

            f.write("\nPer-case spread of the output variable\n")
            f.write(f"  {'case':<28}{'reference':>14}{'min':>14}{'max':>14}\n")
            for i, case_name in enumerate(self.case_names):
                row = self.variable_value_map[i, :]
                f.write(
                    f"  {case_name:<28}{self.reference_value[i]:>14.6e}"
                    f"{row.min():>14.6e}{row.max():>14.6e}\n"
                )
        print(f"Report written to {path}")

    def plot(self):
        os.makedirs(CACHE_ROOT, exist_ok=True)
        labels = [name.replace(CASE_PREFIX, "") for name in self.case_names]

        for corr, title in ((self.spearman_corr, "Spearman"), (self.pearson_corr, "Pearson")):
            fig, ax = plt.subplots(figsize=(7, 4.5))
            for k, entry in enumerate(self.biasing):
                if np.all(np.isnan(corr[:, k])):
                    continue  # factor held fixed: nothing to draw
                ax.plot(labels, corr[:, k], marker="o", linestyle="-", label=entry["name"])
            ax.axhline(0.0, color="grey", ls=":", linewidth=1.0)
            ax.set_ylabel(f"{title} (/)")
            ax.set_ylim(-1.05, 1.05)
            ax.grid(True, ls=":")
            ax.legend(fontsize=8)
            fig.autofmt_xdate(rotation=45)
            fig.tight_layout()
            outpath = os.path.join(CACHE_ROOT, f"{title.lower()}.png")
            fig.savefig(outpath, dpi=160)
            plt.close(fig)
            print("Saved:", outpath)

        # Spread of the output, case by case. Drawn as an absolute [min, max] band rather
        # than as an error bar around the reference: with every factor perturbed at once
        # the reference does not have to sit inside the sampled range, and a signed
        # deviation from it would go negative.
        fig, ax = plt.subplots(figsize=(7, 4.5))
        positions = np.arange(len(labels))
        low = self.variable_value_map.min(axis=1)
        high = self.variable_value_map.max(axis=1)
        ax.vlines(positions, low, high, color="crimson", linewidth=3, alpha=0.5, label="sampled range")
        ax.plot(positions, self.reference_value, "o", color="black", label="reference (all factors = 1)")
        ax.set_xticks(positions)
        ax.set_xticklabels(labels)
        ax.legend(fontsize=8)
        ax.set_ylabel(self.variable_name)
        ax.grid(True, ls=":")
        fig.autofmt_xdate(rotation=45)
        fig.tight_layout()
        outpath = os.path.join(CACHE_ROOT, "spread.png")
        fig.savefig(outpath, dpi=160)
        plt.close(fig)
        print("Saved:", outpath)


def main():
    if RUN_SENSITIVITY and not os.path.isfile(EXECUTABLE):
        print(f"Error: {EXECUTABLE} not found. Build it first:")
        print(f"    cmake -S {ROOT} -B {ROOT}/build && cmake --build {ROOT}/build -j")
        return 1

    analysis = GlobalSensitivityAnalysis(
        GROUP, CASE_PREFIX, SCIANTIX_VARIABLE, BIASING, SAMPLINGS
    )

    if RUN_SENSITIVITY:
        analysis.uncertainty_analysis()
        analysis.save()
    else:
        analysis.load()

    score = analysis.ranking()
    analysis.write_report(score)
    analysis.plot()
    return 0


if __name__ == "__main__":
    sys.exit(main())
