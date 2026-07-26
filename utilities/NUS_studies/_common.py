"""
sciantix -- parametric studies for the generalized spectral diffusion (NUS) solver
author: Giovanni Zullo

Shared plumbing for the studies in this directory: build a case from the reference
Baker 1977 conditions, give it a radial source, run SCIANTIX, read the output back.

Nothing here is configurable on purpose. Each study hardcodes the parameters of the
figure it reproduces; this module only removes the copy-write-run-read boilerplate that
would otherwise be repeated, and drift, three times.
"""

import os
import shutil
import subprocess
import sys

import numpy as np

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
sys.path.append(ROOT)

from regression.core.parser import SciantixOutput
from regression.nus.sources import Source, write_sources

# The conditions the paper states for its case studies: Baker 1977 separate-effects
# experiments as set up by Zullo et al., 1273 K, 5500 h, constant fission rate, with
# Turnbull diffusivity, heterogeneous nucleation and re-solution, diffusional trapping
# and Ainscough grain growth. That is exactly this regression case.
BASE_CASE = os.path.join(ROOT, "regression", "baker", "test_Baker1977__1273K")
EXECUTABLE = os.path.join(ROOT, "build", "sciantix.x")
RUNS_ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "runs")

# Grain radius and fission rate of the base case. Ainscough growth is active but yields
# no measurable growth at 1273 K over 5500 h, so the radius -- and with it the volume
# average of a profile defined on r/a -- is constant for the whole irradiation.
GRAIN_RADIUS = 5.0e-6  # m
FISSION_RATE = 1.0e19  # fiss/m3s
DURATION = 5500.0  # h

# Settings file line indices, zero-based, in the post-merge layout.
I_DIFFUSION_SOLVER = 2
I_GRAIN_BOUNDARY_RESOLUTION = 28

SOLVER_NUS = 4


def admissible_slope_range(volume_average=FISSION_RATE, a=GRAIN_RADIUS):
    """
    Slopes of S(r) = A r + B that keep the source non-negative across the grain at a
    fixed volume average.

    With VA = (3/4) A a + B, requiring S(0) = B >= 0 and S(a) = A a + B >= 0 gives

        -4 VA / a  <=  A  <=  4 VA / (3 a)

    The range is asymmetric: a source rising towards the boundary is limited by the
    grain centre running dry, a falling one by the boundary running dry, and the two
    constraints do not bite equally because the volume weighting favours large radii.
    """
    return -4.0 * volume_average / a, 4.0 * volume_average / (3.0 * a)


def intercept_for(slope, volume_average=FISSION_RATE, a=GRAIN_RADIUS):
    """Intercept B that holds the volume average at the requested value."""
    return volume_average - 0.75 * slope * a


def linear_source(time, slope, volume_average=FISSION_RATE, a=GRAIN_RADIUS):
    """A single-region linear profile over the whole grain, at the given volume average."""
    return Source(time, [0.0, 1.0], [slope], [intercept_for(slope, volume_average, a)])


def shell_source(time, inner, outer, lam, a=GRAIN_RADIUS):
    """
    Two-region profile: a uniform fission source over the grain plus a resolution term
    confined to the outer shell of width lam, which is how the solver represents
    grain-boundary re-solution.
    """
    if lam <= 0.0:
        return Source(time, [0.0, 1.0], [0.0], [inner])
    start = 1.0 - lam / a
    return Source(time, [0.0, start, 1.0], [0.0, 0.0], [inner, outer])


# The NUS options sit past the end of a settings file written for the uniform solver,
# such as the Baker regression case. Reading stops at end of file and leaves them at
# zero, so appending them with their inactive values changes nothing on its own and
# makes the lines available to patch.
NUS_DEFAULT_OPTIONS = [
    "0\t#\tiNUSOutput (0= visualization disabled, 1= visualization enabled)",
    "0\t#\tiNUSAnimation (0= visualization disabled, 1= visualization enabled)",
    "0\t#\tiNonSym (0= sphere, 1= implantation geometry)",
    "0\t#\tiGrainBoundaryResolution (0= off, 1= on)",
]
FIRST_NUS_OPTION = 25


def _patch_settings(path, changes):
    """Rewrite selected option lines in place, keeping the descriptive comments."""
    lines = [line for line in open(path).read().split("\n") if line.strip()]

    highest = max(changes) if changes else -1
    while len(lines) <= highest:
        offset = len(lines) - FIRST_NUS_OPTION
        if not 0 <= offset < len(NUS_DEFAULT_OPTIONS):
            raise IndexError(
                f"{path} has {len(lines)} options and index {highest} is not a known NUS option"
            )
        lines.append(NUS_DEFAULT_OPTIONS[offset])

    for index, value in changes.items():
        comment = lines[index].split("#", 1)
        lines[index] = f"{value}\t#{comment[1]}" if len(comment) > 1 else str(value)
    open(path, "w").write("\n".join(lines) + "\n")


def build_case(study, name, sources, settings=None):
    """
    Materialise one variant: the base case, with the NUS solver selected and the given
    radial source. Returns the case directory.
    """
    case_dir = os.path.join(RUNS_ROOT, study, name)
    if os.path.isdir(case_dir):
        shutil.rmtree(case_dir)
    os.makedirs(case_dir)

    for filename in os.listdir(BASE_CASE):
        if filename.startswith("input_"):
            shutil.copy(os.path.join(BASE_CASE, filename), case_dir)

    changes = {I_DIFFUSION_SOLVER: SOLVER_NUS}
    changes.update(settings or {})
    _patch_settings(os.path.join(case_dir, "input_settings.txt"), changes)

    write_sources(os.path.join(case_dir, "non_uniform_source.txt"), sources)
    return case_dir


def run_case(case_dir):
    """Run SCIANTIX in a case directory and return its output, or None on failure."""
    if not os.path.isfile(EXECUTABLE):
        raise FileNotFoundError(
            f"{EXECUTABLE} not found -- build it first:\n"
            f"    cmake -S {ROOT} -B {ROOT}/build && cmake --build {ROOT}/build -j"
        )

    result = subprocess.run(
        [EXECUTABLE, case_dir + os.sep],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        print(f"  FAILED: {os.path.basename(case_dir)} (exit {result.returncode})")
        if result.stderr.strip():
            print("  " + result.stderr.strip().splitlines()[-1])
        return None

    output_path = os.path.join(case_dir, "output.txt")
    if not os.path.isfile(output_path):
        print(f"  FAILED: {os.path.basename(case_dir)} produced no output.txt")
        return None

    return SciantixOutput(output_path)


def build_and_run(study, name, sources, settings=None):
    """build_case followed by run_case, which is how every study uses them."""
    print(f"  running {study}/{name} ...")
    return run_case(build_case(study, name, sources, settings))


def release_onset(out, species="Xe"):
    """
    Irradiation time at which release first becomes non-zero, in hours. None when the
    gas never leaves the grain within the simulated history.
    """
    column = f"{species} released (at/m3)"
    if column not in out.colmap:
        return None
    released = out.get_all(column)
    time = out.get_all("Time (h)")
    nonzero = np.flatnonzero(released > 0.0)
    return float(time[nonzero[0]]) if nonzero.size else None


def figure_path(study, name):
    """Where a study writes its figure, next to the study scripts."""
    outdir = os.path.join(os.path.dirname(os.path.abspath(__file__)), "figures")
    os.makedirs(outdir, exist_ok=True)
    return os.path.join(outdir, f"{study}_{name}.png")


def report_volume_average(outputs, labels, species="Xe"):
    """
    Print the final produced inventory across variants. The studies constrain every
    profile to the same volume-averaged production, so these must agree; a spread here
    means the constraint was not actually imposed and the comparison is meaningless.
    """
    column = f"{species} produced (at/m3)"
    values = [out.get_last(column) for out in outputs if column in out.colmap]
    if not values:
        return

    print(f"\n  {species} produced at the end of irradiation (volume-average check):")
    for label, value in zip(labels, values):
        print(f"    {label:<24} {value:.6e}")

    spread = (max(values) - min(values)) / max(abs(max(values)), 1e-30)
    verdict = "consistent" if spread < 1e-6 else "INCONSISTENT"
    print(f"    relative spread {spread:.2e}  ({verdict})")
