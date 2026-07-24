"""
sciantix testing suite
author: Giovanni Zullo, Elisa Cappellari
"""


import os
import subprocess
import glob
from testing.core.parser import SciantixOutput


def clean_case_dir(case_dir: str, mode_gold: int):
    """
    Remove old run artifacts inside a test directory.

    Always removed:
        - execution.txt
        - overview.txt
        - input_check.txt
        - *.log
        - *.tmp

    Removed only when appropriate:
        - *_gold.txt   (any gold reference, e.g. output_gold.txt or
                         thermochemistry_output_gold.txt -- removed only in
                         gold-writing modes 1 or 3)
    """

    patterns_always = [
        "execution.txt",
        "overview.txt",
        "input_check.txt",
        "*.log",
        "*.tmp",
    ]

    patterns_gold = ["*_gold.txt"] if mode_gold in (1, 3) else []

    for pattern in patterns_always + patterns_gold:
        for path in glob.glob(os.path.join(case_dir, pattern)):
            try:
                os.remove(path)
            except FileNotFoundError:
                pass

def run_sciantix(case_dir: str):
    """
    Execute sciantix.x from the build directory.

    Returns:
        path to output.txt
    """
    # sciantix.x built inside build/
    # __file__ = testing/core/common.py
    # .. = testing
    # .. = root
    # root/build/sciantix.x
    exe = os.path.abspath(
        os.path.join(os.path.dirname(__file__), "..", "..", "build", "sciantix.x")
    )

    if not os.path.isfile(exe):
        raise FileNotFoundError(f"sciantix.x not found: {exe}")

    cmd = [exe, case_dir + "/"]

    print("Running:", " ".join(cmd))
    subprocess.run(cmd, check=True)

    return os.path.join(case_dir, "output.txt")


def gold_name_for(filename: str) -> str:
    """output.txt -> output_gold.txt; thermochemistry_output.txt -> thermochemistry_output_gold.txt"""
    stem, ext = os.path.splitext(filename)
    return f"{stem}_gold{ext}"


def load_named(case_dir, filename):
    """Load case_dir/<filename> as a SciantixOutput object."""
    return SciantixOutput(os.path.join(case_dir, filename))


def load_output(case_dir, filename="output.txt"):
    """Load case_dir/<filename> (default output.txt) as a SciantixOutput object."""
    return load_named(case_dir, filename)


def load_gold(case_dir, filename="output.txt"):
    """Load case_dir/<gold name for filename> (default output_gold.txt) as a SciantixOutput object."""
    return load_named(case_dir, gold_name_for(filename))
