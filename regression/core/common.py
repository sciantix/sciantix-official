import os
import subprocess
from regression.core.parser import SciantixOutput


def run_sciantix(case_dir: str):
    """
    Execute sciantix.x in the specified regression test directory.

    Returns:
        Path to the generated output.txt file.
    """

    exe = os.path.abspath(
        os.path.join(os.path.dirname(__file__), "..", "sciantix.x")
    )

    cmd = [exe, case_dir + "/"]

    print("Running:", " ".join(cmd))
    subprocess.run(cmd, check=True)

    return os.path.join(case_dir, "output.txt")


def load_output(case_dir):
    """Load case_dir/output.txt as a SciantixOutput object."""
    return SciantixOutput(os.path.join(case_dir, "output.txt"))


def load_gold(case_dir):
    """Load case_dir/gold.txt as a SciantixOutput object."""
    return SciantixOutput(os.path.join(case_dir, "output_gold.txt"))
