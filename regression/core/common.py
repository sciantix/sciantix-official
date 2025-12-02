import os
import shutil
import numpy as np

import os
import subprocess

def run_sciantix(case_dir: str):
    """
    ./build/sciantix.x <case_dir>/
    """

    project_root = os.path.abspath(
        os.path.join(os.path.dirname(__file__), "..", "..")
    )
    
    exe = os.path.join(project_root, "build", "sciantix.x")

    if not os.path.exists(exe):
        raise FileNotFoundError(f"sciantix.x not found: {exe}")

    if not os.path.isdir(case_dir):
        raise FileNotFoundError(f"Case not found: {case_dir}")

    cmd = [exe, case_dir + "/"]
    print("Running:", " ".join(cmd))

    subprocess.run(cmd, check=True)


def make_gold(case_dir: str):
    """Replace output_gold.txt with output.txt."""
    src = os.path.join(case_dir, "output.txt")
    dst = os.path.join(case_dir, "output_gold.txt")
    if os.path.exists(src):
        if os.path.exists(dst):
            os.remove(dst)
        os.rename(src, dst)


def compare_outputs(case_dir: str) -> bool:
    """Comparison byte-per-byte output.txt - output_gold.txt."""
    out = os.path.join(case_dir, "output.txt")
    gold = os.path.join(case_dir, "output_gold.txt")

    if not os.path.exists(out) or not os.path.exists(gold):
        return False

    with open(out, "rb") as f1, open(gold, "rb") as f2:
        return f1.read() == f2.read()
