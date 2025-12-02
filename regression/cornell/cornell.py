import os
from regression.core.common import run_sciantix, compare_outputs, make_gold
from regression.core.parser import SciantixOutput

def run_cornell(case_dir: str, mode_gold: int = 0):
    """
    Execute a single test Cornell.
    mode_gold:
      0 = run + compare
      1 = run + gold
      2 = compare only
      3 = gold only
    """

    output_path = os.path.join(case_dir, "output.txt")
    gold_path   = os.path.join(case_dir, "output_gold.txt")

    if mode_gold in (0, 1):
        run_sciantix(case_dir)

        out  = SciantixOutput(output_path)
        outg = SciantixOutput(gold_path) if os.path.exists(gold_path) else None

        if mode_gold == 1:
            make_gold(case_dir)
            return True

        return compare_outputs(case_dir)

    elif mode_gold == 2:
        return compare_outputs(case_dir)

    elif mode_gold == 3:
        make_gold(case_dir)
        return True
