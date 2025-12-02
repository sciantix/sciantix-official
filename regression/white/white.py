import os
from regression.core.common import run_sciantix, compare_outputs, make_gold
from regression.core.parser import SciantixOutput

def run_white(case_dir: str, mode_gold: int = 0):
    output_path = os.path.join(case_dir, "output.txt")
    gold_path   = os.path.join(case_dir, "output_gold.txt")

    if mode_gold in (0, 1):
        run_sciantix(case_dir)

        if mode_gold == 1:
            make_gold(case_dir)
            return True

        return compare_outputs(case_dir)

    elif mode_gold == 2:
        return compare_outputs(case_dir)

    elif mode_gold == 3:
        make_gold(case_dir)
        return True
