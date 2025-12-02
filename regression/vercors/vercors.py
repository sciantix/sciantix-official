import os
from regression.core.common import run_sciantix, compare_outputs, make_gold

def run_vercors(case_dir: str, mode_gold: int = 0):
    if mode_gold in (0, 1):
        run_sciantix(case_dir)
        if mode_gold == 1:
            make_gold(case_dir)
            return True
        return compare_outputs(case_dir)

    if mode_gold == 2:
        return compare_outputs(case_dir)

    if mode_gold == 3:
        make_gold(case_dir)
        return True


def run_all_vercors(base_path: str, mode_gold: int = 0):
    results = []
    for name in sorted(os.listdir(base_path)):
        if name.startswith("test_Vercors"):
            case_dir = os.path.join(base_path, name)
            if os.path.isdir(case_dir):
                print(f"Running Vercors case: {name}")
                ok = run_vercors(case_dir, mode_gold)
                results.append((name, ok))
    return results
