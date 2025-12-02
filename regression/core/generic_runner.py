import os
import shutil
from regression.core.common import run_sciantix, load_output, load_gold
from regression.core.compare import compare_outputs


def run_group(group_name: str, prefix: str, mode_gold: int):
    """
    Generic runner for any regression group.

    Args:
        group_name: folder under regression/ (e.g. 'baker')
        prefix: test folder prefix (e.g. 'test_Baker')
        mode_gold:
            0 = run + compare
            1 = run + rewrite gold
            2 = compare only
            3 = rewrite gold only

    Returns:
        list of (test_name, ok)
    """

    base = os.path.join(os.path.dirname(__file__), "..", group_name)
    base = os.path.abspath(base)

    results = []

    if not os.path.isdir(base):
        print(f"[ERROR] Regression group '{group_name}' not found: {base}")
        return results

    for name in sorted(os.listdir(base)):
        if not name.startswith(prefix):
            continue

        case = os.path.join(base, name)
        if not os.path.isdir(case):
            continue

        print(f"Running {group_name} case: {name}")

        # run phase
        if mode_gold in (0, 1):
            run_sciantix(case)

        # gold rewrite mode
        if mode_gold in (1, 3):
            shutil.copy(os.path.join(case, "output.txt"),
                        os.path.join(case, "output_gold.txt"))
            results.append((f"{group_name}/{name}", True))
            continue

        # compare
        out = load_output(case)
        gold = load_gold(case)

        ok = compare_outputs(out, gold, abs_tol=1e-8, rel_tol=1e-6)
        results.append((f"{group_name}/{name}", ok))

    return results
