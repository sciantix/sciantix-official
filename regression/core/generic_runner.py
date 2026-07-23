"""
sciantix regression suite
author: Giovanni Zullo
"""

import os
import shutil
import multiprocessing
from regression.core.common import clean_case_dir, run_sciantix, load_named, gold_name_for
from regression.core.compare import compare_outputs


def run_single_case(args):
    """
    Worker function for parallel execution.
    Args:
        args: tuple (group_name, name, case_path, mode_gold, extra_outputs)
            extra_outputs: output filenames beyond "output.txt" to gold-compare
                too, e.g. ("thermochemistry_output.txt",) for OC-coupled groups.
    Returns:
        (test_id, ok, error_msg)
    """
    group_name, name, case, mode_gold, extra_outputs = args
    test_id = f"{group_name}/{name}"
    output_files = ["output.txt", *extra_outputs]

    try:
        # run phase
        if mode_gold in (0, 1):
            run_sciantix(case)
            clean_case_dir(case, 0)

        # gold rewrite mode
        if mode_gold in (1, 3):
            for filename in output_files:
                shutil.copy(os.path.join(case, filename),
                            os.path.join(case, gold_name_for(filename)))
            return (test_id, True, None)

        # compare
        mismatched = []
        for filename in output_files:
            out = load_named(case, filename)
            gold = load_named(case, gold_name_for(filename))
            if not compare_outputs(out, gold, abs_tol=1e-8, rel_tol=1e-6):
                mismatched.append(filename)

        if mismatched:
            return (test_id, False, f"Mismatch with gold standard: {', '.join(mismatched)}")

        return (test_id, True, None)

    except Exception as e:
        return (test_id, False, str(e))


def run_group(group_name: str, prefix: str, mode_gold: int, jobs: int = 1, only=None, extra_outputs=()):
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
        jobs: number of parallel threads
        only: optional iterable of case names/substrings; when given, only
              matching test folders in the group are run
        extra_outputs: output filenames beyond "output.txt" to gold-compare
              too (see run_single_case)

    Returns:
        list of (test_name, ok)
    """

    base = os.path.join(os.path.dirname(__file__), "..", group_name)
    base = os.path.abspath(base)

    if not os.path.isdir(base):
        print(f"[ERROR] Regression group '{group_name}' not found: {base}")
        return []

    # Collect tasks
    tasks = []
    for name in sorted(os.listdir(base)):
        if not name.startswith(prefix):
            continue
        if only and name not in only and not any(o in name for o in only):
            continue
        case = os.path.join(base, name)
        if not os.path.isdir(case):
            continue
        tasks.append((group_name, name, case, mode_gold, extra_outputs))

    if not tasks:
        if only:
            print(f"[WARN] No cases in '{group_name}' matched {sorted(only)}")
        return []

    print(f"Running {len(tasks)} cases in {group_name} with {jobs} threads...")

    results = []
    
    if jobs > 1:
        with multiprocessing.Pool(processes=jobs) as pool:
            # map returns list of results in order
            for res in pool.map(run_single_case, tasks):
                # res is (test_id, ok, msg)
                results.append(res)
                if not res[1]:
                    print(f"FAILED: {res[0]} -> {res[2]}")
    else:
        for task in tasks:
            res = run_single_case(task)
            results.append(res)
            if not res[1]:
                print(f"FAILED: {res[0]} -> {res[2]}")

    return results
