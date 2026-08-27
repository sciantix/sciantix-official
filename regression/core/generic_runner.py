"""
sciantix regression suite
author: Giovanni Zullo
"""

import os
import shutil
import multiprocessing
from regression.core.common import clean_case_dir, run_sciantix, load_output, load_gold
from regression.core.compare import compare_outputs


def run_single_case(args):
    """
    Worker function for parallel execution.
    Args:
        args: tuple (group_name, name, case_path, mode_gold)
    Returns:
        (test_id, ok, error_msg)
    """
    group_name, name, case, mode_gold = args
    test_id = f"{group_name}/{name}"

    try:
        # run phase
        if mode_gold in (0, 1):
            run_sciantix(case)

            ### NEO4MAT - SCIANTIX-DIVA
            # Kept outside the physics try/except: a metadata problem must never
            # be reported as a failed regression test.
            if group_name == "white":
                try:
                    from regression.white.semantic_export import (
                        export_white_case_semantic_outputs,
                    )
                    export_white_case_semantic_outputs(case)
                except Exception as semantic_error:
                    print(f"WARNING: semantic export failed for {test_id} -> {semantic_error}")
            ###

            clean_case_dir(case, 0)

        # gold rewrite mode
        if mode_gold in (1, 3):
            shutil.copy(os.path.join(case, "output.txt"),
                        os.path.join(case, "output_gold.txt"))
            return (test_id, True, None)

        # compare
        out = load_output(case)
        gold = load_gold(case)

        ok = compare_outputs(out, gold, abs_tol=1e-8, rel_tol=1e-6)
        if not ok:
            return (test_id, False, "Mismatch with gold standard")
        
        return (test_id, True, None)

    except Exception as e:
        return (test_id, False, str(e))


def run_group(group_name: str, prefix: str, mode_gold: int, jobs: int = 1):
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
        case = os.path.join(base, name)
        if not os.path.isdir(case):
            continue
        tasks.append((group_name, name, case, mode_gold))

    if not tasks:
        return []

    print(f"Running {len(tasks)} cases in {group_name} with {jobs} threads...")

    ### NEO4MAT - SCIANTIX-DIVA
    if group_name == "white":
        try:
            from regression.white.semantic_export import (
                export_white_experimental_measurements,
            )
            from regression.white.variable_metadata_export import export_variable_catalog

            export_white_experimental_measurements(base)
            export_variable_catalog(base)
        except Exception as semantic_error:
            print(f"WARNING: semantic catalog export failed -> {semantic_error}")
    ###

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
