"""
sciantix testing suite
author: Giovanni Zullo, Elisa Cappellari
"""

import os
import shutil
import multiprocessing
from testing.core.common import clean_case_dir, run_sciantix, load_named, gold_name_for
from testing.core.compare import compare_outputs


def run_single_case(args):
    """
    Worker function for parallel execution.
    Args:
        args: tuple (cli_name, name, case, mode_gold, extra_outputs, ignore_columns, suite)
            extra_outputs: e.g. ("thermochemistry_output.txt",) for OC-coupled groups.
            ignore_columns: substrings.
            suite: "verification" or "validation", carried through to the result
                for report display.
    Returns:
        (test_id, ok, msg, suite) -- ok is True/False/None (None = skipped)
    """
    cli_name, name, case, mode_gold, extra_outputs, ignore_columns, suite = args
    test_id = f"{cli_name}/{name}"
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
            return (test_id, True, None, suite)

        # compare
        mismatched = []
        for filename in output_files:
            out = load_named(case, filename)
            gold = load_named(case, gold_name_for(filename))
            if not compare_outputs(out, gold, abs_tol=1e-8, rel_tol=1e-6,
                                    ignore_columns=ignore_columns):
                mismatched.append(filename)

        if mismatched:
            return (test_id, False, f"Mismatch with gold standard: {', '.join(mismatched)}", suite)

        note = (f"OC unavailable: excluded column(s) matching {list(ignore_columns)}"
                if ignore_columns else None)
        return (test_id, True, note, suite)

    except Exception as e:
        return (test_id, False, str(e), suite)


def discover_cases(base, prefix):
    """
    Find every case directory under `base` (any directory directly containing
    input_settings.txt), at any depth.

    Returns a sorted list of (relative_name, absolute_path).
    """
    found = []
    for dirpath, dirnames, filenames in os.walk(base):
        dirnames.sort()
        if "input_settings.txt" in filenames:
            rel = os.path.relpath(dirpath, base).replace(os.sep, "/")
            if rel == ".":
                rel = os.path.basename(os.path.normpath(base))
            if rel.startswith(prefix):
                found.append((rel, dirpath))
            dirnames[:] = []  # a case directory's subtree is never itself a case

    found.sort(key=lambda item: item[0])
    return found


def run_group(cli_name: str, base_dir: str, prefix: str, mode_gold: int, jobs: int = 1,
              only=None, extra_outputs=(), ignore_columns=(), skip_reason=None,
              skip_is_failure=False, suite=None):
    """
    Generic runner for any test group.

    Args:
        cli_name: folder under verification/ or validation/ (e.g. 'baker')
        base_dir: absolute path to the group's directory
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
        ignore_columns: substrings of column names to exclude from every comparison
        skip_reason: if given, no case is run at all 
        skip_is_failure: when skip_reason is set, report ok=False instead of
              ok=None (used for --oc strict mode and gold-rewrite refusal)
        suite: "verification" or "validation"

    Returns:
        list of (test_name, ok, msg, suite) -- ok is True/False/None
    """

    if not os.path.isdir(base_dir):
        print(f"[ERROR] Test group '{cli_name}' not found: {base_dir}")
        return []

    cases = discover_cases(base_dir, prefix)
    if only:
        cases = [(name, path) for name, path in cases
                 if name in only or any(o in name for o in only)]

    if not cases:
        if only:
            print(f"[WARN] No cases in '{cli_name}' matched {sorted(only)}")
        return []

    if skip_reason:
        status = False if skip_is_failure else None
        return [(f"{cli_name}/{name}", status, skip_reason, suite) for name, _ in cases]

    tasks = [(cli_name, name, path, mode_gold, extra_outputs, ignore_columns, suite)
             for name, path in cases]

    print(f"Running {len(tasks)} cases in {cli_name} with {jobs} threads...")

    results = []

    if jobs > 1:
        with multiprocessing.Pool(processes=jobs) as pool:
            for res in pool.map(run_single_case, tasks):
                results.append(res)
                if res[1] is False:
                    print(f"FAILED: {res[0]} -> {res[2]}")
    else:
        for task in tasks:
            res = run_single_case(task)
            results.append(res)
            if res[1] is False:
                print(f"FAILED: {res[0]} -> {res[2]}")

    return results
