"""
sciantix regression suite
author: Giovanni Zullo

regression.runner

python3 -m regression.runner
python3 -m regression.runner --white
python3 -m regression.runner --white --mode-gold 0
python3 -m regression.runner --white --mode-gold 1
"""

import sys
import os
import argparse
from regression.core.generic_runner import run_group
from regression.core.report import generate_html_report


def main():
    parser = argparse.ArgumentParser(description="SCIANTIX regression test runner")

    parser.add_argument("--baker", action="store_true")
    parser.add_argument("--cornell", action="store_true")
    parser.add_argument("--white", action="store_true")
    parser.add_argument("--kashibe", action="store_true")
    parser.add_argument("--talip", action="store_true")
    parser.add_argument("--oxidation", action="store_true")
    parser.add_argument("--chromium", action="store_true")
    parser.add_argument("--contact", action="store_true")
    parser.add_argument("--hbs", action="store_true")
    parser.add_argument("--vercors", action="store_true")
    parser.add_argument("--pulse", action="store_true")
    parser.add_argument("--analytics", action="store_true")
    parser.add_argument("--gpr", action="store_true")
    parser.add_argument("--all", action="store_true")

    parser.add_argument(
        "--mode-gold",
        type=int,
        default=0,
        help="0=run+compare, 1=run+gold, 2=compare, 3=gold only"
    )

    parser.add_argument(
        "--jobs", "-j",
        type=int,
        default=1,
        help="Number of parallel threads (default=1)"
    )

    args, extras = parser.parse_known_args()

    targeted = {}  # group -> set of case names/substrings
    for tok in extras:
        if tok.startswith("--") and "." in tok:
            group, _, case = tok[2:].partition(".")
            if group and case:
                targeted.setdefault(group, set()).add(case)
        else:
            print(f"[WARN] Ignoring unrecognized argument: {tok}")

    # Dynamic discovery of regression groups
    # We look for folders in regression/ that contain test cases (folders starting with test_)
    regression_root = os.path.join(os.path.dirname(__file__), "..", "regression")
    available_groups = []
    
    if os.path.isdir(regression_root):
        for entry in os.scandir(regression_root):
            if entry.is_dir() and entry.name not in ("core", "__pycache__"):
                # Check if it has at least one test_ folder
                has_tests = False
                for sub in os.scandir(entry.path):
                    if sub.is_dir() and sub.name.startswith("test_"):
                        has_tests = True
                        break
                if has_tests:
                    available_groups.append(entry.name)

    results = []

    # Selected groups map. --pulse is an alias for --analytics and has no folder of its
    # own, so it has to be checked explicitly: otherwise selecting it leaves
    # explicit_selection False and the whole suite is run instead of the chosen group.
    explicit_selection = any([getattr(args, g, False) for g in available_groups if hasattr(args, g)])
    explicit_selection = explicit_selection or args.pulse or bool(targeted)

    # Hardcoded runners list for compatibility and precise prefixes
    runners = [
        ("baker", "test_Baker"),
        ("cornell", "test_Cornell"),
        ("white", "test_White"),
        ("kashibe", "test_Kashibe"),
        ("talip", "test_Talip"),
        ("oxidation", "test_UO2_oxidation"),
        ("chromium", "test_Chromium"),
        ("contact", "test_CONTACT"),
        ("hbs", "test_UO2HBS"),
        ("vercors", "test_Vercors"),
        ("analytics", "test_"), # 'pulse'/'analytics' arg; broad prefix covers all analytics cases
        ("gpr", "test_GPR"),
    ]

    if not explicit_selection and not args.all:
        args.all = True

    for group, prefix in runners:
        # Check if this group is requested
        # The 'analytics' group is reachable via --pulse or --analytics
        if group == "analytics":
            group_flag = args.pulse or args.analytics
        else:
            group_flag = getattr(args, group, False)

        group_only = targeted.get(group)
        should_run = args.all or group_flag or bool(group_only)

        if should_run:
            results.extend(run_group(group, prefix, args.mode_gold, args.jobs, only=group_only))

    print("\n=== RESULTS ===")
    for name, ok, msg in results:
        status = "PASS" if ok else "FAIL"
        print(f"{name:<60} {status}")

    # Generate Report
    generate_html_report(results, regression_root)

    # exit code handling
    failed = [name for name, ok, msg in results if not ok]
    
    if failed:
        sys.exit(1)

    sys.exit(0)

if __name__ == "__main__":
    main()
