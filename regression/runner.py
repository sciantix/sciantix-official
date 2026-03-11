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

    args = parser.parse_args()

    # Dynamic discovery of regression groups
    # We look for folders in regression/ that contain test cases (folders starting with test_)
    regression_root = os.path.join(os.path.dirname(__file__), "..", "regression")
    available_groups = []
    
    if os.path.isdir(regression_root):
        for entry in os.scandir(regression_root):
            if entry.is_dir() and entry.name not in ("core", "__pycache__", "analytics"): # analytics often handled separately or is empty
                # Check if it has at least one test_ folder
                has_tests = False
                for sub in os.scandir(entry.path):
                    if sub.is_dir() and sub.name.startswith("test_"):
                        has_tests = True
                        break
                if has_tests:
                    available_groups.append(entry.name)

    results = []
    
    # Selected groups map
    explicit_selection = any([getattr(args, g, False) for g in available_groups if hasattr(args, g)])
    
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
        ("analytics", "test_powerPulse"), # 'pulse' arg maps to 'analytics' group
        ("gpr", "test_GPR"),
    ]

    if not explicit_selection and not args.all:
        args.all = True

    for group, prefix in runners:
        # Check if this group is requested
        # The arg name might differ from group name (e.g. pulse vs analytics)
        arg_name = group if group != "analytics" else "pulse"
        
        should_run = args.all or getattr(args, arg_name, False)
        
        if should_run:
            results.extend(run_group(group, prefix, args.mode_gold, args.jobs))

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
