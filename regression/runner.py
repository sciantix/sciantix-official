import os
import argparse

from regression.baker.baker import run_baker
from regression.cornell.cornell import run_cornell


def run_all_baker(base_path: str = None, mode_gold: int = 0):
    if base_path is None:
        base_path = os.path.join(os.path.dirname(__file__), "baker")

    results = []
    for name in sorted(os.listdir(base_path)):
        if "Baker" in name:
            case_dir = os.path.join(base_path, name)
            if os.path.isdir(case_dir):
                print(f"Running Baker case: {name}")
                ok = run_baker(case_dir, mode_gold)
                results.append((name, ok))
    return results


def run_all_cornell(base_path: str = None, mode_gold: int = 0):
    if base_path is None:
        base_path = os.path.join(os.path.dirname(__file__), "cornell")

    results = []
    for name in sorted(os.listdir(base_path)):
        if "Cornell" in name:
            case_dir = os.path.join(base_path, name)
            if os.path.isdir(case_dir):
                print(f"Running Cornell case: {name}")
                ok = run_cornell(case_dir, mode_gold)
                results.append((name, ok))
    return results


def main():
    parser = argparse.ArgumentParser(
        description="SCIANTIX regression test runner"
    )

    parser.add_argument(
        "--baker",
        action="store_true",
        help="run Baker regression tests"
    )

    parser.add_argument(
        "--cornell",
        action="store_true",
        help="run Cornell regression tests"
    )

    parser.add_argument(
        "--all",
        action="store_true",
        help="run ALL regression tests"
    )

    parser.add_argument(
        "--mode-gold",
        type=int,
        default=0,
        help="mode_gold: 0=run+compare, 1=run+gold, 2=compare, 3=gold only"
    )

    args = parser.parse_args()

    if not (args.baker or args.cornell or args.all):
        args.all = True

    results = []

    if args.baker or args.all:
        results.extend(run_all_baker(mode_gold=args.mode_gold))

    if args.cornell or args.all:
        results.extend(run_all_cornell(mode_gold=args.mode_gold))

    print("\n=== RESULTS ===")
    for name, ok in results:
        print(f"{name:<40} {'PASS' if ok else 'FAIL'}")


if __name__ == "__main__":
    main()
