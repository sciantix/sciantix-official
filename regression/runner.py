import os
import argparse

from regression.baker.baker import run_baker
from regression.cornell.cornell import run_cornell
from regression.white.white import run_white

# Per gli altri gruppi dovrai creare moduli run_XXX analoghi
# per ora uso un placeholder generico che usa compare_outputs
from regression.core.common import run_sciantix, compare_outputs, make_gold
from regression.core.parser import SciantixOutput

REGRESSION_ROOT = os.path.dirname(__file__)


def generic_run(case_dir: str, mode_gold: int = 0):
    """
    Runner generico per gruppi ancora non implementati.
    Sostituibile in futuro da moduli dedicati.
    """
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


def run_from_pattern(pattern: str, mode_gold: int = 0, func=None):
    """
    Cerca test nella cartella regression/ usando il prefisso pattern.
    func = funzione dedicata (es. run_baker), altrimenti generic_run
    """
    if func is None:
        func = generic_run

    results = []
    for name in sorted(os.listdir(REGRESSION_ROOT)):
        if name.startswith(pattern):
            case_dir = os.path.join(REGRESSION_ROOT, name)
            if os.path.isdir(case_dir):
                print(f"Running {pattern} case: {name}")
                ok = func(case_dir, mode_gold)
                results.append((name, ok))
    return results


def run_all_baker(mode_gold=0):
    return run_from_pattern("test_Baker", mode_gold, func=run_baker)

def run_all_cornell(mode_gold=0):
    return run_from_pattern("test_GPR__", mode_gold, func=run_cornell)

def run_all_white(mode_gold=0):
    return run_from_pattern("test_White", mode_gold, func=run_white)


# ========================
# NUOVI GRUPPI
# ========================

def run_all_kashibe(mode_gold=0):
    return (
        run_from_pattern("test_Kashibe1990", mode_gold) +
        run_from_pattern("test_Kashibe1991", mode_gold) +
        run_from_pattern("test_Kashibe1993", mode_gold)
    )

def run_all_talip(mode_gold=0):
    return run_from_pattern("test_Talip", mode_gold)

def run_all_oxidation(mode_gold=0):
    return run_from_pattern("test_UO2_oxidation", mode_gold)

def run_all_chromium(mode_gold=0):
    return run_from_pattern("test_Chromium", mode_gold)

def run_all_contact(mode_gold=0):
    return run_from_pattern("test_CONTACT", mode_gold)

def run_all_hbs(mode_gold=0):
    return run_from_pattern("test_UO2HBS", mode_gold)

def run_all_vercors(mode_gold=0):
    return run_from_pattern("test_Vercors", mode_gold)

def run_all_pulse(mode_gold=0):
    return run_from_pattern("test_powerPulse", mode_gold)


# ========================
# MAIN + argparse
# ========================

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
    parser.add_argument("--all", action="store_true")

    parser.add_argument(
        "--mode-gold",
        type=int,
        default=0,
        help="0=run+compare, 1=run+gold, 2=compare only, 3=gold only"
    )

    args = parser.parse_args()

    if not any([
        args.baker, args.cornell, args.white, args.kashibe, args.talip,
        args.oxidation, args.chromium, args.contact, args.hbs,
        args.vercors, args.pulse, args.all
    ]):
        args.all = True

    results = []

    if args.baker or args.all:
        results.extend(run_all_baker(args.mode_gold))

    if args.cornell or args.all:
        results.extend(run_all_cornell(args.mode_gold))

    if args.white or args.all:
        results.extend(run_all_white(args.mode_gold))

    if args.kashibe or args.all:
        results.extend(run_all_kashibe(args.mode_gold))

    if args.talip or args.all:
        results.extend(run_all_talip(args.mode_gold))

    if args.oxidation or args.all:
        results.extend(run_all_oxidation(args.mode_gold))

    if args.chromium or args.all:
        results.extend(run_all_chromium(args.mode_gold))

    if args.contact or args.all:
        results.extend(run_all_contact(args.mode_gold))

    if args.hbs or args.all:
        results.extend(run_all_hbs(args.mode_gold))

    if args.vercors or args.all:
        results.extend(run_all_vercors(args.mode_gold))

    if args.pulse or args.all:
        results.extend(run_all_pulse(args.mode_gold))

    print("\n=== RESULTS ===")
    for name, ok in results:
        print(f"{name:<40} {'PASS' if ok else 'FAIL'}")


if __name__ == "__main__":
    main()
