import os
import argparse

from regression.baker.baker import run_baker
from regression.cornell.cornell import run_cornell
from regression.white.white import run_white

from regression.kashibe.kashibe import run_kashibe
from regression.talip.talip import run_talip
from regression.oxidation.oxidation import run_oxidation
from regression.chromium.chromium import run_chromium
from regression.contact.contact import run_contact
from regression.hbs.hbs import run_hbs
from regression.vercors.vercors import run_vercors


REGRESSION_ROOT = os.path.dirname(__file__)


def scan_and_run(group_name: str, prefix: str, func, mode_gold: int):
    """
    Cerca i test nella sottocartella regression/<group_name>/.
    """
    base_path = os.path.join(REGRESSION_ROOT, group_name)
    results = []

    if not os.path.isdir(base_path):
        print(f"Missing directory: {base_path}")
        return results

    for name in sorted(os.listdir(base_path)):
        if name.startswith(prefix):
            case_dir = os.path.join(base_path, name)
            if os.path.isdir(case_dir):
                print(f"Running {group_name} case: {name}")
                ok = func(case_dir, mode_gold)
                results.append((f"{group_name}/{name}", ok))

    return results


# gruppi
def run_all_baker(mode_gold):      return scan_and_run("baker",      "test_Baker",      run_baker,      mode_gold)
def run_all_cornell(mode_gold):    return scan_and_run("cornell",    "test_Cornell",    run_cornell,    mode_gold)
def run_all_white(mode_gold):      return scan_and_run("white",      "test_White",      run_white,      mode_gold)
def run_all_kashibe(mode_gold):    return scan_and_run("kashibe",    "test_Kashibe",    run_kashibe,    mode_gold)
def run_all_talip(mode_gold):      return scan_and_run("talip",      "test_Talip",      run_talip,      mode_gold)
def run_all_oxidation(mode_gold):  return scan_and_run("oxidation",  "test_UO2_oxidation", run_oxidation, mode_gold)
def run_all_chromium(mode_gold):   return scan_and_run("chromium",   "test_Chromium",   run_chromium,   mode_gold)
def run_all_contact(mode_gold):    return scan_and_run("contact",    "test_CONTACT",    run_contact,    mode_gold)
def run_all_hbs(mode_gold):        return scan_and_run("hbs",        "test_UO2HBS",     run_hbs,        mode_gold)
def run_all_vercors(mode_gold):    return scan_and_run("vercors",    "test_Vercors",    run_vercors,    mode_gold)


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
    parser.add_argument("--all", action="store_true")

    parser.add_argument(
        "--mode-gold",
        type=int,
        default=0,
        help="0=run+compare, 1=run+gold, 2=compare, 3=gold only"
    )

    args = parser.parse_args()

    if not any([
        args.baker, args.cornell, args.white, args.kashibe, args.talip,
        args.oxidation, args.chromium, args.contact, args.hbs,
        args.vercors, args.all
    ]):
        args.all = True

    results = []

    if args.baker or args.all:        results.extend(run_all_baker(args.mode_gold))
    if args.cornell or args.all:      results.extend(run_all_cornell(args.mode_gold))
    if args.white or args.all:        results.extend(run_all_white(args.mode_gold))
    if args.kashibe or args.all:      results.extend(run_all_kashibe(args.mode_gold))
    if args.talip or args.all:        results.extend(run_all_talip(args.mode_gold))
    if args.oxidation or args.all:    results.extend(run_all_oxidation(args.mode_gold))
    if args.chromium or args.all:     results.extend(run_all_chromium(args.mode_gold))
    if args.contact or args.all:      results.extend(run_all_contact(args.mode_gold))
    if args.hbs or args.all:          results.extend(run_all_hbs(args.mode_gold))
    if args.vercors or args.all:      results.extend(run_all_vercors(args.mode_gold))

    print("\n=== RESULTS ===")
    for name, ok in results:
        print(f"{name:<60} {'PASS' if ok else 'FAIL'}")


if __name__ == "__main__":
    main()
