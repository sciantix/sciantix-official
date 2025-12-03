"""
regression.runner

python3 -m regression.runner
python3 -m regression.runner --white
python3 -m regression.runner --white --mode-gold 0
python3 -m regression.runner --white --mode-gold 1
"""

import argparse
from regression.core.generic_runner import run_group

def run_all_baker(mode):      return run_group("baker",      "test_Baker",          mode)
def run_all_cornell(mode):    return run_group("cornell",    "test_Cornell",        mode)
def run_all_white(mode):      return run_group("white",      "test_White",          mode)
def run_all_kashibe(mode):    return run_group("kashibe",    "test_Kashibe",        mode)
def run_all_talip(mode):      return run_group("talip",      "test_Talip",          mode)
def run_all_oxidation(mode):  return run_group("oxidation",  "test_UO2_oxidation",  mode)
def run_all_hbs(mode):        return run_group("hbs",        "test_UO2HBS",         mode)
def run_all_chromium(mode):   return run_group("chromium",   "test_Chromium",       mode)
def run_all_contact(mode):    return run_group("contact",    "test_CONTACT",        mode)
def run_all_vercors(mode):    return run_group("vercors",    "test_Vercors",        mode)
def run_all_pulse(mode):      return run_group("analytics",  "test_powerPulse",     mode)
def run_all_gpr(mode):        return run_group("gpr",        "test_GPR",           mode)

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

    args = parser.parse_args()

    # If nothing selected → run all
    if not any([
        args.baker, args.cornell, args.white, args.kashibe, args.talip,
        args.oxidation, args.chromium, args.contact, args.hbs,
        args.vercors, args.pulse, args.gpr, args.all
    ]):
        args.all = True

    results = []

    if args.baker or args.all:       results.extend(run_all_baker(args.mode_gold))
    if args.cornell or args.all:     results.extend(run_all_cornell(args.mode_gold))
    if args.white or args.all:       results.extend(run_all_white(args.mode_gold))
    if args.kashibe or args.all:     results.extend(run_all_kashibe(args.mode_gold))
    if args.talip or args.all:       results.extend(run_all_talip(args.mode_gold))
    if args.oxidation or args.all:   results.extend(run_all_oxidation(args.mode_gold))
    if args.chromium or args.all:    results.extend(run_all_chromium(args.mode_gold))
    if args.contact or args.all:     results.extend(run_all_contact(args.mode_gold))
    if args.hbs or args.all:         results.extend(run_all_hbs(args.mode_gold))
    if args.vercors or args.all:     results.extend(run_all_vercors(args.mode_gold))
    if args.pulse or args.all:       results.extend(run_all_pulse(args.mode_gold))
    if args.gpr or args.all:         results.extend(run_all_gpr(args.mode_gold))

    print("\n=== RESULTS ===")
    for name, ok in results:
        print(f"{name:<60} {'PASS' if ok else 'FAIL'}")


if __name__ == "__main__":
    main()
