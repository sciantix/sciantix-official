"""
sciantix testing suite
author: Giovanni Zullo, Elisa Cappellari

testing.runner

python3 -m testing.runner
python3 -m testing.runner --white
python3 -m testing.runner --white --mode-gold 0
python3 -m testing.runner --white --mode-gold 1
python3 -m testing.runner --verification
python3 -m testing.runner --validation
python3 -m testing.runner --oc            # build/checkout is expected to have OpenCalphad
                                              # linked; any group that still can't use it fails
                                              # loudly instead of skipping

Test groups live under two top-level directories
- verification/ (does SCIANTIX reproduce its reference model?) 
- validation/   (does it match real experimental data?)

Every group is attempted on every run; groups that need OpenCalphad/a
specific CALPHAD database degrade gracefully when it isn't available.
"""

import sys
import os
import argparse
from collections import namedtuple

from testing.core.generic_runner import run_group
from testing.core.report import generate_html_report
from testing.core.oc_status import detect_oc_status
from testing.core import mox_po2_runner

REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
SUITE_ROOTS = {
    "verification": os.path.join(REPO_ROOT, "verification"),
    "validation": os.path.join(REPO_ROOT, "validation"),
}

OCRequirement = namedtuple("OCRequirement", ["kind", "databases", "markers"])
# kind: "column_degrade" -- still run, excluding `markers`-matching columns from comparison
#       "skip_group"     -- OC affects nearly every column with no non-OC analog; skip entirely

# (cli_name, suite, relpath_under_suite, case_prefix, extra_outputs, oc_requirement)
REGISTRY = [
    ("mox-po2", "verification", "test_MOX_po2", "T_",
        (), OCRequirement("column_degrade", ("upuo-v21.TDB",), ("CALPHAD",))),
    ("openPorosity", "verification", "test_openPorosity", "", (), None),
    ("powerPulse",   "verification", "test_powerPulse",   "", (), None),
    ("oxidation",    "verification", "test_oxidation",    "test_UO2_oxidation", (), None),
    ("vercors",      "verification", "test_vercors5",     "", (), None),
    ("gpr",          "verification", "test_gpr",          "test_GPR", (), None),

    ("baker",    "validation", "baker",    "test_Baker",   (), None),
    ("cornell",  "validation", "cornell",  "test_Cornell", (), None),
    ("white",    "validation", "white",    "test_White",   (), None),
    ("kashibe",  "validation", "kashibe",  "test_Kashibe", (), None),
    ("talip",    "validation", "talip",    "test_Talip",   (), None),
    ("chromium", "validation", "chromium", "test_Chromium", (), None),
    ("contact",  "validation", "contact",  "test_CONTACT", (), None),
    ("hbs",      "validation", "hbs",      "test_UO2HBS",  (), None),
    ("jog", "validation", "jog", "test_PHENIXpins", ("thermochemistry_output.txt",),
        OCRequirement("skip_group", ("BaMoO_CsMoO_MoPdRhRuTc_merged.TDB",), ())),
    ("oxygenpotential-freshfuel", "validation", "oxygenpotential/freshfuel", "test_",
        (), OCRequirement("column_degrade", ("upuo-v21.TDB",), ("CALPHAD",))),
    ("oxygenpotential-burnup", "validation", "oxygenpotential/burnup", "test_",
        (), OCRequirement("column_degrade", ("upuo-v21.TDB",), ("CALPHAD",))),
]

def cli_dest(cli_name):
    return cli_name.replace("-", "_")


def build_parser():
    parser = argparse.ArgumentParser(description="SCIANTIX testing runner")

    parser.add_argument("--verification", action="store_true",
                         help="Run every group under verification/")
    parser.add_argument("--validation", action="store_true",
                         help="Run every group under validation/")

    for cli_name, _, _, _, _, _ in REGISTRY:
        parser.add_argument(f"--{cli_name}", action="store_true")

    parser.add_argument("--pulse", action="store_true",
                         help="Alias for openPorosity + powerPulse")
    parser.add_argument("--analytics", action="store_true",
                         help="Alias for openPorosity + powerPulse")
    parser.add_argument("--oxygenpotential", action="store_true",
                         help="Alias for oxygenpotential-freshfuel + oxygenpotential-burnup")

    parser.add_argument("--oc", action="store_true",
                         help="Strict mode: OpenCalphad is expected to be linked and every "
                              "required database present; any group that would otherwise "
                              "degrade/skip for lack of OC instead fails loudly")

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

    return parser


def parse_targeted(extras):
    """--<cli_name>.<case-substring> -> {cli_name: {substrings}}"""
    targeted = {}
    for tok in extras:
        if tok.startswith("--") and "." in tok:
            group, _, case = tok[2:].partition(".")
            if group and case:
                targeted.setdefault(group, set()).add(case)
        else:
            print(f"[WARN] Ignoring unrecognized argument: {tok}")
    return targeted


def is_selected(cli_name, suite, args, targeted, run_everything):
    if run_everything:
        return True
    if suite == "verification" and args.verification:
        return True
    if suite == "validation" and args.validation:
        return True
    if getattr(args, cli_dest(cli_name), False):
        return True
    if cli_name in targeted:
        return True
    if cli_name in ("openPorosity", "powerPulse") and (args.pulse or args.analytics):
        return True
    if cli_name in ("oxygenpotential-freshfuel", "oxygenpotential-burnup") and args.oxygenpotential:
        return True
    return False


def main():
    args, extras = build_parser().parse_known_args()
    targeted = parse_targeted(extras)

    explicit_selection = (
        args.verification or args.validation
        or args.pulse or args.analytics or args.oxygenpotential
        or any(getattr(args, cli_dest(cli_name), False) for cli_name, *_ in REGISTRY)
        or bool(targeted)
    )
    run_everything = not explicit_selection

    databases_needed = set(mox_po2_runner.REQUIRED_DATABASES)
    for _, _, _, _, _, oc_req in REGISTRY:
        if oc_req is not None:
            databases_needed.update(oc_req.databases)
    oc_status = detect_oc_status(databases_needed)

    results = []
    oc_notes = []
    mox_po2_selected = False

    for cli_name, suite, relpath, prefix, extra_outputs, oc_req in REGISTRY:
        if not is_selected(cli_name, suite, args, targeted, run_everything):
            continue

        if cli_name == "mox-po2":
            mox_po2_selected = True

        base_dir = os.path.join(SUITE_ROOTS[suite], *relpath.split("/"))
        group_only = targeted.get(cli_name)

        ignore_columns = ()
        skip_reason = None
        skip_is_failure = False

        if oc_req is not None:
            oc_ok = oc_status.available_for(oc_req.databases)
            if not oc_ok:
                reason = oc_status.reason_for(oc_req.databases)
                if args.mode_gold in (1, 3):
                    skip_reason = f"gold-rewrite refused ({reason})"
                    skip_is_failure = args.oc
                elif args.oc:
                    skip_reason = f"--oc given but {reason}"
                    skip_is_failure = True
                elif oc_req.kind == "skip_group":
                    skip_reason = f"{reason} -- skipping group"
                else:
                    ignore_columns = oc_req.markers
                    skip_reason = None

                if skip_reason:
                    tag = "FAIL" if skip_is_failure else "SKIP"
                    print(f"[{tag}] {cli_name}: {skip_reason}")
                    oc_notes.append(f"{cli_name}: {skip_reason}")
                else:
                    note = f"{cli_name}: {reason} -- excluding column(s) {list(oc_req.markers)}"
                    print(f"[WARN] {note}")
                    oc_notes.append(note)

        results.extend(run_group(
            cli_name, base_dir, prefix, args.mode_gold, args.jobs,
            only=group_only, extra_outputs=extra_outputs,
            ignore_columns=ignore_columns, skip_reason=skip_reason,
            skip_is_failure=skip_is_failure, suite=suite,
        ))

    if mox_po2_selected:
        results.extend(mox_po2_runner.check_accuracy(oc_status, "verification"))

    print("\n=== RESULTS ===")
    for name, ok, msg, suite in results:
        status = "PASS" if ok else ("SKIP" if ok is None else "FAIL")
        line = f"{name:<60} {suite:<13} {status}"
        if msg:
            line += f"  ({msg})"
        print(line)

    passed = sum(1 for _, ok, _, _ in results if ok is True)
    failed = sum(1 for _, ok, _, _ in results if ok is False)
    skipped = sum(1 for _, ok, _, _ in results if ok is None)
    print(f"\n{passed} passed, {failed} failed, {skipped} skipped")

    # Generate Report
    report_dir = os.path.dirname(__file__)
    oc_summary = "\n".join(oc_notes) if oc_notes else None
    generate_html_report(results, report_dir, oc_summary=oc_summary)

    sys.exit(1 if failed else 0)

if __name__ == "__main__":
    main()
