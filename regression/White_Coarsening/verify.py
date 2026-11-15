"""#COARSENING: native scaling, restart, pre-ramp and refinement checks."""

from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
import json
import shutil
import subprocess
import tempfile
import numpy as np
import run as campaign


def verify_model(binary, jobs):
    here, root = campaign.HERE, campaign.ROOT
    rows = json.loads((here / "results.json").read_text())
    by = {(r["sample"], r["model"]): r for r in rows}
    pre_errors = []
    for sample in sorted({r["sample"] for r in rows}):
        a, b = (by[sample, m]["states"]["pre"] for m in ("coarsening", "legacy"))
        e = {k: abs(a[k] / b[k] - 1) for k in ("Rb", "Nb")}
        assert max(e.values()) < 3e-4, (sample, e)
        # The adaptive cold kernel and the legacy outer-step solver approach
        # the same bulk limit; require small differences and refine below.
        e["pool_pp"] = max(abs(a[k] - b[k]) for k in campaign.POOLS)
        assert e["pool_pp"] < 0.05, (sample, a, b)
        pre_errors.append(e)
    refined_jobs = [
        (s, m)
        for s in ("4005-A", "4065-A", "4135-A", "4163-D")
        for m in ("coarsening",)
    ]
    with ThreadPoolExecutor(max_workers=jobs) as pool:
        refined = list(
            pool.map(lambda j: campaign.run(binary, *j, 400, 2e-6), refined_jobs)
        )
    refinement = []
    for b in refined:
        a = by[b["sample"], "coarsening"]["states"]["end"]
        fine = b["states"]["end"]
        e = {k: abs(a[k] / fine[k] - 1) for k in ("R", "N", "S")}
        assert max(e.values()) < 5e-4, (b["sample"], e)
        refinement.append(dict(sample=b["sample"], relative=e))
    native_fine = campaign.run(binary, "4005-A", "legacy", 400, 2e-6)
    model_fine = next(r for r in refined if r["sample"] == "4005-A")
    pre_fine = max(
        abs(model_fine["states"]["pre"][k] - native_fine["states"]["pre"][k])
        for k in campaign.POOLS
    )
    pre_coarse = max(
        abs(
            by["4005-A", "coarsening"]["states"]["pre"][k]
            - by["4005-A", "legacy"]["states"]["pre"][k]
        )
        for k in campaign.POOLS
    )
    assert pre_fine < pre_coarse, ("Preramp convergence", pre_fine, pre_coarse)
    checks = {}
    for label, scales in [
        ("K_zero", (0.0, 1.0, 1.0)),
        ("D_zero", (1.0, 0.0, 1.0)),
        ("Dv_zero", (1.0, 1.0, 0.0)),
    ]:
        r = campaign.run(binary, "4005-A", "coarsening", 200, 2e-5, scales)
        t = campaign.output(here / r["trace"])
        key = {
            "K_zero": "Coarsening dislocation concentration (bub/m3)",
            "D_zero": "Coarsening line trapping rate (1/s)",
            "Dv_zero": "Coarsening vacancy concentration (vac/m3)",
        }[label]
        assert np.all(t[key] == 0), (label, key)
        checks[label] = dict(check=key, all_zero=True)
    fixed = campaign.run(binary, "4005-A", "coarsening", 200, 2e-5, density=0)
    t = campaign.output(here / fixed["trace"])
    assert np.all(t["Dislocation density (m/m3)"] == 4e13)
    checks["density_zero"] = dict(constant_m2=4e13)
    # Old files may omit the density selector and all three new factors.
    with tempfile.TemporaryDirectory(prefix="coarsening-optional-input-") as temporary:
        work = Path(temporary)
        campaign.prepare(work, "4005-A", "coarsening")
        settings = work / "input_settings.txt"
        settings.write_text(
            "\n".join(
                line
                for line in settings.read_text().splitlines()
                if "iDislocationDensity" not in line
            )
            + "\n"
        )
        (work / "input_scaling_factors.txt").unlink()
        process = subprocess.run(
            [str(binary), str(work)],
            cwd=work,
            env=campaign.environment(200, 2e-5),
            capture_output=True,
            text=True,
        )
        assert process.returncode == 0, process.stderr
        default = campaign.summarize(
            work / "output.txt", work / "input_history.txt", "coarsening"
        )
        for key in ("R", "N", "S", "Sgb"):
            assert (
                abs(
                    default["states"]["end"][key]
                    / by["4005-A", "coarsening"]["states"]["end"][key]
                    - 1
                )
                < 1e-12
            )
        checks["optional_input_defaults"] = dict(density=1, all_new_factors=1)
    for label, change in [
        ("old_model5", ("iIntraGranularBubbleBehavior", 5)),
        ("old_model6", ("iIntraGranularBubbleBehavior", 6)),
        ("invalid_density", ("iDislocationDensity", 2)),
    ]:
        with tempfile.TemporaryDirectory(prefix="coarsening-invalid-") as temporary:
            work = Path(temporary)
            campaign.prepare(work, "4005-A", "coarsening")
            path = work / "input_settings.txt"
            path.write_text(campaign.setting(path.read_text(), *change))
            process = subprocess.run(
                [str(binary), str(work)],
                cwd=work,
                env=campaign.environment(200, 2e-5),
                capture_output=True,
                text=True,
            )
            assert process.returncode != 0, label
            checks[label] = dict(rejected=True)
    # The restart helper uses the native public arrays in separate processes.
    build = binary.parent
    objects = list((build / "CMakeFiles/sciantix.dir/src").rglob("*.o"))
    assert objects, "Restart verification requires the CMake object files"
    includes = [root / "include"] + [
        p for p in (root / "include").rglob("*") if p.is_dir()
    ]
    driver = build / "coarsening_restart.x"
    process = subprocess.run(
        [
            "g++",
            "-std=c++17",
            "-O2",
            *[f"-I{p}" for p in includes],
            str(root / "tests/coarsening_restart.C"),
            *[str(p) for p in objects if p.name != "MainSCIANTIX.C.o"],
            "-o",
            str(driver),
        ],
        capture_output=True,
        text=True,
    )
    assert process.returncode == 0, process.stderr
    restart = []
    with tempfile.TemporaryDirectory(prefix="coarsening-restart-") as temporary:
        work = Path(temporary)
        campaign.prepare(work, "4005-A", "coarsening")

        def execute(start, end, path):
            p = subprocess.run(
                [str(driver), str(work), str(start), str(end), str(path)],
                cwd=work,
                env=campaign.environment(200, 2e-5),
                capture_output=True,
                text=True,
            )
            assert p.returncode == 0, p.stderr

        full = work / "full.bin"
        execute(0, 200, full)
        for split in (25, 85, 170):
            checkpoint = work / "checkpoint.bin"
            execute(0, split, checkpoint)
            execute(split + 1, 200, checkpoint)
            assert checkpoint.read_bytes() == full.read_bytes(), split
            restart.append(dict(split=split, identical_bytes=True))
    return dict(
        preramp_max_relative={k: max(r[k] for r in pre_errors) for k in ("Rb", "Nb")},
        preramp_max_pool_pp=max(r["pool_pp"] for r in pre_errors),
        preramp_4005A_convergence_pp=dict(coarse=pre_coarse, fine=pre_fine),
        refinement=refinement,
        scaling_and_selectors=checks,
        restart=restart,
    )
