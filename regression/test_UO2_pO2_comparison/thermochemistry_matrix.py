from __future__ import annotations

import math
import os
import re
import shutil
import subprocess
from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

mpl.rcParams.update({
    "font.family": "arial",
    "font.size": 18,
    "axes.labelsize": 16,
    "axes.titlesize": 16,
    "xtick.labelsize": 14,
    "ytick.labelsize": 14,
    "legend.fontsize": 14,
    "figure.dpi": 300,
    "axes.grid": True,
    "grid.alpha": 0.3,
    "grid.linestyle": "--",
    "lines.linewidth": 2.2,
})

REFERENCE_OXYGEN_PRESSURE_MPA = 0.1013
SCRIPT_DIR = Path(__file__).resolve().parent
INPUT_INITIAL = SCRIPT_DIR / "input_initial_conditions.txt"
RESULTS_DIR = SCRIPT_DIR / "results"
VERIFICATION_DIR = SCRIPT_DIR / "verification"
VERIFICATION_PLOTS_DIR = VERIFICATION_DIR / "plots"
STANDALONE_OC_DIR = VERIFICATION_DIR / "standalone_oc"
LOCAL_BINARY = SCRIPT_DIR / "sciantix.x"
DEFAULT_BUILD_BINARY = SCRIPT_DIR.parent.parent / "build" / "sciantix.x"
OC_BINARY = SCRIPT_DIR.parent.parent.parent / "opencalphad" / "oc6P"
OC_DATABASE = SCRIPT_DIR.parent.parent.parent / "opencalphad" / "data" / "OU.TDB"
STOICHIOMETRY = np.linspace(0.00, 0.01, 11)


def case_sort_key(path: Path):
    try:
        return float(path.name.replace("_", "."))
    except ValueError:
        return float("inf")


def modify_input_initial_conditions(value: float):
    with open(INPUT_INITIAL, "r") as file:
        lines = file.readlines()

    lines[24] = f"{value}\n"

    with open(INPUT_INITIAL, "w") as file:
        file.writelines(lines)


def ensure_binary():
    if DEFAULT_BUILD_BINARY.exists() and not LOCAL_BINARY.exists():
        shutil.copy(DEFAULT_BUILD_BINARY, LOCAL_BINARY)


def run_sciantix(case_folder: Path):
    result = subprocess.run(
        [str(LOCAL_BINARY)],
        cwd=SCRIPT_DIR,
        check=False,
        capture_output=True,
        text=True,
    )

    debug_output = ""
    if result.stdout:
        debug_output += result.stdout
    if result.stderr:
        debug_output += result.stderr

    if debug_output:
        print(debug_output, end="")
        (case_folder / "oc_debug.txt").write_text(debug_output)

    if result.returncode != 0:
        raise subprocess.CalledProcessError(result.returncode, result.args)

    shutil.move(SCRIPT_DIR / "thermochemistry_output.txt", RESULTS_DIR / "thermochemistry_output_chemistry.txt")
    shutil.move(SCRIPT_DIR / "output.txt", RESULTS_DIR / "output_chemistry.txt")
    shutil.move(SCRIPT_DIR / "input_check.txt", RESULTS_DIR / "input_check_output_chemistry.txt")


def plot_comparison_case(case_folder: Path):
    data = pd.read_csv(case_folder / "output_chemistry.txt", sep="\t")

    xdev = data["Stoichiometry deviation (/)"].iloc[0]
    oxygen_fraction = (2 + xdev) / (3 + xdev)
    temperature = data["Temperature (K)"]

    blackburn_po2 = data["Fuel oxygen partial pressure - Blackburn (MPa)"]
    calphad_po2 = data["Fuel oxygen partial pressure - CALPHAD (MPa)"]
    active_po2 = data["Fuel oxygen partial pressure (MPa)"]

    fig, ax = plt.subplots(figsize=(10, 6))
    ax.semilogy(temperature, blackburn_po2, "--", label="Blackburn")
    ax.semilogy(temperature, calphad_po2, "-", label="CALPHAD")
    ax.semilogy(temperature, active_po2, ":", label="Active in SCIANTIX")
    ax.set_xlabel("Temperature (K)")
    ax.set_ylabel("Oxygen partial pressure (MPa)")
    ax.set_title(f"O/U comparison, x = {xdev:.4f}, X(O) = {oxygen_fraction:.6f}")
    ax.legend()
    fig.tight_layout()
    fig.savefig(case_folder / "pO2_comparison.png")
    plt.close(fig)

    fig, ax = plt.subplots(figsize=(10, 6))
    ax.plot(temperature, data["Fuel oxygen potential - Blackburn (KJ/mol)"], "--", label="Blackburn")
    ax.plot(temperature, data["Fuel oxygen potential - CALPHAD (KJ/mol)"], "-", label="CALPHAD")
    ax.plot(temperature, data["Fuel oxygen potential (KJ/mol)"], ":", label="Active in SCIANTIX")
    ax.set_xlabel("Temperature (K)")
    ax.set_ylabel("Oxygen potential (kJ/mol)")
    ax.set_title(f"O/U comparison, x = {xdev:.4f}, X(O) = {oxygen_fraction:.6f}")
    ax.legend()
    fig.tight_layout()
    fig.savefig(case_folder / "oxygen_potential_comparison.png")
    plt.close(fig)

    summary = pd.DataFrame(
        {
            "Temperature (K)": temperature,
            "Stoichiometry deviation (/)": data["Stoichiometry deviation (/)"],
            "Fuel oxygen partial pressure - Blackburn (MPa)": blackburn_po2,
            "Fuel oxygen partial pressure - CALPHAD (MPa)": calphad_po2,
            "Fuel oxygen potential - Blackburn (KJ/mol)": data["Fuel oxygen potential - Blackburn (KJ/mol)"],
            "Fuel oxygen potential - CALPHAD (KJ/mol)": data["Fuel oxygen potential - CALPHAD (KJ/mol)"],
        }
    )
    summary.to_csv(case_folder / "oxygen_comparison_summary.txt", sep="\t", index=False)


def parse_oc_debug(debug_path: Path):
    text = debug_path.read_text()

    input_blocks = re.findall(
        r"\[OC input\].*?set c t=([0-9Ee+\-\.]+) p=([0-9Ee+\-\.]+) n=1 x\(o\)=([0-9Ee+\-\.]+).*?fin",
        text,
        flags=re.S,
    )
    output_blocks = re.findall(
        r"Some data for components .*?^\s*O\s+([0-9Ee+\-\.]+)\s+([0-9Ee+\-\.]+)\s+([0-9Ee+\-\.]+)\s+([0-9Ee+\-\.]+)",
        text,
        flags=re.S | re.M,
    )
    phase_blocks = re.findall(
        r"\[OC parser\] Parsed phases(.*?)(?=\n\[OC input\]|\Z)",
        text,
        flags=re.S,
    )

    rows = []
    count = min(len(input_blocks), len(output_blocks))
    for i in range(count):
        temp, pressure, x_o = input_blocks[i]
        o_moles, o_x, mu_over_rt, activity = output_blocks[i]
        reconstructed_po2 = REFERENCE_OXYGEN_PRESSURE_MPA * float(activity) ** 2
        reconstructed_potential = (
            0.0
            if reconstructed_po2 <= 0.0
            else 8.314e-3 * float(temp) * math.log(reconstructed_po2 / REFERENCE_OXYGEN_PRESSURE_MPA)
        )
        rows.append(
            {
                "Temperature (K)": float(temp),
                "Pressure (Pa)": float(pressure),
                "Input x(O)": float(x_o),
                "OC O moles": float(o_moles),
                "OC O mole fraction": float(o_x),
                "OC mu(O)/RT": float(mu_over_rt),
                "OC activity(O)": float(activity),
                "Reconstructed CALPHAD pO2 (MPa)": reconstructed_po2,
                "Reconstructed CALPHAD oxygen potential (kJ/mol)": reconstructed_potential,
                "Phase dump": phase_blocks[i].strip() if i < len(phase_blocks) else "",
            }
        )

    return pd.DataFrame(rows), text.count("Warning: OpenCalphad returned an invalid equilibrium")


def parse_oc_output_text(output_text: str):
    input_match = re.search(
        r"Conditions .*?1:T=([0-9Ee+\-\.]+), 2:P=([0-9Ee+\-\.]+), 3:N=1, 4:X\(O\)=([0-9Ee+\-\.]+)",
        output_text,
        flags=re.S,
    )
    output_match = re.search(
        r"Some data for components .*?^\s*O\s+([0-9Ee+\-\.]+)\s+([0-9Ee+\-\.]+)\s+([0-9Ee+\-\.]+)\s+([0-9Ee+\-\.]+)",
        output_text,
        flags=re.S | re.M,
    )

    if not input_match or not output_match:
        return None

    temp, pressure, x_o = input_match.groups()
    o_moles, o_x, mu_over_rt, activity = output_match.groups()
    reconstructed_po2 = REFERENCE_OXYGEN_PRESSURE_MPA * float(activity) ** 2
    reconstructed_potential = (
        0.0
        if reconstructed_po2 <= 0.0
        else 8.314e-3 * float(temp) * math.log(reconstructed_po2 / REFERENCE_OXYGEN_PRESSURE_MPA)
    )

    return {
        "Temperature (K)": float(temp),
        "Pressure (Pa)": float(pressure),
        "Input x(O)": float(x_o),
        "OC O moles": float(o_moles),
        "OC O mole fraction": float(o_x),
        "OC mu(O)/RT": float(mu_over_rt),
        "OC activity(O)": float(activity),
        "Standalone CALPHAD pO2 (MPa)": reconstructed_po2,
        "Standalone CALPHAD oxygen potential (kJ/mol)": reconstructed_potential,
        "Standalone invalid equilibrium": (
            "not a valid equilibrium as last calculation failed" in output_text
            or "No results as no equilibrium calculated" in output_text
        ),
    }


def run_standalone_oc_case(case_dir: Path, output_df: pd.DataFrame):
    if not OC_BINARY.exists():
        raise FileNotFoundError(f"OpenCalphad binary not found: {OC_BINARY}")
    if not OC_DATABASE.exists():
        raise FileNotFoundError(f"OpenCalphad database not found: {OC_DATABASE}")

    standalone_case_dir = STANDALONE_OC_DIR / case_dir.name
    standalone_case_dir.mkdir(parents=True, exist_ok=True)

    xdev = float(output_df["Stoichiometry deviation (/)"].iloc[0])
    oxygen_fraction = (2.0 + xdev) / (3.0 + xdev)

    rows = []
    invalid_count = 0
    debug_chunks = []

    for idx, temperature in enumerate(output_df["Temperature (K)"]):
        macro_path = standalone_case_dir / f"standalone_{idx:03d}.OCM"
        dat_path = standalone_case_dir / f"standalone_{idx:03d}.DAT"
        macro_path.write_text(
            "\n".join(
                [
                    "@$ Standalone OC validation",
                    f"r t {OC_DATABASE.with_suffix('')}",
                    "",
                    f"set c t={temperature} p=100000 n=1 x(o)={oxygen_fraction}",
                    "",
                    "c e",
                    "",
                    f"l /out={dat_path} r 1",
                    "",
                    "fin",
                ]
            )
        )

        result = subprocess.run(
            [str(OC_BINARY), str(macro_path)],
            cwd=SCRIPT_DIR,
            check=False,
            capture_output=True,
            text=True,
        )
        debug_chunks.append(f"\n[Standalone OC input] {macro_path}\n{macro_path.read_text()}\n")
        if result.stdout:
            debug_chunks.append(result.stdout)
        if result.stderr:
            debug_chunks.append(result.stderr)

        output_text = dat_path.read_text() if dat_path.exists() else ""
        debug_chunks.append(f"\n[Standalone OC output] {dat_path}\n{output_text}\n")

        parsed = parse_oc_output_text(output_text)
        if parsed is None:
            rows.append(
                {
                    "Temperature (K)": float(temperature),
                    "Pressure (Pa)": 100000.0,
                    "Input x(O)": oxygen_fraction,
                    "OC O moles": np.nan,
                    "OC O mole fraction": np.nan,
                    "OC mu(O)/RT": np.nan,
                    "OC activity(O)": np.nan,
                    "Standalone CALPHAD pO2 (MPa)": np.nan,
                    "Standalone CALPHAD oxygen potential (kJ/mol)": np.nan,
                    "Standalone invalid equilibrium": True,
                }
            )
            invalid_count += 1
            continue

        if parsed["Standalone invalid equilibrium"]:
            invalid_count += 1
        rows.append(parsed)

    debug_path = standalone_case_dir / "standalone_oc_debug.txt"
    debug_path.write_text("\n".join(debug_chunks))

    standalone_df = pd.DataFrame(rows)
    standalone_df.to_csv(standalone_case_dir / "standalone_oc_table.tsv", sep="\t", index=False)
    return standalone_df, invalid_count


def add_derived_columns(df: pd.DataFrame):
    df = df.copy()
    df["Reconstructed x(O) from stoichiometry"] = (df["Stoichiometry deviation (/)"] + 2.0) / (
        df["Stoichiometry deviation (/)"] + 3.0
    )
    df["x(O) abs error"] = (df["Input x(O)"] - df["Reconstructed x(O) from stoichiometry"]).abs()
    df["CALPHAD pO2 abs rel error"] = (
        (df["Fuel oxygen partial pressure - CALPHAD (MPa)"] - df["Reconstructed CALPHAD pO2 (MPa)"]).abs()
        / df["Reconstructed CALPHAD pO2 (MPa)"].replace(0.0, pd.NA)
    )
    df["CALPHAD potential abs error (kJ/mol)"] = (
        df["Fuel oxygen potential - CALPHAD (KJ/mol)"]
        - df["Reconstructed CALPHAD oxygen potential (kJ/mol)"]
    ).abs()
    df["Blackburn/CALPHAD pO2 ratio"] = (
        df["Fuel oxygen partial pressure - Blackburn (MPa)"]
        / df["Fuel oxygen partial pressure - CALPHAD (MPa)"].replace(0.0, pd.NA)
    )
    df["Blackburn minus CALPHAD potential (kJ/mol)"] = (
        df["Fuel oxygen potential - Blackburn (KJ/mol)"]
        - df["Fuel oxygen potential - CALPHAD (KJ/mol)"]
    )
    if "Standalone CALPHAD pO2 (MPa)" in df.columns:
        df["Standalone CALPHAD pO2 abs rel error"] = (
            (df["Fuel oxygen partial pressure - CALPHAD (MPa)"] - df["Standalone CALPHAD pO2 (MPa)"]).abs()
            / df["Standalone CALPHAD pO2 (MPa)"].replace(0.0, pd.NA)
        )
        df["Standalone CALPHAD potential abs error (kJ/mol)"] = (
            df["Fuel oxygen potential - CALPHAD (KJ/mol)"]
            - df["Standalone CALPHAD oxygen potential (kJ/mol)"]
        ).abs()
    return df


def plot_verification_case(case_name: str, df: pd.DataFrame):
    VERIFICATION_PLOTS_DIR.mkdir(parents=True, exist_ok=True)

    fig, ax = plt.subplots(figsize=(9, 5))
    ax.semilogy(df["Temperature (K)"], df["Fuel oxygen partial pressure - Blackburn (MPa)"], "--", label="Blackburn")
    ax.semilogy(df["Temperature (K)"], df["Fuel oxygen partial pressure - CALPHAD (MPa)"], "-", label="CALPHAD stored")
    ax.semilogy(
        df["Temperature (K)"],
        df["Reconstructed CALPHAD pO2 (MPa)"],
        ":",
        label="CALPHAD reconstructed from activity(O)",
    )
    if "Standalone CALPHAD pO2 (MPa)" in df.columns:
        ax.semilogy(
            df["Temperature (K)"],
            df["Standalone CALPHAD pO2 (MPa)"],
            "-.",
            label="Standalone OC",
        )
    ax.set_xlabel("Temperature (K)")
    ax.set_ylabel("pO2 (MPa)")
    ax.set_title(f"{case_name}: pO2 verification")
    ax.legend()
    fig.tight_layout()
    fig.savefig(VERIFICATION_PLOTS_DIR / f"{case_name}_po2_verification.png")
    plt.close(fig)

    fig, ax = plt.subplots(figsize=(9, 5))
    ax.plot(df["Temperature (K)"], df["Fuel oxygen potential - Blackburn (KJ/mol)"], "--", label="Blackburn")
    ax.plot(df["Temperature (K)"], df["Fuel oxygen potential - CALPHAD (KJ/mol)"], "-", label="CALPHAD stored")
    ax.plot(
        df["Temperature (K)"],
        df["Reconstructed CALPHAD oxygen potential (kJ/mol)"],
        ":",
        label="CALPHAD reconstructed",
    )
    if "Standalone CALPHAD oxygen potential (kJ/mol)" in df.columns:
        ax.plot(
            df["Temperature (K)"],
            df["Standalone CALPHAD oxygen potential (kJ/mol)"],
            "-.",
            label="Standalone OC",
        )
    ax.set_xlabel("Temperature (K)")
    ax.set_ylabel("Oxygen potential (kJ/mol)")
    ax.set_title(f"{case_name}: oxygen potential verification")
    ax.legend()
    fig.tight_layout()
    fig.savefig(VERIFICATION_PLOTS_DIR / f"{case_name}_potential_verification.png")
    plt.close(fig)

    fig, ax = plt.subplots(figsize=(9, 5))
    ax.plot(df["Temperature (K)"], df["Blackburn minus CALPHAD potential (kJ/mol)"])
    ax.set_xlabel("Temperature (K)")
    ax.set_ylabel("Blackburn - CALPHAD (kJ/mol)")
    ax.set_title(f"{case_name}: oxygen potential difference")
    fig.tight_layout()
    fig.savefig(VERIFICATION_PLOTS_DIR / f"{case_name}_potential_difference.png")
    plt.close(fig)


def run_verification(case_dirs: list[Path]):
    VERIFICATION_DIR.mkdir(exist_ok=True)

    summary_rows = []
    report_lines = [
        "# U-O pO2 Verification Report",
        "",
        "This report checks whether the CALPHAD values stored by SCIANTIX are consistent with the OpenCalphad output.",
        "",
        "## Verification logic",
        "",
        "1. Read `output_chemistry.txt` from each comparison case.",
        "2. Parse `oc_debug.txt` and extract, for each OpenCalphad call:",
        "   - input `T`, `P`, `x(O)`",
        "   - component line for `O`: moles, mole fraction, `mu/RT`, activity",
        f"3. Reconstruct the CALPHAD oxygen pressure with `pO2 = {REFERENCE_OXYGEN_PRESSURE_MPA} * activity(O)^2` MPa.",
        "4. Reconstruct the oxygen potential with `mu = RT ln(pO2/p_ref)`.",
        "5. Compare reconstructed values with SCIANTIX stored CALPHAD variables.",
        "",
        "## Interpretation",
        "",
        "- Agreement between reconstructed CALPHAD and stored CALPHAD means the coupling implementation is internally consistent.",
        "- Large Blackburn vs CALPHAD differences can still be physical/model differences.",
        "- Disagreement between stored and reconstructed CALPHAD indicates an OC parsing or mapping issue.",
        "",
    ]

    all_frames = []
    for case_dir in case_dirs:
        output_path = case_dir / "output_chemistry.txt"
        debug_path = case_dir / "oc_debug.txt"
        if not output_path.exists() or not debug_path.exists():
            continue

        output_df = pd.read_csv(output_path, sep="\t")
        oc_df, invalid_equilibrium_count = parse_oc_debug(debug_path)
        if oc_df.empty:
            continue
        standalone_df, standalone_invalid_count = run_standalone_oc_case(case_dir, output_df)

        n = min(len(output_df), len(oc_df), len(standalone_df))
        merged = pd.concat(
            [
                output_df.iloc[:n].reset_index(drop=True),
                oc_df.iloc[:n].reset_index(drop=True),
                standalone_df.iloc[:n].reset_index(drop=True).drop(columns=["Temperature (K)", "Pressure (Pa)", "Input x(O)"]),
            ],
            axis=1,
        )
        merged["Case"] = case_dir.name
        merged = add_derived_columns(merged)
        merged.to_csv(case_dir / "verification_table.tsv", sep="\t", index=False)
        plot_verification_case(case_dir.name, merged)
        all_frames.append(merged)

        summary_rows.append(
            {
                "Case": case_dir.name,
                "points": len(merged),
                "max x(O) abs error": merged["x(O) abs error"].max(),
                "max CALPHAD pO2 relative error": merged["CALPHAD pO2 abs rel error"].dropna().max(),
                "max CALPHAD potential abs error (kJ/mol)": merged["CALPHAD potential abs error (kJ/mol)"].max(),
                "median |Blackburn-CALPHAD potential| (kJ/mol)": merged[
                    "Blackburn minus CALPHAD potential (kJ/mol)"
                ].abs().median(),
                "median Blackburn/CALPHAD pO2 ratio": merged["Blackburn/CALPHAD pO2 ratio"].dropna().median(),
                "invalid OC equilibria": invalid_equilibrium_count,
                "invalid standalone OC equilibria": standalone_invalid_count,
                "max standalone CALPHAD pO2 relative error": merged["Standalone CALPHAD pO2 abs rel error"].dropna().max(),
                "max standalone CALPHAD potential abs error (kJ/mol)": merged[
                    "Standalone CALPHAD potential abs error (kJ/mol)"
                ].max(),
            }
        )

        report_lines.extend(
            [
                f"## Case `{case_dir.name}`",
                "",
                f"- Points compared: {len(merged)}",
                f"- Max `x(O)` input consistency error: `{merged['x(O) abs error'].max():.3e}`",
                f"- Max CALPHAD `pO2` relative error vs reconstructed OC value: `{merged['CALPHAD pO2 abs rel error'].dropna().max():.3e}`",
                f"- Max CALPHAD oxygen-potential absolute error: `{merged['CALPHAD potential abs error (kJ/mol)'].max():.3e}` kJ/mol",
                f"- Median `|mu_Blackburn - mu_CALPHAD|`: `{merged['Blackburn minus CALPHAD potential (kJ/mol)'].abs().median():.3f}` kJ/mol",
                f"- Median `pO2_Blackburn / pO2_CALPHAD`: `{merged['Blackburn/CALPHAD pO2 ratio'].dropna().median():.3e}`",
                f"- Invalid OpenCalphad equilibria detected: `{invalid_equilibrium_count}`",
                f"- Invalid standalone OpenCalphad equilibria detected: `{standalone_invalid_count}`",
                f"- Max stored-CALPHAD vs standalone-OC `pO2` relative error: `{merged['Standalone CALPHAD pO2 abs rel error'].dropna().max():.3e}`",
                f"- Max stored-CALPHAD vs standalone-OC oxygen-potential error: `{merged['Standalone CALPHAD potential abs error (kJ/mol)'].max():.3e}` kJ/mol",
                "",
            ]
        )

    summary_df = pd.DataFrame(summary_rows)
    summary_df.to_csv(VERIFICATION_DIR / "verification_summary.tsv", sep="\t", index=False)
    (VERIFICATION_DIR / "verification_report.md").write_text("\n".join(report_lines))

    if all_frames:
        combined = pd.concat(all_frames, ignore_index=True)
        fig, ax = plt.subplots(figsize=(9, 5))
        for case_name, group in combined.groupby("Case"):
            ax.plot(group["Temperature (K)"], group["Blackburn minus CALPHAD potential (kJ/mol)"], label=case_name)
        ax.set_xlabel("Temperature (K)")
        ax.set_ylabel("Blackburn - CALPHAD (kJ/mol)")
        ax.set_title("Global oxygen-potential difference across cases")
        ax.legend()
        fig.tight_layout()
        VERIFICATION_PLOTS_DIR.mkdir(parents=True, exist_ok=True)
        fig.savefig(VERIFICATION_PLOTS_DIR / "global_potential_difference.png")
        plt.close(fig)


def main():
    ensure_binary()

    if not LOCAL_BINARY.exists():
        raise FileNotFoundError("sciantix.x not found in the test folder and no default build binary is available.")

    case_dirs = []
    for stoc in STOICHIOMETRY:
        case_dir = SCRIPT_DIR / str(stoc).replace(".", "_")
        case_dirs.append(case_dir)
        modify_input_initial_conditions(stoc)

        needs_run = (
            not case_dir.exists() or
            not (case_dir / "output_chemistry.txt").exists() or
            not (case_dir / "oc_debug.txt").exists()
        )

        if needs_run:
            case_dir.mkdir(parents=True, exist_ok=True)
            RESULTS_DIR.mkdir(exist_ok=True)
            run_sciantix(case_dir)
            plot_comparison_case(RESULTS_DIR)
            for file_name in os.listdir(RESULTS_DIR):
                shutil.move(RESULTS_DIR / file_name, case_dir / file_name)
            RESULTS_DIR.rmdir()
        else:
            plot_comparison_case(case_dir)

    run_verification(sorted(case_dirs, key=case_sort_key))

if __name__ == "__main__":
    main()
