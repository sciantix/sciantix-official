import os
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

SCRIPT_DIR = Path(__file__).resolve().parent
INPUT_INITIAL = SCRIPT_DIR / "input_initial_conditions.txt"
RESULTS_DIR = SCRIPT_DIR / "results"
STOICHIOMETRY = [-0.1, -0.05, -0.01, 0.0, 0.01, 0.05, 0.1]


def modify_input_initial_conditions(value):
    with open(INPUT_INITIAL, "r") as file:
        lines = file.readlines()

    lines[24] = f"{value}\n"

    with open(INPUT_INITIAL, "w") as file:
        file.writelines(lines)


def run_sciantix(output_name):
    result = subprocess.run(
        [str(LOCAL_BINARY)],
        cwd=SCRIPT_DIR,
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        if result.stdout:
            print(result.stdout, end="")
        if result.stderr:
            print(result.stderr, end="")
        raise subprocess.CalledProcessError(result.returncode, result.args)
    shutil.move(SCRIPT_DIR / "thermochemistry_output.txt", RESULTS_DIR / f"thermochemistry_{output_name}")
    shutil.move(SCRIPT_DIR / "output.txt", RESULTS_DIR / output_name)
    shutil.move(SCRIPT_DIR / "input_check.txt", RESULTS_DIR / f"input_check_{output_name}")


def plot_case(folder_path):
    data = pd.read_csv(Path(folder_path) / "output_chemistry.txt", sep="\t")

    xdev = data["Stoichiometry deviation (/)"].iloc[0]
    oxygen_fraction = (2 + xdev) / (3 + xdev)
    temperature = data["Temperature (K)"]

    blackburn_po2 = data["Fuel oxygen partial pressure - Blackburn (MPa)"]
    calphad_po2 = data["Fuel oxygen partial pressure - CALPHAD (MPa)"]
    active_po2 = data["Fuel oxygen partial pressure (MPa)"]
    use_log_scale = np.any(blackburn_po2 > 0.0) or np.any(calphad_po2 > 0.0) or np.any(active_po2 > 0.0)

    plt.figure(figsize=(10, 6))
    if use_log_scale:
        plt.semilogy(temperature, blackburn_po2, "--", label="Blackburn")
        plt.semilogy(temperature, calphad_po2, "-", label="CALPHAD")
        plt.semilogy(temperature, active_po2, ":", label="Active in SCIANTIX")
    else:
        plt.plot(temperature, blackburn_po2, "--", label="Blackburn")
        plt.plot(temperature, calphad_po2, "-", label="CALPHAD")
        plt.plot(temperature, active_po2, ":", label="Active in SCIANTIX")
    plt.xlabel("Temperature (K)")
    plt.ylabel("Oxygen partial pressure (MPa)")
    plt.title(f"O/U comparison, x = {xdev:.2f}, X(O) = {oxygen_fraction:.3f}")
    plt.legend()
    plt.tight_layout()
    plt.savefig(Path(folder_path) / "pO2_comparison.png")
    plt.close()

    plt.figure(figsize=(10, 6))
    plt.plot(temperature, data["Fuel oxygen potential - Blackburn (KJ/mol)"], "--", label="Blackburn")
    plt.plot(temperature, data["Fuel oxygen potential - CALPHAD (KJ/mol)"], "-", label="CALPHAD")
    plt.plot(temperature, data["Fuel oxygen potential (KJ/mol)"], ":", label="Active in SCIANTIX")
    plt.xlabel("Temperature (K)")
    plt.ylabel("Oxygen potential (kJ/mol)")
    plt.title(f"O/U comparison, x = {xdev:.2f}, X(O) = {oxygen_fraction:.3f}")
    plt.legend()
    plt.tight_layout()
    plt.savefig(Path(folder_path) / "oxygen_potential_comparison.png")
    plt.close()

    summary = pd.DataFrame(
        {
            "Temperature (K)": temperature,
            "Stoichiometry deviation (/)": data["Stoichiometry deviation (/)"],
            "Fuel oxygen partial pressure - Blackburn (MPa)": data["Fuel oxygen partial pressure - Blackburn (MPa)"],
            "Fuel oxygen partial pressure - CALPHAD (MPa)": data["Fuel oxygen partial pressure - CALPHAD (MPa)"],
            "Fuel oxygen potential - Blackburn (KJ/mol)": data["Fuel oxygen potential - Blackburn (KJ/mol)"],
            "Fuel oxygen potential - CALPHAD (KJ/mol)": data["Fuel oxygen potential - CALPHAD (KJ/mol)"],
        }
    )
    summary.to_csv(Path(folder_path) / "oxygen_comparison_summary.txt", sep="\t", index=False)


LOCAL_BINARY = SCRIPT_DIR / "sciantix.x"
DEFAULT_BUILD_BINARY = SCRIPT_DIR.parent.parent / "build" / "sciantix.x"

if DEFAULT_BUILD_BINARY.exists() and not LOCAL_BINARY.exists():
    shutil.copy(DEFAULT_BUILD_BINARY, LOCAL_BINARY)

for stoc in STOICHIOMETRY:
    folder_name = SCRIPT_DIR / str(stoc).replace(".", "_")
    modify_input_initial_conditions(stoc)

    if folder_name.exists():
        plot_case(folder_name)
        continue

    if not LOCAL_BINARY.exists():
        print(f"Skipping {folder_name.name}: no precomputed results found and sciantix.x is missing.")
        continue

    os.makedirs(RESULTS_DIR, exist_ok=True)
    run_sciantix("output_chemistry.txt")
    plot_case(RESULTS_DIR)
    os.makedirs(folder_name, exist_ok=True)
    for file_name in os.listdir(RESULTS_DIR):
        shutil.move(RESULTS_DIR / file_name, folder_name / file_name)
