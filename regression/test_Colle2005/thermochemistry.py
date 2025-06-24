import subprocess
import shutil
import os 

# Path to input settings file
input_file = "input_settings.txt"
results_dir = "results"

# Function to modify input settings
def modify_input_settings(value):
    with open(input_file, 'r') as file:
        lines = file.readlines()
    
    # Modify line 22 (index23) by replacing the first character
    lines[25] = value + lines[25][1:]
    
    with open(input_file, 'w') as file:
        file.writelines(lines)

# Function to run sciantix and save output
def run_sciantix(output_name):
    subprocess.run("./sciantix.x", shell=True, check=True)
    shutil.move("thermochemistry_output.txt", f"{results_dir}/thermochemistry_{output_name}")
    shutil.move("output.txt", f"{results_dir}/{output_name}")
    shutil.move("input_check.txt", f"{results_dir}/input_check_{output_name}")

shutil.copy("../../build/sciantix.x", ".")

# Run with no thermochemistry
modify_input_settings("0")
run_sciantix("output_nochemistry.txt")

# Run with thermochemistry
modify_input_settings("2")
run_sciantix("output_chemistry.txt")

# Run thermochemistry script
subprocess.run("python3 plot_thermochemistry.py", shell=True, check=True)

folder_name = input("Folder name: ").strip()

if folder_name:
    os.makedirs(folder_name, exist_ok=True)
    for file in os.listdir(results_dir):
        if file.endswith(".png") or file.endswith(".txt"):
            shutil.move(os.path.join(results_dir, file), os.path.join(folder_name, file))
    print(f"Results in '{folder_name}'.")
else:
    print("Results in the 'results' directory.")