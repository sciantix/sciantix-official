import subprocess
import shutil

# Path to input settings file
input_file = "input_settings.txt"
output_file = "output.txt"
results_dir = "results"

# Function to modify input settings
def modify_input_settings(value):
    with open(input_file, 'r') as file:
        lines = file.readlines()
    
    # Modify line 22 (index23) by replacing the first character
    lines[23] = value + lines[23][1:]
    
    with open(input_file, 'w') as file:
        file.writelines(lines)

# Function to run sciantix and save output
def run_sciantix(output_name):
    subprocess.run("./sciantix.x", shell=True, check=True)
    shutil.move(output_file, f"{results_dir}/{output_name}")

shutil.copy("../../build/sciantix.x", ".")

# Run with no thermochemistry
modify_input_settings("0")
run_sciantix("output_nochemistry.txt")

# Run with thermochemistry
modify_input_settings("3")
run_sciantix("output_chemistry.txt")

# Run thermochemistry script
subprocess.run("python3 plot_thermochemistry.py", shell=True, check=True)