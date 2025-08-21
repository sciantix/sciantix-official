import subprocess
import shutil
import os 
import plot_thermochemistry

# Path to input settings file
input_file = "input_settings.txt"
input_initial = "input_initial_conditions.txt"
results_dir = "results"
plot_only = False
# Function to modify input settings
def modify_input_settings(value):
    with open(input_file, 'r') as file:
        lines = file.readlines()
    
    lines[25] = value + lines[25][1:]
    
    with open(input_file, 'w') as file:
        file.writelines(lines)

def modify_input_initial_conditions(value):
    with open(input_initial, 'r') as file:
        lines = file.readlines()
    
    lines[24] = value + '\n'

    with open(input_initial, 'w') as file:
        file.writelines(lines)

# Function to run sciantix and save output
def run_sciantix(output_name):
    subprocess.run("./sciantix.x", shell=True, check=True)
    shutil.move("thermochemistry_output.txt", f"{results_dir}/thermochemistry_{output_name}")
    shutil.move("output.txt", f"{results_dir}/{output_name}")
    shutil.move("input_check.txt", f"{results_dir}/input_check_{output_name}")

shutil.copy("../../build/sciantix.x", ".")

stoichiometry = [-1.99, -1.0, -0.5, -0.2, 0.0 , 0.2, 0.5 , 1.0 , 1.99]

for i, stoc in enumerate(stoichiometry):
    print("----------------------------"+str(stoc)+"----------------------------------------")
    modify_input_initial_conditions(str(stoc))
    if plot_only == False:
        run_sciantix("output_chemistry.txt")
        plot_thermochemistry.plot(results_dir)
        folder_name = str(stoc).replace('.', '_')
        os.makedirs(folder_name, exist_ok=True)
        for file in os.listdir(results_dir):
            if file.endswith(".png") or file.endswith(".txt"):
                shutil.move(os.path.join(results_dir, file), os.path.join(folder_name, file))
    elif plot_only == True:
        folder_name = str(stoc).replace('.', '_')
        plot_thermochemistry.plot(folder_name)
