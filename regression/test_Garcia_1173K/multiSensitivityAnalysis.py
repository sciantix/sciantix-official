import os
import random
import shutil
import numpy as np
import matplotlib.pyplot as plt

def findSciantixVariablePosition(output, variable_name):
    """
    This function gets the output.txt file and the variable name,
    giving back its column index in the ndarray.
    """
    i, j = np.where(output == variable_name)
    return int(j)

class multiSensitivityAnalysis():
    def __init__(self):
        self.current_folder = os.getcwd()
        self.file_name = 'input_scaling_factors.txt'
        self.file_path = os.path.join(self.current_folder, self.file_name)
        self.scaling_factors = {}
        self.bias_names = []  # list of scaling factors to vary
        self.sample_number = 0
        self.deviation = 0.0

    def readFile_inputScalingFactors(self):
        print(f"\nRunning: readFile_inputScalingFactors")
        print(f"Looking for {self.file_name} in {os.getcwd()}")
        self.scaling_factors = {}
        with open(self.file_path, 'r') as file:
            lines = file.readlines()
            i = 0
            while i < len(lines):
                value = float(lines[i].strip())
                name = lines[i+1].strip()[len("# scaling factor - "):]
                self.scaling_factors[name] = value
                i += 2
        print("\nScaling factor dictionary:")
        print(self.scaling_factors)

    def setSensitivityParameters(self):
        print("\nSCIANTIX uncertainty analysis (Multiple Factors):")
        print("------------------------------")
        print("Available scaling factors:", self.scaling_factors)
        while True:
            input_str = input("Enter the names of the scaling factors to vary (separated by commas): ")
            factors = [f.strip() for f in input_str.split(',')]
            invalid = [f for f in factors if f not in self.scaling_factors]
            if invalid:
                print(f"The following keys do not exist in the scaling factors: {invalid}. Please try again.")
            else:
                self.bias_names = factors
                break
        self.sample_number = int(input("Enter the number of samplings: "))
        while True:
            input_devs = input(f"Enter the standard deviations for {', '.join(self.bias_names)} (comma-separated, e.g., 0.1, 0.2): ")
            devs = [d.strip() for d in input_devs.split(',')]

            if len(devs) != len(self.bias_names):
                print("The number of standard deviations must match the number of scaling factors. Please try again.")
                continue

            try:
                self.deviations = {factor: float(dev) for factor, dev in zip(self.bias_names, devs)}
                break
            except ValueError:
                print("Invalid input. Ensure all values are numerical and try again.")

        print("\nAssigned standard deviations:")
        for factor, dev in self.deviations.items():
            print(f"{factor}: {dev}")

    def execute_sensitivityAnalysis(self):
        # For each simulation case, modify each selected scaling factor with a random bias.
        for i in range(self.sample_number):
            folder_name = f"case_{i+1}"
            if os.path.exists(folder_name):
                shutil.rmtree(folder_name)
            os.makedirs(folder_name)
            print(f"Folder '{folder_name}' created.")
            
            # Copy necessary files to the case folder
            for file_to_copy in ["sciantix.x", "input_initial_conditions.txt", "input_settings.txt", "input_history.txt"]:
                shutil.copy(file_to_copy, folder_name)
            
            # Update scaling factors for the selected factors
            for bias_name in self.bias_names:
                # Generate random bias using individual deviation for each factor
                deviation = self.deviations[bias_name]  # Get specific deviation for this factor
                bias = random.uniform(1 - deviation, 1 + deviation)
                self.scaling_factors[bias_name] = bias
            
            # Write the updated scaling factors to file in the case folder
            case_file_path = os.path.join(folder_name, self.file_name)
            with open(case_file_path, 'w') as file:
                for key, value in self.scaling_factors.items():
                    file.write(f"{value}\n")
                    file.write(f"# scaling factor - {key}\n")
            
            # Run the simulation
            current_dir = os.getcwd()
            os.chdir(folder_name)
            os.system("./sciantix.x")
            os.chdir(current_dir)
        
        # Execute reference simulation (case_0) with baseline scaling factors.
        folder_name = "case_0"
        if os.path.exists(folder_name):
            shutil.rmtree(folder_name)
        os.makedirs(folder_name)
        print(f"Reference folder '{folder_name}' created.")
        for file_to_copy in ["sciantix.x", "input_initial_conditions.txt", "input_settings.txt", "input_history.txt"]:
            shutil.copy(file_to_copy, folder_name)
        current_dir = os.getcwd()
        os.chdir(folder_name)
        os.system("./sciantix.x")
        os.chdir(current_dir)

    def readFolders(self, variable_name):
        self.variable_name = variable_name
        self.variable_value = np.zeros(self.sample_number)
        # Dictionary to store scaling factor values for each varied factor.
        self.scaling_factor_values = {bias_name: np.zeros(self.sample_number) for bias_name in self.bias_names}
        
        for j in range(self.sample_number):
            folder_name = f"case_{j+1}"
            os.chdir(folder_name)
            # Read scaling factors from file.
            scaling_factors = {}
            with open(self.file_name, 'r') as file:
                lines = file.readlines()
                i = 0
                while i < len(lines):
                    value = float(lines[i].strip())
                    name = lines[i+1].strip()[len("# scaling factor - "):]
                    scaling_factors[name] = value
                    i += 2
            for bias_name in self.bias_names:
                self.scaling_factor_values[bias_name][j] = scaling_factors[bias_name]
            
            # Read output.txt and extract the final value of the variable.
            data = np.genfromtxt('output.txt', dtype='str', delimiter='\t')
            variable_position = findSciantixVariablePosition(data, variable_name)
            self.variable_value[j] = float(data[-1, variable_position])
            os.chdir('..')
        
        print("Scaling factor values for varied factors:")
        for bias_name in self.bias_names:
            print(f"{bias_name}: {self.scaling_factor_values[bias_name]}")

    def sensitivityCoefficient(self):
        # Get the reference output value from case_0.
        folder_name = "case_0"
        os.chdir(folder_name)
        data = np.genfromtxt('output.txt', dtype='str', delimiter='\t')
        variable_position = findSciantixVariablePosition(data, self.variable_name)
        ref_variable_value = float(data[-1, variable_position])
        os.chdir('..')
        
        # Build the design matrix X and response vector Y.
        # For each simulation, dY = ref_variable_value - simulation_value.
        # For each varied factor, dX = 1 - (applied scaling factor value) assuming baseline = 1.
        X = np.column_stack([1 - self.scaling_factor_values[bias_name] for bias_name in self.bias_names])
        Y = ref_variable_value - self.variable_value
        # Solve for sensitivity coefficients (beta) with no intercept.
        beta, residuals, rank, s = np.linalg.lstsq(X, Y, rcond=None)
        self.sensitivity_coefficients = {bias_name: beta[i] for i, bias_name in enumerate(self.bias_names)}
        print("\nSensitivity coefficients (normalized):")
        for bias_name, coeff in self.sensitivity_coefficients.items():
            print(f"{bias_name}: {coeff}")

    def plot_sensitivityAnalysis(self):
        # Create a scatter plot for each varied scaling factor versus the output variable.
        num_factors = len(self.bias_names)
        fig, axs = plt.subplots(num_factors, 1, figsize=(6, 4 * num_factors))
        if num_factors == 1:
            axs = [axs]
        
        # Get reference simulation output for annotation.
        folder_name = "case_0"
        os.chdir(folder_name)
        data = np.genfromtxt('output.txt', dtype='str', delimiter='\t')
        variable_position = findSciantixVariablePosition(data, self.variable_name)
        ref_variable_value = float(data[-1, variable_position])
        os.chdir('..')
        
        for i, bias_name in enumerate(self.bias_names):
            ax = axs[i]
            x = self.scaling_factor_values[bias_name]
            y = self.variable_value
            ax.scatter(x, y, c='#98E18D', edgecolors='#999AA2', marker='o', s=20)
            ax.set_xlabel(bias_name)
            ax.set_ylabel(self.variable_name)
            ax.set_title(f"Scatter plot for {bias_name}\nSensitivity Coefficient: {self.sensitivity_coefficients[bias_name]:.4f}")
            # Draw a horizontal reference line at the baseline output value.
            ax.axhline(ref_variable_value, color='red', linestyle='--', label='Reference Output')
            ax.set_yscale('log')
            ax.legend()
        plt.tight_layout()
        plt.show()

        # If exactly two scaling factors are varied, produce an additional 3D scatter plot.
        if len(self.bias_names) == 2:
            from mpl_toolkits.mplot3d import Axes3D  # Importing for 3D plotting
            fig3d = plt.figure(figsize=(8, 6))
            ax3d = fig3d.add_subplot(111, projection='3d')
            x3d = self.scaling_factor_values[self.bias_names[0]]
            y3d = self.scaling_factor_values[self.bias_names[1]]
            z3d = self.variable_value
            
            # Create the 3D scatter plot.
            sc = ax3d.scatter(x3d, y3d, z3d, c=z3d, cmap='viridis', marker='o', s=50)
            ax3d.set_xlabel(self.bias_names[0])
            ax3d.set_ylabel(self.bias_names[1])
            ax3d.set_zlabel(self.variable_name)
            ax3d.set_title("3D Scatter Plot")
            fig3d.colorbar(sc, ax=ax3d, label=self.variable_name)
            plt.tight_layout()
            plt.show()
            
    def removeFolders(self):
        for i in range(self.sample_number + 1):
            folder_name = f"case_{i}"
            shutil.rmtree(folder_name)

# Usage:
analysis = multiSensitivityAnalysis()
analysis.readFile_inputScalingFactors()
analysis.setSensitivityParameters()
analysis.execute_sensitivityAnalysis()
analysis.readFolders("He fractional release (/)")
analysis.sensitivityCoefficient()
analysis.plot_sensitivityAnalysis()
analysis.removeFolders()
