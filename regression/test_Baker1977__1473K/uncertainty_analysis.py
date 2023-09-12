import os
import random
import csv
import subprocess
import shutil
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def findSciantixVariablePosition(output, variable_name):
  """
  This function gets the output.txt file and the variable name,
  giving back its column index in the ndarray
  """
  i,j = np.where(output == variable_name)
  return int(j)

class uncertaintyAnalysis():
    def __init__(self):
        self.current_folder = os.getcwd()
        self.file_name = 'input_scaling_factors.txt'
        self.file_path = os.path.join(self.current_folder, self.file_name)

    def readFile_inputScalingFactors(self):
        self.scaling_factors = {}
        with open(self.file_path, 'r') as file:
            lines = file.readlines()
            i = 0
            while i < len(lines):
                value = float(lines[i].strip())
                name = lines[i + 1].strip()[len("# scaling factor - "):]
                self.scaling_factors[name] = value
                i += 2

    def exectute_inputScalingFactors(self):

        print("\nSCIANTIX uncertainty analysis:")
        print("------------------------------")
        print("Given these scaling factors:", self.scaling_factors)

        while True:
            self.bias_name = str(input("Enter the name of the scaling factor to vary: "))

            if self.bias_name in self.scaling_factors:
                print(f"The key '{self.bias_name}' exists in the dictionary.")
                break
            else:
                print(f"The key '{self.bias_name}' does not exist in the dictionary. Please try again.")

        self.sample_number = int(input("Enter the number of samplings: "))
        deviation = float(input("Enter the desider standard deviation: "))

        self.scaling_factor_value = np.zeros(self.sample_number)

        for i in range(self.sample_number):

            ####################
            # Generating folders
            ####################

            folder_name = f"case_{i+1}"

            if not os.path.exists(folder_name):
                os.makedirs(folder_name)
                print(f"Folder '{folder_name}' created.")
            else:
                shutil.rmtree(folder_name)
                os.makedirs(folder_name)
                print(f"Folder '{folder_name}' already exists. Re-created.")

            os.chdir(folder_name)  
            shutil.copy("../sciantix.x", os.getcwd())
            shutil.copy("../input_initial_conditions.txt", os.getcwd())
            shutil.copy("../input_settings.txt", os.getcwd())
            shutil.copy("../input_history.txt", os.getcwd())

            bias = random.uniform(1 - deviation, 1 + deviation)

            self.scaling_factor_value[i] = bias

            self.scaling_factors[self.bias_name] = bias

            with open("input_scaling_factors.txt", 'w') as file:

                for key, value in self.scaling_factors.items():
                    file.write(f'{value}\n')
                    file.write(f'# scaling factor - {key}\n')

            os.system("./sciantix.x")
            os.chdir('..')

        ##################
        # Reference folder
        ##################

        folder_name = f"case_0"

        if not os.path.exists(folder_name):
            os.makedirs(folder_name)
            print(f"Folder '{folder_name}' created.")
        else:
            shutil.rmtree(folder_name)
            os.makedirs(folder_name)
            print(f"Folder '{folder_name}' already exists. Re-created.")

        os.chdir(folder_name)  
        shutil.copy("../sciantix.x", os.getcwd())
        shutil.copy("../input_initial_conditions.txt", os.getcwd())
        shutil.copy("../input_settings.txt", os.getcwd())
        shutil.copy("../input_history.txt", os.getcwd())

        os.system("./sciantix.x")
        os.chdir('..')

    def readFolders_inputScalingFactors(self, variable_name):

        self.variable_name = variable_name
        self.variable_value = np.zeros(self.sample_number)

        for i in range(self.sample_number):
            
            folder_name = f"case_{i+1}"
            os.chdir(folder_name)  

            data = np.genfromtxt('output.txt', dtype= 'str', delimiter='\t')
            
            variable_position = findSciantixVariablePosition(data, variable_name)
            self.variable_value[i] = (100*data[-1,variable_position].astype(float))

            os.chdir('..')

    def plot_inputScalingFactors(self):
        fig, ax = plt.subplots()

        ax.scatter(self.scaling_factor_value, self.variable_value, c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20, label='ggg')

        ax.set_xlabel(self.bias_name)
        ax.set_ylabel(self.variable_name)
        ax.legend()

        plt.show()

    def sensitivityCoefficient(self):
        self.sensitivity_coefficient = np.zeros(self.sample_number)

        # Reference value
        folder_name = f"case_0"
        os.chdir(folder_name)  
        data = np.genfromtxt('output.txt', dtype= 'str', delimiter='\t')
        variable_position = findSciantixVariablePosition(data, self.variable_name)
        ref_variable_value = (100*data[-1,variable_position].astype(float))

        os.chdir('..')

        for i in range(self.sample_number):
            
            folder_name = f"case_{i+1}"
            os.chdir(folder_name)  

            data = np.genfromtxt('output.txt', dtype= 'str', delimiter='\t')
            
            variable_position = findSciantixVariablePosition(data, self.variable_name)
            self.variable_value[i] = (100*data[-1,variable_position].astype(float))

            self.sensitivity_coefficient[i] = ((1/ref_variable_value)*((ref_variable_value - self.variable_value[i])/(1 - self.scaling_factor_value[i])))

            os.chdir('..')

    def plot_sensitivityCoefficient(self):
        fig, ax = plt.subplots()

        ax.scatter(self.scaling_factor_value, self.sensitivity_coefficient, c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20, label='ggg')

        ax.set_xlabel(self.bias_name)
        ax.set_ylabel("Sensitivity coefficient")
        ax.legend()

        plt.show()

    def removeFolders(self):
        for i in range(self.sample_number+1):
            folder_name = f"case_{i}"

        shutil.rmtree(folder_name)
        os.makedirs(folder_name)
        print(f"Folder '{folder_name}' already exists. Re-created.")

    def validiationCase_sensitivity(self, validation_name, bias_name, deviation, sample_number):
        files_and_dirs = os.listdir(os.getcwd())
        sorted_files_and_dirs = sorted(files_and_dirs)

        self.scaling_factor_map = np.zeros((sample_number, 9))

        for file in sorted_files_and_dirs:
            
            if validation_name in file:
                
                print(file)
                # os.chdir(file)

                bias = random.uniform(1 - deviation, 1 + deviation)
                self.scaling_factors[bias_name] = bias
                
                os.remove("input_scaling_factors.txt")
                with open("input_scaling_factors.txt", 'w') as file:

                    for key, value in self.scaling_factors.items():
                        file.write(f'{value}\n')
                        file.write(f'# scaling factor - {key}\n')

                os.system("./sciantix.x")

            os.chdir('..')

scaling_factors = uncertaintyAnalysis()

## single analysis
# scaling_factors.readFile_inputScalingFactors()
# scaling_factors.exectute_inputScalingFactors()

# scaling_factors.readFolders_inputScalingFactors("Intragranular gas swelling (/)")
# scaling_factors.sensitivityCoefficient()

# scaling_factors.plot_inputScalingFactors()
# scaling_factors.plot_sensitivityCoefficient()

# scaling_factors.removeFolders()

scaling_factors.readFile_inputScalingFactors()
scaling_factors.validiationCase_sensitivity("Baker", "resolution rate", 0.1, 5)


