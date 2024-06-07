import os
import random
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
  return int(j[0])

class singleSensitivityAnalysis():
    def __init__(self):
        self.current_folder = os.getcwd()
        self.file_name = 'input_scaling_factors.txt'
        self.file_path = os.path.join(self.current_folder, self.file_name)

    def readFile_inputScalingFactors(self):

        print(f"\nRunning: readFile_inputScalingFactors")
        print(f"Looking for input_scaling_factors.txt file in {os.getcwd()}")

        self.scaling_factors = {}
        with open(self.file_path, 'r') as file:
            lines = file.readlines()
            i = 0
            while i < len(lines):
                value = float(lines[i].strip())
                name = lines[i + 1].strip()[len("# scaling factor - "):]
                self.scaling_factors[name] = value
                i += 2

        print("\nScaling factor dictionary:")
        print(self.scaling_factors)

    def setSensitivityParameters(self):
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
        self.deviation = float(input("Enter the desider standard deviation: "))

    def execute_sensitivityAnalysis(self):
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

            bias = random.uniform(1 - self.deviation, 1 + self.deviation)
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

    def readFolders(self, variable_name):

        self.variable_name = variable_name
        self.variable_value = np.zeros(self.sample_number)
        self.scaling_factor_value = np.zeros(self.sample_number)

        for j in range(self.sample_number):
            
            # changing folder
            folder_name = f"case_{j+1}"
            os.chdir(folder_name)

            # reading scaling factors
            scaling_factors = {}
            self.file_path = os.path.join(os.getcwd(), self.file_name)
            with open(self.file_path, 'r') as file:
                lines = file.readlines()
                i = 0
                while i < len(lines):
                    value = float(lines[i].strip())
                    name = lines[i + 1].strip()[len("# scaling factor - "):]
                    scaling_factors[name] = value
                    i += 2

            self.scaling_factor_value[j] = scaling_factors[self.bias_name]

            # reading output.txt
            data = np.genfromtxt('output.txt', dtype= 'str', delimiter='\t')
            
            variable_position = findSciantixVariablePosition(data, variable_name)
            self.variable_value[j] = (data[-1,variable_position].astype(float))

            os.chdir('..')

        print(self.scaling_factor_value)

    def sensitivityCoefficient(self):
        self.sensitivity_coefficient = np.zeros(self.sample_number)

        # Reference value
        folder_name = f"case_0"
        os.chdir(folder_name)  
        data = np.genfromtxt('output.txt', dtype= 'str', delimiter='\t')
        variable_position = findSciantixVariablePosition(data, self.variable_name)
        ref_variable_value = (data[-1,variable_position].astype(float))

        os.chdir('..')

        for i in range(self.sample_number):
            
            folder_name = f"case_{i+1}"
            os.chdir(folder_name)  

            data = np.genfromtxt('output.txt', dtype= 'str', delimiter='\t')
            
            variable_position = findSciantixVariablePosition(data, self.variable_name)
            self.variable_value[i] = (data[-1,variable_position].astype(float))

            self.sensitivity_coefficient[i] = ((1/ref_variable_value)*((ref_variable_value - self.variable_value[i])/(1 - self.scaling_factor_value[i])))

            os.chdir('..')

    def plot_sensitivityAnalysis(self):
        fig, ax = plt.subplots(1,2)

        ax[0].scatter(self.scaling_factor_value, self.variable_value, c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20)
        ax[0].set_xlabel(self.bias_name)
        ax[0].set_ylabel(self.variable_name)

        ax[1].scatter(self.scaling_factor_value, self.sensitivity_coefficient, c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20)
        ax[1].set_xlabel(self.bias_name)
        ax[1].set_ylabel("Sensitivity coefficient")

        plt.show()

    def removeFolders(self):
        for i in range(self.sample_number+1):
            folder_name = f"case_{i}"
            shutil.rmtree(folder_name)

# single analysis
scaling_factors = singleSensitivityAnalysis()
scaling_factors.readFile_inputScalingFactors()
scaling_factors.setSensitivityParameters()
scaling_factors.execute_sensitivityAnalysis()
scaling_factors.readFolders("Intragranular gas bubble swelling (/)")
scaling_factors.sensitivityCoefficient()
scaling_factors.plot_sensitivityAnalysis()
scaling_factors.removeFolders()
