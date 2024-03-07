import os
import random
import shutil
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.stats import spearmanr

subfolder_name = "GSA_output_files"
option_runSensitivity = True
samplings = 200
validation_database = "Baker"
# validation_database = "White"
sciantix_variable = "Intragranular gas swelling (/)"

# subfolder_name = "GSA_output_files_white"
# option_runSensitivity = False
# samplings = 50
# validation_database = "White"
# sciantix_variable = "Intergranular gas swelling (/)"

def findSciantixVariablePosition(output, variable_name):
  """
  This function gets the output.txt file and the variable name,
  giving back its column index in the ndarray
  """
  i,j = np.where(output == variable_name)
  return int(j)

class globalSensitivityAnalysis():
    def __init__(self):

        self.variable_name = []
        self.bias_name = []
        self.deviation = []
        self.feature = []
        self.folder_name = []
        self.folder_number = 0
        self.sample_number = samplings

        self.current_folder = os.getcwd()
        self.file_name = 'input_scaling_factors.txt'
        self.file_path = os.path.join(self.current_folder, self.file_name)

    def readFile_inputScalingFactors(self):
        print(f"\nRunning: readFile_inputScalingFactors")
        print(f"Looking for input_scaling_factors.txt file in {os.getcwd()}")

        # Initialize a dictionary to store scaling factors
        self.scaling_factors = {}
        with open(self.file_path, 'r') as file:
            lines = file.readlines()
            i = 0
            while i < len(lines):
                # Convert the value to float and strip newline characters
                value = float(lines[i].strip())
                # Get the variable name
                name = lines[i + 1].strip()[len("# scaling factor - "):]
                # Store the variable and its corresponding value to the dictionary
                self.scaling_factors[name] = value
                i += 2

        print("\nScaling factor dictionary:")
        print(self.scaling_factors)

    def setValidationParameters(self, parameters_list):
      
        for params in parameters_list:
            variable_name = params.get("variable_name")
            validation_name = params.get("validation_name")
            bias_name = params.get("bias_name")
            deviation = params.get("deviation")
            feature = params.get("feature")

            self.bias_name.append(bias_name)
            self.deviation.append(deviation)
            self.feature.append(feature)
            self.folder_number = 0

        print(self.bias_name)

        self.validation_name = validation_name
        self.variable_name = variable_name

        files_and_directories = os.listdir(self.current_folder)
        sorted_files_and_directories = sorted(files_and_directories)

        for file in sorted_files_and_directories:
            # Count the number of folders in the current directory that contains the validation_name
            if validation_name in file and os.path.isdir(file):
                self.folder_name.append(file)
                self.folder_number = self.folder_number + 1

        print(f"\nLooking for {validation_name} database folders. {self.folder_number} folders have been found!")

        self.print_biasing()


    def print_biasing(self):
        for feature, deviation, name in zip(self.feature, self.deviation, self.bias_name):
            if feature == "above":
                print(f"{name} is biased in {1}, {1 + deviation} range")
            elif feature == "around":
                print(f"{name} is biased in {1 - deviation}, {1 + deviation} range")
            elif feature == "below":
                print(f"{name} is biased in {1 - deviation}, {1} range")
            elif feature == "log10":
                print(f"{name} is biased in {10**(-deviation)}, {10**(deviation)} range")
            elif feature == "scaled":
                print(f"{name} is biased in {1/deviation}, {1*deviation} range")
            else:
                # Handle other cases or raise an exception for unknown features
                raise ValueError(f"Unsupported feature: {feature}")

    def apply_bias(self):
        biases = []

        for feature, deviation, name in zip(self.feature, self.deviation, self.bias_name):
            if feature == "above":
                bias = random.uniform(1, 1 + deviation)
            elif feature == "around":
                bias = random.uniform(1 - deviation, 1 + deviation)
            elif feature == "below":
                bias = random.uniform(1 - deviation, 1)
            elif feature == "log10":
                bias = random.uniform(10**(-deviation), 10**(deviation))
            elif feature == "scaled":
                bias = random.uniform(1/deviation, 1*deviation)
            else:
                # Handle other cases or raise an exception for unknown features
                raise ValueError(f"Unsupported feature: {feature}")

            biases.append(bias)

        return biases
    
    def uncertaintyAnalysis(self):

        print("\nRunning: uncertaintyAnalysis")
        print(f"Collecting reference outputs and sensitivity for {self.variable_name}, biasing {self.bias_name} in {self.validation_name} database.\n")

        files_and_directories = os.listdir(self.current_folder)
        sorted_files_and_directories = sorted(files_and_directories)
        
        self.reference_value_map = np.zeros(self.folder_number)
        self.scaling_factor_map = np.zeros((self.folder_number, self.sample_number, len(self.feature)))
        self.sensitivity_coefficient_map = np.zeros((self.folder_number, len(self.feature)))
        self.variable_value_map = np.zeros((self.folder_number, self.sample_number))

        self.pearson_corr = np.zeros((self.folder_number, len(self.feature)))
        self.spearman_corr = np.zeros((self.folder_number, len(self.feature)))


        i = 0 # folder number
        for file in sorted_files_and_directories:
            if self.validation_name in file and os.path.isdir(file):
                os.chdir(file)
                print("Now in folder ", os.getcwd())
                shutil.copy("../../bin/sciantix.x", os.getcwd())

                # reference calculations
                for key in self.scaling_factors.keys():
                    self.scaling_factors[key] = 1.0

                with open("input_scaling_factors.txt", 'w') as file:
                    for key, value in self.scaling_factors.items():
                        file.write(f'{value}\n')
                        file.write(f'# scaling factor - {key}\n')

                print(f"Running reference calculations in {os.getcwd()}")
                os.system("./sciantix.x")

                data = np.genfromtxt('output.txt', dtype='str', delimiter='\t')
                variable_position = findSciantixVariablePosition(data, self.variable_name)

                self.reference_value_map[i] = data[-1,variable_position].astype(float)

                # sensitivity calculations
                for j in range(self.sample_number):

                    bias = self.apply_bias()

                    for key, value in zip(self.scaling_factors.keys(), bias):
                        self.scaling_factors[key] = value

                    with open("input_scaling_factors.txt", 'w') as file:
                        for key, value in self.scaling_factors.items():
                            file.write(f'{value}\n')
                            file.write(f'# scaling factor - {key}\n')

                    os.system("./sciantix.x")

                    # generating the map with all the scaling factors
                    self.scaling_factor_map[i,j,:] = bias

                    data = np.genfromtxt('output.txt', dtype='str', delimiter='\t')
                    variable_position = findSciantixVariablePosition(data, self.variable_name)
                    output_value = data[-1,variable_position].astype(float)
                    self.variable_value_map[i,j] = output_value

                os.chdir('..')

                # sensitivity coefficient map: one coefficient for each temperataure
                for k in range(0, len(self.feature)):

                    input = self.scaling_factor_map[i, :, k]
                    output = self.variable_value_map[i,:]

                    self.pearson_corr[i,k] = np.corrcoef(input, output)[0,1]
                    self.spearman_corr[i,k] = spearmanr(input, output)[0]


                print(f"reference value: {self.reference_value_map[i]}")
                print(f"sensitivity coefficient = {self.sensitivity_coefficient_map[i][:]}")
                print("\n")

                i = i + 1

            # print(self.reference_value_map)

    def validationCase_save(self):

        print("\nSaving validation output\n")

        # Check if the directory exists, if not then create it
        if not os.path.exists(subfolder_name):
            os.makedirs(subfolder_name)

        # Save numpy arrays to the specified directory
        np.save(os.path.join(subfolder_name, f"{self.validation_name}_sensitivity_coefficient_map_{self.bias_name}.npy"), self.sensitivity_coefficient_map)
        np.save(os.path.join(subfolder_name, f"{self.validation_name}_pearson_corr_{self.bias_name}.npy"), self.pearson_corr)
        np.save(os.path.join(subfolder_name, f"{self.validation_name}_spearman_corr_{self.bias_name}.npy"), self.spearman_corr)

        np.save(os.path.join(subfolder_name, f"{self.validation_name}_scaling_factor_map_{self.bias_name}.npy"), self.scaling_factor_map)
        np.save(os.path.join(subfolder_name, f"{self.validation_name}_variable_value_map_{self.bias_name}.npy"), self.variable_value_map)
        np.save(os.path.join(subfolder_name, f"{self.validation_name}_reference_value_map_{self.bias_name}.npy"), self.reference_value_map)

    def validationCase_load(self):

        print("\nLoading validation quantities")

        # Load saved numpy arrays from the specified directory
        self.sensitivity_coefficient_map = np.load(os.path.join(subfolder_name, f"{self.validation_name}_sensitivity_coefficient_map_{self.bias_name}.npy"))
        self.pearson_corr = np.load(os.path.join(subfolder_name, f"{self.validation_name}_pearson_corr_{self.bias_name}.npy"))
        self.spearman_corr = np.load(os.path.join(subfolder_name, f"{self.validation_name}_spearman_corr_{self.bias_name}.npy"))

        self.scaling_factor_map = np.load(os.path.join(subfolder_name, f"{self.validation_name}_scaling_factor_map_{self.bias_name}.npy"))
        self.variable_value_map = np.load(os.path.join(subfolder_name, f"{self.validation_name}_variable_value_map_{self.bias_name}.npy"))
        self.reference_value_map = np.load(os.path.join(subfolder_name, f"{self.validation_name}_reference_value_map_{self.bias_name}.npy"))

    def plot(self):

        # Iterate over the number of folders
        # for i in range(self.folder_number):
        #     print("folder_name : ",self.folder_name[i])

            # # Create subplots for each folder
            # fig, ax = plt.subplots(1,2)

            # # Adjust the space between subplots
            # plt.subplots_adjust(left=0.1,
            #                     bottom=0.1,
            #                     right=0.9,
            #                     top=0.9,
            #                     wspace=0.34,
            #                     hspace=0.4)

            # # Scatter plot for the first subplot (Scaling Factor vs Variable Value)
            # ax[0].scatter(self.scaling_factor_map[i][:], self.variable_value_map[i][:], c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20, label = self.folder_name[i])
            # ax[0].set_xlabel(f"scaling factor - {self.bias_name}")
            # ax[0].set_ylabel(self.variable_name)
            # ax[0].legend()

            # # Scatter plot for the second subplot (Scaling Factor vs Sensitivity Coefficient)
            # ax[1].scatter(self.scaling_factor_map[i][:], self.sensitivity_coefficient_map[i][:], c = '#98E18D', edgecolors= '#999AA2', marker = 'o', s=20, label = self.folder_name[i])
            # ax[1].set_xlabel(f"scaling factor - {self.bias_name}")
            # ax[1].set_ylabel("Sensitivity coefficient")
            # ax[1].legend()

            # plt.show()
            # # plt.close()

        # fig, ax = plt.subplots()
        # for i in range(self.sample_number):
        #     ax.scatter(data, 100*self.variable_value_map[:,i], c = '#FA82B4', edgecolors= '#999AA2', marker = 'o', s=2)

        # ax.plot([1e-3, 1e2],[1e-3, 1e2], '-', color = '#757575')
        # ax.plot([1e-3, 1e2],[2e-3, 2e2],'--', color = '#757575')
        # ax.plot([1e-3, 1e2],[5e-4, 5e1],'--', color = '#757575')
        # ax.set_xlim(1e-2, 1e1)
        # ax.set_ylim(1e-2, 1e1)

        # ax.set_xscale('log')
        # ax.set_yscale('log')

        # ax.set_xlabel('Experimental (%)')
        # ax.set_ylabel('Calculated (%)')

        # Create a scatter plot for 
        # the global sensitivity coefficients (delta)
        fig, ax = plt.subplots()
        x_axis = [item.replace('test_Baker1977__', '') for item in self.folder_name]
        for i in range(0, len(self.feature)):
            ax.plot(x_axis, self.spearman_corr[:,i], label= self.bias_name[i], marker='o', linestyle='-')

        ax.set_ylabel("Spearman (/)")
        ax.legend()
        plt.show()
        # plt.close()

        fig, ax = plt.subplots()
        x_axis = [item.replace('test_Baker1977__', '') for item in self.folder_name]
        for i in range(0, len(self.feature)):
            ax.plot(x_axis, self.pearson_corr[:,i], label= self.bias_name[i], marker='o', linestyle='-')

        ax.set_ylabel("Pearson (/)")
        ax.legend()
        plt.show()

        # ERROR BAR:
        # map = (folder x sample) --> max and min for each folder, along the samplings
        # axis = 1 -> along the columns (samplings)
        print("ERROR BAR: ")
        print(f"{np.min(self.variable_value_map, axis = 1)}, {np.max(self.variable_value_map, axis = 1)}")
        print("-----------")

        # plt.show()
        # plt.close()

        # Append to a txt file to store the data
        with open('error_bars.txt', 'a') as f:

            # Write header for each parameter
            header = "{:<15}".format(" ")
            for folder in self.folder_name:
                header += "{:<30}".format(folder)
            header += "\n"
            f.write(header)

            # Write the ERROR BARs
            # np.array([0.00080405, 0.00091633, 0.00114718, 0.00131682, 0.00165466, 0.00208512, 0.00213937, 0.00227493, 0.00230765]))
            i=0
            lower_bound = "{:<15}".format(" ")
            upper_bound = "{:<15}".format(" ")

            for folder in self.folder_name:
                lower_bound += f"{np.min(self.variable_value_map[i,:])},"
                upper_bound += f"{np.max(self.variable_value_map[i,:])},"
            f.write(lower_bound)
            f.write('\n')
            f.write(upper_bound)

            f.write('\n')

    def globalSensitivityCoefficients(self):

        print("sensitivity map : ", self.sensitivity_coefficient_map)
        # print("scaling factors map : ", self.scaling_factor_map)

        # shape of sensitivity map: number of cases, number of samplings
        # Calculate the global sensitivity coefficients as the mean of sensitivity_coefficient_map
        self.global_sensitivity_coefficients = np.mean(self.sensitivity_coefficient_map)
        print("mean k :", self.global_sensitivity_coefficients)

        # Calculate the averaged-test global sensitivity coefficients 
        # self.global_sensitivity_coefficients = (np.max(self.sensitivity_coefficient_map, axis=1) + np.min(self.sensitivity_coefficient_map, axis=1)) / 2
        print("GSA-K : ", self.global_sensitivity_coefficients)

        # Calculate the sum of k_over_sf divided by the number of its elements (i.e., the average)
        self.ranking_parameters = np.mean(self.global_sensitivity_coefficients)
        print("Ranking parameter: ", self.ranking_parameters)

        # Append to a txt file to store the data
        with open('sensitivity_coefficients.txt', 'a') as f:
            # Write the current parameter name to the file
            f.write(f"Current parameter: {self.bias_name}\n")

            # Write header for each parameter
            header = "{:<15}".format(" ")
            for folder in self.folder_name:
                header += "{:<30}".format(folder)
            header += "\n"
            f.write(header)

            # Write the global sensitivity coefficients to the file
            row = "{:<15}".format('Σ(ΔK/ΔSF)/T')
            row += "{:<30}".format(self.global_sensitivity_coefficients)
            row += "\n"
            f.write(row)

            # Write the ranking parameters to the file
            row = "{:<15}".format('Σ(ΔK/ΔSF)/T')
            row += "{:<30}".format(self.ranking_parameters)
            row += "\n"
            f.write(row)

            f.write('\n')

def run_sensitivity_analysis(validation_parameters):

    # PreProcessing
    if os.path.exists('sensitivity_coefficients.txt'):
        os.remove('sensitivity_coefficients.txt')
    if os.path.exists('error_bars.txt'):
        os.remove('error_bars.txt')

    analysis = globalSensitivityAnalysis()
    analysis.readFile_inputScalingFactors()        

    analysis.setValidationParameters(validation_parameters)

    if(option_runSensitivity == True):
        analysis.uncertaintyAnalysis()
        analysis.validationCase_save()
    else:
        analysis.validationCase_load()

    analysis.globalSensitivityCoefficients()
    analysis.plot()

    # for params in validation_parameters:

    #     print(f"ANALYSING: {params}")

    #     analysis = globalSensitivityAnalysis()
    #     analysis.readFile_inputScalingFactors()        

    #     analysis.setValidationParameters(**params)

    #     if(option_runSensitivity == True):
    #         analysis.uncertaintyAnalysis()
    #         analysis.validationCase_save()
    #     else:
    #         analysis.validationCase_load()

    #     analysis.validationCase_plot()
    #     analysis.globalSensitivityCoefficients()
    #     analysis.globalPlot([0.06, 0.07, 0.08, 0.09, 0.12, 0.15, 0.18, 0.24, 0.31])
        
        # analysis.globalPlot([0.97, 0.68, 0.53, 0.46, 0.17,                   # 4000
        #                 0.62, 0.7, 0.44, 0.56, 0.27, 0.16,              # 4004
        #                 0.94, 0.57, 0.42, 0.54, 0.27,                   # 4005
        #                 1.07, 0.86, 0.63, 0.74, 0.59,                   # 4064
        #                 1.25, 1.35, 0.97, 0.79, 0.21,                   # 4065
        #                 0.42, 0.16, 0.09,                               # 4135
        #                 0.6, 0.62, 0.26, 0.11,                          # 4136
        #                 0.26, 0.18,                                     # 4140
        #                 0.7, 0.46, 0.43, 0.43,                          # 4162
        #                 0.6, 0.59, 0.35, 0.4                            # 4163
        #                 ])

# THE ORDER MUST BE THE SAME AS IN INPUT_SCALING_FACTORS.TXT
validation_parameters = [
    {
        "variable_name": sciantix_variable,
        "validation_name": validation_database,
        "bias_name": "resolution rate",
        "deviation": 2,
        "feature": "scaled",
    },
    {
        "variable_name": sciantix_variable,
        "validation_name": validation_database,
        "bias_name": "trapping rate",
        "deviation": 5,
        "feature": "scaled",
    },
    {
        "variable_name": sciantix_variable,
        "validation_name": validation_database,
        "bias_name": "nucleation rate",
        "deviation": 0.8,
        "feature": "below",
    },
    {
        "variable_name": sciantix_variable,
        "validation_name": validation_database,
        "bias_name": "diffusivity",
        "deviation": 1,
        "feature": "log10",
    },
    {
        "variable_name": sciantix_variable,
        "validation_name": validation_database,
        "bias_name": "temperature",
        "deviation": 0,
        "feature": "around",
    },
    {
        "variable_name": sciantix_variable,
        "validation_name": validation_database,
        "bias_name": "fission rate",
        "deviation": 0,
        "feature": "around",
    },
]

###############################################
run_sensitivity_analysis(validation_parameters)
