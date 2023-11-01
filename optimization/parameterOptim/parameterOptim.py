""" 
This is a python code to custom your scaling factors(SF) optimization for Talip cases,
it provides the functions:
    1: optimization
    2: plot
it provides the customizations:
    1: select the SFs you want to optimize
    2: select the benchmark
    3: set initial value
    4: select the optimization interval
before running:
    about the dictionary:
        1: make sure this code is located at the same dictionary with test_Talip2014_"temperature"K folders
        2: make sure the parent dictionary of this code is parallel with "bin" so that it can copy the sciantix.x 
            and input_scaling_factors.txt
    about the benchmark:
        1: make sure the interval between the coloumn data within the benchmark file is '\t', otherwise it will cause 
            some problems when the code to extract the data from these files
something maybe useful:
    about the output:
        1: since in Talip cases, we only have the fraction release(FR) and release rate(RR) data sets, so
            this code doesnot provide a function to choose output variables, they are seted automatically to be FR and RR.
            You can modify it in the method "readOutput" of class "forInputOutput".
    about the optimization weight for different variables:
        1: this code doesnot provide a function to set the weight from interacting with terminal
            You can modify it in the function called "errorFunction"
    about the bounds:
        1: only the bounds of SFs for pre exponential and activation energies of He diffusivity and Henry constant
            are seted. For other SFs, they are set between 0 to infinity.
            You can modify it in the class "initialization"
    about the package imported:
        1: in my computer I nee to use 'TkAgg' and xming to plt.show the figure.

@author Shuo Wang
"""

import os
import shutil
import numpy as np
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import scipy.optimize as optimize
from scipy.optimize import Bounds

################
# class and functions
################

class initialization():
    def __init__(self):
        # from bin copy sciantix.x and input_scaling_factors.txt to current dictionary
        copyFromFolderInUpperDic_to_currentDic("sciantix.x","bin")
        copyFromFolderInUpperDic_to_currentDic("input_scaling_factors.txt","bin")
        print("################################################")
        print("#Initilizing-----Select Experimental Benchmark#")
        print("################################################")
        print("0 for Talip2014_1320K")
        print("1 for Talip2014_1400K_b")
        print("2 for Talip2014_1400K_c")
        print("3 for Talip2014_1600K")
        print("4 for Talip2014_1800K")
        while True:
            benchmarkNumber = input("Entering your choice (from 0 to 4): ")
            try:
                self.benchmarkSelection = int(benchmarkNumber)
                if 0 <= self.benchmarkSelection <= 4:
                    print("Your choice is:", self.benchmarkSelection)
                    break
                else:
                    print("Input is out of range, try again.")
            except ValueError:
                print("Invalid input, try again.")

            self.custom_delimiter = r'\s+|\t'
        
        print("################################################")
        print("#Initilizing-----Re-arrange experimental data#")
        print("################################################")

        if self.benchmarkSelection == 0:
            folder_name = "test_Talip2014_1320K"
            
            os.chdir(folder_name)
            history = np.genfromtxt("input_history.txt",dtype = 'float', delimiter = '\t')
            ramp1EndTime = history[2,0]
            ramp2startTime = history[3,0]
            ramp2EndTime = history[4,0]
            ramp1EndTemperature = history[2,1]
            ramp2startTemperature = history[3,1]
            ramp2EndTemperature = history[4,1]

            variable_selected = np.array(["Time (h)","Temperature (K)"])
            timeAndtemperature = getSelectedVariablesValueFromOutput(variable_selected,"output.txt")
            timeSequence = timeAndtemperature[:,0]
            temperatureSequence = timeAndtemperature[:,1]

            self.index_ramp1End = np.where(timeSequence == ramp1EndTime)[0]
            self.index_ramp2Start = np.where(timeSequence == ramp2startTime)[0]
            self.index_ramp2End = np.where(timeSequence == ramp2EndTime)[0]

            self.index_ramp1End = self.index_ramp1End[0]
            self.index_ramp2Start = self.index_ramp2Start[0]
            self.index_ramp2End = self.index_ramp2End[0]
            
            FRfile_name = "Talip2014_release_data.txt"
            RRfile_name = "Talip2014_rrate_data.txt"
            FRexp = np.genfromtxt(FRfile_name, dtype= 'str', delimiter='\t').astype(float)
            RRexp = np.genfromtxt(RRfile_name, dtype= 'str', delimiter='\t').astype(float)
            time = FRexp[:,0]
            fractionRelease = FRexp[:,1]
            temperature = RRexp[:,0]
            releaseRate = RRexp[:,1]
            temperature_reverse = temperature[::-1]
            self.tMax = max(time)
            self.index_tMaxAtTimeSequence = np.where(timeSequence >= self.tMax)[0]            

            self.index_ramp1End_exp = np.where(temperature >= ramp1EndTemperature)[0]
            self.index_ramp2Start_exp = len(temperature)-np.where(temperature_reverse <= ramp2startTemperature)[0]-1
            self.index_ramp2End_exp = np.where(temperature >= ramp2EndTemperature)[0]


            self.index_tMaxAtTimeSequence = self.index_tMaxAtTimeSequence[0]
            self.index_ramp1End_exp = self.index_ramp1End_exp[0]
            self.index_ramp2Start_exp = self.index_ramp2Start_exp[0]
            self.index_ramp2End_exp = self.index_ramp2End_exp[0]


            releaseRate1 = releaseRate[0:self.index_ramp1End_exp]
            releaseRate2 = releaseRate[self.index_ramp2Start_exp:self.index_ramp2End_exp]
            temperature1 = temperature[0:self.index_ramp1End_exp]
            temperature2 = temperature[self.index_ramp2Start_exp:self.index_ramp2End_exp]

        elif self.benchmarkSelection == 1:
            folder_name = "test_Talip2014_1400K_b"

            os.chdir(folder_name)
            history = np.genfromtxt("input_history.txt",dtype = 'float', delimiter = '\t')
            ramp1EndTime = history[2,0]
            ramp2startTime = history[3,0]
            ramp2EndTime = history[4,0]

            variable_selected = np.array(["Time (h)","Temperature (K)"])
            timeAndtemperature = getSelectedVariablesValueFromOutput(variable_selected,"output.txt")
            timeSequence = timeAndtemperature[:,0]
            temperatureSequence = timeAndtemperature[:,1]

            self.index_ramp1End = np.where(timeSequence == ramp1EndTime)[0]
            self.index_ramp2Start = np.where(timeSequence == ramp2startTime)[0]
            self.index_ramp2End = np.where(timeSequence == ramp2EndTime)[0]

            self.index_ramp1End = self.index_ramp1End[0]
            self.index_ramp2Start = self.index_ramp2Start[0]
            self.index_ramp2End = self.index_ramp2End[0]

            ramp1EndTemperature = history[2,1]
            ramp2startTemperature = history[3,1]
            ramp2EndTemperature = history[4,1]

            FRfile_name = "Talip2014_release_data.txt"
            RRfile_name = "Talip2014_rrate_data.txt"
            FRexp = np.genfromtxt(FRfile_name, dtype= 'str', delimiter='\t').astype(float)
            RRexp = np.genfromtxt(RRfile_name, dtype= 'str', delimiter='\t').astype(float)
            time = FRexp[:,0]
            fractionRelease = FRexp[:,1]
            temperature = RRexp[:,0]
            releaseRate = RRexp[:,1]
            temperature_reverse = temperature[::-1]
            self.tMax = max(time)
            self.index_tMaxAtTimeSequence = np.where(timeSequence>=self.tMax)[0]

            self.index_ramp1End_exp = np.where(temperature >= ramp1EndTemperature)[0]
            self.index_ramp2Start_exp = len(temperature)-np.where(temperature_reverse <= ramp2startTemperature)[0]-1
            self.index_ramp2End_exp = np.where(temperature >= ramp2EndTemperature)[0]

            self.index_tMaxAtTimeSequence = self.index_tMaxAtTimeSequence[0]
            self.index_ramp1End_exp = self.index_ramp1End_exp[0]
            self.index_ramp2Start_exp = self.index_ramp2Start_exp[0]
            self.index_ramp2End_exp = self.index_ramp2End_exp[0]

            releaseRate1 = releaseRate[0:self.index_ramp1End_exp]
            releaseRate2 = releaseRate[self.index_ramp2Start_exp:self.index_ramp2End_exp]
            temperature1 = temperature[0:self.index_ramp1End_exp]
            temperature2 = temperature[self.index_ramp2Start_exp:self.index_ramp2End_exp]


        elif self.benchmarkSelection == 2:
            folder_name = "test_Talip2014_1400K_c"

            os.chdir(folder_name)
            history = np.genfromtxt("input_history.txt",dtype = 'float', delimiter = '\t')
            ramp1EndTime = history[1,0]
            ramp2startTime = history[2,0]
            ramp2EndTime = history[3,0]

            variable_selected = np.array(["Time (h)","Temperature (K)"])
            timeAndtemperature = getSelectedVariablesValueFromOutput(variable_selected,"output.txt")
            timeSequence = timeAndtemperature[:,0]
            temperatureSequence = timeAndtemperature[:,1]

            self.index_ramp1End = np.where(timeSequence == ramp1EndTime)[0]
            self.index_ramp2Start = np.where(timeSequence == ramp2startTime)[0]
            self.index_ramp2End = np.where(timeSequence == ramp2EndTime)[0]

            self.index_ramp1End = self.index_ramp1End[0]
            self.index_ramp2Start = self.index_ramp2Start[0]
            self.index_ramp2End = self.index_ramp2End[0]

            ramp1EndTemperature = history[1,1]
            ramp2startTemperature = history[2,1]
            ramp2EndTemperature = history[3,1]

            FRfile_name = "Talip2014_release_data.txt"
            RRfile_name = "Talip2014_rrate_data.txt"
            FRexp = np.genfromtxt(FRfile_name, dtype= 'str', delimiter='\t').astype(float)
            RRexp = np.genfromtxt(RRfile_name, dtype= 'str', delimiter='\t').astype(float)
            time = FRexp[:,0]
            fractionRelease = FRexp[:,1]
            temperature = RRexp[:,0]
            releaseRate = RRexp[:,1]
            temperature_reverse = temperature[::-1]
            self.tMax = max(time)
            self.index_tMaxAtTimeSequence = np.where(timeSequence>=self.tMax)[0]

            self.index_ramp1End_exp = np.where(temperature >= ramp1EndTemperature)[0]
            self.index_ramp2Start_exp = len(temperature)-np.where(temperature_reverse >= ramp2startTemperature)[0]-1
            self.index_ramp2End_exp = len(temperature)-np.where(temperature_reverse > ramp2EndTemperature)[0]-1

            self.index_tMaxAtTimeSequence = self.index_tMaxAtTimeSequence[0]
            self.index_ramp1End_exp = self.index_ramp1End_exp[0]
            self.index_ramp2Start_exp = self.index_ramp2Start_exp[0]
            self.index_ramp2End_exp = self.index_ramp2End_exp[0]

            releaseRate1 = releaseRate[0:self.index_ramp1End_exp]
            releaseRate2 = releaseRate[self.index_ramp2Start_exp:self.index_ramp2End_exp]
            temperature1 = temperature[0:self.index_ramp1End_exp]
            temperature2 = temperature[self.index_ramp2Start_exp:self.index_ramp2End_exp]

        
        elif self.benchmarkSelection == 3:
            folder_name = "test_Talip2014_1600K"

            os.chdir(folder_name)
            history = np.genfromtxt("input_history.txt",dtype = 'float', delimiter = '\t')
            ramp1EndTime = history[2,0]
            ramp2startTime = history[3,0]
            ramp2EndTime = history[4,0]

            variable_selected = np.array(["Time (h)","Temperature (K)"])
            timeAndtemperature = getSelectedVariablesValueFromOutput(variable_selected,"output.txt")
            timeSequence = timeAndtemperature[:,0]
            temperatureSequence = timeAndtemperature[:,1]

            self.index_ramp1End = np.where(timeSequence == ramp1EndTime)[0]
            self.index_ramp2Start = np.where(timeSequence == ramp2startTime)[0]
            self.index_ramp2End = np.where(timeSequence == ramp2EndTime)[0]

            self.index_ramp1End = self.index_ramp1End[0]
            self.index_ramp2Start = self.index_ramp2Start[0]
            self.index_ramp2End = self.index_ramp2End[0]

            ramp1EndTemperature = history[2,1]
            ramp2startTemperature = history[3,1]
            ramp2EndTemperature = history[4,1]

            FRfile_name = "Talip2014_release_data.txt"
            RRfile_name = "Talip2014_rrate_data.txt"
            FRexp = np.genfromtxt(FRfile_name, dtype= 'str', delimiter='\t').astype(float)
            RRexp = np.genfromtxt(RRfile_name, dtype= 'str', delimiter='\t').astype(float)
            time = FRexp[:,0]
            fractionRelease = FRexp[:,1]
            temperature = RRexp[:,0]
            releaseRate = RRexp[:,1]
            temperature_reverse = temperature[::-1]
            self.tMax = max(time)
            self.index_tMaxAtTimeSequence = np.where(timeSequence>=self.tMax)[0]

            self.index_ramp1End_exp = np.where(temperature >= ramp1EndTemperature)[0]
            self.index_ramp2Start_exp = len(temperature)-np.where(temperature_reverse >= ramp2startTemperature)[0]-1
            self.index_ramp2End_exp = len(temperature) - np.where(temperature_reverse >= ramp2EndTemperature)[0]-1

            self.index_tMaxAtTimeSequence = self.index_tMaxAtTimeSequence[0]
            self.index_ramp1End_exp = self.index_ramp1End_exp[0]
            self.index_ramp2Start_exp = self.index_ramp2Start_exp[0]
            self.index_ramp2End_exp = self.index_ramp2End_exp[0]

            releaseRate1 = releaseRate[0:self.index_ramp1End_exp]
            releaseRate2 = releaseRate[self.index_ramp2Start_exp:self.index_ramp2End_exp]
            temperature1 = temperature[0:self.index_ramp1End_exp]
            temperature2 = temperature[self.index_ramp2Start_exp:self.index_ramp2End_exp]

        else:
            folder_name = "test_Talip2014_1800K"

            os.chdir(folder_name)
            history = np.genfromtxt("input_history.txt",dtype = 'float', delimiter = '\t')
            ramp1EndTime = history[2,0]
            ramp2startTime = history[3,0]
            ramp2EndTime = history[4,0]

            variable_selected = np.array(["Time (h)","Temperature (K)"])
            timeAndtemperature = getSelectedVariablesValueFromOutput(variable_selected,"output.txt")
            timeSequence = timeAndtemperature[:,0]
            temperatureSequence = timeAndtemperature[:,1]

            self.index_ramp1End = np.where(timeSequence == ramp1EndTime)[0]
            self.index_ramp2Start = np.where(timeSequence == ramp2startTime)[0]
            self.index_ramp2End = np.where(timeSequence == ramp2EndTime)[0]

            self.index_ramp1End = self.index_ramp1End[0]
            self.index_ramp2Start = self.index_ramp2Start[0]
            self.index_ramp2End = self.index_ramp2End[0]

            ramp1EndTemperature = history[2,1]
            ramp2startTemperature = history[3,1]
            ramp2EndTemperature = history[4,1]

            FRfile_name = "Talip2014_release_data.txt"
            RRfile_name = "Talip2014_rrate_data.txt"
            FRexp = np.genfromtxt(FRfile_name, dtype= 'str', delimiter='\t').astype(float)
            RRexp = np.genfromtxt(RRfile_name, dtype= 'str', delimiter='\t').astype(float)
            time = FRexp[:,0]
            fractionRelease = FRexp[:,1]
            temperature = RRexp[:,0]
            releaseRate = RRexp[:,1]
            temperature_reverse = temperature[::-1]
            self.tMax = max(time)
            self.index_tMaxAtTimeSequence = np.where(timeSequence>self.tMax)[0]

            self.index_ramp1End_exp = np.where(temperature >= ramp1EndTemperature)[0]
            self.index_ramp2Start_exp = len(temperature)-np.where(temperature_reverse >= ramp2startTemperature)[0]-1
            self.index_ramp2End_exp = len(temperature)- np.where(temperature_reverse >= ramp2EndTemperature)[0]-1
            
            self.index_tMaxAtTimeSequence = self.index_tMaxAtTimeSequence[0]
            self.index_ramp1End_exp = self.index_ramp1End_exp[0]
            self.index_ramp2Start_exp = self.index_ramp2Start_exp[0]
            self.index_ramp2End_exp = self.index_ramp2End_exp[0]

            releaseRate1 = releaseRate[0:self.index_ramp1End_exp]
            releaseRate2 = releaseRate[self.index_ramp2Start_exp:self.index_ramp2End_exp]
            temperature1 = temperature[0:self.index_ramp1End_exp]
            temperature2 = temperature[self.index_ramp2Start_exp:self.index_ramp2End_exp]
            
        
        self.releaseRate1_exp = np.zeros((self.index_ramp1End+1))
        self.releaseRate2_exp = np.zeros((self.index_ramp2End-self.index_ramp2Start+1))
        for i in range(self.index_ramp1End + 1):
            index = np.where(temperature1 <= temperatureSequence[i])[0]
            if len(index) == 0:
                    index = [0]
            index = index[-1]
            self.releaseRate1_exp[i] = releaseRate1[index]
        
        for i in range(self.index_ramp2Start,self.index_ramp2End+1):
            index = np.where(temperature2 <= temperatureSequence[i])[0]
            if len(index) == 0:
                index = [0]
            index = index[-1]
            self.releaseRate2_exp[i-self.index_ramp2Start] = releaseRate2[index]
        

        self.fractionRelease_exp = np.zeros((self.index_tMaxAtTimeSequence+1))
        for i in range(self.index_tMaxAtTimeSequence+1):
            index = np.where(time <= timeSequence[i])[0]
            index = index[-1]
            self.fractionRelease_exp[i] = fractionRelease[index]
        
        #self.fractionRelease1_exp = np.zeros((self.index_ramp1End +1))
        if self.index_ramp1End <= self.index_tMaxAtTimeSequence:
            self.fractionRelease1_exp = self.fractionRelease_exp[0:self.index_ramp1End]
            if self.index_ramp2Start <= self.index_tMaxAtTimeSequence:
                self.fractionRelease2_exp = self.fractionRelease_exp[self.index_ramp2Start-1:]
            else:
                self.fractionRelease2_exp = np.array([-1])

        else:
            self.fractionRelease1_exp = self.fractionRelease_exp
            self.fractionRelease2_exp = np.array([-1])
        

        

        if os.path.exists("sciantix.x"):
            os.remove("sciantix.x")
            shutil.copy("../sciantix.x", os.getcwd())
        else:
            shutil.copy("../sciantix.x", os.getcwd())


        print("###############################################################")
        print("#Initilizing-----Select scalingfactors you want to optimize#")
        print("###############################################################")

        file_name  = "input_scaling_factors.txt"
        self.scaling_factors = {}
        with open(file_name, 'r') as file:
            lines = file.readlines()
            i = 0
            l = len(lines)
        
        while True:
            numberOfParameter = input("How many parameters your want to optimize: ")
            try:
                self.numberSelection = int(numberOfParameter)
                
                if 1 <= self.numberSelection <= l/2:
                    print(f"You selected {self.numberSelection} parameters to be optimized")
                    break
                else:
                    print("Input is out of range, try again.") 
            except ValueError:
                print("Invalid input, try again.")
        
        self.sf_name = []
        while i < l:
            value = float(lines[i].strip())
            self.sf_name.append(lines[i + 1].strip()[len("# scaling factor - "):])
            self.scaling_factors[self.sf_name[-1]] = value
            print(f"{int(i/2)} for {self.sf_name[-1]}")
            i += 2
            
        parameterCodeName = np.zeros(self.numberSelection)
        self.parameterSelection = np.zeros(self.numberSelection,dtype = int)
        self.sf_selection = []
        self.sf_selection_initialValue = np.zeros([self.numberSelection])
        self.sf_selection_bounds = np.zeros([2,self.numberSelection])
        for i in range(self.numberSelection):
            while True:
                parameterCodeName[i] = input(f"Selecting your {i+1} th parameter (from 0 to {int(l/2)-1}): ")
                try:
                    self.parameterSelection[i] = int(parameterCodeName[i])
                    if 0 <= self.parameterSelection[i] < l/2:
                        self.sf_selection.append(self.sf_name[self.parameterSelection[i]])
                        print(f"Your {i+1}th choice is:", self.sf_name[self.parameterSelection[i]])
                        if self.sf_selection[i] == "helium diffusivity pre exponential":
                            self.sf_selection_bounds[0,i] = 0.5
                            self.sf_selection_bounds[1,i] = 19.9
                        elif self.sf_selection[i] == "helium diffusivity activation energy":
                            self.sf_selection_bounds[0,i] = 0.835
                            self.sf_selection_bounds[1,i] = 1.2
                        elif self.sf_selection[i] == "henry constant pre exponential":
                            self.sf_selection_bounds[0,i] = 0.0627
                            self.sf_selection_bounds[1,i] = 16.09
                        elif self.sf_selection[i] == "henry constant activation energy":
                            self.sf_selection_bounds[0,i] = 0.431
                            self.sf_selection_bounds[1,i] = 1.55
                        else:
                            self.sf_selection_bounds[0,i] = 0
                            self.sf_selection_bounds[0,i] = float('inf')
                        break
                    else:
                        print("Input is out of range, try again.") 
                except ValueError:
                    print("Invalid input, try again.")

        print("#####################################################")
        print("The scaling factors that you want to optimize are:")
        for i in range(self.numberSelection):
            print(self.sf_selection[i])

        print("###############################################################")
        print("#Initilizing-----seting initial value#")
        print("###############################################################")
        
        print("Using default initial value(1.0)? ")
        
        while True:
            answer_initialvalue = input("[Y/N]:" )
            try:
                if answer_initialvalue == "Y" or answer_initialvalue == "y":
                    defaultInitialValue = True
                    break
                elif answer_initialvalue == "N" or answer_initialvalue == "n":
                    defaultInitialValue = False
                    break
                else:
                    print("Invalid input, try again")
            except ValueError:
                print("Invalid input, try again")
            
        if defaultInitialValue == True:
            self.sf_selection_initialValue = self.sf_selection_initialValue + 1.0
        else:
            for i in range(self.numberSelection):
                while True:
                    try:
                        userinput = float(input(f"Your {self.sf_selection[i]} initial value:"))
                        self.sf_selection_initialValue[i] = userinput
                        break
                    except ValueError:
                        print("Invalid input, try again!")
        print("###############################################################")
        print("#Initilizing-----Set optimization interval#")
        print("###############################################################")
        print("1: 2 ramps(neglect plateau)")
        print("2: 1st ramp")
        print("3: 2nd ramp")
        print("4: Both 2 and 3")
        print("5: All 1, 2, and 3")
        while True:
            optimStrategy = input("Entering your choice(from 1 to 5):")
            try:
                self.optimInterval = int(optimStrategy)
                if self.optimInterval >= 1 and self.optimInterval <= 5:
                    break
                else:
                    print("Input out of the range, try agin")
            except ValueError:
                print("Invalid input, try again!")
        print("#####################################################")
        print("FINISHING INITIALIZATION!!!!")
        print("#####################################################")


        # we can also add a section to let the user to select the variables that we need in output file, but in Talip cases, for sure, are FR and RR
    def getSciantixIndexOfFRexpMaxTime(self):
        return self.index_tMaxAtTimeSequence

    def getSciantixFirstRampEndIndex(self):
        return self.index_ramp1End
    
    def getSciantixSecondRampStartIndex(self):
        return self.index_ramp2Start
    
    def getSciantixSecondRampEndIndex(self):
        return self.index_ramp2End
    
    def getExpFirstRampEndIndex(self):
        return self.index_ramp1End_exp
    
    def getExpSecondRampStartIndex(self):
        return self.index_ramp2Start_exp
    
    def getExpSecondRampEndIndex(self):
        return self.index_ramp2End_exp
    
    def getRRexpFirstRampValue(self):
        return self.releaseRate1_exp

    def getRRexpSecondRampValue(self):
        return self.releaseRate2_exp
    
    def getFRexpValue(self):
        return self.fractionRelease_exp

    def getSlectedScalingFactorsName(self):
        return self.sf_selection
    
    def getScalingFactors(self):
        #This will return an dictionary where obtain the name of all scaling factors and their associated value
        return self.scaling_factors

    def getSelectedScalingFactorsInitialValue(self):
        return self.sf_selection_initialValue

    def getSelectedScalingFactorsBounds(self):
        return self.sf_selection_bounds

    def getNumberOfSelectedScalingFactors(self):
        return self.numberSelection

    def getOptimizationMethod(self):
        return self.optimInterval
    
    def getFRexpFirstRampValue(self):
        return self.fractionRelease1_exp
    
    def getFRexpSecondRampValue(self):
        return self.fractionRelease2_exp
    
class forInputOutput():
    def __init__(self):

        # now build a new folder in selected benchmark folder
        folder_name = "Optimization"
        if not os.path.exists(folder_name):
            os.makedirs(folder_name)
        else:
            shutil.rmtree(folder_name)
            os.makedirs(folder_name)
        
        os.chdir(folder_name)
        shutil.copy("../sciantix.x", os.getcwd())
        shutil.copy("../input_initial_conditions.txt", os.getcwd())
        shutil.copy("../input_settings.txt", os.getcwd())
        shutil.copy("../input_history.txt", os.getcwd())
        
    def writeInputScalingFactors(self,scaling_factors,sf_selected,sf_selected_value):
        # assign new sf value and write the file, and run sciantix.x
        l = len(sf_selected)
        for i in range(l):
            scaling_factors[sf_selected[i]] = sf_selected_value[i]
        file_name = "input_scaling_factors.txt"
        with open(file_name,'w') as file:
            for key, value in scaling_factors.items():
                file.write(f'{value}\n')
                file.write(f'# scaling factor - {key}\n')
        
        self.sf_selected_value = sf_selected_value

        os.system("./sciantix.x")
        # till now, the CWD is parameterOptim/selectedBenchamrk/Optimization

    def readOutput(self,index_tMaxAtTimeSequence,index_ramp1End, index_ramp2Start, index_ramp2End):
        # here, in Talip cases, we only interesed in FR and RR since we only have these two variables experimental data
        self.variable_selected = np.array(("Time (h)","Temperature (K)","He fractional release (/)", "He release rate (at/m3 s)"))
        self.variable_selected_value = getSelectedVariablesValueFromOutput(self.variable_selected,"output.txt")
        
        self.fractionRelease_sciantix = self.variable_selected_value[0:index_tMaxAtTimeSequence,2]
        self.releaseRate1_sciantix = self.variable_selected_value[0:index_ramp1End,3]
        self.releaseRate2_sciantix = self.variable_selected_value[index_ramp2Start:index_ramp2End,3]

        if index_ramp1End <= index_tMaxAtTimeSequence:
            self.fractionRelease1_sciantix = self.fractionRelease_sciantix[0:index_ramp1End]
            if index_ramp2Start <= index_tMaxAtTimeSequence:
                self.fractionRelease2_sciantix = self.fractionRelease_sciantix[index_ramp2Start-1:]
            else:
                self.fractionRelease2_sciantix = np.array([-1])

        else:
            self.fractionRelease1_sciantix = self.fractionRelease_sciantix
            self.fractionRelease2_sciantix = np.array([-1])
     

    def getSelectedVariablesName(self):
        return self.variable_selected
    
    def getFRValue(self):
        return self.fractionRelease_sciantix
    
    def getRRFirstRampValue(self):
        return self.releaseRate1_sciantix
    
    def getRRSecondRampValue(self):
        return self.releaseRate2_sciantix

    def getSelectedVariablesValue(self):
        return self.variable_selected_value
    
    def getCurrentSlectedScalingFactorsValue(self):
        return self.sf_selected_value

    def getFRfirstRampValue(self):
        return self.fractionRelease1_sciantix
    
    def getFRSecondRampValue(self):
        return self.fractionRelease2_sciantix

def errorFunction(optimInterval,FRexp,FRsciantix,RR1exp,RR1sciantix,RR2exp,RR2sciantix, FR1exp, FR2exp, FR1sciantix, FR2sciantix):
    #due to the < and >, sometimes, exp data will have one more or less data point than sciantix data
    FRexp,FRsciantix = sizeEqual(FRexp,FRsciantix)
    FR1exp,FR1sciantix = sizeEqual(FR1exp,FR1sciantix)
    FR2exp,FR2sciantix = sizeEqual(FR2exp,FR2sciantix)
    RR1exp,RR1sciantix = sizeEqual(RR1exp,RR1sciantix)
    RR2exp,RR2sciantix = sizeEqual(RR2exp,RR2sciantix)
    

    errorFR = abs((np.sum(abs(FRsciantix)) - np.sum(abs(FRexp))))/np.sum(abs(FRexp))
    errorRR1 = abs((np.sum(abs(RR1sciantix)) - np.sum(abs(RR1exp))))/np.sum(abs(RR1exp))
    errorRR2 = abs((np.sum(abs(RR1sciantix)) - np.sum(abs(RR2exp))))/np.sum(abs(RR2exp))


    FRweight = 1
    RRweight = 1

    if optimInterval == 1:
        error = FRweight*errorFR + RRweight*(errorRR1 + errorRR2)
    elif optimInterval == 2:
            errorFR1 = abs((np.sum(abs(FR1sciantix)) - np.sum(abs(FR1exp))))/np.sum(abs(FR1exp))
            error = FRweight*errorFR1+RRweight*errorRR1
    elif optimInterval == 3:
        if len(FR2exp) == 1 and FR2exp[0] == -1:
            error = RRweight*errorRR2
        else:
            errorFR2 = abs((np.sum(abs(FR2sciantix)) - np.sum(abs(FR2exp))))/np.sum(abs(FR2exp))
            error = FRweight*errorRR2+RRweight*errorFR2

    return error

def copyToUpperDic_and_rename_file(source_name, target_name):
    current_dir = os.path.abspath(os.getcwd())
    parent_dir = os.path.dirname(current_dir)

    source_path = os.path.join(current_dir, source_name)
    target_path = os.path.join(parent_dir, target_name)

    if os.path.exists(target_path):
        os.remove(target_path)
        print(f"{target_name} exists, re-created")

    shutil.copy(source_path, target_path)
    print(f"{source_name} hase been copied to upper dictionary as {target_name} ")

def afterOptim(optimInterval,results,number_sf_selected,sf_selected,index_ramp1End,index_ramp2Start,index_ramp2End):
    if optimInterval == 1:
        if results.success == True:
            source_input_sf_name = "input_scaling_factors.txt"
            target_input_sf_name = "input_scaling_factors_wholeOptim.txt"
            copyToUpperDic_and_rename_file(source_input_sf_name,target_input_sf_name)

            source_output_name = "output.txt"
            target_output_name = "output_wholeOptim.txt"
            copyToUpperDic_and_rename_file(source_output_name,target_output_name)
            print("####################################")
            print("#OPTIMIZATION FINISHED, HERE IS YOUR RESULTS:")
            print("####################################")
            for i in range(number_sf_selected):
                print(f"{sf_selected[i]}:{results.x[i]}")
            print(f"simultantaneous optimization error is:{results.fun}")

    elif optimInterval == 2:
        if results.success == True:
            print("###########################")
            print("Optimization----first ramp finished")
            print("###########################")
            source_input_sf_name = "input_scaling_factors.txt"
            target_input_sf_name = "input_scaling_factors_firstRampOptim.txt"
            copyToUpperDic_and_rename_file(source_input_sf_name,target_input_sf_name)

            source_output_name = "output.txt"
            target_output_name = "output_firstRampOptim.txt"
            with open(source_output_name, "r") as source_file, open(target_output_name, "w") as target_file:
                all_lines = source_file.readlines()
                target_file.writelines((all_lines[0:index_ramp1End]))
            copyToUpperDic_and_rename_file(target_output_name,target_output_name)
            os.remove(target_output_name)
            print("####################################")
            print("#OPTIMIZATION FINISHED, HERE IS YOUR RESULTS:")
            print("####################################")
            for i in range(number_sf_selected):
                print(f"{sf_selected[i]} for firsr ramp: {results.x[i]}")
            print(f"first ramp error: {results.fun}")

    else:
        if results.success == True:
            print("###########################")
            print("Optimization----second ramp finished")
            print("###########################")
            source_input_sf_name = "input_scaling_factors.txt"
            target_input_sf_name = "input_scaling_factors_secondRampOptim.txt"
            copyToUpperDic_and_rename_file(source_input_sf_name,target_input_sf_name)

            source_output_name = "output.txt"
            target_output_name = "output_secondRampOptim.txt"
            with open(source_output_name, "r") as source_file, open(target_output_name, "w") as target_file:
                firstline = source_file.readline()
                all_lines = source_file.readlines()
                target_file.write(firstline)
                target_file.writelines((all_lines[index_ramp2Start:index_ramp2End]))
            copyToUpperDic_and_rename_file(target_output_name,target_output_name)
            os.remove(target_output_name)
            print("####################################")
            print("#OPTIMIZATION FINISHED, HERE IS YOUR RESULTS:")
            print("####################################")
            for i in range(number_sf_selected):
                print(f"{sf_selected[i]} for second ramp: {results.x[i]}")
            print(f"second ramp error: {results.fun}")

def optimization(initial,setInputOutput):
    index_tMaxAtTimeSequence = initial.getSciantixIndexOfFRexpMaxTime()
    index_ramp1End = initial.getSciantixFirstRampEndIndex()
    index_ramp2Start = initial.getSciantixSecondRampStartIndex()
    index_ramp2End = initial.getSciantixSecondRampEndIndex()
    RR1exp = initial.getRRexpFirstRampValue()
    RR2exp = initial.getRRexpSecondRampValue()
    FRexp = initial.getFRexpValue()
    FR1exp = initial.getFRexpFirstRampValue()
    FR2exp = initial.getFRexpSecondRampValue()

    scaling_factors = initial.getScalingFactors()
    sf_selected = initial.getSlectedScalingFactorsName()
    optimInterval = initial.getOptimizationMethod()
    number_sf_selected = initial.getNumberOfSelectedScalingFactors()
    sf_selected_initial_value = initial.getSelectedScalingFactorsInitialValue()
    sf_selected_bounds = initial.getSelectedScalingFactorsBounds()
    bounds = Bounds(sf_selected_bounds[0,:],sf_selected_bounds[1,:])
    
    def costFunction(sf_selected_value):
        setInputOutput.writeInputScalingFactors(scaling_factors,sf_selected,sf_selected_value)
        setInputOutput.readOutput(index_tMaxAtTimeSequence,index_ramp1End,index_ramp2Start,index_ramp2End)
        FRsciantix = setInputOutput.getFRValue()
        RR1sciantix = setInputOutput.getRRFirstRampValue()
        RR2sciantix = setInputOutput.getRRSecondRampValue()
        FR1sciantix = setInputOutput.getFRfirstRampValue()
        FR2sciantix = setInputOutput.getFRSecondRampValue()
        current_sf_selected_value = setInputOutput.getCurrentSlectedScalingFactorsValue()
        print(f"current value: {current_sf_selected_value}")
        error = errorFunction(optimInterval,FRexp,FRsciantix,RR1exp,RR1sciantix,RR2exp,RR2sciantix, FR1exp, FR2exp, FR1sciantix, FR2sciantix)
        print(f"current error: {error}")
        return error

    print("########################################")
    print("#START OPTIMIZATION!!!#")
    print("########################################")
    if optimInterval <= 3:
        results = optimize.minimize(costFunction,sf_selected_initial_value, method = 'SLSQP',bounds=bounds)
        afterOptim(optimInterval,results,number_sf_selected,sf_selected,index_ramp1End,index_ramp2Start,index_ramp2End)
    elif optimInterval == 4:
        optimInterval_o = optimInterval
        for i in range(1,3): #I want 2 and 3, so 4-1=3, 4-2 =2
            optimInterval = optimInterval-i
            results = optimize.minimize(costFunction,sf_selected_initial_value, method = 'SLSQP',bounds=bounds)
            afterOptim(optimInterval,results,number_sf_selected,sf_selected,index_ramp1End,index_ramp2Start,index_ramp2End)
            if i == 1:
                print("######################")
                print("#start next interval optim")
                print("######################")

    else:
        optimInterval_o = optimInterval
        for i in range(2,5): #I want 1, 2, and 3, so 5-2=3, 5-3 =2, 5-4=1
            optimInterval = optimInterval_o-i
            results = optimize.minimize(costFunction,sf_selected_initial_value, method = 'SLSQP',bounds=bounds)
            afterOptim(optimInterval,results,number_sf_selected,sf_selected,index_ramp1End,index_ramp2Start,index_ramp2End)
            if i == 2 or i == 3:
                print("######################")
                print("#start next interval optim")
                print("######################")
    os.chdir('..')
    return results

def getSelectedVariablesValueFromOutput(variable_selected,source_file):
    numberOfSelectedVariable = len(variable_selected)
    data = np.genfromtxt(source_file, dtype = 'str', delimiter = '\t')
    l = len(data[:,0])-1
    variablePosition = np.zeros((numberOfSelectedVariable),dtype = 'int')
    variable_selected_value = np.zeros((l,numberOfSelectedVariable), dtype = 'float')
    for i in range(numberOfSelectedVariable):
        j = np.where(data == variable_selected[i])
        if len(j[0]==1):
            j = j[1]
            variablePosition[i] = j[0]
        else:
            j = j[0]
            variablePosition[i] = j[1]
        variable_selected_value[:,i] = data[1:,variablePosition[i]].astype(float)
    return variable_selected_value

def do_plot():
    cloumnsFR  = np.genfromtxt("Talip2014_release_data.txt",dtype = 'float',delimiter='\t')
    time  = cloumnsFR[:,0]
    FRexp = cloumnsFR[:,1]
    cloumnsRR = np.genfromtxt("Talip2014_rrate_data.txt",dtype = 'float',delimiter='\t')
    temperature = cloumnsRR[:,0]
    RRexp = cloumnsRR[:,1]
    variable_selected = np.array(["Time (h)","Temperature (K)","He fractional release (/)", "He release rate (at/m3 s)"])
    coloumnsOutput_nominal = getSelectedVariablesValueFromOutput(variable_selected,"output.txt")
    timeSequence = coloumnsOutput_nominal[:,0]
    temperatureSequence = coloumnsOutput_nominal[:,1]
    FRsciantix_nominal = coloumnsOutput_nominal[:,2]
    RRsciantix_nominal = coloumnsOutput_nominal[:,3]
    coloumnsOutput_wholeOptim = getSelectedVariablesValueFromOutput(variable_selected,"output_wholeOptim.txt")
    FRsciantix_optim = coloumnsOutput_wholeOptim[:,2]
    RRsciantix_optim = coloumnsOutput_wholeOptim[:,3]
    coloumnsOutput_ramp1 = getSelectedVariablesValueFromOutput(variable_selected,"output_firstRampOptim.txt")
    time1 = coloumnsOutput_ramp1[:,0]
    temperature1 = coloumnsOutput_ramp1[:,1]
    FR1sciantix = coloumnsOutput_ramp1[:,2]
    RR1sciantix = coloumnsOutput_ramp1[:,3]
    coloumnsOutput_ramp2 = getSelectedVariablesValueFromOutput(variable_selected,"output_secondRampOptim.txt")
    time2 = coloumnsOutput_ramp2[:,0]
    temperature2 = coloumnsOutput_ramp2[:,1]
    FR2sciantix = coloumnsOutput_ramp2[:,2]
    RR2sciantix = coloumnsOutput_ramp2[:,3]

    fig, ax = plt.subplots(1,2)
    plt.subplots_adjust(left=0.1,
                        bottom=0.1,
                        right=0.9,
                        top=0.9,
                        wspace=0.34,
                        hspace=0.4)

    ax[0].scatter(time, FRexp, marker = '.', c = '#B3B3B3', label='Data from Talip et al. (2014)')
    ax[0].plot(timeSequence, FRsciantix_nominal, color = '#98E18D', label='SCIANTIX 2.0 Nominal')
    ax[0].plot(timeSequence, FRsciantix_optim, 'k',linestyle = '--',marker = 'o', label='Optim interval: two ramps')
    ax[0].plot(time1, FR1sciantix, color = 'blue', linestyle ='--',marker = 'x', label = 'Optim interval: first ramp')
    ax[0].plot(time2, FR2sciantix, color = '#800080', linestyle ='--', marker = '+',label = 'Optim interval: second ramp')


    axT = ax[0].twinx()
    axT.set_ylabel('Temperature (K)')
    axT.plot(timeSequence, temperatureSequence, 'r', linewidth=1, label="Temperature")

    # ax.set_title(file + ' - Fractional release')
    ax[0].set_xlabel('Time (h)')
    ax[0].set_ylabel('Helium fractional release (/)')
    h1, l1 = ax[0].get_legend_handles_labels()
    h2, l2 = axT.get_legend_handles_labels()
    # ax[0].legend(h1+h2, l1+l2)
    ax[0].legend(loc = 'upper left')

    """ Plot: Helium release rate """
    ax[1].scatter(temperature, RRexp, marker = '.', c = '#B3B3B3', label='Data from Talip et al. (2014)')
    ax[1].plot(temperatureSequence, RRsciantix_nominal, color = '#98E18D', label='SCIANTIX 2.0 Nominal')
    ax[1].plot(temperatureSequence, RRsciantix_optim, 'k', linestyle = '--', marker = 'o',label='Optim interval: two ramps')
    ax[1].plot(temperature1, RR1sciantix, color = 'blue', linestyle ='--', marker = 'x',label = 'Optim interval: first ramp')
    ax[1].plot(temperature2, RR2sciantix, color = '#800080', linestyle ='--',marker = '+', label = 'Optim interval: second ramp')


    # ax.set_title(file + ' - Release rate')
    ax[1].set_xlabel('Temperature (K)')
    ax[1].set_ylabel('Helium release rate (at m${}^{-3}$ s${}^{-1}$)')
    ax[1].legend()

    # plt.savefig(file + '.png')
    plt.show()

    return 0

def sizeEqual(a,b):
    length = min(len(a),len(b))
    a = a[0:length]
    b = b[0:length]
    return a,b
    
def copyFromFolderInUpperDic_to_currentDic(a,b):

    current_directory = os.getcwd()
    if os.path.isfile(a):
        os.remove(a)
    
    parent_directory = os.path.dirname(current_directory)
    b_directory = os.path.join(parent_directory, b)

    if os.path.exists(b_directory):
        a_file = os.path.join(b_directory, a)
        if os.path.isfile(a_file):
            shutil.copy(a_file, current_directory)
            print(f"Copy {a} from {b} successfully")
        else:
            print(f"There is no {a} in {b}")
    else:
        print(f"There is no {b} in upper dictionary")
    
    return 0


#######################
# RUN CODE
#######################
print("1: Plot a case selected")
print("2: Plot all 5 cases")
print("3: Optimize a case selected")
print("4: Optimize a case selected, then plot it")

while True:
    answer = input("Entering your choice(from 1 to 4): ")
    try:
        answer_input = int(answer)
        if answer_input == 1:
            print("0 for Talip2014_1320K")
            print("1 for Talip2014_1400K_b")
            print("2 for Talip2014_1400K_c")
            print("3 for Talip2014_1600K")
            print("4 for Talip2014_1800K")
            while True:
                caseSelected = input("Entering your choice (from 0 to 4): ")
                try:
                    case = int(caseSelected)
                    if 0 <= case <= 4:
                        if case == 0:
                            os.chdir("test_Talip2014_1320K")
                        elif case == 1:
                            os.chdir("test_Talip2014_1400K_b")
                        elif case == 2:
                            os.chdir("test_Talip2014_1400K_c")
                        elif case == 3:
                            os.chdir("test_Talip2014_1600K")
                        else:
                            os.chdir("test_Talip2014_1800K")
                        do_plot()
                        os.chdir('..')
                        break
                    else:
                        print("Input is out of range, try again.")
                except ValueError:
                    print("Invalid input, try again.")
            break
        elif answer_input == 2:
            for i in range(5):
                if i == 0:
                    os.chdir("test_Talip2014_1320K")
                elif i == 1:
                    os.chdir("test_Talip2014_1400K_b")
                elif i == 2:
                    os.chdir("test_Talip2014_1400K_c")
                elif i == 3:
                    os.chdir("test_Talip2014_1600K")
                else:
                    os.chdir("test_Talip2014_1800K")
                do_plot()
                os.chdir('..')
            break
        elif answer_input == 3:
            initial = initialization()
            setInputOutput = forInputOutput()
            optimization(initial,setInputOutput)
            break
        elif answer_input == 4:
            initial = initialization()
            setInputOutput = forInputOutput()
            optimization(initial,setInputOutput)
            do_plot()
            os.chdir('..')
            break
        else:
            print("Your input is out of the range, try again")
    except ValueError:
        print("Invalid input")
















    # if os.path.exists("output_twoRamps.txt"):
    #     coloumnsOutput_twoRamps = getSelectedVariablesValueFromOutput(variable_selected,"output_twoRamps.txt")
    #     FRsciantix_optim = coloumnsOutput_twoRamps[:,2]
    #     RRsciantix_optim = coloumnsOutput_twoRamps[:,3]
    # if os.path.exists("output_firstRampOptim.txt"):
    #     coloumnsOutput_ramp1 = getSelectedVariablesValueFromOutput(variable_selected,"output_firstRampOptim.txt")
    #     time1 = coloumnsOutput_ramp1[:,0]
    #     temperature1 = coloumnsOutput_ramp1[:,1]
    #     FR1sciantix = coloumnsOutput_ramp1[:,2]
    #     RR1sciantix = coloumnsOutput_ramp1[:,3]
    # if os.path.exists("output_secondRampOptim.txt"):
    #     coloumnsOutput_ramp2 = getSelectedVariablesValueFromOutput(variable_selected,"output_secondRampOptim.txt")
    #     time2 = coloumnsOutput_ramp2[:,0]
    #     temperature2 = coloumnsOutput_ramp2[:,1]
    #     FR2sciantix = coloumnsOutput_ramp2[:,2]
    #     RR2sciantix = coloumnsOutput_ramp2[:,3]

    # if os.path.exists("output_keyPointsOptim.txt"):
    #     coloumnsOutput_keyPoint = getSelectedVariablesValueFromOutput(variable_selected,"output_keyPointsOptim.txt")
    #     FRkeyPoint = coloumnsOutput_keyPoint[:,2]
    #     RRkeyPoint = coloumnsOutput_keyPoint[:,3]
    # if os.path.exists("output_2ndPeakFeatureOptim.txt"):
    #     coloumnsOutput_2ndPeakFeature = getSelectedVariablesValueFromOutput(variable_selected,"output_2ndPeakFeatureOptim.txt")
    #     FR2ndPeakFeature = coloumnsOutput_2ndPeakFeature[:,2]
    #     RR2ndPeakFeature = coloumnsOutput_2ndPeakFeature[:,3]
    # if os.path.exists("output_wholeRangeReduceOptim.txt"):
    #     coloumnsOutput_wholeRange = getSelectedVariablesValueFromOutput(variable_selected, "output_wholeRangeReduceOptim.txt")
    #     FRWholRangeReduce = coloumnsOutput_wholeRange[:,2]
    #     RRWholRangeReduce = coloumnsOutput_wholeRange[:,3]