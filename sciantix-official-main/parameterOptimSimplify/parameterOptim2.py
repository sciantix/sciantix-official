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
        folder_name = "test_Talip2014_1320K"
        
        os.chdir(folder_name)

        FRfile_name = "Talip2014_release_data.txt"
        RRfile_name = "Talip2014_rrate_data.txt"
        FRexp = np.genfromtxt(FRfile_name, dtype= 'str', delimiter='\t').astype(float)
        RRexp = np.genfromtxt(RRfile_name, dtype= 'str', delimiter='\t').astype(float)

        time = FRexp[:,0]
        fractionRelease = FRexp[:,1]
        temperature = RRexp[:,0]
        releaseRate = RRexp[:,1]

        FRintegral = 0
        RRintegral = 0
        # Calculate FRintegral
        for i in range(len(time) - 1):
            FRintegral += ((fractionRelease[i + 1] + fractionRelease[i])/2) * (time[i + 1] - time[i])
        # Calculate RRintegral
        for i in range(len(temperature) - 1):
            RRintegral += ((releaseRate[i + 1] + releaseRate[i])/2) * (temperature[i + 1] - temperature[i])
        # Assign the calculated values to the respective class attributes
        self.FRintegral = FRintegral
        self.RRintegral = RRintegral

        
        
        self.tMax = max(time)


        os.path.exists("sciantix.x")
        os.remove("sciantix.x")
        shutil.copy("../sciantix.x", os.getcwd())

        file_name  = "input_scaling_factors.txt"
        self.scaling_factors = {}
        with open(file_name, 'r') as file:
            lines = file.readlines()
            i = 0
            l = len(lines)
        
        numberOfParameter = 4
        
        self.sf_name = []
        while i < l:
            value = float(lines[i].strip())
            self.sf_name.append(lines[i + 1].strip()[len("# scaling factor - "):])
            self.scaling_factors[self.sf_name[-1]] = value
            print(f"{int(i/2)} for {self.sf_name[-1]}")
            i += 2

        self.numberSelection = numberOfParameter
        parameterCodeName = np.zeros(self.numberSelection)
        self.parameterSelection = np.zeros(self.numberSelection,dtype = int)
        self.sf_selection = []
        self.sf_selection_initialValue = np.ones([self.numberSelection])
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

        # we can also add a section to let the user to select the variables that we need in output file, but in Talip cases, for sure, are FR and RR
    def getSelectedScalingFactorsInitialValue(self):
        return self.sf_selection_initialValue
    def getSelectedScalingFactorsBounds(self):
        return self.sf_selection_bounds
    def getSlectedScalingFactorsName(self):
        return self.sf_selection
    def getScalingFactors(self):
        return self.scaling_factors
    def getMaxExpTime(self):
        return self.tMax
    def getFRintegral(self):
        return self.FRintegral
    def getRRintegral(self):
        return self.RRintegral
    
    def setCase(self, name):
        self.name = name

    # def experimentalData(self, data_name):
    #     self.expdata = np.gen
    #     self.

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

    def readOutput(self):
        # here, in Talip cases, we only interesed in FR and RR since we only have these two variables experimental data
        self.variable_selected = np.array(("Time (h)","Temperature (K)","He fractional release (/)", "He release rate (at/m3 s)"))
        self.variable_selected_value = getSelectedVariablesValueFromOutput(self.variable_selected,"output.txt")
        self.fractionRelease_sciantix = self.variable_selected_value[:,2]
        self.temperature_sciantix = self.variable_selected_value[:,1]
        self.releaseRate_sciantix = self.variable_selected_value[:,3]
        self.time_sciantix = self.variable_selected_value[:,0]
        
    def getFRValue(self):
        return self.fractionRelease_sciantix
    def getRRValue(self):
        return self.releaseRate_sciantix
    def getTime(self):
        return self.time_sciantix
    def getTemperature(self):
        return self.temperature_sciantix
    def getCurrentSlectedScalingFactorsValue(self):
        return self.sf_selected_value


def optimization(initial,setInputOutput):
    scaling_factors = initial.getScalingFactors()
    sf_selected = initial.getSlectedScalingFactorsName()
    sf_selected_initial_value = initial.getSelectedScalingFactorsInitialValue()
    sf_selected_bounds = initial.sf_selection_bounds()
    bounds = Bounds(sf_selected_bounds[0,:],sf_selected_bounds[1,:])
    tMaxExp = initial.getMaxExpTime()
    FRintegral_exp = initial.getFRintegral()
    RRintegral_exp = initial.getRRintegral()
    
    def costFunction(sf_selected_value):
        setInputOutput.writeInputScalingFactors(scaling_factors,sf_selected,sf_selected_value)
        setInputOutput.readOutput()
        FRsciantix = setInputOutput.getFRValue()
        RRsciantix = setInputOutput.getRRValue()
        timeSciantix = setInputOutput.getTime()
        temperatureSciantix = setInputOutput.getTemperature()
        tMaxExpIndex = np.where(timeSciantix >= tMaxExp)[0]
        tMaxExpIndex = tMaxExpIndex[0]
        current_sf_selected_value = setInputOutput.getCurrentSlectedScalingFactorsValue()
        print(f"current value: {current_sf_selected_value}")

        FRintegral = 0
        RRintegral = 0
        # Calculate FRintegral
        for i in range(tMaxExpIndex - 1):
            FRintegral += ((FRsciantix[i + 1] + FRsciantix[i - 1])/2) * (timeSciantix[i + 1] - timeSciantix[i])
        # Calculate RRintegral
        for i in range(len(RRsciantix) - 1):
            RRintegral += ((RRsciantix[i + 1] + RRsciantix[i - 1])/2) * (temperatureSciantix[i + 1] - temperatureSciantix[i])
        # Assign the calculated values to the respective variables
        FRintegral = FRintegral
        RRintegral = RRintegral
        
        # error = abs((FRintegral_exp-FRintegral)/FRintegral_exp) # use this error you can find the optimization is work
        error = abs((FRintegral_exp-FRintegral)/FRintegral_exp)+abs((RRintegral_exp-RRintegral)/RRintegral_exp)

        # error = abs((FRsciantix[tMaxExpIndex] - 0.103517702)/0.103517702)

        print(f"current error: {error}")
        return error
    results = optimize.minimize(costFunction,sf_selected_initial_value, method = 'SLSQP',bounds=bounds)
    for i in range(4):
        print(f"{sf_selected[i]}:{results.x[i]}")
    print(f"Final error:{results.fun}")

    l = len(sf_selected)
    for i in range(l):
        scaling_factors[sf_selected[i]] = results.x[i]
    file_name = "input_scaling_factors.txt"
    with open(file_name,'w') as file:
        for key, value in scaling_factors.items():
            file.write(f'{value}\n')
            file.write(f'# scaling factor - {key}\n')

    os.system("./sciantix.x")


    os.chdir('..')
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
    os.chdir('test_Talip2014_1320K')
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

    os.chdir("Optimization")
    coloumnsOutput_optim = getSelectedVariablesValueFromOutput(variable_selected,"output.txt")
    FRsciantix_optim = coloumnsOutput_optim[:,2]
    RRsciantix_optim = coloumnsOutput_optim[:,3]
    os.chdir('..')
    os.chdir('..')

    fig, ax = plt.subplots(1,2)
    plt.subplots_adjust(left=0.1,
                        bottom=0.1,
                        right=0.9,
                        top=0.9,
                        wspace=0.34,
                        hspace=0.4)

    ax[0].scatter(time, FRexp, marker = '.', c = 'red', label='Data from Talip et al. (2014)')
    ax[0].plot(timeSequence, FRsciantix_nominal, color = '#98E18D', label='SCIANTIX 2.0 Nominal')
    ax[0].plot(timeSequence, FRsciantix_optim, 'k',linestyle = '--', label='Optimization')

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
    ax[1].scatter(temperature, RRexp, marker = '.', c = 'red', label='Data from Talip et al. (2014)')
    ax[1].plot(temperatureSequence, RRsciantix_nominal, color = '#98E18D', label='SCIANTIX 2.0 Nominal')
    ax[1].plot(temperatureSequence, RRsciantix_optim, 'k', linestyle = '--',label='Optimization')


    # ax.set_title(file + ' - Release rate')
    ax[1].set_xlabel('Temperature (K)')
    ax[1].set_ylabel('Helium release rate (at m${}^{-3}$ s${}^{-1}$)')
    ax[1].legend()

    # plt.savefig(file + '.png')
    plt.show()

    return 0


# def temperatureToTime(temperatureSequnce,timeSequence, target)


def findClosestIndex(source_data, targetValue):
    differences = [abs(x - targetValue) for x in source_data]
    index = np.argmin(differences)
    return index

def centered_moving_average(data, window_size):
    half_window = window_size // 2
    smoothed_data = np.convolve(data, np.ones(window_size) / window_size, mode='same')
    smoothed_data[:half_window] = smoothed_data[half_window]
    smoothed_data[-half_window:] = smoothed_data[-half_window]
    return smoothed_data


initial = initialization()
setInputOutput = forInputOutput()
optimization(initial, setInputOutput)


do_plot()

# talip1320 = initialization()
# talip1320.setCase("Talip..")

# optime = class1()
# talip1320.sf_selection_bounds

