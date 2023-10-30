import os
import shutil
import numpy as np
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import scipy.optimize as optimize
from scipy.optimize import Bounds


class experiment():
    def __init__(self) -> None:
        pass
    def setCase(self,caseName):
        """
        caseName: string, for example: "test_Talip_1320K"
        """
        self.caseName = caseName
        #########
        #get data
        #########
        os.chdir(self.caseName)
        cloumnsFR  = np.genfromtxt("Talip2014_release_data.txt",dtype = 'float',delimiter='\t')
        cloumnsRR = np.genfromtxt("Talip2014_rrate_data.txt",dtype = 'float',delimiter='\t')
        variable_selected = np.array(["Time (h)","Temperature (K)","He fractional release (/)", "He release rate (at/m3 s)"])
        coloumnsOutput_nominal = getSelectedVariablesValueFromOutput(variable_selected,"output.txt")
        history = np.genfromtxt("input_history.txt")

        #############
        #nomenclature
        #############
        Helium_total = 1.68e24
        time_exp  = cloumnsFR[:,0]
        FR_exp = cloumnsFR[:,1]
        temperature_exp = cloumnsRR[:,0]
        RR_exp = cloumnsRR[:,1]

        time_sciantix = coloumnsOutput_nominal[:,0]
        temperature_sciantix = coloumnsOutput_nominal[:,1]

        FR = np.zeros_like(time_sciantix)
        RR = np.zeros_like(temperature_sciantix)

        FR_smoothed = moving_average(FR_exp,100)
        # RR_smoothed = moving_average(RR_exp,10) #this only is used in the first region
        FR_rr = np.zeros_like(FR)
        RR_fr = np.zeros_like(RR)

        time_history = history[:,0]
        temperature_history = history[:,1]

        #############
        #interpolate: inserted time_sciantix -----> FR and RR 
        #############

        #region1 : time_sciantix < max(time_exp)
        index_max_time_exp = findClosestIndex_1D(time_sciantix, max(time_exp))
        if time_sciantix[index_max_time_exp] > max(time_exp):
            index_max_time_exp = index_max_time_exp - 1

        for i in range(1,index_max_time_exp + 1):
            if time_sciantix[i] == time_sciantix[i-1]:
                FR[i] = FR[i-1]
                RR_fr[i] = RR_fr[i-1]
                RR[i] = RR[i-1]
            else:
                FR[i] = interpolate_1D(time_exp, FR_smoothed, time_sciantix[i])
                RR_fr[i] =(FR[i]-FR[i-1])/(time_sciantix[i]-time_sciantix[i-1])/3600*Helium_total
                RR[i] = interpolate_2D(temperature_exp, RR_exp, temperature_sciantix[i], RR_fr[i])

        # region2 : 
        for i in range(index_max_time_exp+1,len(time_sciantix)):
            state,temperature_state_start, temperature_state_end = plateauIdentify(time_history,temperature_history,time_sciantix[i])

            if state == "increase" and FR[i-1]<1:
                index_state_end = findClosestIndex_1D(temperature_exp, temperature_state_end)
                RR[i] = interpolate_1D(temperature_exp[:index_state_end], RR_exp[:index_state_end], temperature_sciantix[i])
                FR[i] = FR[i-1] + RR[i] * (time_sciantix[i]-time_sciantix[i-1]) * 3600/Helium_total
                if FR[i] > 1:
                    FR[i] =1
                    RR[i] = 0
            elif state == "plateau" and FR[i-1] < 1:
                slop = (FR[i-1] - FR[i-3])/(time_sciantix[i-1]-time_sciantix[i-3])
                time_saturation = time_sciantix[i-1] + (1-FR[i-1])/slop
                if time_sciantix[i] < time_saturation:
                    FR[i] = FR[i-1] + (time_sciantix[i]-time_sciantix[i-1])*slop
                    RR[i] = FR[i-1]
                else:
                    FR[i] = 1
                    RR[i] = 0
            elif state == "decrease" and FR[i-1] < 1:
                index_state_start = len(temperature_exp) - findClosestIndex_1D(temperature_exp[::-1], temperature_state_start) -1
                RR[i] = interpolate_1D(temperature_exp[index_state_start:],RR_exp[index_state_start:], temperature_sciantix[i])
                FR[i] = FR[i-1] + RR[i] * (time_sciantix[i]-time_sciantix[i-1]) * 3600/Helium_total
                if FR[i] > 1:
                    FR[i] =1
                    RR[i] = 0
            else: #FR[i-1] = 1
                FR[i] = 1
                RR[i] = 0
        self.RR = RR
        self.FR = FR

class inputOutput():
    def __init__(self):

        # now build a new folder in selected benchmark folder
        folder_name = "Optimization"
        if not os.path.exists(folder_name):
            os.makedirs(folder_name)
        else:
            shutil.rmtree(folder_name)
            os.makedirs(folder_name)
    
        os.chdir(folder_name)
        shutil.copy("../input_scaling_factors.txt", os.getcwd())
        shutil.copy("../../../bin/sciantix.x", os.getcwd())
        shutil.copy("../input_initial_conditions.txt", os.getcwd())
        shutil.copy("../input_settings.txt", os.getcwd())
        shutil.copy("../input_history.txt", os.getcwd())

        self.scaling_factors = {}
        with open("input_scaling_factors.txt", 'r') as file:
            lines = file.readlines()
        sf_name = []
        i = 0
        while i < len(lines):
            value = float(lines[i].strip())
            sf_name.append(lines[i + 1].strip()[len("# scaling factor - "):])
            self.scaling_factors[sf_name[-1]] = value
            i += 2


    def writeInputScalingFactors(self,sf_selected,sf_selected_value):
        # assign new sf value and write the file, and run sciantix.x
        for i in range(len(sf_selected)):
            self.scaling_factors[sf_selected[i]] = sf_selected_value[i]

        with open("input_scaling_factors.txt",'w') as file:
            for key, value in self.scaling_factors.items():
                file.write(f'{value}\n')
                file.write(f'# scaling factor - {key}\n')
        
        self.sf_selected_value = sf_selected_value

        os.system("./sciantix.x")
    
    def readOutput(self):
        # here, in Talip cases, we only interesed in FR and RR since we only have these two variables experimental data
        self.variable_selected = np.array(("He fractional release (/)", "He release rate (at/m3 s)"))
        self.variable_selected_value = getSelectedVariablesValueFromOutput(self.variable_selected,"output.txt")
        self.FR_sciantix = self.variable_selected_value[:,0]
        self.RR_sciantix = self.variable_selected_value[:,1]

class optimization():
    def __init__(self) -> None:
        pass
    def setScalingFactors(self,*args):
        self.sf_selected = []
        for arg in args:
            self.sf_selected.append(arg)
        self.sf_selected_initial_value = np.ones([len(self.sf_selected)])
        # self.sf_selected_initial_value = np.array([1.79947,0.998,0.9916,0.979])
        self.sf_selected_bounds = np.zeros([2,len(self.sf_selected_initial_value)])
        for i in range(len(self.sf_selected)):
            if self.sf_selected[i] == "helium diffusivity pre exponential":
                self.sf_selected_bounds[0,i] = 0.5
                self.sf_selected_bounds[1,i] = 19.9
            elif self.sf_selected[i] == "helium diffusivity activation energy":
                self.sf_selected_bounds[0,i] = 0.835
                self.sf_selected_bounds[1,i] = 1.2
            elif self.sf_selected[i] == "henry constant pre exponential":
                self.sf_selected_bounds[0,i] = 0.0627
                self.sf_selected_bounds[1,i] = 16.09
            elif self.sf_selected[i] == "henry constant activation energy":
                self.sf_selected_bounds[0,i] = 0.431
                self.sf_selected_bounds[1,i] = 1.55
            else:
                self.sf_selected_bounds[0,i] = 0
                self.sf_selected_bounds[0,i] = float('inf')
        self.bounds = Bounds(self.sf_selected_bounds[0,:],self.sf_selected_bounds[1,:])
    def optimization(self,experiment,inputOutput):
        FR_exp = experiment.FR
        RR_exp = experiment.RR
        scaling_factors = inputOutput.scaling_factors
        
        def costFunction(sf_selected_value):
            inputOutput.writeInputScalingFactors(self.sf_selected,sf_selected_value)
            inputOutput.readOutput()
            FR_sciantix = inputOutput.FR_sciantix
            RR_sciantix = inputOutput.RR_sciantix
            current_sf_selected_value = inputOutput.sf_selected_value
            print(f"current value: {current_sf_selected_value}")
            error_related = np.zeros_like(FR_sciantix)
            # error = np.sum(abs(FR_exp/max(FR_exp) - FR_sciantix/max(FR_exp)))
            error = np.sum(abs(RR_exp/max(RR_exp) - RR_sciantix/max(RR_exp)))


            # for i in range(len(RR_sciantix)):
            #     if RR_exp[i] == 0 and RR_sciantix[i] !=0:
            #         error_related[i] = abs(RR_sciantix[i]/max(RR_exp))
            #     elif RR_exp[i] == 0 and RR_sciantix[i] == 0:
            #         error_related[i] = 0

            #     else:
            #         error_related[i] = abs((RR_exp[i]-RR_sciantix[i])/RR_exp[i])
            # with open("error_related.txt", 'w') as file:
            #     for i in range(len(RR_sciantix)):
            #         file.write(f'{error_related[i]}\n')
            # error = np.sum(error_related)
            for i in range(len(FR_sciantix)):
                if FR_exp[i] == 0:
                    error_related[i] = 0
                else:
                    error_related[i] = abs((FR_exp[i]-FR_sciantix[i])/FR_exp[i])
            error = np.sum(error_related)

            # error = np.sum(abs((RR_exp-RR_sciantix)/(RR_exp)))




            print(f"current error: {error}")
            return error
        

        minimum_variation = np.array([0.0001, 0.00001, 0, 0])
        def custom_callback(x):
            variations = [abs(x[i] - self.sf_selected_initial_value[i]) for i in range(len(x))]
            return [variations[i] - minimum_variation[i] for i in range(len(x))]

        # results = optimize.minimize(costFunction,self.sf_selected_initial_value, method = 'SLSQP',bounds=self.bounds)
        results = optimize.minimize(costFunction,self.sf_selected_initial_value, method = 'SLSQP',bounds=self.bounds,constraints = {'type': 'ineq', 'fun': custom_callback})
        for i in range(len(self.sf_selected)):
            scaling_factors[self.sf_selected[i]] = results.x[i]
            print(f"{self.sf_selected[i]}:{results.x[i]}")
        print(f"Final error:{results.fun}")

        file_name = "input_scaling_factors.txt"
        with open(file_name,'w') as file:
            for key, value in scaling_factors.items():
                file.write(f'{value}\n')
                file.write(f'# scaling factor - {key}\n')
        os.system("./sciantix.x")

        os.chdir('..')
        os.chdir('..')
        return results


##########
#functions
##########
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

def moving_average(data, window_size):
    half_window = window_size // 2
    smoothed_data = np.convolve(data, np.ones(window_size) / window_size, mode='same')
    smoothed_data[:half_window] = smoothed_data[half_window]
    smoothed_data[-half_window:] = smoothed_data[-half_window]
    return smoothed_data

def findClosestIndex_1D(source_data, targetValue):
    """
    find the index of the value in source_data that closest to targetValue
    """
    differences = [abs(x - targetValue) for x in source_data]
    index = int(np.argmin(differences))
    return index

def findClosedIndex_2D(source_data1, source_data2, targetElement1, targetElement2):
    differences1 = [abs(x - targetElement1) for x in source_data1]
    differences2 = [abs(y - targetElement2) for y in source_data2]

    differences = np.array(differences1)/np.max(differences1) + np.array(differences2)/np.max(differences2)
    index = np.argmin(differences)
    return index

def interpolate_1D(source_data_x, source_data_y, inserted_x):
    differences = np.array([(x - inserted_x) for x in source_data_x])

    index = np.argmin(abs(differences))
    # print(index,inserted_x,source_data_x[index])
    if differences[index] == 0 or index == len(source_data_x)-1 or index == 0:
        value_interpolated = source_data_y[index]
    
    else:
        for i in range(min(index, len(source_data_x)-index-1)):
            if np.sign(differences[index - i]) != np.sign(differences[index+i]):
                index_low = index-i
                index_up = index+i
                up_y = source_data_y[index_up]
                low_y = source_data_y[index_low]
                up_x = source_data_x[index_up]
                low_x = source_data_x[index_low]
                if up_x == low_x:
                    value_interpolated = source_data_y[index]
                else:
                    slop = (up_y - low_y)/(up_x-low_x)
                    value_interpolated = low_y + slop * (inserted_x - low_x)
                break;
            else:
                value_interpolated = source_data_y[index]
                break;

    return value_interpolated

def interpolate_2D(source_data_x, source_data_y, inserted_x, inserted_y):

    differences1 = np.array([abs((x - inserted_x)/inserted_x) for x in source_data_x])
    index_x = np.where(differences1<0.02)[0]
    source_data_y_x = source_data_y[index_x]
    if inserted_y == 0:
        value_interpolated = np.average(source_data_y_x)
    else:    
        differences2 = np.array([abs((y - inserted_y)/inserted_y) for y in source_data_y_x])
        index_y = np.where(differences2<0.02)[0]
        if len(index_y) == 0:
            index_y = np.argmin(differences2)
        index = index_x[index_y]
        value_interpolated = np.average(source_data_y[index])

    return value_interpolated
    
def plateauIdentify(time_history, temperature_history, time):
    index_low = np.where(time_history <= time)[0][-1]
    if time < time_history[-1]:
        index_up = np.where(time_history > time)[0][0]
    else:
        index_up = len(time_history) - 1
    difference = temperature_history[index_up] - temperature_history[index_low]
    if difference == 0:
        state = "plateau"
    elif difference < 0:
        state = "decrease"
    else:
        state = "increase"
    temperature_state_start = temperature_history[index_low]
    temperature_state_end = temperature_history[index_up]

    return state, temperature_state_start, temperature_state_end



Talip1320 = experiment()
Talip1320.setCase("test_Talip2014_1320K")
setInputOutput = inputOutput()
Optim = optimization()
Optim.setScalingFactors("helium diffusivity pre exponential", "helium diffusivity activation energy","henry constant pre exponential","henry constant activation energy")
Optim.optimization(Talip1320,setInputOutput)




#####
#plot
#####

os.chdir("test_Talip2014_1320K")
cloumnsFR  = np.genfromtxt("Talip2014_release_data.txt",dtype = 'float',delimiter='\t')
cloumnsRR = np.genfromtxt("Talip2014_rrate_data.txt",dtype = 'float',delimiter='\t')
variable_selected = np.array(["Time (h)","Temperature (K)","He fractional release (/)", "He release rate (at/m3 s)"])
coloumnsOutput_nominal = getSelectedVariablesValueFromOutput(variable_selected,"output.txt")
history = np.genfromtxt("input_history.txt")
os.chdir("Optimization")
coloumnOutput_new = getSelectedVariablesValueFromOutput(variable_selected,"output.txt")

Helium_total = 1.68e24
time_exp  = cloumnsFR[:,0]
FR_exp = cloumnsFR[:,1]
temperature_exp = cloumnsRR[:,0]
RR_exp = cloumnsRR[:,1]

time_sciantix = coloumnsOutput_nominal[:,0]
temperature_sciantix = coloumnsOutput_nominal[:,1]
FR_nominal = coloumnsOutput_nominal[:,2]
RR_nominal = coloumnsOutput_nominal[:,3]

FR_interpolated = Talip1320.FR
RR_interpolated = Talip1320.RR

FR_new = coloumnOutput_new[:,2]
RR_new = coloumnOutput_new[:,3]

fig, ax = plt.subplots(1,2)
plt.subplots_adjust(left=0.1,
                    bottom=0.1,
                    right=0.9,
                    top=0.9,
                    wspace=0.34,
                    hspace=0.4)

ax[0].scatter(time_exp, FR_exp, marker = '.', c = '#B3B3B3', label='Data from Talip et al. (2014)')
ax[0].scatter(time_sciantix, FR_nominal,marker = 'x' ,color = '#98E18D', label='SCIANTIX 2.0 Nominal')
ax[0].scatter(time_sciantix, FR_new, marker = 'x',color = 'red',label = 'optimized')
ax[0].scatter(time_sciantix, FR_interpolated, marker = 'x',color = 'blue',label = 'interpolated')
axT = ax[0].twinx()
axT.set_ylabel('Temperature (K)')
axT.plot(time_sciantix, temperature_sciantix, 'r', linewidth=1, label="Temperature")

ax[0].set_xlabel('Time (h)')
ax[0].set_ylabel('Helium fractional release (/)')
h1, l1 = ax[0].get_legend_handles_labels()
h2, l2 = axT.get_legend_handles_labels()
# ax[0].legend(h1+h2, l1+l2)
ax[0].legend(loc = 'upper left')

""" Plot: Helium release rate """
ax[1].scatter(temperature_exp, RR_exp, marker = '.', c = '#B3B3B3', label='Data from Talip et al. (2014)')
ax[1].scatter(temperature_sciantix, RR_nominal, marker = 'x',color = '#98E18D', label='SCIANTIX 2.0 Nominal')
ax[1].scatter(temperature_sciantix, RR_new, marker = 'x', color = 'red',label = 'optimized')
ax[1].scatter(temperature_sciantix, RR_interpolated, marker = 'x', color = 'blue',label = 'interpolated')


# ax.set_title(file + ' - Release rate')
ax[1].set_xlabel('Temperature (K)')
ax[1].set_ylabel('Helium release rate (at m${}^{-3}$ s${}^{-1}$)')
ax[1].legend()

# plt.savefig(file + '.png')
plt.show()



