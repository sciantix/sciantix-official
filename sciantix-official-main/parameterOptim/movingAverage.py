import os
import shutil
import numpy as np
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import scipy.optimize as optimize
from scipy.optimize import Bounds
from scipy.ndimage import gaussian_filter
from scipy.signal import savgol_filter
from scipy.interpolate import interp1d






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

def findClosestIndex(source_data, targetValue):
    differences = [abs(x - targetValue) for x in source_data]
    index = int(np.argmin(differences))
    return index

def findClosedDataPair(source_data1, source_data2, targetElement1, targetElement2):

    differences1 = [abs(x - targetElement1) for x in source_data1]
    differences2 = [abs(y - targetElement2) for y in source_data2]

    differences = np.array(differences1)/np.max(differences1) + np.array(differences2)/np.max(differences2)
    index = np.argmin(differences)
    return index

def interpolate(source_data_x, source_data_y, inserted_x):
    differences = [abs(x - inserted_x) for x in source_data_x]
    index = int(np.argmin(differences))
    if differences[index] == 0:
        value_interpolated = source_data_y[index]
    else:
        if differences[index + 1] < differences[index -1]:
        #inserted_x is located at the interval: (x[index], x[index+1])
            index_up = index + 1
            index_low = index
        else:
        #inserted_x is located at the interval: (x[index-1], x[index])
            index_up = index
            index_low = index - 1

        up_y = source_data_y[index_up]
        low_y = source_data_y[index_low]
        up_x = source_data_x[index_up]
        low_x = source_data_x[index_low]
        slop = (up_y - low_y)/(up_x-low_x)
        value_interpolated = low_y + slop * (inserted_x - low_x)
    return value_interpolated


os.chdir("test_Talip2014_1320K")
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

## smooth the data


print(max(RRexp))
# time = adaptive_smoothing(time,windowsize)
FRexp_smooth = moving_average(FRexp,100)
# FRexp_smooth_sa = savgol_filter(FRexp, window_length=100, polyorder=1)
# FRexp_smooth_sa = savgol_filter(FRexp, window_length=100, polyorder=2)
# temperature = adaptive_smoothing(temperature,windowsize)
# RRexp_smooth_vw = moving_average_variable_window(temperature,RRexp,1,10)
RRexp_smooth = moving_average(RRexp,10)
# RRexp_smooth_vw = savgol_filter(RRexp, window_length=100, polyorder=8)
print(max(RRexp_smooth))

##

history = np.genfromtxt("input_history.txt")
time_history = history[:,0]
temperature_history = history[:,1]
Number_of_time_steps_per_interval = 100
time_step = np.zeros(len(time_history)-1)
temperature_step = np.zeros(len(temperature_history)-1)
for i in range(len(time_history)-1):
    time_step[i] = (time_history[i+1] - time_history[i])/Number_of_time_steps_per_interval
    temperature_step[i] = (temperature_history[i+1] - temperature_history[i])/Number_of_time_steps_per_interval

time_endFR = time[-1]
index_time_endFR = findClosestIndex(timeSequence,time_endFR)

index_matched_FR = np.zeros(index_time_endFR+1)
# FRmatched = np.zeros(index_time_endFR+1)
FRmatched = np.zeros(len(timeSequence))
timeMatched= np.zeros(index_time_endFR+1)
print(index_time_endFR)
for i in range(len(index_matched_FR)):

    index_matched_FR[i] = findClosestIndex(time, timeSequence[i])
    index = int(index_matched_FR[i])
    FRmatched[i] = FRexp_smooth[index]
    timeMatched[i] = time[index]
    # if i == 0:
    #     RRfromFR[i] = 0;
    # else:
    #     RRfromFR[i] = FRexp
Helium_total = 1.68e24
RRfromFRmatched = np.zeros(index_time_endFR+1)
index_RRpair = np.zeros(index_time_endFR+1)
RRexpMatched = np.zeros(len(temperatureSequence))
# temperatureMatched=np.zeros(index_time_endFR+1)
temperatureMatched = np.zeros(len(temperatureSequence))
RRfromFRmatched[0] = 0;
temperatureMatched[0] = temperature_history[0]
for i in range(1,len(timeMatched)):
    
    RRfromFRmatched[i] = (FRmatched[i] - FRmatched[i-1])*Helium_total/(timeMatched[i]-timeMatched[i-1])/3600
    index_RRpair[i] = findClosedDataPair(temperature,RRexp,temperatureSequence[i],RRfromFRmatched[i])
    index = int(index_RRpair[i])
    RRexpMatched[i] = RRexp[index]
    temperatureMatched[i] = temperature[index]

print(len(temperature))
# print(index_RRpair)



index_secondrampend = int(findClosestIndex(temperature,temperature_history[4]))
index_secondrampend_sciantix = int(findClosestIndex(temperatureSequence, temperature_history[4]))
print(index_secondrampend, temperature_history[4])

# for i in range(int(index_RRpair[-1])+1, index_secondrampend+1):
#     index = int(findClosestIndex(temperature, temperatureSequence[i]))
#     temperatureMatched[i] = temperature(index)
#     RRexpMatched[i] = RRexp(index)

for i in range(len(index_RRpair),index_secondrampend_sciantix+1):
    index = int(findClosestIndex(temperature[int(index_RRpair[-1])+1:index_secondrampend+1],temperatureSequence[i])) + int(index_RRpair[-1])+1
    temperatureMatched[i] = temperature[index]
    RRexpMatched[i] = RRexp[index]
    FRmatched[i] = FRmatched[i-1] + (RRexpMatched[i] * (timeSequence[i]-timeSequence[i-1])*3600)/Helium_total

print(FRmatched[index_secondrampend_sciantix])

print(index_secondrampend_sciantix)

slop = (FRmatched[index_secondrampend_sciantix] - FRmatched[index_secondrampend_sciantix-1])/(timeSequence[index_secondrampend_sciantix]-timeSequence[index_secondrampend_sciantix-1])
time_saturation = timeSequence[index_secondrampend_sciantix]+(1 - FRmatched[index_secondrampend_sciantix])/slop
for i in range(index_secondrampend_sciantix+1,len(temperatureSequence)):
    if timeSequence[i] <= time_saturation:
        FRmatched[i] = FRmatched[i-1] + (timeSequence[i] - timeSequence[i-1])*slop
        RRexpMatched[i] = (FRmatched[i] - FRmatched[i-1])*Helium_total/(timeSequence[i]-timeSequence[i-1])/3600
        temperatureMatched[i] = temperatureSequence[i]
    else:
        FRmatched[i] = 1
        RRexpMatched[i] = 0
        temperatureMatched[i] = temperatureSequence[i]
    












fig, ax = plt.subplots(1,2)
plt.subplots_adjust(left=0.1,
                    bottom=0.1,
                    right=0.9,
                    top=0.9,
                    wspace=0.34,
                    hspace=0.4)

ax[0].scatter(time, FRexp, marker = '.', c = '#B3B3B3', label='Data from Talip et al. (2014)')
# ax[0].scatter(time, FRexp_smooth, marker= 'x', c = 'red', label = 'smoothed')
# ax[0].scatter(time, FRexp_smooth_sa, marker= 'x', c = 'green', label = 'smoothed')
ax[0].scatter(timeSequence, FRmatched, marker= 'x', c = 'red', label = 'matched or derived from RR')

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
ax[1].scatter(temperature, RRexp_smooth, marker= 'x', c = 'red', label = 'smoothed')
# ax[1].scatter(temperature, RRexp_smooth_vw, marker= 'x', c = 'green', label = 'smoothed')
# ax[1].scatter(temperatureSequence[0:index_time_endFR+1], RRfromFRmatched, marker = 'x', c = 'red', label='drived from FR')

# ax[1].scatter(temperatureMatched, RRexpMatched, marker= 'x', c = 'green', label ='matched')



# ax.set_title(file + ' - Release rate')
ax[1].set_xlabel('Temperature (K)')
ax[1].set_ylabel('Helium release rate (at m${}^{-3}$ s${}^{-1}$)')
ax[1].legend()

# plt.savefig(file + '.png')
plt.show()