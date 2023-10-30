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
    index_x = np.where(differences1<0.03)[0]
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

   


#########
#get data
#########
# os.chdir("test_Talip2014_1320K")
# os.chdir("test_Talip2014_1400K_b")
# os.chdir("test_Talip2014_1400K_c")
os.chdir("test_Talip2014_1600K")
# os.chdir("test_Talip2014_1800K")
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
# temperature_region2_start = temperature_sciantix[index_max_time_exp+1]
# index_region2_start = findClosestIndex_1D(temperature_exp, temperature_region2_start)
for i in range(1,index_max_time_exp + 1):
# for i in range(1,5):
    
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

# print(RR[0:5])
# print(RR)
######
#plot
######


fig, ax = plt.subplots(1,2)
plt.subplots_adjust(left=0.1,
                    bottom=0.1,
                    right=0.9,
                    top=0.9,
                    wspace=0.34,
                    hspace=0.4)

ax[0].scatter(time_exp, FR_exp, marker = '.', c = '#B3B3B3', label='Data from Talip et al. (2014)')
# ax[0].scatter(time, FRexp_smooth, marker= 'x', c = 'red', label = 'smoothed')
# ax[0].scatter(time, FRexp_smooth_sa, marker= 'x', c = 'green', label = 'smoothed')
ax[0].scatter(time_sciantix, FR, marker= 'x', c = 'red', label = 'interpolated')

axT = ax[0].twinx()
axT.set_ylabel('Temperature (K)')
axT.plot(time_sciantix, temperature_sciantix, 'r', linewidth=1, label="Temperature")
# axT.scatter(time_sciantix, temperature_sciantix, marker = 'o', c= 'r', label="Temperature")
# ax.set_title(file + ' - Fractional release')
ax[0].set_xlabel('Time (h)')
ax[0].set_ylabel('Helium fractional release (/)')
h1, l1 = ax[0].get_legend_handles_labels()
h2, l2 = axT.get_legend_handles_labels()
# ax[0].legend(h1+h2, l1+l2)
ax[0].legend(loc = 'upper left')

""" Plot: Helium release rate """
ax[1].scatter(temperature_exp, RR_exp, marker = '.', c = '#B3B3B3', label='Data from Talip et al. (2014)')
ax[1].scatter(temperature_sciantix, RR, marker= 'x', c = 'red', label = 'interpolated')
# ax[1].scatter(temperature_exp, RR_smoothed, marker= 'x', c = 'green', label = 'smoothed')
# ax[1].scatter(temperature, RRexp_smooth_vw, marker= 'x', c = 'green', label = 'smoothed')
# ax[1].scatter(temperatureSequence[0:index_time_endFR+1], RRfromFRmatched, marker = 'x', c = 'red', label='drived from FR')

# ax[1].scatter(temperatureMatched, RRexpMatched, marker= 'x', c = 'green', label ='matched')



# ax.set_title(file + ' - Release rate')
ax[1].set_xlabel('Temperature (K)')
ax[1].set_ylabel('Helium release rate (at m${}^{-3}$ s${}^{-1}$)')
ax[1].legend()

# plt.savefig(file + '.png')
plt.show()





