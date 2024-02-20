import numpy as np
import os
import matplotlib
# matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
from matplotlib import cm
import re, ast


prediction_trace = []


# Open the file for reading.
with open('predicted_trace.txt', 'r') as file:
    # Read the entire file, this assumes that the file is not too large to fit in memory
    data = file.read()

    # Split the data by double newlines, which seems to be your separator between arrays
    array_strings = data.strip().split('\n\n')

    # Iterate over the separated string representations of the arrays
    for array_string in array_strings:
        # Use ast.literal_eval to safely evaluate the string as a Python literal
        array_literal = ast.literal_eval(array_string)
        # Convert the literal to a numpy array and append to the list of arrays
        prediction_trace.append(np.array(array_literal, dtype = 'float'))


params_at_max_prob = np.genfromtxt('params_at_max_prob.txt')
params_optimized = np.genfromtxt('params_optimized.txt')
calibration_data = np.genfromtxt('calibration_data.txt')
optim_exp = np.genfromtxt('optim_data.txt')
talip_data = np.genfromtxt('Talip2014_release_data.txt')
# params = params_optimized.shape[1]
params = params_at_max_prob.ndim
if params == 1:
    plt.plot(optim_exp[:,0], params_at_max_prob, color = 'b', label = 'bayesian_calibration')
    plt.scatter(optim_exp[:,0], params_at_max_prob, color = 'b', marker='o')
    plt.plot(optim_exp[:,0], params_optimized, color = 'g', label = 'optimization')
    plt.scatter(optim_exp[:,0], params_optimized, color = 'g', marker='o')
    plt.xlabel('time / h')
    plt.ylabel('ln(sf) / -')
    plt.legend()
    plt.title('Logarithm scaling factor')
    plt.show()
else:
    for i in range(params):
        plt.plot(optim_exp[:,0], params_at_max_prob[:,i], color = 'b', label = 'bayesian_calibration')
        plt.scatter(optim_exp[:,0], params_at_max_prob[:,i], color = 'b', marker='o')
        plt.plot(optim_exp[:,0], params_optimized[:,i], color = 'g', label = 'optimization')
        plt.scatter(optim_exp[:,0], params_optimized[:,i], color = 'g', marker='o')
        plt.xlabel('time / h')
        plt.ylabel('ln(sf) / -')
        plt.legend()
        plt.title('Logarithm scaling factor')
        plt.show()


plt.scatter(talip_data[:,0], talip_data[:,1], marker = '.', c = '#B3B3B3', label='Talip et al. (2014)')

plt.scatter(optim_exp[:,0], optim_exp[:,1], color = 'g', marker='x', label = 'optimization')
plt.scatter(optim_exp[:,0], optim_exp[:,2], facecolor = 'none', edgecolors='r',marker='o', label = 'filtered exp')
plt.scatter(optim_exp[1:,0], calibration_data[:,2], color = 'black', marker='x', label = 'calibration')
for i, trace in enumerate(prediction_trace):
    if i == 0:
        plt.plot(trace[:,0]+(i)*optim_exp[1,0], trace[:,1], 'g', label = 'Sciantix nomimal')
        # plt.plot(trace[:,0]+(i)*optim_exp[1,0], trace[:,2], 'b', label = 'Optimized')
        plt.plot(trace[:,0]+i*optim_exp[1,0], trace[:,2], 'r--', label = 'Prediction: Upper bound')
        plt.plot(trace[:,0]+i*optim_exp[1,0], trace[:,3], 'b--', label = 'Prediction: Lower bound')
        plt.fill_between(trace[:,0]+i*optim_exp[1,0], trace[:,2], trace[:,3], color = 'skyblue', alpha = 0.5, label = 'Confidence region')
    else:
        plt.plot(trace[:,0]+(i)*optim_exp[1,0], trace[:,1], 'g')
        # plt.plot(trace[:,0]+(i-1)*optim_exp[1,0], trace[:,2], 'b')
        plt.plot(trace[:,0]+i*optim_exp[1,0], trace[:,2], 'r--')
        plt.plot(trace[:,0]+i*optim_exp[1,0], trace[:,3], 'b--')
        plt.fill_between(trace[:,0]+i*optim_exp[1,0], trace[:,2], trace[:,3], color = 'skyblue', alpha = 0.5)
plt.xlabel('time / h')
plt.ylabel('fraction release / -')
plt.legend()
plt.title('Fraction release')
plt.show()


priors_over_time = []
with open('priors_over_time.txt', 'r') as file:
    for line in file:
        # Banish the brackets to the shadow realm
        cleaned_line = line.replace('[', '').replace(']', '')
        # Split the numbers, now free from their bracketed confines
        number_strings = cleaned_line.split()

        # Convert the string representations into the true form of floating-point numbers
        numbers = [float(num) for num in number_strings]
        priors_over_time.append(numbers)

points_over_time = []
with open('points_over_time.txt', 'r') as file:
    for line in file:
        # Banish the brackets to the shadow realm
        cleaned_line = line.replace('[', '').replace(']', '')
        # Split the numbers, now free from their bracketed confines
        number_strings = cleaned_line.split()

        # Convert the string representations into the true form of floating-point numbers
        numbers = [float(num) for num in number_strings]
        points_over_time.append(numbers)



# Now you have a list of numpy arrays, which you can access




i = 0
ax = plt.figure().add_subplot(projection='3d')
while i < len(optim_exp):
    d = np.vstack((points_over_time[i], priors_over_time[i])).T
    d_sorted = d[d[:, 0].argsort()]
    ax.plot(d_sorted[:,0], d_sorted[:,1]/np.sum(d_sorted[:,1]), zs = optim_exp[i,0], zdir = 'x')
    # ax.scatter(points_over_time[i], priors_over_time[i]/np.max(priors_over_time[i]), zs = optim_exp[i,0], zdir = 'x')
    i = i + 1
# plt.scatter(points_over_time[11], priors_over_time[11]/np.max(priors_over_time[0]))
# # plt.show()

# plt.scatter(points_over_time[21], priors_over_time[21]/np.max(priors_over_time[0]))
# # plt.show()

# plt.scatter(points_over_time[31], priors_over_time[31]/np.max(priors_over_time[0]))
# # plt.show()

# plt.scatter(points_over_time[71], priors_over_time[71]/np.max(priors_over_time[0]))
# # plt.show()
ax.set_xlim(0, 3.7)
ax.set_ylim(-4.8, 4.8)
# ax.set_zlim(0,1)
ax.set_xlabel('time')
ax.set_ylabel('scaling factor')
ax.set_zlabel('normalized probability')
# plt.scatter(points_over_time[99], priors_over_time[99]/np.max(priors_over_time[0]))
plt.show()


####color map
points_over_time = np.asfarray(points_over_time)
priors_over_time = np.asfarray(priors_over_time)
time_number = points_over_time.shape[0]
point_number = points_over_time.shape[1]
data_cm = np.zeros((point_number * time_number, 3))

for i in range(time_number):
    data_cm[i*point_number:(i+1)*point_number, 0] = optim_exp[i,0]
    data_cm[i*point_number:(i+1)*point_number, 1] = points_over_time[i]
    data_cm[i*point_number:(i+1)*point_number, 2] = priors_over_time[i]

time = data_cm[:,0]
pt = data_cm[:,1]
p = data_cm[:,2]
# p = p.reshape((point_number, time_number))
p_min, p_max = min(p), max(p)
alpha_values =(p - p_min) / (p_max - p_min)
alpha_values = alpha_values.flatten()
# plt.scatter(time, pt, c=p, cmap='viridis', alpha=alpha_values)  # Using viridis colormap, but you can choose any
# plt.scatter(time, pt, c=p, cmap='plasma', alpha = alpha_values) 
# # plt.scatter(time, pt, c=p, cmap='inferno',alpha= alpha_values) 
# # plt.scatter(time, pt, c=p, cmap='magma',alpha= alpha_values)
# # Adding color bar to understand the mapping of y values to colors
# plt.colorbar(label='Probability')


# Define a colormap and normalize alpha_values to the [0, 1] range
norm = plt.Normalize(p.min(), p.max())
cmap = plt.get_cmap('plasma')

# Plot the scatter plot with different colors and transparency levels
for t, p_value, alpha_value in zip(time, pt, alpha_values):
    color = cmap(norm(p_value))
    plt.scatter(t, p_value, c=[color], alpha=alpha_value)

# Add a colorbar for reference
plt.colorbar(plt.cm.ScalarMappable(norm=norm, cmap=cmap), label='Probability')

# Labeling axes
plt.xlabel('Time')
plt.ylabel('Logarithem scaling factor')
plt.show()



##############################

# time_grid, point_grid = np.meshgrid(np.unique(time), np.unique(pt))

# # Interpolate your y values on this grid
# # This step might need adjustments based on your actual data structure
# # Simplest interpolation for demonstration
# from scipy.interpolate import griddata
# y_grid = griddata((time, pt), p, (time_grid, point_grid), method='cubic')

# # Plot using pcolormesh
# plt.figure(figsize=(8, 6))
# c = plt.pcolormesh(time_grid, point_grid, y_grid, cmap='viridis', shading='auto')
# plt.colorbar(c, label='Y Value')

# plt.xlabel('Time')
# plt.ylabel('X Value')
# plt.title('2D Color-Coded Plot by Y Values')
# plt.show()
