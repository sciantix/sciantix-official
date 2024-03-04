import numpy as np
import corner, ast, os, shutil, subprocess, json
import matplotlib.pyplot as plt
from contextlib import contextmanager
from scipy.interpolate import interp1d
from IPython.display import display, Math

@contextmanager
def change_directory(destination_directory, original_directory):
    try:
        os.chdir(destination_directory)
        yield
    finally:
        os.chdir(original_directory)

def get_selected_variables_value_from_output_last_line(variable_selected, source_file):
    with open(source_file, 'r') as file:
        header = file.readline().strip().split('\t')

        lines = [line for line in file if line.strip()]
        last_line = lines[-1]
        second_last_line = lines[-2] if len(lines) > 1 else None

    if second_last_line:
        # Compare time values (assuming time is in the first column)
        if last_line.split('\t')[0] == second_last_line.split('\t')[0]:
            chosen_line = second_last_line
        else:
            chosen_line = last_line
    else:
        chosen_line = last_line

    variable_positions = [header.index(var) for var in variable_selected if var in header]

    # Optional: Warn about missing variables
    missing_vars = [var for var in variable_selected if var not in header]
    if missing_vars:
        print(f"Warning: The following variables were not found in the file and will be ignored: {missing_vars}")

    # Use the chosen line to create the data array
    data = np.genfromtxt([chosen_line], dtype='str', delimiter='\t')

    # Ensure output is always a 1D array even if only one variable is selected
    variable_selected_value = np.atleast_1d(data[variable_positions].astype(float))
    
    return variable_selected_value

def get_selected_variables_value_from_output(variable_selected, source_file):
    with open(source_file, 'r') as file:
        header = file.readline().strip().split('\t')
        lines = [line for line in file if line.strip()]

    variable_positions = [header.index(var) for var in variable_selected if var in header]
    # Optional: Warn about missing variables
    missing_vars = [var for var in variable_selected if var not in header]
    if missing_vars:
        print(f"Warning: The following variables were not found in the file and will be ignored: {missing_vars}")

    data = np.genfromtxt(lines, dtype='str', delimiter='\t')
    variable_selected_value = data[:, variable_positions].astype(float)
    
    return variable_selected_value

def sciantix(sciantix_folder_path, params:dict, last_value = True):
    """
    params:
        *key: params name
        *value: value
    return(np.array):
        *t_end
        *temperature @ t_end
        *FR @ t_end
        *RR @ t_end
    """
    print(params)
    with change_directory(sciantix_folder_path, os.getcwd()):
        scaling_factors = {}
        with open("input_scaling_factors.txt", 'r') as file:
            lines = file.readlines()

        for i in range(0, len(lines), 2):  # Step of 2 to process pairs of lines
            try:
                value = float(lines[i].strip())
                name = lines[i + 1].strip().replace("# scaling factor - ", "")
                scaling_factors[name] = value
            except ValueError as e:
                print(f"Error processing line {i}: {e}")
            except IndexError:
                print(f"Missing name for value at line {i}")
        
        for key, value in params.items():
            if 'pre exponential' in key:
                params[key] = np.exp(value)
        scaling_factors.update(params)
        
        with open('input_scaling_factors.txt','w') as file:
            for key, value in scaling_factors.items():
                    file.write(f'{value}\n')
                    file.write(f'# scaling factor - {key}\n')	

        subprocess.run(['./sciantix.x'])
        variables = ["Time (h)","Temperature (K)","He fractional release (/)", "He release rate (at/m3 s)"]
        if last_value == True:
            output_data = get_selected_variables_value_from_output_last_line(variables, 'output.txt')
        else:
            output_data = get_selected_variables_value_from_output(variables, 'output.txt')
    return output_data

def filter_and_interpolate_matrix(matrix, time_points):
    # Original x and y-values
    x_original = matrix[:, 0]

    # New x positions where we want to add points
    new_x_positions = time_points
    max_new_x = max(time_points)
    new_x_positions = new_x_positions[~np.isin(new_x_positions, x_original)]
    # Initialize an array to hold the new data points
    new_data_points = np.zeros((len(new_x_positions), matrix.shape[1]))
    
    # Fill in the new x positions in the new data points array
    new_data_points[:, 0] = new_x_positions

    # Interpolating for each y-value column
    for i in range(1, matrix.shape[1]):
        y_original = matrix[:, i]
        interpolation_func = interp1d(x_original, y_original, kind='linear')
        
        # Calculating the new y-values and adding them to the new data points array
        new_data_points[:, i] = interpolation_func(new_x_positions)

    # Combine old and new data points and sort by the first column (x-values)
    combined_data = np.vstack((matrix, new_data_points))
    sorted_combined_data = combined_data[combined_data[:, 0].argsort()]

    # final_data = sorted_combined_data[sorted_combined_data[:, 0] <= max_new_x]

    return sorted_combined_data




samples = []
means = []
time_points = []
params = []
calibrated_data = []
calibrated_data_da = []
percentiles_data_whole = []
percentiles_data_step = []
talip_data = np.genfromtxt('Talip2014_release_data.txt')
final_optim_fr = np.genfromtxt('final_optim_fr.txt')
param_key = np.array(['helium diffusivity pre exponential', 'helium diffusivity activation energy'])
ndim = len(param_key)
param_key = np.sort(param_key)
current_path = os.getcwd()

parent_path = os.path.dirname(current_path)

if os.path.exists('Plot_calib'):
    shutil.rmtree('Plot_calib')
# os.makedirs('Plot_calib')
plot_calib_folder = os.path.join(current_path, 'Plot_calib')
# calibration_folder = os.path.join(current_path, 'Calibration')
case_folder = os.path.join(parent_path, 'test_Talip2014_1600K')
original_history_path = os.path.join(case_folder, 'input_history.txt')
original_history = np.genfromtxt(original_history_path, dtype = 'float', delimiter = '\t')
shutil.copytree(case_folder, plot_calib_folder)

folder_name = os.listdir(plot_calib_folder)
folder_name = np.sort(folder_name)


percentile_value = np.zeros((ndim,3))
percentiles_step = []
percentiles_whole = []
p = [5, 50, 95]
with open('MCMC_samples_1.62.txt', 'r') as file:
    # Read the entire file, this assumes that the file is not too large to fit in memory
    data = file.read()

    # Split the data by double newlines, which seems to be your separator between arrays
    array_strings = data.strip().split('\n\n')

    # Iterate over the separated string representations of the arrays
    for array_string in array_strings:
        # Use ast.literal_eval to safely evaluate the string as a Python literal
        array_literal = ast.literal_eval(array_string)
        # Convert the literal to a numpy array and append to the list of arrays
        array_number = np.array(array_literal, dtype = 'float')
        # array_number[:,0] = array_number[:,0][::-1]
        
        for j in range(ndim):
            percentile_value[j,:] = np.percentile(array_number[:,j], p)
            # print(percentile_value)
        for k in range(len(percentile_value.T)):
            percentile_info = {key:value for key, value in zip(param_key, percentile_value.T[k])}
            percentiles_step.append(percentile_info)
        percentiles_whole.append(percentiles_step)
        
        samples.append(array_number)

        mean = np.mean(array_number, axis = 0)
        param_info = {key:value for key, value in zip(param_key, mean)}
 
        params.append(param_info)
        means.append(mean)
# print(means)

with open('sfs_maxp_da.txt', 'r') as file:
    sfs_maxp_da = json.load(file)
# print(sfs_maxp_da)

for i in range(len(samples)):
    samples[i][:,1] = np.exp(samples[i][:,1])
    # fig = corner.corner(samples[i], truths = [0.9821608153667933, 0.9487537911538045], labels = ['sf_Diffu_activation', 'sf_Diffu_preExp'])
    
    # fig = corner.corner(samples[i], labels = ['sf_actEnergy', 'sf_preExp', 'fm'])
    sf_maxp_da = list(sfs_maxp_da[i].values())
    print(sf_maxp_da)
    fig = corner.corner(samples[i], labels = ['sf_actEnergy', 'sf_preExp'], quantiles = [0.16, 0.5, 0.84], show_titles = True, title_kwargs = {'fontsize':13}, truths = sf_maxp_da)
    plt.show()

# for name in folder_name:
#     if name.startswith('from_0.0_to_'):
#         _,_, time_point = name.rpartition('_')
#         time_points.append(float(time_point))
# time_points = np.array(time_points)

time_points = np.round(np.linspace(0,3.6,21),3)
history = filter_and_interpolate_matrix(original_history, time_points)
for i in range(len(means)):
    # path = os.path.join(plot_calib_folder, folder_name[i])

    # with change_directory(path, os.getcwd()):
    with change_directory(plot_calib_folder, os.getcwd()):
        with open('input_history.txt', 'w') as file:
            file.writelines('\t'.join(str(item) for item in row) + '\n' for row in history[:-1])
            file.write('\t'.join(str(item) for item in history[-1]))
        output_data = sciantix(plot_calib_folder,params[i],last_value=False)[:,[0,2]]
        sfs_maxp_da[i]['helium diffusivity pre exponential'] = np.log(sfs_maxp_da[i]['helium diffusivity pre exponential'])
        output_data_da = sciantix(plot_calib_folder, sfs_maxp_da[i], last_value = False)[:,[0,2]]
        calibrated_data.append(output_data)
        calibrated_data_da.append(output_data_da)



plt.scatter(talip_data[:,0], talip_data[:,1], marker = '.', c = '#B3B3B3', label='Talip et al. (2014)')
plt.plot(final_optim_fr[:,0], final_optim_fr[:,2], 'r', label = 'Optimization')
color = plt.cm.viridis(np.linspace(0,1,len(samples)))
for i in range(len(samples)):
    # plt.plot(calibrated_data[i][:,0], calibrated_data[i][:,1]*np.exp(means[i][2]*calibrated_data[i][:,1]), c = color[i], label = f"calibrated_mean@{np.round(time_points[i],3)}")
    plt.plot(calibrated_data[i][:,0], calibrated_data[i][:,1],c = color[i], label = f"calibrated_mean@{np.round(time_points[i+1],3)}")
    plt.plot(calibrated_data_da[i][:,0], calibrated_data_da[i][:,1],c = color[i],linestyle = 'dashdot', label = f"calibrated_mean_da@{np.round(time_points[i+1],3)}")
    
    plt.axvline(time_points[i+1], c = color[i], linestyle = 'dashdot')

    # for j in range(len(p)):
    #     plt.plot(percentiles_data_whole[i][j][:,0], percentiles_data_whole[i][j][:,1], linestyle = 'dashdot', label = f'calibrated_percentile{p[j]}')
    #     # plt.plot(percentiles_data_whole[i][j][:,0], percentiles_data_whole[i][j][:,1] * np.exp(means[i][2]* percentiles_data_whole[i][j][:,1]), linestyle = 'dashdot', label = f'calibrated_percentile{p[j]}')

plt.xlabel('time / h')
plt.ylabel('helium fraction release /')
plt.legend()
plt.title('Helium fraction release with mean diffusivity calibrated at different time')
plt.show()










