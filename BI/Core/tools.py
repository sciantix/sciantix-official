import numpy as np
import os, ast, shutil, subprocess, tools
from contextlib import contextmanager
from scipy.interpolate import interp1d
from scipy.stats import truncnorm

def setup_directory(dirname):
    if os.path.exists(dirname):
        shutil.rmtree(dirname)
    os.makedirs(dirname)
    return os.path.join(os.getcwd(), dirname)
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

def sciantix(sciantix_folder_path, params:dict, extraction_way = -1):
    """
    params:
        *key: params name
        *value: value
    extraction_way:
        *-1 : last line
        *0: all
        *np.array : time points
    return(np.array):
        *t_end
        *temperature @ t_end
        *FR @ t_end
        *RR @ t_end
    """
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
        if isinstance(extraction_way, int) and extraction_way == -1:
            extracted_data = np.array([get_selected_variables_value_from_output_last_line(variables, 'output.txt')])
        elif isinstance(extraction_way, int) and extraction_way == 0:
            extracted_data = get_selected_variables_value_from_output(variables, 'output.txt')
        else:
            output_data = get_selected_variables_value_from_output(variables, 'output.txt')
            # output_data = [row for row in output_data]
            # print(output_data.shape)
            extracted_data = []
            for j in range(len(extraction_way)):
                clostest_point = find_closest_points(output_data, extraction_way[j])
                value = linear_interpolate(*clostest_point, extraction_way[j])
                extracted_data.append(value)
            extracted_data = np.array(extracted_data)
    return extracted_data

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

def independent_sciantix_folder(destination,required_files_path, points_time, prediction = True, original_history = False):
    points_time = np.round(points_time, 3)
    # t_start = np.round(points_time[0],3)
    # t_end = np.round(points_time[-1],3)
    folder_name = f'from_{points_time[0]}_to_{points_time[-1]}'
    with change_directory(destination, os.getcwd()):
        setup_directory(folder_name)
        os.chdir(folder_name)
        for i in range(len(required_files_path)):
            shutil.copy(required_files_path[i], os.getcwd())
        if original_history == False:
            history_original = np.genfromtxt("input_history.txt",dtype = 'float', delimiter='\t')
            history = filter_and_interpolate_matrix(history_original, points_time)
            if prediction == False:
                history = history[history[:,0]<=points_time[-1]]

            history[:,0] = history[:,0] - points_time[0]
            with open('input_history.txt', 'w') as file:
                file.writelines('\t'.join(str(item) for item in row) + '\n' for row in history[:-1])
                file.write('\t'.join(str(item) for item in history[-1]))
        
        return os.path.join(destination, folder_name)

def moving_average(data, window_size):
    half_window = window_size // 2
    smoothed_data = np.convolve(data, np.ones(window_size) / window_size, mode='same')

    for i in range(half_window):
        smoothed_data[i] = np.mean(data[:i+half_window+1])
        smoothed_data[-i-1] = np.mean(data[-(i+half_window+1):])
    return smoothed_data

def dynamic_std(data, window_size):
    half_window = window_size // 2
    dynamic_std = np.full(data.shape, np.nan)

    for i in range(len(data)):
        start = max(0, i - half_window)
        end = min(len(data), i + half_window + 1)
        dynamic_std[i] = np.std(data[start:end], ddof=1)  # ddof=1 for sample standard deviation

    return dynamic_std

def find_closest_points(matrix, target_time):
    """
    Find the two points in the matrix that are closest to the target time, one before and one after.

    Args:
    matrix (list of lists): The original data matrix.
    target_time (int or float): The target time to find closest points for.

    Returns:
    tuple: A tuple containing the two closest points (each a list), or None if no such points exist.
    """


    for i in range(len(matrix) - 1):
        if matrix[i][0] <= target_time <= matrix[i + 1][0]:
            return matrix[i], matrix[i + 1]
    return None

def linear_interpolate(point1, point2, target_time):
    """
    Perform linear interpolation between two data points.

    Args:
    point1 (list): The data point before the target time. It should be in the format [time, value1, value2, ...].
    point2 (list): The data point after the target time, in the same format as point1.
    target_time (int or float): The time at which we want to interpolate the values.

    Returns:
    list: Interpolated data point at the target time, in the format [target_time, interpolated_value1, interpolated_value2, ...].
    """
    ratio = (target_time - point1[0]) / (point2[0] - point1[0])
    return [target_time] + [p1 + ratio * (p2 - p1) for p1, p2 in zip(point1[1:], point2[1:])]

def exp_data_process(case_folder):
    with change_directory(case_folder, os.getcwd()):
        # print(case_folder)
        exp_fr_data  = np.genfromtxt("Talip2014_release_data.txt",dtype = 'float',delimiter='\t')
        exp_fr_data = exp_fr_data[exp_fr_data[:,0].argsort()]

        exp_rr_data = np.genfromtxt("Talip2014_rrate_data.txt",dtype = 'float',delimiter='\t')
        time_exp = exp_fr_data[:,0]
        temperature_exp = exp_rr_data[:,0]
        fr_ma = sorted(moving_average(exp_fr_data[:,1],20))
        # rr_ma = moving_average(exp_rr_data[:,1],5)
        fr_std = dynamic_std(exp_fr_data[:,1], 20)
        # rr_from_fr = np.append(np.array([0]), np.diff(fr_ma)/np.diff(time_exp))


        fr_info = np.vstack((time_exp, fr_ma, fr_std)).T

    return fr_info

def interpolate(x, y, x_toInter):

    idx = np.searchsorted(x, x_toInter) - 1
    idx = np.clip(idx,0, len(x) - 2)
    dy = y[idx+1] - y[idx]
    dt = x[idx+1] - x[idx]
    t_diff = x_toInter -x[idx]
    return y[idx] + (dy/dt) * t_diff

def truncated_gaussian_pdf(x, mean, std, lower, upper):


    a, b = (lower - mean) / std, (upper, mean) / std

    dst =  truncnorm(a, b, loc= mean, scale = std)
    return dist.pdf(x)

def sciantix_required_files_path(case_folder):
    path_input_initial_conditions = os.path.join(case_folder, 'input_initial_conditions.txt')
    path_input_settings = os.path.join(case_folder,'input_settings.txt')
    path_input_scaling_factor = os.path.join(case_folder, 'input_scaling_factors.txt')
    path_input_history = os.path.join(case_folder, 'input_history.txt')
    _ = os.path.dirname(os.getcwd())
    while not os.path.exists(os.path.join(_, 'bin')):
        _ = os.path.dirname(_)
    path_sciantix = os.path.join(_,'bin','sciantix.x')

    required_files_path = [path_input_initial_conditions, path_input_history, path_input_scaling_factor, path_input_settings, path_sciantix]
    return required_files_path


def log_gaussian_prior(theta, mean, covariance):
    n = len(theta)
    diff = theta - mean
    return -0.5*(n * np.log(2 * np.pi) + np.log(np.linalg.det(covariance)) + np.dot(diff, np.linalg.solve(covariance, diff)))

def log_tg_prior(theta, mean, covariance):
    """
    truncated gaussian
    """
    std = np.sqrt(np.diag(covariance))
    prior = np.zeros_like(theta)
    a, b = -3, 3
    for i in range(len(theta)):
        pdf = truncnorm(a, b, loc = mean[i], scale = std[i])
        prior[i] = pdf.pdf(theta[i])
        if prior[i] == 0:
            return -np.inf
    return np.sum(np.log(prior))

def log_uniform_prior(theta, mean, covariance):
    std = np.sqrt(np.diag(covariance))
    low_bounds = mean - 3 * std
    up_bounds = mean + 3 *std
    count = 0
    for i in range(len(mean)):
        if low_bounds[i] < theta[i] < up_bounds[i]:
            count = count + 1
    if count == len(theta):
        return 0.0
    else:
        return -np.inf

def set_exp_data(case_folder, smooth = True):
    if smooth == True:
        fr_exp_info = tools.exp_data_process(case_folder)
    else:
        with tools.change_directory(case_folder, os.getcwd()):
            fr_exp = np.genfromtxt("Talip2014_release_data.txt",dtype = 'float',delimiter='\t')
            fr_exp = fr_exp[fr_exp[:,0].argsort()]
            fr_std = tools.dynamic_std(fr_exp[:,1], window_size = 20)
            fr_exp_info = np.hstack((fr_exp, fr_std.reshape((-1, 1))))
    return fr_exp_info

def set_calibration_time(time_observed, num_calibration, num_observed):
    if num_calibration - 1 == 0:
        time_calibration = np.array([time_observed[-1]])
    else:
        points_per_calibration = num_observed//num_calibration
        # extra_points = (self.time_observed - 1)%(self.num_calibration)
        time_calibration = np.zeros((num_calibration))
        for i in range(num_calibration - 1):
            time_calibration[i] = time_observed[int(points_per_calibration * (i+1))]
        time_calibration[-1] = time_observed[-1]
    return time_calibration


