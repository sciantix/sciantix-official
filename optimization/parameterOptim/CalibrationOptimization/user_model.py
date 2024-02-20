import numpy as np
import os, shutil, copy, subprocess
from contextlib import contextmanager


class UserModel:
    """
    Represents the user model for the simulation. This class generates time-series data
    using a mathematical model with specified parameters.

    """
    def __init__(
        self,
        case_name:str = None,
        params: np.ndarray = None,
        params_initial_values:np.ndarray = None,
        params_stds:np.ndarray = None,

    ) -> None:
        params_info = {
            params[i]:{
                'mu': params_initial_values[i],
                'sigma': params_stds[i],
                'bound': (params_initial_values[i] - 3 * params_stds[i], params_initial_values[i] + 3*params_stds[i])
            } for i in range(len(params))
        }

        self.params_info = {key:params_info[key] for key in sorted(params_info)}

        self.code_container = os.getcwd() # ../parameterOptim/CalibrationOptimization
        self.case_folder_container = os.path.dirname(self.code_container) # ../parameterOptim
        self.case_folder_path = os.path.join(self.case_folder_container, case_name) # ../parameterOptim/caseName

        self.input_settings_path = os.path.join(self.case_folder_path, 'input_settings.txt')
        self.input_initial_conditions_path = os.path.join(self.case_folder_path, 'input_initial_conditions.txt')
        self.input_scaling_factor_path = os.path.join(self.case_folder_path, 'input_scaling_factors.txt')
        self.input_history_path = os.path.join(self.case_folder_path, 'input_history.txt')
        ####### change here to your sciantix path
        # self.sciantix_path = os.path.join('/home/posh/sciantix_28_9/bin','sciantix.x')
        _ = os.path.dirname(self.case_folder_container)
        sciantix_folder = os.path.dirname(_)
        self.sciantix_path = os.path.join(sciantix_folder,'bin', 'sciantix.x')

        with self.change_directory(self.case_folder_path, self.code_container):
            exp_fr_data  = np.genfromtxt("Talip2014_release_data.txt",dtype = 'float',delimiter='\t')
            exp_fr_data_sorted = exp_fr_data[exp_fr_data[:,0].argsort()]
            exp_rr_data = np.genfromtxt("Talip2014_rrate_data.txt",dtype = 'float',delimiter='\t')
            history = np.genfromtxt("input_history.txt",dtype = 'float', delimiter='\t')

            self.scaling_factors = {}
            with open("input_scaling_factors.txt", 'r') as file:
                lines = file.readlines()

            for i in range(0, len(lines), 2):  # Step of 2 to process pairs of lines
                try:
                    value = float(lines[i].strip())
                    name = lines[i + 1].strip().replace("# scaling factor - ", "")
                    self.scaling_factors[name] = value
                except ValueError as e:
                    print(f"Error processing line {i}: {e}")
                except IndexError:
                    print(f"Missing name for value at line {i}")

        self.time_exp  = exp_fr_data_sorted[:,0]
        self.FR_exp = sorted(self.moving_average(exp_fr_data_sorted[:,1],100))
        self.FR_exp_std = self.dynamic_std(exp_fr_data_sorted[:,1],100)
        RR_from_FR = np.array([0])
        self.RR_from_FR = np.append(RR_from_FR, np.diff(self.FR_exp)/np.diff(self.time_exp))
        self.temperature_exp = exp_rr_data[:,0]
        self.RR_exp = self.moving_average(exp_rr_data[:,1],5)
        
        self.history_original = history
        self.time_history_original = self.history_original[:,0]
        self.temperature_history_original = self.history_original[:,1]

    def _independent_sciantix_folder(self, destination,optim_folder, t_0, t_1):
        """
        Build a standard sciantix folder with newest sciantix.x and the input files for selected case.
        Return the folder's path
        """
        t_start = np.round(t_0,3)
        t_end = np.round(t_1,3)
        forlder_name = f'from_{t_start}_to_{t_end}'

        with self.change_directory(destination, self.code_container):
            if not os.path.exists(forlder_name):
                os.makedirs(forlder_name)
            else:
                shutil.rmtree(forlder_name)
                os.makedirs(forlder_name)
            os.chdir(forlder_name)
            current_sciantix_path = os.getcwd()
            shutil.copy(self.input_settings_path, os.getcwd())
            shutil.copy(self.input_scaling_factor_path, os.getcwd())
            shutil.copy(self.input_initial_conditions_path, os.getcwd())
            shutil.copy(self.sciantix_path, os.getcwd())
            shutil.copy(self.input_history_path, os.getcwd())
            history_original = copy.deepcopy(self.history_original)

            history_info = self._filter_and_interpolate_matrix(history_original, t_start, t_end)
            history_info[:,0] = history_info[:,0] - t_start
            ois = 7.8e-30
            if t_start != 0:
                with self.change_directory(optim_folder, current_sciantix_path):
                    initial_conditions = np.array([
                        "He produced (at/m3)",
                        "He in grain (at/m3)", 
                        "He in intragranular solution (at/m3)", 
                        "He in intragranular bubbles (at/m3)", 
                        "He at grain boundary (at/m3)", 
                        "He released (at/m3)",
                        "Grain radius (m)",
                        "Intragranular bubble concentration (bub/m3)",
                        "Intragranular bubble radius (m)"
                    ])
                    initial_conditions_value = self.get_selected_variables_value_from_output_last_line(initial_conditions, 'output.txt')
                FR_interpolated = self._exp(t_start)[1]
                initial_conditions_info = {
                    initial_conditions[i]: initial_conditions_value[i] for i in range(len(initial_conditions))
                }
                initial_conditions_info["He released (at/m3)"] = FR_interpolated * initial_conditions_info['He produced (at/m3)']
                initial_conditions_info["He in grain (at/m3)"] = (1 - FR_interpolated) * initial_conditions_info['He produced (at/m3)']
                initial_conditions_info["He in intragranular solution (at/m3)"] = initial_conditions_info["He in grain (at/m3)"] * initial_conditions_info["He in intragranular solution (at/m3)"] / (initial_conditions_info["He in intragranular solution (at/m3)"] + initial_conditions_info["He in intragranular bubbles (at/m3)"])
                initial_conditions_info["He in intragranular bubbles (at/m3)"] = initial_conditions_info["He in grain (at/m3)"] - initial_conditions_info["He in intragranular solution (at/m3)"]
                intragranular_bubble_volume = initial_conditions_info['He in intragranular bubbles (at/m3)']/initial_conditions_info['Intragranular bubble concentration (bub/m3)'] * ois
                initial_conditions_info['Intragranular bubble radius (m)'] = 0.620350491 * (intragranular_bubble_volume ** (1/3))
                
                
                keyword1 = "#	initial He (at/m3) produced, intragranular, intragranular in solution, intragranular in bubbles, grain boundary, released"
                ic_new = np.array([
                    initial_conditions_info['He produced (at/m3)'],
                    initial_conditions_info['He in grain (at/m3)'],
                    initial_conditions_info['He in intragranular solution (at/m3)'],
                    initial_conditions_info['He in intragranular bubbles (at/m3)'],
                    initial_conditions_info['He at grain boundary (at/m3)'],
                    initial_conditions_info['He released (at/m3)']
                ])
                keyword2 = "#	initial grain radius (m)"
                ic_grainRadius = initial_conditions_info['Grain radius (m)']
                
                keyword3 = "#	initial intragranular bubble concentration (at/m3), radius (m)"
                ic_intraGrainBubble = np.array([
                    initial_conditions_info['Intragranular bubble concentration (bub/m3)'],
                    initial_conditions_info['Intragranular bubble radius (m)']
                ])
                
                with open("input_initial_conditions.txt", 'r') as file:
                    lines = file.readlines()

                for i, line in enumerate(lines):
                    if keyword1 in line:
                        ic_1 = '\t'.join(map(str, ic_new))
                        lines[i - 1] = ic_1 + '\n'
                    elif keyword2 in line:
                        ic_2 = str(ic_grainRadius)
                        lines[i - 1] = ic_2 + '\n'
                    elif keyword3 in line:
                        ic_3 = '  '.join(map(str, ic_intraGrainBubble))
                        lines[i - 1] = ic_3 + '\n'
                with open("input_initial_conditions.txt", 'w') as file:
                    file.writelines(lines)
            
            with open('input_history.txt', 'w') as file:
                file.writelines('\t'.join(str(item) for item in row) + '\n' for row in history_info[:-1])
                file.write('\t'.join(str(item) for item in history_info[-1]))
        
        
        return os.path.join(destination, forlder_name)
    

    def _sciantix(self, sciantix_folder_path, params:dict, last_value = True):
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
        scaling_factors = copy.deepcopy(self.scaling_factors)

        with self.change_directory(sciantix_folder_path, self.code_container):
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
                output_data = self.get_selected_variables_value_from_output_last_line(variables, 'output.txt')
            else:
                output_data = self.get_selected_variables_value_from_output(variables, 'output.txt')
        return output_data
    
    def _exp(self, time_point):
        """
        return:
            *time_point
            *FR_interpolated @ time_point
            *FR_interpolated_std @ time_point
        """
        FR_exp_info = np.vstack((self.time_exp, self.FR_exp, self.FR_exp_std, self.RR_from_FR)).T

        clostest_point = self.find_closest_points(FR_exp_info, time_point)
        interpolate_FR_info = self.linear_interpolate(*clostest_point, time_point)
            
        return interpolate_FR_info
    
    def calculate_error(self, sciantix_folder_path, params:dict, time_end):
        output_sciantix = self._sciantix(sciantix_folder_path,params)
        FR_interpolated_info = self._exp(time_end)
        
        error = np.abs(output_sciantix[2] - FR_interpolated_info[1])/np.abs(FR_interpolated_info[1])


        ###### ALSO CONSIDER RR
        # RR_sciantix = output_sciantix[3]
        # with self.change_directory(sciantix_folder_path, self.code_container):
        #     time_and_heProduced_sciantix = self.get_selected_variables_value_from_output(['Time (h)','He produced (at/m3)' ],'output.txt')
        # dt_sciantix = time_and_heProduced_sciantix[-2,0] - time_and_heProduced_sciantix[-3,0]
        # he_produced = time_and_heProduced_sciantix[-2,1]
        # RR_exp = FR_interpolated_info[3] / 3600 * he_produced

        # error = (np.abs(output_sciantix[2] - FR_interpolated_info[1])/np.abs(FR_interpolated_info[1]) + np.abs(RR_sciantix - RR_exp)/RR_exp)
        # print(RR_sciantix, RR_exp)
        return error

    @staticmethod
    @contextmanager
    def change_directory(destination_directory, original_directory):
        try:
            os.chdir(destination_directory)
            yield
        finally:
            os.chdir(original_directory)
    
    @staticmethod
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
    
    @staticmethod
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


    @staticmethod
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

    @staticmethod
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
    
    def _filter_and_interpolate_matrix(self ,matrix, start_time, end_time):
        """
        Create a new matrix filtered by start and end times, including interpolated values at these times.

        Args:
        matrix (list of lists): The original data matrix.
        start_time (int or float): The start time for the new matrix.
        end_time (int or float): The end time for the new matrix.

        Returns:
        list of lists: The new matrix with rows between and including the interpolated start and end times.
        """
        new_matrix = []

        # Find points for interpolation at start and end times
        start_points = self.find_closest_points(matrix, start_time)
        end_points = self.find_closest_points(matrix, end_time)

        # Interpolate and add start row
        if start_points:
            new_matrix.append(self.linear_interpolate(*start_points, start_time)) # use * to unpacke the tuple output from linear_interpolate

        # Add in-between rows
        for row in matrix:
            if start_time < row[0] < end_time:
                new_matrix.append(row)

        # Interpolate and add end row
        if end_points:
            new_matrix.append(self.linear_interpolate(*end_points, end_time))
        new_matrix = np.array(new_matrix)
        return new_matrix

    
    @staticmethod
    def moving_average(data, window_size):
        half_window = window_size // 2
        smoothed_data = np.convolve(data, np.ones(window_size) / window_size, mode='same')

        for i in range(half_window):
            smoothed_data[i] = np.mean(data[:i+half_window+1])
            smoothed_data[-i-1] = np.mean(data[-(i+half_window+1):])
        return smoothed_data
    
    @staticmethod
    def dynamic_std(data, window_size):
        half_window = window_size // 2
        dynamic_std = np.full(data.shape, np.nan)

        for i in range(len(data)):
            start = max(0, i - half_window)
            end = min(len(data), i + half_window + 1)
            dynamic_std[i] = np.std(data[start:end], ddof=1)  # ddof=1 for sample standard deviation

        return dynamic_std
