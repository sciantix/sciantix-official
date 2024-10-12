import time
import os
import sys 
from sciantix_module import getMainVar, Sciantix, sciantixModule
import numpy as np

def logExecutionTime(timer, time_step_number):
    with open(f"{path}execution.txt", "a") as execution_file:
        execution_file.write(f"{timer:.12e}\t{time.process_time()}\t{timer * time.process_time():.12e}\t{time_step_number}\n")

if len(sys.argv) > 1:
    path = sys.argv[1]
else:
    print("No path specified, using current directory.")
    path = "./"

# Equivalent to the MainSCIANTIX.C
def main(Path_of_execution):
    ''' this function is the same as MainSciantix.C but it uses pybind11 '''

    # this set the path of execution
    sciantixModule.setTestPath(Path_of_execution)

    # Fetch and convert the arrays from sciantixModule
    Sciantix_options = np.array(sciantixModule.Sciantix_options, dtype=int)  # Numpy array of int
    Sciantix_variables = np.array(sciantixModule.Sciantix_variables, dtype=float)  # Numpy array of float
    Sciantix_scaling_factors = np.array(sciantixModule.Sciantix_scaling_factors, dtype=float)  # Numpy array of float
    Sciantix_diffusion_modes = np.array(sciantixModule.Sciantix_diffusion_modes, dtype=float)  # Numpy array of float
    Sciantix_history = np.array(sciantixModule.Sciantix_history, dtype=float)  # Numpy array of float

    # Convert other variables
    Input_history_points = int(sciantixModule.Input_history_points)  # int
    Time_input = np.array(sciantixModule.Time_input, dtype=float)  # Numpy array of floats
    Temperature_input = np.array(sciantixModule.Temperature_input, dtype=float)  # Numpy array of floats
    Fissionrate_input = np.array(sciantixModule.Fissionrate_input, dtype=float)  # Numpy array of floats
    Hydrostaticstress_input = np.array(sciantixModule.Hydrostaticstress_input, dtype=float)  # Numpy array of floats
    Steampressure_input = np.array(sciantixModule.Steampressure_input, dtype=float)  # Numpy array of floats
    Time_end_h = float(sciantixModule.Time_end_h)  # float
    Time_end_s = float(sciantixModule.Time_end_s)  # float

    # Initialisation 
    sciantixModule.InputReading(
        Sciantix_options, 
        Sciantix_variables, 
        Sciantix_scaling_factors,
        Input_history_points,
        Time_input, 
        Temperature_input,
        Fissionrate_input, 
        Hydrostaticstress_input,
        Steampressure_input,
        Time_end_h,
        Time_end_s
    )

    # Update the Initialization call with the required inputs
    sciantixModule.Initialization(
        Sciantix_history, 
        Sciantix_variables, 
        Sciantix_diffusion_modes, 
        Temperature_input, 
        Fissionrate_input, 
        Hydrostaticstress_input, 
        Steampressure_input
    )

    # remove output.txt if exists
    if os.path.exists(f"{path}output.txt"):
        os.remove(f"{path}output.txt")

    timer_start = time.time()

    while sciantixModule.Time_h <= sciantixModule.Time_end_h:
        
        # Creation an local variable for sciantix_histroy
        Sciantix_history = sciantixModule.getHistoryInArray_double()

        Sciantix_history[0] = Sciantix_history[1]
        Sciantix_history[1] = sciantixModule.InputInterpolation(sciantixModule.Time_h, sciantixModule.Time_input, sciantixModule.Temperature_input, sciantixModule.Input_history_points)
        Sciantix_history[2] = Sciantix_history[3]
        Sciantix_history[3] = sciantixModule.InputInterpolation(sciantixModule.Time_h, sciantixModule.Time_input, sciantixModule.Fissionrate_input, sciantixModule.Input_history_points)
        
        if Sciantix_history[3] < 0.0:
            Sciantix_history[3] = 0.0
        
        Sciantix_history[4] = Sciantix_history[5]
        Sciantix_history[5] = sciantixModule.InputInterpolation(sciantixModule.Time_h, sciantixModule.Time_input, sciantixModule.Hydrostaticstress_input, sciantixModule.Input_history_points)
        Sciantix_history[7] = sciantixModule.Time_h
        Sciantix_history[8] = float(sciantixModule.Time_step_number)
        Sciantix_history[9] = Sciantix_history[10]
        Sciantix_history[10] = sciantixModule.InputInterpolation(sciantixModule.Time_h, sciantixModule.Time_input, sciantixModule.Steampressure_input, sciantixModule.Input_history_points)
        
        # Call of the Sciantix Function
        Sciantix(sciantixModule.getOptionsInArray_int(), sciantixModule.getHistoryInArray_double(), sciantixModule.getVariablesInArray_double(), sciantixModule.getScalingFactorsInArray_double(), sciantixModule.getDiffusionModesInArray_double())

        # Fetch all the global variables 
        getMainVar()

        # time calculation
        sciantixModule.setSciantixDTimeH(sciantixModule.TimeStepCalculation())

        # Fetch all the global variables 
        getMainVar()

        Sciantix_history[6] = sciantixModule.dTime_h * 3600
        
        if sciantixModule.Time_h < sciantixModule.Time_end_h:
            sciantixModule.setSciantixTimeStepNumber(sciantixModule.Time_step_number + 1)
            
            sciantixModule.setSciantixTimeH(sciantixModule.dTime_h + sciantixModule.Time_h)
            sciantixModule.setSciantixTimeS(sciantixModule.Time_s + Sciantix_history[6])
            getMainVar()

        else:
            break
        
        # set the History 
        for i in range(20):
            sciantixModule.setHistory(i, Sciantix_history[i])
    
    timer_end = time.time()
    logExecutionTime(timer_end - timer_start, sciantixModule.Time_step_number)

if __name__ == "__main__":
    main(path)
