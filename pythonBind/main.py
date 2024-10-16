import time
import os
import sys 
from sciantix_module import Sciantix, sciantixModule
import numpy as np

def logExecutionTime(timer, time_step_number):
    with open(f"{path}execution.txt", "a") as execution_file:
        execution_file.write(f"{timer:.12e}\t{time.process_time()}\t{timer * time.process_time():.12e}\t{time_step_number}\n")

if len(sys.argv) > 1:
    path = sys.argv[1]
else:
    print("No path specified, using current directory.")
    path = "./"

# Equivalent to MainSCIANTIX.C
def main(Path_of_execution):
    ''' this function is the same as MainSciantix.C but it uses pybind11 '''

    sciantixModule.setTestPath(Path_of_execution)

    # Ensure correct data types and convert arrays
    Sciantix_options = np.array(sciantixModule.Sciantix_options, dtype=np.int32)
    Sciantix_variables = np.array(sciantixModule.Sciantix_variables, dtype=np.float64)
    Sciantix_scaling_factors = np.array(sciantixModule.Sciantix_scaling_factors, dtype=np.float64)
    Sciantix_history = np.array(sciantixModule.Sciantix_history, dtype=np.float64)
    Sciantix_diffusion_modes = np.array(sciantixModule.Sciantix_diffusion_modes, dtype=np.float64)
    Time_input = np.array(sciantixModule.Time_input, dtype=np.float64)
    Temperature_input = np.array(sciantixModule.Temperature_input, dtype=np.float64)
    Fissionrate_input = np.array(sciantixModule.Fissionrate_input, dtype=np.float64)
    Hydrostaticstress_input = np.array(sciantixModule.Hydrostaticstress_input, dtype=np.float64)
    Steampressure_input = np.array(sciantixModule.Steampressure_input, dtype=np.float64)

    # Fetch the integer and float variables
    Input_history_points = int(sciantixModule.Input_history_points)

    # Print the initial values from C++ before calling InputReading
    # Time_end_h = sciantixModule.getTimeEndH()
    # Time_end_s = sciantixModule.getTimeEndS()
    Time_end_h = np.array([0.0], dtype=np.float64)  # Initialize with zero or appropriate value
    Time_end_s = np.array([0.0], dtype=np.float64)  # Initialize with zero or appropriate value

    print(f"P (Before InputReading): Time_end_h: {Time_end_h}")
    print(f"P (Before InputReading): Time_end_s: {Time_end_s}")

    # Call InputReading with Time_end_h and Time_end_s
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

    # After InputReading, get the updated values from C++
    print(f"P (After InputReading): Time_end_h: {Time_end_h[0]}")
    print(f"P (After InputReading): Time_end_s: {Time_end_s[0]}")

    print(Sciantix_options) # updated
    sciantixModule.Sciantix_options = Sciantix_options
    print(sciantixModule.Sciantix_options) # updated
    # print(Sciantix_variables)

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

    # print(sciantixModule.Time_h)
    # print(sciantixModule.Time_end_h)

    while sciantixModule.Time_h <= sciantixModule.Time_end_h:

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

        # time calculation
        dTime_h = sciantixModule.TimeStepCalculation(
            Input_history_points,  # Pass the input history points
            sciantixModule.Time_h,  # Pass the current simulation time (in hours)
            sciantixModule.Time_input,  # Pass the time input array
            sciantixModule.Number_of_time_steps_per_interval  # Pass the steps per interval
        )

        Sciantix_history[6] = sciantixModule.dTime_h * 3600

        # print(sciantixModule.Time_h)

        if sciantixModule.Time_h < sciantixModule.Time_end_h:
            sciantixModule.setSciantixTimeStepNumber(sciantixModule.Time_step_number + 1)
            
            sciantixModule.setSciantixTimeH(sciantixModule.dTime_h + sciantixModule.Time_h)
            sciantixModule.setSciantixTimeS(sciantixModule.Time_s + Sciantix_history[6])
            
        else:
            break
        
        # set the History 
        for i in range(20):
            sciantixModule.setHistory(i, Sciantix_history[i])
    
    timer_end = time.time()
    logExecutionTime(timer_end - timer_start, sciantixModule.Time_step_number)

if __name__ == "__main__":
    main(path)
