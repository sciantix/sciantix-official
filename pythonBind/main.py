import time
import os
from sciantix_module import getMainVar, Sciantix, sciantixModule

def logExecutionTime(timer, time_step_number):
    with open("execution.txt", "a") as execution_file:
        execution_file.write(f"{timer:.12e}\t{time.process_time()}\t{timer * time.process_time():.12e}\t{time_step_number}\n")



# Equivatent to the MainSCIANTIX.C
def main():
    ''' this function is the same as MainSciantix.C but it uses pybind11 '''
    # Initialisation 
    sciantixModule.InputReading()
    sciantixModule.Initialization()

    # get the main variables
    getMainVar()

    # remove output.txt if exists
    if os.path.exists("output.txt"):
        os.remove("output.txt")

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
        Sciantix(sciantixModule.getOptionsInArray_int(), Sciantix_history, sciantixModule.getVariablesInArray_double(), sciantixModule.getScalingFactorsInArray_double(), sciantixModule.getDiffusionModesInArray_double())

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
    main()
