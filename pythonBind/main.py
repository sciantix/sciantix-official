import time
import os
from sciantix_module import getMainVar, Sciantix, sciantixModule

def logExecutionTime(timer, time_step_number):
    with open("execution.txt", "a") as execution_file:
        execution_file.write(f"{timer:.12e}\t{time.process_time()}\t{timer * time.process_time():.12e}\t{time_step_number}\n")

def main():
    
    sciantixModule.InputReading()
    sciantixModule.Initialization()

    getMainVar()
    # print(sciantixModule.Sciantix_history)

    if os.path.exists("output.txt"):
        os.remove("output.txt")

    timer_start = time.time()

    n = 1 
    while sciantixModule.Time_h <= sciantixModule.Time_end_h:
        
        s2 = sciantixModule.getVariablesInArray_double()
        print("zoubaby" ,s2[3])

        Sciantix_history = sciantixModule.getHistoryInArray_double()
        for i in range(20):
            if n == 1:
                print(sciantixModule.Sciantix_history[i])

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
        
        Sciantix(sciantixModule.getOptionsInArray_int(), Sciantix_history, sciantixModule.getVariablesInArray_double(), sciantixModule.getScalingFactorsInArray_double(), sciantixModule.getDiffusionModesInArray_double())

        getMainVar()
        sciantixModule.setSciantixDTimeH(sciantixModule.TimeStepCalculation())
        getMainVar()
        Sciantix_history[6] = sciantixModule.dTime_h * 3600
        
        if sciantixModule.Time_h < sciantixModule.Time_end_h:
            sciantixModule.setSciantixTimeStepNumber(sciantixModule.Time_step_number + 1)
            
            sciantixModule.setSciantixTimeH(sciantixModule.dTime_h + sciantixModule.Time_h)
            sciantixModule.setSciantixTimeS(sciantixModule.Time_s + Sciantix_history[6])
            getMainVar()

        else:
            break
        
        for i in range(20):
            sciantixModule.setHistory(i, Sciantix_history[i])

        for i in range(20):
            if n == 1:
                print(sciantixModule.Sciantix_history[i])
            
        n = 2
    
    timer_end = time.time()
    logExecutionTime(timer_end - timer_start, sciantixModule.Time_step_number)

if __name__ == "__main__":
    main()
