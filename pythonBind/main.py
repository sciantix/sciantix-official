import sys
import os
import time
import numpy as np

# path to the compiled sciantixModule 
module_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'build', 'python'))
if module_path not in sys.path:
    # add the module to the system path
    sys.path.append(module_path)

# importation of the Module
import sciantixModule

Sciantix_variables = sciantixModule.getVariablesInArray_double()

Sciantix_history = sciantixModule.getHistoryInArray_double()
Sciantix_diffusion_modes = sciantixModule.getDiffusionModesInArray_double()
Sciantix_options = sciantixModule.getOptionsInArray_int()
Sciantix_scaling_factors = sciantixModule.getScalingFactorsInArray_double()
Time_step_number = sciantixModule.getSciantixTimeStepNumberConversion()
Time_h = sciantixModule.getSciantixTimeHConversion()
dTime_h = sciantixModule.getSciantixDTimeHConversion()
Time_end_h = sciantixModule.getSciantixTimeEndHConversion()
Time_s = sciantixModule.getSciantixTimeSConversion()
Input_history_points = sciantixModule.getSciantixInputHistoryPointsConversion()
Temperature_input = sciantixModule.getSciantixTemperatureInputConversion()
Fissionrate_input = sciantixModule.getSciantixFissionrateInputConversion()
Hydrostaticstress_input = sciantixModule.getSciantixHydrostaticstressInputConversion()
Steampressure_input = sciantixModule.getSciantixSteampressureInputConversion()


def getMainVar():
        sciantixModule.Sciantix_variables = sciantixModule.getVariablesInArray_double()
        sciantixModule.Sciantix_history = sciantixModule.getHistoryInArray_double()
        sciantixModule.Sciantix_diffusion_modes = sciantixModule.getDiffusionModesInArray_double()
        sciantixModule.Sciantix_options = sciantixModule.getOptionsInArray_int()
        sciantixModule.Sciantix_scaling_factors = sciantixModule.getScalingFactorsInArray_double()
        sciantixModule.Time_step_number = sciantixModule.getSciantixTimeStepNumberConversion()
        sciantixModule.Time_h = sciantixModule.getSciantixTimeHConversion()
        sciantixModule.dTime_h = sciantixModule.getSciantixDTimeHConversion()
        sciantixModule.Time_end_h = sciantixModule.getSciantixTimeEndHConversion()
        sciantixModule.Time_s = sciantixModule.getSciantixTimeSConversion()
        sciantixModule.Input_history_points = sciantixModule.getSciantixInputHistoryPointsConversion()
        sciantixModule.Temperature_input = sciantixModule.getSciantixTemperatureInputConversion()
        sciantixModule.Fissionrate_input = sciantixModule.getSciantixFissionrateInputConversion()
        sciantixModule.Hydrostaticstress_input = sciantixModule.getSciantixHydrostaticstressInputConversion()
        sciantixModule.Steampressure_input = sciantixModule.getSciantixSteampressureInputConversion()

def Sciantix(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes):

    # Set variables Sciantix_options,Sciantix_history,Sciantix_variables,Sciantix_scaling_factors,Sciantix_diffusion_modes = 
    sciantixModule.SetVariables(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes)
    # Initialize components

    sciantixModule.SetGas()
    sciantixModule.SetMatrix()
    sciantixModule.SetSystem()
    # Create a Simulation instance
    sciantix_simulation = sciantixModule.Simulation()

    # Perform simulation steps
    sciantixModule.Burnup()
    sciantixModule.MapModel()
    sciantix_simulation.Burnup()

    sciantixModule.EffectiveBurnup()
    sciantixModule.MapModel()
    sciantix_simulation.EffectiveBurnup()

    sciantixModule.EnvironmentComposition()
    sciantixModule.MapModel()

    sciantixModule.UO2Thermochemistry()
    sciantixModule.MapModel()
    sciantix_simulation.UO2Thermochemistry()

    sciantixModule.StoichiometryDeviation()
    sciantixModule.MapModel()
    sciantix_simulation.StoichiometryDeviation()

    sciantixModule.HighBurnupStructureFormation()
    sciantixModule.MapModel()
    sciantix_simulation.HighBurnupStructureFormation()

    sciantixModule.HighBurnupStructurePorosity()
    sciantixModule.MapModel()
    sciantix_simulation.HighBurnupStructurePorosity()

    sciantixModule.GrainGrowth()
    sciantixModule.MapModel()
    sciantix_simulation.GrainGrowth()

    sciantixModule.GrainBoundarySweeping()
    sciantixModule.MapModel()
    sciantix_simulation.GrainBoundarySweeping()

    sciantixModule.GasProduction()
    sciantixModule.MapModel()
    sciantix_simulation.GasProduction()

    sciantix_simulation.GasDecay()

    sciantixModule.IntraGranularBubbleEvolution()
    sciantixModule.MapModel()
    sciantix_simulation.IntraGranularBubbleBehaviour()

    sciantixModule.GasDiffusion()
    sciantixModule.MapModel()
    sciantix_simulation.GasDiffusion()

    sciantixModule.GrainBoundaryMicroCracking()
    sciantixModule.MapModel()
    sciantix_simulation.GrainBoundaryMicroCracking()

    sciantixModule.GrainBoundaryVenting()
    sciantixModule.MapModel()
    sciantix_simulation.GrainBoundaryVenting()

    sciantixModule.InterGranularBubbleEvolution()
    sciantixModule.MapModel()
    sciantix_simulation.InterGranularBubbleBehaviour()

    sciantixModule.FiguresOfMerit()


    sciantixModule.UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes)

    sciantixModule.Output()

    #Clear variables
    sciantixModule.clearHistoryVariable()
    sciantixModule.clearSciantixVariable()
    sciantixModule.clearSystem()
    sciantixModule.clearPhysicsVariable()
    sciantixModule.clearModel()
    sciantixModule.clearMaterial()
    sciantixModule.clearGas()
    sciantixModule.clearMatrix()





def logExecutionTime(timer, time_step_number):
    with open("execution.txt", "a") as execution_file:
        execution_file.write(f"{timer:.12e}\t{time.process_time()}\t{timer * time.process_time():.12e}\t{time_step_number}\n")


# Equivalent to MainSCIANTIX.C
def main():
    
   
    sciantixModule.InputReading()
    
    sciantixModule.Initialization()

    getMainVar()

    print(sciantixModule.Sciantix_history)

    if os.path.exists("output.txt"):
        os.remove("output.txt")

    timer_start = time.time()

    n=1 
    while sciantixModule.Time_h <= sciantixModule.Time_end_h:
        
        Sciantix_history = sciantixModule.getHistoryInArray_double()
        for i in range (20):
            if(n==1):
                print(sciantixModule.Sciantix_history[i])
                
        Sciantix_history[0] = Sciantix_history[1]
        Sciantix_history[1] = sciantixModule.InputInterpolation(sciantixModule.Time_h, sciantixModule.Time_input, sciantixModule.Temperature_input, sciantixModule.Input_history_points)
        Sciantix_history[2] = Sciantix_history[3]
        Sciantix_history[3] = sciantixModule.InputInterpolation(sciantixModule.Time_h, sciantixModule.Time_input, sciantixModule.Fissionrate_input, sciantixModule.Input_history_points)
        # sciantixModule.setHistory(0, sciantixModule.Sciantix_history[1])
        # sciantixModule.setHistory(1, sciantixModule.InputInterpolation(sciantixModule.Time_h, sciantixModule.Time_input, sciantixModule.Temperature_input, sciantixModule.Input_history_points))
        # sciantixModule.setHistory(2, sciantixModule.Sciantix_history[3])
        # sciantixModule.setHistory(3, sciantixModule.InputInterpolation(sciantixModule.Time_h, sciantixModule.Time_input, sciantixModule.Fissionrate_input, sciantixModule.Input_history_points))
        if Sciantix_history[3] < 0.0:
            Sciantix_history[3] = 0.0
        # if sciantixModule.Sciantix_history[3] < 0.0:
        #             sciantixModule.setHistory(3, 0.0)
        Sciantix_history[4] = Sciantix_history[5]
        Sciantix_history[5] = sciantixModule.InputInterpolation(sciantixModule.Time_h, sciantixModule.Time_input, sciantixModule.Hydrostaticstress_input, sciantixModule.Input_history_points)
        Sciantix_history[7] = sciantixModule.Time_h
        Sciantix_history[8] = float(sciantixModule.Time_step_number)
        Sciantix_history[9] = Sciantix_history[10]
        Sciantix_history[10] = sciantixModule.InputInterpolation(sciantixModule.Time_h, sciantixModule.Time_input, sciantixModule.Steampressure_input, sciantixModule.Input_history_points)
        # sciantixModule.setHistory(4, sciantixModule.Sciantix_history[5])
        # sciantixModule.setHistory(5, sciantixModule.InputInterpolation(sciantixModule.Time_h, sciantixModule.Time_input, sciantixModule.Hydrostaticstress_input, sciantixModule.Input_history_points))
        # sciantixModule.setHistory(7, sciantixModule.Time_h)
        # sciantixModule.setHistory(8, float(sciantixModule.Time_step_number))
        # sciantixModule.setHistory(9, sciantixModule.Sciantix_history[10])
        # sciantixModule.setHistory(10, sciantixModule.InputInterpolation(sciantixModule.Time_h, sciantixModule.Time_input, sciantixModule.Steampressure_input, sciantixModule.Input_history_points))

        # The python Sciantix function
        Sciantix(sciantixModule.getOptionsInArray_int(), Sciantix_history, sciantixModule.getVariablesInArray_double(), sciantixModule.getScalingFactorsInArray_double(), sciantixModule.getDiffusionModesInArray_double())
        getMainVar()
        sciantixModule.setSciantixDTimeH(sciantixModule.TimeStepCalculation())
        getMainVar()
        Sciantix_history[6] = sciantixModule.dTime_h * 3600
        # sciantixModule.setHistory(6, sciantixModule.dTime_h * 3600)
        
        if sciantixModule.Time_h < sciantixModule.Time_end_h:
            sciantixModule.setSciantixTimeStepNumber(sciantixModule.Time_step_number + 1)
            sciantixModule.setSciantixTimeH(sciantixModule.dTime_h + sciantixModule.Time_h)
            sciantixModule.setSciantixTimeS(sciantixModule.Time_s + Sciantix_history[6])
        else:
            break
        
        
        for i in range (20):
            sciantixModule.setHistory(i ,Sciantix_history[i])

        for i in range (20):
            if(n==1):
                print(sciantixModule.Sciantix_history[i])
            
        n=2
    
    timer_end = time.time()
    logExecutionTime(timer_end - timer_start, sciantixModule.Time_step_number)


# Equivalent to MainSCIANTIX.C
if __name__ == "__main__":
    main()