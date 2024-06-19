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
Sciantix_options = np.array([0] * 40, dtype=np.intc)
Sciantix_history = np.array([0.0] * 20, dtype=np.float64)
Sciantix_variables = np.array([0.0] * 300, dtype=np.float64)
Sciantix_scaling_factors = np.array([1.0] * 10, dtype=np.float64)
Sciantix_diffusion_modes = np.array([0.0] * 1000 , dtype=np.float64 )

# Utilisation des variables du module sciantixModule
# Sciantix_options = sciantixModule.Sciantix_options
# Sciantix_history = sciantixModule.Sciantix_history
# Sciantix_variables = sciantixModule.Sciantix_variables
# Sciantix_scaling_factors = sciantixModule.Sciantix_scaling_factors
# Sciantix_diffusion_modes = sciantixModule.Sciantix_diffusion_modes

# Variables pour le suivi du temps et autres paramètres numériques
Time_step_number = sciantixModule.Time_step_number
Time_h = sciantixModule.Time_h
dTime_h = sciantixModule.dTime_h
Time_end_h = sciantixModule.Time_end_h
Time_s = sciantixModule.Time_s
Time_end_s = sciantixModule.Time_end_s
Number_of_time_steps_per_interval = sciantixModule.Number_of_time_steps_per_interval

# Points d'entrée pour diverses entrées de simulation
Input_history_points = sciantixModule.Input_history_points


# Listes pour les données d'entrée, synchronisées avec le module sciantixModule
Time_input = sciantixModule.Time_input
Temperature_input = sciantixModule.Temperature_input
Fissionrate_input = sciantixModule.Fissionrate_input
Hydrostaticstress_input = sciantixModule.Hydrostaticstress_input
Steampressure_input = sciantixModule.Steampressure_input




def Sciantix(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes):
    
    print("Sciantix_options:", Sciantix_options)
    print("Sciantix_history:", Sciantix_history)
    print("Sciantix_variables:", Sciantix_variables)
    print("Sciantix_scaling_factors:", Sciantix_scaling_factors)
    print("Sciantix_diffusion_modes:", Sciantix_diffusion_modes)

    
    # Set variables
    sciantixModule.SetVariables(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes)
    # Initialize components
    sciantixModule.SetGas()
    sciantixModule.SetMatrix()
    sciantixModule.SetSystem()
    
    
    print("Sciantix_options:", Sciantix_options)
    print("Sciantix_history:", Sciantix_history)
    print("Sciantix_variables:", Sciantix_variables)
    print("Sciantix_scaling_factors:", Sciantix_scaling_factors)
    print("Sciantix_diffusion_modes:", Sciantix_diffusion_modes)


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

    # Clear variables
    # sciantixModule.history_variable.clear()
    # sciantixModule.sciantix_variable.clear()
    # sciantixModule.sciantix_system.clear()
    # sciantixModule.physics_variable.clear()
    # sciantixModule.model.clear()
    # sciantixModule.material.clear()
    # sciantixModule.gas.clear()
    # sciantixModule.matrix.clear()




def logExecutionTime(timer, time_step_number):
    with open("execution.txt", "a") as execution_file:
        execution_file.write(f"{timer:.12e}\t{time.process_time()}\t{timer * time.process_time():.12e}\t{time_step_number}\n")


# Equivalent to MainSCIANTIX.C
def main():
    global Time_h, Time_end_h, Time_s, Time_step_number, dTime_h

    sciantixModule.InputReading()
    sciantixModule.Initialization()

    if os.path.exists("output.txt"):
        os.remove("output.txt")



    timer_start = time.time()

    while Time_h <= Time_end_h:
        Sciantix_history[0] = Sciantix_history[1]
        Sciantix_history[1] = sciantixModule.InputInterpolation(Time_h, Time_input, Temperature_input, Input_history_points)
        Sciantix_history[2] = Sciantix_history[3]
        Sciantix_history[3] = sciantixModule.InputInterpolation(Time_h, Time_input, Fissionrate_input, Input_history_points)
        if Sciantix_history[3] < 0.0:
            Sciantix_history[3] = 0.0
        Sciantix_history[4] = Sciantix_history[5]
        Sciantix_history[5] = sciantixModule.InputInterpolation(Time_h, Time_input, Hydrostaticstress_input, Input_history_points)
        Sciantix_history[7] = Time_h
        Sciantix_history[8] = float(Time_step_number)
        Sciantix_history[9] = Sciantix_history[10]
        Sciantix_history[10] = sciantixModule.InputInterpolation(Time_h, Time_input, Steampressure_input, Input_history_points)

        # The python Sciantix function
        Sciantix(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes)
        dTime_h = sciantixModule.TimeStepCalculation()
        Sciantix_history[6] = dTime_h * 3600

        if Time_h < Time_end_h:
            Time_step_number += 1
            Time_h += dTime_h
            Time_s += Sciantix_history[6]
        else:
            break

    timer_end = time.time()
    logExecutionTime(timer_end - timer_start, Time_step_number)


# Equivalent to MainSCIANTIX.C
if __name__ == "__main__":
    main()