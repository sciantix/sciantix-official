import sys
import os
import sciantixModule

# Ensure the module path is correctly set for importing sciantixModule
module_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'build', 'python'))
if module_path not in sys.path:
    sys.path.append(module_path)

def getMainVar():
    """This function fetch the values of globals attributs in the C++/C code 
    """
    # --- All the global variables to the SciantixModule --- # 
    # sciantixModule.Sciantix_variables = sciantixModule.getVariablesInArray_double()
    # sciantixModule.Sciantix_history = sciantixModule.getHistoryInArray_double()
    # sciantixModule.Sciantix_diffusion_modes = sciantixModule.getDiffusionModesInArray_double()
    # sciantixModule.Sciantix_options = sciantixModule.getOptionsInArray_int()
    # sciantixModule.Sciantix_scaling_factors = sciantixModule.getScalingFactorsInArray_double()
    # sciantixModule.Time_step_number = sciantixModule.getSciantixTimeStepNumberConversion()
    sciantixModule.Time_h = sciantixModule.getSciantixTimeHConversion()
    sciantixModule.dTime_h = sciantixModule.getSciantixDTimeHConversion()
    sciantixModule.Time_end_h = sciantixModule.getSciantixTimeEndHConversion()
    sciantixModule.Time_s = sciantixModule.getSciantixTimeSConversion()
    # sciantixModule.Input_history_points = sciantixModule.getSciantixInputHistoryPointsConversion()
    # sciantixModule.Time_input = sciantixModule.getSciantixTimeInputConversion()
    # sciantixModule.Temperature_input = sciantixModule.getSciantixTemperatureInputConversion()
    # sciantixModule.Fissionrate_input = sciantixModule.getSciantixFissionrateInputConversion()
    # sciantixModule.Hydrostaticstress_input = sciantixModule.getSciantixHydrostaticstressInputConversion()
    # sciantixModule.Steampressure_input = sciantixModule.getSciantixSteampressureInputConversion()
    # sciantixModule.gas = sciantixModule.getGas()
    # sciantixModule.model = sciantixModule.getModel()
    # sciantixModule.matrix = sciantixModule.getMatrix()
    # sciantixModule.material = sciantixModule.getMaterial()
    # sciantixModule.history_variable = sciantixModule.getHistoryVariable()
    # sciantixModule.physics_variable = sciantixModule.getPhysicsVariable()
    # sciantixModule.sciantix_system = sciantixModule.getSystem()
    # sciantixModule.sciantix_variable = sciantixModule.getSciantixVariable()


def initialize_simulation(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes):
    """Initialize the Sciantix simulation using the provided input arrays."""
    sciantixModule.initialize_simulation(
        Sciantix_options, 
        Sciantix_history, 
        Sciantix_variables, 
        Sciantix_scaling_factors, 
        Sciantix_diffusion_modes
    )


def update_simulation(Sciantix_variables, Sciantix_diffusion_modes):
    """Update the Sciantix simulation with updated variables."""
    sciantixModule.update_simulation(Sciantix_variables, Sciantix_diffusion_modes)


def Sciantix(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes):
    """This function is the Python function of the simulation of Sciantix."""

    # Initialize the Sciantix simulation
    initialize_simulation(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes)

    # Proceed with the specific simulation steps
    sciantix_simulation = sciantixModule.Simulation.getInstance()

    # Set the various components of the simulation
    sciantix_simulation.setGas()
    sciantix_simulation.setMatrix()
    sciantix_simulation.setSystem()

    # Perform specific physics calculations in the simulation
    sciantix_simulation.Burnup()
    sciantix_simulation.EffectiveBurnup()
    sciantix_simulation.UO2Thermochemistry()
    sciantix_simulation.StoichiometryDeviation()
    sciantix_simulation.HighBurnupStructureFormation()
    sciantix_simulation.HighBurnupStructurePorosity()
    sciantix_simulation.GrainGrowth()
    sciantix_simulation.GrainBoundarySweeping()
    sciantix_simulation.GasProduction()
    sciantix_simulation.GasDecay()
    sciantix_simulation.IntraGranularBubbleBehavior()
    sciantix_simulation.GasDiffusion()
    sciantix_simulation.GrainBoundaryMicroCracking()
    sciantix_simulation.GrainBoundaryVenting()
    sciantix_simulation.InterGranularBubbleBehavior()

    # Update the simulation variables and diffusion modes after the physics calculations
    update_simulation(Sciantix_variables, Sciantix_diffusion_modes)

    # Output the final results of the simulation
    sciantix_simulation.output()
