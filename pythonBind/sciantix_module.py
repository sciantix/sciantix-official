import sys
import os

# path to the compiled sciantixModule 
module_path = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'build', 'python'))
if module_path not in sys.path:
    # add the module to the system path
    sys.path.append(module_path)

# importation of the Module
import sciantixModule


def getMainVar():
    """This fonction Fetch the values of globals attributs in the C++/C code 
    """
    # --- All the global variables to the SciantixModule --- # 
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
    sciantixModule.Time_input = sciantixModule.getSciantixTimeInputConversion()
    sciantixModule.Temperature_input = sciantixModule.getSciantixTemperatureInputConversion()
    sciantixModule.Fissionrate_input = sciantixModule.getSciantixFissionrateInputConversion()
    sciantixModule.Hydrostaticstress_input = sciantixModule.getSciantixHydrostaticstressInputConversion()
    sciantixModule.Steampressure_input = sciantixModule.getSciantixSteampressureInputConversion()
    sciantixModule.gas = sciantixModule.getGas()
    sciantixModule.model = sciantixModule.getModel()
    sciantixModule.matrix = sciantixModule.getMatrix()
    sciantixModule.material = sciantixModule.getMaterial()
    sciantixModule.history_variable = sciantixModule.getHistoryVariable()
    sciantixModule.physics_variable = sciantixModule.getPhysicsVariable()
    sciantixModule.sciantix_system = sciantixModule.getSystem()
    sciantixModule.sciantix_variable = sciantixModule.getSciantixVariable()


def Sciantix(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes):
    """This fonction is the python fonction of the simulation of Sciantix 
    """
    
    # Set variables
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
    
    sciantixModule.UpdateVariables()
    
    getMainVar()
    sciantixModule.Output()

    # Clear variables
    sciantixModule.clearHistoryVariable()
    sciantixModule.clearSciantixVariable()
    sciantixModule.clearSystem()
    sciantixModule.clearPhysicsVariable()
    sciantixModule.clearModel()
    sciantixModule.clearMaterial()
    sciantixModule.clearGas()
    sciantixModule.clearMatrix()
