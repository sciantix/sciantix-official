//////////////////////////////////////////////////////////////////////////////////////
//       _______.  ______  __       ___      .__   __. .___________. __  ___   ___  //
//      /       | /      ||  |     /   \     |  \ |  | |           ||  | \  \ /  /  //
//     |   (----`|  ,----'|  |    /  ^  \    |   \|  | `---|  |----`|  |  \  V  /   //
//      \   \    |  |     |  |   /  /_\  \   |  . `  |     |  |     |  |   >   <    //
//  .----)   |   |  `----.|  |  /  _____  \  |  |\   |     |  |     |  |  /  .  \   //
//  |_______/     \______||__| /__/     \__\ |__| \__|     |__|     |__| /__/ \__\  //
//                                                                                  //
//  Originally developed by D. Pizzocri & T. Barani                                 //
//                                                                                  //
//  Version: 2.0                                                                    //
//  Year: 2022                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "SetVariables.h"
#include "Simulation.h"

/// SetVariables
/// This routine builds the vectors of objects:
/// - physics_variable
/// - history_variable
/// - sciantix_variable
/// - input_variable
/// together with the diffusion modes, and the scaling factors.

void Simulation::SetVariables(
	int Sciantix_options[], 
	double Sciantix_history[], 
	double Sciantix_variables[], 
	double Sciantix_scaling_factors[], 
	double Sciantix_diffusion_modes[]
)
{
	// -----------------------------------------------------------------------------------------------
	// Input variable
	// The vector is used to collect all user input settings relating to the choice of SCIANTIX models
	// -----------------------------------------------------------------------------------------------

	if (input_variable.empty())
	{
		std::vector<std::string> name_list = getInputVariableNames();
		for (int i = 0; i < name_list.size(); i++)
		{
			input_variable.push(InputVariable(name_list[i], i));
		}
	}


	bool toOutputRadioactiveFG = input_variable["iRadioactiveFissionGas"].getValue() != 0,
		toOutputVenting = input_variable["iGrainBoundaryVenting"].getValue() != 0,
		toOutputHelium = input_variable["iHelium"].getValue() != 0,
		toOutputCracking = input_variable["iGrainBoundaryMicroCracking"].getValue() != 0,
		toOutputFracture = input_variable["iGrainBoundaryMicroCracking"].getValue() == 2,
		toOutputGrainBoundary = input_variable["iGrainBoundaryBehaviour"].getValue() == 1,
		toOutputHighBurnupStructure = input_variable["iHighBurnupStructureFormation"].getValue() == 1,
		toOutputStoichiometryDeviation = input_variable["iStoichiometryDeviation"].getValue() > 0;

	// ----------------
	// Physics variable
	// ----------------
	
	physics_variable.push(SciantixVariable("Time step", "(s)", Sciantix_history[6], Sciantix_history[6], 0));


	// ----------------
	// History variable
	// ----------------
	
	
	std::vector<SciantixVariable> initial_history_values = initHistoryVariableValues(
		Sciantix_history,
		toOutputStoichiometryDeviation
	);

	for (SciantixVariable initial_value : initial_history_values)
	{
		history_variable.push(initial_value);
	}


	// ----------------------------------------------------------------------------
	// Sciantix variable
	// ----------------------------------------------------------------------------
	

	std::vector<SciantixVariable> initial_sciantix_values = initSciantixVariableValues(
			Sciantix_variables,
			toOutputRadioactiveFG,
			toOutputVenting,
			toOutputHelium,
			toOutputCracking,
			toOutputFracture,
			toOutputGrainBoundary,
			toOutputHighBurnupStructure,
			toOutputStoichiometryDeviation
		);

	for (SciantixVariable initial_value : initial_sciantix_values)
	{
		sciantix_variable.push(initial_value);
	}



	// ------------------------------------------------------------------------------------------------
	// ------------------------------------------------------------------------------------------------

	// ---------------
	// Diffusion modes
	// ---------------
	for (int i = 0; i < modes_initial_conditions.size(); ++i)
	{
		for (int j = 0; j <= 17; j++)
		{
			modes_initial_conditions[j * modes_initial_conditions.size() + i] = Sciantix_diffusion_modes[j * modes_initial_conditions.size() + i];	
		}
	}

	// ---------------
	// Scaling factors
	// ---------------
	int index = 0;
	for (std::string name : getScalingFactorsNames())
	{
		scaling_factors.push(InputVariable(name, Sciantix_scaling_factors[index]));
		index++;
	}
}
