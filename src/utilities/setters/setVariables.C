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

#include "setVariables.h"

/// SetVariables
/// This routine builds the vectors of objects:
/// - physics_variable
/// - history_variable
/// - sciantix_variable
/// - input_variable
/// together with the diffusion modes, and the scaling factors.

void SetVariables(
	int Sciantix_options[], 
	double Sciantix_history[], 
	double Sciantix_variables[], 
	double Sciantix_scaling_factors[], 
	double Sciantix_diffusion_modes[],
    VariableArray<InputVariable> &input_variable,
    VariableArray<PhysicsVariable> &history_variable,
    VariableArray<PhysicsVariable> &sciantix_variable,
	VariableArray<PhysicsVariable> &physics_variable,
	std::vector<double> modes_initial_conditions
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
	
	physics_variable.push(PhysicsVariable("Time step", "(s)", Sciantix_history[6], Sciantix_history[6], 0));


	// ----------------
	// History variable
	// ----------------
	
	
	std::vector<PhysicsVariable> initial_history_values = initHistoryVariableValues(
		Sciantix_history,
		toOutputStoichiometryDeviation
	);

	for (PhysicsVariable initial_value : initial_history_values)
	{
		history_variable.push(initial_value);
	}


	// ----------------------------------------------------------------------------
	// Sciantix variable
	// ----------------------------------------------------------------------------
	

	std::vector<PhysicsVariable> initial_sciantix_values = initSciantixVariableValues(
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

	for (PhysicsVariable initial_value : initial_sciantix_values)
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
		modes_initial_conditions[i] = Sciantix_diffusion_modes[i]; // Xe
		for (int j = 1; j <= 17; j++)
		{
			modes_initial_conditions[j * modes_initial_conditions.size() + i] = Sciantix_diffusion_modes[j * modes_initial_conditions.size() + i];	
		}
	}

	// ---------------
	// Scaling factors
	// ---------------
	sf_resolution_rate = Sciantix_scaling_factors[0];
	sf_trapping_rate = Sciantix_scaling_factors[1];
	sf_nucleation_rate = Sciantix_scaling_factors[2];
	sf_diffusivity = Sciantix_scaling_factors[3];
	sf_temperature = Sciantix_scaling_factors[4];
	sf_fission_rate = Sciantix_scaling_factors[5];
	sf_cent_parameter = Sciantix_scaling_factors[6];
	sf_helium_production_rate = Sciantix_scaling_factors[7];
	sf_dummy = Sciantix_scaling_factors[8];
}
