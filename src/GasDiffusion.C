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
//////////////////////////////////////////////////////////////////////////////////////

#include "GasDiffusion.h"

/**
 * @brief Defines models for gas diffusion within the fuel grain.
 * 
 * This function computes diffusion models for gas atoms within the fuel grain
 * based on the selected diffusion solver option.
 */
void GasDiffusion()
{
	switch (static_cast<int>(input_variable[iv["iDiffusionSolver"]].getValue()))
	{
		case 1:
			defineSpectralDiffusion1Equation();
			break;

		case 2:
			defineSpectralDiffusion2Equations();
			break;

		default:
			errorHandling();
			break;
	}
}

/**
 * @brief Defines diffusion models using the spectral diffusion with one equation.
 */
void defineSpectralDiffusion1Equation()
{
	std::string reference;

    for (auto& system : sciantix_system)
	{
		model.emplace_back();
		int modelIndex = static_cast<int>(model.size()) - 1;
		model[modelIndex].setName("Gas diffusion - " + system.getName());
		model[modelIndex].setRef(reference);

		std::vector<double> parameters;
		parameters.push_back(n_modes);
		double gasDiffusivity;
		if (system.getResolutionRate() + system.getTrappingRate() == 0)
		{
			gasDiffusivity = system.getFissionGasDiffusivity() * gas[ga[system.getGasName()]].getPrecursorFactor();
		}
		else
		{
			gasDiffusivity = (system.getResolutionRate() / (system.getResolutionRate() + system.getTrappingRate())) * system.getFissionGasDiffusivity() * gas[ga[system.getGasName()]].getPrecursorFactor() +
							 (system.getTrappingRate() / (system.getResolutionRate() + system.getTrappingRate())) * system.getBubbleDiffusivity();
		}
		parameters.push_back(gasDiffusivity);
		parameters.push_back(matrix[sma[system.getMatrixName()]].getGrainRadius());
		parameters.push_back(system.getProductionRate());
		parameters.push_back(gas[ga[system.getGasName()]].getDecayRate());

		model[modelIndex].setParameter(parameters);
	}
}

/**
 * @brief Defines diffusion models using the spectral diffusion with two equations.
 */
void defineSpectralDiffusion2Equations()
{
	std::string reference;

    for (auto& system : sciantix_system)
	{
		model.emplace_back();
		int modelIndex = static_cast<int>(model.size()) - 1;
		model[modelIndex].setName("Gas diffusion - " + system.getName());
		model[modelIndex].setRef(reference);

		std::vector<double> parameters;
		parameters.push_back(n_modes);
		parameters.push_back(system.getFissionGasDiffusivity() * gas[ga[system.getGasName()]].getPrecursorFactor());
		parameters.push_back(system.getResolutionRate());
		parameters.push_back(system.getTrappingRate());
		parameters.push_back(gas[ga[system.getGasName()]].getDecayRate());
		parameters.push_back(sciantix_variable[sv["Grain radius"]].getFinalValue());
		parameters.push_back(system.getProductionRate());
		parameters.push_back(0.0);  // source_term
		parameters.push_back(system.getBubbleDiffusivity());

		model[modelIndex].setParameter(parameters);
	}
}

/**
 * @brief Handles unsupported diffusion solver options.
 */
void errorHandling()
{
	ErrorMessages::Switch(__FILE__, "iDiffusionSolver", static_cast<int>(input_variable[iv["iDiffusionSolver"]].getValue()));
}
