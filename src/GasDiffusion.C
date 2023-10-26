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

#include "GasDiffusion.h"

void GasDiffusion()
{
	/**
	 * @brief This routine defines the models for the diffusion of atoms within the fuel grain.
	 * The intragranular diffusion exploits the hypothesis of the equivalent Booth approach,
	 * namely, considering the atom diffusion in a spherical fuel grain.
	 * 
	 * 
	 */

	std::string reference;
	switch (int(input_variable[iv["iDiffusionSolver"]].getValue()))
	{
	case 1:
	{
		int model_index;
		std::vector<double> parameter;
		for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
		{
			model.emplace_back();
			model_index = int(model.size()) - 1;
			model[model_index].setName("Gas diffusion - " + sciantix_system[i].getName());
			model[model_index].setRef(reference);

			parameter.push_back(n_modes);
			if (sciantix_system[i].getResolutionRate() + sciantix_system[i].getTrappingRate() == 0)
				parameter.push_back(sciantix_system[i].getFissionGasDiffusivity() * gas[ga[sciantix_system[i].getGasName()]].getPrecursorFactor());
			else
				parameter.push_back(
					sciantix_system[i].getResolutionRate() /
					(sciantix_system[i].getResolutionRate() + sciantix_system[i].getTrappingRate()) * sciantix_system[i].getFissionGasDiffusivity() * gas[ga[sciantix_system[i].getGasName()]].getPrecursorFactor() + 
					sciantix_system[i].getTrappingRate() /
					(sciantix_system[i].getResolutionRate() + sciantix_system[i].getTrappingRate()) * sciantix_system[i].getBubbleDiffusivity() 
				);

			parameter.push_back(sciantix_variable[sv["Grain radius"]].getFinalValue());
			parameter.push_back(sciantix_system[i].getProductionRate());
			parameter.push_back(gas[ga[sciantix_system[i].getGasName()]].getDecayRate());

			model[model_index].setParameter(parameter);
			parameter.clear();
		}
		break;
	}

	case 2:
	{
		// parameter --> N
		//               diffusion_coefficient
		//               resolution_rate
		//               trapping_rate
		//               decay rate 
		//               domain_radius 
		//               source_term 
		//               source_term_bubbles
		int model_index;
		std::vector<double> parameter;

		for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
		{
			model.emplace_back();
			model_index = int(model.size()) - 1;
			model[model_index].setName("Gas diffusion - " + sciantix_system[i].getName());
			model[model_index].setRef(reference);

			parameter.push_back(n_modes);
			parameter.push_back(sciantix_system[i].getFissionGasDiffusivity() * gas[ga[sciantix_system[i].getGasName()]].getPrecursorFactor());
			parameter.push_back(sciantix_system[i].getResolutionRate());
			parameter.push_back(sciantix_system[i].getTrappingRate());
			parameter.push_back(gas[ga[sciantix_system[i].getGasName()]].getDecayRate());

			parameter.push_back(sciantix_variable[sv["Grain radius"]].getFinalValue());

			parameter.push_back(sciantix_system[i].getProductionRate());
			parameter.push_back(0.0);
			parameter.push_back(sciantix_system[i].getBubbleDiffusivity());
			
			model[model_index].setParameter(parameter);
			parameter.clear();
		}
		break;
	}

	default:
		ErrorMessages::Switch("GasDiffusion.cpp", "iDiffusionSolver", int(input_variable[iv["iDiffusionSolver"]].getValue()));
		break;
	}
}
