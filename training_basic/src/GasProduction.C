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

#include "GasProduction.h"

/**
 * @brief Calculates the concentration of fission gas (Xe+Kr) produced by fission reactions in the fuel.
 */

void GasProduction()
{
	for (auto& system : sciantix_system)
	{
		int model_index = model.size();

		model.emplace_back();
		model[model_index].setName("Gas production - " + system.getName());
		model[model_index].setRef(" ");

		double productionRate = system.getProductionRate();
		double timeStep = physics_variable[pv["Time step"]].getFinalValue();

		std::vector<double> parameter;
		parameter.push_back(productionRate);
		parameter.push_back(timeStep);
		model[model_index].setParameter(parameter);

		parameter.clear();
	}
}
