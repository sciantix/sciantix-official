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

/// Model: Gas production
/// This model calculates the concentration of fission gas (Xe+Kr) produced by fission reactions in the fuel

void GasProduction()
{
	int model_index;
	std::vector<double> parameter;

	for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
	{
		model.emplace_back();
		model_index = int(model.size()) - 1;
		model[model_index].setName("Gas production - " + sciantix_system[i].getName());

		model[model_index].setRef(" ");

		parameter.push_back(sciantix_system[i].getProductionRate());
		parameter.push_back(physics_variable[pv["Time step"]].getFinalValue());

		model[model_index].setParameter(parameter);

		parameter.clear();
	}
}
