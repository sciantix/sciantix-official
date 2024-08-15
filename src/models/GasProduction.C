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

#include "Simulation.h"

void Simulation::GasProduction()
{
	// Model declaration
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

	// Mapping the model
	MapModel();

	// Model resolution
	for (auto& system : sciantix_system)
	{	
		if(system.getRestructuredMatrix() == 0)
			sciantix_variable[sv[system.getGasName() + " produced"]].setFinalValue(
				solver.Integrator(
					sciantix_variable[sv[system.getGasName() + " produced"]].getInitialValue(),
					model[sm["Gas production - " + system.getName()]].getParameter().at(0),
					model[sm["Gas production - " + system.getName()]].getParameter().at(1)
				)
			);
		else if(system.getRestructuredMatrix() == 1)
			sciantix_variable[sv[system.getGasName() + " produced in HBS"]].setFinalValue(
				solver.Integrator(
					sciantix_variable[sv[system.getGasName() + " produced in HBS"]].getInitialValue(),
					model[sm["Gas production - " + system.getName()]].getParameter().at(0),
					model[sm["Gas production - " + system.getName()]].getParameter().at(1)
				)
			);
	}

}
