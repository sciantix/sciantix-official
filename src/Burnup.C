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

#include "Burnup.h"

void Burnup()
{	
	/**
	 * @brief This function defines the sciantix model *Burnup*.
	 * 
	 * The model Burnup is used to evaluate the local fuel burnup according to the local power density, evaluated from the local fission rate density.
	 * The calculation is executed in void Simulation::BurnupEvolution(), by using the Solver::Integrator.
	 * 
	 */

	model.emplace_back();
	int model_index = int(model.size()) - 1;
	model[model_index].setName("Burnup");

	sciantix_variable[sv["Specific power"]].setFinalValue((history_variable[hv["Fission rate"]].getFinalValue() * (3.12e-17) / sciantix_variable[sv["Fuel density"]].getFinalValue()));
	
	std::string reference = "The local burnup is calculated from the fission rate density.";
	std::vector<double> parameter;
	parameter.push_back(sciantix_variable[sv["Specific power"]].getFinalValue() / 86400.0); // conversion to get burnup in MWd/kg

	model[model_index].setParameter(parameter);
	model[model_index].setRef(reference);
}
