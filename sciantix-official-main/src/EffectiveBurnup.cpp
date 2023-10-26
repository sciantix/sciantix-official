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
//  Authors: D. Pizzocri, G. Zullo, A. Magni, E. Redaelli                           //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "EffectiveBurnup.h"

void EffectiveBurnup()
{
	/**
	 * @brief This function defines the sciantix model *Effective burnup*.
	 * 
	 * The model *Effective burnup* is used to evaluate the local effecive burnup according to the local fission rate density and temperature.
	 * 
	 */

	model.emplace_back();
	int model_index = int(model.size()) - 1;

	model[model_index].setName("Effective burnup");

	std::string reference;
	std::vector<double> parameter;

	double temperature_threshold = matrix[0].getHealingTemperatureThreshold();

	if ((history_variable[hv["Temperature"]].getFinalValue()) <= temperature_threshold)
		parameter.push_back(sciantix_variable[sv["Specific power"]].getFinalValue() / 86400.0);
	else if (history_variable[hv["Temperature"]].getFinalValue() > temperature_threshold && history_variable[hv["Temperature"]].getInitialValue() < temperature_threshold)
		parameter.push_back(sciantix_variable[sv["Specific power"]].getFinalValue() / 86400.0);
	else
		parameter.push_back(0.0);

	/// @ref G. Khvostov et al., WRFPM-2005, Kyoto, Japan, 2005
	reference += ": G. Khvostov et al., WRFPM-2005, Kyoto, Japan, 2005.";

	model[model_index].setParameter(parameter);
	model[model_index].setRef(reference);
}
