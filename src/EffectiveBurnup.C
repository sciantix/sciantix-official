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
//  Authors: D. Pizzocri, G. Zullo                                                  //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "EffectiveBurnup.h"

void EffectiveBurnup()
{
	model.emplace_back();
	int model_index = int(model.size()) - 1;

	model[model_index].setName("Effective burnup");

	std::string reference;
	std::vector<double> parameter;

	const double temperature_threshold = 1273.15;

    if (history_variable[hv["Temperature"]].getFinalValue() <= temperature_threshold || (history_variable[hv["Temperature"]].getFinalValue() > temperature_threshold && history_variable[hv["Temperature"]].getInitialValue() < temperature_threshold))
		parameter.push_back(sciantix_variable[sv["Specific power"]].getFinalValue() / 86400.0);
	else
		parameter.push_back(0.0);

	reference += ": G. Khvostov et al., WRFPM-2005, Kyoto, Japan, 2005.";

	model[model_index].setParameter(parameter);
	model[model_index].setRef(reference);
}
