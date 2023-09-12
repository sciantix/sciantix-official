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

#include "GrainBoundaryMicroCracking.h"

void GrainBoundaryMicroCracking()
{
	if (!input_variable[iv["iGrainBoundaryMicroCracking"]].getValue()) return;

	model.emplace_back();
	int model_index = int(model.size()) - 1;
	model[model_index].setName("Grain-boundary micro-cracking");
	std::vector<double> parameter;

	const double dTemperature = history_variable[hv["Temperature"]].getIncrement();

	const bool heating = (dTemperature > 0.0) ? 1 : 0;
	const double transient_type = heating ? +1.0 : -1.0;
	const double span = 10.0;

	// microcracking parameter
	const double inflection = 1773.0 + 520.0 * exp(-sciantix_variable[sv["Burnup"]].getFinalValue() / (10.0 * 0.8814));
	const double exponent = 33.0;
	const double arg = (transient_type / span) * (history_variable[hv["Temperature"]].getFinalValue() - inflection);
	const double microcracking_parameter = (transient_type / span) * exp(arg) * pow((exponent * exp(arg) + 1), -1. / exponent - 1.); // dm/dT

	parameter.push_back(microcracking_parameter);

	// healing parameter
	const double healing_parameter = 1.0 / 0.8814; // 1 / (u * burnup)
	parameter.push_back(healing_parameter);

	model[model_index].setParameter(parameter);
	model[model_index].setRef("from Barani et al. (2017), JNM");
}

