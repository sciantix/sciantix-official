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

#include "GrainBoundaryVenting.h"

void GrainBoundaryVenting()
{
	model.emplace_back();
	int model_index = int(model.size()) - 1;
	model[model_index].setName("Grain-boundary venting");

	std::vector<double> parameter;
	std::string reference;

	/// @brief
	/// These model defines the venting of fission gas from the grain boundaries of the UO2 fuel matrix.

	switch (int(input_variable[iv["iGrainBoundaryVenting"]].getValue()))
	{
	case 0:
	{
		/// @brief
		/// This case corresponds to no grain boundary venting.

		reference = "not considered.";

		parameter.push_back(0.0);
		parameter.push_back(0.0);
		parameter.push_back(0.0);

		break;
	}

	case 1:
	{
		/// @brief
		/// This case defines a set of parameters of the sigmoid function describing the venting probability 

		// screw parameter
		const double screw_parameter = 0.1;
		parameter.push_back(sf_screw_parameter * screw_parameter);

		// span parameter
		const double span_parameter = 10.0;
		parameter.push_back(sf_span_parameter * span_parameter);

		// cent parameter
		const double cent_parameter = 0.43;
		parameter.push_back(sf_cent_parameter * cent_parameter);

		reference = "from Pizzocri et al., D6.4 (2020), H2020 Project INSPYRE";

		break;
	}

	default:
		ErrorMessages::Switch("GrainBoundaryVenting.cpp", "iGrainBoundaryVenting", int(input_variable[iv["iGrainBoundaryVenting"]].getValue()));
		break;
	}

	model[model_index].setParameter(parameter);
	model[model_index].setRef(reference);

}

