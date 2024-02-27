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
        /**
         * This case defines the vented fraction triggering the fission gas release
         *
        */ 

        // screw parameter
        const double screw_parameter = 0.1;

        // span parameter
        const double span_parameter = 10.0;

        // cent parameter
        const double cent_parameter = 0.43;

		double sigmoid_variable;
		sigmoid_variable = sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue() *
			exp(-sciantix_variable[sv["Intergranular fractional intactness"]].getIncrement());

		// Vented fraction
		sciantix_variable[sv["Intergranular vented fraction"]].setFinalValue(
			1.0 /
			pow((1.0 + screw_parameter *
				exp(- span_parameter *
					(sigmoid_variable - cent_parameter))),
				(1.0 / screw_parameter))
		);

		// Venting probability
		sciantix_variable[sv["Intergranular venting probability"]].setFinalValue(
			(1.0 - sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue()) +
			sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue() * sciantix_variable[sv["Intergranular vented fraction"]].getFinalValue()
		);

        reference = "Pizzocri et al., D6.4 (2020), H2020 Project INSPYRE";

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
		ErrorMessages::Switch(__FILE__, "iGrainBoundaryVenting", int(input_variable[iv["iGrainBoundaryVenting"]].getValue()));
		break;
	}

	model[model_index].setParameter(parameter);
	model[model_index].setRef(reference);

}

