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
    /**
     * @brief GrainBoundaryVenting() defines models for release mechanisms caused by venting through open porosities
    */

    model.emplace_back();
    int model_index = int(model.size()) - 1;
    model[model_index].setName("Grain-boundary venting");

    std::vector<double> parameter;
    std::string reference;

    switch (int(input_variable[iv["iGrainBoundaryVenting"]].getValue()))
    {
    case 0:
    {
        /**
         * @brief Not considered.
         * 
        */

        sciantix_variable[sv["Intergranular venting probability"]].setFinalValue(0.0);
        reference = "not considered.";

        break;
    }

    case 1:
    {
        /**
         * @brief Release mechanisms quantified via the vented fraction
         *
        */
       
        // Shape of the sigmoid function
        const double screw_parameter = 0.1;
        const double span_parameter = 10.0;
        const double cent_parameter = 0.43;

		double sigmoid_variable;
		sigmoid_variable = sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue() *
			exp(-sciantix_variable[sv["Intergranular fractional intactness"]].getIncrement());

		// Vented fraction
		sciantix_variable[sv["Intergranular vented fraction"]].setFinalValue(
			1.0 / pow( (1.0 + screw_parameter * exp(- span_parameter * (sigmoid_variable - cent_parameter))) , (1.0 / screw_parameter))
		);

        // Venting probability
        sciantix_variable[sv["Intergranular venting probability"]].setFinalValue(
            (1.0 - sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue())
            + sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue() * sciantix_variable[sv["Intergranular vented fraction"]].getFinalValue()
        );

        reference = "Pizzocri et al., D6.4 (2020), H2020 Project INSPYRE";

        break;
    }

    default:
        ErrorMessages::Switch("GrainBoundaryVenting", "iGrainBoundaryVenting", int(input_variable[iv["iGrainBoundaryVenting"]].getValue()));
        break;
    }

    parameter.push_back(sciantix_variable[sv["Intergranular venting probability"]].getFinalValue());

    model[model_index].setParameter(parameter);
    model[model_index].setRef(reference);
}

