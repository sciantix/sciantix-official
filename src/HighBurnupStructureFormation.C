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

#include "HighBurnupStructureFormation.h"

void HighBurnupStructureFormation()
{
    /**
     * @brief HighBurnupStructureFormation
     * This model describes the formation of a HBS structure in UO2 fuel.
     * 
     * @author
     * A. Magni, E. Redaelli, G. Zullo
    */

	model.emplace_back();

	int model_index = int(model.size()) - 1;

	model[model_index].setName("High-burnup structure formation");

	std::string reference;
	std::vector<double> parameter;

	switch (int(input_variable[iv["iHighBurnupStructureFormation"]].getValue()))
	{
	case 0:
	{
		/// @brief
		/// iHighBurnupStructureFormation == 0
		/// ----------------------------------
		///
		/// This case corresponds to the no HBS forming in the UO2 fuel matrix.

		reference += ": not considered.";
		parameter.push_back(0.0);
		parameter.push_back(0.0);
		parameter.push_back(0.0);
		parameter.push_back(0.0);

		break;
	}

	case 1:
	{
		/// @brief
		/// iHighBurnupStructureFormation == 1
		/// ----------------------------------
		///
		/// This case calculates the fraction of HBS-restructured volume of the UO2 fuel matrix based on the KJMA approach.
		/// @ref Barani et al. Journal of Nuclear Materials 539 (2020) 152296
		/// @param[out] avrami_constant
		/// @param[out] transformation_rate
		/// @param[out] resolution_layer_thickness
		/// @param[out] resolution_critical_distance

		reference += ": Barani et al. Journal of Nuclear Materials 539 (2020) 152296";

		double avrami_constant(3.54);
		double transformation_rate(2.77e-7);
		double resolution_layer_thickness = 1.0e-9; //(m)
		double resolution_critical_distance = 1.0e-9; //(m)

		parameter.push_back(avrami_constant);
		parameter.push_back(transformation_rate);
		parameter.push_back(resolution_layer_thickness);
		parameter.push_back(resolution_critical_distance);

		break;
	}

	default:
		ErrorMessages::Switch(__FILE__, "iHighBurnupStructureFormation", int(input_variable[iv["iHighBurnupStructureFormation"]].getValue()));
		break;
	}

	model[model_index].setParameter(parameter);
	model[model_index].setRef(reference);

}
