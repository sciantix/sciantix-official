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

#include "HighBurnupStructurePorosity.h"

void HighBurnupStructurePorosity()
{
	/// @brief
	/// This routine sets the model for High burnup structure porosity evolution

	model.emplace_back();
	int model_index = int(model.size()) - 1;
	model[model_index].setName("High burnup structure porosity");
	double porosity_increment = 0.0;
	
  const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;

	std::string reference;
	std::vector<double> parameter;

	switch (int(input_variable[iv["iHighBurnupStructurePorosity"]].getValue()))
	{
	case 0:
	{
		/// @brief 
		/// No HBS case - no evolution of HBS porosity

		reference += "HBS not considered.";
		parameter.push_back(0.0);
		sciantix_variable[sv["HBS porosity"]].setInitialValue(0.0);
		sciantix_variable[sv["HBS porosity"]].setFinalValue(0.0);
		break;
	}

	case 1:
	{
		/// @brief 
		/// Correlation for the HBS porosity evolution based on Spino et al. 2006 data

		double rate_coefficient = 1.3e-3;
		double porosity_upper_threshold = 0.15;
		double burnup_threshold = 50.0;

		if (sciantix_variable[sv["HBS porosity"]].getInitialValue() < porosity_upper_threshold)
		{
			if (sciantix_variable[sv["Burnup"]].getFinalValue() < burnup_threshold)
				porosity_increment = 0.0;
			else
				porosity_increment = rate_coefficient;
		}

		else
		{
			sciantix_variable[sv["HBS porosity"]].setInitialValue(0.15);
			porosity_increment = 0.0;
		}

		reference = "TRANSURANUS model, based on data from Spino et al. JNM 354 (2006) 66-84";
		parameter.push_back(porosity_increment);

		break;
	}

	case 2:
	{
		/// @brief 
		/// HBS porosity evolution based on Barani et al. - part II (2022)

		break;
	}

	default:
		ErrorMessages::Switch("HighBurnupStructurePorosity.cpp", "HighBurnupStructurePorosity", int(input_variable[iv["HighBurnupStructurePorosity"]].getValue()));
		break;
	}

	model[model_index].setParameter(parameter);
	model[model_index].setRef(reference);

}