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

#include "GrainGrowth.h"

void GrainGrowth()
{
	/**
	 * @brief This routine defines the model for the grain growth phenomenon.
	 * The choice of the model depends on the input_value iGrainGrowth.
	 * 
	 */

	model.emplace_back();

	int model_index = int(model.size()) - 1;

	model[model_index].setName("Grain growth");
	std::string reference;
	std::vector<double> parameter;

	switch (int(input_variable[iv["iGrainGrowth"]].getValue()))
	{
	case 0:
	{
		/**
		 * @brief iGrainGrowth = 0 is used to neglect the grain growth.
		 * The radius of the grain is constant throughout the simulation.
		 * 
		*/

		reference += "constant grain radius.";

		parameter.push_back(sciantix_variable[sv["Grain radius"]].getInitialValue());
		parameter.push_back(0.0);
		parameter.push_back(0.0);
		parameter.push_back(0.0);
		parameter.push_back(1.0);
		parameter.push_back(-sciantix_variable[sv["Grain radius"]].getInitialValue());

		break;
	}

	case 1:
		/** @brief iGrainGrowth = 1 considers that the grain growth kinetic is described by the semi-empirical model from
		 * @ref *Ainscough et al., JNM, 49 (1973) 117-128*.
		 * This model includes essentially two contributions on the grain growth:
		 * 1. Temperature;
		 * 2. Burnup (i.e., increasing retarding effect due fission product accumulation).
		 * 
		 * Note that, the equation for grain growth is written in grain size.
		*/
	{
		reference += "Ainscough et al., JNM, 49 (1973) 117-128.";

		double limiting_grain_radius = 2.23e-03 * (1.56/2.0) * exp(-7620.0 / history_variable[hv["Temperature"]].getFinalValue());
		double burnup_factor = 1.0 + 2.0 * sciantix_variable[sv["Burnup"]].getFinalValue() / 0.8815;

		if (sciantix_variable[sv["Grain radius"]].getInitialValue() < limiting_grain_radius / burnup_factor)
		{
			double rate_constant = matrix[0].getGrainBoundaryMobility();
			rate_constant *= (1.0 - burnup_factor / (limiting_grain_radius / (sciantix_variable[sv["Grain radius"]].getFinalValue())));

			parameter.push_back(sciantix_variable[sv["Grain radius"]].getInitialValue());
			parameter.push_back(0.0);
			parameter.push_back(0.0);
			parameter.push_back(1.0);
			parameter.push_back(- sciantix_variable[sv["Grain radius"]].getInitialValue());
			parameter.push_back(- rate_constant * physics_variable[pv["Time step"]].getFinalValue());

		}

		else
		{
			parameter.push_back(sciantix_variable[sv["Grain radius"]].getInitialValue());
			parameter.push_back(0.0);
			parameter.push_back(0.0);
			parameter.push_back(0.0);
			parameter.push_back(1.0);
			parameter.push_back(- sciantix_variable[sv["Grain radius"]].getInitialValue());
		}
		break;
	}

	case 2 :
	{
		/**
		 * @brief The grain growth kinetic is described according to @ref Van Uffelen et al. JNM, 434 (2013) 287–29
		 * by means of the following equations:
		 * 1) dD/dt = k/4D^3   if D < Dm
		 * 2) dD/dt = 0        if D > Dm
		 * where
		 * D = grain diameter (um)
		 * k, the rate constant, is
		 * k =  3.1347e+14 * exp(-46524 / T)  (um^4/h)
		 * T = temperature (K)
		 * Dm = limiting grain diameter
		*/

		double limiting_grain_radius = 3.345e-3 / 2.0 * exp(-7620.0 / history_variable[hv["Temperature"]].getFinalValue()); // (m)

		reference += "Van Uffelen et al. JNM, 434 (2013) 287–29.";

		if(sciantix_variable[sv["Grain radius"]].getInitialValue() < limiting_grain_radius)
		{
			double rate_constant = matrix[0].getGrainBoundaryMobility();

			parameter.push_back(sciantix_variable[sv["Grain radius"]].getInitialValue());
			parameter.push_back(1.0);
			parameter.push_back(- sciantix_variable[sv["Grain radius"]].getInitialValue());
			parameter.push_back(0.0);
			parameter.push_back(0.0);
			parameter.push_back(- rate_constant * physics_variable[pv["Time step"]].getFinalValue());
		}
		else
		{
			parameter.push_back(sciantix_variable[sv["Grain radius"]].getInitialValue());
			parameter.push_back(0.0);
			parameter.push_back(0.0);
			parameter.push_back(0.0);
			parameter.push_back(1.0);
			parameter.push_back(- sciantix_variable[sv["Grain radius"]].getInitialValue());
		}
		break;
	}

	default:
		ErrorMessages::Switch("GrainGrowth.cpp", "iGrainGrowth", int(input_variable[iv["iGrainGrowth"]].getValue()));
		break;
	}
	model[model_index].setParameter(parameter);
	model[model_index].setRef(reference);
}
