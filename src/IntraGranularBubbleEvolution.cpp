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

#include "IntraGranularBubbleEvolution.h"

void IntraGranularBubbleEvolution()
{
	/// @brief
	/// IntraGranularBubbleEvolution builds an object Model according to the input_variable "iIntraGranularBubbleEvolution".
	/// The models available in this routine determine the calculation of local bubble density and average size.
	model.emplace_back();
	int model_index = int(model.size()) - 1;

	model[model_index].setName("Intragranular bubble evolution");

	std::string reference;
	std::vector<double> parameter;

	switch (int(input_variable[iv["iIntraGranularBubbleEvolution"]].getValue()))
	{
	case 0:
	{
		/// @brief
		/// iIntraGranularBubbleEvolution == 0
		/// ----------------------------------
		///
		/// This case assumes constant trial values for the intragranular bubble number density and radius.
		/// @param[out] intragranular_bubble_concentration
		/// @param[out] intragranular_bubble_radius

		reference += "No evolution.";

		sciantix_variable[sv["Intragranular bubble concentration"]].setInitialValue(7.0e23);
		sciantix_variable[sv["Intragranular bubble radius"]].setInitialValue(1.0e-9);

		sciantix_variable[sv["Intragranular bubble concentration"]].setFinalValue(7.0e23);
		sciantix_variable[sv["Intragranular bubble radius"]].setFinalValue(1.0e-9);

		parameter.push_back(0.);
		parameter.push_back(0.);

		break;
	}

	case 1:
	{
		/// @brief
		/// iIntraGranularBubbleEvolution == 1
		/// ----------------------------------
		///
		/// The evolution of small intra-granular bubbles in fuel grains is controlled by bubble nucleation, gas atom trapping, and irradiation-induced gas atom re-solution back in the lattice.
		/// Description of the model in @ref Pizzocri et al., JNM, 502 (2018) 323-330.
		/// @param[out] intragranular_bubble_concentration
		/// @param[out] intragranular_bubble_radius
		
		reference += "Pizzocri et al., JNM, 502 (2018) 323-330.";

		/// @param[in] resolution_rate
		parameter.push_back(sciantix_system[sy["Xe in UO2"]].getResolutionRate());

		/// @param[in] nucleation_rate
		parameter.push_back(sciantix_system[sy["Xe in UO2"]].getNucleationRate());

		break;
	}

	case 2:
	{
		/// @brief
		/// iIntraGranularBubbleEvolution == 2
		/// ----------------------------------
		///
		/// The evolution of intragranular bubbles is modelled by means of temperature-driven correlations.
		/// Description of the model in @ref White, Tucker, Journal of Nuclear Materials, 118 (1983), 1-38.
		/// @param[in] local_fuel_temperature

		reference += "White and Tucker, JNM, 118 (1983), 1-38.";
		
		sciantix_variable[sv["Intragranular bubble concentration"]].setInitialValue(1.52e+27 / history_variable[hv["Temperature"]].getFinalValue() - 3.3e+23);
		parameter.push_back(0.0);
		parameter.push_back(0.0);
		break;
	}

	case 3:
	{
		/**
		 * @brief iIntraGranularBubbleEvolution == 3
		 * 
		 * The evolution of intragranular bubble concentration, radius and atoms per bubble is described through
		 * the similarity ratio, based on the evolution of intragranular concentration of gas in bubbles.
		 */
    
		reference += "Case specific for annealing experiments and helium intragranular behaviour.";

		if(physics_variable[pv["Time step"]].getFinalValue() > 0.0)
			parameter.push_back((1.0 / sciantix_variable[sv["Intragranular similarity ratio"]].getFinalValue() - 1.0) / physics_variable[pv["Time step"]].getFinalValue());
		else
			parameter.push_back(0.);

		parameter.push_back(0.);

		break;
	}

	case 99:
	{
		/**
		 * @brief iIntraGranularBubbleEvolution == 99
		 * No intragranular bubbles.
		 * 
		 * To be used with iTrappingRate = 99, iResolutionRate = 99, iNucleationRate = 99.
		 * 
		 * @param[out] intragranular_bubble_radius
		 * @param[out] intragranular_bubble_concentration
		 * 
		 */

		reference += "No intragranular bubbles.";

		sciantix_variable[sv["Intragranular bubble concentration"]].setInitialValue(0.0);
		sciantix_variable[sv["Intragranular bubble radius"]].setInitialValue(0.0);
		sciantix_variable[sv["Intragranular atoms per bubble"]].setInitialValue(0.0);

		sciantix_variable[sv["Intragranular bubble concentration"]].setFinalValue(0.0);
		sciantix_variable[sv["Intragranular bubble radius"]].setFinalValue(0.0);
		sciantix_variable[sv["Intragranular atoms per bubble"]].setFinalValue(0.0);

		parameter.push_back(0.);
		parameter.push_back(0.);

		break;
	}

	default:
		ErrorMessages::Switch("IntraGranularBubbleEvolution.cpp", "iIntraGranularBubbleEvolution", int(input_variable[iv["iIntraGranularBubbleEvolution"]].getValue()));
		break;
	}

	model[model_index].setParameter(parameter);
	model[model_index].setRef(reference);

}


