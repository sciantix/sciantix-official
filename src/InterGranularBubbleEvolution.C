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

/// InterGranularBubbleEvolution
/// This function contains a choice among possible
/// expressions for the bubble number density and
/// the bubble radius at grain boundaries.
/// The model considers one-off nucleation,
/// growth of lenticular bubbles by vacancy absorption
/// and coalescence of bubbles.
/// [2] White, JNM, 325 (2004) 61-77

#include "InterGranularBubbleEvolution.h"

void InterGranularBubbleEvolution()
{
	model.emplace_back();
	int model_index = int(model.size()) - 1;

	model[model_index].setName("Intergranular bubble evolution");
	std::string reference;
	std::vector<double> parameter;
	
	const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;
	const double boltzmann_constant = CONSTANT_NUMBERS_H::PhysicsConstants::boltzmann_constant;

	switch (int(input_variable[iv["iGrainBoundaryBehaviour"]].getValue()))
	{
	case 0:
	{
		parameter.push_back(0.0);
		parameter.push_back(0.0);

		reference += ": No model for grain-boundary bubble evolution.";
		break;
	}

	case 1:
	{
		// Gas is distributed among bubbles
		// n(at/bub) = c(at/m3) / (N(bub/m2) S/V(1/m))
		double n_at(0);
		for (auto& system : sciantix_system)
		{
			if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
			{
				sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].setFinalValue(
					sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue() /
					(sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue() * (3.0 / sciantix_variable[sv["Grain radius"]].getFinalValue())));

				n_at += sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].getFinalValue();
			}
		}
		sciantix_variable[sv["Intergranular atoms per bubble"]].setFinalValue(n_at);

		// Calculation of the bubble dimension
		// initial volume
		double vol(0);
		for (auto& system : sciantix_system)
		{
			if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
			{
				vol += sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].getFinalValue() *
					gas[ga[system.getGasName()]].getVanDerWaalsVolume();
			}
		}
		vol += sciantix_variable[sv["Intergranular vacancies per bubble"]].getInitialValue() * matrix[sma["UO2"]].getSchottkyVolume();
		sciantix_variable[sv["Intergranular bubble volume"]].setInitialValue(vol);

		// initial radius
		sciantix_variable[sv["Intergranular bubble radius"]].setInitialValue(
			0.620350491 * pow(sciantix_variable[sv["Intergranular bubble volume"]].getInitialValue() / (matrix[sma["UO2"]].getLenticularShapeFactor()), 1. / 3.));

		// initial area
		sciantix_variable[sv["Intergranular bubble area"]].setInitialValue(
			pi * pow(sciantix_variable[sv["Intergranular bubble radius"]].getInitialValue() * sin(matrix[sma["UO2"]].getSemidihedralAngle()), 2));

		// initial fractional coverage  
		sciantix_variable[sv["Intergranular fractional coverage"]].setInitialValue(
			sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue() *
			sciantix_variable[sv["Intergranular bubble area"]].getInitialValue());

		// approximation of 1/S, S = -1/4 ((1-F)(3-F)+2lnF)
		const double AA = 1830.1;
		const double BB = -1599.2;
		const double CC = 690.91;
		const double DD = -99.993;
		const double EE = 20.594;

		double sink_strength = 0.4054 +
			AA * pow(sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue(), 5) +
			BB * pow(sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue(), 4) +
			CC * pow(sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue(), 3) +
			DD * pow(sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue(), 2) +
			EE * sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue();

		double volume_flow_rate
			= 2.0 * pi * matrix[sma["UO2"]].getGrainBoundaryThickness() * matrix[sma["UO2"]].getGrainBoundaryVacancyDiffusivity() * sink_strength;

		// Initial value of the growth rate = 2 pi t D n / S V
		const double growth_rate = volume_flow_rate * sciantix_variable[sv["Intergranular atoms per bubble"]].getFinalValue() / matrix[sma["UO2"]].getSchottkyVolume();

		double equilibrium_pressure(0), equilibrium_term(0);
		if (sciantix_variable[sv["Intergranular bubble radius"]].getInitialValue())
		{
			equilibrium_pressure = 2.0 * matrix[sma["UO2"]].getSurfaceTension() / sciantix_variable[sv["Intergranular bubble radius"]].getInitialValue() -
				history_variable[hv["Hydrostatic stress"]].getFinalValue() * 1e6;

			equilibrium_term = -volume_flow_rate * equilibrium_pressure /
				(boltzmann_constant * history_variable[hv["Temperature"]].getFinalValue());
		}

		parameter.push_back(growth_rate);
		parameter.push_back(equilibrium_term);

		reference += ": Pastore et al., NED, 256 (2013) 75-86.";

		break;
	}

    default:
        ErrorMessages::Switch(__FILE__, "iGrainBoundaryBehaviour", int(input_variable[iv["iGrainBoundaryBehaviour"]].getValue()));
        break;
    }

	model[model_index].setParameter(parameter);
	model[model_index].setRef(reference);
}
