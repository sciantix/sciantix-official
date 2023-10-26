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

#include "SetSystem.h"

/// SetSystem

void SetSystem()
{
	switch (int(input_variable[iv["iFuelMatrix"]].getValue()))
	{
		case 0: 
		{
			Xe_in_UO2();
			MapSystem();

			Kr_in_UO2();
			MapSystem();

			He_in_UO2();
			MapSystem();

			Xe133_in_UO2();
			MapSystem();

			Kr85m_in_UO2();
			MapSystem();
						
			break;
		}

		case 1: 
		{
			Xe_in_UO2HBS();
			MapSystem();

			break;
		}
		
		default:
			break;
	}
}

void System::setBubbleDiffusivity(int input_value)
{
	const double boltzmann_constant = 8.6173e-5; // eV
	const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;

	switch(input_value)
	{
		case 0:
		{
			bubble_diffusivity = 0;
			break;
		}

		case 1:
		{	
			if(sciantix_variable[sv["Intragranular bubble radius"]].getInitialValue() == 0)
				bubble_diffusivity = 0;

			else
			{
				// Assuming that the bubble motion during irradiation is controlled by the volume diffusion mechanism,
				// the bubble diffusivty takes the form Db = (V_atom_in_lattice/bubble_volume) * volume_self_diffusivity
				// @ref Evans, JNM 210 (1994) 21-29
				// @ref Van Uffelen et al. NET 43-6 (2011)

				double volume_self_diffusivity = 3.0e-5*exp(-4.5/(boltzmann_constant*history_variable[hv["Temperature"]].getFinalValue()));
				double bubble_radius = sciantix_variable[sv["Intragranular bubble radius"]].getInitialValue();

				bubble_diffusivity = 3 * matrix[0].getSchottkyVolume() * volume_self_diffusivity / (4.0 * pi * pow(bubble_radius,3.0));
			}
			
			break;
		}

		default:
			ErrorMessages::Switch("SetSystem.cpp", "iBubbleDiffusivity", input_value);
			break;
	}

}

void System::setFissionGasDiffusivity(int input_value)
{
	/** 
	 * ### setFissionGasDiffusivity
	 * @brief The intra-granular fission gas (xenon and krypton) diffusivity within the fuel grain is set according to the input_variable iFGDiffusionCoefficient
	 * 
	 */
	const double boltzmann_constant = CONSTANT_NUMBERS_H::PhysicsConstants::boltzmann_constant;

	switch (input_value)
	{
	case 0:
	{
		/**
		 * @brief iFGDiffusionCoefficient = 0 corresponds to a constant intra-granular diffusivity value, equal to 7e-19 m^2/s.
		 * 
		 */
		
		reference += "iFGDiffusionCoefficient: constant diffusivity.\n\t";
		diffusivity = 7e-19;
		diffusivity *= sf_diffusivity;

		break;
	}

	case 1:
	{
		/**
		 * @brief iFGDiffusionCoefficient = 1 set the fission gas (xenon and krypton) single-atom intragranular diffusivity equal to the expression 
		 * in @ref *Turnbull et al (1988), IWGFPT-32, Preston, UK, Sep 18-22*.
		 * 
		 */

		reference += "iFGDiffusionCoefficient: Turnbull et al (1988), IWGFPT-32, Preston, UK, Sep 18-22.\n\t";

		double temperature = history_variable[hv["Temperature"]].getFinalValue();
		double fission_rate = history_variable[hv["Fission rate"]].getFinalValue();

		double d1 = 7.6e-10 * exp(-4.86e-19 / (boltzmann_constant * temperature));
		double d2 = 4.0 * 1.41e-25 * sqrt(fission_rate) * exp(-1.91e-19 / (boltzmann_constant * temperature));
		double d3 = 8.0e-40 * fission_rate;

		diffusivity = d1 + d2 + d3;
		diffusivity *= sf_diffusivity;

		break;
	}

	case 2:
	{
		/**
		 * @brief iFGDiffusionCoefficient = 2 set the xenon effective intragranular diffusivity equal to the expression 
		 * in @ref *Matzke (1980), Radiation Effects, 53, 219-242*.
		 * 
		 */

		reference += "iFGDiffusionCoefficient: Matzke (1980), Radiation Effects, 53, 219-242.\n\t";
		diffusivity = 5.0e-08 * exp(-40262.0 / history_variable[hv["Temperature"]].getFinalValue());
		diffusivity *= sf_diffusivity;

		break;
	}

	case 3:
	{
		/**
		 * @brief iFGDiffusionCoefficient = 3 set the xenon single-atom intragranular diffusivity equal to the expression 
		 * in @ref *Turnbull et al., (2010), Background and Derivation of ANS-5.4 Standard Fission Product Release Model*.
		 * 
		 */

		reference += "iFGDiffusionCoefficient: Turnbull et al., (2010), Background and Derivation of ANS-5.4 Standard Fission Product Release Model.\n\t";

		double temperature = history_variable[hv["Temperature"]].getFinalValue();
		double fission_rate = history_variable[hv["Fission rate"]].getFinalValue();

		double d1 = 7.6e-11 * exp(-4.86e-19 / (boltzmann_constant * temperature));
		double d2 = 1.41e-25 * sqrt(fission_rate) * exp(-1.91e-19 / (boltzmann_constant * temperature));
		double d3 = 2.0e-40 * fission_rate;

		diffusivity = d1 + d2 + d3;
		diffusivity *= sf_diffusivity;

		break;
	}

	case 4:
	{
		/**
		 * @brief iFGDiffusionCoefficient = 4 set the xenon single-atom intragranular diffusivity equal to the expression 
		 * in @ref *iFGDiffusionCoefficient: Ronchi, C. High Temp 45, 552-571 (2007)*.
		 * 
		 */

		reference += "iFGDiffusionCoefficient: Ronchi, C. High Temp 45, 552-571 (2007).\n\t";

		double temperature = history_variable[hv["Temperature"]].getFinalValue();
		double fission_rate = history_variable[hv["Fission rate"]].getFinalValue();

		double d1 = 7.6e-10 * exp(-4.86e-19 / (boltzmann_constant * temperature));
		double d2 = 6.64e-25 * sqrt(fission_rate) * exp(-1.91e-19 / (boltzmann_constant * temperature));
		double d3 = 1.2e-39 * fission_rate;

		diffusivity = d1 + d2 + d3;
		diffusivity *= sf_diffusivity;

		break;
	}

	case 5:
	{
		/**
		 * @brief this case is for the UO2HBS. value from @ref Barani et al. Journal of Nuclear Materials 539 (2020) 152296
		 * 
		 */

		diffusivity = 4.5e-42 * history_variable[hv["Fission rate"]].getFinalValue();
		diffusivity *= sf_diffusivity;
		
		reference += "inert fission gases in UO2-HBS.\n\t";
		break;
	}

	case 6:
	{
		/**
		 * @brief this case is for 
		 * 
		 */
		double x = sciantix_variable[sv["Stoichiometry deviation"]].getFinalValue();
		double temperature = history_variable[hv["Temperature"]].getFinalValue();
		double fission_rate = history_variable[hv["Fission rate"]].getFinalValue();

		double d1 = 7.6e-10 * exp(-4.86e-19 / (boltzmann_constant * temperature));
		double d2 = 4.0 * 1.41e-25 * sqrt(fission_rate) * exp(-1.91e-19 / (boltzmann_constant * temperature));
		double d3 = 8.0e-40 * fission_rate;

		double S = exp(-74100/temperature);
		double G = exp(-35800/temperature);
		double uranium_vacancies = 0.0;

		uranium_vacancies = S/pow(G,2.0) * (0.5*pow(x,2.0) + G + 0.5*pow((pow(x,4.0) + 4*G*pow(x,2.0)),0.5));

		double d4 = pow(3e-10,2)*1e13*exp(-27800/temperature)*uranium_vacancies;

		diffusivity = d1 + d2 + d3 + d4;

		diffusivity *= sf_diffusivity;

		break;		
	}


	case 99:
	{
		/**
		 * @brief iFGDiffusionCoefficient = 99 set the xenon single-atom intragranular diffusivity to zero.
		 * 
		 */

		reference += "iFGDiffusionCoefficient: Test case: zero diffusion coefficient.\n\t";
		diffusivity = 0.0;

		break;
	}

	default:
		ErrorMessages::Switch("SetSystem.cpp", "iFGDiffusionCoefficient", input_value);
		break;
	}
}

void System::setHeliumDiffusivity(int input_value)
{

	/** 
	 * ### setHeliumDiffusivity
	 * @brief The intra-granular helium diffusivity within the fuel grain is set according to the input_variable iHeDiffusivity
	 * 
	 */
	switch (input_value)
	{
	case 0:
	{
		/**
		 * @brief iHeDiffusivity = 0 corresponds to a constant intra-granular diffusivity value
		 * 
		 */
		
		reference += "iHeDiffusivity: constant intragranular diffusivity.\n\t";
		diffusivity = 7e-19;
		break;
	}

	case 1:
	{
		/**
		 * @brief iHeDiffusivity = 1 is the best-estimate correlation, from data available in literature, for samples with no or very limited lattice damage.
		 * This correlation is also recommended for simulations of helium in UO<sub>2</sub> samples in which **infusion** technique has been adopted.
		 * The correlation is from @ref *L. Luzzi et al., Nuclear Engineering and Design, 330 (2018) 265-271*.
		 * 
		 */

		reference += "(no or very limited lattice damage) L. Luzzi et al., Nuclear Engineering and Design, 330 (2018) 265-271.\n\t";
		diffusivity = 2.0e-10 * exp(-24603.4 / history_variable[hv["Temperature"]].getFinalValue());
		break;
	}

	case 2:
	{
		/**
		 * @brief iHeDiffusivity = 2 is the best-estimate correlation, from data available in literature, for samples with significant lattice damage.
		 * This correlation is also recommended for simulations of helium in UO<sub>2</sub> samples in which **implantation** technique has been adopted.
		 * The correlation is from @ref *L. Luzzi et al., Nuclear Engineering and Design, 330 (2018) 265-271*.
		 * 
		 */

		reference += "(significant lattice damage) L. Luzzi et al., Nuclear Engineering and Design, 330 (2018) 265-271.\n\t";
		diffusivity = 3.3e-10 * exp(-19032.8 / history_variable[hv["Temperature"]].getFinalValue());
		break;
	}


	case 3:
	{
		/**
		 * @brief iHeDiffusivity = 2 sets the single gas-atom intra-granular diffusivity equal to the correlation reported in @ref *Z. Talip et al. JNM 445 (2014) 117�127*.
		 * 
		 */

		reference += "iHeDiffusivity: Z. Talip et al. JNM 445 (2014) 117-127.\n\t";
		diffusivity = 1.0e-7 * exp(-30057.9 / history_variable[hv["Temperature"]].getFinalValue());
		break;
	}

	case 99:
	{
		/**
		 * @brief iHeDiffusivity = 4 corresponds to a null intra-granular diffusivity value
		 * 
		 */
		
		reference += "iHeDiffusivity: null intragranular diffusivity.\n\t";
		diffusivity = 0.0;
		break;
	}

	default:
		ErrorMessages::Switch("SetSystem.cpp", "iHeDiffusivity", input_value);
		break;
	}
}

void System::setResolutionRate(int input_value)
{
	/** 
	 * ### setResolutionRate
	 * @brief The helium intra-granular resolution rate is set according to the input_variable iResolutionRate.
	 * 
	 */

	const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;
	const double boltzmann_constant = CONSTANT_NUMBERS_H::PhysicsConstants::boltzmann_constant;

	switch (input_value)
	{
	case 0:
	{
		/**
		 * @brief iResolutionRate = 0 corresponds to a constant intra-granular resolution rate, equal to 0.0001 1/s.
		 * This value is from @ref *Olander, Wongsawaeng, JNM, 354 (2006), 94-109*.
		 * 
		 */

		reference += "iResolutionRate: Constant resolution rate from Olander, Wongsawaeng, JNM, 354 (2006), 94-109.\n\t";
		resolution_rate = 1.0e-4;
		resolution_rate *= sf_resolution_rate;
		break;
	}

	case 1:
	{
		/**
		 * @brief iResolutionRate = 1 corresponds to the irradiation-induced intra-granular resolution rate from *J.A. Turnbull, JNM, 38 (1971), 203*.
		 * 
		 */

		reference += "iResolutionRate: J.A. Turnbull, JNM, 38 (1971), 203.\n\t";
		resolution_rate = 2.0 * pi * matrix[0].getFFrange() * pow(matrix[0].getFFinfluenceRadius()
			+ sciantix_variable[sv["Intragranular bubble radius"]].getFinalValue(), 2) * history_variable[hv["Fission rate"]].getFinalValue();
		resolution_rate *= sf_resolution_rate;

		break;
	}

	case 2:
	{
		/**
		 * @brief iResolutionRate = 2 corresponds to the irradiation-induced intra-granular resolution rate from *P. Losonen, JNM 304 (2002) 29�49*.
		 * 
		 */

		reference += "iResolutionRate: P. Losonen, JNM 304 (2002) 29�49.\n\t";
		resolution_rate = 3.0e-23 * history_variable[hv["Fission rate"]].getFinalValue();
		resolution_rate *= sf_resolution_rate;

		break;
	}

	case 3:
	{
		/**
		 * @brief iResolutionRate = 3 corresponds to the intra-granular resolution rate from *Cognini et al. NET 53 (2021) 562-571*.
		 * 
		 * iResolutionRate = 3 includes the helium solubility in the resolution rate, with a thermal resolution term.
		 * 
		 */

		reference += "iResolutionRate: Cognini et al. NET 53 (2021) 562-571.\n\t";

		/// @param irradiation_resolution_rate
		double irradiation_resolution_rate = 2.0 * pi * matrix[0].getFFrange() * pow(matrix[0].getFFinfluenceRadius()
			+ sciantix_variable[sv["Intragranular bubble radius"]].getFinalValue(), 2) * history_variable[hv["Fission rate"]].getFinalValue();

		/// @param compressibility_factor
		double helium_hard_sphere_diameter = 2.973e-10 * (0.8414 - 0.05 * log(history_variable[hv["Temperature"]].getFinalValue() / 10.985)); // (m)
		double helium_volume_in_bubble = matrix[0].getOIS(); // 7.8e-30, approximation of saturated nanobubbles
		double y = pi * pow(helium_hard_sphere_diameter, 3) / (6.0 * helium_volume_in_bubble);
		double compressibility_factor = (1.0 + y + pow(y, 2) - pow(y, 3)) / (pow(1.0 - y, 3));
		
		/// @param thermal_resolution_rate
		// thermal_resolution_rate = 3 D k_H k_B T Z / R_b^2
		double thermal_resolution_rate;
		if (sciantix_variable[sv["Intragranular bubble radius"]].getFinalValue() > 0.0)
		{
			thermal_resolution_rate = 3.0 * diffusivity * henry_constant * boltzmann_constant * history_variable[hv["Temperature"]].getFinalValue() * compressibility_factor / pow(sciantix_variable[sv["Intragranular bubble radius"]].getFinalValue(), 2);
			if (sciantix_variable[sv["Intragranular bubble radius"]].getFinalValue() < (2.0 * radius_in_lattice))
				thermal_resolution_rate = 3 * diffusivity * henry_constant * boltzmann_constant * history_variable[hv["Temperature"]].getFinalValue() * compressibility_factor / pow(sciantix_variable[sv["Intragranular bubble radius"]].getFinalValue(), 2)
					- 2.0 * 3.0 * diffusivity * henry_constant * boltzmann_constant * history_variable[hv["Temperature"]].getFinalValue() * compressibility_factor * (sciantix_variable[sv["Intragranular bubble radius"]].getFinalValue() - radius_in_lattice) / pow(radius_in_lattice, 3)
					+ 3.0 * 3.0 * diffusivity * henry_constant * boltzmann_constant * history_variable[hv["Temperature"]].getFinalValue() * compressibility_factor * pow(sciantix_variable[sv["Intragranular bubble radius"]].getFinalValue() - radius_in_lattice, 2) / pow(radius_in_lattice, 4);
		}
		else
			thermal_resolution_rate = 0.0;

		resolution_rate = irradiation_resolution_rate + thermal_resolution_rate;
		resolution_rate *= sf_resolution_rate;

		break;
	}

	case 99:
	{
		/**
		 * @brief iResolutionRate = 99 corresponds to a null intra-granular resolution rate.
		 * 
		 */

		reference += "iResolutionRate: Null resolution rate.\n\t";
		resolution_rate = 0.0;
		break;
	}

	default:
		ErrorMessages::Switch("SetSystem.cpp", "iResolutionRate", input_value);
		break;
	}
	resolution_rate *= sf_resolution_rate;
}

void System::setTrappingRate(int input_value)
{
	/** 
	 * ### setTrappingRate
	 * @brief The krypton intra-granular trapping rate is set according to the input_variable iTrappingRate.
	 * 
	 */
	const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;

	switch (input_value)
	{
	case 0:
	{
		/**
		 * @brief iTrappingRate = 0 corresponds to a constant intra-granular trapping rate, equal to 9.35e-6 1/s.
		 * This value is from @ref *Olander, Wongsawaeng, JNM, 354 (2006), 94-109*.
		 * 
		 */

		reference += "iTrappingRate: constant value from Olander, Wongsawaeng, JNM, 354 (2006), 94-109.\n\t";
		trapping_rate = 9.35e-6;
		trapping_rate *= sf_trapping_rate;

		break;
	}

	case 1:
	{
		/**
		 * @brief iTrappingRate = 1 corresponds to the intra-granular trapping rate 
		 * from @ref *F.S. Ham, Journal of Physics and Chemistry of Solids, 6 (1958) 335-351*.
		 * 
		 * This formula is based on the assumptions that the trapping centre density is dilute enough.
		 * g = 4 pi D_s R_b N_b
		 * 
		 */

		reference += "iTrappingRate: F.S. Ham, Journal of Physics and Chemistry of Solids, 6 (1958) 335-351.\n\t";

		if (sciantix_variable[sv["Intragranular bubble concentration"]].getFinalValue() == 0.0)
			trapping_rate = 0.0;

		else
			trapping_rate = 4.0 * pi * diffusivity *
			(sciantix_variable[sv["Intragranular bubble radius"]].getFinalValue() + radius_in_lattice) *
			sciantix_variable[sv["Intragranular bubble concentration"]].getFinalValue();

		trapping_rate *= sf_trapping_rate;


		break;
	}

	case 99:
	{
		/**
		 * @brief iTrappingRate = 99 stands for the case with zero trapping rate.
		 * 
		 */
		reference += "iTrappingRate: case with zero trapping rate.\n\t";

		trapping_rate = 0.0;
		break;
	}

	default:
		ErrorMessages::Switch("SetSystem.cpp", "iTrappingRate", input_value);
		break;
	}
}

void System::setNucleationRate(int input_value)
{
	/** 
	 * ### setNucleationRate
	 * @brief Evaluation of the nucleation rate of intragranular gas bubble inside the UO<sub>2</sub> matrix
	 * 
	 */
	/// @param nucleation_rate
	switch (input_value)
	{
	case 0:
	{
		/**
		 * @brief iNucleationRate = 0 correspond to the case with constant nucleation rate.
		 * 
		 */

		reference += "iNucleationRate: constant value.\n\t";
		nucleation_rate = 4e20;
		nucleation_rate *= sf_nucleation_rate;

		break;
	}

	case 1:
	{
		/**
		 * @brief iNucleationRate = 1 correspond to expression for intragranular bubble nucleation rate from
		 * @ref Olander, Wongsawaeng, JNM, 354 (2006), 94-109.
		 * 
		 */

		reference += "iNucleationRate: Olander, Wongsawaeng, JNM, 354 (2006), 94-109.\n\t";
		nucleation_rate = 2.0 * history_variable[hv["Fission rate"]].getFinalValue() * 25;
		nucleation_rate *= sf_nucleation_rate;

		break;
	}

	case 99:
	{
		/**
		 * @brief iNucleationRate = 99 correspond to case with zero nucleation rate.
		 */

		reference += "iNucleationRate: zero nucleation rate.\n\t";
		nucleation_rate = 0.0;

		break;
	}

	default:
		ErrorMessages::Switch("SetSystem.cpp", "inucleation_rate", input_value);
		break;
	}
}

void System::setProductionRate(int input_value)
{
	/** 
	 * ### setProductionRate
	 * 
	 */
	switch (input_value)
	{
	case 0:
	{
		reference += "No production rate.\n\t";
		production_rate = 0.0;
		break;
	}
	case 1:
	{
		/**
		 * @brief Production rate = cumulative yield * fission rate density
		 * 
		 */
		
		reference += "Production rate = cumulative yield * fission rate density.\n\t";
		production_rate = yield * history_variable[hv["Fission rate"]].getFinalValue(); // (at/m3s)
		break;
	}

	case 2:
	{
		/**
		 * @brief Surrogate model derived from **helium production in fast reactor conditions**.
		 * The helium production rate is fitted with a function linearly dependent on the local burnup.
	 	 * The default fit is from @ref *A. Cechet et al., Nuclear Engineering and Technology, 53 (2021) 1893-1908*.
		 * 
	   * **Default range of utilization of the default fit**
	   * - Fast reactor conditions: (U,Pu)O<sub>2</sub> MOX fuel in SFR conditions
	   * - Up to 200 GWd/tHM
	   * - Pu/HM concentration of 20-40%
		 * 
		 * The default fit (hence the helium production rate) can be calibrated by using the dedicated
		 * scaling factor (to be set in input_scaling_factors.txt).
		 * 
		 */

		reference += "Case for helium production rate: Cechet et al., Nuclear Engineering and Technology, 53 (2021) 1893-1908.\n\t";
		
		// specific power = dburnup
		sciantix_variable[sv["Specific power"]].setFinalValue((history_variable[hv["Fission rate"]].getFinalValue() * (3.12e-17) / sciantix_variable[sv["Fuel density"]].getFinalValue()));

		// production rate in dproduced / dburnup -> dproduced / time
		production_rate = 2.0e+21 * sciantix_variable[sv["Burnup"]].getFinalValue() + 3.0e+23; // (at/m3 burnup)
		if(physics_variable[pv["Time step"]].getFinalValue())
			production_rate *= sciantix_variable[sv["Specific power"]].getFinalValue() / 86400.0 / physics_variable[pv["Time step"]].getFinalValue();  // (at/m3s)
		else
			production_rate = 0.0;

		production_rate *= sf_helium_production_rate;

		break;
	}

	case 3:
	{
		/**
		 * @brief Constant production rate
		 * 
		 */

		reference += "Constant production rate.\n\t";
		production_rate = 1e18;

		break;
	}

	default:
		ErrorMessages::Switch("SetSystem.cpp", "iHeliumProductionRate", input_value);
		break;
	}

}