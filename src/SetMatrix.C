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

#include "SetMatrix.h"

void SetMatrix( )
{
	switch (int(input_variable[iv["iFuelMatrix"]].getValue()))
	{
		case 0: 
		{
			UO2();
			MapMatrix();

			break;
		}

		case 1: 
		{
			UO2();
			MapMatrix();
			
			UO2HBS();
			MapMatrix();

			break;
		}
		
		default:
			ErrorMessages::Switch(__FILE__, "iFuelMatrix", int(input_variable[iv["iFuelMatrix"]].getValue()));
			break;
	}
}

void Matrix::setGrainBoundaryMobility(int input_value)
{
	switch (input_value)
	{
	case 0:
	{
		/**
		 * @brief iGrainGrowth = 0 corresponds to a 0 grain-boundary mobility
		 * 
		*/

		reference += ": Null grain-boundary mobility.\n\t";
		grain_boundary_mobility = 0.0;

		break;
	}

	case 1:
	{
		/** 
		 * @brief iGrainGrowth = 1 corresponds to the Ainscough et al. (1973) grain-boundary mobility
		 * 
		*/
		reference += ": Ainscough et al., JNM, 49 (1973) 117-128.\n\t";
		grain_boundary_mobility = 1.455e-8 * exp(- 32114.5 / history_variable[hv["Temperature"]].getFinalValue());
		break;
	}

	case 2 :
	{
		/**
		 * @brief iGrainGrowth = 2 corresponds to the @ref Van Uffelen et al. JNM, 434 (2013) 287–29 grain-boundary mobility
		 * 
		*/

		reference += ": Van Uffelen et al. JNM, 434 (2013) 287-29.\n\t";
		grain_boundary_mobility = 1.360546875e-15 * exp(- 46524.0 / history_variable[hv["Temperature"]].getFinalValue());
		break;
	}

	default:
		ErrorMessages::Switch(__FILE__, "iGrainGrowth", input_value);
		break;
	}
}

void Matrix::setGrainBoundaryVacancyDiffusivity(int input_value)
{
	/** 
	 * ### GrainBoundaryVacancyDiffusivity
	 * @brief The diffusivity of the vacancies on the grain-boundaries is set according to the input_variable iGrainBoundaryVacancyDiffusivity.
	 * 
	 */
	
	const double boltzmann_constant = CONSTANT_NUMBERS_H::PhysicsConstants::boltzmann_constant;

	switch (input_value)
	{
		case 0:
		{
			/**
			 * @brief iGrainBoundaryVacancyDiffusivity = 0 corresponds to a constant diffusivity value, equal to 1e-30 m^2/s.
			 * 
			 */

			grain_boundary_diffusivity = 1e-30;
			reference += "iGrainBoundaryVacancyDiffusivity: constant value (1e-30 m^2/s).\n\t";

			break;
		}

		case 1:
		{
			/**
			 * @brief iGrainBoundaryVacancyDiffusivity = 1 corresponds to the relation from @ref Reynolds and Burton, JNM, 82 (1979) 22-25.
			 * 
			 */

			grain_boundary_diffusivity = 6.9e-04 * exp(- 5.35e-19 / (boltzmann_constant * history_variable[hv["Temperature"]].getFinalValue()));
			reference += "iGrainBoundaryVacancyDiffusivity: from Reynolds and Burton, JNM, 82 (1979) 22-25.\n\t";

			break;
		}

		case 2:
		{
			/**
			 * @brief iGrainBoundaryVacancyDiffusivity = 2 corresponds to the correction from @ref White, JNM, 325 (2004), 61-77
			 * 
			 */

			grain_boundary_diffusivity = 3.5/5 * 8.86e-6 * exp(- 4.17e4 / history_variable[hv["Temperature"]].getFinalValue());
			reference += "iGrainBoundaryVacancyDiffusivity: from White, JNM, 325 (2004), 61-77.\n\t";

			break;
		}

		case 5:
		{
			/**
			 * @brief iGrainBoundaryVacancyDiffusivity = 5 corresponds to the vacancy diffusivities along HBS grain boundaries.
			 * This model is from @ref Barani et al., JNM 563 (2022) 153627.
			 *
			 */

			grain_boundary_diffusivity = (1.3e-7 * exp(-4.52e-19 /
					(boltzmann_constant * history_variable[hv["Temperature"]].getFinalValue()))
			);

			reference += "iGrainBoundaryVacancyDiffusivity: HBS case, from Barani et al., JNM 563 (2022) 153627.\n\t";
			break;
		}

		default:
			ErrorMessages::Switch(__FILE__, "iGrainBoundaryVacancyDiffusivity", input_value);
			break;
	}
}

void Matrix::setPoreNucleationRate()
{
	/**
	 * @brief nucleation rate of HBS pores.
	 * This model is from @ref *Barani et al., JNM 563 (2022) 153627*.
	 *
	 */

	double sf_nucleation_rate_porosity = 1.25e-6; // from dburnup to dtime

	pore_nucleation_rate =
			(5.0e17 * 2.77e-7 * 3.54 * (1.0-sciantix_variable[sv["Restructured volume fraction"]].getFinalValue()) *
		pow(sciantix_variable[sv["Effective burnup"]].getFinalValue(), 2.54));

	pore_nucleation_rate *= sf_nucleation_rate_porosity;
}

void Matrix::setPoreResolutionRate()
{
	/**
	 * @brief re-solution rate of gas atoms from HBS pores.
	 * This model is from @ref *Barani et al., JNM 563 (2022) 153627*.
	 *
	 */

	double correction_coefficient = (1.0 - exp(pow(-sciantix_variable[sv["HBS pore radius"]].getFinalValue() / (9.0e-9), 3)));
	double b0(2.0e-23 * history_variable[hv["Fission rate"]].getFinalValue());

	pore_resolution_rate =
		b0 * correction_coefficient *
		(3.0 * 1.0e-9 / (3.0 * 1.0e-9 + sciantix_variable[sv["HBS pore radius"]].getFinalValue())) *
		(1.0e-9 / (1.0e-9 + sciantix_variable[sv["HBS pore radius"]].getFinalValue()));
}
 
void Matrix::setPoreTrappingRate()
{
	/**
	 * @brief trapping rate of gas atoms in HBS pores.
	 * This model is from @ref *Barani et al., JNM 563 (2022) 153627*.
	 *
	 */

	const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;

	pore_trapping_rate = 4.0 * pi * matrix[sma["UO2HBS"]].getGrainBoundaryVacancyDiffusivity() *
	sciantix_variable[sv["Xe at grain boundary"]].getFinalValue() *
	sciantix_variable[sv["HBS pore radius"]].getFinalValue() *
	(1.0 + 1.8 * pow(sciantix_variable[sv["HBS porosity"]].getFinalValue(), 1.3));
}
