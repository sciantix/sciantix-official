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
  /**
   * @brief This routine defines the available options for fuel matrices and their properties.
   * 
   */

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
			ErrorMessages::Switch("SetMatrix.cpp", "iFuelMatrix", int(input_variable[iv["iFuelMatrix"]].getValue()));
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

		reference += "no grain-boundary mobility.\n\t";
		grain_boundary_mobility = 0.0;

		break;
	}

	case 1:
	{
		/** 
		 * @brief iGrainGrowth = 1 corresponds to the Ainscough et al. (1973) grain-boundary mobility
		 * 
		*/
		reference += "Ainscough et al., JNM, 49 (1973) 117-128.\n\t";
		grain_boundary_mobility = 1.455e-8 * exp(- 32114.5 / history_variable[hv["Temperature"]].getFinalValue());
		break;
	}

	case 2 :
	{
		/**
		 * @brief iGrainGrowth = 2 corresponds to the @ref Van Uffelen et al. JNM, 434 (2013) 287–29 grain-boundary mobility
		 * 
		*/

		reference += "Van Uffelen et al. JNM, 434 (2013) 287–29.\n\t";
		grain_boundary_mobility = 1.360546875e-15 * exp(- 46524.0 / history_variable[hv["Temperature"]].getFinalValue());
		break;
	}

	default:
		ErrorMessages::Switch("SetMatrix.cpp", "iGrainGrowth", input_value);
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
			 * @brief iGrainBoundaryVacancyDiffusivity = 2 corresponds to the correction from @ref Pastore et al., JNM, 456 (2015) 156.
			 * 
			 */

			grain_boundary_diffusivity = 8.86e-6 * exp(- 5.75e-19 / (boltzmann_constant * history_variable[hv["Temperature"]].getFinalValue()));
			reference += "iGrainBoundaryVacancyDiffusivity: from Pastore et al., JNM, 456 (2015) 156.\n\t";

			break;
		}

		default:
			ErrorMessages::Switch("SetMatrix.cpp", "iGrainBoundaryVacancyDiffusivity", input_value);
			break;
	}
}
