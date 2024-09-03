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

#include "SetSystemShell.h"

/// SetSystem

void SetSystemShell()
{
			Xe_in_SiC();
			MapSystemShell();

			Kr_in_SiC();
			MapSystemShell();

			He_in_SiC();
			MapSystemShell();
}

void SystemShell::setFissionGasDiffusivity(int input_value)
{
	/** 
	 * ### setFissionGasDiffusivity
	 * @brief The intra-granular fission gas (xenon and krypton) diffusivity within the fuel grain is set according to the input_variable iFGDiffusionCoefficient
	 * 
	 */

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
		//@brief iFGDiffusionCoefficient = 1 set the fission gas diffusivity according to "???" STILL TO BE REPLACED 

		break;
	}

	default:
		ErrorMessages::Switch(__FILE__, "iFGDiffusionCoefficient", input_value);
		break;
	}
}

void SystemShell::setHeliumDiffusivity(int input_value)
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
		 * @brief set the diffussivity according to "???" STILL OT BE REPLACED */
		
		break;
	}

	default:
		ErrorMessages::Switch(__FILE__, "iHeDiffusivity", input_value);
		break;
	}
}
