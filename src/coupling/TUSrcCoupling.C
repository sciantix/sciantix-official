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
//  Version: 2.1                                                                    //
//  Year: 2024                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "TUSrcCoupling.h"
#include "Simulation.h"
#include "Sciantix.h"
#include "InputReading.h"
#include <iostream>

void callSciantix(int Sciantix_options[], double Sciantix_history[], double Sciantix_variables[], double Sciantix_scaling_factors[], double Sciantix_diffusion_modes[])
{
    Simulation* simulation = Simulation::getInstance();

    simulation->initialize(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes); 

    simulation->execute();

	simulation->update(Sciantix_variables, Sciantix_diffusion_modes);
}

void getSciantixOptions(int Sciantix_options[], double Sciantix_scaling_factors[])
{
 	std::ofstream input_check("input_check.txt", std::ios::out);

	  // Abort execution if any of the input files does not exist
	  std::ifstream input_settings("input_settings.txt", std::ios::in);
	  if (!input_settings)
		  ErrorMessages::MissingInputFile("input_settings.txt");

    std::ifstream input_scaling_factors(TestPath + "input_scaling_factors.txt", std::ios::in);

    Sciantix_options[0] = ReadOneSetting("iGrainGrowth", input_settings, input_check);
    Sciantix_options[1] = ReadOneSetting("iFissionGasDiffusivity", input_settings, input_check);
    Sciantix_options[2] = ReadOneSetting("iDiffusionSolver", input_settings, input_check);
    Sciantix_options[3] = ReadOneSetting("iIntraGranularBubbleBehavior", input_settings, input_check);
    Sciantix_options[4] = ReadOneSetting("iResolutionRate", input_settings, input_check);
    Sciantix_options[5] = ReadOneSetting("iTrappingRate", input_settings, input_check);
    Sciantix_options[6] = ReadOneSetting("iNucleationRate", input_settings, input_check);
    Sciantix_options[7] = ReadOneSetting("iOutput", input_settings, input_check);
    Sciantix_options[8] = ReadOneSetting("iGrainBoundaryVacancyDiffusivity", input_settings, input_check);
    Sciantix_options[9] = ReadOneSetting("iGrainBoundaryBehaviour", input_settings, input_check);
    Sciantix_options[10] = ReadOneSetting("iGrainBoundaryMicroCracking", input_settings, input_check);
    Sciantix_options[11] = ReadOneSetting("iFuelMatrix", input_settings, input_check);
    Sciantix_options[12] = ReadOneSetting("iGrainBoundaryVenting", input_settings, input_check);
    Sciantix_options[13] = ReadOneSetting("iRadioactiveFissionGas", input_settings, input_check);
    Sciantix_options[14] = ReadOneSetting("iHelium", input_settings, input_check);
    Sciantix_options[15] = ReadOneSetting("iHeDiffusivity", input_settings, input_check);
    Sciantix_options[16] = ReadOneSetting("iGrainBoundarySweeping", input_settings, input_check);
    Sciantix_options[17] = ReadOneSetting("iHighBurnupStructureFormation", input_settings, input_check);
    Sciantix_options[18] = ReadOneSetting("iHighBurnupStructurePorosity", input_settings, input_check);
    Sciantix_options[19] = ReadOneSetting("iHeliumProductionRate", input_settings, input_check);
    Sciantix_options[20] = ReadOneSetting("iStoichiometryDeviation", input_settings, input_check);
    Sciantix_options[21] = ReadOneSetting("iBubbleDiffusivity",input_settings,input_check);
	Sciantix_options[22] = ReadOneSetting("iChromiumSolubility",input_settings,input_check);
	Sciantix_options[23] = ReadOneSetting("iDensification", input_settings, input_check);
	Sciantix_options[24] = ReadOneSetting("iReleaseMode", input_settings, input_check);

if (!input_scaling_factors.fail())
	{
		Sciantix_scaling_factors[0] = ReadOneParameter("sf_resolution_rate", input_scaling_factors, input_check);
		Sciantix_scaling_factors[1] = ReadOneParameter("sf_trapping_rate", input_scaling_factors, input_check);
		Sciantix_scaling_factors[2] = ReadOneParameter("sf_nucleation_rate", input_scaling_factors, input_check);
		Sciantix_scaling_factors[3] = ReadOneParameter("sf_diffusivity", input_scaling_factors, input_check);
		Sciantix_scaling_factors[4] = ReadOneParameter("sf_diffusivity2", input_scaling_factors, input_check);
		Sciantix_scaling_factors[5] = ReadOneParameter("sf_temperature", input_scaling_factors, input_check);
		Sciantix_scaling_factors[6] = ReadOneParameter("sf_fission_rate", input_scaling_factors, input_check);
		Sciantix_scaling_factors[7] = ReadOneParameter("sf_diffusion_based_release", input_scaling_factors, input_check);
		Sciantix_scaling_factors[8] = ReadOneParameter("sf_helium_production_rate", input_scaling_factors, input_check);
		Sciantix_scaling_factors[9] = ReadOneParameter("sf_dummy", input_scaling_factors, input_check);
	}
	else
	{
		Sciantix_scaling_factors[0] = 1.0;
		Sciantix_scaling_factors[1] = 1.0;
		Sciantix_scaling_factors[2] = 1.0;
		Sciantix_scaling_factors[3] = 1.0;
		Sciantix_scaling_factors[4] = 1.0;
		Sciantix_scaling_factors[5] = 1.0;
		Sciantix_scaling_factors[6] = 1.0;
		Sciantix_scaling_factors[7] = 1.0;
		Sciantix_scaling_factors[8] = 1.0;
		Sciantix_scaling_factors[8] = 1.0;
	}

	input_check.close();
	input_settings.close();
	input_scaling_factors.close();
}