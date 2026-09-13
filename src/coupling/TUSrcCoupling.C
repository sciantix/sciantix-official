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
//  Version: 2.5                                                                    //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E. Cappellari.                                  //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "ErrorMessages.h"
#include "InputReading.h"
#include "MainVariables.h"
#include "Simulation.h"
#include "TUSrcCoupling.h"
#include "ThermochemistrySettings.h"

#include <fstream>

void callSciantix(int                            Sciantix_options[],
                  double                         Sciantix_history[],
                  double                         Sciantix_variables[],
                  double                         Sciantix_scaling_factors[],
                  double                         Sciantix_diffusion_modes[],
                  double                         Sciantix_thermochemistry[],
                  const ThermochemistrySettings* Sciantix_thermochemistry_options)
{
    Simulation* simulation = Simulation::getInstance();

    simulation->initialize(Sciantix_options,
                           Sciantix_history,
                           Sciantix_variables,
                           Sciantix_scaling_factors,
                           Sciantix_diffusion_modes,
                           Sciantix_thermochemistry,
                           Sciantix_thermochemistry_options);
    simulation->execute();

    simulation->update(Sciantix_variables, Sciantix_diffusion_modes, Sciantix_thermochemistry);
}

void getSciantixOptions(int    Sciantix_options[],
                        double Sciantix_scaling_factors[],
                        double /* Sciantix_thermochemistry */[],
                        ThermochemistrySettings** Sciantix_thermochemistry_settings)
{
    std::ofstream input_check("input_check.txt", std::ios::out);

    // Abort execution if any of the input files does not exist
    if (!std::ifstream("input_settings.txt", std::ios::in))
        ErrorMessages::MissingInputFile("input_settings.txt");

    // Read by name; see ParseNamedEntries in InputReading.C.
    const NamedInput settings = ParseNamedEntries("input_settings.txt");

    Sciantix_options[0]  = ReadOneSetting("iGrainGrowth", settings, input_check);
    Sciantix_options[1]  = ReadOneSetting("iFissionProductDiffusivity", settings, input_check);
    Sciantix_options[2]  = ReadOneSetting("iDiffusionSolver", settings, input_check);
    Sciantix_options[3]  = ReadOneSetting("iIntraGranularBubbleBehavior", settings, input_check);
    Sciantix_options[4]  = ReadOneSetting("iResolutionRate", settings, input_check);
    Sciantix_options[5]  = ReadOneSetting("iTrappingRate", settings, input_check);
    Sciantix_options[6]  = ReadOneSetting("iNucleationRate", settings, input_check);
    Sciantix_options[7]  = ReadOneSetting("iOutput", settings, input_check);
    Sciantix_options[8]  = ReadOneSetting("iGrainBoundaryVacancyDiffusivity", settings, input_check);
    Sciantix_options[9]  = ReadOneSetting("iGrainBoundaryBehaviour", settings, input_check);
    Sciantix_options[10] = ReadOneSetting("iGrainBoundaryMicroCracking", settings, input_check);
    Sciantix_options[11] = ReadOneSetting("iFuelMatrix", settings, input_check);
    Sciantix_options[12] = ReadOneSetting("iGrainBoundaryVenting", settings, input_check);
    Sciantix_options[13] = ReadOneSetting("iRadioactiveFissionGas", settings, input_check);
    Sciantix_options[14] = ReadOneSetting("iHelium", settings, input_check);
    Sciantix_options[15] = ReadOneSetting("iHeDiffusivity", settings, input_check);
    Sciantix_options[16] = ReadOneSetting("iGrainBoundarySweeping", settings, input_check);
    Sciantix_options[17] = ReadOneSetting("iHighBurnupStructureFormation", settings, input_check);
    Sciantix_options[18] = ReadOneSetting("iHighBurnupStructurePorosity", settings, input_check);
    Sciantix_options[19] = ReadOneSetting("iHeliumProductionRate", settings, input_check);
    Sciantix_options[20] = ReadOneSetting("iStoichiometryDeviation", settings, input_check);
    Sciantix_options[21] = ReadOneSetting("iBubbleDiffusivity", settings, input_check);
    Sciantix_options[22] = ReadOneSetting("iChromiumSolubility", settings, input_check);
    Sciantix_options[23] = ReadOneSetting("iDensification", settings, input_check);
    Sciantix_options[24] = ReadOneSetting("iReleaseMode", settings, input_check);
    Sciantix_options[25] = ReadOneSetting("iThermochimica", settings, input_check);

    // The time discretisation is TRANSURANUS's own, so a settings file shared with standalone runs
    // may carry Number_of_time_steps_per_interval without it being an error here
    settings.consumed.insert("Number_of_time_steps_per_interval");
    ReportUnrecognisedEntries("input_settings.txt", settings);

    ReadScalingFactors(TestPath + "input_scaling_factors.txt", Sciantix_scaling_factors, input_check);

    input_check.close();

    delete *Sciantix_thermochemistry_settings;
    *Sciantix_thermochemistry_settings = nullptr;
    if (Sciantix_options[25] > 0)
    {
        *Sciantix_thermochemistry_settings =
            new ThermochemistrySettings(LoadThermochemistrySettings(TestPath + "input_thermochemistry_settings.txt"));
    }
}
