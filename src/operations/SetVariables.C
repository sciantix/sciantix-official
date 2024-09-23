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
//  Year: 2023                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "SetVariables.h"
#include "Simulation.h"

void Simulation::setVariables(
    int Sciantix_options[],
    double Sciantix_history[],
    double Sciantix_variables[],
    double Sciantix_scaling_factors[],
    double Sciantix_diffusion_modes[]
)
{
    // Input variable
    if (input_variable.empty())
    {
        std::vector<std::string> name_list = getInputVariableNames();
        for (int i = 0; i < name_list.size(); i++)
        {
            input_variable.push(InputVariable(name_list[i], Sciantix_options[i]));
        }
    }


    // toOutput flags
    bool toOutputRadioactiveFG = input_variable["iRadioactiveFissionGas"].getValue() != 0,
         toOutputVenting = input_variable["iGrainBoundaryVenting"].getValue() != 0,
         toOutputHelium = input_variable["iHelium"].getValue() != 0,
         toOutputCracking = input_variable["iGrainBoundaryMicroCracking"].getValue() != 0,
         toOutputGrainBoundary = input_variable["iGrainBoundaryBehaviour"].getValue() == 1,
         toOutputHighBurnupStructure = input_variable["iHighBurnupStructureFormation"].getValue() == 1,
         toOutputStoichiometryDeviation = input_variable["iStoichiometryDeviation"].getValue() > 0;


    // Physics variable
    physics_variable.push(SciantixVariable("Time step", "(s)", Sciantix_history[6], Sciantix_history[6], 0));


    // History variable
    std::vector<SciantixVariable> values = initializeHistoryVariable(
        Sciantix_history,
        Sciantix_scaling_factors,
        toOutputStoichiometryDeviation
    );
    for (SciantixVariable initial_value : values)
    {
        history_variable.push(initial_value);
    }

    // Sciantix variable
    values = initializeSciantixVariable(
            Sciantix_variables,
            toOutputRadioactiveFG,
            toOutputVenting,
            toOutputHelium,
            toOutputCracking,
            toOutputGrainBoundary,
            toOutputHighBurnupStructure,
            toOutputStoichiometryDeviation
        );

    for (SciantixVariable initial_value : values)
    {
        sciantix_variable.push(initial_value);
    }

#if defined(COUPLING_TU)

  sciantix_variable["Burnup"]setInitialValue(Sciantix_history["Burnup"]);

#endif

    // Diffusion modes
    for (int i = 0; i < n_modes; ++i)
    {
        for (int j = 0; j <= 17; j++)
        {
            modes_initial_conditions[j * n_modes + i] = Sciantix_diffusion_modes[j * n_modes + i];
        }
    }

    // Scaling factors
    int index = 0;
    for (std::string name : getScalingFactorsNames())
    {
        scaling_factors.push(InputVariable(name, Sciantix_scaling_factors[index]));
        index++;
    }
}
