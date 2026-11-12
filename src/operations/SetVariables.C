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
//  Version: 2.2.1                                                                  //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "SetVariables.h"
#include "Simulation.h"

void Simulation::setVariables(int    Sciantix_options[],
                              double Sciantix_history[],
                              double Sciantix_variables[],
                              double Sciantix_scaling_factors[],
                              double Sciantix_diffusion_modes[],
                              double Sciantix_diffusion_modes_NUS[])
{
    // Input variable
    if (input_variable.empty())
    {
        std::vector<std::string> name_list = getInputVariableNames();
        for (size_t i = 0; i < name_list.size(); i++)
        {
            input_variable.push(InputVariable(name_list[i], Sciantix_options[i]));
        }
    }

    // toOutput flags
    bool toOutputRadioactiveFG          = input_variable["iRadioactiveFissionGas"].getValue() != 0,
         toOutputVenting                = input_variable["iGrainBoundaryVenting"].getValue() != 0,
         toOutputHelium                 = input_variable["iHelium"].getValue() != 0,
         toOutputCracking               = input_variable["iGrainBoundaryMicroCracking"].getValue() != 0,
         toOutputGrainBoundary          = input_variable["iGrainBoundaryBehaviour"].getValue() == 1,
         toOutputHighBurnupStructure    = input_variable["iHighBurnupStructureFormation"].getValue() == 1,
         toOutputStoichiometryDeviation = input_variable["iStoichiometryDeviation"].getValue() > 0,
         toOutputChromiumContent        = input_variable["iChromiumSolubility"].getValue() > 0;

    // Physics variable
    physics_variable.push(SciantixVariable("Time step", "(s)", Sciantix_history[6], Sciantix_history[6], 0));

    // History variable
    std::vector<SciantixVariable> values =
        initializeHistoryVariable(Sciantix_history, Sciantix_scaling_factors, toOutputStoichiometryDeviation);

    for (SciantixVariable initial_value : values)
    {
        history_variable.push(initial_value);
    }

    // Sciantix variable
    values = initializeSciantixVariable(Sciantix_variables,
                                        toOutputRadioactiveFG,
                                        toOutputVenting,
                                        toOutputHelium,
                                        toOutputCracking,
                                        toOutputGrainBoundary,
                                        toOutputHighBurnupStructure,
                                        toOutputStoichiometryDeviation,
                                        toOutputChromiumContent);

    for (SciantixVariable initial_value : values)
    {
        sciantix_variable.push(initial_value);
    }

    // # COARSENING: persistent population, gas partition and bubble properties.
    if (input_variable["iIntraGranularBubbleBehavior"].getValue() == 5)
    {
        sciantix_variable.push(SciantixVariable("Coarsening dislocation concentration", "(bub/m3)", Sciantix_variables[170], Sciantix_variables[170], 1));
        sciantix_variable.push(SciantixVariable("Coarsening vacancy concentration", "(vac/m3)", Sciantix_variables[171], Sciantix_variables[171], 1));
        sciantix_variable.push(SciantixVariable("Xe coarsening partition state", "(at/m3)", Sciantix_variables[172], Sciantix_variables[172], 1));
        sciantix_variable.push(SciantixVariable("Kr coarsening partition state", "(at/m3)", Sciantix_variables[173], Sciantix_variables[173], 1));
        sciantix_variable.push(SciantixVariable("Coarsening initialized", "(/)", Sciantix_variables[174], Sciantix_variables[174], 1));
        sciantix_variable.push(SciantixVariable("Coarsening dislocation radius", "(m)", Sciantix_variables[175], Sciantix_variables[175], 1));
        sciantix_variable.push(SciantixVariable("Coarsening vacancies per bubble", "(vac/bub)", Sciantix_variables[176], Sciantix_variables[176], 1));
        sciantix_variable.push(SciantixVariable("Coarsening pressure", "(Pa)", Sciantix_variables[177], Sciantix_variables[177], 1));
        sciantix_variable.push(SciantixVariable("Coarsening equilibrium pressure", "(Pa)", Sciantix_variables[178], Sciantix_variables[178], 1));
        sciantix_variable.push(SciantixVariable("Coarsening dislocation swelling", "(/)", Sciantix_variables[179], Sciantix_variables[179], 1));
        sciantix_variable.push(SciantixVariable("Coarsening bulk swelling", "(/)", Sciantix_variables[180], Sciantix_variables[180], 1));
        sciantix_variable.push(SciantixVariable("Xe in bulk bubbles", "(at/m3)", Sciantix_variables[181], Sciantix_variables[181], 1));
        sciantix_variable.push(SciantixVariable("Kr in bulk bubbles", "(at/m3)", Sciantix_variables[182], Sciantix_variables[182], 1));
        sciantix_variable.push(SciantixVariable("Coarsening line trapping rate", "(1/s)", Sciantix_variables[183], Sciantix_variables[183], 1));
        sciantix_variable.push(SciantixVariable("Xe in dislocation bubbles", "(at/m3)", 0, 0, 1));
        sciantix_variable.push(SciantixVariable("Kr in dislocation bubbles", "(at/m3)", 0, 0, 1));
    }

#if defined(COUPLING_TU)

    sciantix_variable["Burnup"].setInitialValue(Sciantix_history[7]);
    sciantix_variable["Burnup"].setFinalValue(Sciantix_history[8]);

#endif

    // Diffusion modes
    for (int i = 0; i < n_modes; ++i)
    {
        for (int j = 0; j < N_MODE_BLOCKS; j++)
        {
            modes_initial_conditions[j * n_modes + i] = Sciantix_diffusion_modes[j * n_modes + i];
        }
    }

    // Diffusion modes NUS
    for (int i = 0; i < n_modes; ++i)
    {
        for (int j = 0; j <= 17; j++)
        {
            modes_initial_conditions_NUS[j * n_modes + i] = Sciantix_diffusion_modes_NUS[j * n_modes + i];
        }
    }

    // Scaling factors
    int index = 0;
    for (std::string name : getScalingFactorsNames())
    {
        scaling_factors.push(InputVariable(name, Sciantix_scaling_factors[index]));
        index++;
    }

    // Source profiles seen by the NUS solver: the grain-boundary re-solution model consumes the
    // corrected profiles, everything else the interpolated ones.
    if (input_variable["iGrainBoundaryResolution"].getValue() == 1)
        sourcesinput = sources_correct;
    else
        sourcesinput = sources_interp;
}
