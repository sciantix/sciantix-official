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

#include "SetVariables.h"
#include "MainVariables.h"
#include "Simulation.h"

void Simulation::setVariables(int    Sciantix_options[],
                              double Sciantix_history[],
                              double Sciantix_variables[],
                              double Sciantix_scaling_factors[],
                              double Sciantix_diffusion_modes[],
                              double Sciantix_thermochemistry[],
                              const ThermochemistrySettings* Sciantix_thermochemistry_settings
)
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
    // CODE DEVELOPMENT : THERMOCHEMISTRY OUTER-NODE FLAG
    // iThermochimicaOuterNode varies every call (per radial node, set by
    // FisPro3.f95): input_variable entries are otherwise only initialized once
    // for the whole run (see the empty() guard above), so this one needs an
    // explicit refresh on every call instead of relying on the one-time push.
    input_variable["iThermochimicaOuterNode"].setValue(Sciantix_options[26]);

    // toOutput flags
    bool toOutputRadioactiveFG          = input_variable["iRadioactiveFissionGas"].getValue() != 0,
         toOutputVenting                = input_variable["iGrainBoundaryVenting"].getValue() != 0,
         toOutputHelium                 = input_variable["iHelium"].getValue() != 0,
         toOutputCracking               = input_variable["iGrainBoundaryMicroCracking"].getValue() != 0,
         toOutputGrainBoundary          = input_variable["iGrainBoundaryBehaviour"].getValue() == 1,
         toOutputHighBurnupStructure    = input_variable["iHighBurnupStructureFormation"].getValue() == 1,
         toOutputStoichiometryDeviation = input_variable["iStoichiometryDeviation"].getValue() > 0,
         toOutputChromiumContent        = input_variable["iChromiumSolubility"].getValue() > 0,
         toOutputPrescribedOMRatio      = input_variable["iStoichiometryDeviation"].getValue() == 9,
         toOutputThermochimica          = input_variable["iThermochimica"].getValue() != 0,
         toOutputMOX                    = input_variable["iFuelMatrix"].getValue() == 2;

    // Physics variable
    physics_variable.push(SciantixVariable("Time step", "(s)", Sciantix_history[6], Sciantix_history[6], 0));

    // History variable
    std::vector<SciantixVariable> values = initializeHistoryVariable(
        Sciantix_history,
        Sciantix_scaling_factors,
        toOutputStoichiometryDeviation,
        toOutputThermochimica,
        toOutputPrescribedOMRatio
    );
    
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
                                        toOutputChromiumContent,
                                        toOutputThermochimica,
                                        toOutputMOX
                                    );

    for (SciantixVariable initial_value : values)
    {
        sciantix_variable.push(initial_value);
    }

    thermochemistry_settings = Sciantix_thermochemistry_settings;

    std::vector<ThermochemistryVariable> values_th;
    if (toOutputThermochimica)
    {
        // The manifest describes static structure (indices, phases, uom, locations) parsed
        // from a file on disk, unlike Sciantix_thermochemistry which holds the actual evolving
        // state; load it from disk once per run and reuse it on every subsequent call instead
        // of reopening/reparsing the file every timestep.
        if (thermochemistry_manifest.empty())
        {
            thermochemistry_manifest = LoadThermochemistryManifest(TestPath + "input_thermochemistry.txt");
        }
        // Keep the full manifest for output variables so parsed phases/species that are
        // not part of the selected solve inputs can still be stored when OpenCalphad
        // reports them in the equilibrium result.
        values_th = initializeThermochemistryVariable(
                thermochemistry_manifest,
                Sciantix_thermochemistry
        );
    }
    
    for (ThermochemistryVariable initial_value : values_th)
    {
        thermochemistry_variable.push(initial_value);
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

    // Scaling factors
    int index = 0;
    for (std::string name : getScalingFactorsNames())
    {
        scaling_factors.push(InputVariable(name, Sciantix_scaling_factors[index]));
        index++;
    }
}
