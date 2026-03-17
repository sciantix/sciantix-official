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
//  Year: 2025                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef SET_VARIABLES_FUNCTIONS_H
#define SET_VARIABLES_FUNCTIONS_H

#include <vector>

#include "SciantixVariable.h"
#include "ThermochemistryManifest.h"
#include "ThermochemistryVariable.h"

std::vector<std::string> getInputVariableNames();

std::vector<SciantixVariable> initializeHistoryVariable(
    double Sciantix_history[],
    double Sciantix_scaling_factors[],
    bool toOutputStoichiometryDeviation,
    bool toOutputThermochimica,
    bool toOutputPrescribedOMRatio
);

std::vector<SciantixVariable> initializeSciantixVariable(
    double Sciantix_variables[],
    bool toOutputRadioactiveFG,
    bool toOutputVenting,
    bool toOutputHelium,
    bool toOutputCracking,
    bool toOutputGrainBoundary,
    bool toOutputHighBurnupStructure,
    bool toOutputStoichiometryDeviation,
    bool toOutputChromiumContent,
    bool toOutputThermochimica
);

std::vector<ThermochemistryVariable> initializeThermochemistryVariable(
    const std::vector<ThermochemistryManifestEntry>& manifest,
    double Sciantix_thermochemistry[]
);

std::vector<std::string> getScalingFactorsNames();

#endif
