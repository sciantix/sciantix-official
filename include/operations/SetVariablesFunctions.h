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

#include <vector>
#include "SciantixVariable.h"

std::vector<std::string> getInputVariableNames();


std::vector<SciantixVariable> initializeHistoryVariable(
    double Sciantix_history[],
    double Sciantix_scaling_factors[],
    bool toOutput
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
    bool toOutputChromiumContent
);

std::vector<std::string> getScalingFactorsNames();

