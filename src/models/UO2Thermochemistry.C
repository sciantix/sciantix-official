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

#include "Simulation.h"

void Simulation::UO2Thermochemistry()
{
    if (!input_variable["iStoichiometryDeviation"].getValue())
        return;

    // Model declaration
    Model model_;

    model_.setName("UO2 thermochemistry");

    std::string reference;
    reference = " : Blackburn (1973) J. Nucl. Mater., 46, 244-252.";

    std::vector<double> parameter;

    parameter.push_back(sciantix_variable["Stoichiometry deviation"].getInitialValue());
    parameter.push_back(history_variable["Temperature"].getFinalValue());
    parameter.push_back(sciantix_variable["Gap oxygen partial pressure"].getFinalValue());  // (atm)

    model_.setParameter(parameter);
    model_.setRef(reference);

    model.push(model_);

    // Model resolution
    if (history_variable["Temperature"].getFinalValue() < 1000.0 ||
        sciantix_variable["Gap oxygen partial pressure"].getFinalValue() == 0)
        sciantix_variable["Equilibrium stoichiometry deviation"].setFinalValue(0.0);

    else
        sciantix_variable["Equilibrium stoichiometry deviation"].setFinalValue(
            solver.NewtonBlackburn(model["UO2 thermochemistry"].getParameter()));
}