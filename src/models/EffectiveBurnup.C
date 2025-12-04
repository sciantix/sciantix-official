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
//  Version: 2.2.1                                                                    //
//  Year: 2025                                                                      //
//  Authors: D. Pizzocri, G. Zullo                                                  //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"

void Simulation::EffectiveBurnup()
{
    // Model declaration
    Model model_;
    model_.setName("Effective burnup");

    std::string         reference;
    std::vector<double> parameter;

    const double temperature_threshold = 1273.15;

    if (history_variable["Temperature"].getFinalValue() <= temperature_threshold ||
        (history_variable["Temperature"].getFinalValue() > temperature_threshold &&
         history_variable["Temperature"].getInitialValue() < temperature_threshold))
        parameter.push_back(sciantix_variable["Specific power"].getFinalValue() / 86400.0);
    else
        parameter.push_back(0.0);

    reference += ": G. Khvostov et al., WRFPM-2005, Kyoto, Japan, 2005.";

    model_.setParameter(parameter);
    model_.setRef(reference);
    model.push(model_);

    // Model resolution
    sciantix_variable["Effective burnup"].setFinalValue(
        solver.Integrator(sciantix_variable["Effective burnup"].getInitialValue(),
                          model["Effective burnup"].getParameter().at(0),
                          physics_variable["Time step"].getFinalValue()));
}
