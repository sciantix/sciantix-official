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

void Simulation::Burnup()
{
    // Model declaration
    Model model_;
    model_.setName("Burnup");

    double fissionRate = history_variable["Fission rate"].getFinalValue();
    double fuelDensity = sciantix_variable["Fuel density"].getFinalValue(); // kgUO2 / m3
    double specificPower = fissionRate * 3.12e-17 / fuelDensity;

    double burnup = specificPower / 86400.0; // specific power in MW/kgUo2, burnup in MWd/kgUO2
    sciantix_variable["Specific power"].setFinalValue(specificPower);
    
    std::vector<double> parameter;
    parameter.push_back(burnup);

    std::string reference = ": The local burnup is calculated from the fission rate density.";

    model_.setParameter(parameter);
    model_.setRef(reference);
    model.push(model_);

    // Model resolution
    sciantix_variable["Burnup"].setFinalValue(
        solver.Integrator(
            sciantix_variable["Burnup"].getInitialValue(),
            model["Burnup"].getParameter().at(0),
            physics_variable["Time step"].getFinalValue()));

    if (history_variable["Fission rate"].getFinalValue() > 0.0)
        sciantix_variable["Irradiation time"].setFinalValue(
            solver.Integrator(
                sciantix_variable["Irradiation time"].getInitialValue(),
                1.0 / sciantix_variable["Specific power"].getFinalValue(),
                24.0 * sciantix_variable["Burnup"].getIncrement()));
    else
        sciantix_variable["Irradiation time"].setConstant();

    sciantix_variable["FIMA"].setFinalValue(
        solver.Integrator(
            sciantix_variable["FIMA"].getInitialValue(),
            history_variable["Fission rate"].getFinalValue() * 3.6e5 / sciantix_variable["U"].getFinalValue(),
            sciantix_variable["Irradiation time"].getIncrement()));
}
