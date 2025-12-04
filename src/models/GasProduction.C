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
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"

void Simulation::GasProduction()
{
    // Model declaration
    for (auto& system : sciantix_system)
    {
        Model model_;
        model_.setName("Gas production - " + system.getName());
        model_.setRef(" ");

        double productionRate = system.getProductionRate();
        double timeStep       = physics_variable["Time step"].getFinalValue();

        std::vector<double> parameter;
        parameter.push_back(productionRate);
        parameter.push_back(timeStep);
        model_.setParameter(parameter);

        parameter.clear();
        model.push(model_);

        // Model resolution
        if (system.getRestructuredMatrix() == 0)
            sciantix_variable[system.getGasName() + " produced"].setFinalValue(solver.Integrator(
                sciantix_variable[system.getGasName() + " produced"].getInitialValue(),
                model["Gas production - " + system.getName()].getParameter().at(0),
                model["Gas production - " + system.getName()].getParameter().at(1)));
        else if (system.getRestructuredMatrix() == 1)
            sciantix_variable[system.getGasName() + " produced in HBS"].setFinalValue(
                solver.Integrator(
                    sciantix_variable[system.getGasName() + " produced in HBS"].getInitialValue(),
                    model["Gas production - " + system.getName()].getParameter().at(0),
                    model["Gas production - " + system.getName()].getParameter().at(1)));
    }
}