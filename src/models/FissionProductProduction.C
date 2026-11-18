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
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"
#include "SourceHandler.h"

void Simulation::FissionProductProduction()
{
    // Model declaration
    for (auto& system : sciantix_system)
    {
        Model model_;
        model_.setName("Fission product production - " + system.getName());
        model_.setRef(" ");

        double productionRate = system.getProductionRate();
        // Volume average of the radial production profile, used by the NUS solver (option 4).
        Source productionRateNUS = system.getProductionRateNUS();
        double SVA = Source_Volume_Average(sciantix_variable["Grain radius"].getFinalValue(), productionRateNUS);

        double timeStep = physics_variable["Time step"].getFinalValue();

        std::vector<double> parameter;

        if (int(input_variable["iDiffusionSolver"].getValue()) == 4)
            parameter.push_back(SVA);
        else
            parameter.push_back(productionRate);
        parameter.push_back(timeStep);
        model_.setParameter(parameter);

        parameter.clear();
        model.push(model_);

        // Model resolution
        if (system.getRestructuredMatrix() == 0)
            sciantix_variable[system.getFissionProductName() + " produced"].setFinalValue(
                solver.Integrator(sciantix_variable[system.getFissionProductName() + " produced"].getInitialValue(),
                                  model["Fission product production - " + system.getName()].getParameter().at(0),
                                  model["Fission product production - " + system.getName()].getParameter().at(1)));
        else if (system.getRestructuredMatrix() == 1)
            sciantix_variable[system.getFissionProductName() + " produced in HBS"].setFinalValue(solver.Integrator(
                sciantix_variable[system.getFissionProductName() + " produced in HBS"].getInitialValue(),
                model["Fission product production - " + system.getName()].getParameter().at(0),
                model["Fission product production - " + system.getName()].getParameter().at(1)));
    }
}