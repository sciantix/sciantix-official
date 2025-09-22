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
#include "TimeManager.h"



void Simulation::GasProduction()
{
    // Model declaration
    for (auto &system : sciantix_system)
    {
        Model model_;
        model_.setName("Gas production - " + system.getName());
        model_.setRef(" ");
        

        std::vector<double> times;
        readFirstColumn(TestPath + "input_history.txt",times);
        std::vector<double> interpolated_times;
        std::vector<double> S_M(Number_of_time_steps_per_interval+1);
        interpolateTimes(times, interpolated_times,Number_of_time_steps_per_interval);
        for (int i = 0; i <= Number_of_time_steps_per_interval; ++i)
        {
            S_M[i] = 0.4 * pow(Sciantix_variables[0],2) * cos(interpolated_times[i]) + 6*sin(interpolated_times[i]);
        }

        // double productionRate = system.getProductionRate();
        double productionRate = S_M.at(history_variable["Time step number"].getFinalValue());
        double timeStep = physics_variable["Time step"].getFinalValue()/3600;

        std::vector<double> parameter;
        parameter.push_back(productionRate);
        parameter.push_back(timeStep);
        model_.setParameter(parameter);

        parameter.clear();
        model.push(model_);

        // Model resolution
        if (system.getRestructuredMatrix() == 0)
            sciantix_variable[system.getGasName() + " produced"].setFinalValue(
                solver.Integrator(
                    sciantix_variable[system.getGasName() + " produced"].getInitialValue(),
                    model["Gas production - " + system.getName()].getParameter().at(0),
                    model["Gas production - " + system.getName()].getParameter().at(1)
                )
            );
        else if (system.getRestructuredMatrix() == 1)
            sciantix_variable[system.getGasName() + " produced in HBS"].setFinalValue(
                solver.Integrator(
                    sciantix_variable[system.getGasName() + " produced in HBS"].getInitialValue(),
                    model["Gas production - " + system.getName()].getParameter().at(0),
                    model["Gas production - " + system.getName()].getParameter().at(1)
                )
            );
    }
}