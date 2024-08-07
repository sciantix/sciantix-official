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
//  Version: 2.0                                                                    //
//  Year: 2022                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"

void Simulation::GasProduction()
{
    for (auto &system : sciantix_system)
    {
        Model gas_prod_model;
        gas_prod_model.setName("Gas production - " + system.getName());
		gas_prod_model.setRef(" ");

		double productionRate = system.getProductionRate();
		double timeStep = physics_variable["Time step"].getFinalValue();

		std::vector<double> parameter;
		parameter.push_back(productionRate);
		parameter.push_back(timeStep);
		gas_prod_model.setParameter(parameter);

		parameter.clear();
        model.push(gas_prod_model);




        if (system.getRestructuredMatrix() == 0)
        {
            sciantix_variable[system.getGasName() + " produced"].setFinalValue(
                solver.Integrator(
                    sciantix_variable[system.getGasName() + " produced"].getInitialValue(),
                    model["Gas production - " + system.getName()].getParameter().at(0),
                    model["Gas production - " + system.getName()].getParameter().at(1)));
        }
        else if (system.getRestructuredMatrix() == 1)
            sciantix_variable[system.getGasName() + " produced in HBS"].setFinalValue(
                solver.Integrator(
                    sciantix_variable[system.getGasName() + " produced in HBS"].getInitialValue(),
                    model["Gas production - " + system.getName()].getParameter().at(0),
                    model["Gas production - " + system.getName()].getParameter().at(1)));
    }
}