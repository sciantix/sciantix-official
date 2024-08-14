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
#include "SciantixVariableDeclaration.h"
#include "MapSciantixVariable.h"
#include "ModelDeclaration.h"
#include "SetMatrix.h"

void Simulation::Burnup()
{
    // Model declaration
    model.emplace_back();
    int modelIndex = model.size() - 1;
    model[modelIndex].setName("Burnup");

    double fissionRate = history_variable[hv["Fission rate"]].getFinalValue();
    double fuelDensity = sciantix_variable[sv["Fuel density"]].getFinalValue();
    double specificPower = fissionRate * (3.12e-17) / fuelDensity;

    double burnup = specificPower / 86400.0; // specific power in MW/kg, burnup in MWd/kg
    sciantix_variable[sv["Specific power"]].setFinalValue(specificPower);

    std::vector<double> parameter;
    parameter.push_back(burnup);

    std::string reference = ": The local burnup is calculated from the fission rate density.";

    model[modelIndex].setParameter(parameter);
    model[modelIndex].setRef(reference);

    // Model mapping
    MapModel();
    
    // Model resolution
    sciantix_variable[sv["Burnup"]].setFinalValue(
        solver.Integrator(
            sciantix_variable[sv["Burnup"]].getInitialValue(),
            model[sm["Burnup"]].getParameter().at(0),
            physics_variable[pv["Time step"]].getFinalValue()
        )
    );

    if(history_variable[hv["Fission rate"]].getFinalValue() > 0.0)
        sciantix_variable[sv["Irradiation time"]].setFinalValue(
            solver.Integrator(
                sciantix_variable[sv["Irradiation time"]].getInitialValue(),
                1.0 / sciantix_variable[sv["Specific power"]].getFinalValue(),
                24.0 * sciantix_variable[sv["Burnup"]].getIncrement()
            )
        );
    else
        sciantix_variable[sv["Irradiation time"]].setConstant();
        
    sciantix_variable[sv["FIMA"]].setFinalValue(
        solver.Integrator(
            sciantix_variable[sv["FIMA"]].getInitialValue(),
            history_variable[hv["Fission rate"]].getFinalValue() * 3.6e5 / sciantix_variable[sv["U"]].getFinalValue(), 
            sciantix_variable[sv["Irradiation time"]].getIncrement()
        )
    );
}
