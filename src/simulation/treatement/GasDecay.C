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

void Simulation::GasDecay()
{
    for (auto &system : sciantix_system)
    {
        if (gas[system.getGasName()].getDecayRate() > 0.0 && system.getRestructuredMatrix() == 0)
        {
            sciantix_variable[system.getGasName() + " decayed"].setFinalValue(
                solver.Decay(
                    sciantix_variable[system.getGasName() + " decayed"].getInitialValue(),
                    gas[system.getGasName()].getDecayRate(),
                    gas[system.getGasName()].getDecayRate() * sciantix_variable[system.getGasName() + " produced"].getFinalValue(), // sarebbe produced + produced in HBS ma le seconde devono esistere per tutte le specie..
                    physics_variable["Time step"].getFinalValue()));
        }
    }
}