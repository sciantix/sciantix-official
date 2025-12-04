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

void Simulation::GasRelease()
{
    // Calculation of the gas concentration arrived at the grain boundary, by mass balance.
    for (auto& system : sciantix_system)
    {
        if (system.getRestructuredMatrix() == 0)
        {
            sciantix_variable[system.getGasName() + " released"].setFinalValue(
                sciantix_variable[system.getGasName() + " produced"].getFinalValue() -
                sciantix_variable[system.getGasName() + " decayed"].getFinalValue() -
                sciantix_variable[system.getGasName() + " in grain"].getFinalValue() -
                sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue());

            if (sciantix_variable[system.getGasName() + " released"].getFinalValue() < 0.0)
                sciantix_variable[system.getGasName() + " released"].setFinalValue(0.0);
        }
    }

    // Intergranular gaseous swelling
    sciantix_variable["Intergranular gas swelling"].setFinalValue(
        3 / sciantix_variable["Grain radius"].getFinalValue() *
        sciantix_variable["Intergranular bubble concentration"].getFinalValue() *
        sciantix_variable["Intergranular bubble volume"].getFinalValue());

    // Fission gas release
    if (sciantix_variable["Xe produced"].getFinalValue() +
            sciantix_variable["Kr produced"].getFinalValue() >
        0.0)
        sciantix_variable["Fission gas release"].setFinalValue(
            (sciantix_variable["Xe released"].getFinalValue() +
             sciantix_variable["Kr released"].getFinalValue()) /
            (sciantix_variable["Xe produced"].getFinalValue() +
             sciantix_variable["Kr produced"].getFinalValue()));
    else
        sciantix_variable["Fission gas release"].setFinalValue(0.0);

    // Release-to-birth ratio: Xe133
    // Note that R/B is not defined with a null fission rate.
    if (sciantix_variable["Xe133 produced"].getFinalValue() -
            sciantix_variable["Xe133 decayed"].getFinalValue() >
        0.0)
        sciantix_variable["Xe133 R/B"].setFinalValue(
            sciantix_variable["Xe133 released"].getFinalValue() /
            (sciantix_variable["Xe133 produced"].getFinalValue() -
             sciantix_variable["Xe133 decayed"].getFinalValue()));
    else
        sciantix_variable["Xe133 R/B"].setFinalValue(0.0);

    // Release-to-birth ratio: Kr85m
    // Note that R/B is not defined with a null fission rate.
    if (sciantix_variable["Kr85m produced"].getFinalValue() -
            sciantix_variable["Kr85m decayed"].getFinalValue() >
        0.0)
        sciantix_variable["Kr85m R/B"].setFinalValue(
            sciantix_variable["Kr85m released"].getFinalValue() /
            (sciantix_variable["Kr85m produced"].getFinalValue() -
             sciantix_variable["Kr85m decayed"].getFinalValue()));
    else
        sciantix_variable["Kr85m R/B"].setFinalValue(0.0);

    // Helium fractional release
    if (sciantix_variable["He produced"].getFinalValue() > 0.0)
        sciantix_variable["He fractional release"].setFinalValue(
            sciantix_variable["He released"].getFinalValue() /
            sciantix_variable["He produced"].getFinalValue());
    else
        sciantix_variable["He fractional release"].setFinalValue(0.0);

    // Helium release rate
    if (physics_variable["Time step"].getFinalValue() > 0.0)
        sciantix_variable["He release rate"].setFinalValue(
            sciantix_variable["He released"].getIncrement() /
            physics_variable["Time step"].getFinalValue());
    else
        sciantix_variable["He release rate"].setFinalValue(0.0);
}